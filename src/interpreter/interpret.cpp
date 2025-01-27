#include "interpret.h"
#include "Expression.h"
#include "MacroExpression.h"
#include "Procedure.h"
#include "Value.h"
#include "builtins/JawsEq.h"
#include "builtins/JawsHof.h"
#include "builtins/JawsIO.h"
#include "builtins/JawsList.h"
#include "builtins/JawsMath.h"
#include "builtins/JawsVector.h"
#include "builtins/jaws_ffi.h"
#include "parse.h"
#include "run.h"
#include "scan.h"
#include <format>

namespace interpret {

InterpreterState createInterpreter()
{
    InterpreterState state;


    auto define = [&state](const std::string& name, BuiltInProcedure::Func func) {
        state.env->define(name, SchemeValue(std::make_shared<BuiltInProcedure>(func)));
    };
    define("+", jaws_math::plus);
    define("-", jaws_math::minus);
    define("*", jaws_math::mult);
    define("/", jaws_math::div);
    define("<=", jaws_math::lessOrEqual);
    define(">=", jaws_math::greaterOrEqual);
    define("<", jaws_math::less);
    define(">", jaws_math::greater);
    auto eq = std::make_shared<BuiltInProcedure>(jaws_math::equal);
    state.env->define("=", SchemeValue(eq));
    state.env->define("eq?", SchemeValue(eq));
    state.env->define("equal?", SchemeValue(eq));
    define("boolean?", jaws_eq::isBooleanProc);
    define("procedure?", jaws_eq::isProcedure);
    define("pair?", jaws_eq::isPair);
    define("null?", jaws_eq::isNull);
    define("port?", jaws_eq::isPort);
    define("eq?", jaws_eq::isEq);
    define("eqv?", jaws_eq::isEqv);

    define("error", jaws_io::error);
    define("symbol?", jaws_eq::isSymbol);
    define("open-input-file", jaws_io::openInputFile);
    define("open-output-file", jaws_io::openOutputFile);
    define("close-port", jaws_io::closePort);
    define("read", jaws_io::read);
    define("write", jaws_io::write);
    define("display", jaws_io::display);
    define("newline", jaws_io::newline);
    define("map", jaws_list::map);

    define("list?", jaws_eq::isList);
    define("list", jaws_list::listProcedure);
    define("car", jaws_list::carProcudure);
    define("cdr", jaws_list::cdrProcedure);
    define("cons", jaws_list::cons);
    define("length", jaws_list::length);
    define("append", jaws_list::append);
    define("load-library", jaws_ffi::loadLibrary);
    define("register-function", jaws_ffi::registerFunction);
    define("reverse", jaws_list::reverse);
    define("list-ref", jaws_list::listRef);
    define("list-tail", jaws_list::listTail);
    define("list-set!", jaws_list::listSet);
    define("member", jaws_list::member);
    define("assq", jaws_list::assq);
    define("make-vector", jaws_vec::makeVector);
    define("vector", jaws_vec::vectorProcedure);
    define("vector-ref", jaws_vec::vectorRef);
    define("vector-set!", jaws_vec::vectorSet);
    define("vector-length", jaws_vec::vectorLength);
    define("eval", jaws_hof::eval);
    define("apply", jaws_hof::apply);
    return state;
}

std::optional<SchemeValue> interpret(
    InterpreterState& state,
    const std::vector<std::shared_ptr<Expression>>& exprs)
{
    std::optional<SchemeValue> result = std::nullopt;
    for (const auto& expr : exprs) {
        result = interpret(state, expr);
    }
    return result;
}

std::optional<SchemeValue> interpretBegin(InterpreterState& state, const BeginExpression& begin)
{
    std::optional<SchemeValue> result = std::nullopt;
    for (const auto& expr : begin.body) {
        result = interpret(state, expr);
    }
    return result;
}

std::optional<SchemeValue> interpret(
    InterpreterState& state,
    const std::shared_ptr<Expression>& expr)
{
    return std::visit(overloaded {
                          [&](const AtomExpression& e) { return interpretAtom(state, e); },
                          [&](const ListExpression& e) { return interpretList(state, e); },
                          [&](const BeginExpression& e) { return interpretBegin(state, e); },
                          [&](const sExpression& e) { return interpretSExpression(state, e); },
                          [&](const DefineExpression& e) { return interpretDefine(state, e); },
                          [&](const DefineSyntaxExpression& e) { return interpretDefineSyntax(state, e); },
                          [&](const DefineProcedure& e) { return interpretDefineProcedure(state, e); },
                          [&](const LambdaExpression& e) { return interpretLambda(state, e); },
                          [&](const IfExpression& e) { return interpretIf(state, e); },
                          [&](const QuoteExpression& e) { return interpretQuote(state, e); },
                          [&](const VectorExpression& e) { return interpretVector(state, e); },
                          [&](const TailExpression& e) { return interpretTailCall(state, e); },
                          [&](const LetExpression& e) { return interpretLet(state, e); },
                          [&](const ImportExpression& e) { return interpretImport(state, e); },
                          [&](const SetExpression& e) { return interpretSet(state, e); },
                          [&](const auto&) -> std::optional<SchemeValue> {
                              throw InterpreterError("Unknown expression type");
                          } },
        expr->as);
}

std::optional<SchemeValue> interpretAtom(InterpreterState& state, const AtomExpression& atom)
{
    const Token& token = atom.value;
    try {
        switch (token.type) {
        case Tokentype::INTEGER:
            return SchemeValue(Number(std::stoi(token.lexeme)));

        case Tokentype::FLOAT:
            return SchemeValue(Number(std::stod(token.lexeme)));

        case Tokentype::COMPLEX: {
            std::string complexStr = token.lexeme.substr(0, token.lexeme.length() - 1);
            size_t plusPos = complexStr.find('+', 1);
            size_t minusPos = complexStr.find('-', 1);
            size_t separatorPos = std::min(plusPos, minusPos);

            if (separatorPos == std::string::npos) {
                return SchemeValue(Number(std::complex<double>(0, std::stod(complexStr))));
            }
            double real = std::stod(complexStr.substr(0, separatorPos));
            double imag = std::stod(complexStr.substr(separatorPos));
            return SchemeValue(Number(std::complex<double>(real, imag)));
        }

        case Tokentype::RATIONAL: {
            size_t slashPos = token.lexeme.find('/');
            if (slashPos != std::string::npos) {
                int numerator = std::stoi(token.lexeme.substr(0, slashPos));
                int denominator = std::stoi(token.lexeme.substr(slashPos + 1));
                return SchemeValue(Number(Number::Rational(numerator, denominator)));
            }
            throw InterpreterError("Invalid rational number format");
        }

        case Tokentype::STRING:
            return SchemeValue(token.lexeme.substr(1, token.lexeme.length() - 2));

        case Tokentype::TRUE:
            return SchemeValue(true);

        case Tokentype::FALSE:
            return SchemeValue(false);

        case Tokentype::IDENTIFIER:
            if (auto value = state.env->get(token.lexeme)) {
                return value;
            }
            throw InterpreterError("Undefined variable: " + token.lexeme);

        default:
            throw InterpreterError("Unexpected token type in atom");
        }
    } catch (const std::exception& e) {
        throw InterpreterError(e.what());
    }
}

std::optional<SchemeValue> interpretList(InterpreterState& state, const ListExpression& list)
{
    std::list<SchemeValue> elements;
    for (const auto& ele : list.elements) {
        auto item = interpret(state, ele);
        if (!item)
            return std::nullopt;
        elements.push_back(*item);
    }
    return SchemeValue(std::move(elements));
}

std::optional<SchemeValue> interpretSExpression(InterpreterState& state, const sExpression& sexpr)
{
    if (auto* atomExpr = std::get_if<AtomExpression>(&sexpr.elements[0]->as)) {
        std::string name = atomExpr->value.lexeme;
        if (state.env->isMacro(name)) {
            auto rules = state.env->getMacroDefinition(name);
            if (!rules) {
                throw InterpreterError("Internal error: macro rules not found");
            }
            if (auto expanded = expandMacro(state, name, sexpr, rules->literals)) {
                if (auto fullyExpanded = expandMacrosIn(state, *expanded)) {
                    return interpret(state, *fullyExpanded);
                }
                return interpret(state, *expanded);
            }
            throw InterpreterError("No matching pattern for macro: " + name);
        }
    }
    auto call = evaluateProcedureCall(state, sexpr);
    if (!call)
        return std::nullopt;
    return executeProcedure(state, call->procedure, call->arguments);
}

std::optional<SchemeValue> interpretDefine(InterpreterState& state, const DefineExpression& def)
{
    auto value = interpret(state, def.value);
    if (!value)
        return std::nullopt;
    state.env->define(def.name.lexeme, *value);
    return std::nullopt;
}

std::optional<SchemeValue> interpretDefineProcedure(InterpreterState& state, const DefineProcedure& proc)
{
    auto procedure = std::make_shared<UserProcedure>(proc.parameters, proc.body, state.env);
    state.env->define(proc.name.lexeme, SchemeValue(std::move(procedure)));
    return std::nullopt;
}

std::optional<SchemeValue> interpretLambda(InterpreterState& state, const LambdaExpression& lambda)
{
    auto proc = std::make_shared<UserProcedure>(
        lambda.parameters,
        lambda.body, state.env);
    return SchemeValue(std::move(proc));
}

std::optional<SchemeValue> interpretIf(InterpreterState& state, const IfExpression& ifexpr)
{
    auto condition = interpret(state, ifexpr.condition);
    if (!condition)
        return std::nullopt;

    if (condition->isTrue()) {
        return interpret(state, ifexpr.then);
    } else if (ifexpr.el) {
        return interpret(state, *ifexpr.el);
    }
    return std::nullopt;
}

std::optional<SchemeValue> interpretLet(InterpreterState& state, const LetExpression& let)
{
    auto letEnv = std::make_shared<Environment>(state.env);
    letEnv->pushFrame();

    std::vector<Token> params;
    std::vector<SchemeValue> args;
    for (const auto& [param, argExpr] : let.arguments) {
        auto arg = interpret(state, argExpr);
        if (!arg) {
            return std::nullopt;
        }
        letEnv->define(param.lexeme, *arg);
        params.push_back(param);
        args.push_back(*arg);
    }
    auto proc = std::make_shared<UserProcedure>(
        params,
        let.body,
        letEnv);
    if (let.name) {
        letEnv->define(let.name->lexeme, SchemeValue(proc));
    }

    return executeProcedure(state, SchemeValue(proc), args);
}

std::optional<SchemeValue> interpretQuote(InterpreterState& state, const QuoteExpression& quote)
{
    return SchemeValue(quote.expression);
}

std::optional<SchemeValue> interpretVector(InterpreterState& state, const VectorExpression& vec)
{
    std::vector<SchemeValue> elements;
    elements.reserve(vec.elements.size());

    for (const auto& ele : vec.elements) {
        auto item = interpret(state, ele);
        if (!item)
            return std::nullopt;
        elements.push_back(*item);
    }
    return SchemeValue(std::move(elements));
}

std::optional<SchemeValue> interpretTailCall(InterpreterState& state, const TailExpression& tail)
{
    if (auto sexpr = std::get_if<sExpression>(&tail.expression->as)) {
        auto call = evaluateProcedureCall(state, *sexpr);
        if (!call)
            return std::nullopt;
        if (call->procedure.asProc()->isBuiltin()) {
            return interpret(state, tail.expression);
        }
        auto tailCall = std::make_shared<TailCall>(call->procedure.asProc(), call->arguments);
        return executeProcedure(state, SchemeValue(tailCall), tailCall->args);
    }
    return interpret(state, tail.expression);
}

std::optional<SchemeValue> interpretDefineSyntax(
    InterpreterState& state,
    const DefineSyntaxExpression& syntax)
{
    if (auto* rules = std::get_if<SyntaxRulesExpression>(&syntax.rule->as)) {
        state.env->defineMacro(syntax.name.lexeme, rules->literals, rules->rules);
        return std::nullopt;
    }
    throw InterpreterError("Invalid syntax-rules expression in define-syntax");
}

bool fileExists(const std::string& path)
{
    std::ifstream f(path.c_str());
    return f.good();
}

std::optional<SchemeValue> interpretImport(InterpreterState& state, const ImportExpression& import)
{
    for (const auto& spec : import.imports) {
        auto libEnv = std::make_shared<Environment>(state.env);
        libEnv->pushFrame();
        std::string filePath;
        if (spec.library.size() == 1) {
            if (auto* atom = std::get_if<AtomExpression>(&spec.library[0]->as)) {
                std::string localPath = atom->value.lexeme + ".scm";
                if (fileExists(localPath)) {
                    filePath = localPath;
                } else {
                    filePath = std::format("../lib/{}", localPath);
                }
            }
        } else {
            std::string libPath;
            for (const auto& part : spec.library) {
                if (auto* atom = std::get_if<AtomExpression>(&part->as)) {
                    libPath += atom->value.lexeme + "/";
                }
            }
            filePath = std::format("../lib/{}.scm", libPath.substr(0, libPath.length() - 1));
        }

        if (!fileExists(filePath)) {
            throw InterpreterError("Cannot find file to import: " + filePath);
        }
        auto source = readFile(filePath);
        auto tokens = scanner::tokenize(source);
        auto exprs = parse::parse(std::move(tokens));
        InterpreterState libState;
        libState.env = libEnv;
        interpret(libState, *exprs);
        switch (spec.type) {
        case ImportExpression::ImportSet::Type::DIRECT: {
            for (const auto& [name, value] : libEnv->frames.front()->bound) {
                state.env->define(name, value);
            }
            break;
        }
        case ImportExpression::ImportSet::Type::ONLY: {
            for (const auto& id : spec.identifiers) {
                if (auto value = libEnv->get(id.lexeme)) {
                    state.env->define(id.lexeme, *value);
                } else {
                    throw InterpreterError("Symbol not found in library: " + id.lexeme);
                }
            }
            break;
        }
        case ImportExpression::ImportSet::Type::EXCEPT: {
            for (const auto& [name, value] : libEnv->frames.front()->bound) {
                bool shouldSkip = false;
                for (const auto& id : spec.identifiers) {
                    if (name == id.lexeme) {
                        shouldSkip = true;
                        break;
                    }
                }
                if (!shouldSkip) {
                    state.env->define(name, value);
                }
            }
            break;
        }
        case ImportExpression::ImportSet::Type::PREFIX: {
            for (const auto& [name, value] : libEnv->frames.front()->bound) {
                state.env->define(spec.prefix.lexeme + name, value);
            }
            break;
        }
        case ImportExpression::ImportSet::Type::RENAME: {
            for (const auto& [name, value] : libEnv->frames.front()->bound) {
                bool isRenamed = false;
                for (const auto& [oldName, newName] : spec.renames) {
                    if (name == oldName.lexeme) {
                        state.env->define(newName.lexeme, value);
                        isRenamed = true;
                        break;
                    }
                }
                if (!isRenamed) {
                    state.env->define(name, value);
                }
            }
            break;
        }
        }
    }
    return std::nullopt;
}
std::optional<SchemeValue> interpretSet(InterpreterState& state, const SetExpression& set)
{
    auto val = interpret(state, set.value);
    if (!val) {
        throw InterpreterError("Cannot set undefined value");
    }
    state.env->set(set.identifier.lexeme, *val);
    return std::nullopt;
}

std::optional<ProcedureCall> evaluateProcedureCall(
    InterpreterState& state,
    const sExpression& sexpr)
{
    if (sexpr.elements.empty()) {
        throw InterpreterError("Empty procedure call");
    }

    auto proc = interpret(state, sexpr.elements[0]);
    if (!proc)
        return std::nullopt;
    if (!proc->isProc()) {
        throw InterpreterError("First element is not a procedure: " + proc->toString());
    }

    std::vector<SchemeValue> args;
    args.reserve(sexpr.elements.size() - 1);
    for (size_t i = 1; i < sexpr.elements.size(); i++) {
        auto arg = interpret(state, sexpr.elements[i]);
        if (!arg)
            return std::nullopt;
        args.push_back(*arg);
    }

    return ProcedureCall { *proc, std::move(args) };
}

std::optional<SchemeValue> executeProcedure(
    InterpreterState& state,
    const SchemeValue& proc,
    const std::vector<SchemeValue>& args)
{
    if (!proc.isProc()) {
        throw InterpreterError(
            "Cannot execute non-procedure: " + proc.toString());
    }

    auto procedure = proc.asProc();
    auto currentArgs = args;

    while (true) {
        while (procedure->isTailCall()) {
            auto tailCall = std::dynamic_pointer_cast<TailCall>(procedure);
            procedure = tailCall->proc;
            currentArgs = tailCall->args;
        }

        auto result = (*procedure)(state, currentArgs);
        if (!result)
            return std::nullopt;

        if (!result->isProc() || !result->asProc()->isTailCall()) {
            return result;
        }

        procedure = result->asProc();
    }
}
}
