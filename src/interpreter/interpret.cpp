#include "interpret.h"
#include "Expression.h"
#include "Procedure.h"

#include "Value.h"
#include "builtins/JawsEq.h"
#include "builtins/JawsHof.h"
#include "builtins/JawsIO.h"
#include "builtins/JawsList.h"
#include "builtins/JawsMath.h"
#include "builtins/JawsThread.h"
#include "builtins/JawsVector.h"
#include "builtins/jaws_ffi.h"
#include "builtins/jaws_string.h"

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
    define("=", jaws_eq::equal);
    define("eq?", jaws_eq::equal);
    define("equal?", jaws_eq::equal);
    define("boolean?", jaws_eq::isBooleanProc);
    define("procedure?", jaws_eq::isProcedure);
    define("char?", jaws_eq::isChar);
    define("pair?", jaws_eq::isPair);
    define("null?", jaws_eq::isNull);
    define("port?", jaws_eq::isPort);
    define("eqv?", jaws_eq::isEqv);
    define("socket-server", jaws_io::socketServer);
    define("socket-connect", jaws_io::socketConnect);
    define("socket-accept", jaws_io::socketAccept);
    define("socket-read", jaws_io::socketRead);
    define("socket-write", jaws_io::socketWrite);
    define("socket-close", jaws_io::socketClose);
    define("socket-set-nonblocking!", jaws_io::socketSetNonBlocking);
    define("error", jaws_io::error);
    define("symbol?", jaws_eq::isSymbol);
    define("number?", jaws_eq::isNumber);
    define("string?", jaws_eq::isString);
    define("open-input-file", jaws_io::openInputFile);
    define("open-output-file", jaws_io::openOutputFile);
    define("close-port", jaws_io::closePort);
    define("read", jaws_io::read);
    define("write", jaws_io::write);
    define("display", jaws_io::display);
    define("newline", jaws_io::newline);
    define("map", jaws_hof::map);
    define("list?", jaws_eq::isList);
    define("list", jaws_list::listProcedure);
    define("car", jaws_list::carProcudure);
    define("cdr", jaws_list::cdrProcedure);
    define("cons", jaws_list::cons);
    define("append", jaws_list::append);
    define("load-library", jaws_ffi::loadLibrary);
    define("register-function", jaws_ffi::registerFunction);
    define("list-ref", jaws_list::listRef);
    define("list-set!", jaws_list::listSet);
    define("make-vector", jaws_vec::makeVector);
    define("vector", jaws_vec::vectorProcedure);
    define("vector-ref", jaws_vec::vectorRef);
    define("vector-set!", jaws_vec::vectorSet);
    define("vector-length", jaws_vec::vectorLength);
    define("vector-copy", jaws_vec::vectorCopy);
    define("vector-copy!", jaws_vec::vectorCopyTo);
    define("vector-fill!", jaws_vec::vectorFill);
    define("eval", jaws_hof::eval);
    define("apply", jaws_hof::apply);
    define("call/cc", jaws_hof::callCC);
    define("call-with-current-continuation", jaws_hof::callCC);
    define("number->string", jaws_string::numberToString);
    define("string=?", jaws_string::stringEqual);
    define("string<?", jaws_string::stringLess);
    define("string>?", jaws_string::stringGreater);
    define("string-ci=?", jaws_string::stringCiEqual);
    define("string-length", jaws_string::stringLength);
    define("string-append", jaws_string::stringAppend);
    define("substring", jaws_string::substring);
    define("string-ref", jaws_string::stringRef);
    define("string->list", jaws_string::stringToList);
    define("list->string", jaws_string::listToString);
    define("string-copy", jaws_string::stringCopy);
    define("string-upcase", jaws_string::stringUpcase);
    define("string-downcase", jaws_string::stringDowncase);
    define("thread-spawn", jaws_thread::threadSpawn);
    define("thread-join", jaws_thread::threadJoin);
    define("thread-sleep!", jaws_thread::threadSleep);
    define("thread-current-id", jaws_thread::threadCurrentId);
    define("mutex-create", jaws_thread::mutexCreate);
    define("mutex-lock!", jaws_thread::mutexLock);
    define("mutex-unlock!", jaws_thread::mutexUnlock);
    define("condition-variable-create", jaws_thread::conditionCreate);
    define("condition-variable-wait", jaws_thread::conditionWait);
    define("condition-variable-signal!", jaws_thread::conditionSignal);
    define("condition-variable-broadcast!", jaws_thread::conditionBroadcast);

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

std::optional<SchemeValue> interpret(
    InterpreterState& state,
    const std::shared_ptr<Expression>& expr)
{
    return std::visit(overloaded {
                          [&](const AtomExpression& e) -> std::optional<SchemeValue> {
                              return interpretAtom(state, e);
                          },
                          [&](const ListExpression& e) -> std::optional<SchemeValue> {
                              return interpretList(state, e);
                          },
                          [&](const sExpression& e) -> std::optional<SchemeValue> {
                              return interpretSExpression(state, e);
                          },
                          [&](const DefineExpression& e) -> std::optional<SchemeValue> {
                              return interpretDefine(state, e);
                          },
                          [&](const DefineSyntaxExpression& e) -> std::optional<SchemeValue> {
                              return std::nullopt;
                          },
                          [&](const DefineProcedure& e) -> std::optional<SchemeValue> {
                              return interpretDefineProcedure(state, e);
                          },
                          [&](const LambdaExpression& e) -> std::optional<SchemeValue> {
                              return interpretLambda(state, e);
                          },
                          [&](const IfExpression& e) -> std::optional<SchemeValue> {
                              return interpretIf(state, e);
                          },
                          [&](const QuoteExpression& e) -> std::optional<SchemeValue> {
                              return interpretQuote(state, e);
                          },
                          [&](const VectorExpression& e) -> std::optional<SchemeValue> {
                              return interpretVector(state, e);
                          },
                          [&](const TailExpression& e) -> std::optional<SchemeValue> {
                              return interpretTailCall(state, e);
                          },
                          [&](const LetExpression& e) -> std::optional<SchemeValue> {
                              return interpretLet(state, e);
                          },
                          [&](const ImportExpression& e) -> std::optional<SchemeValue> {
                              return std::nullopt;
                          },
                          [&](const SetExpression& e) -> std::optional<SchemeValue> {
                              return interpretSet(state, e);
                          },
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

        case Tokentype::CHAR: // or Tokentype::CHAR
        {
            std::string charLiteral = token.lexeme;
            std::string charValue = charLiteral.substr(2);
            if (charValue == "space") {
                return SchemeValue(char(' '));
            } else if (charValue == "newline") {
                return SchemeValue(char('\n'));
            } else if (charValue == "tab") {
                return SchemeValue(char('\t'));
            } else if (charValue == "return") {
                return SchemeValue(char('\r'));
            } else if (charValue.length() == 1) {
                return SchemeValue(charValue[0]);
            } else {
                throw InterpreterError("Unknown character literal: " + charLiteral);
            }
        }
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
    if (value->isProc() && !value->asProc()->isBuiltin()) {
        auto proc = value->asProc();
        if (auto userProc = std::dynamic_pointer_cast<UserProcedure>(proc)) {
            userProc->closure->define(def.name.lexeme, *value);
        }
    }
    state.env->define(def.name.lexeme, *value);
    return std::nullopt;
}

std::optional<SchemeValue> interpretDefineProcedure(InterpreterState& state, const DefineProcedure& proc)
{
    auto procedure = std::make_shared<UserProcedure>(proc.parameters, proc.body, state.env, proc.isVariadic);
    state.env->define(proc.name.lexeme, SchemeValue(std::move(procedure)));
    return std::nullopt;
}

std::optional<SchemeValue> interpretLambda(InterpreterState& state, const LambdaExpression& lambda)
{
    auto closureEnv = state.env;

    auto proc = std::make_shared<UserProcedure>(
        lambda.parameters,
        lambda.body,
        closureEnv,
        lambda.isVariadic);
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

// #TODO: Refactor this so tail calls are less hacky
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

        if (sexpr->elements.size() == 1) {
            return executeProcedure(state, SchemeValue(tailCall), tailCall->args);
        }

        return SchemeValue(tailCall);
    }
    return interpret(state, tail.expression);
}

bool fileExists(const std::string& path)
{
    std::ifstream f(path.c_str());
    return f.good();
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

        if (arg->isProc() && arg->asProc()->isTailCall()) {
            auto tailProc = arg->asProc();
            auto tailCall = std::dynamic_pointer_cast<TailCall>(tailProc);
            auto result = executeProcedure(state, SchemeValue(tailCall->proc), tailCall->args);
            if (result) {
                arg = result;
            }
        }

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
        throw InterpreterError("Cannot execute non-procedure: " + proc.toString());
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
        if (!result) {
            return std::nullopt;
        }

        if (!result->isProc() || !result->asProc()->isTailCall()) {
            return result;
        }
        procedure = result->asProc();
    }
}
}
