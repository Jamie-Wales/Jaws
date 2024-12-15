#include "Interpreter.h"
#include "Error.h"
#include "Expression.h"
#include "ExpressionUtils.h"
#include "Macro.h"
#include "Parser.h"
#include "Procedure.h"
#include "Scanner.h"
#include "Value.h"
#include "builtins/JawsEq.h"
#include "builtins/JawsHof.h"
#include "builtins/JawsIO.h"
#include "builtins/JawsList.h"
#include "builtins/JawsMath.h"
#include "builtins/JawsVector.h"
#include "run.h"
#include <memory>
#include <optional>
#include <variant>

void Interpreter::init()
{
    auto exprs = p->parse();
    if (!exprs) {
        throw InterpreterError("Could not parse AST");
    }

    std::vector<std::shared_ptr<Expression>> interpretExprs;
    for (auto expr : *exprs) {
        if (auto ele = std::get_if<DefineSyntaxExpression>(&expr->as)) {
            std::cout << "Collecting " << expr->toString();
            macroProcessor.collectMacros(expr);
        } else {
            interpretExprs.push_back(expr);
        }
    }

    for (auto expr : interpretExprs) {
        if (std::get_if<sExpression>(&expr->as)) {
            auto expanded = macroProcessor.expandMacros(exprToList(expr));
            std::string expanded_str = expanded->toString();
            p->load(s->tokenize(expanded_str));
            auto processedExpr = p->expression();

            if (processedExpr) {
                auto result = interpret(processedExpr);
                if (result) {
                    outputStream << result->toString() << std::endl;
                }
            }
        } else {
            auto result = interpret(expr);
            if (result) {
                outputStream << result->toString() << std::endl;
            }
        }
    }
}
Interpreter::Interpreter(std::shared_ptr<Scanner> s, std::shared_ptr<Parser> p)
    : scope(std::make_shared<Environment>())
    , s { s }
    , p { p }
{
    auto define = [this](const std::string& name, BuiltInProcedure::Func func) {
        scope->define(name, SchemeValue(std::make_shared<BuiltInProcedure>(func)));
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
    scope->define("=", SchemeValue(eq));
    scope->define("eq?", SchemeValue(eq));
    define("boolean?", jaws_eq::isBooleanProc);
    define("open-input-file", jaws_io::openInputFile);
    define("open-output-file", jaws_io::openOutputFile);
    define("close-port", jaws_io::closePort);
    define("read", jaws_io::read);
    define("write", jaws_io::write);
    define("display", jaws_io::display);
    define("newline", jaws_io::newline);
    define("eval", jaws_hof::eval);
    define("help", jaws_hof::printHelp);
    define("map", jaws_list::map);
    define("list", jaws_list::listProcedure);
    define("car", jaws_list::carProcudure);
    define("cdr", jaws_list::cdrProcedure);
    define("cadr", jaws_list::cadrProcedure);
    define("cons", jaws_list::cons);
    define("length", jaws_list::length);
    define("append", jaws_list::append);
    define("reverse", jaws_list::reverse);
    define("list-ref", jaws_list::listRef);
    define("list-tail", jaws_list::listTail);
    define("make-vector", jaws_vec::makeVector);
    define("vector", jaws_vec::vectorProcedure);
    define("vector-ref", jaws_vec::vectorRef);
    define("vector-set!", jaws_vec::vectorSet);
    define("vector-length", jaws_vec::vectorLength);
    define("procedure?", jaws_eq::isProcedure);
    define("null?", jaws_eq::isNull);
    define("port?", jaws_eq::isPort);
    define("eq?", jaws_eq::isEq);
    define("eqv?", jaws_eq::isEqv);
    define("apply", jaws_hof::apply);
}
std::optional<SchemeValue> Interpreter::interpretQuoteExpression(const QuoteExpression& qe, const Expression& e)
{
    return SchemeValue(qe.expression);
}

std::optional<SchemeValue> Interpreter::interpretAtom(const AtomExpression& atom, const Expression& expr)
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
            } else {
                double real = std::stod(complexStr.substr(0, separatorPos));
                double imag = std::stod(complexStr.substr(separatorPos));
                return SchemeValue(Number(std::complex<double>(real, imag)));
            }
        }
        case Tokentype::RATIONAL: {
            size_t slashPos = token.lexeme.find('/');
            if (slashPos != std::string::npos) {
                int numerator = std::stoi(token.lexeme.substr(0, slashPos));
                int denominator = std::stoi(token.lexeme.substr(slashPos + 1));
                return SchemeValue(Number(Number::Rational(numerator, denominator)));
            }
            throw InterpreterError("Invalid rational number format", expr);
        }
        case Tokentype::STRING:
            return SchemeValue(token.lexeme.substr(1, token.lexeme.length() - 2));
        case Tokentype::TRUE:
            return SchemeValue(true);
        case Tokentype::FALSE:
            return SchemeValue(false);
        case Tokentype::SYMBOL:
        case Tokentype::IDENTIFIER:
            if (auto value = scope->get(token.lexeme)) {
                return value;
            }
            throw InterpreterError("Undefined variable: " + token.lexeme, expr);
        default:
            expr.print();
            throw InterpreterError("Unexpected token type in atom");
        }
    } catch (const std::exception& e) {
        throw InterpreterError(e.what(), expr);
    }
}

std::optional<SchemeValue> Interpreter::interpretList(const ListExpression& list, const Expression& expr)
{
    std::list<SchemeValue> elements;
    for (const auto& ele : list.elements) {
        std::optional<SchemeValue> item = interpret(ele);
        if (!item) {
            return std::nullopt;
        }
        elements.push_back(*item);
    }
    return SchemeValue(std::move(elements));
}

std::optional<SchemeValue> Interpreter::interpretSExpression(const sExpression& se, const Expression& expr)
{
    auto call = evaluateProcedureCall(se, expr);
    if (!call) {
        return std::nullopt;
    }
    return executeProcedure(call->procedure, call->arguments);
}
std::optional<SchemeValue> Interpreter::defineExpression(const DefineExpression& de, const Expression& expr)
{
    std::optional<SchemeValue> exprValue = interpret(de.value);
    if (exprValue) {
        scope->define(de.name.lexeme, *exprValue);
        return std::nullopt;
    }
    throw InterpreterError("Cannot interpret define with name " + de.name.lexeme, expr);
}

std::optional<SchemeValue> Interpreter::defineProcedure(DefineProcedure& dp, const Expression&)
{
    auto proc = std::make_shared<UserProcedure>(
        dp.parameters,
        dp.body);
    scope->define(dp.name.lexeme, SchemeValue { std::move(proc) });
    return std::nullopt;
}
void Interpreter::run(const std::vector<std::shared_ptr<Expression>>& expressions)
{
    for (const auto& expr : expressions) {
        try {
            auto result = interpret(expr);
            if (result) {
                outputStream << result->toString() << std::endl;
            }
        } catch (const InterpreterError& e) {
            e.printFormattedError();
        }
    }
}

std::optional<SchemeValue> Interpreter::lambda(LambdaExpression& l, const Expression& e)
{
    auto proc = std::make_shared<UserProcedure>(
        std::move(l.parameters),
        std::move(l.body));

    auto lambda = SchemeValue(std::move(proc));
    return lambda;
}

std::optional<SchemeValue> Interpreter::ifExpression(const IfExpression& i, const Expression& e)
{

    auto condval = interpret(i.condition);

    if (condval->isTrue()) {
        return interpret(i.then);
    } else if (i.el) {
        return interpret(*i.el);
    }
    return std::nullopt;
}

std::optional<SchemeValue> Interpreter::interpretLetExpression(const LetExpression& le, const Expression& e)
{
    scope->pushFrame();
    std::vector<Token> params;
    std::vector<SchemeValue> args;
    for (const auto& [param, argExpr] : le.arguments) {
        auto arg = interpret(argExpr);
        if (!arg) {
            scope->popFrame();
            return std::nullopt;
        }
        scope->define(param.lexeme, *arg);
        params.push_back(param);
        args.push_back(*arg);
    }
    auto proc = std::make_shared<UserProcedure>(params, le.body);
    auto result = executeProcedure(SchemeValue(proc), args);
    scope->popFrame();
    return result;
}

std::optional<SchemeValue> Interpreter::interpretTailExpression(const TailExpression& t, const Expression& expr)
{
    if (auto se = std::get_if<sExpression>(&t.expression->as)) {
        auto call = evaluateProcedureCall(*se, expr);
        if (!call) {
            return std::nullopt;
        }
        if (call->procedure.asProc()->isBuiltin()) {
            return interpret(t.expression);
        }
        auto tailCall = std::make_shared<TailCall>(call->procedure.asProc(), call->arguments);
        return SchemeValue(tailCall);
    }
    return interpret(t.expression);
}

std::optional<SchemeValue> Interpreter::interpretImport(const ImportExpression& im, const Expression& e)
{
    for (auto& tok : im.import) {
        auto f = readFile(std::format("../lib/{}.scm", tok.lexeme));
        p->load(s->tokenize(f));
        auto exprs = p->parse();
        if (exprs) {
            std::vector<std::shared_ptr<Expression>> interpretExprs;
            for (auto expr : *exprs) {
                if (auto ele = std::get_if<DefineSyntaxExpression>(&expr->as)) {
                    std::cout << "Collecting from import: " << expr->toString();
                    macroProcessor.collectMacros(expr);
                } else {
                    interpretExprs.push_back(expr);
                }
            }
            for (auto expr : interpretExprs) {
                if (std::get_if<sExpression>(&expr->as)) {
                    auto expanded = macroProcessor.expandMacros(exprToList(expr));
                    std::string expanded_str = expanded->toString();
                    p->load(s->tokenize(expanded_str));
                    auto processedExpr = p->expression();

                    if (processedExpr) {
                        auto result = interpret(processedExpr);
                        if (result) {
                            outputStream << result->toString() << std::endl;
                        }
                    }
                } else {
                    auto result = interpret(expr);
                    if (result) {
                        outputStream << result->toString() << std::endl;
                    }
                }
            }
        }
    }
    return std::nullopt;
}
std::optional<SchemeValue> Interpreter::interpretSetExpression(const SetExpression& s, const Expression& e)
{
    if (auto val = interpret(s.value)) {
        this->scope->set(s.identifier.lexeme, *val);
    }

    throw std::runtime_error("Cannot set null value");
}

std::optional<SchemeValue> Interpreter::interpret(const std::shared_ptr<Expression>& e)
{
    return std::visit(overloaded {

                          [this, &e](const ImportExpression& a) -> std::optional<SchemeValue> { return interpretImport(a, *e); },
                          [this, &e](const AtomExpression& a) -> std::optional<SchemeValue> { return interpretAtom(a, *e); },
                          [this, &e](const ListExpression& l) -> std::optional<SchemeValue> { return interpretList(l, *e); },
                          [this, &e](const SetExpression& s) -> std::optional<SchemeValue> { return interpretSetExpression(s, *e); },
                          [this, &e](const sExpression& se) -> std::optional<SchemeValue> { return interpretSExpression(se, *e); },
                          [this, &e](const DefineExpression& de) -> std::optional<SchemeValue> { return defineExpression(de, *e); },
                          [this, &e](const VectorExpression& v) -> std::optional<SchemeValue> { return interpretVector(v, *e); },
                          [this, &e](DefineProcedure& dp) -> std::optional<SchemeValue> { return defineProcedure(dp, *e); },
                          [this, &e](LambdaExpression& l) -> std::optional<SchemeValue> { return lambda(l, *e); },
                          [this, &e](IfExpression& i) -> std::optional<SchemeValue> { return ifExpression(i, *e); },
                          [this, &e](QuoteExpression& q) -> std::optional<SchemeValue> { return interpretQuoteExpression(q, *e); },
                          [this, &e](TailExpression& t) -> std::optional<SchemeValue> { return interpretTailExpression(t, *e); },
                          [this, &e](LetExpression& t) -> std::optional<SchemeValue> { return interpretLetExpression(t, *e); },
                          [&e](const auto&) -> std::optional<SchemeValue> {
                              throw InterpreterError("Unknown expression type", *e);
                          } },
        e->as);
}

std::optional<SchemeValue> Interpreter::executeProcedure(const SchemeValue& proc,
    const std::vector<SchemeValue>& args)
{
    if (!proc.isProc()) {
        throw InterpreterError("Cannot execute non-procedure value: " + proc.toString());
    }

    auto procedure = proc.asProc();
    auto currentArgs = args;

    while (true) {
        if (procedure->isTailCall()) {
            auto tailCall = std::dynamic_pointer_cast<TailCall>(procedure);
            procedure = tailCall->proc;
            currentArgs = tailCall->args;
        }
        auto result = (*procedure)(*this, currentArgs);
        if (!result) {
            return std::nullopt;
        }
        if (result->isProc() && result->asProc()->isTailCall()) {
            procedure = result->asProc();
            continue;
        }
        return result;
    }
}

std::optional<Interpreter::ProcedureCall> Interpreter::evaluateProcedureCall(
    const sExpression& se, const Expression& expr)
{
    if (se.elements.empty()) {
        throw InterpreterError("Empty procedure call", expr);
    }
    auto proc = interpret(se.elements[0]);
    if (!proc) {
        return std::nullopt;
    }
    if (!proc->isProc()) {
        throw InterpreterError("First element is not a procedure: " + proc->toString(), expr);
    }
    std::vector<SchemeValue> args;
    args.reserve(se.elements.size() - 1);
    for (size_t i = 1; i < se.elements.size(); i++) {
        auto arg = interpret(se.elements[i]);
        if (!arg) {
            return std::nullopt;
        }
        args.push_back(*arg);
    }
    return ProcedureCall { *proc, std::move(args) };
}

std::optional<SchemeValue> Interpreter::interpretVector(const VectorExpression& v, const Expression& e)
{

    std::vector<SchemeValue> elements;
    elements.reserve(v.elements.size());
    for (const auto& ele : v.elements) {
        std::optional<SchemeValue> item = interpret(ele);
        if (!item) {
            return std::nullopt;
        }
        elements.push_back(*item);
    }
    return SchemeValue(std::move(elements));
}
