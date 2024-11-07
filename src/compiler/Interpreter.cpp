#include "Interpreter.h"
#include "Error.h"
#include "Procedure.h"
#include <memory>
#include <optional>
#include <variant>

Interpreter::Interpreter()
    : scope(std::make_shared<Environment>())
{
    auto define = [this](const std::string& name, BuiltInProcedure::Func func) {
        scope->define(name, SchemeValue(std::make_shared<BuiltInProcedure>(func)));
    };

    define("+", plus);
    define("-", minus);
    define("*", mult);
    define("/", div);
    define("<=", less);
    define(">=", greater);
    define("<", less);
    define(">", greater);

    // Equality
    auto eq = std::make_shared<BuiltInProcedure>(equal);
    scope->define("=", SchemeValue(eq));
    scope->define("eq?", SchemeValue(eq));
    define("boolean?", isBooleanProc);
    define("open-input-file", openInputFile);
    define("open-output-file", openOutputFile);
    define("close-port", closePort);
    define("read", read);
    define("write", write);
    define("display", display);
    define("newline", newline);
    define("eval", eval);
    define("help", printHelp);
    define("map", map);
    define("list", listProcedure);
    define("car", carProcudure);
    define("cdr", cdrProcedure);
    define("cadr", cadrProcedure);
    define("cons", cons);
    define("length", length);
    define("append", append);
    define("reverse", reverse);
    define("list-ref", listRef);
    define("list-tail", listTail);
    define("make-vector", makeVector);
    define("vector", vectorProcedure);
    define("vector-ref", vectorRef);
    define("vector-set!", vectorSet);
    define("vector-length", vectorLength);
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
    if (se.elements.empty()) {
        throw InterpreterError("Empty procedure call", expr);
    }
    std::optional<SchemeValue> proc = interpret(se.elements[0]);
    if (proc) {
        std::vector<SchemeValue> args;
        args.reserve(se.elements.size() - 1);
        for (size_t i = 1; i < se.elements.size(); ++i) {
            auto ele = interpret(se.elements[i]);
            if (ele) {
                args.push_back(*ele);
            } else {
                return std::nullopt;
            }
        }
        return *proc->call(*this, args);
    }

    throw InterpreterError("Cannot interpret sExpression", expr);
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
        (dp.body));
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
std::optional<SchemeValue> Interpreter::interpret(const std::shared_ptr<Expression>& e)
{
    return std::visit(overloaded {
                          [this, &e](const AtomExpression& a) -> std::optional<SchemeValue> { return interpretAtom(a, *e); },
                          [this, &e](const ListExpression& l) -> std::optional<SchemeValue> { return interpretList(l, *e); },
                          [this, &e](const sExpression& se) -> std::optional<SchemeValue> { return interpretSExpression(se, *e); },
                          [this, &e](const DefineExpression& de) -> std::optional<SchemeValue> { return defineExpression(de, *e); },
                          [this, &e](const VectorExpression& v) -> std::optional<SchemeValue> { return interpretVector(v, *e); },
                          [this, &e](DefineProcedure& dp) -> std::optional<SchemeValue> { return defineProcedure(dp, *e); },
                          [this, &e](LambdaExpression& l) -> std::optional<SchemeValue> { return lambda(l, *e); },
                          [this, &e](IfExpression& i) -> std::optional<SchemeValue> { return ifExpression(i, *e); },
                          [this, &e](QuoteExpression& q) -> std::optional<SchemeValue> { return interpretQuoteExpression(q, *e); },
                          [&e](const auto&) -> std::optional<SchemeValue> {
                              throw InterpreterError("Unknown expression type", *e);
                          } },
        e->as);
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
