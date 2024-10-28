#include "Interpreter.h"
#include "Error.h"
#include "Procedure.h"
#include <optional>
#include <variant>

Interpreter::Interpreter()
{
    environment["+"] = SchemeValue(std::make_shared<BuiltInProcedure>(plus));
    environment["-"] = SchemeValue(std::make_shared<BuiltInProcedure>(minus));
    environment["*"] = SchemeValue(std::make_shared<BuiltInProcedure>(mult));
    environment["/"] = SchemeValue(std::make_shared<BuiltInProcedure>(div));
    environment["<"] = SchemeValue(std::make_shared<BuiltInProcedure>(less));
    environment[">"] = SchemeValue(std::make_shared<BuiltInProcedure>(greater));
    environment["help"] = SchemeValue(std::make_shared<BuiltInProcedure>(printHelp));

    auto eq
        = std::make_shared<BuiltInProcedure>(equal);
    environment["="] = SchemeValue(eq);
    environment["eq?"] = SchemeValue(eq);
    environment["boolean?"] = SchemeValue(std::make_shared<BuiltInProcedure>(isBooleanProc));
    environment["open-input-file"] = SchemeValue(std::make_shared<BuiltInProcedure>(openInputFile));
    environment["open-output-file"] = SchemeValue(std::make_shared<BuiltInProcedure>(openOutputFile));
    environment["close-port"] = SchemeValue(std::make_shared<BuiltInProcedure>(closePort));
    environment["read"] = SchemeValue(std::make_shared<BuiltInProcedure>(read));
    environment["write"] = SchemeValue(std::make_shared<BuiltInProcedure>(write));
    environment["display"] = SchemeValue(std::make_shared<BuiltInProcedure>(display));
    environment["newline"] = SchemeValue(std::make_shared<BuiltInProcedure>(newline));

    environment["list"] = SchemeValue(std::make_shared<BuiltInProcedure>(listProcedure));
    environment["car"] = SchemeValue(std::make_shared<BuiltInProcedure>(carProcudure));
    environment["cdr"] = SchemeValue(std::make_shared<BuiltInProcedure>(cdrProcedure));
    environment["cadr"] = SchemeValue(std::make_shared<BuiltInProcedure>(cadrProcedure));
    environment["cons"] = SchemeValue(std::make_shared<BuiltInProcedure>(cons));
    environment["length"] = SchemeValue(std::make_shared<BuiltInProcedure>(length));
    environment["append"] = SchemeValue(std::make_shared<BuiltInProcedure>(append));
    environment["revrse"] = SchemeValue(std::make_shared<BuiltInProcedure>(reverse));
    environment["list-ref"] = SchemeValue(std::make_shared<BuiltInProcedure>(listRef));
    environment["list-tail"] = SchemeValue(std::make_shared<BuiltInProcedure>(listTail));

    // Vector operations
    environment["make-vector"] = SchemeValue(std::make_shared<BuiltInProcedure>(makeVector));
    environment["vector"] = SchemeValue(std::make_shared<BuiltInProcedure>(vectorProcedure));
    environment["vector-ref"] = SchemeValue(std::make_shared<BuiltInProcedure>(vectorRef));
    environment["vector-set!"] = SchemeValue(std::make_shared<BuiltInProcedure>(vectorSet));
    environment["vector-length"] = SchemeValue(std::make_shared<BuiltInProcedure>(vectorLength));
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
            if (auto it = environment.find(token.lexeme); it != environment.end()) {
                return it->second;
            }
            return SchemeValue(Symbol(token.lexeme));
        default:
            throw InterpreterError("Unexpected token type in atom", expr);
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
        return proc->call(*this, args);
    }

    throw InterpreterError("Cannot interpret sExpression", expr);
}

std::optional<SchemeValue> Interpreter::defineExpression(const DefineExpression& de, const Expression& expr)
{
    std::optional<SchemeValue> exprValue = interpret(de.value);
    if (exprValue) {
        environment[de.name.lexeme] = *exprValue;
        return std::nullopt;
    }
    throw InterpreterError("Cannot interpret define with name " + de.name.lexeme, expr);
}
std::optional<SchemeValue> Interpreter::defineProcedure(DefineProcedure& dp, const Expression&)
{
    auto proc = std::make_shared<UserProcedure>(
        std::move(dp.parameters),
        std::move(dp.body));

    environment[dp.name.lexeme] = SchemeValue(std::move(proc));
    return std::nullopt;
}
void Interpreter::run(const std::vector<std::unique_ptr<Expression>>& expressions)
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
std::optional<SchemeValue> Interpreter::interpret(const std::unique_ptr<Expression>& e)
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

std::optional<SchemeValue> Interpreter::lookupVariable(const std::string& name) const
{
    auto it = environment.find(name);
    if (it != environment.end()) {
        return it->second;
    }
    throw InterpreterError("Undefined variable: " + name);
}
