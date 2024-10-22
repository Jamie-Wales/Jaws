#include "Interpreter.h"
#include "Error.h"
#include <stdexcept>

Interpreter::Interpreter()
{
    environment["+"] = SchemeValue(std::make_shared<BuiltInProcedure>(plus));
    environment["-"] = SchemeValue(std::make_shared<BuiltInProcedure>(minus));
    environment["boolean?"] = SchemeValue(std::make_shared<BuiltInProcedure>(isBooleanProc));
    environment["list"] = SchemeValue(std::make_shared<BuiltInProcedure>(listProcedure));
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
            return SchemeValue(SchemeValue::Symbol(token.lexeme));
        default:
            throw InterpreterError("Unexpected token type in atom", expr);
        }
    } catch (const std::exception& e) {
        throw InterpreterError(e.what(), expr);
    }
}

std::optional<SchemeValue> Interpreter::interpretList(const ListExpression& list, const Expression& expr)
{
    std::vector<SchemeValue> elements;
    elements.reserve(list.elements.size());
    for (const auto& ele : list.elements) {
        std::optional<SchemeValue> item = interpret(ele);
        if (item) {
            elements.push_back(*item);
            return SchemeValue(std::move(elements));
        }
    }

    throw InterpreterError("Cannot interpret list", expr);
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

std::optional<SchemeValue> Interpreter::define(const DefineExpression& de, const Expression& expr)
{
    std::optional<SchemeValue> exprValue = interpret(de.value);
    if (exprValue) {
        environment[de.name.lexeme] = *exprValue;
        return std::nullopt;
    }
    throw InterpreterError("Cannot interpret define with name " + de.name.lexeme, expr);
}

std::optional<SchemeValue> Interpreter::interpret(const std::unique_ptr<Expression>& e)
{
    return std::visit(overloaded {
                          [this, &e](const AtomExpression& a) { return interpretAtom(a, *e); },
                          [this, &e](const ListExpression& l) { return interpretList(l, *e); },
                          [this, &e](const sExpression& se) { return interpretSExpression(se, *e); },
                          [this, &e](const DefineExpression& de) { return define(de, *e); },
                          [this, &e](const DefineProcedure& dp) -> std::optional<SchemeValue> {
                              /** ----
                               * :TODO DO THIS
                               *
                               * return defineProcedure(dp);
                               * **/
                              throw InterpreterError("DefineProcedure not implemented", *e);
                          },
                          [&e](const auto&) -> std::optional<SchemeValue> {
                              throw InterpreterError("Unknown expression type", *e);
                          } },
        e->as);
}

SchemeValue Interpreter::lookupVariable(const std::string& name) const
{
    auto it = environment.find(name);
    if (it != environment.end()) {
        return it->second;
    }
    throw InterpreterError("Undefined variable: " + name);
}
