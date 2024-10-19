#include "Interpreter.h"
#include <stdexcept>

Interpreter::Interpreter()
{
    environment["+"] = SchemeValue(std::make_shared<BuiltInProcedure>(plus));
    environment["-"] = SchemeValue(std::make_shared<BuiltInProcedure>(minus));
    environment["boolean?"] = SchemeValue(std::make_shared<BuiltInProcedure>(isBooleanProc));
}

std::optional<SchemeValue> Interpreter::interpretAtom(const AtomExpression& atom)
{
    const Token& token = atom.value;
    switch (token.type) {
    case Tokentype::INTEGER:
        return SchemeValue(std::stoi(token.lexeme));
    case Tokentype::FLOAT:
        return SchemeValue(std::stod(token.lexeme));
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
        throw std::runtime_error("Unexpected token type in atom");
    }
}

std::optional<SchemeValue> Interpreter::interpretList(const ListExpression& list)
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

    throw std::runtime_error("Cannot interpert list");
}

std::optional<SchemeValue> Interpreter::interpretSExpression(const sExpression& se)
{
    if (se.elements.empty()) {
        throw std::runtime_error("Empty procedure call");
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

    throw std::runtime_error("Cannot interpret sExpression");
}

std::optional<SchemeValue> Interpreter::define(const DefineExpression& de)
{
    std::optional<SchemeValue> expr = interpret(de.value);
    if (expr) {
        environment[de.name.lexeme] = *expr;
        return std::nullopt;
    }
    throw std::runtime_error("Cannot interpret define with name " + de.name.lexeme);
}

std::optional<SchemeValue> Interpreter::interpret(const std::unique_ptr<Expression>& e)
{
    return std::visit(overloaded {
                          [this](const AtomExpression& a) { return interpretAtom(a); },
                          [this](const ListExpression& l) { return interpretList(l); },
                          [this](const sExpression& se) { return interpretSExpression(se); },
                          [this](const DefineExpression& de) { return define(de); },
                          [this](const DefineProcedure& dp) -> std::optional<SchemeValue> {
                              /** ----
                               * :TODO DO THIS
                               *
                               * return defineProcedure(dp);
                               * **/
                              return std::nullopt;
                          },
                          [](const auto&) -> std::optional<SchemeValue> {
                              throw std::runtime_error("Unknown expression type");
                          } },
        e->as);
}

SchemeValue Interpreter::lookupVariable(const std::string& name) const
{
    auto it = environment.find(name);
    if (it != environment.end()) {
        return it->second;
    }
    throw std::runtime_error("Undefined variable: " + name);
}
