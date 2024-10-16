#include "Interpreter.h"
#include <stdexcept>
#include <iostream>

std::unordered_map<std::string, SchemeValue> Interpreter::environment;

Interpreter::Interpreter()
{
    environment["+"] = SchemeValue(std::make_shared<BuiltInProcedure>(plus));
    environment["-"] = SchemeValue(std::make_shared<BuiltInProcedure>(minus));
    environment["define"] = SchemeValue(std::make_shared<BuiltInProcedure>(define));
}

SchemeValue Interpreter::interpretAtom(const AtomExpression& atom)
{
    const Token& token = atom.value;
    switch (token.type) {
    case Tokentype::INTEGER:
        return SchemeValue(std::stoi(token.lexeme));
    case Tokentype::FLOAT:
        return SchemeValue(std::stod(token.lexeme));
    case Tokentype::STRING:
        return SchemeValue(token.lexeme.substr(1, token.lexeme.length() - 2)); // Remove quotes
    case Tokentype::TRUE:
        return SchemeValue(true);
    case Tokentype::FALSE:
        return SchemeValue(false);
    case Tokentype::SYMBOL:
    case Tokentype::IDENTIFIER:
    case Tokentype::DEFINE:
        if (auto it = environment.find(token.lexeme); it != environment.end()) {
            return it->second;
        }
        // Return a symbol instead of throwing an error
        return SchemeValue(SchemeValue::Symbol(token.lexeme));
    default:
        throw std::runtime_error("Unexpected token type in atom");
    }
}

SchemeValue Interpreter::interpretList(const ListExpression& list)
{
    std::vector<SchemeValue> elements;
    elements.reserve(list.elements.size());
    for (const auto& ele : list.elements) {
        elements.push_back(interpret(ele));
    }
    return SchemeValue(std::move(elements));
}

SchemeValue Interpreter::interpretSExpression(const sExpression& se)
{
    if (se.elements.empty()) {
        throw std::runtime_error("Empty procedure call");
    }
    SchemeValue proc = interpret(se.elements[0]);
    std::vector<SchemeValue> args;
    args.reserve(se.elements.size() - 1);
    for (size_t i = 1; i < se.elements.size(); ++i) {
        args.push_back(interpret(se.elements[i]));
    }
    return proc.call(*this, args);
}

SchemeValue Interpreter::plus(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.empty())
        return SchemeValue(0);
    SchemeValue result = args[0];
    for (size_t i = 1; i < args.size(); ++i) {
        result = result + args[i];
    }
    return result;
}

SchemeValue Interpreter::minus(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.empty())
        throw std::runtime_error("'-' requires at least one argument");
    if (args.size() == 1)
        return SchemeValue(-args[0].as<int>());
    SchemeValue result = args[0];
    for (size_t i = 1; i < args.size(); ++i) {
        result = result + SchemeValue(-args[i].as<int>());
    }
    return result;
}

SchemeValue Interpreter::define(Interpreter& interp, const std::vector<SchemeValue>& args)
{
    if (args.size() != 2) {
        throw std::runtime_error("Define requires exactly two arguments");
    }
    if (!args[0].isSymbol()) {
        throw std::runtime_error("First argument to define must be a symbol");
    }
    std::string name = args[0].asSymbol();
    SchemeValue value = args[1];
    interp.environment[name] = value;
    std::cout << "Defined: " << name << " = " << value.toString() << std::endl;
    return value;
}

SchemeValue Interpreter::interpret(const std::unique_ptr<Expression>& e)
{
    return std::visit(overloaded {
        [this](const AtomExpression& a) { return interpretAtom(a); },
        [this](const ListExpression& l) { return interpretList(l); },
        [this](const sExpression& se) { return interpretSExpression(se); },
        [](const auto&) -> SchemeValue {
            throw std::runtime_error("Unknown expression type");
        }
    }, e->as);
}

SchemeValue Interpreter::lookupVariable(const std::string& name) const
{
    auto it = environment.find(name);
    if (it != environment.end()) {
        return it->second;
    }
    throw std::runtime_error("Undefined variable: " + name);
}
