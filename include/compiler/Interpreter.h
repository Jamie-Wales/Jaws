#pragma once
#include "Value.h"
#include <Expression.h>
#include <memory>
#include <unordered_map>

class Interpreter {
public:
    SchemeValue interpret(const std::unique_ptr<Expression>& e)
    {
        return std::visit(overloaded {
                              [this](const AtomExpression& e) -> SchemeValue { return interpretAtom(e); },
                              [this](const ListExpression& e) -> SchemeValue { return interpretList(e); },
                              [](const auto&) -> SchemeValue {
                                  throw std::runtime_error("Unknown expression type");
                              } },
            e->as);
    }

private:
    std::unordered_map<std::string, SchemeValue> environment;

    SchemeValue interpretAtom(const AtomExpression& atom)
    {
        const Token& token = atom.value;
        switch (token.type) {
        case Tokentype::INTEGER:
            return SchemeValue(std::stoi(token.lexeme));
        case Tokentype::FLOAT:
            return SchemeValue(std::stod(token.lexeme));
        case Tokentype::STRING:
            return SchemeValue(token.lexeme.substr(1, token.lexeme.length() - 2)); // Remove quotes
        case Tokentype::SYMBOL:
            if (auto it = environment.find(token.lexeme); it != environment.end()) {
                return it->second;
            }
            throw std::runtime_error("Undefined symbol: " + token.lexeme);
        default:
            throw std::runtime_error("Unexpected token type in atom");
        }
    }

    SchemeValue interpretList(const ListExpression& list)
    {
        if (list.elements.empty()) {
            return SchemeValue(std::string("()")); // Empty list
        }

        const auto& first = list.elements[0];
        const auto& firstAtom = std::get<AtomExpression>(first->as);
        std::string op = firstAtom.value.lexeme;

        if (op == "define") {
            return interpretDefine(list);
        } else if (op == "if") {
            return interpretIf(list);
        } else if (op == "+") {
            return interpretAdd(list);
        }

        throw std::runtime_error("Unknown operation: " + op);
    }

    SchemeValue interpretDefine(const ListExpression& list)
    {
        if (list.elements.size() != 3) {
            throw std::runtime_error("Invalid 'define' syntax");
        }
        const auto& secondElem = list.elements[1];
        const auto& atomExpr = std::get<AtomExpression>(secondElem->as);
        std::string name = atomExpr.value.lexeme;
        SchemeValue value = interpret(list.elements[2]);
        environment[name] = value;
        return value;
    }

    SchemeValue interpretIf(const ListExpression& list)
    {
        if (list.elements.size() != 4) {
            throw std::runtime_error("Invalid 'if' syntax");
        }
        SchemeValue condition = interpret(list.elements[1]);
        if (condition.isTrue()) {
            return interpret(list.elements[2]);
        } else {
            return interpret(list.elements[3]);
        }
    }

    SchemeValue interpretAdd(const ListExpression& list)
    {
        if (list.elements.size() < 2) {
            throw std::runtime_error("'+' requires at least one argument");
        }
        SchemeValue result = interpret(list.elements[1]);
        for (size_t i = 2; i < list.elements.size(); ++i) {
            result = result + interpret(list.elements[i]);
        }
        return result;
    }
};
