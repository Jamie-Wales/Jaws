#pragma once
#include "Token.h"
#include "Visit.h"
#include <iostream>
#include <memory>
#include <string>
#include <variant>
#include <vector>

class Expression;

class AtomExpression {
public:
    Token value;
    AtomExpression(Token token) : value(std::move(token)) {}
};

class ListExpression {
public:
    std::vector<std::unique_ptr<Expression>> elements;
    ListExpression(std::vector<std::unique_ptr<Expression>> elems)
        : elements(std::move(elems)) {}
};

class sExpression {
public:
    std::vector<std::unique_ptr<Expression>> elements;
    sExpression(std::vector<std::unique_ptr<Expression>> elems)
        : elements(std::move(elems)) {}
};

class Expression {
public:
    std::variant<AtomExpression, sExpression, ListExpression> as;
    int line;
    Expression(std::variant<AtomExpression, sExpression, ListExpression> as, int line)
        : as(std::move(as)), line(line) {}

    void print(int indent = 0) const {
        std::string indentation(indent * 2, ' ');
        std::visit(overloaded {
            [&](const AtomExpression& e) {
                std::cout << indentation << e.value.lexeme << std::endl;
            },
            [&](const ListExpression& e) {
                std::cout << indentation << "(" << std::endl;
                for (const auto& elem : e.elements) {
                    elem->print(indent + 1);
                }
                std::cout << indentation << ")" << std::endl;
            },
            [&](const sExpression& s) {
                std::cout << indentation << "(" << std::endl;
                for (const auto& elem : s.elements) {
                    elem->print(indent + 1);
                }
                std::cout << indentation << ")" << std::endl;
            },
        }, as);
    }
};
