#pragma once
#include "Token.h"
#include "visit.h"
#include <iostream>
#include <memory>
#include <string>
#include <variant>
#include <vector>

class Expression;

class AtomExpression {
public:
    Token value;
    AtomExpression(Token value)
        : value(std::move(value))
    {
    }
};

class ListExpression {
public:
    std::vector<std::unique_ptr<Expression>> elements;
    ListExpression(std::vector<std::unique_ptr<Expression>> elements)
        : elements(std::move(elements))
    {
    }
};

class Expression {
public:
    std::variant<AtomExpression, ListExpression> as;
    int line;
    Expression(std::variant<AtomExpression, ListExpression> as, int line)
        : as(std::move(as))
        , line(line)
    {
    }

    void print(int indent = 0) const
    {
        std::string indentation(indent * 2, ' ');
        std::visit(overloaded {
                       [&](const AtomExpression& e) {
                           std::cout << indentation << e.value.lexeme;
                       },
                       [&](const ListExpression& e) {
                           std::cout << indentation << "(list";
                           for (const auto& elem : e.elements) {
                               std::cout << " ";
                               elem->print(0);
                           }
                           std::cout << ")";
                       } },
            as);
    }
};
template <typename T>
std::unique_ptr<Expression> make_expression(T&& expr, int line)
{
    return std::make_unique<Expression>(std::forward<T>(expr), line);
}
