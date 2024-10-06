#pragma once
#include "Token.h"
#include "visit.h"
#include <iostream>
#include <memory>
#include <string>
#include <variant>
#include <vector>

class Expression;

class LiteralExpression {
public:
    Token value;
    explicit LiteralExpression(Token value)
        : value(std::move(value))
    {
    }
};

class BinaryExpression {
public:
    std::unique_ptr<Expression> left;
    Token operatorToken;
    std::unique_ptr<Expression> right;
    BinaryExpression(std::unique_ptr<Expression> left, Token operatorToken, std::unique_ptr<Expression> right)
        : left(std::move(left))
        , operatorToken(std::move(operatorToken))
        , right(std::move(right))
    {
    }
};

class PrefixExpression {
public:
    Token op;
    std::vector<std::unique_ptr<Expression>> args;
    PrefixExpression(Token op, std::vector<std::unique_ptr<Expression>> args)
        : op(std::move(op))
        , args(std::move(args))
    {
    }
};

class ListExpression {
public:
    std::vector<std::unique_ptr<Expression>> elements;
    explicit ListExpression(std::vector<std::unique_ptr<Expression>> elements)
        : elements(std::move(elements))
    {
    }
};

class Expression {
public:
    std::variant<LiteralExpression, BinaryExpression, PrefixExpression, ListExpression> as;
    int line;
    explicit Expression(std::variant<LiteralExpression, BinaryExpression, PrefixExpression, ListExpression> as, int line)
        : as(std::move(as))
        , line(line)
    {
    }

    void print(int indent = 0) const
    {
        std::string indentation(indent * 2, ' ');
        std::visit(overloaded {
                       [&](const LiteralExpression& e) {
                           std::cout << indentation << e.value.lexeme;
                       },
                       [&](const BinaryExpression& e) {
                           std::cout << indentation << "(" << e.operatorToken.lexeme << " ";
                           e.left->print(0);
                           std::cout << " ";
                           e.right->print(0);
                           std::cout << ")";
                       },
                       [&](const PrefixExpression& e) {
                           std::cout << indentation << "(" << e.op.lexeme;
                           for (const auto& arg : e.args) {
                               std::cout << " ";
                               arg->print(0);
                           }
                           std::cout << ")";
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
