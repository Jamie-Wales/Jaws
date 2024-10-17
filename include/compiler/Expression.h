#pragma once
#include "Token.h"
#include "Visit.h"
#include <format>
#include <iostream>
#include <memory>
#include <string>
#include <variant>
#include <vector>

class Expression;

class AtomExpression {
public:
    Token value;
    AtomExpression(Token token)
        : value(std::move(token))
    {
    }
};

class ListExpression {
public:
    std::vector<std::unique_ptr<Expression>> elements;
    ListExpression(std::vector<std::unique_ptr<Expression>> elems)
        : elements(std::move(elems))
    {
    }
};

class sExpression {
public:
    std::vector<std::unique_ptr<Expression>> elements;
    sExpression(std::vector<std::unique_ptr<Expression>> elems)
        : elements(std::move(elems))
    {
    }
};
class DefineExpression {
public:
    Token name;
    std::unique_ptr<Expression> value;
    DefineExpression(Token n, std::unique_ptr<Expression> v)
        : name(std::move(n))
        , value(std::move(v))
    {
    }
};

class DefineProcedure {
public:
    Token name;
    size_t arity;
    std::unique_ptr<Expression> body;

    DefineProcedure(Token name, size_t arity, std::unique_ptr<Expression> body)
        : name(name)
        , arity(arity)
        , body(std::move(body))
    {
    }
};

class Expression {
public:
    std::variant<AtomExpression, sExpression, ListExpression, DefineExpression, DefineProcedure> as;
    int line;
    Expression(std::variant<AtomExpression, sExpression, ListExpression, DefineExpression, DefineProcedure> as, int line)
        : as(std::move(as))
        , line(line)
    {
    }

    void print(int indent = 0) const
    {
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
                       [&](const DefineExpression& d) {
                           std::cout << indentation << std::format("(define {}", d.name.lexeme) << std::endl;
                           d.value->print(indent + 1);
                           std::cout << indentation << ")" << std::endl;
                       },

                       [&](const DefineProcedure& d) {
                           std::cout << indentation << std::format("(define {}", d.name.lexeme) << std::endl;
                           d.body->print(indent + 1);
                           std::cout << indentation << ")" << std::endl;
                       },
                   },
            as);
    }
};
