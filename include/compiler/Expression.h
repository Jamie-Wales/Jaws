#pragma once
#include "Token.h"
#include "Visit.h"
#include <format>
#include <iostream>
#include <memory>
#include <optional>
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

class LambdaExpression {
public:
    std::vector<Token> parameters;
    std::unique_ptr<Expression> body;
    LambdaExpression(std::vector<Token> parameters, std::unique_ptr<Expression> body)
        : parameters(std::move(parameters))
        , body(std::move(body))
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
    std::vector<Token> parameters;
    std::unique_ptr<Expression> body;

    DefineProcedure(Token name, std::vector<Token> parameters, std::unique_ptr<Expression> body)
        : name(name)
        , parameters(std::move(parameters))
        , body(std::move(body))
    {
    }
};

class VectorExpression {
public:
    std::vector<std::unique_ptr<Expression>> elements;
    VectorExpression(std::vector<std::unique_ptr<Expression>> elems)
        : elements(std::move(elems))
    {
    }
};

class IfExpression {
public:
    std::unique_ptr<Expression> condition;
    std::unique_ptr<Expression> then;
    std::optional<std::unique_ptr<Expression>> el;
    IfExpression(std::unique_ptr<Expression> condition, std::unique_ptr<Expression> then, std::optional<std::unique_ptr<Expression>> el)
        : condition(std::move(condition))
        , then(std::move(then))
        , el(std::move(el))
    {
    }
};

class Expression {
public:
    std::variant<AtomExpression, sExpression, ListExpression, DefineExpression, DefineProcedure, VectorExpression, LambdaExpression, IfExpression> as;
    int line;
    Expression(std::variant<AtomExpression, sExpression, ListExpression, DefineExpression, DefineProcedure, VectorExpression, LambdaExpression, IfExpression> as, int line)
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

                       [&](const IfExpression& i) {
                           std::cout << indentation << "(if";
                           i.condition->print();
                           std::cout << "then" << std::endl;
                           i.then->print();
                           if (i.el) {
                               std::cout << "else" << std::endl;
                               (*i.el)->print();
                           }
                           std::cout << indentation << ")" << std::endl;
                       },
                       [&](const VectorExpression& e) {
                           std::cout << indentation << "#(" << std::endl;
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

                       [&](const LambdaExpression& l) {
                           std::cout << indentation << "(lambda " << std::endl;
                           l.body->print();
                           std::cout << indentation << ")" << std::endl;
                       },
                   },
            as);
    }
    std::unique_ptr<Expression> clone() const
    {
        return std::visit(overloaded {
                              [&](const AtomExpression& e) -> std::unique_ptr<Expression> {
                                  return std::make_unique<Expression>(AtomExpression { e.value }, line);
                              },
                              [&](const ListExpression& e) -> std::unique_ptr<Expression> {
                                  std::vector<std::unique_ptr<Expression>> clonedElements;
                                  clonedElements.reserve(e.elements.size());
                                  for (const auto& elem : e.elements) {
                                      clonedElements.push_back(elem->clone());
                                  }
                                  return std::make_unique<Expression>(ListExpression { std::move(clonedElements) }, line);
                              },

                              [&](const VectorExpression& e) -> std::unique_ptr<Expression> {
                                  std::vector<std::unique_ptr<Expression>> clonedElements;
                                  clonedElements.reserve(e.elements.size());
                                  for (const auto& elem : e.elements) {
                                      clonedElements.push_back(elem->clone());
                                  }
                                  return std::make_unique<Expression>(VectorExpression { std::move(clonedElements) }, line);
                              },
                              [&](const sExpression& e) -> std::unique_ptr<Expression> {
                                  std::vector<std::unique_ptr<Expression>> clonedElements;
                                  clonedElements.reserve(e.elements.size());
                                  for (const auto& elem : e.elements) {
                                      clonedElements.push_back(elem->clone());
                                  }
                                  return std::make_unique<Expression>(sExpression { std::move(clonedElements) }, line);
                              },
                              [&](const DefineExpression& e) -> std::unique_ptr<Expression> {
                                  return std::make_unique<Expression>(
                                      DefineExpression { e.name, e.value->clone() },
                                      line);
                              },
                              [&](const DefineProcedure& e) -> std::unique_ptr<Expression> {
                                  return std::make_unique<Expression>(
                                      DefineProcedure { e.name, e.parameters, e.body->clone() },
                                      line);
                              },
                              [&](const LambdaExpression& l) -> std::unique_ptr<Expression> {
                                  return std::make_unique<Expression>(
                                      LambdaExpression { l.parameters, l.body->clone() },
                                      line);
                              },

                              [&](const IfExpression& i) -> std::unique_ptr<Expression> {
                                  if (i.el) {
                                      return std::make_unique<Expression>(
                                          IfExpression {
                                              i.condition->clone(),
                                              i.then->clone(),
                                              (*i.el)->clone() },
                                          line);
                                  } else {
                                      return std::make_unique<Expression>(
                                          IfExpression {
                                              i.condition->clone(),
                                              i.then->clone(),
                                              std::nullopt },
                                          line);
                                  }
                              } },
            as);
    };
};
