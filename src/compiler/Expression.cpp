
#include "Expression.h"
#include "Token.h"
#include "Visit.h"
#include <cstddef>
#include <iostream>
#include <memory>
#include <optional>
#include <sstream>
#include <string>
#include <variant>

Expression::Expression(Expression::ExpressionVariant as, int line)
    : as { as }
    , line { line }
{
}
sExpression::sExpression(std::vector<std::shared_ptr<Expression>> elems)
    : elements(std::move(elems))
{
}

BeginExpression::BeginExpression(std::vector<std::shared_ptr<Expression>> body)
    : body(std::move(body))
{
}

CondExpression::CondExpression(
    std::vector<std::pair<std::shared_ptr<Expression>, std::shared_ptr<Expression>>> conditions,
    std::optional<std::shared_ptr<Expression>> elseCond)
    : conditions(std::move(conditions))
    , elseCond(std::move(elseCond))
{
}
SetExpression::SetExpression(const Token& identifier, std::shared_ptr<Expression> value)
    : identifier(std::move(identifier))
    , value(std::move(value))
{
}
DefineSyntaxExpression::DefineSyntaxExpression(Token name, std::shared_ptr<Expression> rule)
    : name(std::move(name))
    , rule(std::move(rule))
{
}
DefineProcedure::DefineProcedure(Token name, std::vector<Token> parameters, std::vector<std::shared_ptr<Expression>> body, bool isVariadic)
    : name(std::move(name))
    , parameters(std::move(parameters))
    , body(std::move(body))
    , isVariadic(isVariadic)
{
}
AtomExpression::AtomExpression(Token token)
    : value(std::move(token))
{
}
LetExpression::LetExpression(std::optional<Token> name, Args arguments, std::vector<std::shared_ptr<Expression>> body)
    : name { std::move(name) }
    , arguments { std::move(arguments) }
    , body { std::move(body) } { };

SyntaxRule::SyntaxRule(std::shared_ptr<Expression> pattern, std::shared_ptr<Expression> template_expr)
    : pattern(std::move(pattern))
    , template_expr(std::move(template_expr)) {
    };

SyntaxRulesExpression::SyntaxRulesExpression(std::vector<Token> literals, std::vector<SyntaxRule> rules)
    : literals(std::move(literals))
    , rules(std::move(rules))
{
}

QuoteExpression::QuoteExpression(std::shared_ptr<Expression> expression)
    : expression(std::move(expression))
{
}

class DefineSyntaxExpression::DefineSyntaxExpression {
public:
    Token name;
    std::shared_ptr<Expression> rule;

    DefineSyntaxExpression(Token name, std::shared_ptr<Expression> rule)
        : name(std::move(name))
        , rule(std::move(rule))
    {
    }
};
DefineExpression::DefineExpression(Token n, std::shared_ptr<Expression> v)
    : name(std::move(n))
    , value(std::move(v))
{
}

TailExpression::TailExpression(std::shared_ptr<Expression> expression)
    : expression(expression)
{
}
ImportExpression::ImportExpression(std::vector<Token> import)
    : import { import } { };

ListExpression::ListExpression(std::vector<std::shared_ptr<Expression>> elems, bool variadic)
    : elements(std::move(elems))
    , isVariadic(variadic)
{
}
LambdaExpression::LambdaExpression(std::vector<Token> parameters, std::vector<std::shared_ptr<Expression>> body, bool isVariadic)
    : parameters(std::move(parameters))
    , body(std::move(body))
    , isVariadic(isVariadic)
{
}
VectorExpression::VectorExpression(std::vector<std::shared_ptr<Expression>> elems)
    : elements(std::move(elems))
{
}
IfExpression::IfExpression(std::shared_ptr<Expression> condition, std::shared_ptr<Expression> then, std::optional<std::shared_ptr<Expression>> el)
    : condition(std::move(condition))
    , then(std::move(then))
    , el(std::move(el))
{
}

std::string Expression::toString() const
{
    std::stringstream ss;
    toString(ss);
    return ss.str();
}
void Expression::toString(std::stringstream& ss) const
{
    std::visit(
        overloaded {
            [&](const AtomExpression& e) {
                ss << e.value.lexeme;
            },
            [&](const sExpression& e) {
                ss << "(";
                for (size_t i = 0; i < e.elements.size(); ++i) {
                    e.elements[i]->toString(ss);
                    if (i < e.elements.size() - 1)
                        ss << " ";
                }
                ss << ")";
            },
            [&](const ListExpression& e) {
                ss << "(";
                for (size_t i = 0; i < e.elements.size(); ++i) {
                    e.elements[i]->toString(ss);
                    if (i < e.elements.size() - 1)
                        ss << " ";
                }
                ss << ")";
            },
            [&](const DefineExpression& e) {
                ss << "(define " << e.name.lexeme << " ";
                e.value->toString(ss);
                ss << ")";
            },
            [&](const DefineProcedure& e) {
                ss << "(define (" << e.name.lexeme;
                for (const auto& param : e.parameters) {
                    ss << " " << param.lexeme;
                }
                ss << ") ";
                for (const auto& expr : e.body) {
                    expr->toString(ss);
                }
                ss << ")";
            },
            [&](const VectorExpression& e) {
                ss << "#(";
                for (size_t i = 0; i < e.elements.size(); ++i) {
                    e.elements[i]->toString(ss);
                    if (i < e.elements.size() - 1)
                        ss << " ";
                }
                ss << ")";
            },
            [&](const LambdaExpression& e) {
                ss << "(lambda (";
                for (size_t i = 0; i < e.parameters.size(); ++i) {
                    ss << e.parameters[i].lexeme;
                    if (i < e.parameters.size() - 1)
                        ss << " ";
                }
                ss << ") ";
                for (size_t i = 0; i < e.body.size(); ++i) {
                    e.body[i]->toString(ss);
                    if (i < e.body.size() - 1)
                        ss << " ";
                }
                ss << ")";
            },
            [&](const IfExpression& e) {
                ss << "(if ";
                e.condition->toString(ss);
                ss << " ";
                e.then->toString(ss);
                if (e.el) {
                    ss << " ";
                    (*e.el)->toString(ss);
                }
                ss << ")";
            },
            [&](const QuoteExpression& e) {
                ss << "'";
                e.expression->toString(ss);
            },
            [&](const SetExpression& e) {
                ss << "(set! " << e.identifier.lexeme << " ";
                e.value->toString(ss);
                ss << ")";
            },
            [&](const TailExpression& e) {
                ss << "(tail ";
                e.expression->toString(ss);
                ss << ")";
            },
            [&](const ImportExpression& e) {
                ss << "(import";
                for (const auto& module : e.import) {
                    ss << " " << module.lexeme;
                }
                ss << ")";
            },
            [&](const SyntaxRulesExpression& e) {
                ss << "(syntax-rules (";
                for (size_t i = 0; i < e.literals.size(); ++i) {
                    ss << e.literals[i].lexeme;
                    if (i < e.literals.size() - 1)
                        ss << " ";
                }
                ss << ") ";
                for (const auto& rule : e.rules) {
                    ss << "(";
                    rule.pattern->toString(ss);
                    ss << " ";
                    rule.template_expr->toString(ss);
                    ss << ") ";
                }
                ss << ")";
            },
            [&](const DefineSyntaxExpression& e) {
                ss << "(define-syntax " << e.name.lexeme << " ";
                e.rule->toString(ss);
                ss << ")";
            },
            [&](const BeginExpression& e) {
                ss << "(begin ";
                for (size_t i = 0; i < e.body.size(); ++i) {
                    e.body[i]->toString(ss);
                    if (i < e.body.size() - 1)
                        ss << " ";
                }
                ss << ")";
            },
            [&](const CondExpression& e) {
                ss << "(cond ";
                for (const auto& [condition, expr] : e.conditions) {
                    ss << "(";
                    condition->toString(ss);
                    ss << " ";
                    expr->toString(ss);
                    ss << ") ";
                }
                if (e.elseCond) {
                    ss << "(else ";
                    (*e.elseCond)->toString(ss);
                    ss << ") ";
                }
                ss << ")";
            },
            [&](const LetExpression& e) {
                ss << "(let ";
                if (e.name) {
                    ss << e.name->lexeme << " ";
                }
                ss << "(";
                for (size_t i = 0; i < e.arguments.size(); ++i) {
                    ss << "(" << e.arguments[i].first.lexeme << " ";
                    e.arguments[i].second->toString(ss);
                    ss << ")";
                    if (i < e.arguments.size() - 1)
                        ss << " ";
                }
                ss << ") ";
                for (size_t i = 0; i < e.body.size(); ++i) {
                    e.body[i]->toString(ss);
                    if (i < e.body.size() - 1)
                        ss << " ";
                }
                ss << ")";
            } },
        as);
}
std::shared_ptr<Expression> Expression::clone() const
{
    return std::visit(overloaded {
                          [&](const LetExpression& p) -> std::shared_ptr<Expression> {
                              LetExpression::Args output;
                              for (const auto& [first, second] : p.arguments) {
                                  output.push_back({ first, second->clone() });
                              }
                              std::vector<std::shared_ptr<Expression>> clonedBody;
                              for (const auto& expr : p.body) {
                                  clonedBody.push_back(expr->clone());
                              }
                              return std::make_shared<Expression>(Expression {
                                  LetExpression {
                                      p.name, output, clonedBody },
                                  line });
                          },
                          [&](const CondExpression& c) -> std::shared_ptr<Expression> {
                              std::vector<std::pair<std::shared_ptr<Expression>, std::shared_ptr<Expression>>> body = {};
                              for (const auto& [first, second] : c.conditions) {
                                  body.push_back({ first->clone(), second->clone() });
                              }
                              std::optional<std::shared_ptr<Expression>> elseClone = std::nullopt;
                              if (c.elseCond) {
                                  elseClone = (*c.elseCond)->clone();
                              }

                              return std::make_shared<Expression>(
                                  CondExpression {
                                      std::move(body),
                                      elseClone },
                                  line);
                          },

                          [&](const BeginExpression& b) -> std::shared_ptr<Expression> {
                              std::vector<std::shared_ptr<Expression>> body = {};
                              for (const auto& first : b.body) {
                                  body.push_back(first->clone());
                              }
                              return std::make_shared<Expression>(
                                  BeginExpression {
                                      std::move(body),
                                  },
                                  line);
                          },
                          [&](const SyntaxRulesExpression& s) -> std::shared_ptr<Expression> {
                              std::vector<Token> clonedLiterals = s.literals; // Tokens can be copied directly
                              std::vector<SyntaxRule> clonedRules;

                              for (const auto& rule : s.rules) {
                                  clonedRules.push_back(SyntaxRule(rule.pattern->clone(), rule.template_expr->clone()));
                              }

                              return std::make_shared<Expression>(
                                  SyntaxRulesExpression {
                                      std::move(clonedLiterals),
                                      std::move(clonedRules) },
                                  line);
                          },
                          [&](const DefineSyntaxExpression& d) -> std::shared_ptr<Expression> {
                              return std::make_shared<Expression>(
                                  DefineSyntaxExpression {
                                      d.name,
                                      d.rule->clone() },
                                  line);
                          },

                          [&](const SetExpression& d) -> std::shared_ptr<Expression> {
                              return std::make_shared<Expression>(
                                  SetExpression {
                                      d.identifier,
                                      d.value->clone() },
                                  line);
                          },
                          [&](const ImportExpression& i) -> std::shared_ptr<Expression> {
                              return std::make_shared<Expression>(ImportExpression { i.import }, line);
                          },
                          [&](const TailExpression& e) -> std::shared_ptr<Expression> {
                              return std::make_shared<Expression>(TailExpression { e.expression->clone() }, line);
                          },
                          [&](const AtomExpression& e) -> std::shared_ptr<Expression> {
                              return std::make_shared<Expression>(AtomExpression { e.value }, line);
                          },
                          [&](const ListExpression& e) -> std::shared_ptr<Expression> {
                              std::vector<std::shared_ptr<Expression>> clonedElements;
                              clonedElements.reserve(e.elements.size());
                              for (const auto& elem : e.elements) {
                                  clonedElements.push_back(elem->clone());
                              }
                              return std::make_shared<Expression>(ListExpression { std::move(clonedElements), e.isVariadic }, line);
                          },
                          [&](const QuoteExpression& e) -> std::shared_ptr<Expression> {
                              return std::make_shared<Expression>(QuoteExpression { e.expression->clone() }, line);
                          },
                          [&](const VectorExpression& e) -> std::shared_ptr<Expression> {
                              std::vector<std::shared_ptr<Expression>> clonedElements;
                              clonedElements.reserve(e.elements.size());
                              for (const auto& elem : e.elements) {
                                  clonedElements.push_back(elem->clone());
                              }
                              return std::make_shared<Expression>(VectorExpression { std::move(clonedElements) }, line);
                          },
                          [&](const sExpression& e) -> std::shared_ptr<Expression> {
                              std::vector<std::shared_ptr<Expression>> clonedElements;
                              clonedElements.reserve(e.elements.size());
                              for (const auto& elem : e.elements) {
                                  clonedElements.push_back(elem->clone());
                              }
                              return std::make_shared<Expression>(sExpression { std::move(clonedElements) }, line);
                          },
                          [&](const DefineExpression& e) -> std::shared_ptr<Expression> {
                              return std::make_shared<Expression>(
                                  DefineExpression { e.name, e.value->clone() },
                                  line);
                          },
                          [&](const DefineProcedure& e) -> std::shared_ptr<Expression> {
                              std::vector<std::shared_ptr<Expression>> clonedBody;
                              for (const auto& expr : e.body) {
                                  clonedBody.push_back(expr->clone());
                              }
                              return std::make_shared<Expression>(

                                  DefineProcedure { e.name, e.parameters, std::move(clonedBody), e.isVariadic },
                                  line);
                          },
                          [&](const LambdaExpression& l) -> std::shared_ptr<Expression> {
                              std::vector<std::shared_ptr<Expression>> clonedBody;
                              for (const auto& expr : l.body) {
                                  clonedBody.push_back(expr->clone());
                              }
                              return std::make_shared<Expression>(
                                  LambdaExpression { l.parameters, std::move(clonedBody), l.isVariadic },
                                  line);
                          },
                          [&](const IfExpression& i) -> std::shared_ptr<Expression> {
                              if (i.el) {
                                  return std::make_shared<Expression>(
                                      IfExpression {
                                          i.condition->clone(),
                                          i.then->clone(),
                                          (*i.el)->clone() },
                                      line);
                              } else {
                                  return std::make_shared<Expression>(
                                      IfExpression {
                                          i.condition->clone(),
                                          i.then->clone(),
                                          std::nullopt },
                                      line);
                              }
                          } },
        as);
}

void Expression::print(int indent) const
{
    std::string indentation(indent * 2, ' ');
    std::cout << indentation << toString() << std::endl;
}
