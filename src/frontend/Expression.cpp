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

sExpression::sExpression(std::vector<std::shared_ptr<Expression>> elems, bool variadic)
    : elements(std::move(elems))
    , isVariadic(variadic)
{
}

BeginExpression::BeginExpression(std::vector<std::shared_ptr<Expression>> body)
    : body(std::move(body))
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

DefineProcedure::DefineProcedure(Token name, std::vector<Token> parameters,
    std::vector<std::shared_ptr<Expression>> body, bool isVariadic)
    : name(std::move(name))
    , parameters(std::move(parameters))
    , body(std::move(body))
    , isVariadic(isVariadic)
{
}

MacroAtomExpression::MacroAtomExpression(Token token, bool isVariadic)
    : value(std::move(token))
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
    , body { std::move(body) } {
    };

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

DefineExpression::DefineExpression(Token n, std::shared_ptr<Expression> v)
    : name(std::move(n))
    , value(std::move(v))
{
}

TailExpression::TailExpression(std::shared_ptr<Expression> expression)
    : expression(expression)
{
}

#include <cassert>

// ImportSpec constructors
ImportExpression::ImportSpec::ImportSpec(std::vector<std::shared_ptr<Expression>> lib)
    : type(ImportSet::Type::DIRECT)
    , library(std::move(lib))
{
}

ImportExpression::ImportSpec::ImportSpec(ImportSet::Type t,
    std::vector<std::shared_ptr<Expression>> lib,
    std::vector<Token> ids)
    : type(t)
    , library(std::move(lib))
    , identifiers(std::move(ids))
{
    assert(t == ImportSet::Type::ONLY || t == ImportSet::Type::EXCEPT);
}

ImportExpression::ImportSpec::ImportSpec(std::vector<std::shared_ptr<Expression>> lib,
    Token pfx)
    : type(ImportSet::Type::PREFIX)
    , library(std::move(lib))
    , prefix(std::move(pfx))
{
}

ImportExpression::ImportSpec::ImportSpec(std::vector<std::shared_ptr<Expression>> lib,
    std::vector<std::pair<Token, Token>> renames_)
    : type(ImportSet::Type::RENAME)
    , library(std::move(lib))
    , renames(std::move(renames_))
{
}

// ImportExpression constructor
ImportExpression::ImportExpression(std::vector<ImportSpec> imports_)
    : imports(std::move(imports_))
{
}

ImportExpression::ImportSpec makeDirectImport(std::vector<std::shared_ptr<Expression>> library)
{
    return ImportExpression::ImportSpec(std::move(library));
}

ImportExpression::ImportSpec makeOnlyImport(
    std::vector<std::shared_ptr<Expression>> library,
    std::vector<Token> identifiers)
{
    return ImportExpression::ImportSpec(
        ImportExpression::ImportSet::Type::ONLY,
        std::move(library),
        std::move(identifiers));
}

ImportExpression::ImportSpec makeExceptImport(
    std::vector<std::shared_ptr<Expression>> library,
    std::vector<Token> identifiers)
{
    return ImportExpression::ImportSpec(
        ImportExpression::ImportSet::Type::EXCEPT,
        std::move(library),
        std::move(identifiers));
}

ImportExpression::ImportSpec makePrefixImport(
    std::vector<std::shared_ptr<Expression>> library,
    Token prefix)
{
    return ImportExpression::ImportSpec(std::move(library), std::move(prefix));
}

ImportExpression::ImportSpec makeRenameImport(
    std::vector<std::shared_ptr<Expression>> library,
    std::vector<std::pair<Token, Token>> renames)
{
    return ImportExpression::ImportSpec(std::move(library), std::move(renames));
}

ImportExpression::ImportSpec::ImportSpec(const ImportSpec& other)
    : type(other.type)
    , library(other.library) // shared_ptr copy
    , identifiers(other.identifiers)
    , prefix(other.prefix)
    , renames(other.renames)
{
}

ListExpression::ListExpression(std::vector<std::shared_ptr<Expression>> elems, bool variadic)
    : elements(std::move(elems))
    , isVariadic(variadic)
{
}

LambdaExpression::LambdaExpression(std::vector<Token> parameters, std::vector<std::shared_ptr<Expression>> body,
    bool isVariadic)
    : parameters(std::move(parameters))
    , body(std::move(body))
    , isVariadic(isVariadic)
{
}

VectorExpression::VectorExpression(std::vector<std::shared_ptr<Expression>> elems)
    : elements(std::move(elems))
{
}

IfExpression::IfExpression(std::shared_ptr<Expression> condition, std::shared_ptr<Expression> then,
    std::optional<std::shared_ptr<Expression>> el)
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
            [&](const MacroAtomExpression& e) {
                ss << e.value.lexeme;
                if (e.isVariadic)
                    ss << "...";
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
                ss << "(quote ";
                e.expression->toString(ss);
                ss << ")";
            },
            [&](const SetExpression& e) {
                ss << "(set! " << e.identifier.lexeme << " ";
                e.value->toString(ss);
                ss << ")";
            },
            [&](const TailExpression& e) {
                e.expression->toString(ss);
            },
            [&](const ImportExpression& e) {
                ss << "(import";
                for (const auto& spec : e.imports) {
                    ss << " ";
                    switch (spec.type) {
                    case ImportExpression::ImportSet::Type::DIRECT: {
                        ss << "(";
                        for (const auto& part : spec.library) {
                            ss << " " << part->toString();
                        }
                        ss << ")";
                        break;
                    }
                    case ImportExpression::ImportSet::Type::ONLY: {
                        ss << "(only (";
                        for (const auto& part : spec.library) {
                            ss << " " << part->toString();
                        }
                        ss << ")";
                        for (const auto& id : spec.identifiers) {
                            ss << " " << id.lexeme;
                        }
                        ss << ")";
                        break;
                    }
                    case ImportExpression::ImportSet::Type::EXCEPT: {
                        ss << "(except (";
                        for (const auto& part : spec.library) {
                            ss << " " << part->toString();
                        }
                        ss << ")";
                        for (const auto& id : spec.identifiers) {
                            ss << " " << id.lexeme;
                        }
                        ss << ")";
                        break;
                    }
                    case ImportExpression::ImportSet::Type::PREFIX: {
                        ss << "(prefix (";
                        for (const auto& part : spec.library) {
                            ss << " " << part->toString();
                        }
                        ss << ") " << spec.prefix.lexeme << ")";
                        break;
                    }
                    case ImportExpression::ImportSet::Type::RENAME: {
                        ss << "(rename (";
                        for (const auto& part : spec.library) {
                            ss << " " << part->toString();
                        }
                        ss << ")";
                        for (const auto& [old_name, new_name] : spec.renames) {
                            ss << " (" << old_name.lexeme << " " << new_name.lexeme << ")";
                        }
                        ss << ")";
                        break;
                    }
                    }
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
                              std::vector<ImportExpression::ImportSpec> importsCopy;
                              for (const auto& spec : i.imports) {
                                  importsCopy.push_back(ImportExpression::ImportSpec(spec)); // Use copy constructor
                              }
                              return std::make_shared<Expression>(
                                  Expression { ImportExpression { std::move(importsCopy) }, line });
                          },
                          [&](const TailExpression& e) -> std::shared_ptr<Expression> {
                              return std::make_shared<Expression>(TailExpression { e.expression->clone() }, line);
                          },
                          [&](const AtomExpression& e) -> std::shared_ptr<Expression> {
                              return std::make_shared<Expression>(AtomExpression { e.value }, line);
                          },

                          [&](const MacroAtomExpression& e) -> std::shared_ptr<Expression> {
                              return std::make_shared<Expression>(MacroAtomExpression { e.value, e.isVariadic }, line);
                          },
                          [&](const ListExpression& e) -> std::shared_ptr<Expression> {
                              std::vector<std::shared_ptr<Expression>> clonedElements;
                              clonedElements.reserve(e.elements.size());
                              for (const auto& elem : e.elements) {
                                  clonedElements.push_back(elem->clone());
                              }
                              return std::make_shared<Expression>(ListExpression {
                                                                      std::move(clonedElements), e.isVariadic },
                                  line);
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
