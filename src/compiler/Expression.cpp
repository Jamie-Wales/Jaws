
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
DefineProcedure::DefineProcedure(Token name, std::vector<Token> parameters, std::vector<std::shared_ptr<Expression>> body)
    : name(std::move(name))
    , parameters(std::move(parameters))
    , body(std::move(body))
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

SyntaxRulesExpression::SyntaxRulesExpression(
    std::vector<Token> literals,
    std::vector<std::shared_ptr<Expression>> pattern,
    std::vector<std::shared_ptr<Expression>> template_expr)
    : literals(std::move(literals))
    , pattern(std::move(pattern))
    , template_expr(std::move(template_expr))
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
LambdaExpression::LambdaExpression(std::vector<Token> parameters, std::vector<std::shared_ptr<Expression>> body)
    : parameters(std::move(parameters))
    , body(std::move(body))
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
    std::visit(overloaded {

                   [&](const SetExpression& s) {
                       ss << "(set! " << std::endl;
                       ss << s.identifier.lexeme << " " << s.value->toString() << ")" << std::endl;
                   },
                   [&](const LetExpression& l) {
                       ss << "(let (";
                       for (const auto& [first, second] : l.arguments) {
                           ss << first.lexeme << " ";
                           second->toString(ss);
                       }
                       ss << ")";
                       for (const auto& ref : l.body) {
                           ref->toString(ss);
                       }
                       ss << ")";
                   },
                   [&](const AtomExpression& e) {
                       ss << e.value.lexeme;
                   },
                   [&](const SyntaxRulesExpression& s) {
                       ss << "(syntax-rules (";
                       bool first = true;
                       for (const auto& lit : s.literals) {
                           if (!first)
                               ss << " ";
                           ss << lit.lexeme;
                           first = false;
                       }
                       ss << ") ";
                       for (const auto& pat : s.pattern) {
                           pat->toString(ss);
                           ss << " ";
                       }
                       for (const auto& temp : s.template_expr) {
                           temp->toString(ss);
                           ss << " ";
                       }
                       ss << ")";
                   },
                   [&](const DefineSyntaxExpression& d) {
                       ss << "(define-syntax " << d.name.lexeme << " ";
                       d.rule->toString(ss);
                       ss << ")";
                   },
                   [&](const ImportExpression& i) {
                       ss << "(import ";
                       for (auto& tok : i.import) {
                           ss << tok.lexeme << " ";
                       }
                       ss << ")";
                   },
                   [&](const TailExpression& e) {
                       e.expression->toString(ss);
                   },
                   [&](const ListExpression& e) {
                       ss << "(";
                       bool first = true;
                       for (const auto& elem : e.elements) {
                           if (!first)
                               ss << " ";
                           elem->toString(ss);
                           first = false;
                       }
                       ss << ")";
                   },
                   [&](const IfExpression& i) {
                       ss << "(if ";
                       i.condition->toString(ss);
                       ss << " ";
                       i.then->toString(ss);
                       if (i.el) {
                           ss << " ";
                           (*i.el)->toString(ss);
                       }
                       ss << ")";
                   },
                   [&](const VectorExpression& e) {
                       ss << "#(";
                       bool first = true;
                       for (const auto& elem : e.elements) {
                           if (!first)
                               ss << " ";
                           elem->toString(ss);
                           first = false;
                       }
                       ss << ")";
                   },
                   [&](const sExpression& s) {
                       ss << "(";
                       bool first = true;
                       for (const auto& elem : s.elements) {
                           if (!first)
                               ss << " ";
                           elem->toString(ss);
                           first = false;
                       }
                       ss << ")";
                   },
                   [&](const QuoteExpression& e) {
                       ss << "'";
                       e.expression->toString(ss);
                   },
                   [&](const DefineExpression& d) {
                       ss << "(define " << d.name.lexeme << " ";
                       d.value->toString(ss);
                       ss << ")";
                   },
                   [&](const DefineProcedure& d) {
                       ss << "(define " << d.name.lexeme << " (";
                       bool first = true;
                       for (const auto& param : d.parameters) {
                           if (!first)
                               ss << " ";
                           ss << param.lexeme;
                           first = false;
                       }
                       ss << ") ";
                       first = true;
                       for (const auto& expr : d.body) {
                           if (!first)
                               ss << " ";
                           expr->toString(ss);
                           first = false;
                       }
                       ss << ")";
                   },
                   [&](const LambdaExpression& l) {
                       ss << "(lambda (";
                       bool first = true;
                       for (const auto& param : l.parameters) {
                           if (!first)
                               ss << " ";
                           ss << param.lexeme;
                           first = false;
                       }
                       ss << ") ";
                       first = true;
                       for (const auto& expr : l.body) {
                           if (!first)
                               ss << " ";
                           expr->toString(ss);
                           first = false;
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
                              return std::make_shared<Expression>(Expression {
                                  LetExpression {
                                      p.name, output, p.body },
                                  line });
                          },
                          [&](const SyntaxRulesExpression& s) -> std::shared_ptr<Expression> {
                              std::vector<Token> clonedLiterals = s.literals; // Tokens can be copied directly
                              std::vector<std::shared_ptr<Expression>> clonedPattern;
                              std::vector<std::shared_ptr<Expression>> clonedTemplate;

                              for (const auto& pat : s.pattern) {
                                  clonedPattern.push_back(pat->clone());
                              }
                              for (const auto& temp : s.template_expr) {
                                  clonedTemplate.push_back(temp->clone());
                              }

                              return std::make_shared<Expression>(
                                  SyntaxRulesExpression {
                                      std::move(clonedLiterals),
                                      std::move(clonedPattern),
                                      std::move(clonedTemplate) },
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
                              return std::make_shared<Expression>(ListExpression { std::move(clonedElements) }, line);
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
                              return std::make_shared<Expression>(

                                  DefineProcedure { e.name, e.parameters, std::move(e.body) },
                                  line);
                          },
                          [&](const LambdaExpression& l) -> std::shared_ptr<Expression> {
                              return std::make_shared<Expression>(
                                  LambdaExpression { l.parameters, std::move(l.body) },
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
    std::visit(overloaded {

                   [&](const LetExpression& l) {
                       std::cout << indentation << "(" << std::endl;
                       std::cout << "let" << std::endl;
                       std::cout << indentation << "  (";
                       for (const auto& [first, second] : l.arguments) {
                           std::cout << first.lexeme << " ";
                           second->print(indent);
                       }
                       for (const auto& ref : l.body) {
                           ref->print(indent * 2);
                       }
                       std::cout << ")" << std::endl;
                   },
                   [&](const SyntaxRulesExpression& s) {
                       std::cout << this->toString() << std::endl;
                   },
                   [&](const DefineSyntaxExpression& d) {
                       std::cout << indentation << "(define-syntax " << d.name.lexeme << std::endl;
                       d.rule->print(indent + 2); // Changed from rules to rule
                       std::cout << indentation << ")" << std::endl;
                   },
                   [&](const SetExpression& d) {
                       std::cout << indentation << "(set!"
                                 << d.identifier.lexeme << std::endl;
                       d.value->print(indent + 2);
                       std::cout << indentation << ")" << std::endl;
                   },
                   [&](const ImportExpression& i) {
                       std::cout << "(import ";
                       for (auto& tok : i.import)
                           std::cout << tok.lexeme;
                       std::cout << std::endl;
                   },
                   [&](const TailExpression& e) {
                       std::cout << indentation;
                       e.expression->print();
                       std::cout << std::endl;
                   },
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
                       std::cout << indentation << "(if" << std::endl;
                       i.condition->print(indent + 1);
                       std::cout << indentation << "then" << std::endl;
                       i.then->print(indent + 1);
                       if (i.el) {
                           std::cout << indentation << "else" << std::endl;
                           (*i.el)->print(indent + 1);
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
                   [&](const QuoteExpression& e) {
                       std::cout << indentation << "(quote " << std::endl;
                       e.expression->print(indent + 1);
                       std::cout << indentation << ")" << std::endl;
                   },
                   [&](const DefineExpression& d) {
                       std::cout << indentation << std::format("(define {}", d.name.lexeme) << std::endl;
                       d.value->print(indent + 1);
                       std::cout << indentation << ")" << std::endl;
                   },
                   [&](const DefineProcedure& d) {
                       std::cout << indentation << std::format("(define {}", d.name.lexeme) << std::endl;
                       for (auto& ele : d.body) {
                           ele->print();
                       }
                       std::cout << indentation << ")" << std::endl;
                   },
                   [&](const LambdaExpression& l) {
                       std::cout << indentation << "(lambda " << std::endl;
                       for (auto& ele : l.body) {
                           ele->print();
                       }
                       std::cout << indentation << ")" << std::endl;
                   },
               },
        as);
}
