#include "ExpressionUtils.h"
#include "Visit.h"

std::shared_ptr<Expression> makeAtom(const std::string& lexeme, Tokentype type)
{
    Token token { type, lexeme, 1, 1 };
    return std::make_shared<Expression>(Expression { AtomExpression { token }, 1 });
}

std::shared_ptr<Expression> exprToList(std::shared_ptr<Expression> expr)
{
    if (!expr)
        return nullptr;

    return std::visit(overloaded {
                          [&](const AtomExpression& e) -> std::shared_ptr<Expression> {
                              return std::make_shared<Expression>(Expression { e, expr->line });
                          },
                          [&](const sExpression& e) -> std::shared_ptr<Expression> {
                              std::vector<std::shared_ptr<Expression>> elements;
                              for (const auto& elem : e.elements) {
                                  elements.push_back(exprToList(elem));
                              }
                              return std::make_shared<Expression>(
                                  Expression { ListExpression { elements, false }, expr->line });
                          },
                          [&](const ListExpression& e) -> std::shared_ptr<Expression> {
                              std::vector<std::shared_ptr<Expression>> elements;
                              for (const auto& elem : e.elements) {
                                  elements.push_back(exprToList(elem));
                              }
                              return std::make_shared<Expression>(
                                  Expression { ListExpression { elements, e.isVariadic }, expr->line });
                          },
                          [&](const DefineExpression& d) -> std::shared_ptr<Expression> {
                              std::vector<std::shared_ptr<Expression>> elements;
                              elements.push_back(makeAtom("define"));
                              elements.push_back(std::make_shared<Expression>(Expression { AtomExpression { d.name }, expr->line }));
                              elements.push_back(exprToList(d.value));
                              return std::make_shared<Expression>(
                                  Expression { ListExpression { elements, false }, expr->line });
                          },
                          [&](const DefineProcedure& d) -> std::shared_ptr<Expression> {
                              std::vector<std::shared_ptr<Expression>> elements;
                              elements.push_back(makeAtom("define"));
                              elements.push_back(std::make_shared<Expression>(Expression { AtomExpression { d.name }, expr->line }));

                              std::vector<std::shared_ptr<Expression>> params;
                              for (const auto& param : d.parameters) {
                                  params.push_back(std::make_shared<Expression>(
                                      Expression { AtomExpression { param }, expr->line }));
                              }
                              elements.push_back(std::make_shared<Expression>(
                                  Expression { ListExpression { params, false }, expr->line }));

                              for (const auto& body_expr : d.body) {
                                  elements.push_back(exprToList(body_expr));
                              }
                              return std::make_shared<Expression>(
                                  Expression { ListExpression { elements, false }, expr->line });
                          },
                          [&](const VectorExpression& v) -> std::shared_ptr<Expression> {
                              std::vector<std::shared_ptr<Expression>> elements;
                              for (const auto& elem : v.elements) {
                                  elements.push_back(exprToList(elem));
                              }
                              return std::make_shared<Expression>(
                                  Expression { VectorExpression { elements }, expr->line });
                          },
                          [&](const IfExpression& i) -> std::shared_ptr<Expression> {
                              std::vector<std::shared_ptr<Expression>> elements;
                              elements.push_back(makeAtom("if"));
                              elements.push_back(exprToList(i.condition));
                              elements.push_back(exprToList(i.then));
                              if (i.el) {
                                  elements.push_back(exprToList(*i.el));
                              }
                              return std::make_shared<Expression>(
                                  Expression { ListExpression { elements, false }, expr->line });
                          },
                          [&](const QuoteExpression& q) -> std::shared_ptr<Expression> {
                              std::vector<std::shared_ptr<Expression>> elements;
                              elements.push_back(makeAtom("quote"));
                              elements.push_back(exprToList(q.expression));
                              return std::make_shared<Expression>(
                                  Expression { ListExpression { elements, false }, expr->line });
                          },
                          [&](const TailExpression& t) -> std::shared_ptr<Expression> {
                              return t.expression ? exprToList(t.expression) : nullptr;
                          },
                          [&](const ImportExpression& i) -> std::shared_ptr<Expression> {
                              std::vector<std::shared_ptr<Expression>> elements;
                              elements.push_back(makeAtom("import"));
                              for (const auto& module : i.import) {
                                  elements.push_back(std::make_shared<Expression>(
                                      Expression { AtomExpression { module }, expr->line }));
                              }
                              return std::make_shared<Expression>(
                                  Expression { ListExpression { elements, false }, expr->line });
                          },
                          [&](const SyntaxRulesExpression& s) -> std::shared_ptr<Expression> {
                              std::vector<std::shared_ptr<Expression>> elements;
                              elements.push_back(makeAtom("syntax-rules"));

                              std::vector<std::shared_ptr<Expression>> literalElements;
                              for (const auto& literal : s.literals) {
                                  literalElements.push_back(std::make_shared<Expression>(
                                      Expression { AtomExpression { literal }, expr->line }));
                              }
                              elements.push_back(std::make_shared<Expression>(
                                  Expression { ListExpression { literalElements, false }, expr->line }));

                              for (const auto& pat : s.pattern) {
                                  elements.push_back(exprToList(pat));
                              }
                              for (const auto& templ : s.template_expr) {
                                  elements.push_back(exprToList(templ));
                              }
                              return std::make_shared<Expression>(
                                  Expression { ListExpression { elements, false }, expr->line });
                          },
                          [&](const DefineSyntaxExpression& d) -> std::shared_ptr<Expression> {
                              std::vector<std::shared_ptr<Expression>> elements;
                              elements.push_back(makeAtom("define-syntax"));
                              elements.push_back(std::make_shared<Expression>(
                                  Expression { AtomExpression { d.name }, expr->line }));
                              elements.push_back(exprToList(d.rule));
                              return std::make_shared<Expression>(
                                  Expression { ListExpression { elements, false }, expr->line });
                          },

                          [&](const SetExpression& se) -> std::shared_ptr<Expression> {
                              std::vector<std::shared_ptr<Expression>> elements;
                              elements.push_back(makeAtom("set!"));
                              elements.push_back(makeAtom(se.identifier.lexeme));
                              elements.push_back(exprToList(se.value));
                              return std::make_shared<Expression>(
                                  Expression { ListExpression { elements, false }, expr->line });
                          },
                          [&](const LetExpression& l) -> std::shared_ptr<Expression> {
                              std::vector<std::shared_ptr<Expression>> elements;
                              elements.push_back(makeAtom("let"));

                              std::vector<std::shared_ptr<Expression>> bindingElements;
                              for (const auto& [name, value] : l.arguments) {
                                  std::vector<std::shared_ptr<Expression>> binding;
                                  binding.push_back(std::make_shared<Expression>(
                                      Expression { AtomExpression { name }, expr->line }));
                                  binding.push_back(exprToList(value));
                                  bindingElements.push_back(std::make_shared<Expression>(
                                      Expression { ListExpression { binding, false }, expr->line }));
                              }
                              elements.push_back(std::make_shared<Expression>(
                                  Expression { ListExpression { bindingElements, false }, expr->line }));

                              for (const auto& body_expr : l.body) {
                                  elements.push_back(exprToList(body_expr));
                              }
                              return std::make_shared<Expression>(
                                  Expression { ListExpression { elements, false }, expr->line });
                          },
                          [&](const LambdaExpression& l) -> std::shared_ptr<Expression> {
                              std::vector<std::shared_ptr<Expression>> elements;
                              elements.push_back(makeAtom("lambda"));

                              std::vector<std::shared_ptr<Expression>> params;
                              for (const auto& param : l.parameters) {
                                  params.push_back(std::make_shared<Expression>(
                                      Expression { AtomExpression { param }, expr->line }));
                              }
                              elements.push_back(std::make_shared<Expression>(
                                  Expression { ListExpression { params, false }, expr->line }));

                              for (const auto& body_expr : l.body) {
                                  elements.push_back(exprToList(body_expr));
                              }
                              return std::make_shared<Expression>(
                                  Expression { ListExpression { elements, false }, expr->line });
                          } },
        expr->as);
}
