#include "ExpressionUtils.h"
#include "Expression.h"
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
                              return expr;
                          },
                          [&](const DefineLibraryExpression& dl) -> std::shared_ptr<Expression> {
                              std::vector<std::shared_ptr<Expression>> elements;
                              elements.push_back(makeAtom("define-library"));

                              // Library Name List
                              std::vector<std::shared_ptr<Expression>> nameElements;
                              for (const auto& part : dl.libraryName) {
                                  nameElements.push_back(exprToList(part)); // Convert each part
                              }
                              elements.push_back(std::make_shared<Expression>(
                                  Expression { ListExpression { nameElements, false }, expr->line }));

                              // Export Clause (if exists)
                              if (!dl.exports.empty()) {
                                  std::vector<std::shared_ptr<Expression>> exportElements;
                                  exportElements.push_back(makeAtom("export"));
                                  for (const auto& exp : dl.exports) {
                                      exportElements.push_back(std::make_shared<Expression>(
                                          Expression { AtomExpression { exp }, expr->line }));
                                  }
                                  elements.push_back(std::make_shared<Expression>(
                                      Expression { ListExpression { exportElements, false }, expr->line }));
                              }

                              if (!dl.imports.empty()) {
                                  std::vector<std::shared_ptr<Expression>> importClauseElements;
                                  importClauseElements.push_back(makeAtom("import"));
                                  // TODO: Iterate through dl.imports and convert each spec back to its list form
                                  // This requires logic similar to the ImportExpression case in reverse.
                                  // Example for one spec (needs full logic):
                                  // if (!dl.imports.empty()) {
                                  //    auto firstSpecConverted = convertImportSpecToList(dl.imports[0], expr->line);
                                  //    importClauseElements.push_back(firstSpecConverted);
                                  // }
                                  elements.push_back(std::make_shared<Expression>(
                                      Expression { ListExpression { importClauseElements, false }, expr->line }));
                              }

                              // Begin Clause (if exists)
                              if (!dl.body.empty()) {
                                  std::vector<std::shared_ptr<Expression>> beginElements;
                                  beginElements.push_back(makeAtom("begin"));
                                  for (const auto& bodyExpr : dl.body) {
                                      // Handle potential TailExpression if reversing DefineProcedure/Lambda inside body
                                      if (auto* tail = std::get_if<TailExpression>(&bodyExpr->as)) {
                                          beginElements.push_back(exprToList(tail->expression));
                                      } else {
                                          beginElements.push_back(exprToList(bodyExpr));
                                      }
                                  }
                                  elements.push_back(std::make_shared<Expression>(
                                      Expression { ListExpression { beginElements, false }, expr->line }));
                              }

                              return std::make_shared<Expression>(
                                  Expression { ListExpression { elements, false }, expr->line });
                          },
                          [&](const QuasiQuoteExpression& e) -> std::shared_ptr<Expression> {
                              std::vector<std::shared_ptr<Expression>> elements;
                              elements.push_back(makeAtom("quasiquote", Tokentype::BACKQUOTE));
                              elements.push_back(exprToList(e.value));
                              return std::make_shared<Expression>(
                                  Expression { ListExpression { elements, false }, expr->line });
                          },
                          [&](const UnquoteExpression& e) -> std::shared_ptr<Expression> {
                              std::vector<std::shared_ptr<Expression>> elements;
                              elements.push_back(makeAtom("unquote", Tokentype::COMMA));
                              elements.push_back(exprToList(e.value));
                              return std::make_shared<Expression>(
                                  Expression { ListExpression { elements, false }, expr->line });
                          },

                          [&](const SpliceExpression& e) -> std::shared_ptr<Expression> {
                              std::vector<std::shared_ptr<Expression>> elements;
                              elements.push_back(makeAtom("unquote-splice", Tokentype::COMMA_AT));
                              elements.push_back(exprToList(e.value));
                              return std::make_shared<Expression>(
                                  Expression { ListExpression { elements, false }, expr->line });
                          },
                          [&](const sExpression& e) -> std::shared_ptr<Expression> {
                              std::vector<std::shared_ptr<Expression>> elements;
                              for (const auto& elem : e.elements) {
                                  elements.push_back(exprToList(elem));
                              }
                              return std::make_shared<Expression>(
                                  Expression { ListExpression { elements, false }, expr->line });
                          },

                          [&](const BeginExpression& e) -> std::shared_ptr<Expression> {
                              std::vector<std::shared_ptr<Expression>> elements;
                              elements.push_back(makeAtom("begin"));
                              for (const auto& elem : e.values) {
                                  elements.push_back(exprToList(elem));
                              }
                              return std::make_shared<Expression>(
                                  Expression { ListExpression {
                                                   elements,
                                                   false },
                                      expr->line });
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

                              std::vector<std::shared_ptr<Expression>> params;
                              params.push_back(std::make_shared<Expression>(Expression { AtomExpression { d.name }, expr->line }));
                              if (d.isVariadic && !d.parameters.empty()) {
                                  for (size_t i = 0; i < d.parameters.size() - 1; i++) {
                                      params.push_back(std::make_shared<Expression>(
                                          Expression { AtomExpression { d.parameters[i] }, expr->line }));
                                  }
                                  params.push_back(makeAtom(".", Tokentype::DOT));
                                  params.push_back(std::make_shared<Expression>(
                                      Expression { AtomExpression { d.parameters.back() }, expr->line }));
                              } else {
                                  for (const auto& param : d.parameters) {
                                      params.push_back(std::make_shared<Expression>(
                                          Expression { AtomExpression { param }, expr->line }));
                                  }
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
                              elements.push_back(makeAtom("#"));
                              for (const auto& elem : v.elements) {
                                  elements.push_back(exprToList(elem));
                              }
                              return std::make_shared<Expression>(
                                  Expression { ListExpression { elements }, expr->line });
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
                              elements.push_back(makeAtom("quote", Tokentype::IDENTIFIER));

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

                              for (const auto& spec : i.imports) {
                                  std::vector<std::shared_ptr<Expression>> specElements;

                                  switch (spec.type) {
                                  case ImportExpression::ImportSet::Type::DIRECT: {
                                      specElements = spec.library;
                                      break;
                                  }
                                  case ImportExpression::ImportSet::Type::ONLY: {
                                      specElements.push_back(makeAtom("only"));
                                      specElements.push_back(std::make_shared<Expression>(
                                          Expression { ListExpression { spec.library, false }, expr->line }));
                                      for (const auto& id : spec.identifiers) {
                                          specElements.push_back(makeAtom(id.token.lexeme));
                                      }
                                      break;
                                  }
                                  case ImportExpression::ImportSet::Type::EXCEPT: {
                                      specElements.push_back(makeAtom("except"));
                                      specElements.push_back(std::make_shared<Expression>(
                                          Expression { ListExpression { spec.library, false }, expr->line }));
                                      for (const auto& id : spec.identifiers) {
                                          specElements.push_back(makeAtom(id.token.lexeme));
                                      }
                                      break;
                                  }
                                  case ImportExpression::ImportSet::Type::PREFIX: {
                                      specElements.push_back(makeAtom("prefix"));
                                      specElements.push_back(std::make_shared<Expression>(
                                          Expression { ListExpression { spec.library, false }, expr->line }));
                                      specElements.push_back(makeAtom(spec.prefix.token.lexeme));
                                      break;
                                  }
                                  case ImportExpression::ImportSet::Type::RENAME: {
                                      specElements.push_back(makeAtom("rename"));
                                      specElements.push_back(std::make_shared<Expression>(
                                          Expression { ListExpression { spec.library, false }, expr->line }));
                                      for (const auto& [old_name, new_name] : spec.renames) {
                                          std::vector<std::shared_ptr<Expression>> renameElements;
                                          renameElements.push_back(makeAtom(old_name.token.lexeme));
                                          renameElements.push_back(makeAtom(new_name.token.lexeme));
                                          specElements.push_back(std::make_shared<Expression>(
                                              Expression { ListExpression { renameElements, false }, expr->line }));
                                      }
                                      break;
                                  }
                                  }

                                  if (spec.type != ImportExpression::ImportSet::Type::DIRECT) {
                                      elements.push_back(std::make_shared<Expression>(
                                          Expression { ListExpression { specElements, false }, expr->line }));
                                  } else {
                                      elements.push_back(std::make_shared<Expression>(
                                          Expression { ListExpression { specElements, false }, expr->line }));
                                  }
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

                              for (const auto& rule : s.rules) {
                                  std::vector<std::shared_ptr<Expression>> ruleElements;
                                  ruleElements.push_back(exprToList(rule.pattern));
                                  ruleElements.push_back(exprToList(rule.template_expr));
                                  elements.push_back(std::make_shared<Expression>(
                                      Expression { ListExpression { ruleElements, false }, expr->line }));
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
                              elements.push_back(makeAtom(se.identifier.token.lexeme));
                              elements.push_back(exprToList(se.value));
                              return std::make_shared<Expression>(
                                  Expression { ListExpression { elements, false }, expr->line });
                          },
                          [&](const LetExpression& l) -> std::shared_ptr<Expression> {
                              std::vector<std::shared_ptr<Expression>> elements;
                              elements.push_back(makeAtom("let"));
                              if (l.name.has_value()) {
                                  elements.push_back(makeAtom(l.name.value().token.lexeme));
                              }
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
                              if (l.isVariadic && !l.parameters.empty()) {
                                  for (size_t i = 0; i < l.parameters.size() - 1; i++) {
                                      params.push_back(std::make_shared<Expression>(
                                          Expression { AtomExpression { l.parameters[i] }, expr->line }));
                                  }
                                  params.push_back(makeAtom(".", Tokentype::DOT));
                                  params.push_back(std::make_shared<Expression>(
                                      Expression { AtomExpression { l.parameters.back() }, expr->line }));
                              } else {
                                  for (const auto& param : l.parameters) {
                                      params.push_back(std::make_shared<Expression>(
                                          Expression { AtomExpression { param }, expr->line }));
                                  }
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
