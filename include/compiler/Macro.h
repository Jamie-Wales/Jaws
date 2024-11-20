#pragma once
#include "Expression.h"
#include <iostream>
#include <memory>
#include <unordered_map>

#define MACRO_DEBUG

#ifdef MACRO_DEBUG
#define DEBUG_PRINT(msg) std::cout << msg
#define DEBUG_EXPR(expr) expr->print()
#else
#define DEBUG_PRINT(msg)
#define DEBUG_EXPR(expr)
#endif

class MacroExpander {
private:
    struct MacroDefinition {
        Token name;
        std::shared_ptr<Expression> pattern;
        std::shared_ptr<Expression> templ;
    };

    struct Environment {
        std::string macroName;
        std::unordered_map<std::string, std::vector<std::shared_ptr<Expression>>> bindings;
        Environment(std::string name)
            : macroName(std::move(name))
        {
        }
    };

    std::unordered_map<std::string, MacroDefinition> macros;

    bool isEllipsis(const std::shared_ptr<Expression>& expr)
    {
        return std::get_if<AtomExpression>(&expr->as) && std::get<AtomExpression>(expr->as).value.lexeme == "...";
    }

    bool isWildcard(const AtomExpression& atom)
    {
        return atom.value.type == Tokentype::IDENTIFIER && atom.value.lexeme == "_";
    }

    bool matchPattern(const std::shared_ptr<Expression>& pattern,
        const std::shared_ptr<Expression>& expr,
        Environment& env)
    {
        DEBUG_PRINT("\nMatching pattern:\n");
        DEBUG_EXPR(pattern);
        DEBUG_PRINT("Against expression:\n");
        DEBUG_EXPR(expr);

        return std::visit(overloaded {
                              [&](const AtomExpression& p) -> bool {
                                  DEBUG_PRINT("Atom pattern: " << p.value.lexeme << "\n");

                                  // Handle wildcards
                                  if (isWildcard(p))
                                      return true;

                                  if (p.value.type == Tokentype::IDENTIFIER) {
                                      // Handle macro name matching
                                      if (p.value.lexeme == env.macroName) {
                                          if (auto atom = std::get_if<AtomExpression>(&expr->as)) {
                                              return p.value.lexeme == atom->value.lexeme;
                                          }
                                          return false;
                                      }

                                      // Handle pattern variable binding
                                      DEBUG_PRINT("Binding " << p.value.lexeme << " to:\n");
                                      DEBUG_EXPR(expr);
                                      env.bindings[p.value.lexeme].push_back(expr);
                                      return true;
                                  }

                                  // Handle literal matching
                                  if (auto atom = std::get_if<AtomExpression>(&expr->as)) {
                                      return p.value.lexeme == atom->value.lexeme;
                                  }
                                  return false;
                              },
                              [&](const sExpression& p) -> bool {
                                  if (auto sexp = std::get_if<sExpression>(&expr->as)) {
                                      // Handle ellipsis patterns
                                      if (p.elements.size() >= 2 && isEllipsis(p.elements[1])) {
                                          for (const auto& element : sexp->elements) {
                                              if (!matchPattern(p.elements[0], element, env)) {
                                                  return false;
                                              }
                                          }
                                          return true;
                                      }

                                      // Regular pattern matching
                                      if (p.elements.size() != sexp->elements.size())
                                          return false;

                                      for (size_t i = 0; i < p.elements.size(); i++) {
                                          if (!matchPattern(p.elements[i], sexp->elements[i], env)) {
                                              return false;
                                          }
                                      }
                                      return true;
                                  }
                                  return false;
                              },
                              [&](const TailExpression& t) -> bool {
                                  return matchPattern(t.expression, expr, env);
                              },
                              [](const auto&) -> bool {
                                  return false;
                              } },
            pattern->as);
    }

    std::shared_ptr<Expression> expandTemplate(
        const std::shared_ptr<Expression>& templ,
        const Environment& env)
    {
        DEBUG_PRINT("\nExpanding template:\n");
        DEBUG_EXPR(templ);

        return std::visit(overloaded {
                              [&](const AtomExpression& t) -> std::shared_ptr<Expression> {
                                  if (t.value.type == Tokentype::IDENTIFIER) {
                                      auto it = env.bindings.find(t.value.lexeme);
                                      if (it != env.bindings.end() && !it->second.empty()) {
                                          return it->second.front()->clone();
                                      }
                                  }
                                  return std::make_shared<Expression>(t, templ->line);
                              },
                              [&](const sExpression& t) -> std::shared_ptr<Expression> {
                                  // Handle ellipsis expansion
                                  if (t.elements.size() >= 2 && isEllipsis(t.elements[1])) {
                                      if (auto firstAtom = std::get_if<AtomExpression>(&t.elements[0]->as)) {
                                          auto it = env.bindings.find(firstAtom->value.lexeme);
                                          if (it != env.bindings.end()) {
                                              std::vector<std::shared_ptr<Expression>> expanded;
                                              for (const auto& binding : it->second) {
                                                  expanded.push_back(binding->clone());
                                              }
                                              return std::make_shared<Expression>(
                                                  sExpression { std::move(expanded) }, templ->line);
                                          }
                                      }
                                  }

                                  // Regular expansion
                                  std::vector<std::shared_ptr<Expression>> expanded;
                                  for (const auto& elem : t.elements) {
                                      expanded.push_back(expandTemplate(elem, env));
                                  }
                                  return std::make_shared<Expression>(
                                      sExpression { std::move(expanded) }, templ->line);
                              },
                              [&](const TailExpression& t) -> std::shared_ptr<Expression> {
                                  return std::make_shared<Expression>(
                                      TailExpression { expandTemplate(t.expression, env) },
                                      templ->line);
                              },
                              [&](const IfExpression& i) -> std::shared_ptr<Expression> {
                                  return std::make_shared<Expression>(
                                      IfExpression {
                                          expandTemplate(i.condition, env),
                                          expandTemplate(i.then, env),
                                          i.el ? std::optional { expandTemplate(*i.el, env) } : std::nullopt },
                                      templ->line);
                              },
                              [&](const auto&) -> std::shared_ptr<Expression> {
                                  return templ->clone();
                              } },
            templ->as);
    }

    void collectMacros(const std::vector<std::shared_ptr<Expression>>& ast)
    {
        for (const auto& expr : ast) {
            collectMacrosInExpr(expr);
        }
    }

    void collectMacrosInExpr(const std::shared_ptr<Expression>& expr)
    {
        if (auto define = std::get_if<DefineSyntaxExpression>(&expr->as)) {
            if (auto rule = std::get_if<SyntaxRuleExpression>(&define->rule->as)) {
                DEBUG_PRINT("Found syntax rule for: " << define->name.lexeme << "\n");
                macros[define->name.lexeme] = MacroDefinition {
                    define->name,
                    rule->pattern,
                    rule->template_expr
                };
            }
        }

        // Recursively collect macros in nested expressions
        std::visit(overloaded {
                       [&](const sExpression& s) {
                           for (const auto& elem : s.elements) {
                               collectMacrosInExpr(elem);
                           }
                       },
                       [&](const TailExpression& t) {
                           collectMacrosInExpr(t.expression);
                       },
                       [&](const ListExpression& l) {
                           for (const auto& elem : l.elements) {
                               collectMacrosInExpr(elem);
                           }
                       },
                       [](const auto&) {} },
            expr->as);
    }

    std::shared_ptr<Expression> expandExpr(const std::shared_ptr<Expression>& expr)
    {
        if (auto sexp = std::get_if<sExpression>(&expr->as)) {
            // Try to expand as macro
            if (!sexp->elements.empty()) {
                if (auto first = std::get_if<AtomExpression>(&sexp->elements[0]->as)) {
                    auto it = macros.find(first->value.lexeme);
                    if (it != macros.end()) {
                        Environment env(it->second.name.lexeme);
                        if (matchPattern(it->second.pattern, expr, env)) {
                            DEBUG_PRINT("Expanding macro: " << first->value.lexeme << "\n");
                            return expandTemplate(it->second.templ, env);
                        }
                    }
                }
            }

            // If not a macro, expand children
            std::vector<std::shared_ptr<Expression>> expanded;
            for (const auto& elem : sexp->elements) {
                expanded.push_back(expandExpr(elem));
            }
            return std::make_shared<Expression>(
                sExpression { std::move(expanded) }, expr->line);
        }

        return expr->clone();
    }

public:
    std::optional<std::vector<std::shared_ptr<Expression>>> expand(
        const std::optional<std::vector<std::shared_ptr<Expression>>>& ast)
    {
        if (!ast)
            return std::nullopt;

        collectMacros(*ast);

        std::vector<std::shared_ptr<Expression>> expanded;
        for (const auto& expr : *ast) {
            if (!std::get_if<DefineSyntaxExpression>(&expr->as)) {
                expanded.push_back(expandExpr(expr));
            }
        }

        return expanded;
    }
};
