#pragma once

#include "Expression.h"
#include "ExpressionUtils.h"
#include "Value.h"
#include "Visit.h"
#include "interpret.h"
#include "parse.h"
#include "scan.h"
#include <cassert>
#include <iostream>
#include <memory>
#include <unordered_map>
#include <variant>
#include <vector>

struct MacroExpr;

struct MacroAtom {
    Token token;
    explicit MacroAtom(Token t)
        : token(std::move(t))
    {
    }
};

struct MacroList {
    std::vector<std::shared_ptr<MacroExpr>> elements;
};

struct MacroExpr {
    std::variant<MacroAtom, MacroList> value;
    bool isVariadic = false;
    int line;

    static std::shared_ptr<MacroExpr> fromExpr(const std::shared_ptr<Expression>& expr)
    {
        if (!expr) {
            return nullptr;
        }

        return std::visit(overloaded {
                              [&](const AtomExpression& atom) -> std::shared_ptr<MacroExpr> {
                                  if (atom.value.type == Tokentype::ELLIPSIS) {
                                      return nullptr;
                                  }
                                  return std::make_shared<MacroExpr>(
                                      MacroExpr { MacroAtom { atom.value }, false, expr->line });
                              },

                              [&](const ListExpression& list) -> std::shared_ptr<MacroExpr> {
                                  MacroList macroList;
                                  bool isVariadic = false;

                                  if (!list.elements.empty()) {
                                      auto last = list.elements.back();
                                      if (auto* atom = std::get_if<AtomExpression>(&last->as)) {
                                          if (atom->value.type == Tokentype::ELLIPSIS) {
                                              isVariadic = true;
                                          }
                                      }
                                  }

                                  size_t processElements = list.elements.size() - (isVariadic ? 1 : 0);

                                  for (size_t i = 0; i < processElements; i++) {
                                      if (auto elem = fromExpr(list.elements[i])) {
                                          macroList.elements.push_back(elem);
                                      }
                                  }

                                  return std::make_shared<MacroExpr>(
                                      MacroExpr { std::move(macroList), isVariadic, expr->line });
                              },

                              [](const auto&) -> std::shared_ptr<MacroExpr> {
                                  return nullptr;
                              } },
            expr->as);
    }

    std::shared_ptr<Expression> toExpr() const
    {

        return std::visit(overloaded {
                              [&](const MacroAtom& atom) -> std::shared_ptr<Expression> {
                                  return std::make_shared<Expression>(
                                      Expression { AtomExpression { atom.token }, line });
                              },

                              [&](const MacroList& list) -> std::shared_ptr<Expression> {
                                  std::vector<std::shared_ptr<Expression>> elements;
                                  for (const auto& elem : list.elements) {
                                      if (elem) {
                                          elements.push_back(elem->toExpr());
                                      }
                                  }
                                  return std::make_shared<Expression>(
                                      Expression { ListExpression { elements, isVariadic }, line });
                              } },
            value);
    }
};

struct PatternMatch {
    std::vector<std::shared_ptr<MacroExpr>> matches;
};

using MatchEnv = std::unordered_map<std::string, PatternMatch>;

inline std::shared_ptr<MacroExpr> prepareForMacro(const std::shared_ptr<Expression>& expr)
{
    auto listed = exprToList(expr);
    return MacroExpr::fromExpr(listed);
}

class PatternMatcher {
public:
    static void debugPrintEnv(const MatchEnv& env)
    {
        for (const auto& [key, value] : env) {
            for (const auto& match : value.matches) {
                if (auto expr = match->toExpr()) {
                }
            }
        }
    }

    static std::optional<MatchEnv> match(
        const std::shared_ptr<Expression>& pattern,
        const std::shared_ptr<Expression>& expr,
        const std::vector<Token>& literals)
    {

        auto macroPattern = prepareForMacro(pattern);
        auto macroExpr = prepareForMacro(expr);

        MatchEnv env;
        if (matchExpr(macroPattern, macroExpr, env, literals)) {
            debugPrintEnv(env);
            return env;
        }
        return std::nullopt;
    }

private:
    static bool matchExpr(
        const std::shared_ptr<MacroExpr>& pattern,
        const std::shared_ptr<MacroExpr>& expr,
        MatchEnv& env,
        const std::vector<Token>& literals)
    {
        if (!pattern || !expr) {
            return false;
        }
        if (auto* patAtom = std::get_if<MacroAtom>(&pattern->value)) {
            bool isLiteral = std::find_if(literals.begin(), literals.end(),
                                 [&](const Token& lit) { return lit.lexeme == patAtom->token.lexeme; })
                != literals.end();
            if (isLiteral) {
                if (auto* exprAtom = std::get_if<MacroAtom>(&expr->value)) {
                    return patAtom->token.lexeme == exprAtom->token.lexeme;
                }
                return false;
            }

            if (patAtom->token.lexeme != "_") {
                env[patAtom->token.lexeme].matches.push_back(expr);
                return true;
            }
        }

        return std::visit(overloaded {
                              [&](const MacroAtom& patAtom, const MacroAtom& exprAtom) -> bool {
                                  if (patAtom.token.lexeme == "_") {
                                      return true;
                                  }

                                  bool isLiteral = std::find_if(literals.begin(), literals.end(),
                                                       [&](const Token& lit) { return lit.lexeme == patAtom.token.lexeme; })
                                      != literals.end();

                                  if (isLiteral) {
                                      return patAtom.token.lexeme == exprAtom.token.lexeme;
                                  }

                                  // Non-literal matches anything
                                  env[patAtom.token.lexeme].matches.push_back(expr);
                                  return true;
                              },

                              [&](const MacroList& patList, const MacroList& exprList) -> bool {
                                  if (pattern->isVariadic) {
                                      if (patList.elements.empty()) {
                                          return false;
                                      }

                                      size_t fixedCount = patList.elements.size() - 1;
                                      if (exprList.elements.size() < fixedCount) {
                                          return false;
                                      }
                                      for (size_t i = 0; i < fixedCount; i++) {
                                          if (!matchExpr(patList.elements[i], exprList.elements[i], env, literals)) {
                                              return false;
                                          }
                                      }
                                      if (auto last = patList.elements.back()) {
                                          std::visit(overloaded {
                                                         [&](const MacroAtom& patAtom) {
                                                             auto& matches = env[patAtom.token.lexeme].matches;
                                                             for (size_t i = fixedCount; i < exprList.elements.size(); i++) {
                                                                 matches.push_back(exprList.elements[i]);
                                                             }
                                                         },
                                                         [](const auto&) {} },
                                              last->value);
                                      }

                                      return true;
                                  }
                                  if (patList.elements.size() > 0 && exprList.elements.size() > 0) {
                                      if (auto* firstPatAtom = std::get_if<MacroAtom>(&patList.elements[0]->value)) {
                                          if (std::find_if(literals.begin(), literals.end(),
                                                  [&](const Token& lit) { return lit.lexeme == firstPatAtom->token.lexeme; })
                                              != literals.end()) {
                                              return matchExpr(patList.elements[0], exprList.elements[0], env, literals);
                                          }
                                      }
                                  }
                                  if (patList.elements.size() != exprList.elements.size()) {
                                      return false;
                                  }

                                  for (size_t i = 0; i < patList.elements.size(); i++) {
                                      if (!matchExpr(patList.elements[i], exprList.elements[i], env, literals)) {
                                          return false;
                                      }
                                  }

                                  return true;
                              },

                              [](const auto&, const auto&) -> bool {
                                  return false;
                              } },
            pattern->value, expr->value);
    }
};
class MacroTransformer {
public:
    static std::shared_ptr<Expression> transform(
        const std::shared_ptr<Expression>& templ,
        const MatchEnv& env)
    {
        auto macroTemplate = prepareForMacro(templ);
        auto result = transformMacro(macroTemplate, env)->toExpr();
        return result;
    }

private:
    static std::shared_ptr<MacroExpr> transformMacro(
        const std::shared_ptr<MacroExpr>& expr,
        const MatchEnv& env)
    {
        if (!expr) {
            return nullptr;
        }

        return std::visit(overloaded {
                              [&](const MacroAtom& atom) -> std::shared_ptr<MacroExpr> {
                                  auto it = env.find(atom.token.lexeme);
                                  if (it != env.end()) {
                                      if (it->second.matches.empty()) {
                                          return nullptr;
                                      }
                                      if (it->second.matches.size() == 1) {
                                          return it->second.matches[0];
                                      } else {
                                          MacroList list;
                                          list.elements = it->second.matches;
                                          return std::make_shared<MacroExpr>(
                                              MacroExpr { std::move(list), false, expr->line });
                                      }
                                  }
                                  return std::make_shared<MacroExpr>(MacroExpr { atom, false, expr->line });
                              },

                              [&](const MacroList& list) -> std::shared_ptr<MacroExpr> {
                                  MacroList newList;
                                  bool hasElements = false;

                                  for (const auto& elem : list.elements) {
                                      if (auto transformed = transformMacro(elem, env)) {
                                          newList.elements.push_back(transformed);
                                          hasElements = true;
                                      }
                                  }

                                  // Only return the list if it has elements or if it's not a variadic pattern
                                  if (hasElements || !expr->isVariadic) {
                                      return std::make_shared<MacroExpr>(
                                          MacroExpr { std::move(newList), expr->isVariadic, expr->line });
                                  }
                                  return nullptr;
                              } },
            expr->value);
    }
};

class InterpreterState;

std::optional<std::shared_ptr<Expression>> expandMacro(
    interpret::InterpreterState& state,
    const std::string& macroName,
    const sExpression& sexpr,
    const std::vector<Token>& literals)
{
    auto expr = std::make_shared<Expression>(sExpression(sexpr.elements), sexpr.elements[0]->line);
    auto rules = state.env->getMacroDefinition(macroName);
    if (!rules) {
        return std::nullopt;
    }
    for (const auto& rule : (*rules).rules) {
        auto matchResult = PatternMatcher::match(rule.pattern, expr, literals);
        if (matchResult) {
            auto transformed = MacroTransformer::transform(rule.template_expr, *matchResult);
            if (transformed) {
                std::string exprStr = transformed->toString();
                auto tokens = scanner::tokenize(exprStr);
                auto reparsed = parse::parse(tokens);
                if (!reparsed || reparsed->empty()) {
                    return std::nullopt;
                }
                return (*reparsed)[0];
            }
        }
    }

    return std::nullopt;
}
bool isMacroCall(const std::shared_ptr<Expression>& expr, const Environment& env)
{
    if (auto* sexpr = std::get_if<sExpression>(&expr->as)) {
        if (!sexpr->elements.empty()) {
            if (auto* atom = std::get_if<AtomExpression>(&sexpr->elements[0]->as)) {
                return env.isMacro(atom->value.lexeme);
            }
        }
    }
    return false;
}

std::optional<std::shared_ptr<Expression>> expandMacrosIn(
    interpret::InterpreterState& state,
    const std::shared_ptr<Expression>& expr)
{
    if (!expr)
        return std::nullopt;

    if (isMacroCall(expr, *state.env)) {
        auto* sexpr = std::get_if<sExpression>(&expr->as);
        auto* atom = std::get_if<AtomExpression>(&sexpr->elements[0]->as);
        auto name = atom->value.lexeme;
        auto rules = state.env->getMacroDefinition(name);
        if (!rules)
            return std::nullopt;
        if (auto expanded = expandMacro(state, name, *sexpr, rules->literals)) {
            return expandMacrosIn(state, *expanded);
        }
        return std::nullopt;
    }

    return std::visit(overloaded {
                          [&](const AtomExpression& atom) -> std::shared_ptr<Expression> {
                              return expr; // Atoms can't contain macros
                          },
                          [&](const ListExpression& list) -> std::shared_ptr<Expression> {
                              std::vector<std::shared_ptr<Expression>> newElements;
                              bool changed = false;
                              for (const auto& e : list.elements) {
                                  if (auto expanded = expandMacrosIn(state, e)) {
                                      newElements.push_back(*expanded);
                                      changed = true;
                                  } else {
                                      newElements.push_back(e);
                                  }
                              }
                              if (changed) {
                                  return std::make_shared<Expression>(
                                      Expression { ListExpression { newElements, list.isVariadic }, expr->line });
                              }
                              return expr;
                          },
                          [&](const sExpression& sexpr) -> std::shared_ptr<Expression> {
                              std::vector<std::shared_ptr<Expression>> newElements;
                              bool changed = false;
                              for (const auto& e : sexpr.elements) {
                                  if (auto expanded = expandMacrosIn(state, e)) {
                                      newElements.push_back(*expanded);
                                      changed = true;
                                  } else {
                                      newElements.push_back(e);
                                  }
                              }
                              if (changed) {
                                  return std::make_shared<Expression>(
                                      Expression { sExpression { newElements }, expr->line });
                              }
                              return expr;
                          },
                          [&](const BeginExpression& begin) -> std::shared_ptr<Expression> {
                              std::vector<std::shared_ptr<Expression>> newBody;
                              bool changed = false;
                              for (const auto& e : begin.body) {
                                  if (auto expanded = expandMacrosIn(state, e)) {
                                      newBody.push_back(*expanded);
                                      changed = true;
                                  } else {
                                      newBody.push_back(e);
                                  }
                              }
                              if (changed) {
                                  return std::make_shared<Expression>(
                                      Expression { BeginExpression { newBody }, expr->line });
                              }
                              return expr;
                          },
                          [&](const LetExpression& let) -> std::shared_ptr<Expression> {
                              std::vector<std::pair<Token, std::shared_ptr<Expression>>> newArgs;
                              std::vector<std::shared_ptr<Expression>> newBody;
                              bool changed = false;

                              for (const auto& [name, value] : let.arguments) {
                                  if (auto expanded = expandMacrosIn(state, value)) {
                                      newArgs.push_back({ name, *expanded });
                                      changed = true;
                                  } else {
                                      newArgs.push_back({ name, value });
                                  }
                              }

                              for (const auto& e : let.body) {
                                  if (auto expanded = expandMacrosIn(state, e)) {
                                      newBody.push_back(*expanded);
                                      changed = true;
                                  } else {
                                      newBody.push_back(e);
                                  }
                              }
                              if (changed) {
                                  return std::make_shared<Expression>(
                                      Expression { LetExpression { let.name, newArgs, newBody }, expr->line });
                              }
                              return expr;
                          },
                          [&](const IfExpression& if_expr) -> std::shared_ptr<Expression> {
                              bool changed = false;
                              auto newCond = expandMacrosIn(state, if_expr.condition);
                              auto newThen = expandMacrosIn(state, if_expr.then);
                              std::optional<std::shared_ptr<Expression>> newElse;

                              if (if_expr.el) {
                                  newElse = expandMacrosIn(state, *if_expr.el);
                              }

                              if (newCond || newThen || newElse) {
                                  return std::make_shared<Expression>(
                                      Expression { IfExpression {
                                                       newCond ? *newCond : if_expr.condition,
                                                       newThen ? *newThen : if_expr.then,
                                                       newElse ? std::optional<std::shared_ptr<Expression>>(*newElse) : if_expr.el },
                                          expr->line });
                              }
                              return expr;
                          },

                          [&](const TailExpression& tail) -> std::shared_ptr<Expression> {
                              if (auto expanded = expandMacrosIn(state, tail.expression)) {
                                  return std::make_shared<Expression>(
                                      Expression { TailExpression { *expanded }, expr->line });
                              }
                              return expr;
                          },

                          [&](const QuoteExpression&) -> std::shared_ptr<Expression> {
                              return expr;
                          },

                          [&](const LambdaExpression& lambda) -> std::shared_ptr<Expression> {
                              std::vector<std::shared_ptr<Expression>> newBody;
                              bool changed = false;
                              for (const auto& e : lambda.body) {
                                  if (auto expanded = expandMacrosIn(state, e)) {
                                      newBody.push_back(*expanded);
                                      changed = true;
                                  } else {
                                      newBody.push_back(e);
                                  }
                              }
                              if (changed) {
                                  return std::make_shared<Expression>(
                                      Expression { LambdaExpression { lambda.parameters, newBody, lambda.isVariadic }, expr->line });
                              }
                              return expr;
                          },

                          [&](const auto&) -> std::shared_ptr<Expression> {
                              return expr;
                          } },
        expr->as);
}
