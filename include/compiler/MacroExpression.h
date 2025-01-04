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
            std::cout << "DEBUG: fromExpr called with null expression\n";
            return nullptr;
        }

        std::cout << "DEBUG: Converting expression to MacroExpr: " << expr->toString() << "\n";

        return std::visit(overloaded {
                              [&](const AtomExpression& atom) -> std::shared_ptr<MacroExpr> {
                                  std::cout << "DEBUG: Processing atom: " << atom.value.lexeme
                                            << " (type: " << static_cast<int>(atom.value.type) << ")\n";

                                  if (atom.value.type == Tokentype::ELLIPSIS) {
                                      std::cout << "DEBUG: Skipping ellipsis atom\n";
                                      return nullptr;
                                  }
                                  return std::make_shared<MacroExpr>(
                                      MacroExpr { MacroAtom { atom.value }, false, expr->line });
                              },

                              [&](const ListExpression& list) -> std::shared_ptr<MacroExpr> {
                                  std::cout << "DEBUG: Processing list with " << list.elements.size() << " elements\n";
                                  MacroList macroList;
                                  bool isVariadic = false;

                                  if (!list.elements.empty()) {
                                      auto last = list.elements.back();
                                      std::cout << "DEBUG: Checking last element: " << last->toString() << "\n";
                                      if (auto* atom = std::get_if<AtomExpression>(&last->as)) {
                                          if (atom->value.type == Tokentype::ELLIPSIS) {
                                              std::cout << "DEBUG: Found variadic pattern (ellipsis)\n";
                                              isVariadic = true;
                                          }
                                      }
                                  }

                                  size_t processElements = list.elements.size() - (isVariadic ? 1 : 0);
                                  std::cout << "DEBUG: Processing " << processElements << " elements\n";

                                  for (size_t i = 0; i < processElements; i++) {
                                      std::cout << "DEBUG: Converting list element " << i << ": "
                                                << list.elements[i]->toString() << "\n";
                                      if (auto elem = fromExpr(list.elements[i])) {
                                          macroList.elements.push_back(elem);
                                      }
                                  }

                                  return std::make_shared<MacroExpr>(
                                      MacroExpr { std::move(macroList), isVariadic, expr->line });
                              },

                              [](const auto&) -> std::shared_ptr<MacroExpr> {
                                  std::cout << "DEBUG: Unknown expression type\n";
                                  return nullptr;
                              } },
            expr->as);
    }

    std::shared_ptr<Expression> toExpr() const
    {
        std::cout << "DEBUG: Converting MacroExpr back to Expression\n";

        return std::visit(overloaded {
                              [&](const MacroAtom& atom) -> std::shared_ptr<Expression> {
                                  std::cout << "DEBUG: Converting atom back: " << atom.token.lexeme << "\n";
                                  return std::make_shared<Expression>(
                                      Expression { AtomExpression { atom.token }, line });
                              },

                              [&](const MacroList& list) -> std::shared_ptr<Expression> {
                                  std::cout << "DEBUG: Converting list back with " << list.elements.size()
                                            << " elements, variadic=" << isVariadic << "\n";
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
    std::cout << "DEBUG: Preparing expression for macro: " << expr->toString() << "\n";
    auto listed = exprToList(expr);
    std::cout << "DEBUG: After listing: " << listed->toString() << "\n";
    return MacroExpr::fromExpr(listed);
}

class PatternMatcher {
public:
    static void debugPrintEnv(const MatchEnv& env)
    {
        std::cout << "DEBUG: Current environment state:\n";
        for (const auto& [key, value] : env) {
            std::cout << "  " << key << " -> [";
            for (const auto& match : value.matches) {
                if (auto expr = match->toExpr()) {
                    std::cout << expr->toString() << " ";
                }
            }
            std::cout << "]\n";
        }
    }

    static std::optional<MatchEnv> match(
        const std::shared_ptr<Expression>& pattern,
        const std::shared_ptr<Expression>& expr,
        const std::vector<Token>& literals)
    {
        std::cout << "DEBUG: Starting pattern matching\n"
                  << "Pattern: " << pattern->toString() << "\n"
                  << "Expression: " << expr->toString() << "\n"
                  << "Literals count: " << literals.size() << "\n";

        auto macroPattern = prepareForMacro(pattern);
        auto macroExpr = prepareForMacro(expr);

        MatchEnv env;
        if (matchExpr(macroPattern, macroExpr, env, literals)) {
            std::cout << "DEBUG: Pattern matching succeeded\n";
            debugPrintEnv(env);
            return env;
        }
        std::cout << "DEBUG: Pattern matching failed\n";
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
            std::cout << "DEBUG: matchExpr called with null pattern or expression\n";
            return false;
        }

        std::cout << "DEBUG: Matching expressions:\n";
        if (auto patExpr = pattern->toExpr()) {
            std::cout << "Pattern: " << patExpr->toString() << "\n";
        }
        if (auto expExpr = expr->toExpr()) {
            std::cout << "Expression: " << expExpr->toString() << "\n";
        }

        // First check if pattern is an atom that's a pattern variable
        if (auto* patAtom = std::get_if<MacroAtom>(&pattern->value)) {
            // Check if it's a literal
            bool isLiteral = std::find_if(literals.begin(), literals.end(),
                                 [&](const Token& lit) { return lit.lexeme == patAtom->token.lexeme; })
                != literals.end();

            if (!isLiteral && patAtom->token.lexeme != "_") {
                // It's a pattern variable - it can match anything
                std::cout << "DEBUG: Pattern variable match: " << patAtom->token.lexeme << "\n";
                env[patAtom->token.lexeme].matches.push_back(expr);
                return true;
            }
        }

        // If it's not a pattern variable, do normal matching
        return std::visit(overloaded {
                              [&](const MacroAtom& patAtom, const MacroAtom& exprAtom) -> bool {
                                  std::cout << "DEBUG: Matching atoms: " << patAtom.token.lexeme
                                            << " with " << exprAtom.token.lexeme << "\n";

                                  if (patAtom.token.lexeme == "_") {
                                      std::cout << "DEBUG: Wildcard match\n";
                                      return true;
                                  }

                                  std::cout << "DEBUG: Literal match: " << (patAtom.token.lexeme == exprAtom.token.lexeme) << "\n";
                                  return patAtom.token.lexeme == exprAtom.token.lexeme;
                              },

                              [&](const MacroList& patList, const MacroList& exprList) -> bool {
                                  if (!pattern->isVariadic) {
                                      std::cout << "DEBUG: Non-variadic list match\n";
                                      if (patList.elements.size() != exprList.elements.size()) {
                                          std::cout << "DEBUG: List size mismatch\n";
                                          return false;
                                      }

                                      for (size_t i = 0; i < patList.elements.size(); i++) {
                                          if (!matchExpr(patList.elements[i], exprList.elements[i], env, literals)) {
                                              return false;
                                          }
                                      }
                                      return true;
                                  }

                                  std::cout << "DEBUG: Variadic list match\n";
                                  if (patList.elements.empty()) {
                                      std::cout << "DEBUG: Empty pattern list\n";
                                      return false;
                                  }

                                  size_t fixedCount = patList.elements.size() - 1;
                                  if (exprList.elements.size() < fixedCount) {
                                      std::cout << "DEBUG: Not enough elements for fixed part\n";
                                      return false;
                                  }

                                  for (size_t i = 0; i < fixedCount; i++) {
                                      if (!matchExpr(patList.elements[i], exprList.elements[i], env, literals)) {
                                          return false;
                                      }
                                  }

                                  if (auto last = patList.elements.back()) {
                                      std::cout << "DEBUG: Processing variadic part\n";
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
                              },

                              [](const auto&, const auto&) -> bool {
                                  std::cout << "DEBUG: Type mismatch between pattern and expression\n";
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
        std::cout << "DEBUG: Starting macro transformation\n"
                  << "Template: " << templ->toString() << "\n";

        auto macroTemplate = prepareForMacro(templ);
        auto result = transformMacro(macroTemplate, env)->toExpr();

        std::cout << "DEBUG: Transformation result: " << result->toString() << "\n";
        return result;
    }

private:
    static std::shared_ptr<MacroExpr> transformMacro(
        const std::shared_ptr<MacroExpr>& expr,
        const MatchEnv& env)
    {
        if (!expr) {
            std::cout << "DEBUG: transformMacro called with null expression\n";
            return nullptr;
        }

        return std::visit(overloaded {
                              [&](const MacroAtom& atom) -> std::shared_ptr<MacroExpr> {
                                  std::cout << "DEBUG: Transforming atom: " << atom.token.lexeme << "\n";
                                  auto it = env.find(atom.token.lexeme);
                                  if (it != env.end()) {
                                      std::cout << "DEBUG: Found pattern variable in environment\n";
                                      if (it->second.matches.size() == 1) {
                                          return it->second.matches[0];
                                      } else {
                                          MacroList list;
                                          list.elements = it->second.matches;
                                          return std::make_shared<MacroExpr>(
                                              MacroExpr { std::move(list), false, expr->line });
                                      }
                                  }
                                  std::cout << "DEBUG: Keeping original atom\n";
                                  return std::make_shared<MacroExpr>(MacroExpr { atom, false, expr->line });
                              },

                              [&](const MacroList& list) -> std::shared_ptr<MacroExpr> {
                                  std::cout << "DEBUG: Transforming list with " << list.elements.size() << " elements\n";
                                  MacroList newList;
                                  for (const auto& elem : list.elements) {
                                      if (auto transformed = transformMacro(elem, env)) {
                                          newList.elements.push_back(transformed);
                                      }
                                  }
                                  return std::make_shared<MacroExpr>(
                                      MacroExpr { std::move(newList), expr->isVariadic, expr->line });
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

    auto rules = state.env->getMacroRules(macroName);
    if (!rules) {
        return std::nullopt;
    }

    for (const auto& rule : *rules) {
        auto matchResult = PatternMatcher::match(rule.pattern, expr, literals);
        if (matchResult) {
            auto transformed = MacroTransformer::transform(rule.template_expr, *matchResult);
            if (transformed) {
                // Convert to string and reparse
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
bool isMacroCall(const std::shared_ptr<Expression>& expr, const Environment& env) {
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
    if (!expr) return std::nullopt;

    if (isMacroCall(expr, *state.env)) {
        auto* sexpr = std::get_if<sExpression>(&expr->as);
        auto* atom = std::get_if<AtomExpression>(&sexpr->elements[0]->as);
        auto name = atom->value.lexeme;

        auto rules = state.env->getMacroRules(name);
        if (!rules) return std::nullopt;

        std::vector<Token> literals;
        for (const auto& rule : *rules) {
            if (auto* patternAtom = std::get_if<AtomExpression>(&rule.pattern->as)) {
                literals.push_back(patternAtom->value);
            }
        }

        if (auto expanded = expandMacro(state, name, *sexpr, literals)) {
            return expandMacrosIn(state, *expanded);
        }
        return std::nullopt;
    }

    // If not a macro call, recursively check all nested expressions
    return std::visit(overloaded {
        [&](const AtomExpression& atom) -> std::shared_ptr<Expression> {
            return expr;  // Atoms can't contain macros
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
                    Expression{ListExpression{newElements, list.isVariadic}, expr->line});
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
                    Expression{sExpression{newElements}, expr->line});
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
                    Expression{BeginExpression{newBody}, expr->line});
            }
            return expr;
        },

        [&](const LetExpression& let) -> std::shared_ptr<Expression> {
            std::vector<std::pair<Token, std::shared_ptr<Expression>>> newArgs;
            std::vector<std::shared_ptr<Expression>> newBody;
            bool changed = false;

            for (const auto& [name, value] : let.arguments) {
                if (auto expanded = expandMacrosIn(state, value)) {
                    newArgs.push_back({name, *expanded});
                    changed = true;
                } else {
                    newArgs.push_back({name, value});
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
                    Expression{LetExpression{let.name, newArgs, newBody}, expr->line});
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
                    Expression{IfExpression{
                        newCond ? *newCond : if_expr.condition,
                        newThen ? *newThen : if_expr.then,
                        newElse ? std::optional<std::shared_ptr<Expression>>(*newElse) : if_expr.el
                    }, expr->line});
            }
            return expr;
        },

        [&](const TailExpression& tail) -> std::shared_ptr<Expression> {
            if (auto expanded = expandMacrosIn(state, tail.expression)) {
                return std::make_shared<Expression>(
                    Expression{TailExpression{*expanded}, expr->line});
            }
            return expr;
        },

        [&](const QuoteExpression&) -> std::shared_ptr<Expression> {
            return expr;  // Don't expand inside quotes
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
                    Expression{LambdaExpression{lambda.parameters, newBody, lambda.isVariadic}, expr->line});
            }
            return expr;
        },

        [&](const auto&) -> std::shared_ptr<Expression> {
            return expr;  // Default case for other expression types
        }
    }, expr->as);
}
