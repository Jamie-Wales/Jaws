#include "MacroTraits.h"
#include "parse.h"
#include "scan.h"
#include <iostream>
#include <sstream>

namespace macroexp {
MacroAtom::MacroAtom(Token t)
    : token(std::move(t))
{
}

MacroExpression::MacroExpression(const MacroAtom& atom, bool variadic, int lineNum)
    : value(atom)
    , isVariadic(variadic)
    , line(lineNum)
{
}

MacroExpression::MacroExpression(const MacroList& list, bool variadic, int lineNum)
    : value(list)
    , isVariadic(variadic)
    , line(lineNum)
{
}

std::string MacroExpression::toString()
{
    return std::visit(overloaded {
                          [&](const MacroAtom& me) -> std::string {
                              if (isVariadic)
                                  return me.token.lexeme + "...";
                              return me.token.lexeme;
                          },
                          [&](const MacroList& me) -> std::string {
                              std::stringstream ss;
                              if (me.elements.empty()) {
                                  ss << "()";
                              } else {
                                  for (int i = 0; i < me.elements.size(); i++) {
                                      if (i == 0)
                                          ss << "(" << me.elements[i]->toString();
                                      else
                                          ss << " " << me.elements[i]->toString();
                                  }
                                  ss << ")";
                              }
                              if (isVariadic)
                                  ss << "...";
                              return ss.str();
                          } },
        value);
}

void MacroExpression::print()
{
    std::cout << toString() << std::endl;
}

bool isEllipsis(std::shared_ptr<MacroExpression> me)
{
    return std::visit(overloaded {
                          [](MacroAtom& me) { return me.token.type == Tokentype::ELLIPSIS; },
                          [](MacroList& ml) { return false; } },
        me->value);
}

bool isPatternVariable(const Token& token, const std::vector<Token>& literals)
{
    if (token.lexeme == "_") {
        return true;
    }
    return std::find_if(literals.begin(), literals.end(),
               [&](const Token& lit) { return lit.lexeme == token.lexeme; })
        == literals.end();
}

std::shared_ptr<MacroExpression> fromExpr(const std::shared_ptr<Expression>& expr)
{
    if (!expr)
        throw std::runtime_error("Cannot generate MacroExpression from nullptr");

    const auto toProcess = exprToList(expr);
    return std::visit(overloaded {
                          [&](const AtomExpression& atom) -> std::shared_ptr<MacroExpression> {
                              return std::make_shared<MacroExpression>(MacroExpression {
                                  MacroAtom { atom.value }, false, expr->line });
                          },
                          [&](const ListExpression& list) -> std::shared_ptr<MacroExpression> {
                              std::vector<std::shared_ptr<MacroExpression>> processed;

                              for (size_t i = 0; i < list.elements.size(); i++) {
                                  auto elem = fromExpr(list.elements[i]);
                                  if (!elem)
                                      continue;

                                  if (auto* atom = std::get_if<MacroAtom>(&elem->value)) {
                                      // Handle ellipsis case
                                      if (atom->token.type == Tokentype::ELLIPSIS && !processed.empty()) {
                                          processed.back()->isVariadic = true;
                                          continue;
                                      }
                                      // Handle dot case
                                      if (atom->token.type == Tokentype::DOT) {
                                          if (i + 1 >= list.elements.size()) {
                                              throw std::runtime_error("Invalid dot syntax in pattern");
                                          }
                                          // Just process the element after the dot as part of the list
                                          elem = fromExpr(list.elements[++i]);
                                      }
                                  }
                                  processed.push_back(elem);
                              }

                              return std::make_shared<MacroExpression>(
                                  MacroExpression { MacroList { std::move(processed) }, false, expr->line });
                          },
                          [&](const auto&) -> std::shared_ptr<MacroExpression> {
                              throw std::runtime_error("ExpressionToList should only return List or Atom");
                          } },
        toProcess->as);
}

void printMatchEnv(const MatchEnv& env, int indent)
{
    std::string indentStr(indent * 2, ' ');
    for (const auto& [variable, pattern] : env) {
        std::cout << indentStr << "Variable: " << variable << std::endl;
        std::cout << indentStr << "Matches (" << pattern.matches.size() << "):" << std::endl;
        for (size_t i = 0; i < pattern.matches.size(); i++) {
            std::cout << indentStr << "  [" << i << "]: " << pattern.matches[i]->toString() << std::endl;
            if (auto* list = std::get_if<MacroList>(&pattern.matches[i]->value)) {
                for (size_t j = 0; j < list->elements.size(); j++) {
                    std::cout << indentStr << "    element[" << j << "]: "
                              << list->elements[j]->toString() << std::endl;
                }
            }
        }
        std::cout << std::endl;
    }
}

void printMatchResult(const std::pair<MatchEnv, bool>& result)
{
    std::cout << "\nMatch result: " << (result.second ? "SUCCESS" : "FAILED") << std::endl;
    if (result.second) {
        std::cout << "Environment contents:" << std::endl;
        printMatchEnv(result.first);
    }
    std::cout << std::endl;
}

std::pair<MatchEnv, bool> tryMatch(std::shared_ptr<MacroExpression> pattern,
    std::shared_ptr<MacroExpression> expr,
    const std::vector<Token>& literals,
    const std::string& macroName)
{
    MatchEnv env;
    bool success = visit_many(multi_visitor {
                                  [&](const MacroAtom& patAtom, const MacroAtom& exprAtom) -> bool {
                                      if (!isPatternVariable(patAtom.token, literals)) {
                                          return patAtom.token.lexeme == exprAtom.token.lexeme;
                                      }
                                      // Don't store matches for underscore
                                      if (patAtom.token.lexeme != "_") {
                                          env[patAtom.token.lexeme].matches.push_back(
                                              std::make_shared<MacroExpression>(MacroExpression {
                                                  exprAtom, false, expr->line }));
                                      }
                                      return true;
                                  },
                                  [&](const MacroAtom& patAtom, const MacroList& exprList) -> bool {
                                      if (isPatternVariable(patAtom.token, literals)) {
                                          // Don't store matches for underscore
                                          if (patAtom.token.lexeme != "_") {
                                              env[patAtom.token.lexeme].matches.push_back(
                                                  std::make_shared<MacroExpression>(MacroExpression {
                                                      exprList, false, expr->line }));
                                          }
                                          return true;
                                      }
                                      return false;
                                  },
                                  [&](const MacroList& patList, const MacroList& exprList) -> bool {
                                      if (patList.elements.empty() && exprList.elements.empty())
                                          return true;

                                      size_t i = 0;
                                      size_t j = 0;

                                      while (i < patList.elements.size()) {
                                          if (j >= exprList.elements.size()) {
                                              while (i < patList.elements.size()) {
                                                  if (!patList.elements[i]->isVariadic)
                                                      return false;
                                                  i++;
                                              }
                                              return true;
                                          }

                                          if (patList.elements[i]->isVariadic) {
                                              if (auto* list = std::get_if<
                                                      MacroList>(&patList.elements[i]->value)) {
                                                  while (j < exprList.elements.size()) {
                                                      auto [subEnv, matched] = tryMatch(
                                                          patList.elements[i],
                                                          exprList.elements[j],
                                                          literals,
                                                          macroName);
                                                      if (!matched)
                                                          break;

                                                      for (const auto& [key, value] : subEnv) {
                                                          if (key != "_") {
                                                              // Skip underscores when merging environments
                                                              env[key].matches.insert(
                                                                  env[key].matches.end(),
                                                                  value.matches.begin(),
                                                                  value.matches.end());
                                                          }
                                                      }
                                                      j++;
                                                  }
                                                  i++;
                                                  continue;
                                              }

                                              auto* atom = std::get_if<MacroAtom>(&patList.elements[i]->value);
                                              if (atom && !isPatternVariable(atom->token, literals))
                                                  return false;

                                              if (i + 1 < patList.elements.size()) {
                                                  size_t k = j;
                                                  while (k < exprList.elements.size()) {
                                                      auto [nextEnv, matched] = tryMatch(
                                                          patList.elements[i + 1],
                                                          exprList.elements[k],
                                                          literals,
                                                          macroName);
                                                      if (matched) {
                                                          if (atom && atom->token.lexeme != "_") {
                                                              // Only store if not underscore
                                                              for (size_t m = j; m < k; m++) {
                                                                  env[atom->token.lexeme].matches.push_back(
                                                                      exprList.elements[m]);
                                                              }
                                                          }
                                                          j = k;
                                                          break;
                                                      }
                                                      k++;
                                                  }
                                                  if (k == exprList.elements.size()) {
                                                      if (atom && atom->token.lexeme != "_") {
                                                          // Only store if not underscore
                                                          for (size_t m = j; m < exprList.elements.size(); m++) {
                                                              env[atom->token.lexeme].matches.push_back(
                                                                  exprList.elements[m]);
                                                          }
                                                      }
                                                      j = exprList.elements.size();
                                                  }
                                              } else {
                                                  if (atom && atom->token.lexeme != "_") {
                                                      // Only store if not underscore
                                                      for (size_t m = j; m < exprList.elements.size(); m++) {
                                                          env[atom->token.lexeme].matches.push_back(
                                                              exprList.elements[m]);
                                                      }
                                                  }
                                                  j = exprList.elements.size();
                                              }
                                              i++;
                                          } else {
                                              auto [subEnv, matched] = tryMatch(
                                                  patList.elements[i],
                                                  exprList.elements[j],
                                                  literals,
                                                  macroName);
                                              if (!matched)
                                                  return false;

                                              for (const auto& [key, value] : subEnv) {
                                                  if (key != "_") {
                                                      // Skip underscores when merging environments
                                                      env[key].matches.insert(
                                                          env[key].matches.end(),
                                                          value.matches.begin(),
                                                          value.matches.end());
                                                  }
                                              }
                                              i++;
                                              j++;
                                          }
                                      }
                                      return j == exprList.elements.size();
                                  },
                                  [&](const MacroList&, const MacroAtom&) -> bool {
                                      return false;
                                  },
                                  [&](const auto&, const auto&) -> bool {
                                      std::cout << "Pattern:";
                                      pattern->print();
                                      std::cout << "Expr:";
                                      expr->print();
                                      throw std::runtime_error("Cannot match different types");
                                  } },
        pattern->value, expr->value);

    return { env, success };
}

void findVariadics(const MacroList& list,
    std::vector<std::string>& variadicVars,
    size_t& variadicCount,
    const MatchEnv& env)
{
    for (const auto& e : list.elements) {
        if (auto* a = std::get_if<MacroAtom>(&e->value)) {
            auto it = env.find(a->token.lexeme);
            if (it != env.end()) {
                if (variadicCount == 0) {
                    variadicCount = it->second.matches.size();
                } else if (variadicCount != it->second.matches.size()) {
                    throw std::runtime_error("Mismatched variadic pattern lengths");
                }
                variadicVars.push_back(a->token.lexeme);
            }
        } else if (auto* l2 = std::get_if<MacroList>(&e->value)) {
            findVariadics(*l2, variadicVars, variadicCount, env);
        }
    }
}

std::shared_ptr<MacroExpression> transformTemplate(const std::shared_ptr<MacroExpression>& template_expr,
    const MatchEnv& env)
{
    return std::visit(overloaded {
                          [&](const MacroAtom& atom) -> std::shared_ptr<MacroExpression> {
                              auto it = env.find(atom.token.lexeme);
                              if (it != env.end() && !it->second.matches.empty()) {
                                  if (!template_expr->isVariadic)
                                      return it->second.matches[0];
                              }
                              return std::make_shared<MacroExpression>(
                                  atom, template_expr->isVariadic, template_expr->line);
                          },
                          [&](const MacroList& list) -> std::shared_ptr<MacroExpression> {
                              std::vector<std::shared_ptr<MacroExpression>> transformed;

                              for (size_t i = 0; i < list.elements.size(); i++) {
                                  auto elem = list.elements[i];
                                  if (elem->isVariadic) {
                                      if (auto* atom = std::get_if<MacroAtom>(&elem->value)) {
                                          auto it = env.find(atom->token.lexeme);
                                          if (it != env.end()) {
                                              transformed.insert(transformed.end(),
                                                  it->second.matches.begin(),
                                                  it->second.matches.end());
                                          }
                                      } else if (auto* innerList = std::get_if<MacroList>(&elem->value)) {
                                          size_t variadicCount = 0;
                                          std::vector<std::string> variadicVars;

                                          findVariadics(*innerList, variadicVars, variadicCount, env);

                                          for (size_t idx = 0; idx < variadicCount; idx++) {
                                              MatchEnv tempEnv;
                                              for (const auto& var : variadicVars) {
                                                  tempEnv[var].matches = { env.at(var).matches[idx] };
                                              }

                                              std::vector<std::shared_ptr<MacroExpression>> nestedElements;

                                              for (const auto& innerElem : innerList->elements) {
                                                  auto transformedInner = transformTemplate(innerElem, tempEnv);
                                                  nestedElements.push_back(transformedInner);
                                              }

                                              transformed.push_back(std::make_shared<MacroExpression>(
                                                  MacroList { std::move(nestedElements) },
                                                  false,
                                                  template_expr->line));
                                          }
                                      }
                                  } else {
                                      transformed.push_back(transformTemplate(elem, env));
                                  }
                              }

                              return std::make_shared<MacroExpression>(
                                  MacroList { std::move(transformed) },
                                  template_expr->isVariadic,
                                  template_expr->line);
                          } },
        template_expr->value);
}

std::shared_ptr<Expression> exprFromMacro(const std::shared_ptr<MacroExpression>& macro)
{
    return std::visit(overloaded {
                          [&](const MacroAtom& atom) -> std::shared_ptr<Expression> {
                              return std::make_shared<Expression>(Expression {
                                  AtomExpression { atom.token }, macro->line });
                          },
                          [&](const MacroList& list) -> std::shared_ptr<Expression> {
                              std::vector<std::shared_ptr<Expression>> elements;
                              for (const auto& elem : list.elements) {
                                  elements.push_back(exprFromMacro(elem));
                              }
                              return std::make_shared<Expression>(Expression {
                                  ListExpression { std::move(elements) }, macro->line });
                          } },
        macro->value);
}

std::shared_ptr<MacroExpression> transformMacroRecursive(
    const std::shared_ptr<MacroExpression>& expr,
    pattern::MacroEnvironment& env)
{
    bool expanded;
    auto current = expr;

    do {
        expanded = false;

        if (auto* atom = std::get_if<MacroAtom>(&current->value)) {
            if (env.isMacro(atom->token.lexeme)) {
                auto synt = env.getMacroDefinition(atom->token.lexeme);
                auto rules = std::get<SyntaxRulesExpression>((*synt)->as).rules;
                auto literals = std::get<SyntaxRulesExpression>((*synt)->as).literals;

                for (const auto& rule : rules) {
                    auto patternExpr = fromExpr(rule.pattern);
                    auto [matchEnv, success] = tryMatch(patternExpr, current, literals, atom->token.lexeme);
                    if (success) {
                        current = transformTemplate(fromExpr(rule.template_expr), matchEnv);
                        expanded = true;
                        break;
                    }
                }
            }
        } else if (auto* list = std::get_if<MacroList>(&current->value)) {
            if (!list->elements.empty()) {
                if (auto* firstAtom = std::get_if<MacroAtom>(&list->elements[0]->value)) {
                    if (env.isMacro(firstAtom->token.lexeme)) {
                        auto synt = env.getMacroDefinition(firstAtom->token.lexeme);
                        auto rules = std::get<SyntaxRulesExpression>((*synt)->as).rules;
                        auto literals = std::get<SyntaxRulesExpression>((*synt)->as).literals;

                        for (const auto& rule : rules) {
                            auto patternExpr = fromExpr(rule.pattern);
                            auto [matchEnv, success] = tryMatch(patternExpr, current, literals,
                                firstAtom->token.lexeme);
                            if (success) {
                                current = transformTemplate(fromExpr(rule.template_expr), matchEnv);
                                expanded = true;
                                break;
                            }
                        }
                    }
                }

                if (!expanded) {
                    std::vector<std::shared_ptr<MacroExpression>> newElements;
                    bool anyExpanded = false;

                    for (const auto& elem : list->elements) {
                        auto newElem = transformMacroRecursive(elem, env);
                        if (newElem->toString() != elem->toString()) {
                            anyExpanded = true;
                        }
                        newElements.push_back(newElem);
                    }

                    if (anyExpanded) {
                        current = std::make_shared<MacroExpression>(
                            MacroList { std::move(newElements) },
                            current->isVariadic,
                            current->line);
                        expanded = true;
                    }
                }
            }
        }
    } while (expanded);

    return current;
}

std::shared_ptr<MacroExpression> transformMacro(
    const std::shared_ptr<Expression>& template_expr,
    const MatchEnv& env,
    const std::string& macroName,
    pattern::MacroEnvironment& macroEnv)
{
    auto transformed = transformTemplate(fromExpr(template_expr), env);
    auto fullyExpanded = transformMacroRecursive(transformed, macroEnv);
    return fullyExpanded;
}

std::shared_ptr<Expression> expandMacro(
    std::shared_ptr<Expression> expr,
    pattern::MacroEnvironment& env)
{
    auto userExpr = fromExpr(expr);

    return std::visit(overloaded {
                          [&](const MacroAtom& ma) -> std::shared_ptr<Expression> {
                              if (env.isMacro(ma.token.lexeme)) {
                                  auto synt = env.getMacroDefinition(ma.token.lexeme);
                                  auto rules = std::get<SyntaxRulesExpression>((*synt)->as).rules;
                                  auto literals = std::get<SyntaxRulesExpression>((*synt)->as).literals;

                                  for (const auto& rule : rules) {
                                      auto patternExpr = fromExpr(rule.pattern);
                                      auto [matchEnv, success] = tryMatch(
                                          patternExpr, userExpr, literals, ma.token.lexeme);
                                      if (success) {
                                          auto expanded = transformMacro(
                                              rule.template_expr, matchEnv, ma.token.lexeme, env);
                                          auto tokens = scanner::tokenize(expanded->toString());
                                          return (*parse::parse(std::move(tokens)))[0];
                                      }
                                  }
                              }
                              return expr;
                          },
                          [&](const MacroList& ml) -> std::shared_ptr<Expression> {
                              if (!ml.elements.empty()) {
                                  if (auto* firstAtom = std::get_if<MacroAtom>(&ml.elements[0]->value)) {
                                      if (env.isMacro(firstAtom->token.lexeme)) {
                                          auto synt = env.getMacroDefinition(firstAtom->token.lexeme);
                                          auto rules = std::get<SyntaxRulesExpression>((*synt)->as).rules;
                                          auto literals = std::get<SyntaxRulesExpression>((*synt)->as).literals;

                                          for (const auto& rule : rules) {
                                              auto patternExpr = fromExpr(rule.pattern);
                                              auto [matchEnv, success] = tryMatch(
                                                  patternExpr, userExpr, literals, firstAtom->token.lexeme);
                                              if (success) {
                                                  auto expanded = transformMacro(
                                                      rule.template_expr, matchEnv, firstAtom->token.lexeme, env);
                                                  auto tokens = scanner::tokenize(expanded->toString());
                                                  return (*parse::parse(std::move(tokens)))[0];
                                              }
                                          }
                                      }
                                  }
                              }
                              return expr;
                          } },
        userExpr->value);
}

std::vector<std::shared_ptr<Expression>> expandMacros(std::vector<std::shared_ptr<Expression>> exprs)
{
    std::vector<std::shared_ptr<Expression>> toExpand;
    std::vector<std::shared_ptr<Expression>> expanded;
    pattern::MacroEnvironment env;

    // First collect macro definitions
    for (const auto& expr : exprs) {
        std::visit(overloaded {
                       [&](const DefineSyntaxExpression& de) {
                           auto rule = std::make_shared<Expression>(
                               Expression { std::get<SyntaxRulesExpression>(de.rule->as), expr->line });
                           env.defineMacro(de.name.lexeme, rule);
                       },
                       [&](const auto&) {
                           toExpand.push_back(expr);
                       } },
            expr->as);
    }

    // Then expand each expression
    for (const auto& expr : toExpand) {
        auto userExpr = fromExpr(expr);
        auto expandedExpr = std::visit(overloaded {
                                           [&](const MacroAtom& ma) -> std::shared_ptr<Expression> {
                                               if (env.isMacro(ma.token.lexeme)) {
                                                   auto synt = env.getMacroDefinition(ma.token.lexeme);
                                                   auto rules = std::get<SyntaxRulesExpression>((*synt)->as).rules;
                                                   auto literals = std::get<SyntaxRulesExpression>((*synt)->as).literals;

                                                   for (const auto& rule : rules) {
                                                       auto patternExpr = fromExpr(rule.pattern);
                                                       auto [matchEnv, success] = tryMatch(
                                                           patternExpr, userExpr, literals, ma.token.lexeme);
                                                       if (success) {
                                                           auto expanded = transformMacro(
                                                               rule.template_expr, matchEnv, ma.token.lexeme, env);
                                                           auto tokens = scanner::tokenize(expanded->toString());
                                                           return (*parse::parse(std::move(tokens)))[0];
                                                       }
                                                   }
                                               }
                                               return expr;
                                           },
                                           [&](const MacroList& ml) -> std::shared_ptr<Expression> {
                                               if (!ml.elements.empty()) {
                                                   if (auto* firstAtom = std::get_if<MacroAtom>(
                                                           &ml.elements[0]->value)) {
                                                       if (env.isMacro(firstAtom->token.lexeme)) {
                                                           auto synt = env.getMacroDefinition(
                                                               firstAtom->token.lexeme);
                                                           auto rules = std::get<SyntaxRulesExpression>((*synt)->as)
                                                                            .rules;
                                                           auto literals = std::get<SyntaxRulesExpression>(
                                                               (*synt)->as)
                                                                               .literals;

                                                           for (const auto& rule : rules) {
                                                               auto patternExpr = fromExpr(rule.pattern);
                                                               auto [matchEnv, success] = tryMatch(
                                                                   patternExpr, userExpr, literals,
                                                                   firstAtom->token.lexeme);
                                                               if (success) {
                                                                   auto expanded = transformMacro(
                                                                       rule.template_expr, matchEnv,
                                                                       firstAtom->token.lexeme, env);
                                                                   auto tokens = scanner::tokenize(
                                                                       expanded->toString());
                                                                   return (*parse::parse(std::move(tokens)))[0];
                                                               }
                                                           }
                                                       }
                                                   }
                                               }
                                               return expr;
                                           } },
            userExpr->value);

        expanded.push_back(expandedExpr);
    }

    return expanded;
}
}
