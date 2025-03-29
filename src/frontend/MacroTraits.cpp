#include "MacroTraits.h"
#include "parse.h"
#include "scan.h"
#include <algorithm>
#include <iomanip>
#include <iostream>
#include <map>
#include <memory>
#include <sstream>
#include <stdexcept>
#include <string>
#include <variant>
#include <vector>

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

// --- toString (No Changes) ---
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
                                  ss << "(";
                                  for (size_t i = 0; i < me.elements.size(); i++) {
                                      if (i > 0)
                                          ss << " ";
                                      ss << me.elements[i]->toString();
                                  }
                                  ss << ")";
                              }
                              if (isVariadic)
                                  ss << "...";
                              return ss.str();
                          } },
        value);
}

void findVariadics(const MacroList& list,
    std::vector<std::string>& variadicVars,
    size_t& variadicCount,
    const MatchEnv& env)
{
    for (const auto& e : list.elements) {
        std::vector<std::string> current_element_vars;
        size_t current_element_count = 0;
        std::function<void(const std::shared_ptr<MacroExpression>&)> findVars =
            [&](const std::shared_ptr<MacroExpression>& elem) {
                if (auto* a = std::get_if<MacroAtom>(&elem->value)) {
                    auto it = env.find(a->token.lexeme);
                    if (it != env.end() && isPatternVariable(a->token, {})) {
                        bool already_found = false;
                        for (const auto& vv : variadicVars) {
                            if (vv == a->token.lexeme) {
                                already_found = true;
                                break;
                            }
                        }
                        if (already_found)
                            return;

                        size_t matches_size = it->second.matches.size();

                        if (matches_size > 1) {
                            if (variadicCount == 0) {
                                variadicCount = matches_size;
                                variadicVars.push_back(a->token.lexeme);
                            } else if (variadicCount != matches_size) {
                                if (matches_size != 1) {
                                    throw std::runtime_error("Mismatched variadic pattern lengths (" + std::to_string(variadicCount) + " vs " + std::to_string(matches_size) + " for " + a->token.lexeme + ")");
                                }
                                variadicVars.push_back(a->token.lexeme);
                            } else {
                                variadicVars.push_back(a->token.lexeme);
                            }
                        } else if (matches_size == 1) {
                            variadicVars.push_back(a->token.lexeme);
                            if (variadicCount == 0) {
                                variadicCount = 1;
                            }
                        } else {
                            variadicVars.push_back(a->token.lexeme);
                            if (variadicCount == 0)
                                variadicCount = 0;
                        }
                    }
                } else if (auto* l2 = std::get_if<MacroList>(&elem->value)) {
                    for (const auto& sub_e : l2->elements) {
                        findVars(sub_e); // Recurse
                    }
                }
            };

        findVars(e);
    }
    std::sort(variadicVars.begin(), variadicVars.end());
    variadicVars.erase(std::unique(variadicVars.begin(), variadicVars.end()), variadicVars.end());
    if (variadicCount == 0 && !variadicVars.empty()) {
        variadicCount = 1;
    }
}

void MacroExpression::print()
{
    std::cout << toString() << std::endl;
}

// --- isEllipsis (No Changes) ---
bool isEllipsis(std::shared_ptr<MacroExpression> me)
{
    return std::visit(overloaded {
                          [](const MacroAtom& me) { return me.token.type == Tokentype::ELLIPSIS; },
                          [](const MacroList& ml) { return false; } },
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

                                  if (auto* atom_ptr = std::get_if<MacroAtom>(&elem->value)) {
                                      if (atom_ptr->token.type == Tokentype::ELLIPSIS) {
                                          if (!processed.empty()) {
                                              processed.back()->isVariadic = true;
                                              continue;
                                          } else {
                                              throw std::runtime_error("Invalid ellipsis placement near line " + std::to_string(elem->line));
                                          }
                                      }
                                      if (atom_ptr->token.type == Tokentype::DOT) {
                                          if (i + 1 >= list.elements.size()) {
                                              throw std::runtime_error("Invalid dot syntax in pattern near line " + std::to_string(elem->line));
                                          }
                                          elem = fromExpr(list.elements[++i]);
                                      }
                                  }
                                  processed.push_back(elem);
                              }
                              return std::make_shared<MacroExpression>(
                                  MacroExpression { MacroList { std::move(processed) }, false, expr->line });
                          },
                          [&](const VectorExpression& vec) -> std::shared_ptr<MacroExpression> {
                              std::vector<std::shared_ptr<MacroExpression>> processed;
                              for (const auto& el : vec.elements) {
                                  processed.push_back(fromExpr(el));
                              }
                              return std::make_shared<MacroExpression>(
                                  MacroExpression { MacroList { std::move(processed) }, false, expr->line });
                          },
                          [&](const auto& other) -> std::shared_ptr<MacroExpression> {
                              throw std::runtime_error("Unsupported expression type in fromExpr: " + expr->toString());
                          } },
        toProcess->as);
}

void printMatchEnv(const MatchEnv& env, int indent)
{
    std::string indentStr(indent * 2, ' ');
    if (env.empty()) {
        std::cout << indentStr << "<Empty Environment>" << std::endl;
        return;
    }
    for (const auto& [variable, pattern] : env) {
        std::cout << indentStr << "Variable: " << variable << std::endl;
        std::cout << indentStr << "  Matches (" << pattern.matches.size() << "):" << std::endl;
        for (size_t i = 0; i < pattern.matches.size(); i++) {
            std::cout << indentStr << "    [" << i << "]: " << pattern.matches[i]->toString() << std::endl;
        }
    }
    std::cout << std::endl;
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
                                      if (patAtom.token.lexeme != "_") {
                                          env[patAtom.token.lexeme].matches.push_back(
                                              std::make_shared<MacroExpression>(MacroExpression {
                                                  exprAtom, false, expr->line }));
                                      }
                                      return true;
                                  },
                                  [&](const MacroAtom& patAtom, const MacroList& exprList) -> bool {
                                      if (isPatternVariable(patAtom.token, literals)) {
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
                                                  if (auto* atom = std::get_if<MacroAtom>(&patList.elements[i]->value)) {
                                                      if (isPatternVariable(atom->token, literals) && atom->token.lexeme != "_") {
                                                          env.try_emplace(atom->token.lexeme); // Ensure entry exists, even if empty vector
                                                      }
                                                  } else if (auto* list = std::get_if<MacroList>(&patList.elements[i]->value)) {
                                                      std::vector<std::string> zero_vars;
                                                      size_t dummy_count = 0;
                                                      findVariadics(*list, zero_vars, dummy_count, env); // Find vars in sub-pattern
                                                      for (const auto& var_name : zero_vars) {
                                                          env.try_emplace(var_name);
                                                      }
                                                  }
                                                  i++;
                                              }
                                              return true; // All remaining patterns were variadic
                                          }

                                          if (patList.elements[i]->isVariadic) {
                                              auto current_pattern_element = patList.elements[i];
                                              current_pattern_element->isVariadic = false;

                                              MatchEnv accumulated_sub_env;
                                              size_t variadic_match_count = 0;
                                              size_t initial_j = j;

                                              if (i + 1 < patList.elements.size()) {
                                                  auto next_pattern_element = patList.elements[i + 1];
                                                  size_t k = j;
                                                  while (k < exprList.elements.size()) {
                                                      auto [nextMatchEnv, nextMatched] = tryMatch(next_pattern_element, exprList.elements[k], literals, macroName);
                                                      if (nextMatched) {
                                                          variadic_match_count = k - initial_j;
                                                          break;
                                                      }
                                                      k++;
                                                  }
                                                  if (k == exprList.elements.size()) {
                                                      variadic_match_count = exprList.elements.size() - initial_j;
                                                  }
                                              } else {
                                                  variadic_match_count = exprList.elements.size() - initial_j;
                                              }

                                              for (size_t v_idx = 0; v_idx < variadic_match_count; ++v_idx) {
                                                  auto [subEnv, matched] = tryMatch(current_pattern_element, exprList.elements[j], literals, macroName);
                                                  if (!matched) {
                                                      current_pattern_element->isVariadic = true;
                                                      // this ideally not fail if lookahead worked
                                                      return false;
                                                  }
                                                  for (const auto& [key, value] : subEnv) {
                                                      if (key != "_") {
                                                          accumulated_sub_env[key].matches.insert(
                                                              accumulated_sub_env[key].matches.end(),
                                                              value.matches.begin(), value.matches.end());
                                                      }
                                                  }
                                                  j++;
                                              }
                                              for (const auto& [key, value] : accumulated_sub_env) {
                                                  env[key].matches.insert(env[key].matches.end(), value.matches.begin(), value.matches.end());
                                              }

                                              current_pattern_element->isVariadic = true;
                                              i++;

                                          } else {
                                              auto [subEnv, matched] = tryMatch(patList.elements[i], exprList.elements[j], literals, macroName);
                                              if (!matched)
                                                  return false;
                                              for (const auto& [key, value] : subEnv) {
                                                  if (key != "_") {
                                                      env[key].matches.insert(
                                                          env[key].matches.end(),
                                                          value.matches.begin(), value.matches.end());
                                                  }
                                              }
                                              i++;
                                              j++;
                                          }
                                      }
                                      return j == exprList.elements.size();
                                  },
                                  [&](const MacroList&, const MacroAtom&) -> bool { return false; },
                                  // catch all for unexpected types
                                  [&](const auto&, const auto&) -> bool {
                                      throw std::runtime_error("Unhandled types in tryMatch visitor");
                                  } },
        pattern->value, expr->value);

    return { env, success };
}

std::shared_ptr<MacroExpression> transformTemplate(const std::shared_ptr<MacroExpression>& template_expr,
    const MatchEnv& env)
{
    return std::visit(overloaded {
                          [&](const MacroAtom& atom) -> std::shared_ptr<MacroExpression> {
                              auto it = env.find(atom.token.lexeme);
                              if (it != env.end()) {
                                  if (!template_expr->isVariadic) {
                                      if (!it->second.matches.empty()) {
                                          return it->second.matches[0];
                                      } else {
                                          return std::make_shared<MacroExpression>(MacroList {}, false, template_expr->line);
                                      }
                                  }
                              }
                              return std::make_shared<MacroExpression>(
                                  atom, template_expr->isVariadic, template_expr->line);
                          },
                          [&](const MacroList& list) -> std::shared_ptr<MacroExpression> {
                              std::vector<std::shared_ptr<MacroExpression>> transformed_elements;
                              for (size_t i = 0; i < list.elements.size(); ++i) {
                                  auto current_template_element = list.elements[i];

                                  if (current_template_element->isVariadic) {
                                      if (auto* atom = std::get_if<MacroAtom>(&current_template_element->value)) {
                                          auto it = env.find(atom->token.lexeme);
                                          if (it != env.end()) {
                                              transformed_elements.insert(transformed_elements.end(),
                                                  it->second.matches.begin(), it->second.matches.end());
                                          } else {
                                              transformed_elements.push_back(current_template_element);
                                          }
                                      } else if (auto* innerList = std::get_if<MacroList>(&current_template_element->value)) {
                                          size_t variadicCount = 0;
                                          std::vector<std::string> variadicVars;
                                          findVariadics(*innerList, variadicVars, variadicCount, env);
                                          if (variadicCount > 0) {
                                              for (size_t idx = 0; idx < variadicCount; ++idx) {
                                                  MatchEnv tempEnv; // Env for this iteration
                                                  bool possible = true;
                                                  for (const auto& var_name : variadicVars) {
                                                      if (env.count(var_name)) { // Ensure var exists in outer env
                                                          const auto& matches = env.at(var_name).matches;
                                                          if (matches.size() == variadicCount) {
                                                              tempEnv[var_name].matches = { matches[idx] };
                                                          } else if (matches.size() == 1) {
                                                              tempEnv[var_name].matches = { matches[0] };
                                                          } else if (matches.empty() && variadicCount > 0) {
                                                              tempEnv[var_name].matches = {};
                                                          } else {
                                                              possible = false;
                                                              break;
                                                          }
                                                      } else {
                                                          possible = false;
                                                          break;
                                                      }
                                                  }
                                                  if (!possible)
                                                      continue;

                                                  auto inner_template_expr = std::make_shared<MacroExpression>(*innerList, false, current_template_element->line);
                                                  auto transformed_inner = transformTemplate(inner_template_expr, tempEnv);
                                                  transformed_elements.push_back(transformed_inner);
                                              }
                                          } else {
                                              auto inner_template_expr = std::make_shared<MacroExpression>(*innerList, false, current_template_element->line);
                                              transformed_elements.push_back(transformTemplate(inner_template_expr, env));
                                          }
                                      } else {
                                          throw std::runtime_error("Invalid state in transformTemplate variadic list");
                                      }
                                  } else {
                                      transformed_elements.push_back(transformTemplate(current_template_element, env));
                                  }
                              }
                              return std::make_shared<MacroExpression>(
                                  MacroList { std::move(transformed_elements) },
                                  false,
                                  template_expr->line);
                          } },
        template_expr->value);
}

std::shared_ptr<MacroExpression> transformMacroRecursive(
    const std::shared_ptr<MacroExpression>& expr,
    pattern::MacroEnvironment& env)
{
    bool expanded;
    auto current = expr;
    int recursion_depth = 0;
    const int MAX_RECURSION = 100;

    do {
        if (++recursion_depth > MAX_RECURSION) {
            return current;
        }

        expanded = false;
        if (auto* atom = std::get_if<MacroAtom>(&current->value)) {
            if (env.isMacro(atom->token.lexeme)) {
                std::string macro_name_str = atom->token.lexeme;
                auto synt = env.getMacroDefinition(macro_name_str);
                if (!synt || !*synt) {
                    std::cerr << "[ERROR] Macro definition not found or invalid for atom: " << macro_name_str << std::endl;
                    break;
                }
                if (!std::holds_alternative<SyntaxRulesExpression>((*synt)->as)) {
                    break;
                }
                auto rules = std::get<SyntaxRulesExpression>((*synt)->as).rules;
                auto literals = std::get<SyntaxRulesExpression>((*synt)->as).literals;

                for (const auto& rule : rules) {
                    auto patternExpr = fromExpr(rule.pattern);
                    auto [matchEnv, success] = tryMatch(patternExpr, current, literals, macro_name_str);

                    if (success) {
                        printMatchEnv(matchEnv, 2);
                        auto templateMacroExpr = fromExpr(rule.template_expr);
                        current = transformTemplate(templateMacroExpr, matchEnv);
                        expanded = true;
                        break;
                    }
                }
            }
        } else if (auto* list = std::get_if<MacroList>(&current->value)) {
            if (!list->elements.empty()) {
                if (auto* firstAtom = std::get_if<MacroAtom>(&list->elements[0]->value)) {
                    if (env.isMacro(firstAtom->token.lexeme)) {
                        std::string macro_name_str = firstAtom->token.lexeme;
                        auto synt = env.getMacroDefinition(macro_name_str);
                        auto rules = std::get<SyntaxRulesExpression>((*synt)->as).rules;
                        auto literals = std::get<SyntaxRulesExpression>((*synt)->as).literals;

                        for (const auto& rule : rules) {
                            auto patternExpr = fromExpr(rule.pattern);
                            auto [matchEnv, success] = tryMatch(patternExpr, current, literals, macro_name_str);

                            if (success) {
                                auto templateMacroExpr = fromExpr(rule.template_expr);
                                current = transformTemplate(templateMacroExpr, matchEnv);
                                expanded = true;
                                break;
                            }
                        }
                    }
                }

                if (!expanded) {
                    std::vector<std::shared_ptr<MacroExpression>> newElements;
                    bool anySubExpanded = false;
                    for (const auto& elem : list->elements) {
                        auto newElem = transformMacroRecursive(elem, env);
                        if (newElem->toString() != elem->toString()) {
                            anySubExpanded = true;
                        }
                        newElements.push_back(newElem);
                    }
                    if (anySubExpanded) {
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
    auto templateMacroExpr = fromExpr(template_expr);
    auto transformed = transformTemplate(templateMacroExpr, env);
    auto fullyExpanded = transformMacroRecursive(transformed, macroEnv);
    return fullyExpanded;
}

std::vector<std::shared_ptr<Expression>> expandMacros(std::vector<std::shared_ptr<Expression>> exprs)
{
    std::vector<std::shared_ptr<Expression>> toExpand;
    std::vector<std::shared_ptr<Expression>> finalExpandedExprs;
    pattern::MacroEnvironment env;
    for (const auto& expr : exprs) {
        std::visit(overloaded {
                       [&](const DefineSyntaxExpression& de) {
                           if (std::holds_alternative<SyntaxRulesExpression>(de.rule->as)) {
                               auto ruleExpr = std::make_shared<Expression>(*de.rule);
                               env.defineMacro(de.name.lexeme, ruleExpr);
                           } },
                       [&](const auto&) {
                       } },
            expr->as);
    }

    for (const auto& expr : exprs) {
        if (std::holds_alternative<DefineSyntaxExpression>(expr->as)) {
            // skip definitions in this pass
        } else if (std::holds_alternative<VectorExpression>(expr->as)) {
            finalExpandedExprs.push_back(expr);
        } else {
            auto userMacroExpr = fromExpr(expr);
            auto expandedMacro = transformMacroRecursive(userMacroExpr, env);
            std::string expandedString = expandedMacro->toString();
            std::vector<Token> tokens;
            try {
                tokens = scanner::tokenize(expandedString);
            } catch (const std::exception& e) {
                finalExpandedExprs.push_back(expr);
                continue;
            }

            std::vector<std::shared_ptr<Expression>> parsedExprsPtr;
            try {
                parsedExprsPtr = *parse::parse(std::move(tokens));
            } catch (const std::exception& e) {
                finalExpandedExprs.push_back(expr);
                continue;
            }

            if (!parsedExprsPtr.empty()) {
                for (const auto& expandedExpr : parsedExprsPtr) {
                    finalExpandedExprs.push_back(expandedExpr);
                }
            }
        }
    }

    return finalExpandedExprs;
}

} // namespace (macroexp)
