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
                                  return me.syntax.token.lexeme + "...";
                              return me.syntax.token.lexeme;
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
                    auto it = env.find(a->syntax.token.lexeme);
                    if (it != env.end() && isPatternVariable(a->syntax.token, {})) {
                        bool already_found = false;
                        for (const auto& vv : variadicVars) {
                            if (vv == a->syntax.token.lexeme) {
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
                                variadicVars.push_back(a->syntax.token.lexeme);
                            } else if (variadicCount != matches_size) {
                                if (matches_size != 1) {
                                    throw std::runtime_error("Mismatched variadic pattern lengths (" + std::to_string(variadicCount) + " vs " + std::to_string(matches_size) + " for " + a->syntax.token.lexeme + ")");
                                }
                                variadicVars.push_back(a->syntax.token.lexeme);
                            } else {
                                variadicVars.push_back(a->syntax.token.lexeme);
                            }
                        } else if (matches_size == 1) {
                            variadicVars.push_back(a->syntax.token.lexeme);
                            if (variadicCount == 0) {
                                variadicCount = 1;
                            }
                        } else {
                            variadicVars.push_back(a->syntax.token.lexeme);
                            if (variadicCount == 0)
                                variadicCount = 0;
                        }
                    }
                } else if (auto* l2 = std::get_if<MacroList>(&elem->value)) {
                    for (const auto& sub_e : l2->elements) {
                        findVars(sub_e);
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

bool isEllipsis(std::shared_ptr<MacroExpression> me)
{
    return std::visit(overloaded {
                          [](const MacroAtom& me) { return me.syntax.token.type == Tokentype::ELLIPSIS; },
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

    const auto toProcess = ::exprToList(expr);
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
                                      if (atom_ptr->syntax.token.type == Tokentype::ELLIPSIS) {
                                          if (!processed.empty()) {
                                              processed.back()->isVariadic = true;
                                              continue;
                                          } else {
                                              throw std::runtime_error("Invalid ellipsis placement near line " + std::to_string(elem->line));
                                          }
                                      }
                                      if (atom_ptr->syntax.token.type == Tokentype::DOT) {
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
    }
    std::cout << std::endl;
}

void findPatternVariables(
    const std::shared_ptr<MacroExpression>& pattern,
    std::vector<std::string>& vars,
    const std::vector<Token>& literals,
    std::set<std::string>& visited)
{
    if (!pattern)
        return;

    std::visit(overloaded {
                   [&](const MacroAtom& atom) {
                       if (isPatternVariable(atom.syntax.token, literals) && atom.syntax.token.lexeme != "_") {
                           if (visited.insert(atom.syntax.token.lexeme).second) {
                               vars.push_back(atom.syntax.token.lexeme);
                           }
                       }
                   },
                   [&](const MacroList& list) {
                       for (const auto& elem : list.elements) {
                           findPatternVariables(elem, vars, literals, visited);
                       }
                   } },
        pattern->value);
}
void findPatternVariables(
    const std::shared_ptr<MacroExpression>& pattern,
    std::vector<std::string>& vars,
    const std::vector<Token>& literals)
{
    std::set<std::string> visited;
    findPatternVariables(pattern, vars, literals, visited);
}

std::pair<MatchEnv, bool> tryMatch(std::shared_ptr<MacroExpression> pattern,
    std::shared_ptr<MacroExpression> expr,
    const std::vector<Token>& literals,
    const std::string& macroName)
{
    MatchEnv env;
    bool success = visit_many(multi_visitor {
                                  [&](const MacroAtom& patAtom, const MacroAtom& exprAtom) -> bool {
                                      bool isVar = isPatternVariable(patAtom.syntax.token, literals);
                                      if (!isVar) {
                                          return patAtom.syntax.token.lexeme == exprAtom.syntax.token.lexeme;
                                      }
                                      if (patAtom.syntax.token.lexeme != "_") {
                                          env[patAtom.syntax.token.lexeme].matches.push_back(
                                              std::make_shared<MacroExpression>(MacroExpression { exprAtom, false, expr->line }));
                                      }
                                      return true;
                                  },

                                  [&](const MacroAtom& patAtom, const MacroList& exprList) -> bool {
                                      if (isPatternVariable(patAtom.syntax.token, literals)) {
                                          if (patAtom.syntax.token.lexeme != "_") {
                                              env[patAtom.syntax.token.lexeme].matches.push_back(
                                                  std::make_shared<MacroExpression>(MacroExpression { exprList, false, expr->line }));
                                          }
                                          return true;
                                      }
                                      return false;
                                  },

                                  [&](const MacroList& patList, const MacroList& exprList) -> bool {
                                      if (patList.elements.empty() && exprList.elements.empty()) {
                                          return true;
                                      }

                                      size_t i = 0;
                                      size_t j = 0;

                                      while (i < patList.elements.size()) {
                                          if (j >= exprList.elements.size()) {
                                              while (i < patList.elements.size()) {
                                                  if (!patList.elements[i]->isVariadic) {
                                                      return false;
                                                  }
                                                  if (auto* atom = std::get_if<MacroAtom>(&patList.elements[i]->value)) {
                                                      if (isPatternVariable(atom->syntax.token, literals) && atom->syntax.token.lexeme != "_") {
                                                          env.try_emplace(atom->syntax.token.lexeme);
                                                      }
                                                  } else if (auto* list = std::get_if<MacroList>(&patList.elements[i]->value)) {
                                                      std::vector<std::string> zero_vars;
                                                      findPatternVariables(patList.elements[i], zero_vars, literals);
                                                      for (const auto& var_name : zero_vars) {
                                                          env.try_emplace(var_name);
                                                      }
                                                  }
                                                  i++;
                                              }
                                              return true;
                                          }

                                          auto current_pattern_element = patList.elements[i];

                                          if (current_pattern_element->isVariadic) {
                                              current_pattern_element->isVariadic = false;
                                              MatchEnv accumulated_sub_env;
                                              size_t variadic_match_count = 0;
                                              size_t initial_j = j;

                                              if (i + 1 < patList.elements.size()) {
                                                  auto next_pattern_element = patList.elements[i + 1];
                                                  size_t k = j;
                                                  while (k < exprList.elements.size()) {
                                                      auto [nextMatchEnv_lookahead, nextMatched_lookahead] = tryMatch(
                                                          next_pattern_element, exprList.elements[k], literals, macroName);
                                                      if (nextMatched_lookahead) {
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

                                              if (variadic_match_count == 0) {
                                                  std::vector<std::string> zero_vars;
                                                  findPatternVariables(current_pattern_element, zero_vars, literals);
                                                  for (const auto& var_name : zero_vars) {
                                                      if (var_name != "_") {
                                                          env.try_emplace(var_name);
                                                      }
                                                  }
                                              } else {
                                                  for (size_t v_idx = 0; v_idx < variadic_match_count; ++v_idx) {
                                                      auto [subEnv, matched] = tryMatch(
                                                          current_pattern_element, exprList.elements[j], literals, macroName);
                                                      if (!matched) {
                                                          current_pattern_element->isVariadic = true;
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
                                                      env[key].matches.insert(
                                                          env[key].matches.end(), value.matches.begin(), value.matches.end());
                                                  }
                                              }
                                              current_pattern_element->isVariadic = true;
                                              i++;

                                          } else {
                                              auto [subEnv, matched] = tryMatch(
                                                  current_pattern_element, exprList.elements[j], literals, macroName);
                                              if (!matched) {
                                                  return false;
                                              }
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

                                  [&](const MacroList&, const MacroAtom&) -> bool {
                                      return false;
                                  },

                                  [&](const auto&, const auto&) -> bool {
                                      throw std::runtime_error("Unhandled types in tryMatch visitor during pattern matching");
                                  } },
        pattern->value, expr->value);

    return { env, success };
}
std::shared_ptr<MacroExpression> addUsageContextRecursive(
    const std::shared_ptr<MacroExpression>& expr,
    const SyntaxContext& context)
{
    DEBUG_LOG("Adding usage context " << context.toString() << " to " << expr->toString());
    return std::visit(overloaded {
                          [&](const MacroAtom& atom) -> std::shared_ptr<MacroExpression> {
                              if (atom.syntax.token.type == Tokentype::IDENTIFIER) {
                                  HygienicSyntax newSyntax {
                                      atom.syntax.token,
                                      atom.syntax.context.addMarks(context.marks)
                                  };
                                  return std::make_shared<MacroExpression>(
                                      MacroAtom { newSyntax }, expr->isVariadic, expr->line);
                              }
                              return std::make_shared<MacroExpression>(atom, expr->isVariadic, expr->line);
                          },
                          [&](const MacroList& list) -> std::shared_ptr<MacroExpression> {
                              std::vector<std::shared_ptr<MacroExpression>> newElements;
                              newElements.reserve(list.elements.size());
                              for (const auto& elem : list.elements) {
                                  newElements.push_back(addUsageContextRecursive(elem, context));
                              }
                              return std::make_shared<MacroExpression>(
                                  MacroList { std::move(newElements) }, expr->isVariadic, expr->line);
                          } },
        expr->value);
}

HygienicSyntax createFreshSyntaxObject(const Token& token)
{
    return HygienicSyntax { token, SyntaxContext::createFresh() };
}

std::shared_ptr<MacroExpression> transformTemplate(
    const std::shared_ptr<MacroExpression>& template_expr,
    const MatchEnv& env,
    SyntaxContext macroContext)
{
    DEBUG_LOG("Transform template " << template_expr->toString() << " with macro context: " << macroContext.toString());
    return std::visit(overloaded {
                          [&](const MacroAtom& atom) -> std::shared_ptr<MacroExpression> {
                              auto it = env.find(atom.syntax.token.lexeme);
                              if (it != env.end()) {
                                  // For pattern variables, propagate the macro's context
                                  if (!template_expr->isVariadic) {
                                      if (!it->second.matches.empty()) {
                                          // Propagate context to substituted expression
                                          auto match = it->second.matches[0];
                                          return addUsageContextRecursive(match, macroContext);
                                      } else {
                                          return std::make_shared<MacroExpression>(MacroList {}, false, template_expr->line);
                                      }
                                  }
                              } else if (atom.syntax.token.type == Tokentype::IDENTIFIER) {
                                  // For macro-introduced identifiers, merge contexts
                                  HygienicSyntax freshSyntax {
                                      atom.syntax.token,
                                      atom.syntax.context.addMarks(macroContext.marks)
                                  };
                                  return std::make_shared<MacroExpression>(
                                      MacroAtom { freshSyntax }, template_expr->isVariadic, template_expr->line);
                              }

                              return std::make_shared<MacroExpression>(
                                  atom, template_expr->isVariadic, template_expr->line);
                          },
                          [&](const MacroList& list) -> std::shared_ptr<MacroExpression> {
                              std::vector<std::shared_ptr<MacroExpression>> transformed_elements;
                              for (size_t i = 0; i < list.elements.size(); ++i) {
                                  auto current_template_element = list.elements[i];

                                  if (current_template_element->isVariadic) {
                                      // Handle ellipsis (...)
                                      if (auto* atom = std::get_if<MacroAtom>(&current_template_element->value)) {
                                          auto it = env.find(atom->syntax.token.lexeme);
                                          if (it != env.end()) {
                                              // Expand variadic pattern and propagate context
                                              for (const auto& match : it->second.matches) {
                                                  auto transformedMatch = addUsageContextRecursive(match, macroContext);
                                                  transformed_elements.push_back(transformedMatch);
                                              }
                                          }
                                      } else if (auto* innerList = std::get_if<MacroList>(&current_template_element->value)) {
                                          // Handle nested lists with ellipsis
                                          size_t variadicCount = 0;
                                          std::vector<std::string> variadicVars;
                                          findVariadics(*innerList, variadicVars, variadicCount, env);

                                          if (variadicCount > 0) {
                                              // Expand each occurrence
                                              for (size_t idx = 0; idx < variadicCount; ++idx) {
                                                  MatchEnv tempEnv;
                                                  bool possible = true;

                                                  // Build environment for this occurrence
                                                  for (const auto& var_name : variadicVars) {
                                                      if (env.count(var_name)) {
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

                                                  auto inner_template_expr = std::make_shared<MacroExpression>(
                                                      *innerList, false, current_template_element->line);
                                                  auto transformed_inner = transformTemplate(inner_template_expr, tempEnv, macroContext);
                                                  transformed_elements.push_back(transformed_inner);
                                              }
                                          } else {
                                              // No variadic patterns found, transform normally
                                              auto inner_template_expr = std::make_shared<MacroExpression>(
                                                  *innerList, false, current_template_element->line);
                                              transformed_elements.push_back(
                                                  transformTemplate(inner_template_expr, env, macroContext));
                                          }
                                      }
                                  } else {
                                      // Normal (non-variadic) template element
                                      transformed_elements.push_back(
                                          transformTemplate(current_template_element, env, macroContext));
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
    std::shared_ptr<pattern::MacroEnvironment> env)
{
    bool expanded;
    auto current = expr;
    int recursion_depth = 0;
    const int MAX_RECURSION = 100;

    if (!env) {
        return expr;
    }

    do {
        if (++recursion_depth > MAX_RECURSION) {
            std::cerr << "[Warning] Max macro expansion depth exceeded for: " << expr->toString() << std::endl;
            return current;
        }

        expanded = false;
        if (auto* atom = std::get_if<MacroAtom>(&current->value)) {
            // Use the passed-in env (shared_ptr)
            if (env->isMacro(atom->syntax.token.lexeme)) {
                std::string macro_name_str = atom->syntax.token.lexeme;
                auto synt = env->getMacroDefinition(macro_name_str); // Use env->
                if (!synt || !*synt || !std::holds_alternative<SyntaxRulesExpression>((*synt)->as)) {
                    // Handle non-syntax-rules macros or errors
                    break;
                }
                // Extract rules and literals from the macro definition (Expression*)
                auto rules = std::get<SyntaxRulesExpression>((*synt)->as).rules;
                auto literals = std::get<SyntaxRulesExpression>((*synt)->as).literals;
                SyntaxContext macroContext = SyntaxContext::createFresh();

                for (const auto& rule : rules) {
                    auto patternExpr = fromExpr(rule.pattern);
                    auto [matchEnv, success] = tryMatch(patternExpr, current, literals, macro_name_str);
                    if (success) {
                        auto templateMacroExpr = fromExpr(rule.template_expr);
                        current = transformTemplate(templateMacroExpr, matchEnv, macroContext);
                        expanded = true;
                        break;
                    }
                }
            }
        } else if (auto* list = std::get_if<MacroList>(&current->value)) {
            if (!list->elements.empty()) {
                bool list_was_macro_call = false;
                if (auto* firstAtom = std::get_if<MacroAtom>(&list->elements[0]->value)) {
                    // Use the passed-in env (shared_ptr)
                    if (env->isMacro(firstAtom->syntax.token.lexeme)) {
                        std::string macro_name_str = firstAtom->syntax.token.lexeme;
                        auto synt = env->getMacroDefinition(macro_name_str); // Use env->
                        if (synt && *synt && std::holds_alternative<SyntaxRulesExpression>((*synt)->as)) {
                            auto rules = std::get<SyntaxRulesExpression>((*synt)->as).rules;
                            auto literals = std::get<SyntaxRulesExpression>((*synt)->as).literals;
                            SyntaxContext macroContext = SyntaxContext::createFresh();

                            for (const auto& rule : rules) {
                                auto patternExpr = fromExpr(rule.pattern);
                                auto [matchEnv, success] = tryMatch(patternExpr, current, literals, macro_name_str);
                                if (success) {
                                    auto templateMacroExpr = fromExpr(rule.template_expr);
                                    current = transformTemplate(templateMacroExpr, matchEnv, macroContext);
                                    expanded = true;
                                    list_was_macro_call = true; // Mark that this list *was* a macro call
                                    break;
                                }
                            }
                        }
                    }
                }

                if (!list_was_macro_call) { // Only recurse if the list itself wasn't a macro call that expanded
                    std::vector<std::shared_ptr<MacroExpression>> newElements;
                    bool anySubExpanded = false;
                    for (const auto& elem : list->elements) {
                        // Pass the env shared_ptr down recursively
                        auto newElem = transformMacroRecursive(elem, env);
                        if (newElem != elem) {
                            anySubExpanded = true;
                        }
                        newElements.push_back(newElem);
                    }
                    if (anySubExpanded) {
                        current = std::make_shared<MacroExpression>(
                            MacroList { std::move(newElements) },
                            current->isVariadic,
                            current->line);
                        // Set expanded = true to force another pass in case the new structure is a macro call
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
    std::shared_ptr<pattern::MacroEnvironment> macroEnv // Takes shared_ptr
)
{
    auto templateMacroExpr = fromExpr(template_expr);

    SyntaxContext macroContext = SyntaxContext::createFresh();

    auto transformed = transformTemplate(templateMacroExpr, env, macroContext);
    auto fullyExpanded = transformMacroRecursive(transformed, macroEnv);
    return fullyExpanded;
}

std::shared_ptr<Expression> convertMacroResultToExpressionInternal(
    const std::shared_ptr<MacroExpression>& macroResult)
{
    if (!macroResult)
        return nullptr;
    int line = macroResult->line;

    return std::visit(overloaded {
                          [&](const MacroAtom& ma) -> std::shared_ptr<Expression> {
                              return std::make_shared<Expression>(AtomExpression { ma.syntax }, line);
                          },

                          [&](const MacroList& ml) -> std::shared_ptr<Expression> {
                              if (ml.elements.empty()) {
                                  return std::make_shared<Expression>(sExpression { {} }, line);
                              }

                              std::string keyword = getKeyword(ml);
                              if (keyword == "quote") {
                                  return convertQuote(ml, line);
                              } else if (keyword == "set!") {
                                  return convertSet(ml, line);
                              } else if (keyword == "if") {
                                  return convertIf(ml, line);
                              } else if (keyword == "let") {
                                  return convertLet(ml, line);
                              } else if (keyword == "lambda") {
                                  return convertLambda(ml, line);
                              } else if (keyword == "define") {
                                  return convertDefine(ml, line);
                              } else {
                                  std::vector<std::shared_ptr<Expression>> convertedElements;
                                  convertedElements.reserve(ml.elements.size());
                                  for (const auto& elem : ml.elements) {
                                      if (auto convertedElem = convertMacroResultToExpressionInternal(elem)) {
                                          convertedElements.push_back(convertedElem);
                                      } else {
                                          throw std::runtime_error("Null expression encountered during sExpression conversion");
                                      }
                                  }
                                  return std::make_shared<Expression>(sExpression { std::move(convertedElements) }, line);
                              }
                          },

                          [&](const auto& other) -> std::shared_ptr<Expression> {
                              throw std::runtime_error("Internal Error: Unexpected variant type found within MacroExpression");
                          } },
        macroResult->value);
}

std::string getKeyword(const MacroList& ml)
{
    if (ml.elements.empty())
        return "";
    const auto& firstElement = ml.elements[0];
    if (!firstElement)
        return "";

    if (const MacroAtom* atom = std::get_if<MacroAtom>(&firstElement->value)) {
        return atom->syntax.token.lexeme;
    }
    return "";
}

std::shared_ptr<Expression> convertQuote(const MacroList& ml, int line)
{
    if (ml.elements.size() != 2)
        throw std::runtime_error("Invalid quote structure during conversion");
    auto datum = convertMacroResultToExpressionInternal(ml.elements[1]);
    if (!datum)
        throw std::runtime_error("Invalid quote datum during conversion");
    return std::make_shared<Expression>(QuoteExpression { datum }, line);
}

std::shared_ptr<Expression> convertSet(const MacroList& ml, int line)
{
    if (ml.elements.size() != 3)
        throw std::runtime_error("Invalid set! structure during conversion");
    auto identExpr = convertMacroResultToExpressionInternal(ml.elements[1]);
    auto valueExpr = convertMacroResultToExpressionInternal(ml.elements[2]);
    if (!identExpr || !valueExpr || !std::holds_alternative<AtomExpression>(identExpr->as)) {
        throw std::runtime_error("Invalid set! parts during conversion (expected identifier and value)");
    }
    HygienicSyntax identifier = std::get<AtomExpression>(identExpr->as).value;
    return std::make_shared<Expression>(SetExpression { identifier, valueExpr }, line);
}

std::shared_ptr<Expression> convertIf(const MacroList& ml, int line)
{
    if (ml.elements.size() < 3 || ml.elements.size() > 4)
        throw std::runtime_error("Invalid if structure during conversion");
    auto condition = convertMacroResultToExpressionInternal(ml.elements[1]);
    auto thenBranch = convertMacroResultToExpressionInternal(ml.elements[2]);
    std::optional<std::shared_ptr<Expression>> elseBranchOpt = std::nullopt;

    if (!condition || !thenBranch)
        throw std::runtime_error("Invalid if parts during conversion");

    if (ml.elements.size() == 4) {
        auto elseConv = convertMacroResultToExpressionInternal(ml.elements[3]);
        if (!elseConv)
            throw std::runtime_error("Invalid if else part during conversion");
    }
    return std::make_shared<Expression>(IfExpression { condition, thenBranch, elseBranchOpt }, line);
}

std::pair<std::vector<HygienicSyntax>, bool> parseMacroParameters(
    const std::shared_ptr<MacroExpression>& paramsMacroExpr)
{
    std::vector<HygienicSyntax> params;
    bool isVariadic = false;

    if (!paramsMacroExpr) {
        throw std::runtime_error("Invalid null parameter list structure during conversion");
    }

    if (const MacroList* paramList = std::get_if<MacroList>(&paramsMacroExpr->value)) {
        for (size_t i = 0; i < paramList->elements.size(); ++i) {
            const auto& paramNode = paramList->elements[i];
            if (!paramNode)
                throw std::runtime_error("Null parameter node during conversion");

            if (const MacroAtom* paramAtom = std::get_if<MacroAtom>(&paramNode->value)) {
                if (paramAtom->syntax.token.type == Tokentype::DOT) {
                    if (isVariadic)
                        throw std::runtime_error("Multiple dots found in params");
                    if (i == 0 || i != paramList->elements.size() - 2) {
                        throw std::runtime_error("Invalid dot placement in lambda params during conversion");
                    }
                    isVariadic = true;
                    continue;
                }
                if (isVariadic) {
                    params.push_back(paramAtom->syntax);
                    if (i != paramList->elements.size() - 1)
                        throw std::runtime_error("More tokens found after dot parameter");
                    break;
                }
                params.push_back(paramAtom->syntax);
            } else {
                throw std::runtime_error("Non-atom found in parameter list structure during conversion");
            }
        }

    } else if (const MacroAtom* paramAtom = std::get_if<MacroAtom>(&paramsMacroExpr->value)) {
        params.push_back(paramAtom->syntax);
        isVariadic = false;
    } else {
        throw std::runtime_error("Unsupported parameter structure during conversion (neither list nor atom)");
    }
    return { params, isVariadic };
}

std::shared_ptr<Expression> convertLambda(const MacroList& ml, int line)
{
    if (ml.elements.size() < 2)
        throw std::runtime_error("Invalid lambda structure (needs params)");
    auto [params, isVariadic] = parseMacroParameters(ml.elements[1]);

    std::vector<std::shared_ptr<Expression>> body;
    body.reserve(ml.elements.size() - 2);
    for (size_t i = 2; i < ml.elements.size(); ++i) {
        if (auto converted = convertMacroResultToExpressionInternal(ml.elements[i])) {
            body.push_back(converted);
        } else {
            throw std::runtime_error("Invalid lambda body element during conversion");
        }
    }
    wrapLastBodyExpression(body);

    return std::make_shared<Expression>(LambdaExpression { params, std::move(body), isVariadic }, line);
}

std::shared_ptr<Expression> convertDefine(const MacroList& ml, int line)
{
    if (ml.elements.size() < 3)
        throw std::runtime_error("Invalid define structure during conversion");

    if (ml.elements[1] && std::holds_alternative<MacroList>(ml.elements[1]->value)) {
        const auto& procHeaderNode = ml.elements[1];
        const auto& procHeaderList = std::get<MacroList>(procHeaderNode->value);

        if (procHeaderList.elements.empty())
            throw std::runtime_error("Empty define procedure header");

        HygienicSyntax name;
        if (procHeaderList.elements[0] && std::holds_alternative<MacroAtom>(procHeaderList.elements[0]->value)) {
            name = std::get<MacroAtom>(procHeaderList.elements[0]->value).syntax;
        } else {
            throw std::runtime_error("Expected procedure name atom in define");
        }

        auto paramsOnlyList = std::make_shared<MacroList>();
        paramsOnlyList->elements.assign(procHeaderList.elements.begin() + 1, procHeaderList.elements.end());
        int paramsLine = procHeaderNode->line;
        auto paramsMacroExpr = std::make_shared<MacroExpression>(*paramsOnlyList, false, paramsLine);

        auto [params, isVariadic] = parseMacroParameters(paramsMacroExpr);

        std::vector<std::shared_ptr<Expression>> body;
        body.reserve(ml.elements.size() - 2);
        for (size_t i = 2; i < ml.elements.size(); ++i) {
            if (auto converted = convertMacroResultToExpressionInternal(ml.elements[i])) {
                body.push_back(converted);
            } else {
                throw std::runtime_error("Invalid define body element during conversion");
            }
        }
        if (body.empty())
            throw std::runtime_error("Define procedure requires a body");
        wrapLastBodyExpression(body);

        return std::make_shared<Expression>(DefineProcedure { name, params, std::move(body), isVariadic }, line);

    } else if (ml.elements[1] && std::holds_alternative<MacroAtom>(ml.elements[1]->value)) {
        if (ml.elements.size() != 3)
            throw std::runtime_error("Invalid define variable structure");
        HygienicSyntax name = std::get<MacroAtom>(ml.elements[1]->value).syntax;
        auto value = convertMacroResultToExpressionInternal(ml.elements[2]);
        if (!value)
            throw std::runtime_error("Invalid define value part during conversion");
        return std::make_shared<Expression>(DefineExpression { name, value }, line);
    } else {
        throw std::runtime_error("Invalid structure after define keyword during conversion");
    }
}

std::shared_ptr<Expression> convertLet(const MacroList& ml, int line)
{
    if (ml.elements.size() < 2) {
        throw std::runtime_error("Invalid let structure during conversion (too few parts)");
    }

    std::optional<HygienicSyntax> letNameSyntax = std::nullopt;
    size_t bindingsIndex = 0;
    size_t bodyStartIndex = 0;

    const auto& secondElementNode = ml.elements[1];
    if (!secondElementNode)
        throw std::runtime_error("Internal error: null node after let");

    if (const MacroAtom* nameAtom = std::get_if<MacroAtom>(&secondElementNode->value)) {
        letNameSyntax = nameAtom->syntax;
        bindingsIndex = 2;
        bodyStartIndex = 3;
        if (ml.elements.size() < 3) {
            throw std::runtime_error("Invalid named let structure (missing bindings list)");
        }
    } else if (std::holds_alternative<MacroList>(secondElementNode->value)) {
        letNameSyntax = std::nullopt;
        bindingsIndex = 1;
        bodyStartIndex = 2;
    } else {
        throw std::runtime_error("Invalid let structure: expected identifier or bindings list after 'let'");
    }

    if (bindingsIndex >= ml.elements.size()) {
        throw std::runtime_error("Missing bindings list in let structure");
    }
    const auto& bindingsNode = ml.elements[bindingsIndex];
    if (!bindingsNode || !std::holds_alternative<MacroList>(bindingsNode->value)) {
        throw std::runtime_error("Expected list structure for let bindings");
    }
    const auto& bindingsList = std::get<MacroList>(bindingsNode->value);

    std::vector<std::pair<HygienicSyntax, std::shared_ptr<Expression>>> bindings;
    bindings.reserve(bindingsList.elements.size());
    for (const auto& bindingPairNode : bindingsList.elements) {
        if (!bindingPairNode || !std::holds_alternative<MacroList>(bindingPairNode->value)) {
            throw std::runtime_error("Expected list for let binding pair");
        }
        const auto& bindingPairList = std::get<MacroList>(bindingPairNode->value);
        if (bindingPairList.elements.size() != 2) {
            throw std::runtime_error("Let binding pair must have exactly size 2 (variable value)");
        }

        if (!bindingPairList.elements[0] || !std::holds_alternative<MacroAtom>(bindingPairList.elements[0]->value)) {
            throw std::runtime_error("Expected identifier atom for let binding variable");
        }
        HygienicSyntax varSyntax = std::get<MacroAtom>(bindingPairList.elements[0]->value).syntax;
        auto valExpr = convertMacroResultToExpressionInternal(bindingPairList.elements[1]);
        if (!valExpr)
            throw std::runtime_error("Invalid let binding value during conversion");

        bindings.push_back({ varSyntax, valExpr });
    }

    std::vector<std::shared_ptr<Expression>> body;
    body.reserve(ml.elements.size() - bodyStartIndex);
    for (size_t i = bodyStartIndex; i < ml.elements.size(); ++i) {
        if (auto converted = convertMacroResultToExpressionInternal(ml.elements[i])) {
            body.push_back(converted);
        } else {
            throw std::runtime_error("Invalid let body element during conversion");
        }
    }
    wrapLastBodyExpression(body);
    return std::make_shared<Expression>(LetExpression { letNameSyntax, std::move(bindings), std::move(body) }, line);
}

void wrapLastBodyExpression(std::vector<std::shared_ptr<Expression>>& body)
{
    if (!body.empty() && body.back()) {
        if (!std::holds_alternative<TailExpression>(body.back()->as)) {
            body.back() = std::make_shared<Expression>(TailExpression { body.back() }, body.back()->line);
        }
    }
}

std::shared_ptr<Expression> convertMacroResultToExpression(
    const std::shared_ptr<MacroExpression>& macroResult)
{
    try {
        return convertMacroResultToExpressionInternal(macroResult);
    } catch (const std::exception& e) {
        std::cerr << "[Error] Macro Conversion Failed: " << e.what();
        std::cerr << " (Line approx " << (macroResult ? macroResult->line : -1) << ")" << std::endl;
        std::cerr << " Input: " << (macroResult ? macroResult->toString() : "null") << std::endl;
        return nullptr;
    }
}

std::vector<std::shared_ptr<Expression>> expandMacros(
    const std::vector<std::shared_ptr<Expression>>& exprs,
    std::shared_ptr<pattern::MacroEnvironment> env)
{
    std::vector<std::shared_ptr<Expression>> finalExpandedExprs;
    auto currentMacroEnv = env ? env : std::make_shared<pattern::MacroEnvironment>();

    for (const auto& expr : exprs) {
        if (std::holds_alternative<DefineSyntaxExpression>(expr->as)) {
            continue;
        }

        try {
            auto userMacroExpr = fromExpr(expr);
            if (!userMacroExpr) {
                std::cerr << "[Warning] fromExpr failed for: " << expr->toString() << std::endl;
                finalExpandedExprs.push_back(expr);
                continue;
            }

            auto expandedMacro = transformMacroRecursive(userMacroExpr, currentMacroEnv);
            if (!expandedMacro) {
                finalExpandedExprs.push_back(expr);
                std::cerr << "[Warning] transformMacroRecursive returned null for: " << expr->toString() << std::endl;
                continue;
            }

            auto finalExpr = convertMacroResultToExpression(expandedMacro);
            if (finalExpr) {
                finalExpandedExprs.push_back(finalExpr);
            } else {
                finalExpandedExprs.push_back(expr);
                std::cerr << "[Warning] Macro expansion conversion failed for: " << expr->toString() << ", keeping original." << std::endl;
            }
        } catch (const std::exception& e) {
            std::cerr << "[Error] Macro expansion failed for expression near line " << expr->line << ": " << e.what() << std::endl;
            finalExpandedExprs.push_back(expr);
        }
    }
    return finalExpandedExprs;
}
} // namespace macroexp
