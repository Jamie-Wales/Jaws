#include "MacroTraits.h"
#include "Expression.h"
#include "Syntax.h"
#include "Token.h"
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
#define DEBUG_LOGGING
static int transform_depth = 0;
struct TransformTracer {
    std::string name;
    bool entered = false;
    TransformTracer(const std::string& n)
        : name(n) { /*...*/ };
    ~TransformTracer() { /*...*/ };
};
#define TRACE_TRANSFORM(name) TransformTracer _tracer(name)
#ifdef DEBUG_LOGGING
#define DEBUG_LOG(x)                              \
    do {                                          \
        for (int i = 0; i < transform_depth; ++i) \
            std::cerr << "  ";                    \
        std::cerr << "(MACRO) " << x << "\n";     \
        std::cerr.flush();                        \
    } while (0)
#else
#define DEBUG_LOG(x)
#endif
template <typename T>
std::string vectorToString(const std::vector<T>& vec, const std::string& delim = ", ")
{
    std::stringstream ss;
    ss << "[";
    for (size_t i = 0; i < vec.size(); ++i) {
        if (i > 0)
            ss << delim;
        if constexpr (std::is_same_v<T, Token>) {
            ss << vec[i].lexeme;
        } else if constexpr (requires(const T& t) { t->toString(); }) {
            ss << (vec[i] ? vec[i]->toString() : "null");
        } else if constexpr (requires(const T& t) { t.toString(); }) {
            ss << vec[i].toString();
        } else if constexpr (requires(const T& t) { ss << t; }) {
            ss << vec[i];
        } else {
            ss << "<unstringable>";
        }
    }
    ss << "]";
    return ss.str();
}
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
                    auto matches_opt = env.lookup(a->syntax.token.lexeme);
                    if (matches_opt && isPatternVariable(a->syntax.token, {})) {
                        bool already_found = false;
                        for (const auto& vv : variadicVars) {
                            if (vv == a->syntax.token.lexeme) {
                                already_found = true;
                                break;
                            }
                        }
                        if (already_found)
                            return;

                        size_t matches_size = matches_opt->matches.size();
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
    variadicVars.erase(std::unique(variadicVars.begin(), variadicVars.end()),
        variadicVars.end());
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

bool isPatternVariable(const Token& token,
    const std::vector<Token>& literals)
{
    if (token.lexeme == "_") {
        return true;
    }
    if (token.lexeme == ".") {
        return false;
    }

    bool isLiteral = std::find_if(literals.begin(), literals.end(),
                         [&](const Token& lit) { return lit.lexeme == token.lexeme; })
        != literals.end(); // Found in literals

    if (isLiteral) {
        DEBUG_LOG("isPatternVariable: Token '" << token.lexeme << "' found in literals list. Not a variable."); // Optional log
        return false;
    }
    bool result = (token.type == Tokentype::IDENTIFIER || token.type == Tokentype::ELSE || Tokentype::BEGIN == token.type);
    DEBUG_LOG("isPatternVariable: Token '" << token.lexeme << "' isIdentifier=" << (token.type == Tokentype::IDENTIFIER) << ", isLiteral=false, isKeyword=false, isDot=false, isWildcard=false. Result: " << result); // Optional log
    return result;
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
    for (const auto& [variable, pattern] : env.getAllBindings()) {
        std::cout << indent << "Variable: " << variable << std::endl;
        std::cout << indent << "  Matches (" << pattern.matches.size() << "):" << std::endl;
        for (size_t i = 0; i < pattern.matches.size(); i++) {
            std::cout << indent << "    [" << i << "]: " << pattern.matches[i]->toString() << std::endl;
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

std::shared_ptr<MacroExpression> createNonVariadicCopy(const std::shared_ptr<MacroExpression>& expr)
{
    return std::visit(overloaded {
                          [&](const MacroAtom& atom) -> std::shared_ptr<MacroExpression> {
                              return std::make_shared<MacroExpression>(atom, false, expr->line);
                          },
                          [&](const MacroList& list) -> std::shared_ptr<MacroExpression> {
                              return std::make_shared<MacroExpression>(list, false, expr->line);
                          } },
        expr->value);
}

std::pair<MatchEnv, bool> tryMatch(
    std::shared_ptr<MacroExpression> pattern,
    std::shared_ptr<MacroExpression> expr,
    const std::vector<Token>& literals,
    const std::string& macroName)
{
    DEBUG_LOG("--- tryMatch invoked ---");
    DEBUG_LOG("  Pattern: " << (pattern ? pattern->toString() : "null"));
    DEBUG_LOG("  Expr:    " << (expr ? expr->toString() : "null"));
    DEBUG_LOG("  Literals: " << vectorToString(literals));

    MatchEnv env;
    bool success = false;

    if (!pattern || !expr) {
        DEBUG_LOG("  Pattern or Expr is null. Match FAILED.");
        return { env, false };
    }

    success = visit_many(multi_visitor {
                             // --- Case 1: Pattern Atom vs Expression Atom ---
                             [&](const MacroAtom& patAtom, const MacroAtom& exprAtom) -> bool {
                                 DEBUG_LOG("  [Atom vs Atom]");
                                 DEBUG_LOG("    Pat Atom: " << patAtom.syntax.token.lexeme);
                                 DEBUG_LOG("    Expr Atom: " << exprAtom.syntax.token.lexeme);

                                 bool isVar = isPatternVariable(patAtom.syntax.token, literals);
                                 DEBUG_LOG("    Is Pattern Variable? " << (isVar ? "Yes" : "No"));

                                 if (!isVar) {
                                     bool match = patAtom.syntax.token.lexeme == exprAtom.syntax.token.lexeme;
                                     DEBUG_LOG("    Literal Match? " << (match ? "Yes" : "No"));
                                     return match;
                                 } else {
                                     if (patAtom.syntax.token.lexeme != "_") {
                                         DEBUG_LOG("    Binding var '" << patAtom.syntax.token.lexeme << "' to atom: " << exprAtom.syntax.token.lexeme);
                                         auto matchedExpr = std::make_shared<MacroExpression>(exprAtom, false, expr->line);
                                         env.addMatch(patAtom.syntax.token.lexeme, matchedExpr);
                                     } else {
                                         DEBUG_LOG("    Ignoring wildcard '_' match");
                                     }
                                     return true;
                                 }
                             },

                             // --- Case 2: Pattern Atom vs Expression List ---
                             [&](const MacroAtom& patAtom, const MacroList& exprList) -> bool {
                                 DEBUG_LOG("  [Atom vs List]");
                                 DEBUG_LOG("    Pat Atom: " << patAtom.syntax.token.lexeme);
                                 DEBUG_LOG("    Expr List: " << expr->toString());

                                 bool isVar = isPatternVariable(patAtom.syntax.token, literals);
                                 DEBUG_LOG("    Is Pattern Variable? " << (isVar ? "Yes" : "No"));

                                 if (isVar) {
                                     if (patAtom.syntax.token.lexeme != "_") {
                                         DEBUG_LOG("    Binding var '" << patAtom.syntax.token.lexeme << "' to list: " << expr->toString());
                                         auto matchedExpr = std::make_shared<MacroExpression>(exprList, false, expr->line);
                                         env.addMatch(patAtom.syntax.token.lexeme, matchedExpr);
                                     }
                                     return true;
                                 }
                                 DEBUG_LOG("    Literal atom cannot match list. Match FAILED.");
                                 return false;
                             },

                             // --- Case 3: Pattern List vs Expression List ---
                             [&](const MacroList& patList, const MacroList& exprList) -> bool {
                                 DEBUG_LOG("  [List vs List]");
                                 DEBUG_LOG("    Pat List: " << pattern->toString());
                                 DEBUG_LOG("    Expr List: " << expr->toString());

                                 env.pushScope(); // Create new scope for this list match

                                 if (patList.elements.empty() && exprList.elements.empty()) {
                                     DEBUG_LOG("    Both lists empty. Match SUCCESS.");
                                     return true;
                                 }

                                 size_t i = 0; // Pattern index
                                 size_t j = 0; // Expression index

                                 while (i < patList.elements.size()) {
                                     if (j >= exprList.elements.size()) {
                                         // Check remaining pattern elements must be variadic with zero matches
                                         while (i < patList.elements.size()) {
                                             if (!patList.elements[i]->isVariadic) {
                                                 env.popScope();
                                                 return false;
                                             }
                                             // Handle zero matches for variadic pattern
                                             if (auto* atom = std::get_if<MacroAtom>(&patList.elements[i]->value)) {
                                                 if (atom->syntax.token.lexeme != "_") {
                                                     // Create empty matches for the variable
                                                     DEBUG_LOG("    Creating empty matches for variadic var: " << atom->syntax.token.lexeme);
                                                     env.bind(atom->syntax.token.lexeme, PatternMatches {});
                                                 }
                                             }
                                             i++;
                                         }
                                         break;
                                     }

                                     auto currentPattern = patList.elements[i];
                                     auto currentExpr = exprList.elements[j];

                                     if (currentPattern->isVariadic) {
                                         DEBUG_LOG("    Handling variadic pattern at index " << i);

                                         // Look ahead for next non-variadic pattern
                                         size_t next_pattern_idx = i + 1;
                                         while (next_pattern_idx < patList.elements.size() && patList.elements[next_pattern_idx]->isVariadic) {
                                             next_pattern_idx++;
                                         }

                                         // Calculate how many expressions this variadic pattern should consume
                                         size_t remaining_exprs = exprList.elements.size() - j;
                                         size_t min_required = patList.elements.size() - next_pattern_idx;
                                         if (remaining_exprs < min_required) {
                                             env.popScope();
                                             return false;
                                         }

                                         size_t max_to_consume = remaining_exprs - min_required;

                                         // Create a non-variadic version of the pattern for matching
                                         auto nonVariadicPattern = createNonVariadicCopy(currentPattern);

                                         MatchEnv variadicEnv;
                                         for (size_t k = 0; k < max_to_consume; k++) {
                                             auto [subEnv, matched] = tryMatch(
                                                 nonVariadicPattern,
                                                 exprList.elements[j + k],
                                                 literals,
                                                 macroName);

                                             if (!matched)
                                                 break;

                                             variadicEnv.merge(subEnv);
                                         }

                                         // Merge variadic matches into current environment
                                         env.merge(variadicEnv);

                                         j += max_to_consume;
                                         i++;
                                     } else {
                                         auto [subEnv, matched] = tryMatch(
                                             currentPattern,
                                             currentExpr,
                                             literals,
                                             macroName);

                                         if (!matched) {
                                             env.popScope();
                                             return false;
                                         }

                                         env.merge(subEnv);
                                         i++;
                                         j++;
                                     }
                                 }

                                 bool finalSuccess = (j == exprList.elements.size());
                                 if (!finalSuccess) {
                                     env.popScope();
                                 }
                                 return finalSuccess;
                             },
                             [&](const MacroList& patList, const MacroAtom& exprAtom) -> bool {
                                 DEBUG_LOG("  [List vs Atom]");
                                 DEBUG_LOG("    List pattern cannot match atomic expression. Match FAILED.");
                                 return false;
                             } },
        pattern->value, expr->value);

    if (!success) {
        DEBUG_LOG("  Match failed, discarding environment");
    } else {
        DEBUG_LOG("  Match succeeded with environment:");
        auto bindings = env.getAllBindings();
        for (const auto& [var, matches] : bindings) {
            DEBUG_LOG("    " << var << ": " << matches.matches.size() << " matches");
        }
    }

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

HygienicSyntax createFreshSyntaxObject(const Token& token, SyntaxContext context)
{
    return HygienicSyntax { token, context };
}

bool containsRepeatingVariable(
    const std::shared_ptr<MacroExpression>& expr,
    const std::set<std::string>& repeating_pattern_vars) // Set of names of vars like "var"
{
    if (!expr)
        return false;
    bool found = false;
    std::set<const MacroExpression*> visited; // Avoid infinite recursion on cycles

    std::function<void(const std::shared_ptr<MacroExpression>&)> check =
        [&](const std::shared_ptr<MacroExpression>& current) {
            if (!current || found || !visited.insert(current.get()).second)
                return;

            std::visit(overloaded {
                           [&](const MacroAtom& atom) {
                               if (repeating_pattern_vars.count(atom.syntax.token.lexeme)) {
                                   DEBUG_LOG("  Helper found repeating var: " << atom.syntax.token.lexeme);
                                   found = true;
                               }
                           },
                           [&](const MacroList& lst) {
                               for (const auto& sub_elem : lst.elements) {
                                   check(sub_elem); // Recurse
                                   if (found)
                                       return; // Optimization
                               }
                           } },
                current->value);
        };
    check(expr);
    return found;
}
std::shared_ptr<MacroExpression> expandParallel(
    const std::shared_ptr<MacroExpression>& template_part,
    MatchEnv& pattern_env,
    size_t iteration,
    SyntaxContext macroContext,
    const std::vector<std::string>& pattern_vars_in_group,
    const std::vector<std::string>& template_vars_in_group,
    const PatternVariableInfo& pattern_info)
{
    std::string node_str = template_part ? template_part->toString() : "null";
    TRACE_TRANSFORM("expandParallel k=" + std::to_string(iteration) + " on: " + node_str);

    if (!template_part)
        return nullptr;

    MatchEnv iter_env;

    auto all_accessible_bindings = pattern_env.getAllBindings();

    for (const auto& [var_name, matches] : all_accessible_bindings) {
        if (var_name == "_")
            continue;

        PatternMatches iter_matches;
        bool is_controlling_var = std::find(pattern_vars_in_group.begin(), pattern_vars_in_group.end(), var_name) != pattern_vars_in_group.end();

        if (is_controlling_var) {
            if (matches.matches.size() > iteration) {
                iter_matches.matches.push_back(matches.matches[iteration]);
                DEBUG_LOG("  iter_env: Bound controlling var '" << var_name << "' to k=" << iteration << " match: " << iter_matches.matches[0]->toString());
            } else {
                DEBUG_LOG("  Error: Not enough matches for controlling var '" << var_name << "' in iteration " << iteration);
                throw std::runtime_error("Internal error: Not enough matches during parallel expansion for controlling var " + var_name);
            }
        } else {
            if (matches.matches.size() == 1) {
                iter_matches.matches.push_back(matches.matches[0]);
                DEBUG_LOG("  iter_env: Bound non-controlling var '" << var_name << "' to single match: " << iter_matches.matches[0]->toString());
            } else if (matches.matches.empty()) {
                DEBUG_LOG("  iter_env: Non-controlling var '" << var_name << "' has 0 matches. Binding empty.");
            } else {
                if (!matches.matches.empty()) {
                    iter_matches.matches.push_back(matches.matches[0]);
                    DEBUG_LOG("  iter_env: Warning: Non-controlling var '" << var_name << "' has >1 matches. Using first for iter_env.");
                } else {
                    DEBUG_LOG("  iter_env: Non-controlling var '" << var_name << "' has 0 matches. Binding empty.");
                }
            }
        }
        iter_env.bind(var_name, iter_matches);
    }
    for (const auto& tvar_name : template_vars_in_group) {
        if (tvar_name == "_")
            continue;
        if (!iter_env.isBound(tvar_name)) {
            int line = template_part->line;
            Token placeholder_token = { Tokentype::IDENTIFIER, tvar_name, line, 0 }; // Column is dummy
            SyntaxContext base_placeholder_context = SyntaxContext::createFresh();
            SyntaxContext combined_context = base_placeholder_context.addMarks(macroContext.marks);
            HygienicSyntax hygienic_placeholder_syntax = createFreshSyntaxObject(placeholder_token, combined_context);
            auto placeholder_expr = std::make_shared<MacroExpression>(MacroAtom { hygienic_placeholder_syntax }, false, line);
            PatternMatches placeholder_match;
            placeholder_match.matches.push_back(placeholder_expr);
            iter_env.bind(tvar_name, placeholder_match);
            DEBUG_LOG("  iter_env: Bound template var '" << tvar_name << "' to placeholder: " << placeholder_expr->toString());
        } else {
            DEBUG_LOG("  Warning: Template variable '" << tvar_name << "' conflicts with existing pattern variable binding in iter_env.");
        }
    }

    return transformTemplate(template_part, iter_env, macroContext, pattern_info);
}

// Helper function to find variables that control a variadic pattern
void findControllingVariables(
    const std::shared_ptr<MacroExpression>& expr,
    const MatchEnv& env,
    std::set<std::string>& vars)
{
    if (!expr)
        return;

    std::visit(overloaded {
                   [&](const MacroAtom& atom) {
                       if (atom.syntax.token.type == Tokentype::IDENTIFIER) {
                           auto matches = env.lookup(atom.syntax.token.lexeme);
                           if (matches && matches->matches.size() > 1) {
                               // This variable has multiple matches, so it controls expansion
                               vars.insert(atom.syntax.token.lexeme);
                               DEBUG_LOG("      Found controlling variable: " << atom.syntax.token.lexeme
                                                                              << " with " << matches->matches.size() << " matches");
                           }
                       }
                   },
                   [&](const MacroList& list) {
                       for (const auto& elem : list.elements) {
                           findControllingVariables(elem, env, vars);
                       }
                   } },
        expr->value);
}

// Determine if two variadic elements belong to the same pattern
bool sameControllingPattern(
    const std::set<std::string>& vars1,
    const std::set<std::string>& vars2,
    const MatchEnv& env)
{
    // If the sets are equal, they definitely belong to the same pattern
    if (vars1 == vars2)
        return true;

    // If both sets have controlling variables, check if they share any
    if (!vars1.empty() && !vars2.empty()) {
        std::set<std::string> intersection;
        std::set_intersection(
            vars1.begin(), vars1.end(),
            vars2.begin(), vars2.end(),
            std::inserter(intersection, intersection.begin()));

        return !intersection.empty();
    }

    // If one set is empty but the other isn't, they're different patterns
    if (vars1.empty() != vars2.empty()) {
        return false;
    }

    // Both sets are empty - consider them part of the same pattern
    // This handles cases like (a...) (b...) where neither has controlling vars
    return true;
}

// Collect all variables in an expression and their match counts
void collectVariables(
    const std::shared_ptr<MacroExpression>& expr,
    const MatchEnv& env,
    std::set<std::string>& all_vars,
    std::map<std::string, size_t>& var_match_counts)
{
    if (!expr)
        return;

    std::visit(overloaded {
                   [&](const MacroAtom& atom) {
                       if (atom.syntax.token.type == Tokentype::IDENTIFIER && atom.syntax.token.lexeme != "_") {
                           all_vars.insert(atom.syntax.token.lexeme);
                           auto matches = env.lookup(atom.syntax.token.lexeme);
                           var_match_counts[atom.syntax.token.lexeme] = matches ? matches->matches.size() : 0;

                           DEBUG_LOG("    Variable '" << atom.syntax.token.lexeme
                                                      << "' has " << var_match_counts[atom.syntax.token.lexeme]
                                                      << " matches in env");
                       }
                   },
                   [&](const MacroList& list) {
                       for (const auto& elem : list.elements) {
                           collectVariables(elem, env, all_vars, var_match_counts);
                       }
                   } },
        expr->value);
}

// Setup an environment for a specific iteration of a variadic pattern
void setupIterationEnvironment(
    const MatchEnv& source_env,
    MatchEnv& iter_env,
    size_t iteration)
{
    auto all_bindings = source_env.getAllBindings();

    for (const auto& [var_name, matches] : all_bindings) {
        if (var_name == "_")
            continue;

        PatternMatches iter_matches;

        if (matches.matches.size() > 1 && iteration < matches.matches.size()) {
            // Multi-match variable: use k-th match
            iter_matches.matches.push_back(matches.matches[iteration]);
            DEBUG_LOG("      Bound var '" << var_name << "' to k=" << iteration << " match");
        } else if (!matches.matches.empty()) {
            // Single-match variable: always use first match
            iter_matches.matches.push_back(matches.matches[0]);
            DEBUG_LOG("      Bound var '" << var_name << "' to first match");
        } else {
            DEBUG_LOG("      Var '" << var_name << "' has no matches, binding empty");
        }

        iter_env.bind(var_name, iter_matches);
    }
}

// Expand a group of variadic patterns that belong together
void expandVariadicPatternGroup(
    const std::vector<std::shared_ptr<MacroExpression>>& pattern_group,
    MatchEnv& env,
    SyntaxContext macroContext,
    const PatternVariableInfo& pattern_info,
    std::vector<std::shared_ptr<MacroExpression>>& result)
{
    DEBUG_LOG("  Processing variadic pattern group with " << pattern_group.size() << " elements");

    // Collect all variables in this pattern group
    std::set<std::string> all_vars;
    std::map<std::string, size_t> var_match_counts;

    for (const auto& elem : pattern_group) {
        auto non_variadic = createNonVariadicCopy(elem);
        collectVariables(non_variadic, env, all_vars, var_match_counts);
    }

    // Skip the pattern if any ellipsis variables have zero matches
    for (const auto& var : all_vars) {
        if (pattern_info.ellipsis_vars.count(var) > 0 && var_match_counts[var] == 0) {
            DEBUG_LOG("  Skipping variadic pattern - ellipsis variable '" << var << "' has zero matches");
            return;
        }
    }

    // Determine expansion count
    size_t max_match_count = 0;
    bool expansion_count_determined = false;

    for (const auto& [var, count] : var_match_counts) {
        if (count > 1) {
            max_match_count = std::max(max_match_count, count);
            expansion_count_determined = true;
            DEBUG_LOG("  Variable '" << var << "' has " << count << " matches - setting expansion count");
        }
    }

    if (!expansion_count_determined && env.getAllBindings().empty()) {
        DEBUG_LOG("  No variables have multiple matches and env is empty");
        max_match_count = 0;
    }

    if (max_match_count == 0) {
        max_match_count = 1;
        DEBUG_LOG("  No pattern variables with multiple matches found, defaulting to single expansion");
    }

    DEBUG_LOG("  Generating " << max_match_count << " expansions for variadic pattern group");

    // Generate expansions
    for (size_t k = 0; k < max_match_count; k++) {
        DEBUG_LOG("    Expansion iteration k = " << k);

        // Create iteration environment
        MatchEnv iter_env;
        setupIterationEnvironment(env, iter_env, k);

        // Expand each element in the pattern group
        for (const auto& elem : pattern_group) {
            auto non_variadic = createNonVariadicCopy(elem);

            DEBUG_LOG("      Expanding: " << non_variadic->toString());
            auto expanded = transformTemplate(non_variadic, iter_env, macroContext, pattern_info);

            if (expanded) {
                DEBUG_LOG("      Result: " << expanded->toString());
                result.push_back(expanded);
            } else {
                DEBUG_LOG("      Expansion returned null, skipping");
            }
        }
    }
}

std::shared_ptr<MacroExpression> transformTemplate(
    const std::shared_ptr<MacroExpression>& template_expr,
    MatchEnv& env,
    SyntaxContext macroContext,
    const PatternVariableInfo& pattern_info)
{
    if (!template_expr) {
        DEBUG_LOG("transformTemplate: Input template is null, returning null.");
        return nullptr;
    }

    std::string node_str = template_expr->toString();
    if (node_str.length() > 50)
        node_str = node_str.substr(0, 47) + "...";
    TRACE_TRANSFORM("transformTemplate on: " + node_str);

    return std::visit(overloaded {
                          // Atom transformation remains unchanged
                          [&](const MacroAtom& atom) -> std::shared_ptr<MacroExpression> {
                              DEBUG_LOG("  Atom Template: " << atom.syntax.token.lexeme << (template_expr->isVariadic ? "..." : ""));
                              auto matches_opt = env.lookup(atom.syntax.token.lexeme);
                              if (matches_opt) {
                                  DEBUG_LOG("  Found match for '" << atom.syntax.token.lexeme << "' in env. Matches: " << matches_opt->matches.size());

                                  for (size_t i = 0; i < matches_opt->matches.size(); i++) {
                                      DEBUG_LOG("    Match[" << i << "]: " << matches_opt->matches[i]->toString());
                                  }

                                  if (template_expr->isVariadic) {
                                      DEBUG_LOG("  Atom is variadic (...). Expanding all matches into a List.");
                                      if (matches_opt->matches.empty()) {
                                          DEBUG_LOG("  Variadic atom expansion: No matches found, returning empty list.");
                                          return std::make_shared<MacroExpression>(MacroList {}, false, template_expr->line);
                                      }

                                      std::vector<std::shared_ptr<MacroExpression>> expanded_atoms;
                                      expanded_atoms.reserve(matches_opt->matches.size());
                                      for (const auto& match : matches_opt->matches) {
                                          expanded_atoms.push_back(addUsageContextRecursive(match, macroContext));
                                          DEBUG_LOG("    Added expanded atom: " << expanded_atoms.back()->toString());
                                      }

                                      return std::make_shared<MacroExpression>(
                                          MacroList { std::move(expanded_atoms) }, false, template_expr->line);
                                  } else {
                                      DEBUG_LOG("  Atom is not variadic. Expanding with first match (if any).");
                                      if (!matches_opt->matches.empty()) {
                                          auto result = addUsageContextRecursive(matches_opt->matches[0], macroContext);
                                          DEBUG_LOG("  Substituted '" << atom.syntax.token.lexeme << "' with: " << result->toString());
                                          return result;
                                      } else {
                                          DEBUG_LOG("  Warning: Variable '" << atom.syntax.token.lexeme << "' bound but has no matches in current env. Treating as literal?");
                                      }
                                  }
                              }

                              if (atom.syntax.token.type == Tokentype::IDENTIFIER) {
                                  DEBUG_LOG("  Identifier '" << atom.syntax.token.lexeme << "' treated as literal. Adding hygiene context.");
                                  HygienicSyntax freshSyntax { atom.syntax.token, atom.syntax.context.addMarks(macroContext.marks) };
                                  return std::make_shared<MacroExpression>(
                                      MacroAtom { freshSyntax }, template_expr->isVariadic, template_expr->line);
                              } else {
                                  DEBUG_LOG("  Atom '" << atom.syntax.token.lexeme << "' treated as non-identifier literal.");
                                  return std::make_shared<MacroExpression>(atom, template_expr->isVariadic, template_expr->line);
                              }
                          },

                          // New list transformation with improved variadic handling
                          [&](const MacroList& list) -> std::shared_ptr<MacroExpression> {
                              DEBUG_LOG("  List Template: " << template_expr->toString());

                              std::vector<std::shared_ptr<MacroExpression>> transformed_elements;
                              size_t i = 0;

                              while (i < list.elements.size()) {
                                  const auto& current_element = list.elements[i];

                                  if (!current_element) {
                                      DEBUG_LOG("  Skipping null element at index " << i);
                                      i++;
                                      continue;
                                  }

                                  if (current_element->isVariadic) {
                                      DEBUG_LOG("  Found start of potential variadic sequence at index " << i);

                                      // Identify separate variadic pattern groups
                                      size_t group_start = i;
                                      std::vector<std::shared_ptr<MacroExpression>> current_group;
                                      std::set<std::string> current_controlling_vars;

                                      // Find controlling variables for the first variadic element
                                      auto non_variadic_elem = createNonVariadicCopy(current_element);
                                      findControllingVariables(non_variadic_elem, env, current_controlling_vars);
                                      current_group.push_back(current_element);
                                      i++;

                                      DEBUG_LOG("  Starting variadic group with controlling vars: "
                                          << [&]() {
                                                 std::stringstream ss;
                                                 for (const auto& v : current_controlling_vars)
                                                     ss << v << " ";
                                                 return ss.str();
                                             }());

                                      // Group consecutive variadic elements by controlling variables
                                      while (i < list.elements.size() && list.elements[i] && list.elements[i]->isVariadic) {
                                          std::set<std::string> next_controlling_vars;
                                          auto next_non_variadic = createNonVariadicCopy(list.elements[i]);
                                          findControllingVariables(next_non_variadic, env, next_controlling_vars);

                                          DEBUG_LOG("  Next variadic element has controlling vars: "
                                              << [&]() {
                                                     std::stringstream ss;
                                                     for (const auto& v : next_controlling_vars)
                                                         ss << v << " ";
                                                     return ss.str();
                                                 }());

                                          // Check if this element belongs to the same pattern group
                                          if (!sameControllingPattern(current_controlling_vars, next_controlling_vars, env)) {
                                              DEBUG_LOG("  Detected new pattern group, breaking");
                                              break;
                                          }

                                          // Add to current group
                                          current_group.push_back(list.elements[i]);
                                          i++;
                                      }

                                      DEBUG_LOG("  Variadic group contains " << current_group.size()
                                                                             << " elements from indices [" << group_start << ", " << i << ")");

                                      // Process this variadic pattern group
                                      std::vector<std::shared_ptr<MacroExpression>> group_expansion;
                                      expandVariadicPatternGroup(current_group, env, macroContext, pattern_info, group_expansion);

                                      // Add expanded elements to result
                                      transformed_elements.insert(
                                          transformed_elements.end(),
                                          group_expansion.begin(),
                                          group_expansion.end());

                                  } else {
                                      // Non-variadic element
                                      DEBUG_LOG("  Transforming non-variadic element at index " << i << ": " << current_element->toString());
                                      transformed_elements.push_back(transformTemplate(current_element, env, macroContext, pattern_info));
                                      i++;
                                  }
                              }

                              DEBUG_LOG("  Finished list transform. Result elements count: " << transformed_elements.size());
                              return std::make_shared<MacroExpression>(
                                  MacroList { std::move(transformed_elements) },
                                  false, // Result list is never variadic
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
            if (env->isMacro(atom->syntax.token.lexeme)) {
                std::string macro_name_str = atom->syntax.token.lexeme;
                auto synt = env->getMacroDefinition(macro_name_str);
                if (!synt || !*synt || !std::holds_alternative<SyntaxRulesExpression>((*synt)->as)) {
                    break;
                }
                auto rules = std::get<SyntaxRulesExpression>((*synt)->as).rules;
                auto literals = std::get<SyntaxRulesExpression>((*synt)->as).literals;
                SyntaxContext macroContext = SyntaxContext::createFresh();

                for (const auto& rule : rules) {
                    auto patternExpr = fromExpr(rule.pattern);
                    auto [matchEnv, success] = tryMatch(patternExpr, current, literals, macro_name_str);
                    if (success) {
                        auto templateMacroExpr = fromExpr(rule.template_expr);
                        // Use rule.pattern_info - add this to handle pattern variables correctly
                        current = transformTemplate(templateMacroExpr, matchEnv, macroContext, rule.pattern_info);
                        expanded = true;
                        break;
                    }
                }
            }
        } else if (auto* list = std::get_if<MacroList>(&current->value)) {
            if (!list->elements.empty()) {
                bool list_was_macro_call = false;
                if (auto* firstAtom = std::get_if<MacroAtom>(&list->elements[0]->value)) {
                    if (env->isMacro(firstAtom->syntax.token.lexeme)) {
                        std::string macro_name_str = firstAtom->syntax.token.lexeme;
                        auto synt = env->getMacroDefinition(macro_name_str);
                        if (synt && *synt && std::holds_alternative<SyntaxRulesExpression>((*synt)->as)) {
                            auto rules = std::get<SyntaxRulesExpression>((*synt)->as).rules;
                            auto literals = std::get<SyntaxRulesExpression>((*synt)->as).literals;
                            SyntaxContext macroContext = SyntaxContext::createFresh();

                            for (const auto& rule : rules) {
                                auto patternExpr = fromExpr(rule.pattern);
                                auto [matchEnv, success] = tryMatch(patternExpr, current, literals, macro_name_str);
                                if (success) {
                                    auto templateMacroExpr = fromExpr(rule.template_expr);
                                    current = transformTemplate(templateMacroExpr, matchEnv, macroContext, rule.pattern_info);
                                    expanded = true;
                                    list_was_macro_call = true;
                                    break;
                                }
                            }
                        }
                    }
                }

                if (!list_was_macro_call) {
                    std::vector<std::shared_ptr<MacroExpression>> newElements;
                    bool anySubExpanded = false;
                    for (const auto& elem : list->elements) {
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
    MatchEnv& env,
    const std::string& macroName,
    std::shared_ptr<pattern::MacroEnvironment> macroEnv,
    const PatternVariableInfo& pattern_info)
{
    auto templateMacroExpr = fromExpr(template_expr);
    SyntaxContext macroContext = SyntaxContext::createFresh();
    auto transformed = transformTemplate(templateMacroExpr, env, macroContext, pattern_info);
    auto fullyExpanded = transformMacroRecursive(transformed, macroEnv);
    return fullyExpanded;
}
std::shared_ptr<Expression> convertBegin(const macroexp::MacroList& ml, int line)
{
    std::vector<std::shared_ptr<Expression>> values = {};
    values.reserve(ml.elements.size() > 0 ? ml.elements.size() - 1 : 0);
    for (size_t i = 1; i < ml.elements.size(); i++) { // Use size_t
        auto datum = convertMacroResultToExpressionInternal(ml.elements[i]);
        if (!datum) {
            throw std::runtime_error("Invalid expression in 'begin' body during conversion");
        }
        values.push_back(datum);
    }
    return std::make_shared<Expression>(BeginExpression { std::move(values) }, line);
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
                              } else if (keyword == "quasiquote") {
                                  return convertQuasiQuote(ml, line);
                              } else if (keyword == "begin") {
                                  return convertBegin(ml, line);
                              } else if (keyword == "unquote") {
                                  return convertUnquote(ml, line);
                              } else if (keyword == "unquote-splice") {
                                  return convertSplice(ml, line);
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

std::shared_ptr<Expression> convertQuasiQuote(const MacroList& ml, int line)
{
    if (ml.elements.size() != 2)
        throw std::runtime_error("Invalid quote structure during conversion");
    auto datum = convertMacroResultToExpressionInternal(ml.elements[1]);
    if (!datum)
        throw std::runtime_error("Invalid quote datum during conversion");
    return std::make_shared<Expression>(QuasiQuoteExpression { datum }, line);
}

std::shared_ptr<Expression> convertUnquote(const MacroList& ml, int line)
{
    if (ml.elements.size() != 2)
        throw std::runtime_error("Invalid quote structure during conversion");
    auto datum = convertMacroResultToExpressionInternal(ml.elements[1]);
    if (!datum)
        throw std::runtime_error("Invalid quote datum during conversion");
    return std::make_shared<Expression>(UnquoteExpression { datum }, line);
}

std::shared_ptr<Expression> convertSplice(const MacroList& ml, int line)
{
    if (ml.elements.size() != 2)
        throw std::runtime_error("Invalid quote structure during conversion");
    auto datum = convertMacroResultToExpressionInternal(ml.elements[1]);
    if (!datum)
        throw std::runtime_error("Invalid quote datum during conversion");
    return std::make_shared<Expression>(SpliceExpression { datum }, line);
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
    thenBranch = std::make_shared<Expression>(TailExpression { thenBranch }, thenBranch->line);

    if (ml.elements.size() == 4) {
        auto elseConv = convertMacroResultToExpressionInternal(ml.elements[3]);
        if (!elseConv)
            throw std::runtime_error("Invalid if else part during conversion");
        elseBranchOpt = std::make_shared<Expression>(TailExpression { elseConv }, elseConv->line);
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
