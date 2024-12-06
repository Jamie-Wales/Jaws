#include "MacroProcessor.h"
#include "DebugUtils.h"
#include "ExpressionUtils.h"
#include "Visit.h"
#include <iostream>

void MacroProcessor::collectMacros(std::shared_ptr<Expression> expr)
{
    if (!expr)
        return;

    std::visit(overloaded {
                   [&](const DefineSyntaxExpression& d) {
                       std::cout << "\n=== Found Macro Definition ===\n";
                       std::cout << "Macro name: " << d.name.lexeme << "\n";

                       if (auto syntaxRules = std::get_if<SyntaxRulesExpression>(&d.rule->as)) {
                           MacroDefinition macro;
                           macro.literals = syntaxRules->literals;
                           macro.patterns = syntaxRules->pattern;
                           macro.templates = syntaxRules->template_expr;
                           macros[d.name.lexeme] = macro;

                           std::cout << "Literals: ";
                           for (const auto& lit : syntaxRules->literals) {
                               std::cout << lit.lexeme << " ";
                           }
                           std::cout << "\nPatterns:\n";
                           for (const auto& pat : syntaxRules->pattern) {
                               debugPrintExpression(pat);
                           }
                           std::cout << "Templates:\n";
                           for (const auto& templ : syntaxRules->template_expr) {
                               debugPrintExpression(templ);
                           }
                       }
                   },
                   [&](const ListExpression& l) {
                       for (const auto& elem : l.elements) {
                           collectMacros(elem);
                       }
                   },
                   [](const auto&) {} },
        expr->as);
}

std::shared_ptr<Expression> MacroProcessor::expandMacros(std::shared_ptr<Expression> expr)
{
    if (!expr)
        return nullptr;

    return std::visit(overloaded {
                          [&](const ListExpression& l) -> std::shared_ptr<Expression> {
                              if (l.elements.empty())
                                  return expr;

                              std::cout << "\n=== Checking Expression for Macro Expansion ===\n";
                              std::cout << "Expression: ";
                              debugPrintExpression(expr);

                              if (auto firstAtom = std::get_if<AtomExpression>(&l.elements[0]->as)) {
                                  std::cout << "First atom: " << firstAtom->value.lexeme << "\n";
                                  auto it = macros.find(firstAtom->value.lexeme);
                                  if (it != macros.end()) {
                                      std::cout << "Found macro definition for: " << firstAtom->value.lexeme << "\n";

                                      auto exprAsList = exprToList(expr);
                                      std::cout << "Expression as list:\n";
                                      debugPrintExpression(exprAsList);

                                      for (size_t i = 0; i < it->second.patterns.size(); i++) {
                                          std::cout << "\nTrying pattern " << i << ":\n";
                                          debugPrintExpression(it->second.patterns[i]);

                                          // Match against the full pattern
                                          if (auto result = matcher.match(it->second.patterns[i], exprAsList)) {
                                              std::cout << "\nPattern matched! Bindings:\n";
                                              for (const auto& [k, v] : result->bindings) {
                                                  std::cout << "'" << k << "' bound to " << v.size() << " expressions:\n";
                                                  for (const auto& e : v) {
                                                      debugPrintExpression(e);
                                                  }
                                              }

                                              std::cout << "\nExpanding with template:\n";
                                              debugPrintExpression(it->second.templates[i]);

                                              auto expanded = substitutor.substitute(it->second.templates[i], *result);
                                              std::cout << "\nExpanded result:\n";
                                              debugPrintExpression(expanded);

                                              std::cout << "\nRecursively expanding result...\n";
                                              return expandMacros(expanded);
                                          }
                                          std::cout << "Pattern did not match\n";
                                      }
                                  }
                              }

                              // Process elements recursively
                              std::vector<std::shared_ptr<Expression>> newElements;
                              for (const auto& elem : l.elements) {
                                  newElements.push_back(expandMacros(elem));
                              }
                              return std::make_shared<Expression>(
                                  Expression { ListExpression { newElements, l.isVariadic }, expr->line });
                          },
                          [&](const auto&) -> std::shared_ptr<Expression> {
                              return expr;
                          } },
        expr->as);
}
void MacroProcessor::printMacroTable() const
{
    std::cout << "\n=== Macro Table ===\n";
    for (const auto& [name, def] : macros) {
        std::cout << "\nMacro: " << name << "\n";
        std::cout << "Literals: ";
        for (const auto& lit : def.literals) {
            std::cout << lit.lexeme << " ";
        }
        std::cout << "\nPatterns:\n";
        for (const auto& pat : def.patterns) {
            debugPrintExpression(pat);
        }
        std::cout << "Templates:\n";
        for (const auto& templ : def.templates) {
            debugPrintExpression(templ);
        }
    }
}
