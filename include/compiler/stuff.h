#pragma once

#include "Expression.h"
#include "Interpreter.h"
#include "Parser.h"
#include "Scanner.h"
#include <map>
#include <memory>
#include <string>
#include <variant>
#include <vector>

void debugPrintExpression(std::shared_ptr<Expression> expr, int indent = 0)
{
    std::string spaces(indent * 2, ' ');

    if (!expr) {
        std::cout << spaces << "null\n";
        return;
    }

    std::visit(overloaded {
                   [&](const AtomExpression& e) {
                       std::cout << spaces << "Atom: " << e.value.lexeme << "\n";
                   },
                   [&](const ListExpression& l) {
                       std::cout << spaces << "List (variadic=" << l.isVariadic << ") with "
                                 << l.elements.size() << " elements:\n";
                       for (const auto& elem : l.elements) {
                           debugPrintExpression(elem, indent + 1);
                       }
                   },
                   [&](const auto&) {
                       std::cout << spaces << "Other expression type\n";
                   } },
        expr->as);
}

// Environment to store pattern matches
class PatternEnvironment {

public:
    std::map<std::string, std::vector<std::shared_ptr<Expression>>> bindings;
    void bind(const std::string& identifier, std::shared_ptr<Expression> syntax)
    {
        bindings[identifier] = { syntax };
    }

    void bindList(const std::string& identifier, const std::vector<std::shared_ptr<Expression>>& syntaxList)
    {
        bindings[identifier] = syntaxList;
    }

    bool merge(const PatternEnvironment& other)
    {
        for (const auto& [key, value] : other.bindings) {
            if (bindings.contains(key)) {
                // For variadic patterns, we want to accumulate values
                bindings[key].insert(
                    bindings[key].end(),
                    value.begin(),
                    value.end());
            } else {
                bindings[key] = value;
            }
        }
        return true;
    }
};

void debugPrintPatternStructure(std::shared_ptr<Expression> expr, int indent = 0)
{
    std::string spaces(indent * 2, ' ');

    std::visit(overloaded {
                   [&](const AtomExpression& atom) {
                       std::cout << spaces << "Atom: " << atom.value.lexeme << "\n";
                   },
                   [&](const ListExpression& list) {
                       std::cout << spaces << "List (variadic=" << list.isVariadic << ") with "
                                 << list.elements.size() << " elements:\n";
                       for (const auto& elem : list.elements) {
                           debugPrintPatternStructure(elem, indent + 1);
                       }
                   },
                   [&](const auto&) {
                       std::cout << spaces << "Other type\n";
                   } },
        expr->as);
}

class PatternMatcher {
public:
    std::optional<PatternEnvironment> match(std::shared_ptr<Expression> pattern,
        std::shared_ptr<Expression> syntax)
    {
        std::cout << "\n=== Starting New Pattern Match ===\n";
        debugPrintPatternStructure(pattern, 0);
        std::cout << "\nAgainst Syntax:\n";
        debugPrintPatternStructure(syntax, 0);

        if (!pattern || !syntax) {
            std::cout << "Null pattern or syntax\n";
            return std::nullopt;
        }

        return std::visit(overloaded {
                              [&](const AtomExpression& p) -> std::optional<PatternEnvironment> {
                                  std::cout << "\nAtom pattern match: " << p.value.lexeme
                                            << " (type: " << static_cast<int>(p.value.type) << ")\n";

                                  if (p.value.type != Tokentype::IDENTIFIER) {
                                      return matchLiteral(p, syntax);
                                  }
                                  PatternEnvironment env;
                                  env.bind(p.value.lexeme, syntax);
                                  std::cout << "Bound " << p.value.lexeme << " to syntax\n";
                                  return env;
                              },

                              [&](const ListExpression& p) -> std::optional<PatternEnvironment> {
                                  std::cout << "\nList pattern match (variadic=" << p.isVariadic << ")\n";
                                  if (auto syntaxList = std::get_if<ListExpression>(&syntax->as)) {
                                      return p.isVariadic ? matchVariadicList(p, *syntaxList) : matchList(p, *syntaxList);
                                  }
                                  std::cout << "Syntax is not a list\n";
                                  return std::nullopt;
                              },

                              [](const auto&) -> std::optional<PatternEnvironment> {
                                  std::cout << "Unknown pattern type\n";
                                  return std::nullopt;
                              } },
            pattern->as);
    }

private:
    std::optional<PatternEnvironment> matchLiteral(const AtomExpression& pattern,
        std::shared_ptr<Expression> syntax)
    {
        std::cout << "\n=== Matching Literal ===\n";
        std::cout << "Pattern: " << pattern.value.lexeme << "\n";

        if (auto syntaxAtom = std::get_if<AtomExpression>(&syntax->as)) {
            std::cout << "Syntax: " << syntaxAtom->value.lexeme << "\n";
            if (pattern.value.lexeme == syntaxAtom->value.lexeme) {
                std::cout << "Literal match successful\n";
                return PatternEnvironment();
            }
        }
        std::cout << "Literal match failed\n";
        return std::nullopt;
    }

    std::optional<PatternEnvironment> matchVariadicList(const ListExpression& pattern, const ListExpression& syntax)
    {
        std::cout << "\n=== Matching Variadic List ===\n";
        if (pattern.elements.empty()) {
            return std::nullopt;
        }

        PatternEnvironment env;

        for (size_t i = 0; i < pattern.elements.size(); i++) {
            auto patternElem = pattern.elements[i];

            if (i < syntax.elements.size()) {
                auto subMatch = match(patternElem, syntax.elements[i]);
                if (!subMatch) {
                    std::cout << "Failed to match pattern element " << i << "\n";
                    return std::nullopt;
                }
                if (!env.merge(*subMatch)) {
                    return std::nullopt;
                }
            }
        }

        return env;
    }

    std::optional<PatternEnvironment> matchList(const ListExpression& pattern,
        const ListExpression& syntax)
    {
        std::cout << "\n=== Matching List ===\n";
        std::cout << "Pattern elements: " << pattern.elements.size()
                  << " (variadic=" << pattern.isVariadic << ")\n";
        std::cout << "Syntax elements: " << syntax.elements.size() << "\n";

        if (pattern.isVariadic) {
            return matchVariadicList(pattern, syntax);
        }

        if (pattern.elements.size() != syntax.elements.size()) {
            std::cout << "Size mismatch in regular list match\n";
            return std::nullopt;
        }

        PatternEnvironment env;
        for (size_t i = 0; i < pattern.elements.size(); i++) {
            std::cout << "\nMatching element " << i << ":\n";
            debugPrintPatternStructure(pattern.elements[i], 1);
            std::cout << "Against:\n";
            debugPrintPatternStructure(syntax.elements[i], 1);

            auto subMatch = match(pattern.elements[i], syntax.elements[i]);
            if (!subMatch) {
                std::cout << "Failed to match element " << i << "\n";
                return std::nullopt;
            }
            if (!env.merge(*subMatch)) {
                std::cout << "Failed to merge environment for element " << i << "\n";
                return std::nullopt;
            }
            std::cout << "Successfully matched element " << i << "\n";
        }

        std::cout << "List match successful\n";
        return env;
    }
};

#include <cassert>
#include <iostream>
class PatternSubstitutor {
public:
    std::shared_ptr<Expression> substitute(std::shared_ptr<Expression> templateExpr,
        const PatternEnvironment& env)
    {
        if (!templateExpr)
            return nullptr;

        return std::visit(overloaded {
                              [&](const AtomExpression& atom) -> std::shared_ptr<Expression> {
                                  // If atom is an identifier and exists in bindings, substitute it
                                  if (atom.value.type == Tokentype::IDENTIFIER) {
                                      auto it = env.bindings.find(atom.value.lexeme);
                                      if (it != env.bindings.end() && !it->second.empty()) {
                                          return it->second[0]; // Return first binding
                                      }
                                  }
                                  // Otherwise return copy of original atom
                                  return std::make_shared<Expression>(Expression { atom, templateExpr->line });
                              },

                              [&](const ListExpression& list) -> std::shared_ptr<Expression> {
                                  std::vector<std::shared_ptr<Expression>> newElements;

                                  // For each element in the template list
                                  for (const auto& elem : list.elements) {
                                      if (auto atomExpr = std::get_if<AtomExpression>(&elem->as)) {
                                          // Check if it's a pattern variable
                                          auto it = env.bindings.find(atomExpr->value.lexeme);
                                          if (it != env.bindings.end()) {
                                              // If variadic, expand all bindings
                                              if (list.isVariadic) {
                                                  newElements.insert(newElements.end(),
                                                      it->second.begin(),
                                                      it->second.end());
                                                  continue;
                                              }
                                          }
                                      }
                                      // Regular substitution for non-variadic or non-pattern variables
                                      auto substituted = substitute(elem, env);
                                      if (substituted) {
                                          newElements.push_back(substituted);
                                      }
                                  }

                                  return std::make_shared<Expression>(
                                      Expression { ListExpression { newElements, list.isVariadic }, templateExpr->line });
                              },

                              [&](const auto&) -> std::shared_ptr<Expression> {
                                  return templateExpr;
                              } },
            templateExpr->as);
    }
};
// Helper to create atomic expressions
std::shared_ptr<Expression> makeAtom(const std::string& lexeme, Tokentype type = Tokentype::IDENTIFIER)
{
    Token token { type, lexeme, 1, 1 };
    return std::make_shared<Expression>(AtomExpression { token }, 1);
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
void printTestResult(const std::string& testName, bool success)
{
    std::cout << (success ? "[PASS] " : "[FAIL] ") << testName << "\n";
}

void testPatternMatcher()
{
    PatternMatcher matcher;

    // Test 1: Simple identifier match
    std::cout << "\nTest 1: Simple identifier match\n";
    {
        auto pattern = makeAtom("x");
        auto syntax = makeAtom("42", Tokentype::INTEGER);

        auto result = matcher.match(pattern, syntax);
        bool success = result.has_value() && result->bindings["x"].size() == 1 && std::holds_alternative<AtomExpression>(result->bindings["x"][0]->as);
        printTestResult("Simple identifier match", success);
    }

    // Test 2: List pattern without variadic
    std::cout << "\nTest 2: List pattern without variadic\n";
    {
        // Create pattern (a b)
        std::vector<std::shared_ptr<Expression>> patternElements;
        patternElements.push_back(makeAtom("a"));
        patternElements.push_back(makeAtom("b"));
        auto pattern = std::make_shared<Expression>(ListExpression { patternElements, false }, 1);

        // Create syntax (1 2)
        std::vector<std::shared_ptr<Expression>> syntaxElements;
        syntaxElements.push_back(makeAtom("1", Tokentype::INTEGER));
        syntaxElements.push_back(makeAtom("2", Tokentype::INTEGER));
        auto syntax = std::make_shared<Expression>(ListExpression { syntaxElements, false }, 1);

        auto result = matcher.match(pattern, syntax);
        bool success = result.has_value() && result->bindings["a"].size() == 1 && result->bindings["b"].size() == 1;
        printTestResult("List pattern without variadic", success);
    }

    // Test 3: Variadic pattern
    std::cout << "\nTest 3: Variadic pattern\n";
    {
        // Create pattern (x ...)
        std::vector<std::shared_ptr<Expression>> patternElements;
        patternElements.push_back(makeAtom("x"));
        auto pattern = std::make_shared<Expression>(ListExpression { patternElements, true }, 1);

        // Create syntax (1 2 3)
        std::vector<std::shared_ptr<Expression>> syntaxElements;
        syntaxElements.push_back(makeAtom("1", Tokentype::INTEGER));
        syntaxElements.push_back(makeAtom("2", Tokentype::INTEGER));
        syntaxElements.push_back(makeAtom("3", Tokentype::INTEGER));
        auto syntax = std::make_shared<Expression>(ListExpression { syntaxElements, false }, 1);

        auto result = matcher.match(pattern, syntax);
        bool success = result.has_value() && result->bindings["x"].size() == 3; // Should bind all three elements
        printTestResult("Variadic pattern", success);
    }

    // Test 4: Literal match
    std::cout << "\nTest 4: Literal match\n";
    {
        auto pattern = makeAtom("42", Tokentype::INTEGER);
        auto syntax = makeAtom("42", Tokentype::INTEGER);

        auto result = matcher.match(pattern, syntax);
        bool success = result.has_value(); // Should match but have no bindings
        printTestResult("Literal match", success);
    }

    // Test 5: Nested list pattern
    std::cout << "\nTest 5: Nested list pattern\n";
    {
        // Create pattern ((a b) c)
        std::vector<std::shared_ptr<Expression>> innerPatternElements;
        innerPatternElements.push_back(makeAtom("a"));
        innerPatternElements.push_back(makeAtom("b"));
        auto innerPattern = std::make_shared<Expression>(ListExpression { innerPatternElements, false }, 1);

        std::vector<std::shared_ptr<Expression>> outerPatternElements;
        outerPatternElements.push_back(innerPattern);
        outerPatternElements.push_back(makeAtom("c"));
        auto pattern = std::make_shared<Expression>(ListExpression { outerPatternElements, false }, 1);

        // Create syntax ((1 2) 3)
        std::vector<std::shared_ptr<Expression>> innerSyntaxElements;
        innerSyntaxElements.push_back(makeAtom("1", Tokentype::INTEGER));
        innerSyntaxElements.push_back(makeAtom("2", Tokentype::INTEGER));
        auto innerSyntax = std::make_shared<Expression>(ListExpression { innerSyntaxElements, false }, 1);

        std::vector<std::shared_ptr<Expression>> outerSyntaxElements;
        outerSyntaxElements.push_back(innerSyntax);
        outerSyntaxElements.push_back(makeAtom("3", Tokentype::INTEGER));
        auto syntax = std::make_shared<Expression>(ListExpression { outerSyntaxElements, false }, 1);

        auto result = matcher.match(pattern, syntax);
        bool success = result.has_value() && result->bindings["a"].size() == 1 && result->bindings["b"].size() == 1 && result->bindings["c"].size() == 1;
        printTestResult("Nested list pattern", success);
    }
}
void test_syntax_rules()
{
    auto scanner = std::make_shared<Scanner>();
    Parser parser;
    parser.initialize(scanner);

    // Test macro definition with pattern
    std::string testMacro = R"(
        (define-syntax mylet
          (syntax-rules ()
            ((myLet ((name value) ...) body ...)
             ((lambda (name ...) body ...) value ...))))
    )";

    auto tokens = scanner->tokenize(testMacro);
    parser.load(tokens);

    // Create test input syntax: (myLet ((x 10)) (display x))

    // Create binding pair ((x 10))
    std::vector<std::shared_ptr<Expression>> bindingPair;
    bindingPair.push_back(makeAtom("x"));
    bindingPair.push_back(makeAtom("10"));
    auto binding = std::make_shared<Expression>(ListExpression { bindingPair, false }, 1);

    std::vector<std::shared_ptr<Expression>> bindingsList;
    bindingsList.push_back(binding);
    auto bindings = std::make_shared<Expression>(ListExpression { bindingsList, false }, 1);

    // Create (display x) expression
    std::vector<std::shared_ptr<Expression>> display;
    display.push_back(makeAtom("display"));
    display.push_back(makeAtom("x"));
    auto displayexpr = std::make_shared<Expression>(ListExpression { display, false }, 1);

    // Create complete test syntax: (myLet ((x 10)) (display x))
    std::vector<std::shared_ptr<Expression>> testSyntax;
    testSyntax.push_back(makeAtom("myLet"));
    testSyntax.push_back(bindings);
    testSyntax.push_back(displayexpr);
    auto syn = std::make_shared<Expression>(Expression { ListExpression { testSyntax }, 1 });

    try {
        PatternMatcher matcher;
        PatternSubstitutor substitutor;
        auto expr = parser.expression();

        auto defineSyntax = std::get<DefineSyntaxExpression>(expr->as);
        auto syntaxRules = std::get<SyntaxRulesExpression>(defineSyntax.rule->as);

        std::cout << "\nAttempting pattern match:\n";
        for (const auto& pattern : syntaxRules.pattern) {
            if (auto list = std::get_if<ListExpression>(&pattern->as)) {
                if (!list->elements.empty()) {
                    auto result = matcher.match(list->elements[0], syn);

                    if (result) {
                        std::cout << "\nMatch successful! Bindings:\n";
                        for (const auto& [k, v] : result->bindings) {
                            std::cout << "Variable '" << k << "' bound to " << v.size()
                                      << " expression(s):\n";
                            for (const auto& expr : v) {
                                std::cout << "\t" << expr->toString() << "\n";
                            }
                        }

                        // Get the template (second element of pattern)
                        if (list->elements.size() > 1) {
                            auto templateExpr = list->elements[1];
                            std::cout << "\nTemplate before substitution:\n";
                            debugPrintExpression(templateExpr);

                            auto substituted = substitutor.substitute(templateExpr, *result);
                            std::cout << "\nAfter substitution:\n";
                            debugPrintExpression(substituted);
                            substituted->print();
                        }
                    }
                }
            }
        }
    } catch (const std::exception& e) {
        std::cout << "Error: " << e.what() << "\n";
    }
}

class MacroProcessor {
private:
    struct MacroDefinition {
        std::vector<Token> literals;
        std::vector<std::shared_ptr<Expression>> patterns;
        std::vector<std::shared_ptr<Expression>> templates;
    };
    std::unordered_map<std::string, MacroDefinition> macros;
    PatternMatcher matcher;
    PatternSubstitutor substitutor;

public:
    void collectMacros(std::shared_ptr<Expression> expr)
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

    std::shared_ptr<Expression> expandMacros(std::shared_ptr<Expression> expr)
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
                                      } else {
                                          std::cout << "Not a macro call\n";
                                      }
                                  }

                                  // Process elements recursively
                                  std::cout << "\nProcessing list elements recursively\n";
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

    void printMacroTable() const
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
};
