#include "PatternMatching.h"
#include "DebugUtils.h"
#include <iostream>

void PatternEnvironment::bind(const std::string& identifier, std::shared_ptr<Expression> syntax)
{
    bindings[identifier] = { syntax };
}

void PatternEnvironment::bindList(const std::string& identifier,
    const std::vector<std::shared_ptr<Expression>>& syntaxList)
{
    bindings[identifier] = syntaxList;
}

bool PatternEnvironment::merge(const PatternEnvironment& other)
{
    for (const auto& [key, value] : other.bindings) {
        if (bindings.contains(key)) {
            bindings[key].insert(bindings[key].end(), value.begin(), value.end());
        } else {
            bindings[key] = value;
        }
    }
    return true;
}

std::optional<PatternEnvironment> PatternMatcher::match(
    std::shared_ptr<Expression> pattern,
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

std::optional<PatternEnvironment> PatternMatcher::matchLiteral(
    const AtomExpression& pattern,
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

std::optional<PatternEnvironment> PatternMatcher::matchVariadicList(
    const ListExpression& pattern,
    const ListExpression& syntax)
{
    std::cout << "\n=== Matching Variadic List ===\n";
    if (pattern.elements.empty()) {
        return std::nullopt;
    }

    PatternEnvironment env;

    // Handle the non-variadic prefix elements first
    size_t fixedPrefixLength = pattern.elements.size() - 1; // Last element is variadic
    for (size_t i = 0; i < fixedPrefixLength && i < syntax.elements.size(); i++) {
        std::cout << "Matching fixed prefix element " << i << "\n";
        auto patternElem = pattern.elements[i];
        auto subMatch = match(patternElem, syntax.elements[i]);
        if (!subMatch) {
            std::cout << "Failed to match prefix element " << i << "\n";
            return std::nullopt;
        }
        if (!env.merge(*subMatch)) {
            return std::nullopt;
        }
    }

    // Handle the variadic part (last pattern element)
    if (fixedPrefixLength < pattern.elements.size()) {
        auto variadicPattern = pattern.elements[fixedPrefixLength];
        std::vector<std::shared_ptr<Expression>> variadicMatches;

        // Collect all remaining syntax elements for variadic matching
        for (size_t i = fixedPrefixLength; i < syntax.elements.size(); i++) {
            auto subMatch = match(variadicPattern, syntax.elements[i]);
            if (!subMatch) {
                std::cout << "Failed to match variadic element " << i << "\n";
                return std::nullopt;
            }

            // For each variadic binding in the submatch, extend the list
            for (const auto& [key, values] : subMatch->bindings) {
                auto it = env.bindings.find(key);
                if (it != env.bindings.end()) {
                    it->second.insert(it->second.end(), values.begin(), values.end());
                } else {
                    env.bindings[key] = values;
                }
            }
        }
    }

    return env;
}

std::optional<PatternEnvironment> PatternMatcher::matchList(
    const ListExpression& pattern,
    const ListExpression& syntax)
{
    std::cout << "\n=== Matching List ===\n";
    std::cout << "Pattern elements: " << pattern.elements.size()
              << " (variadic=" << pattern.isVariadic << ")\n";
    std::cout << "Syntax elements: " << syntax.elements.size() << "\n";

    // For variadic patterns, size comparison is different
    if (pattern.isVariadic) {
        // For variadic patterns, syntax size must be >= pattern size - 1
        // (the last element can match zero or more elements)
        if (syntax.elements.size() < pattern.elements.size() - 1) {
            std::cout << "Size too small for variadic pattern\n";
            return std::nullopt;
        }
    } else if (pattern.elements.size() != syntax.elements.size()) {
        std::cout << "Size mismatch in regular list match\n";
        return std::nullopt;
    }

    PatternEnvironment env;
    // Match fixed prefix elements
    size_t i;
    for (i = 0; i < pattern.elements.size() - (pattern.isVariadic ? 1 : 0); i++) {
        std::cout << "\nMatching fixed element " << i << ":\n";
        debugPrintExpression(pattern.elements[i]);
        std::cout << "Against:\n";
        debugPrintExpression(syntax.elements[i]);

        auto subMatch = match(pattern.elements[i], syntax.elements[i]);
        if (!subMatch) {
            std::cout << "Failed to match element " << i << "\n";
            return std::nullopt;
        }
        if (!env.merge(*subMatch)) {
            std::cout << "Failed to merge environment for element " << i << "\n";
            return std::nullopt;
        }
    }

    // If pattern is variadic, match remaining elements
    if (pattern.isVariadic && i < pattern.elements.size()) {
        auto varPattern = pattern.elements[i];
        std::vector<std::shared_ptr<Expression>> varElements;

        // Collect all remaining syntax elements
        for (; i < syntax.elements.size(); i++) {
            if (auto subMatch = match(varPattern, syntax.elements[i])) {
                // For each binding in the submatch, accumulate in the environment
                for (const auto& [key, values] : subMatch->bindings) {
                    auto& targetList = env.bindings[key];
                    targetList.insert(targetList.end(), values.begin(), values.end());
                }
            } else {
                return std::nullopt;
            }
        }
    }

    std::cout << "List match successful\n";
    return env;
}

std::shared_ptr<Expression> PatternSubstitutor::substitute(
    std::shared_ptr<Expression> templateExpr,
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
                                      return it->second[0]; // Return first binding for non-variadic case
                                  }
                              }
                              // Otherwise return copy of original atom
                              return std::make_shared<Expression>(Expression { atom, templateExpr->line });
                          },

                          [&](const ListExpression& list) -> std::shared_ptr<Expression> {
                              std::vector<std::shared_ptr<Expression>> newElements;

                              for (const auto& elem : list.elements) {
                                  if (auto atomExpr = std::get_if<AtomExpression>(&elem->as)) {
                                      // Check if it's a pattern variable
                                      auto it = env.bindings.find(atomExpr->value.lexeme);
                                      if (it != env.bindings.end()) {
                                          // If this is a variadic context, expand all bindings
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

                              return std::make_shared<Expression>(Expression {
                                  ListExpression { newElements, list.isVariadic },
                                  templateExpr->line });
                          },

                          [&](const auto&) -> std::shared_ptr<Expression> {
                              return templateExpr;
                          } },
        templateExpr->as);
}
