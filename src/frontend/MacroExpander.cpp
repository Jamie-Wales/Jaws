#include "MacroExpander.h"
#include "PatternMatcher.h"
#include "Transformer.h"
#include <iostream>

namespace pattern {

namespace {
    bool isMacroCall(const std::shared_ptr<Expression>& expr, const Environment& env)
    {
        if (auto* sexpr = std::get_if<sExpression>(&expr->as)) {
            if (!sexpr->elements.empty()) {
                if (auto* atom = std::get_if<AtomExpression>(&sexpr->elements[0]->as)) {
                    std::cout << "DEBUG: Checking if " << atom->value.lexeme << " is a macro\n";
                    bool result = env.isMacro(atom->value.lexeme);
                    std::cout << "DEBUG: " << atom->value.lexeme << " is "
                              << (result ? "" : "not ") << "a macro\n";
                    return result;
                }
            }
        }
        return false;
    }
}

std::optional<std::shared_ptr<Expression>> MacroExpander::expandMacrosIn(
    interpret::InterpreterState& state,
    const std::shared_ptr<Expression>& expr)
{
    if (!expr)
        return std::nullopt;

    std::cout << "DEBUG: Checking for macros in: " << expr->toString() << "\n";

    if (isMacroCall(expr, *state.env)) {
        auto* sexpr = std::get_if<sExpression>(&expr->as);
        auto* atom = std::get_if<AtomExpression>(&sexpr->elements[0]->as);
        auto name = atom->value.lexeme;

        std::cout << "DEBUG: Found macro call: " << name << "\n";

        auto rules = state.env->getMacroDefinition(name);
        if (!rules) {
            std::cout << "DEBUG: No rules found for macro\n";
            return std::nullopt;
        }

        if (auto expanded = expandMacro(state, name, *sexpr, rules->literals)) {
            std::cout << "DEBUG: Successfully expanded macro to: " << (*expanded)->toString() << "\n";
            // Recursively expand any macros in the expanded code
            return expandMacrosIn(state, *expanded);
        }
        std::cout << "DEBUG: Macro expansion failed\n";
        return std::nullopt;
    }

    return std::visit(overloaded {
                          [&](const sExpression& sexpr) -> std::optional<std::shared_ptr<Expression>> {
                              // Not a macro call, expand elements recursively
                              std::vector<std::shared_ptr<Expression>> newElements;
                              bool changed = false;

                              for (const auto& elem : sexpr.elements) {
                                  if (auto expanded = expandMacrosIn(state, elem)) {
                                      newElements.push_back(*expanded);
                                      changed = true;
                                  } else {
                                      newElements.push_back(elem);
                                  }
                              }

                              if (changed) {
                                  return std::make_shared<Expression>(
                                      Expression { sExpression { std::move(newElements) }, expr->line });
                              }
                              return expr;
                          },

                          [&](const auto& other) -> std::optional<std::shared_ptr<Expression>> {
                              return expr;
                          } },
        expr->as);
}

std::optional<std::shared_ptr<Expression>> MacroExpander::expandMacro(
    interpret::InterpreterState& state,
    const std::string& macroName,
    const sExpression& sexpr,
    const std::vector<Token>& literals)
{
    std::cout << "DEBUG: Expanding macro: " << macroName << "\n";
    std::cout << "DEBUG: With arguments: ";
    for (size_t i = 1; i < sexpr.elements.size(); ++i) {
        std::cout << sexpr.elements[i]->toString() << " ";
    }
    std::cout << "\n";

    auto expr = std::make_shared<Expression>(sExpression(sexpr.elements), sexpr.elements[0]->line);
    auto rules = state.env->getMacroDefinition(macroName);

    if (!rules) {
        std::cout << "DEBUG: No rules found for macro\n";
        return std::nullopt;
    }

    std::cout << "DEBUG: Found " << rules->rules.size() << " rules to try\n";

    for (const auto& rule : rules->rules) {
        std::cout << "DEBUG: Trying pattern: " << rule.pattern->toString() << "\n";

        auto matchResult = PatternMatcher::match(rule.pattern, expr, literals);
        if (matchResult) {
            std::cout << "DEBUG: Pattern matched!\n";
            auto transformed = Transformer::transform(rule.template_expr, *matchResult);
            if (transformed) {
                std::cout << "DEBUG: Transformation result: " << transformed->toString() << "\n";
                return transformed;
            }
        }
    }

    return std::nullopt;
}

} // namespace pattern
