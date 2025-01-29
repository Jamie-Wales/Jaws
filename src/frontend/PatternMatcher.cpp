#include "PatternMatcher.h"
#include "Visit.h"
#include <iostream>
#include <typeinfo>

namespace pattern {
namespace {
    void debugPrint(const std::string& msg)
    {
        std::cout << "DEBUG: " << msg << std::endl;
    }

    void debugPrintExpr(const std::string& prefix, const std::shared_ptr<Expression>& expr)
    {
        std::cout << "DEBUG: " << prefix << ": " << expr->toString() << std::endl;
    }
}

std::optional<MatchEnv> PatternMatcher::match(
    const std::shared_ptr<Expression>& pattern,
    const std::shared_ptr<Expression>& expr,
    const std::vector<Token>& literals)
{
    debugPrintExpr("Matching pattern", pattern);
    debugPrintExpr("Against expression", expr);

    MatchEnv env;
    if (matchExpr(pattern, expr, env, literals)) {
        debugPrint("Match successful");
        return env;
    }
    debugPrint("Match failed");
    return std::nullopt;
}

bool PatternMatcher::matchExpr(
    const std::shared_ptr<Expression>& pattern,
    const std::shared_ptr<Expression>& expr,
    MatchEnv& env,
    const std::vector<Token>& literals)
{
    if (!pattern || !expr) {
        debugPrint("Null pattern or expression");
        return false;
    }

    return std::visit(overloaded {
                          [&](const auto& p, const auto& e) {
                              return matchVariant(p, e, env, literals);
                          } },
        pattern->as, expr->as);
}

template <typename P, typename E>
bool PatternMatcher::matchVariant(
    const P& pattern,
    const E& expr,
    MatchEnv& env,
    const std::vector<Token>& literals)
{
    using PatternType = std::decay_t<P>;
    using ExprType = std::decay_t<E>;

    debugPrint("Matching variant of type: " + std::string(typeid(PatternType).name()));

    if constexpr (expr::traits::is_pattern_variable<PatternType>::value) {
        debugPrint("Found pattern variable");
        return matchPatternVariable(pattern, expr, env, literals);
    } else if constexpr (expr::traits::is_list_like<PatternType>::value && expr::traits::is_list_like<ExprType>::value) {
        debugPrint("Found list-like pattern");
        return matchListLike(pattern, expr, env, literals);
    } else if constexpr (std::is_same_v<PatternType, ExprType>) {
        debugPrint("Direct type match");
        if constexpr (std::is_same_v<PatternType, AtomExpression>) {
            debugPrint("Comparing atoms: " + pattern.value.lexeme + " == " + expr.value.lexeme);
            return pattern.value.lexeme == expr.value.lexeme;
        }
        return true;
    }
    debugPrint("No match path found");
    return false;
}

bool PatternMatcher::matchVariadicList(
    const std::vector<std::shared_ptr<Expression>>& pattern,
    const std::vector<std::shared_ptr<Expression>>& expr,
    MatchEnv& env,
    const std::vector<Token>& literals)
{
    if (pattern.empty()) {
        debugPrint("Empty pattern in variadic list");
        return false;
    }

    size_t fixedCount = pattern.size() - 1;
    if (expr.size() < fixedCount) {
        debugPrint("Expression too short for variadic pattern");
        return false;
    }

    debugPrint("Matching fixed part of variadic pattern");
    for (size_t i = 0; i < fixedCount; i++) {
        if (!matchExpr(pattern[i], expr[i], env, literals)) {
            debugPrint("Fixed part match failed at index " + std::to_string(i));
            return false;
        }
    }

    if (auto* lastPattern = std::get_if<AtomExpression>(&pattern.back()->as)) {
        debugPrint("Capturing variadic elements under: " + lastPattern->value.lexeme);
        auto& matches = env[lastPattern->value.lexeme].matches;
        for (size_t i = fixedCount; i < expr.size(); i++) {
            matches.push_back(expr[i]);
        }
        return true;
    }

    return false;
}

template <typename P, typename E>
bool PatternMatcher::matchListLike(
    const P& pattern,
    const E& expr,
    MatchEnv& env,
    const std::vector<Token>& literals)
{
    const auto& patElements = expr::traits::get_elements<P>::get(pattern);
    const auto& exprElements = expr::traits::get_elements<E>::get(expr);

    debugPrint("List matching: pattern size = " + std::to_string(patElements.size()) + ", expr size = " + std::to_string(exprElements.size()));

    if constexpr (expr::traits::has_variadic_v<P>) {
        if (pattern.isVariadic) {
            debugPrint("Pattern is variadic");
            return matchVariadicList(patElements, exprElements, env, literals);
        }
    }

    if (patElements.size() != exprElements.size()) {
        debugPrint("Size mismatch in list matching");
        return false;
    }

    for (size_t i = 0; i < patElements.size(); i++) {
        debugPrint("Matching list element " + std::to_string(i));
        if (!matchExpr(patElements[i], exprElements[i], env, literals)) {
            debugPrint("List element match failed");
            return false;
        }
    }
    return true;
}

bool PatternMatcher::matchPatternVariable(
    const AtomExpression& pattern,
    const auto& expr,
    MatchEnv& env,
    const std::vector<Token>& literals)
{
    const auto& name = pattern.value.lexeme;
    debugPrint("Matching pattern variable: " + name);

    bool isLiteral = std::find_if(literals.begin(), literals.end(),
                         [&](const Token& lit) { return lit.lexeme == name; })
        != literals.end();

    if (isLiteral) {
        debugPrint("Pattern is a literal");
        if constexpr (expr::traits::is_literal_matchable<std::decay_t<decltype(expr)>>::value) {
            return name == expr.value.lexeme;
        }
        return false;
    }

    if (name != "_") {
        debugPrint("Binding pattern variable: " + name);
        auto exprPtr = std::make_shared<Expression>(Expression { expr, pattern.value.line });
        env[name].matches.push_back(exprPtr);
    }
    return true;
}

} // namespace pattern
