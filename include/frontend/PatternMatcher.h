#pragma once
#include "MacroTraits.h"

namespace pattern {

using MatchEnv = expr::MatchEnv;

class PatternMatcher {
public:
    static std::optional<MatchEnv> match(
        const std::shared_ptr<Expression>& pattern,
        const std::shared_ptr<Expression>& expr,
        const std::vector<Token>& literals);

private:
    static bool matchExpr(
        const std::shared_ptr<Expression>& pattern,
        const std::shared_ptr<Expression>& expr,
        MatchEnv& env,
        const std::vector<Token>& literals);

    template <typename P, typename E>
    static bool matchVariant(
        const P& pattern,
        const E& expr,
        MatchEnv& env,
        const std::vector<Token>& literals);

    static bool matchPatternVariable(
        const AtomExpression& pattern,
        const auto& expr,
        MatchEnv& env,
        const std::vector<Token>& literals);

    template <typename P, typename E>
    static bool matchListLike(
        const P& pattern,
        const E& expr,
        MatchEnv& env,
        const std::vector<Token>& literals);

    static bool matchVariadicList(
        const std::vector<std::shared_ptr<Expression>>& pattern,
        const std::vector<std::shared_ptr<Expression>>& expr,
        MatchEnv& env,
        const std::vector<Token>& literals);
};

} // namespace pattern
