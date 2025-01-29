#pragma once
#include "MacroTraits.h"

namespace pattern {

using MatchEnv = expr::MatchEnv;

class Transformer {
public:
    static std::shared_ptr<Expression> transform(
        const std::shared_ptr<Expression>& templ,
        const MatchEnv& env);

private:
    template <typename T>
    static std::shared_ptr<Expression> transformVariant(
        const T& expr,
        const MatchEnv& env,
        int line);

    static std::shared_ptr<Expression> transformPatternVariable(
        const AtomExpression& expr,
        const MatchEnv& env,
        int line);

    template <typename T>
    static std::shared_ptr<Expression> transformListLike(
        const T& expr,
        const MatchEnv& env,
        int line);

    static std::shared_ptr<Expression> transformExpr(
        const std::shared_ptr<Expression>& expr,
        const MatchEnv& env);
};
} // namespace pattern
