#include "Transformer.h"
#include "Expression.h"
#include "MacroTraits.h"
#include "Visit.h"
#include <iostream>

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

    void debugPrintEnv(const MatchEnv& env)
    {
        debugPrint("Environment contents:");
        for (const auto& [key, value] : env) {
            std::cout << "  " << key << " -> [";
            for (const auto& match : value.matches) {
                std::cout << match->toString() << ", ";
            }
            std::cout << "]\n";
        }
    }
}

std::shared_ptr<Expression> Transformer::transform(
    const std::shared_ptr<Expression>& templ,
    const MatchEnv& env)
{
    debugPrintExpr("Transforming template", templ);
    debugPrintEnv(env);
    return transformExpr(templ, env);
}

std::shared_ptr<Expression> Transformer::transformExpr(
    const std::shared_ptr<Expression>& expr,
    const MatchEnv& env)
{
    if (!expr) {
        debugPrint("Null expression in transform");
        return nullptr;
    }

    return std::visit([&](const auto& val) {
        return transformVariant(val, env, expr->line);
    },
        expr->as);
}

template <typename T>
std::shared_ptr<Expression> Transformer::transformVariant(
    const T& expr,
    const MatchEnv& env,
    int line)
{
    using ExprType = std::decay_t<T>;
    debugPrint("Transforming Variant of type: " + std::string(typeid(ExprType).name()));

    if constexpr (std::is_same_v<ExprType, AtomExpression>) {
        const AtomExpression& atom = expr;
        debugPrint("Matched AtomExpression with value: " + atom.value.lexeme);
        auto it = env.find(atom.value.lexeme);
        if (it != env.end() && !it->second.matches.empty()) {
            debugPrintExpr("Found match", it->second.matches[0]);
            return it->second.matches[0];
        }
        return std::make_shared<Expression>(Expression { expr, line });
    }

    if constexpr (std::is_same_v<T, BeginExpression>) {
        debugPrint("Is beginExpression");
    }
    if constexpr (std::is_same_v<T, ListExpression>) {
        debugPrint("Is ListExpression");
    }
    if constexpr (std::is_same_v<T, sExpression>) {
        debugPrint("Is sExpression");
    }
    if constexpr (std::is_same_v<T, QuoteExpression>) {
        debugPrint("Is QuoteExpression");
    }

    if constexpr (expr::traits::is_pattern_variable<ExprType>::value) {
        debugPrint("transforming pattern");
        return transformPatternVariable(expr, env, line);
    } else if constexpr (expr::traits::is_list_like<ExprType>::value) {
        debugPrint("Transforming list like variable");
        return transformListLike(expr, env, line);
    } else {
        debugPrint("Unhandled type");
        return std::make_shared<Expression>(Expression { expr, line });
    }
}
std::shared_ptr<Expression> Transformer::transformPatternVariable(
    const AtomExpression& expr,
    const MatchEnv& env,
    int line)
{
    const auto& name = expr.value.lexeme;
    debugPrint("Transforming pattern variable: " + name);
    auto it = env.find(name);
    if (it == env.end()) {
        debugPrint("No binding found - preserving original");
        return std::make_shared<Expression>(Expression { expr, line });
    }

    const auto& matches = it->second.matches;
    if (matches.empty()) {
        debugPrint("No matches for pattern variable");
        return nullptr;
    }

    debugPrint("Using bound value: " + matches[0]->toString());
    return matches[0];
}

template <typename T>
std::shared_ptr<Expression> Transformer::transformListLike(
    const T& expr,
    const MatchEnv& env,
    int line)
{
    debugPrint("Transforming list-like expression");
    const auto& elements = expr::traits::get_expression_elements(expr);
    std::vector<std::shared_ptr<Expression>> newElements;
    bool changed = false;

    for (const auto& elem : elements) {
        if (auto transformed = transformExpr(elem, env)) {
            newElements.push_back(transformed);
            changed = changed || (transformed != elem);
        } else {
            debugPrint("Element transformation failed");
            newElements.push_back(elem);
        }
    }

    if constexpr (std::is_same_v<T, BeginExpression>) {
        return std::make_shared<Expression>(
            Expression { BeginExpression { std::move(newElements) }, line });
    } else {
        return std::make_shared<Expression>(
            Expression { ListExpression { std::move(newElements), expr.isVariadic }, line });
    }
}
}
