#include "ATransformer.h"
#include <optional>
#include <ranges>
#include <stdexcept>

namespace ir {

std::vector<std::shared_ptr<ANF>> ANFtransform(const std::vector<std::shared_ptr<Expression>> &expressions)
{
    size_t currentNumber = 0;
    std::vector<std::shared_ptr<ANF>> output = {};
    for (const auto& expression : expressions) {
        if (auto element = transform(expression, currentNumber)) {
            output.push_back(*element);
        }
    }

    return output;
}

std::shared_ptr<ANF> Aatom(const AtomExpression& e)
{
    return std::make_shared<ANF>(Atom { e.value });
}

std::shared_ptr<ANF> ALet(const LetExpression& l, size_t currentNumber)
{

    std::vector<std::pair<Token, std::shared_ptr<ANF>>> transformed_bindings;

    for (const auto& [name, value] : l.arguments) {
        auto transformed_value = transform(value, currentNumber);
        if (!transformed_value) {
            throw std::runtime_error("Failed to transform let binding");
        }
        transformed_bindings.emplace_back( name, *transformed_value );
    }

    std::shared_ptr<ANF> body = nullptr;
    for (const auto& it : std::ranges::reverse_view(l.body)) {
        auto transformed_expr = transform(it, currentNumber);
        if (!transformed_expr) {
            throw std::runtime_error("Failed to transform let body");
        }
        if (!body) {
            body = *transformed_expr;
        } else {
            Token temp = { Tokentype::IDENTIFIER, std::format("temp{}", currentNumber++), 0, 0 };
            body = std::make_shared<ANF>(Let {
                std::optional<Token>(temp),
                *transformed_expr,
                body });
        }
    }

    for (auto& [fst, snd] : std::ranges::reverse_view(transformed_bindings)) {
        body = std::make_shared<ANF>(Let {
            std::optional<Token>(fst),
            snd,
            body });
    }

    return body;
}

std::shared_ptr<ANF> ASExpr(const sExpression& s, size_t& currentNumber)
{
    if (s.elements.empty()) {
        throw std::runtime_error("Empty s-expression");
    }
    const auto func_expr = transform(s.elements[0], currentNumber);
    if (!func_expr) {
        throw std::runtime_error("Failed to transform function position");
    }
    Token func_name;
    if (const auto atom = std::get_if<Atom>(&(*func_expr)->term)) {
        func_name = atom->atom;
    } else {
        func_name = Token { Tokentype::IDENTIFIER, std::format("temp{}", currentNumber++), 0, 0 };
    }
    std::vector<Token> final_args;
    std::vector<std::pair<Token, std::shared_ptr<ANF>>> bindings;
    for (size_t i = 1; i < s.elements.size(); i++) {
        auto arg_expr = transform(s.elements[i], currentNumber);
        if (!arg_expr) {
            throw std::runtime_error("Failed to transform argument");
        }

        if (const auto atom = std::get_if<Atom>(&(*arg_expr)->term)) {
            final_args.push_back(atom->atom);
        } else {
            Token temp = { Tokentype::IDENTIFIER, std::format("temp{}", currentNumber++), 0, 0 };
            final_args.push_back(temp);
            bindings.emplace_back(temp, *arg_expr);
        }
    }

    const auto app = std::make_shared<ANF>(App { func_name, final_args, false });
    auto result = app;
    for (auto& [first, second] : std::ranges::reverse_view(bindings)) {
        result = std::make_shared<ANF>(Let {
            std::optional<Token>(first),
            second,
            result });
    }
    if (!std::get_if<Atom>(&(*func_expr)->term)) {
        result = std::make_shared<ANF>(Let {
            std::optional<Token>(func_name),
            *func_expr,
            result });
    }

    return result;
}

std::shared_ptr<ANF> AIf(const IfExpression& ie, size_t& currentNumber)
{
    const auto anf_cond = transform(ie.condition, currentNumber);
    if (!anf_cond) {
        throw std::runtime_error("Failed to transform if condition");
    }
    const auto anf_then = transform(ie.then, currentNumber);
    if (!anf_then) {
        throw std::runtime_error("Failed to transform then branch");
    }
    std::optional<std::shared_ptr<ANF>> anf_else = nullptr;
    if (ie.el) {
        anf_else = transform(*ie.el, currentNumber);
        if (!anf_else) {
            throw std::runtime_error("Failed to transform else branch");
        }
    }
    Token cond_name;
    if (const auto atom = std::get_if<Atom>(&(*anf_cond)->term)) {
        cond_name = atom->atom;
    } else {
        cond_name = { Tokentype::IDENTIFIER, std::format("temp{}", currentNumber++), 0, 0 };
    }
    auto result = std::make_shared<ANF>(If { cond_name, *anf_then, anf_else });
    if (!std::get_if<Atom>(&(*anf_cond)->term)) {
        result = std::make_shared<ANF>(Let {
            std::optional<Token>(cond_name),
            *anf_cond,
            result });
    }

    return result;
}

std::optional<std::shared_ptr<ANF>> transform(const std::shared_ptr<Expression>& toTransform, size_t& currentNumber)
{

    return std::visit(overloaded {
                          [&](const AtomExpression& e) -> std::optional<std::shared_ptr<ANF>> { return Aatom(e); },
                          [&](const ListExpression& e) -> std::optional<std::shared_ptr<ANF>> { return std::nullopt; },
                          [&](const BeginExpression& e) -> std::optional<std::shared_ptr<ANF>> { return std::nullopt; },
                          [&](const sExpression& e) -> std::optional<std::shared_ptr<ANF>> { return ASExpr(e, currentNumber); },
                          [&](const DefineExpression& e) -> std::optional<std::shared_ptr<ANF>> { return std::nullopt; },
                          [&](const DefineSyntaxExpression& e) -> std::optional<std::shared_ptr<ANF>> { return std::nullopt; },
                          [&](const DefineProcedure& e) -> std::optional<std::shared_ptr<ANF>> { return std::nullopt; },
                          [&](const LambdaExpression& e) -> std::optional<std::shared_ptr<ANF>> { return std::nullopt; },
                          [&](const IfExpression& e) -> std::optional<std::shared_ptr<ANF>> { return AIf(e, currentNumber); },
                          [&](const QuoteExpression& e) -> std::optional<std::shared_ptr<ANF>> { return std::nullopt; },
                          [&](const VectorExpression& e) -> std::optional<std::shared_ptr<ANF>> { return std::nullopt; },
                          [&](const TailExpression& e) -> std::optional<std::shared_ptr<ANF>> { return transform(e.expression, currentNumber); },
                          [&](const LetExpression& e) -> std::optional<std::shared_ptr<ANF>> { return ALet(e, currentNumber); },
                          [&](const ImportExpression& e) -> std::optional<std::shared_ptr<ANF>> { return std::nullopt; },
                          [&](const SetExpression& e) -> std::optional<std::shared_ptr<ANF>> { return std::nullopt; },
                          [&](const auto& e) -> std::optional<std::shared_ptr<ANF>> {
                              throw std::runtime_error("Unknown expression type");
                          },
                      },
        toTransform->as);
};
}
