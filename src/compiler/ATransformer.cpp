#include "ATransformer.h"
#include <stdexcept>
namespace ir {

std::vector<std::shared_ptr<ANF>> ANFtransform(std::vector<std::shared_ptr<Expression>> expressions)
{
    size_t currentNumber = 0;
    std::vector<std::shared_ptr<ANF>> output = {};
    for (const auto& expression : expressions) {
        if (auto element = transform(expression, currentNumber))
            output.push_back(*element);
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
        transformed_bindings.push_back({ name, *transformed_value });
    }

    std::shared_ptr<ANF> body = nullptr;
    for (auto it = l.body.rbegin(); it != l.body.rend(); ++it) {
        auto transformed_expr = transform(*it, currentNumber);
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

    for (auto it = transformed_bindings.rbegin(); it != transformed_bindings.rend(); ++it) {
        body = std::make_shared<ANF>(Let {
            std::optional<Token>(it->first),
            it->second,
            body });
    }

    return body;
}
std::shared_ptr<ANF> ASExpr(const sExpression& s, size_t& currentNumber)
{
    std::vector<std::shared_ptr<ANF>> transformed_args;
    transformed_args.reserve(s.elements.size());
    for (const auto& arg : s.elements) {
        auto transformed = transform(arg, currentNumber);
        if (!transformed) {
            throw std::runtime_error("Failed to transform sExpression argument");
        }
        transformed_args.push_back(*transformed);
    }

    auto func_result = transform(s.elements[0], currentNumber);
    if (!func_result) {
        throw std::runtime_error("Failed to transform function position");
    }

    std::vector<Token> final_args;
    std::shared_ptr<ANF> result = std::make_shared<ANF>(App {
        Token {},
        final_args,
        false });

    for (size_t i = 1; i < transformed_args.size(); i++) {
        Token temp = { Tokentype::IDENTIFIER, std::format("temp{}", currentNumber++), 0, 0 };
        final_args.push_back(temp);
        // Create Let wrapped in ANFTerm variant
        result = std::make_shared<ANF>(ANF::ANFTerm(Let {
            std::optional<Token>(temp),
            transformed_args[i],
            result }));
    }

    Token func_temp = { Tokentype::IDENTIFIER, std::format("temp{}", currentNumber++), 0, 0 };
    // Create final Let wrapped in ANFTerm variant
    return std::make_shared<ANF>(ANF::ANFTerm(Let {
        std::optional<Token>(func_temp),
        *func_result,
        result }));
}

std::optional<std::shared_ptr<ANF>> transform(std::shared_ptr<Expression>& toTransform, size_t& currentNumber)
{

    return std::visit(overloaded {
                          [&](const AtomExpression& e) -> std::optional<std::shared_ptr<ANF>> { return Aatom(e); },
                          [&](const ListExpression& e) -> std::optional<std::shared_ptr<ANF>> { return std::nullopt; },
                          [&](const BeginExpression& e) -> std::optional<std::shared_ptr<ANF>> { return std::nullopt; },
                          [&](const sExpression& e) -> std::optional<std::shared_ptr<ANF>> { return std::nullopt; },
                          [&](const DefineExpression& e) -> std::optional<std::shared_ptr<ANF>> { return std::nullopt; },
                          [&](const DefineSyntaxExpression& e) -> std::optional<std::shared_ptr<ANF>> { return std::nullopt; },
                          [&](const DefineProcedure& e) -> std::optional<std::shared_ptr<ANF>> { return std::nullopt; },
                          [&](const LambdaExpression& e) -> std::optional<std::shared_ptr<ANF>> { return std::nullopt; },
                          [&](const IfExpression& e) -> std::optional<std::shared_ptr<ANF>> { return std::nullopt; },
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
