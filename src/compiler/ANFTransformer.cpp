#include "ANFTransformer.h"
#include "Visit.h"
#include <memory>
#include <optional>
#include <ranges>
#include <stdexcept>

namespace ir {

std::shared_ptr<ANF> flattenLets(std::shared_ptr<ANF> expr)
{
    if (!expr) {
        return nullptr;
    }

    return std::visit(overloaded {
                          [&](const Atom& a) -> std::shared_ptr<ANF> {
                              return expr;
                          },
                          [&](Let& l) -> std::shared_ptr<ANF> {
                              // First check if binding exists
                              if (!l.binding) {
                                  throw std::runtime_error("Let binding is null");
                              }

                              // Recursively flatten the binding and body
                              l.binding = flattenLets(l.binding);
                              if (l.body) {
                                  l.body = flattenLets(l.body);
                              }

                              // Check if binding is a Let expression
                              if (auto innerLet = std::get_if<Let>(&l.binding->term)) {
                                  // Verify all required components exist
                                  if (!innerLet->binding || !innerLet->body) {
                                      throw std::runtime_error("Inner let has null binding or body");
                                  }

                                  // Create new structure carefully
                                  auto newBody = std::make_shared<ANF>(ANF {
                                      Let {
                                          l.name,
                                          innerLet->body,
                                          l.body } });

                                  if (!newBody) {
                                      throw std::runtime_error("Failed to create new body");
                                  }

                                  auto pulledOut = std::make_shared<ANF>(ANF {
                                      Let {
                                          innerLet->name,
                                          innerLet->binding,
                                          newBody } });

                                  if (!pulledOut) {
                                      throw std::runtime_error("Failed to create pulled out expression");
                                  }

                                  return flattenLets(pulledOut);
                              }
                              return expr;
                          },
                          [&](const App& app) -> std::shared_ptr<ANF> {
                              return expr;
                          },
                          [&](If& _if) -> std::shared_ptr<ANF> {
                              if (!_if.then) {
                                  throw std::runtime_error("If statement has null then branch");
                              }

                              _if.then = flattenLets(_if.then);
                              if (_if._else) {
                                  _if._else = flattenLets(*_if._else);
                              }
                              return expr;
                          },
                          [&](const Quote& _) -> std::shared_ptr<ANF> {
                              return expr;
                          },
                          [&](Lambda& l) -> std::shared_ptr<ANF> {
                              if (!l.body) {
                                  throw std::runtime_error("Lambda has null body");
                              }

                              l.body = flattenLets(l.body);
                              return expr;
                          },
                          [&](const auto& _) -> std::shared_ptr<ANF> {
                              throw std::runtime_error("Unable to transform ANF Type");
                          } },
        expr->term);
}

std::vector<std::shared_ptr<TopLevel>> ANFtransform(const std::vector<std::shared_ptr<Expression>>& expressions)
{
    size_t currentNumber = 0;
    std::vector<std::shared_ptr<TopLevel>> output;
    output.reserve(expressions.size());

    for (const auto& expression : expressions) {
        if (!expression)
            continue;
        auto transformed = transformTop(expression, currentNumber);
        if (transformed && *transformed) {
            output.push_back(*transformed);
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
        transformed_bindings.emplace_back(name, *transformed_value);
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

    Token cond_name = { Tokentype::IDENTIFIER, std::format("g{}", currentNumber++), 0, 0 };
    auto result = std::make_shared<ANF>(If { cond_name, *anf_then, anf_else });

    result = std::make_shared<ANF>(Let {
        std::optional<Token>(cond_name),
        *anf_cond,
        result });

    return result;
}

std::shared_ptr<ANF> ADefine(const DefineExpression& define, size_t& currentNumber)
{
    auto transformed_value = transform(define.value, currentNumber);
    if (!transformed_value) {
        throw std::runtime_error("Failed to transform define value");
    }

    return *transformed_value;
}

std::shared_ptr<ANF> ASet(const SetExpression& set, size_t& currentNumber)
{
    auto transformed_value = transform(set.value, currentNumber);
    if (!transformed_value) {
        throw std::runtime_error("Failed to transform define value");
    }
    Token value_token;
    std::shared_ptr<ANF> result;

    if (const auto atom = std::get_if<Atom>(&(*transformed_value)->term)) {
        value_token = atom->atom;
        result = std::make_shared<ANF>(App {
            Token { Tokentype::IDENTIFIER, "set!", 0, 0 },
            std::vector<Token> { set.identifier, value_token },
            false });
    } else {
        Token temp = { Tokentype::IDENTIFIER, std::format("temp{}", currentNumber++), 0, 0 };
        result = std::make_shared<ANF>(Let {
            std::optional<Token>(temp),
            *transformed_value,
            std::make_shared<ANF>(App {
                Token { Tokentype::IDENTIFIER, "set!", 0, 0 },
                std::vector<Token> { set.identifier, temp },
                false }) });
    }
    return result;
}
std::shared_ptr<ANF> ADefineProcedure(const DefineProcedure& proc, size_t& currentNumber)
{
    std::shared_ptr<ANF> body = nullptr;
    for (const auto& expr : std::ranges::reverse_view(proc.body)) {
        auto transformed = transform(expr, currentNumber);
        if (!transformed) {
            throw std::runtime_error("Failed to transform procedure body");
        }
        if (!body) {
            body = *transformed;
        } else {
            Token temp = { Tokentype::IDENTIFIER, std::format("temp{}", currentNumber++), 0, 0 };
            body = std::make_shared<ANF>(Let {
                std::optional<Token>(temp),
                *transformed,
                body });
        }
    }

    return std::make_shared<ANF>(Lambda {
        proc.parameters,
        body });
}
std::optional<std::shared_ptr<ANF>> ALambda(const LambdaExpression& le, size_t& currentNumber)
{
    if (le.body.empty()) {
        return std::nullopt;
    }

    std::shared_ptr<ANF> body = nullptr;
    for (const auto& expr : std::ranges::reverse_view(le.body)) {
        auto transformed = transform(expr, currentNumber);
        if (!transformed) {
            return std::nullopt;
        }
        if (!body) {
            body = *transformed;
        } else {
            Token temp = { Tokentype::IDENTIFIER, std::format("temp{}", currentNumber++), 0, 0 };
            body = std::make_shared<ANF>(Let {
                std::optional<Token>(temp),
                *transformed,
                body });
        }
    }

    if (!body) {
        return std::nullopt;
    }

    const auto lambda = std::make_shared<ANF>(Lambda { le.parameters, body });
    Token lambdaTemp = { Tokentype::IDENTIFIER, std::format("temp{}", currentNumber++), 0, 0 };
    return std::make_shared<ANF>(Let {
        std::optional<Token>(lambdaTemp),
        lambda,
        std::make_shared<ANF>(Atom { lambdaTemp }) });
}
std::shared_ptr<ANF> AVector(const VectorExpression& ve, size_t& currentNumber)
{
    std::vector<Token> final_args;
    std::vector<std::pair<Token, std::shared_ptr<ANF>>> bindings;

    for (const auto& elem : ve.elements) {
        auto transformed = transform(elem, currentNumber);
        if (!transformed) {
            throw std::runtime_error("Failed to transform vector element");
        }
        if (const auto atom = std::get_if<Atom>(&(*transformed)->term)) {
            final_args.push_back(atom->atom);
        } else {
            Token temp = { Tokentype::IDENTIFIER, std::format("temp{}", currentNumber++), 0, 0 };
            final_args.push_back(temp);
            bindings.emplace_back(temp, *transformed);
        }
    }
    const auto app = std::make_shared<ANF>(App {
        Token { Tokentype::IDENTIFIER, "vector", 0, 0 },
        final_args,
        false });

    auto result = app;
    for (auto& [first, second] : std::ranges::reverse_view(bindings)) {
        result = std::make_shared<ANF>(Let {
            std::optional<Token>(first),
            second,
            result });
    }

    return result;
}

std::shared_ptr<ANF> AQuote(const QuoteExpression& qe, size_t& currentNumber)
{
    return std::make_shared<ANF>(ANF {
        Quote { qe.expression } });
}

std::optional<std::shared_ptr<ANF>> transform(const std::shared_ptr<Expression>& toTransform, size_t& currentNumber)
{
    return std::visit(overloaded {
                          [&](const AtomExpression& e) -> std::optional<std::shared_ptr<ANF>> {
                              return Aatom(e);
                          },
                          [&](const ListExpression& e) -> std::optional<std::shared_ptr<ANF>> { return std::nullopt; },
                          [&](const sExpression& e) -> std::optional<std::shared_ptr<ANF>> {
                              return ASExpr(e, currentNumber);
                          },
                          [&](const DefineExpression& e) -> std::optional<std::shared_ptr<ANF>> {
                              return ADefine(e, currentNumber);
                          },
                          [&](const DefineSyntaxExpression& e) -> std::optional<std::shared_ptr<ANF>> { return std::nullopt; },
                          [&](const DefineProcedure& e) -> std::optional<std::shared_ptr<ANF>> {
                              return ADefineProcedure(e, currentNumber);
                          },
                          [&](const LambdaExpression& e) -> std::optional<std::shared_ptr<ANF>> {
                              return ALambda(e, currentNumber);
                          },
                          [&](const IfExpression& e) -> std::optional<std::shared_ptr<ANF>> {
                              return AIf(e, currentNumber);
                          },
                          [&](const QuoteExpression& e) -> std::optional<std::shared_ptr<ANF>> {
                              return AQuote(e, currentNumber);
                          },
                          [&](const VectorExpression& e) -> std::optional<std::shared_ptr<ANF>> {
                              return AVector(e, currentNumber);
                          },
                          [&](const TailExpression& e) -> std::optional<std::shared_ptr<ANF>> {
                              return transform(e.expression, currentNumber);
                          },
                          [&](const LetExpression& e) -> std::optional<std::shared_ptr<ANF>> {
                              return ALet(e, currentNumber);
                          },
                          [&](const ImportExpression& e) -> std::optional<std::shared_ptr<ANF>> { return std::nullopt; },
                          [&](const SetExpression& e) -> std::optional<std::shared_ptr<ANF>> {
                              return ASet(e, currentNumber);
                          },
                          [&](const auto& e) -> std::optional<std::shared_ptr<ANF>> {
                              throw std::runtime_error("Unknown expression type");
                          },
                      },
        toTransform->as);
};

std::optional<std::shared_ptr<TopLevel>> transformTop(const std::shared_ptr<Expression>& toTransform, size_t& currentNumber)
{
    return std::visit(overloaded {
                          [&](const DefineExpression& e) -> std::optional<std::shared_ptr<TopLevel>> {
                              return std::make_shared<TopLevel>(TDefine {
                                  e.name, flattenLets(ADefine(e, currentNumber)) });
                          },
                          [&](const DefineSyntaxExpression& _) -> std::optional<std::shared_ptr<TopLevel>> { return std::nullopt; },
                          [&](const DefineProcedure& e) -> std::optional<std::shared_ptr<TopLevel>> {
                              return std::make_shared<TopLevel>(TDefine { e.name, flattenLets(ADefineProcedure(e, currentNumber)) });
                          },
                          [&](const auto& _) -> std::optional<std::shared_ptr<TopLevel>> {
                              if (const auto ele = transform(toTransform, currentNumber)) {
                                  return std::make_shared<TopLevel>(flattenLets(*ele));
                              }
                              return std::nullopt;
                          },
                      },
        toTransform->as);
}
}
