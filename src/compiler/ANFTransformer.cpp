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
                              if (!l.binding) {
                                  throw std::runtime_error("Let binding is null");
                              }

                              l.binding = flattenLets(l.binding);
                              if (l.body) {
                                  l.body = flattenLets(l.body);
                              }
                              if (auto innerLet = std::get_if<Let>(&l.binding->term)) {
                                  if (!innerLet->binding || !innerLet->body) {
                                      throw std::runtime_error("Inner let has null binding or body");
                                  }
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

// Forward declaration with tail position parameter
std::optional<std::shared_ptr<ANF>> transform(const std::shared_ptr<Expression>& toTransform, size_t& currentNumber, bool inTailPosition = false);

std::shared_ptr<ANF> Aatom(const AtomExpression& e)
{
    return std::make_shared<ANF>(Atom { e.value.token });
}

std::shared_ptr<ANF> ABegin(const BeginExpression& begin, size_t& currentNumber, bool inTailPosition)
{
    if (begin.values.empty()) {
        throw std::runtime_error("Empty begin expression encountered during ANF transform");
    }

    std::shared_ptr<ANF> body = nullptr;
    size_t index = 0;
    for (const auto& expr : std::ranges::reverse_view(begin.values)) {
        // Only the last expression (first in reverse) is in tail position
        bool isTail = (index == 0) && inTailPosition;
        auto transformed_opt = transform(expr, currentNumber, isTail);
        if (!transformed_opt) {
            throw std::runtime_error("Failed to transform expression within begin");
        }
        auto transformed = *transformed_opt;

        if (!body) {
            body = transformed;
        } else {
            body = std::make_shared<ANF>(Let {
                std::nullopt,
                transformed,
                body });
        }
        index++;
    }

    if (!body) {
        throw std::logic_error("Internal error: Begin transformation resulted in null body");
    }

    return body;
}

std::shared_ptr<ANF> AQuasiQuote(const QuasiQuoteExpression& qqe, size_t& currentNumber)
{
    return std::make_shared<ANF>(Quote { qqe.value });
}

std::optional<std::shared_ptr<ANF>> AUnquote(const UnquoteExpression& uqe, size_t& currentNumber, bool inTailPosition)
{
    return transform(uqe.value, currentNumber, inTailPosition);
}

std::optional<std::shared_ptr<ANF>> ASplice(const SpliceExpression& se, size_t& currentNumber, bool inTailPosition)
{
    return transform(se.value, currentNumber, inTailPosition);
}

std::shared_ptr<ANF> ALet(const LetExpression& l, size_t& currentNumber, bool inTailPosition)
{
    std::vector<std::pair<Token, std::shared_ptr<ANF>>> transformed_bindings;

    for (const auto& [name, value] : l.arguments) {
        // Bindings are never in tail position
        auto transformed_value = transform(value, currentNumber, false);
        if (!transformed_value) {
            throw std::runtime_error("Failed to transform let binding");
        }
        transformed_bindings.emplace_back(name.token, *transformed_value);
    }

    std::shared_ptr<ANF> body = nullptr;
    size_t index = 0;
    for (const auto& it : std::ranges::reverse_view(l.body)) {
        // Only the last expression is in tail position
        bool isTail = (index == 0) && inTailPosition;
        auto transformed_expr = transform(it, currentNumber, isTail);
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
        index++;
    }

    for (auto& [fst, snd] : std::ranges::reverse_view(transformed_bindings)) {
        body = std::make_shared<ANF>(Let {
            std::optional<Token>(fst),
            snd,
            body });
    }

    return body;
}

std::shared_ptr<ANF> ASExpr(const sExpression& s, size_t& currentNumber, bool inTailPosition)
{
    if (s.elements.empty()) {
        throw std::runtime_error("Empty s-expression");
    }
    // Function position is never in tail position
    const auto func_expr = transform(s.elements[0], currentNumber, false);
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
        // Arguments are never in tail position
        auto arg_expr = transform(s.elements[i], currentNumber, false);
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

    // Mark the App as a tail call if we're in tail position
    const auto app = std::make_shared<ANF>(App { func_name, final_args, inTailPosition });
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

std::shared_ptr<ANF> AIf(const IfExpression& ie, size_t& currentNumber, bool inTailPosition)
{
    // Condition is never in tail position
    const auto anf_cond = transform(ie.condition, currentNumber, false);
    if (!anf_cond) {
        throw std::runtime_error("Failed to transform if condition");
    }
    // Both branches inherit tail position
    const auto anf_then = transform(ie.then, currentNumber, inTailPosition);
    if (!anf_then) {
        throw std::runtime_error("Failed to transform then branch");
    }
    std::optional<std::shared_ptr<ANF>> anf_else = nullptr;
    if (ie.el) {
        anf_else = transform(*ie.el, currentNumber, inTailPosition);
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

std::shared_ptr<ANF> ADefine(const DefineExpression& define, size_t& currentNumber, bool inTailPosition)
{
    // Define values are never in tail position
    auto transformed_value = transform(define.value, currentNumber, false);
    if (!transformed_value) {
        throw std::runtime_error("Failed to transform define value");
    }

    return *transformed_value;
}

std::shared_ptr<ANF> ASet(const SetExpression& set, size_t& currentNumber, bool inTailPosition)
{
    // Set values are never in tail position
    auto transformed_value = transform(set.value, currentNumber, false);
    if (!transformed_value) {
        throw std::runtime_error("Failed to transform define value");
    }
    Token value_token;
    std::shared_ptr<ANF> result;

    if (const auto atom = std::get_if<Atom>(&(*transformed_value)->term)) {
        value_token = atom->atom;
        result = std::make_shared<ANF>(App {
            Token { Tokentype::IDENTIFIER, "set!", 0, 0 },
            std::vector<Token> { set.identifier.token, value_token },

            inTailPosition });
    } else {
        Token temp = { Tokentype::IDENTIFIER, std::format("temp{}", currentNumber++), 0, 0 };
        result = std::make_shared<ANF>(Let {
            std::optional<Token>(temp),
            *transformed_value,
            std::make_shared<ANF>(App {
                Token { Tokentype::IDENTIFIER, "set!", 0, 0 },
                std::vector<Token> { set.identifier.token, temp },
                inTailPosition }) });
    }
    return result;
}

std::shared_ptr<ANF> ADefineProcedure(const DefineProcedure& proc, size_t& currentNumber)
{
    std::shared_ptr<ANF> body = nullptr;
    size_t index = 0;
    for (const auto& expr : std::ranges::reverse_view(proc.body)) {
        // Only the last expression in the body is in tail position
        bool isTail = (index == 0);
        auto transformed = transform(expr, currentNumber, isTail);
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
        index++;
    }

    // Extract Token from HygienicSyntax
    std::vector<Token> paramTokens;
    paramTokens.reserve(proc.parameters.size());
    for (const auto& param : proc.parameters) {
        paramTokens.push_back(param.token);
    }

    return std::make_shared<ANF>(Lambda {
        paramTokens,
        body });
}

std::optional<std::shared_ptr<ANF>> ALambda(const LambdaExpression& le, size_t& currentNumber)
{
    if (le.body.empty()) {
        return std::nullopt;
    }

    std::shared_ptr<ANF> body = nullptr;
    size_t index = 0;
    for (const auto& expr : std::ranges::reverse_view(le.body)) {
        // Only the last expression in the body is in tail position
        bool isTail = (index == 0);
        auto transformed = transform(expr, currentNumber, isTail);
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
        index++;
    }

    if (!body) {
        return std::nullopt;
    }

    // Extract Token from HygienicSyntax
    std::vector<Token> paramTokens;
    paramTokens.reserve(le.parameters.size());
    for (const auto& param : le.parameters) {
        paramTokens.push_back(param.token);
    }

    const auto lambda = std::make_shared<ANF>(Lambda { paramTokens, body });
    Token lambdaTemp = { Tokentype::IDENTIFIER, std::format("temp{}", currentNumber++), 0, 0 };
    return std::make_shared<ANF>(Let {
        std::optional<Token>(lambdaTemp),
        lambda,
        std::make_shared<ANF>(Atom { lambdaTemp }) });
}

std::shared_ptr<ANF> AVector(const VectorExpression& ve, size_t& currentNumber, bool inTailPosition)
{
    std::vector<Token> final_args;
    std::vector<std::pair<Token, std::shared_ptr<ANF>>> bindings;

    for (const auto& elem : ve.elements) {
        // Vector elements are never in tail position
        auto transformed = transform(elem, currentNumber, false);
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
        inTailPosition });

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

std::optional<std::shared_ptr<ANF>> transform(const std::shared_ptr<Expression>& toTransform, size_t& currentNumber, bool inTailPosition)
{
    if (!toTransform) {
        return std::nullopt;
    }
    return std::visit(overloaded {
                          [&](const AtomExpression& e) -> std::optional<std::shared_ptr<ANF>> { return Aatom(e); },
                          [&](const sExpression& e) -> std::optional<std::shared_ptr<ANF>> { return ASExpr(e, currentNumber, inTailPosition); },
                          [&](const DefineExpression& e) -> std::optional<std::shared_ptr<ANF>> { return ADefine(e, currentNumber, inTailPosition); },
                          [&](const DefineProcedure& e) -> std::optional<std::shared_ptr<ANF>> { return ADefineProcedure(e, currentNumber); },
                          [&](const LambdaExpression& e) -> std::optional<std::shared_ptr<ANF>> { return ALambda(e, currentNumber); },
                          [&](const IfExpression& e) -> std::optional<std::shared_ptr<ANF>> { return AIf(e, currentNumber, inTailPosition); },
                          [&](const QuoteExpression& e) -> std::optional<std::shared_ptr<ANF>> { return AQuote(e, currentNumber); },
                          [&](const VectorExpression& e) -> std::optional<std::shared_ptr<ANF>> { return AVector(e, currentNumber, inTailPosition); },
                          [&](const TailExpression& e) -> std::optional<std::shared_ptr<ANF>> { return transform(e.expression, currentNumber, inTailPosition); },
                          [&](const LetExpression& e) -> std::optional<std::shared_ptr<ANF>> { return ALet(e, currentNumber, inTailPosition); },
                          [&](const SetExpression& e) -> std::optional<std::shared_ptr<ANF>> { return ASet(e, currentNumber, inTailPosition); },
                          [&](const BeginExpression& e) -> std::optional<std::shared_ptr<ANF>> {
                              return ABegin(e, currentNumber, inTailPosition);
                          },
                          [&](const QuasiQuoteExpression& e) -> std::optional<std::shared_ptr<ANF>> {
                              return AQuasiQuote(e, currentNumber);
                          },
                          [&](const UnquoteExpression& e) -> std::optional<std::shared_ptr<ANF>> {
                              return AUnquote(e, currentNumber, inTailPosition);
                          },
                          [&](const SpliceExpression& e) -> std::optional<std::shared_ptr<ANF>> {
                              return ASplice(e, currentNumber, inTailPosition);
                          },
                          [&](const DefineSyntaxExpression& e) -> std::optional<std::shared_ptr<ANF>> {
                              return std::nullopt;
                          },
                          [&](const DefineLibraryExpression& e) -> std::optional<std::shared_ptr<ANF>> {
                              return std::nullopt;
                          },
                          [&](const ImportExpression& e) -> std::optional<std::shared_ptr<ANF>> {
                              return std::nullopt;
                          },
                          [&](const SyntaxRulesExpression& e) -> std::optional<std::shared_ptr<ANF>> {
                              return std::nullopt;
                          },
                          [&](const auto& e) -> std::optional<std::shared_ptr<ANF>> {
                              throw std::runtime_error("Unknown or unhandled expression type encountered during ANF transformation.");
                          } },
        toTransform->as);
}

std::optional<std::shared_ptr<TopLevel>> transformTop(const std::shared_ptr<Expression>& toTransform, size_t& currentNumber)
{
    if (!toTransform) {
        return std::nullopt;
    }
    return std::visit(overloaded {
                          [&](const DefineExpression& e) -> std::optional<std::shared_ptr<TopLevel>> {
                              auto anf_value_opt = ADefine(e, currentNumber, false);
                              if (!anf_value_opt) {
                                  std::cerr << "Warning: Failed to transform definition value for " << e.name.token.lexeme << std::endl;
                                  return std::nullopt;
                              }
                              return std::make_shared<TopLevel>(TDefine { e.name.token, flattenLets(anf_value_opt) });
                          },
                          [&](const DefineProcedure& e) -> std::optional<std::shared_ptr<TopLevel>> {
                              auto anf_proc_opt = ADefineProcedure(e, currentNumber);
                              if (!anf_proc_opt) {
                                  std::cerr << "Warning: Failed to transform procedure definition for " << e.name.token.lexeme << std::endl;
                                  return std::nullopt;
                              }
                              return std::make_shared<TopLevel>(TDefine { e.name.token, flattenLets(anf_proc_opt) });
                          },
                          [&](const DefineSyntaxExpression& _) -> std::optional<std::shared_ptr<TopLevel>> { return std::nullopt; },
                          [&](const DefineLibraryExpression& _) -> std::optional<std::shared_ptr<TopLevel>> { return std::nullopt; },
                          [&](const ImportExpression& _) -> std::optional<std::shared_ptr<TopLevel>> { return std::nullopt; },

                          [&](const auto& _) -> std::optional<std::shared_ptr<TopLevel>> {
                              // Top-level expressions are never in tail position
                              auto transformed_expr_opt = transform(toTransform, currentNumber, false);
                              if (transformed_expr_opt && *transformed_expr_opt) {
                                  return std::make_shared<TopLevel>(flattenLets(*transformed_expr_opt));
                              } else {
                                  return std::nullopt;
                              }
                          } },
        toTransform->as);
}

std::vector<std::shared_ptr<TopLevel>> ANFtransform(const std::vector<std::shared_ptr<Expression>>& expressions)
{
    size_t currentNumber = 0;
    std::vector<std::shared_ptr<TopLevel>> output;

    for (const auto& expression : expressions) {
        if (!expression)
            continue;

        auto transformed_opt = transformTop(expression, currentNumber);

        if (transformed_opt) {
            if (*transformed_opt) {
                output.push_back(*transformed_opt);
            } else {
                std::cerr << "Warning: transformTop resulted in an engaged optional containing nullptr." << std::endl;
            }
        }
    }

    return output;
}

}
