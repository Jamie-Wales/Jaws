#include "ANF.h"
#include "Visit.h"
#include <optional>
#include <unordered_set>
#include <variant>

namespace optimise {

using NumericConstant = std::variant<int, double>;
using Constant = std::variant<NumericConstant, bool>;

bool isPureFunction(const Token& func)
{
    static const std::unordered_set<std::string> pureFuncs = {
        "+", "-", "*", "/", ">", "<", ">=", "<=", "=",
        "and", "or", "not"
    };
    return func.type == Tokentype::IDENTIFIER && pureFuncs.contains(func.lexeme);
}

std::optional<NumericConstant> getNumericValue(const Token& token)
{
    try {
        if (token.type == Tokentype::INTEGER) {
            return NumericConstant(std::stoi(token.lexeme));
        }
        if (token.type == Tokentype::FLOAT) {
            return NumericConstant(std::stod(token.lexeme));
        }
        return std::nullopt;
    } catch (...) {
        return std::nullopt;
    }
}

std::optional<bool> getBoolValue(const Token& token)
{
    if (token.type == Tokentype::TRUE)
        return true;
    if (token.type == Tokentype::FALSE)
        return false;
    return std::nullopt;
}

Token createNumberToken(const NumericConstant& value, const Token& original)
{
    return Token {
        std::holds_alternative<int>(value) ? Tokentype::INTEGER : Tokentype::FLOAT,
        std::visit([](const auto& v) { return std::to_string(v); }, value),
        original.line,
        original.column
    };
}

Token createBoolToken(bool value, const Token& original)
{
    return Token {
        value ? Tokentype::TRUE : Tokentype::FALSE,
        value ? "#t" : "#f",
        original.line,
        original.column
    };
}

std::shared_ptr<ir::ANF> createConstantNode(const Constant& value, const Token& original)
{
    if (std::holds_alternative<NumericConstant>(value)) {
        return std::make_shared<ir::ANF>(ir::Atom {
            createNumberToken(std::get<NumericConstant>(value), original) });
    } else {
        return std::make_shared<ir::ANF>(ir::Atom {
            createBoolToken(std::get<bool>(value), original) });
    }
}

static std::optional<NumericConstant> foldNums(
    const std::vector<NumericConstant>& nums,
    const auto& op,
    const NumericConstant& init)
{
    auto result = init;
    for (const auto& num : nums) {
        result = std::visit([op](const auto& x, const auto& y) -> NumericConstant {
            if constexpr (std::is_same_v<std::decay_t<decltype(x)>, double> || std::is_same_v<std::decay_t<decltype(y)>, double>) {
                return op(static_cast<double>(x), static_cast<double>(y));
            } else {
                return op(x, y);
            }
        },
            result, num);
    }
    return result;
}

static std::optional<Constant> evaluateConstantApp(
    const ir::App& app,
    const std::unordered_map<std::string, Constant>& env)
{
    std::vector<Constant> args;
    args.reserve(app.params.size());

    for (const auto& param : app.params) {
        if (param.type == Tokentype::IDENTIFIER && env.contains(param.lexeme)) {
            args.push_back(env.at(param.lexeme));
        } else if (auto num = getNumericValue(param)) {
            args.push_back(*num);
        } else if (auto b = getBoolValue(param)) {
            args.push_back(*b);
        } else {
            return std::nullopt;
        }
    }

    // Boolean operations
    if (app.name.lexeme == "and") {
        bool result = true;
        for (const auto& arg : args) {
            if (!std::holds_alternative<bool>(arg))
                return std::nullopt;
            result = result && std::get<bool>(arg);
        }
        return Constant(result);
    }

    if (app.name.lexeme == "or") {
        bool result = false;
        for (const auto& arg : args) {
            if (!std::holds_alternative<bool>(arg))
                return std::nullopt;
            result = result || std::get<bool>(arg);
        }
        return Constant(result);
    }

    if (app.name.lexeme == "not") {
        if (args.size() != 1)
            return std::nullopt;
        if (!std::holds_alternative<bool>(args[0]))
            return std::nullopt;
        return Constant(!std::get<bool>(args[0]));
    }

    std::vector<NumericConstant> nums;
    for (const auto& arg : args) {
        if (!std::holds_alternative<NumericConstant>(arg)) {
            return std::nullopt;
        }
        nums.push_back(std::get<NumericConstant>(arg));
    }

    if (nums.empty()) {
        if (app.name.lexeme == "+")
            return Constant(NumericConstant(0));
        if (app.name.lexeme == "*")
            return Constant(NumericConstant(1));
        return std::nullopt;
    }

    if (nums.size() == 1) {
        if (app.name.lexeme == "-") {
            return Constant(std::visit([](const auto& x) -> NumericConstant {
                return -x;
            },
                nums[0]));
        }
        if (app.name.lexeme == "/") {
            return Constant(std::visit([](const auto& x) -> NumericConstant {
                return NumericConstant(1.0 / static_cast<double>(x));
            },
                nums[0]));
        }
    }

    if (app.name.lexeme == "+") {
        if (auto result = foldNums(nums, std::plus {}, NumericConstant(0))) {
            return Constant(*result);
        }
        return std::nullopt;
    }
    if (app.name.lexeme == "*") {
        if (auto result = foldNums(nums, std::multiplies {}, NumericConstant(1))) {
            return Constant(*result);
        }
        return std::nullopt;
    }
    if (app.name.lexeme == "-") {
        auto rest = std::vector<NumericConstant>(nums.begin() + 1, nums.end());
        if (auto result = foldNums(rest, std::minus {}, nums[0])) {
            return Constant(*result);
        }
        return std::nullopt;
    }
    if (app.name.lexeme == "/") {
        for (size_t i = 1; i < nums.size(); i++) {
            bool isZero = std::visit([](const auto& v) { return v == 0; }, nums[i]);
            if (isZero)
                return std::nullopt;
        }
        auto rest = std::vector<NumericConstant>(nums.begin() + 1, nums.end());
        if (auto result = foldNums(rest, [](auto x, auto y) { return static_cast<double>(x) / static_cast<double>(y); }, nums[0])) {
            return Constant(*result);
        }
        return std::nullopt;
    }

    // Comparisons (require at least 2 args)
    if (nums.size() < 2)
        return std::nullopt;

    auto compare = [](const auto& op) {
        return [op](const std::vector<NumericConstant>& nums) -> bool {
            for (size_t i = 0; i < nums.size() - 1; i++) {
                bool result = std::visit([op](const auto& x, const auto& y) -> bool {
                    if constexpr (std::is_same_v<std::decay_t<decltype(x)>, double> || std::is_same_v<std::decay_t<decltype(y)>, double>) {
                        return op(static_cast<double>(x), static_cast<double>(y));
                    } else {
                        return op(x, y);
                    }
                },
                    nums[i], nums[i + 1]);
                if (!result)
                    return false;
            }
            return true;
        };
    };

    if (app.name.lexeme == "<") {
        return Constant(compare(std::less {})(nums));
    }
    if (app.name.lexeme == ">") {
        return Constant(compare(std::greater {})(nums));
    }
    if (app.name.lexeme == "<=") {
        return Constant(compare(std::less_equal {})(nums));
    }
    if (app.name.lexeme == ">=") {
        return Constant(compare(std::greater_equal {})(nums));
    }
    if (app.name.lexeme == "=") {
        return Constant(compare(std::equal_to {})(nums));
    }

    return std::nullopt;
}

static std::optional<Constant> evaluateIfCondition(
    const std::shared_ptr<ir::ANF>& anf,
    const std::unordered_map<std::string, Constant>& env)
{
    if (!anf)
        return std::nullopt;

    return std::visit(overloaded {
                          [&](const ir::Atom& atom) -> std::optional<Constant> {
                              if (auto num = getNumericValue(atom.atom)) {
                                  return Constant(*num);
                              }
                              if (auto b = getBoolValue(atom.atom)) {
                                  return Constant(*b);
                              }
                              if (atom.atom.type == Tokentype::IDENTIFIER && env.contains(atom.atom.lexeme)) {
                                  return env.at(atom.atom.lexeme);
                              }
                              return std::nullopt;
                          },
                          [&](const ir::App& app) -> std::optional<Constant> {
                              if (isPureFunction(app.name)) {
                                  if (auto result = evaluateConstantApp(app, env)) {
                                      return result;
                                  }
                              }
                              if (app.name.type == Tokentype::IDENTIFIER && env.contains(app.name.lexeme)) {
                                  const auto& func_value = env.at(app.name.lexeme);
                                  return std::nullopt;
                              }

                              return std::nullopt;
                          },
                          [&](const auto&) -> std::optional<Constant> {
                              return std::nullopt;
                          } },
        anf->term);
}

std::shared_ptr<ir::ANF> constantFold(
    const std::shared_ptr<ir::ANF>& anf,
    std::unordered_map<std::string, Constant>& env)
{
    if (!anf)
        return nullptr;

    return std::visit(overloaded {
                          [&](const ir::Let& let) -> std::shared_ptr<ir::ANF> {
                              auto new_binding = constantFold(let.binding, env);

                              if (let.name) {
                                  if (auto const_val = evaluateIfCondition(new_binding, env)) {
                                      env[let.name->lexeme] = *const_val;
                                      if (let.body)
                                          return constantFold(let.body, env);
                                  }
                              }

                              if (let.body) {
                                  auto new_body = constantFold(let.body, env);
                                  return std::make_shared<ir::ANF>(ir::Let {
                                      let.name,
                                      new_binding,
                                      new_body });
                              }

                              return nullptr;
                          },
                          [&](const ir::App& app) -> std::shared_ptr<ir::ANF> {
                              if (isPureFunction(app.name)) {
                                  if (auto result = evaluateConstantApp(app, env)) {
                                      return createConstantNode(*result, app.name);
                                  }
                              }
                              if (app.name.type == Tokentype::IDENTIFIER && env.contains(app.name.lexeme)) {
                                  const auto& func_value = env.at(app.name.lexeme);
                              }
                              return std::make_shared<ir::ANF>(app);
                          },

                          [&](const ir::If& if_) -> std::shared_ptr<ir::ANF> {
                              if (auto cond_val = evaluateIfCondition(std::make_shared<ir::ANF>(ir::Atom { if_.cond }), env)) {
                                  bool is_true = std::visit(overloaded {
                                                                [](const NumericConstant& n) -> bool {
                                                                    return std::visit([](const auto& x) { return x != 0; }, n);
                                                                },
                                                                [](bool b) -> bool { return b; } },
                                      *cond_val);
                                  return constantFold(is_true ? if_.then : *if_._else, env);
                              }
                              auto new_then = constantFold(if_.then, env);
                              auto new_else = if_._else ? constantFold(*if_._else, env) : nullptr;
                              return std::make_shared<ir::ANF>(ir::If {
                                  if_.cond,
                                  new_then,
                                  new_else });
                          },

                          [&](const ir::Lambda& lambda) -> std::shared_ptr<ir::ANF> {
                              auto lambda_env = env;
                              auto new_body = constantFold(lambda.body, lambda_env);

                              return std::make_shared<ir::ANF>(ir::Lambda {
                                  lambda.params,
                                  new_body });
                          },

                          [&](const auto& x) -> std::shared_ptr<ir::ANF> {
                              return std::make_shared<ir::ANF>(x);
                          } },
        anf->term);
}

std::shared_ptr<ir::TDefine> constantFoldTDefine(
    const ir::TDefine& tdefine,
    std::unordered_map<std::string, Constant>& env)
{
    auto new_body = constantFold(tdefine.body, env);
    if (auto const_val = evaluateIfCondition(new_body, env)) {
        env[tdefine.name.lexeme] = *const_val;
    }

    return std::make_shared<ir::TDefine>(tdefine.name, new_body);
}

std::vector<std::shared_ptr<ir::TopLevel>> optimiseConstants(std::vector<std::shared_ptr<ir::TopLevel>>& topLevels)
{
    std::unordered_map<std::string, Constant> env;
    for (auto& tl : topLevels) {
        if (tl) {
            std::visit(overloaded {
                           [&](ir::TDefine& def) {
                               auto new_def = constantFoldTDefine(def, env);
                               tl = std::make_shared<ir::TopLevel>(ir::TopLevel::Declaration(*new_def));
                           },
                           [&](std::shared_ptr<ir::ANF>&) {
                           } },
                tl->decl);
        }
    }
    for (auto& tl : topLevels) {
        if (tl) {
            std::visit(overloaded {
                           [&](ir::TDefine& def) {
                           },
                           [&](std::shared_ptr<ir::ANF>& anf) {
                               anf = constantFold(anf, env);
                               tl = std::make_shared<ir::TopLevel>(ir::TopLevel::Declaration(anf));
                           } },
                tl->decl);
        }
    }

    return topLevels;
}

}
