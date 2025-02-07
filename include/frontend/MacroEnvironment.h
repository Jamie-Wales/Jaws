#pragma once
#include "Expression.h"
#include "Token.h"
#include <memory>
#include <optional>
#include <string>
#include <unordered_map>
#include <vector>

namespace pattern {

class MacroEnvironment {
private:
    std::unordered_map<std::string, std::shared_ptr<Expression>> macros;

public:
    /**
     * @brief Define a new macro or update an existing one
     * @param name The name of the macro
     * @param macro The macro expression (usually a SyntaxRulesExpression)
     */
    void defineMacro(const std::string& name, std::shared_ptr<Expression> macro);

    /**
     * @brief Check if a macro with the given name exists
     * @param name The name to check
     * @return true if the macro exists, false otherwise
     */
    bool isMacro(const std::string& name) const;

    /**
     * @brief Get the definition of a macro
     * @param name The name of the macro
     * @return The macro expression if it exists, nullopt otherwise
     */
    std::optional<std::shared_ptr<Expression>> getMacroDefinition(const std::string& name) const;
};
namespace {
    const SyntaxRulesExpression* getSyntaxRules(const std::shared_ptr<Expression>& expr)
    {
        if (!expr || expr->type() != ExprType::SyntaxRules) {
            return nullptr;
        }
        return &std::get<SyntaxRulesExpression>(expr->as);
    }
}
} // namespace pattern
