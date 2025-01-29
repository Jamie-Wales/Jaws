#pragma once
#include "interpret.h"
#include <string>

namespace pattern {

class MacroExpander {
public:
    static std::optional<std::shared_ptr<Expression>> expandMacro(
        interpret::InterpreterState& state,
        const std::string& macroName,
        const sExpression& sexpr,
        const std::vector<Token>& literals);

    static std::optional<std::shared_ptr<Expression>> expandMacrosIn(
        interpret::InterpreterState& state,
        const std::shared_ptr<Expression>& expr);

private:
    template <typename T>
    static std::optional<std::shared_ptr<Expression>> expandExpr(
        interpret::InterpreterState& state,
        const T& expr,
        int line);
};

} // namespace pattern
