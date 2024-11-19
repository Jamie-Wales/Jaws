#pragma once
#include "Expression.h"
#include "Value.h"
#include <memory>
#include <optional>
#include <string>
#include <unordered_map>
#include <vector>

class Interpreter;

class MacroExpander {
public:
    explicit MacroExpander(Interpreter& interp)
        : interpreter(interp)
    {
    }

    bool isMacro(const std::string& name) const
    {
        return macros.contains(name);
    }

    void defineMacro(const std::string& name, std::shared_ptr<Expression> rule)
    {
        macros[name] = std::move(rule);
    }

    std::optional<std::shared_ptr<Expression>> expand(
        const std::string& name,
        const std::vector<std::shared_ptr<Expression>>& args);

private:
    Interpreter& interpreter;
    std::unordered_map<std::string, std::shared_ptr<Expression>> macros;

    bool matchPattern(
        const SchemeValue& pattern,
        const SchemeValue& expr,
        std::unordered_map<std::string, SchemeValue>& bindings);

    SchemeValue expandTemplate(
        const SchemeValue& templ,
        const std::unordered_map<std::string, SchemeValue>& bindings);
};
