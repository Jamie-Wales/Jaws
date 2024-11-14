#pragma once
#include "Expression.h"
#include <unordered_map>

class Interpreter;

class MacroExpander {
    std::unordered_map<std::string, std::shared_ptr<Expression>> macros;
    Interpreter& interpreter;

public:
    explicit MacroExpander(Interpreter& interp)
        : interpreter(interp)
    {
    }

    void defineMacro(const std::string& name, std::shared_ptr<Expression> rules)
    {
        macros[name] = rules;
    }

    bool isMacro(const std::string& name) const
    {
        return macros.find(name) != macros.end();
    }

    std::optional<std::shared_ptr<Expression>> expand(
        const std::string& name,
        const std::vector<std::shared_ptr<Expression>>& args);

private:
    bool matchPattern(
        const SyntaxPattern& pattern,
        std::shared_ptr<Expression> expr,
        std::unordered_map<std::string, std::shared_ptr<Expression>>& bindings);

    std::shared_ptr<Expression> expandTemplate(
        std::shared_ptr<Expression> templ,
        const std::unordered_map<std::string, std::shared_ptr<Expression>>& bindings);
};
