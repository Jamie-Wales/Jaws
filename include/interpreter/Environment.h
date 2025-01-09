#pragma once
#include "Value.h"
#include <deque>
#include <memory>
#include <optional>
#include <string>
#include <unordered_map>

struct Frame {
    std::unordered_map<std::string, SchemeValue> bound;
};

class Environment {
public:
    std::shared_ptr<Environment> enclosing;
    std::deque<std::shared_ptr<Frame>> frames;
    struct MacroDefinition {
        std::vector<Token> literals;
        std::vector<SyntaxRule> rules;
    };

    static std::unordered_map<std::string, MacroDefinition> macroBindings;
    Environment();
    Environment(std::shared_ptr<Environment> parent);

    void defineMacro(const std::string& name, const std::vector<Token>& literals,
        const std::vector<SyntaxRule>& rules);
    std::optional<MacroDefinition> getMacroDefinition(const std::string& name) const;
    bool isMacro(const std::string& name) const;

    void pushFrame();
    void popFrame();
    void define(const std::string& name, const SchemeValue& value);
    void set(const std::string& name, const SchemeValue& value);
    std::optional<SchemeValue> get(const std::string& name) const;
};
