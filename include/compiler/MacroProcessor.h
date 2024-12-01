#pragma once
#include "Expression.h"
#include "PatternMatching.h"
#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

class MacroProcessor {
private:
    struct MacroDefinition {
        std::vector<Token> literals;
        std::vector<std::shared_ptr<Expression>> patterns;
        std::vector<std::shared_ptr<Expression>> templates;
    };
    std::unordered_map<std::string, MacroDefinition> macros;
    PatternMatcher matcher;
    PatternSubstitutor substitutor;

public:
    void collectMacros(std::shared_ptr<Expression> expr);
    std::shared_ptr<Expression> expandMacros(std::shared_ptr<Expression> expr);
    void printMacroTable() const;
};
