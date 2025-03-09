#pragma once
#include "Expression.h"
#include "ExpressionUtils.h"
#include "MacroEnvironment.h"
#include <Visit.h>
#include <iostream>
#include <unordered_map>
#include <variant>
#include <vector>

namespace macroexp {
class MacroExpression;

struct MacroAtom {
    Token token;
    explicit MacroAtom(Token t);
};

struct MacroList {
    std::vector<std::shared_ptr<MacroExpression>> elements;
};

class MacroExpression {
public:
    std::variant<MacroAtom, MacroList> value;
    bool isVariadic;
    int line;

    MacroExpression(const MacroAtom& atom, bool variadic, int lineNum);
    MacroExpression(const MacroList& list, bool variadic, int lineNum);
    MacroExpression(const MacroExpression& other) = default;

    std::string toString();
    void print();
};

struct PatternMatch {
    std::vector<std::shared_ptr<MacroExpression>> matches;
};

using MatchEnv = std::unordered_map<std::string, PatternMatch>;

bool isEllipsis(std::shared_ptr<MacroExpression> me);
bool isPatternVariable(const Token& token, const std::vector<Token>& literals);

std::shared_ptr<MacroExpression> fromExpr(const std::shared_ptr<Expression>& expr);
std::pair<MatchEnv, bool> tryMatch(std::shared_ptr<MacroExpression> pattern,
    std::shared_ptr<MacroExpression> expr,
    const std::vector<Token>& literals,
    const std::string& macroName);

void printMatchEnv(const MatchEnv& env, int indent = 0);
void printMatchResult(const std::pair<MatchEnv, bool>& result);

std::shared_ptr<MacroExpression> transformTemplate(const std::shared_ptr<MacroExpression>& template_expr,
    const MatchEnv& env);

std::shared_ptr<MacroExpression> transformMacroRecursive(const std::shared_ptr<MacroExpression>& expr,
    pattern::MacroEnvironment& env);

std::shared_ptr<MacroExpression> transformMacro(const std::shared_ptr<Expression>& template_expr,
    const MatchEnv& env,
    const std::string& macroName,
    pattern::MacroEnvironment& macroEnv);

std::vector<std::shared_ptr<Expression>> expandMacros(std::vector<std::shared_ptr<Expression>> exprs);
}
