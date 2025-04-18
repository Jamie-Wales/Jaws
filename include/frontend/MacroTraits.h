#pragma once
#include "Expression.h"
#include "ExpressionUtils.h"
#include "MacroEnvironment.h"
#include <iostream>
#include <memory>
#include <unordered_map>
#include <variant>
#include <vector>

namespace macroexp {

class MacroExpression;
class MacroAtom {
public:
    HygienicSyntax syntax;

    MacroAtom(Token t)
        : syntax { t, SyntaxContext {} }
    {
    }

    MacroAtom(HygienicSyntax s)
        : syntax(std::move(s))
    {
    }
};

class MacroList {
public:
    std::vector<std::shared_ptr<MacroExpression>> elements;
};

class PatternMatches {
public:
    std::vector<std::shared_ptr<MacroExpression>> matches;
};

class MatchEnv {
private:
    struct Scope {
        std::map<std::string, PatternMatches> bindings;
        std::shared_ptr<Scope> parent;
    };

    std::shared_ptr<Scope> currentScope;

public:
    MatchEnv()
        : currentScope(std::make_shared<Scope>())
    {
    }

    void pushScope()
    {
        auto newScope = std::make_shared<Scope>();
        newScope->parent = currentScope;
        currentScope = newScope;
    }

    void popScope()
    {
        if (currentScope->parent) {
            currentScope = currentScope->parent;
        }
    }

    void bind(const std::string& name, const PatternMatches& matches)
    {
        currentScope->bindings[name] = matches;
    }

    void addMatch(const std::string& name, std::shared_ptr<MacroExpression> expr)
    {
        auto scope = findScopeWithBinding(name);
        if (scope) {
            scope->bindings[name].matches.push_back(expr);
        } else {
            currentScope->bindings[name].matches.push_back(expr);
        }
    }

    std::optional<PatternMatches> lookup(const std::string& name) const
    {
        auto scope = findScopeWithBinding(name);
        if (scope) {
            return scope->bindings.at(name);
        }
        return std::nullopt;
    }

    bool isBound(const std::string& name) const
    {
        return findScopeWithBinding(name) != nullptr;
    }

    std::map<std::string, PatternMatches> getCurrentScopeBindings() const
    {
        return currentScope->bindings;
    }

    std::map<std::string, PatternMatches> getAllBindings() const
    {
        std::map<std::string, PatternMatches> result;
        auto scope = currentScope;
        while (scope) {
            for (const auto& [name, matches] : scope->bindings) {
                if (!result.contains(name)) {
                    result[name] = matches;
                }
            }
            scope = scope->parent;
        }
        return result;
    }

    void merge(const MatchEnv& other)
    {
        for (const auto& [name, matches] : other.getCurrentScopeBindings()) {
            if (!currentScope->bindings.contains(name)) {
                currentScope->bindings[name] = matches;
            } else {
                auto& existing = currentScope->bindings[name];
                existing.matches.insert(
                    existing.matches.end(),
                    matches.matches.begin(),
                    matches.matches.end());
            }
        }
    }

private:
    std::shared_ptr<Scope> findScopeWithBinding(const std::string& name) const
    {
        auto scope = currentScope;
        while (scope) {
            if (scope->bindings.contains(name)) {
                return scope;
            }
            scope = scope->parent;
        }
        return nullptr;
    }
};

class MacroExpression {
public:
    using MacroExpressionValue = std::variant<MacroAtom, MacroList>;
    MacroExpressionValue value;
    bool isVariadic;
    int line;

    MacroExpression(const MacroAtom& atom, bool variadic, int lineNum);
    MacroExpression(const MacroList& list, bool variadic, int lineNum);

    std::string toString();
    void print();
};

std::shared_ptr<Expression> exprToList(const std::shared_ptr<Expression>& expr);
std::shared_ptr<MacroExpression> fromExpr(const std::shared_ptr<Expression>& expr);
bool isPatternVariable(const Token& t, const std::vector<Token>& literals);
bool isEllipsis(std::shared_ptr<MacroExpression> me);
std::shared_ptr<MacroExpression> createNonVariadicCopy(const std::shared_ptr<MacroExpression>& expr);
void findVariadics(const MacroList& list, std::vector<std::string>& variadicVars, size_t& variadicCount, const MatchEnv& env);
void printMatchResult(const std::pair<MatchEnv, bool>& result);

// Pattern matching
std::pair<MatchEnv, bool> tryMatch(
    std::shared_ptr<MacroExpression> pattern,
    std::shared_ptr<MacroExpression> expr,
    const std::vector<Token>& literals,
    const std::string& macroName);

// Template transformation with PatternVariableInfo
std::shared_ptr<MacroExpression> transformTemplate(
    const std::shared_ptr<MacroExpression>& template_expr,
    MatchEnv& env,
    SyntaxContext macroContext,
    const PatternVariableInfo& pattern_info = PatternVariableInfo());

std::shared_ptr<MacroExpression> expandParallel(
    const std::shared_ptr<MacroExpression>& template_part,
    MatchEnv& pattern_env,
    size_t iteration,
    SyntaxContext macroContext,
    const std::vector<std::string>& pattern_vars_in_group,
    const std::vector<std::string>& template_vars_in_group,
    const PatternVariableInfo& pattern_info = PatternVariableInfo());

std::shared_ptr<MacroExpression> transformMacro(
    const std::shared_ptr<Expression>& template_expr,
    MatchEnv& env,
    const std::string& macroName,
    std::shared_ptr<pattern::MacroEnvironment> macroEnv,
    const PatternVariableInfo& pattern_info = PatternVariableInfo());

std::shared_ptr<MacroExpression> transformMacroRecursive(
    const std::shared_ptr<MacroExpression>& expr,
    std::shared_ptr<pattern::MacroEnvironment> env);

std::shared_ptr<MacroExpression> addUsageContextRecursive(
    const std::shared_ptr<MacroExpression>& expr,
    const SyntaxContext& context);

HygienicSyntax createFreshSyntaxObject(const Token& token);
HygienicSyntax createFreshSyntaxObject(const Token& token, SyntaxContext context);

// Conversion functions
std::shared_ptr<Expression> convertMacroResultToExpressionInternal(const std::shared_ptr<MacroExpression>& macroResult);
std::shared_ptr<Expression> convertMacroResultToExpression(const std::shared_ptr<MacroExpression>& macroResult);
void wrapLastBodyExpression(std::vector<std::shared_ptr<Expression>>& body);
std::vector<std::shared_ptr<Expression>> expandMacros(
    const std::vector<std::shared_ptr<Expression>>& exprs,
    std::shared_ptr<pattern::MacroEnvironment> env);

// Specific conversion functions
std::string getKeyword(const MacroList& ml);
std::shared_ptr<Expression> convertBegin(const MacroList& ml, int line);
std::shared_ptr<Expression> convertSplice(const MacroList& ml, int line);
std::shared_ptr<Expression> convertQuasiQuote(const MacroList& ml, int line);
std::shared_ptr<Expression> convertUnquote(const MacroList& ml, int line);
std::shared_ptr<Expression> convertQuote(const MacroList& ml, int line);
std::shared_ptr<Expression> convertSet(const MacroList& ml, int line);
std::shared_ptr<Expression> convertIf(const MacroList& ml, int line);
std::shared_ptr<Expression> convertLambda(const MacroList& ml, int line);
std::shared_ptr<Expression> convertDefine(const MacroList& ml, int line);
std::shared_ptr<Expression> convertLet(const MacroList& ml, int line);
std::pair<std::vector<HygienicSyntax>, bool> parseMacroParameters(const std::shared_ptr<MacroExpression>& paramsMacroExpr);

// Helper functions
void findPatternVariables(
    const std::shared_ptr<MacroExpression>& pattern,
    std::vector<std::string>& vars,
    const std::vector<Token>& literals);

bool containsRepeatingVariable(
    const std::shared_ptr<MacroExpression>& expr,
    const std::set<std::string>& repeating_pattern_vars);
}
