#pragma once
#include "Expression.h"
#include "ExpressionUtils.h"
#include <iostream>
#include <memory>
#include <unordered_map>
#include <variant>
#include <vector>

namespace pattern {
class MacroEnvironment {
public:
    void defineMacro(const std::string& name, std::shared_ptr<Expression> expr)
    {
        macros[name] = expr;
    }

    bool isMacro(const std::string& name) const
    {
        return macros.find(name) != macros.end();
    }

    std::optional<std::shared_ptr<Expression>> getMacroDefinition(const std::string& name) const
    {
        auto it = macros.find(name);
        if (it != macros.end()) {
            return it->second;
        }
        return std::nullopt;
    }

private:
    std::unordered_map<std::string, std::shared_ptr<Expression>> macros;
};
}

namespace macroexp {

// Forward declarations
class MacroExpression;

// Define MacroAtom first to avoid incomplete type error
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

using MatchEnv = std::unordered_map<std::string, PatternMatches>;

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
void findVariadics(const MacroList& list, std::vector<std::string>& variadicVars, size_t& variadicCount, const MatchEnv& env);
void printMatchResult(const std::pair<MatchEnv, bool>& result);
std::pair<MatchEnv, bool> tryMatch(std::shared_ptr<MacroExpression> pattern, std::shared_ptr<MacroExpression> expr, const std::vector<Token>& literals, const std::string& macroName);
std::shared_ptr<MacroExpression> transformTemplate(const std::shared_ptr<MacroExpression>& template_expr, const MatchEnv& env, SyntaxContext macroContext = SyntaxContext {});
std::shared_ptr<MacroExpression> transformMacroRecursive(
    const std::shared_ptr<MacroExpression>& expr,
    std::shared_ptr<pattern::MacroEnvironment> env);
std::shared_ptr<MacroExpression> transformMacro(
    const std::shared_ptr<Expression>& template_expr,
    const MatchEnv& env,
    const std::string& macroName,
    std::shared_ptr<pattern::MacroEnvironment> macroEnv // Takes shared_ptr
);
std::shared_ptr<Expression> convertMacroResultToExpressionInternal(const std::shared_ptr<MacroExpression>& macroResult);
std::shared_ptr<Expression> convertMacroResultToExpression(const std::shared_ptr<MacroExpression>& macroResult);
void wrapLastBodyExpression(std::vector<std::shared_ptr<Expression>>& body);
std::vector<std::shared_ptr<Expression>> expandMacros(
    const std::vector<std::shared_ptr<Expression>>& exprs,
    std::shared_ptr<pattern::MacroEnvironment> env);

std::string getKeyword(const MacroList& ml);
std::shared_ptr<Expression> convertQuote(const MacroList& ml, int line);
std::shared_ptr<Expression> convertSet(const MacroList& ml, int line);
std::shared_ptr<Expression> convertIf(const MacroList& ml, int line);
std::shared_ptr<Expression> convertLambda(const MacroList& ml, int line);
std::shared_ptr<Expression> convertDefine(const MacroList& ml, int line);
std::shared_ptr<Expression> convertLet(const MacroList& ml, int line);

std::pair<std::vector<HygienicSyntax>, bool> parseMacroParameters(const std::shared_ptr<MacroExpression>& paramsMacroExpr);
HygienicSyntax createFreshSyntaxObject(const Token& token);
}
