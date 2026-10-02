#pragma once
#include "Expression.h"
#include "ExpressionUtils.h"
#include "MacroEnvironment.h"
#include <map>
#include <memory>
#include <optional>
#include <stdexcept>
#include <string>
#include <variant>
#include <vector>

namespace macroexp {

class MacroExpression;
using MacroExprPtr = std::shared_ptr<MacroExpression>;

class MacroError : public std::runtime_error {
public:
    MacroError(const std::string& message, const std::string& macroName, int line);

    const std::string macroName;
    const int line;
};

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
    std::vector<MacroExprPtr> elements;
};

class MacroExpression {
public:
    using MacroExpressionValue = std::variant<MacroAtom, MacroList>;
    MacroExpressionValue value;
    bool isVariadic;
    int line;

    MacroExpression(const MacroAtom& atom, bool variadic, int lineNum);
    MacroExpression(const MacroList& list, bool variadic, int lineNum);

    std::string toString() const;
    void print() const;
};

// What a pattern variable matched, nested once per enclosing ellipsis in the
// pattern: a depth-0 variable is a leaf, a depth-n variable is n layers of Seq.
class MatchTree {
public:
    using Seq = std::vector<MatchTree>;
    std::variant<MacroExprPtr, Seq> node;

    static MatchTree leafOf(MacroExprPtr expr) { return MatchTree { std::move(expr) }; }
    static MatchTree seqOf(Seq seq) { return MatchTree { std::move(seq) }; }

    bool isLeaf() const { return std::holds_alternative<MacroExprPtr>(node); }
    const MacroExprPtr& leaf() const { return std::get<MacroExprPtr>(node); }
    const Seq& seq() const { return std::get<Seq>(node); }
};

// Pattern variable bindings produced by a successful match.
class MatchEnv {
public:
    void bind(const std::string& name, MatchTree tree) { bindings[name] = std::move(tree); }

    const MatchTree* lookup(const std::string& name) const
    {
        auto it = bindings.find(name);
        return it == bindings.end() ? nullptr : &it->second;
    }

private:
    std::map<std::string, MatchTree> bindings;
};

// A MatchEnv with some variables narrowed to a single ellipsis iteration.
// Template expansion passes views so matched forms are never copied.
class MatchEnvView {
public:
    explicit MatchEnvView(const MatchEnv& env)
        : base(&env)
    {
    }

    const MatchTree* lookup(const std::string& name) const;
    MatchEnvView child(const std::vector<std::string>& drivers, size_t iteration) const;

private:
    const MatchEnv* base;
    std::map<std::string, const MatchTree*> narrowed;
};

MacroExprPtr fromExpr(const std::shared_ptr<Expression>& expr);
bool isPatternVariable(const Token& t, const std::vector<Token>& literals);
bool isEllipsis(const MacroExprPtr& me);
MacroExprPtr createNonVariadicCopy(const MacroExprPtr& expr);

void findPatternVariables(
    const MacroExprPtr& pattern,
    std::vector<std::string>& vars,
    const std::vector<Token>& literals);

// Pattern matching
std::optional<MatchEnv> matchPattern(
    const MacroExprPtr& pattern,
    const MacroExprPtr& form,
    const std::vector<Token>& literals);

// Template instantiation. Pattern variables are replaced by what they matched,
// untouched; every other identifier is template-introduced and gets macroScope.
MacroExprPtr expandTemplate(
    const MacroExprPtr& templateExpr,
    const MatchEnvView& env,
    const PatternVariableInfo& patternInfo,
    const SyntaxContext& macroScope,
    int depth = 0);

// Expands `node` until it is no longer a macro call, then each of its children.
MacroExprPtr expandFully(
    const MacroExprPtr& node,
    const std::shared_ptr<pattern::MacroEnvironment>& env,
    int depth = 0);

HygienicSyntax createFreshSyntaxObject(const Token& token, SyntaxContext context);

// Conversion functions
std::shared_ptr<Expression> convertMacroResultToExpressionInternal(const MacroExprPtr& macroResult);
std::shared_ptr<Expression> convertMacroResultToExpression(const MacroExprPtr& macroResult);
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
std::shared_ptr<Expression> convertVector(const MacroList& ml, int line);
std::shared_ptr<Expression> convertLet(const MacroList& ml, int line);
std::pair<std::vector<HygienicSyntax>, bool> parseMacroParameters(const MacroExprPtr& paramsMacroExpr);
}
