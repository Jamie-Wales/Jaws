#pragma once
#include "Token.h"
#include <memory>
#include <string>
#include <variant>
#include <vector>

class Expression;

/**
 * @brief Represents atomic expressions like numbers, identifiers, and literals
 */
class AtomExpression {
public:
    Token value;
    AtomExpression(Token token);
    void toString(std::stringstream& ss) const;
};

class SetExpression {
public:
    Token identifier;
    std::shared_ptr<Expression> value;
    SetExpression(const Token& token, std::shared_ptr<Expression>);
    void toString(std::stringstream& ss) const;
};
/**
 * @brief Represents let expressions for local variable bindings
 */
class LetExpression {
public:
    std::optional<Token> name;
    using Args = std::vector<std::pair<Token, std::shared_ptr<Expression>>>;
    Args arguments;
    std::vector<std::shared_ptr<Expression>> body;
    LetExpression(std::optional<Token> name, Args arguments, std::vector<std::shared_ptr<Expression>> body);
};

class BeginExpression {
public:
    std::vector<std::shared_ptr<Expression>> body;
    BeginExpression(std::vector<std::shared_ptr<Expression>> body);
};

class SyntaxRule {
public:
    std::shared_ptr<Expression> pattern;
    std::shared_ptr<Expression> template_expr;
    SyntaxRule(std::shared_ptr<Expression> pattern, std::shared_ptr<Expression> template_expr);
};

/**
 * @brief Represents syntax-rules expressions for macro pattern matching
 */
class SyntaxRulesExpression {
public:
    std::vector<Token> literals;
    std::vector<SyntaxRule> rules;
    SyntaxRulesExpression(std::vector<Token> literals, std::vector<SyntaxRule> rules);
};

/**
 * @brief Represents macro definitions using define-syntax
 */
class DefineSyntaxExpression {
public:
    Token name;
    std::shared_ptr<Expression> rule;
    DefineSyntaxExpression(Token name, std::shared_ptr<Expression> rule);
};

/**
 * @brief Represents tail expressions for proper tail recursion
 */
class TailExpression {
public:
    std::shared_ptr<Expression> expression;
    TailExpression(std::shared_ptr<Expression> expression);
};

/**
 * @brief Represents import expressions for loading external modules
 */

class ImportExpression {
public:
    struct ImportSet {
        enum class Type {
            DIRECT,
            ONLY,
            EXCEPT,
            PREFIX,
            RENAME
        };
    };

    struct ImportSpec {
        ImportSet::Type type;
        std::vector<std::shared_ptr<Expression>> library;
        std::vector<Token> identifiers;
        Token prefix;
        std::vector<std::pair<Token, Token>> renames; // Used for RENAME

        explicit ImportSpec(std::vector<std::shared_ptr<Expression>> lib);
        ImportSpec(ImportSet::Type t, std::vector<std::shared_ptr<Expression>> lib,
            std::vector<Token> ids);
        ImportSpec(std::vector<std::shared_ptr<Expression>> lib, Token pfx);
        ImportSpec(std::vector<std::shared_ptr<Expression>> lib,
            std::vector<std::pair<Token, Token>> renames);
        ImportSpec(const ImportSpec& other);
    };

    std::vector<ImportSpec> imports;

    explicit ImportExpression(std::vector<ImportSpec> imports_);
};

ImportExpression::ImportSpec makeDirectImport(std::vector<std::shared_ptr<Expression>> library);
ImportExpression::ImportSpec makeOnlyImport(std::vector<std::shared_ptr<Expression>> library,
    std::vector<Token> identifiers);
ImportExpression::ImportSpec makeExceptImport(std::vector<std::shared_ptr<Expression>> library,
    std::vector<Token> identifiers);
ImportExpression::ImportSpec makePrefixImport(std::vector<std::shared_ptr<Expression>> library,
    Token prefix);
ImportExpression::ImportSpec makeRenameImport(std::vector<std::shared_ptr<Expression>> library,
    std::vector<std::pair<Token, Token>> renames);

/**
 * @brief Represents list expressions (proper lists)
 */
class ListExpression {
public:
    std::vector<std::shared_ptr<Expression>> elements;
    bool isVariadic;
    ListExpression(std::vector<std::shared_ptr<Expression>> elems, bool variadic = false);
};

/**
 * @brief Represents lambda expressions (anonymous functions)
 */
class LambdaExpression {
public:
    std::vector<Token> parameters;
    std::vector<std::shared_ptr<Expression>> body;
    bool isVariadic;
    LambdaExpression(std::vector<Token> parameters, std::vector<std::shared_ptr<Expression>> body, bool isVariadic = false);
};

/**
 * @brief Represents s-expressions (function applications)
 */
class sExpression {
public:
    std::vector<std::shared_ptr<Expression>> elements;
    sExpression(std::vector<std::shared_ptr<Expression>> elems);
};

/**
 * @brief Represents variable definitions
 */
class DefineExpression {
public:
    Token name;
    std::shared_ptr<Expression> value;
    DefineExpression(Token n, std::shared_ptr<Expression> v);
};

/**
 * @brief Represents procedure definitions
 */
class DefineProcedure {
public:
    Token name;
    std::vector<Token> parameters;
    std::vector<std::shared_ptr<Expression>> body;
    bool isVariadic;
    DefineProcedure(Token name, std::vector<Token> parameters, std::vector<std::shared_ptr<Expression>> body, bool isVariadic = false);
};

/**
 * @brief Represents quoted expressions
 */
class QuoteExpression {
public:
    std::shared_ptr<Expression> expression;
    QuoteExpression(std::shared_ptr<Expression> expression);
};

/**
 * @brief Represents vector expressions
 */
class VectorExpression {
public:
    std::vector<std::shared_ptr<Expression>> elements;
    VectorExpression(std::vector<std::shared_ptr<Expression>> elems);
};

/**
 * @brief Represents conditional expressions
 */
class IfExpression {
public:
    std::shared_ptr<Expression> condition;
    std::shared_ptr<Expression> then;
    std::optional<std::shared_ptr<Expression>> el;
    IfExpression(std::shared_ptr<Expression> condition, std::shared_ptr<Expression> then,
        std::optional<std::shared_ptr<Expression>> el);
};

/**
 * @brief Base class representing any Scheme expression
 *
 * Uses std::variant to implement a discriminated union of all possible expression types.
 * Provides methods for converting expressions to strings and creating deep copies.
 */
class Expression {
public:
    using ExpressionVariant = std::variant<
        AtomExpression,
        sExpression,
        ListExpression,
        DefineExpression,
        DefineProcedure,
        VectorExpression,
        LambdaExpression,
        IfExpression,
        QuoteExpression,
        SetExpression,
        TailExpression,
        ImportExpression,
        SyntaxRulesExpression,
        DefineSyntaxExpression,
        BeginExpression,
        LetExpression>;

    ExpressionVariant as;
    int line;

    /**
     * @brief Constructs an Expression from a variant and line number
     * @param as The specific expression type
     * @param line Source code line number
     */
    Expression(ExpressionVariant as, int line);

    /**
     * @brief Converts the expression to its string representation
     * @param ss Output string stream to write to
     */
    void toString(std::stringstream& ss) const;

    /**
     * @brief Creates a deep copy of the expression
     * @return Shared pointer to the cloned expression
     */
    std::shared_ptr<Expression> clone() const;

    /**
     * @brief Prints the expression with proper indentation
     * @param indent Number of spaces to indent
     */
    void print(int indent = 0) const;

    /**
     * @brief Converts the expression to its string representation
     * @return String representation of the expression
     */
    std::string toString() const;
};