#pragma once
#include "Expression.h"
#include "Scanner.h"
#include "Token.h"
#include <memory>
#include <optional>
#include <vector>

/**
 * @brief Recursive descent parser for the Scheme programming language
 *
 * @details
 * The Parser class implements a recursive descent parser that converts a stream of tokens
 * into an Abstract Syntax Tree (AST). The parser processes a wide range of Scheme expressions including:
 *
 * Core Features:
 * - Atoms (numbers, identifiers, booleans)
 * - S-expressions (function calls)
 * - Special forms (if, lambda, define)
 * - Lists and vectors
 * - Quoted expressions
 *
 * Advanced Features:
 * - Macro system (define-syntax, syntax-rules)
 * - Let expressions
 * - Proper tail calls
 * - Import statements
 *
 * Example usage:
 * @code{.cpp}
 * auto scanner = std::make_shared<Scanner>();
 * auto parser = std::make_shared<Parser>();
 *
 * parser->initialize(scanner);
 * std::vector<Token> tokens = scanner->tokenize("(define x 42)");
 * parser->load(tokens);
 * auto ast = parser->parse();
 * @endcode
 *
 * Error Handling:
 * The parser implements error recovery using panic mode synchronization and provides
 * detailed error messages with source location information.
 */
class Parser {
private:
    /** @brief Token stream to be parsed */
    std::vector<Token> tokens;

    std::shared_ptr<Expression> parseAtomOrExpr();
    /** @brief Scanner for error reporting with source context */
    std::shared_ptr<Scanner> scanner;

    /** @brief Current position in token stream */
    size_t current = 0;

    /** @brief Error recovery state */
    bool panicMode = false;

    /**
     * @brief Parses pattern-matching syntax for macro definitions
     * @return AST node representing the pattern
     */
    std::shared_ptr<Expression> listPattern();

    /**
     * @brief Looks ahead in token stream
     * @param add number of tokens to look ahead
     * @return Token at the specified position
     */
    Token peek(int add) const;

    /**
     * @brief Parses atomic expressions
     * @return AST node for the atomic expression
     * @throws ParseError if current token isn't a valid atom
     */
    std::shared_ptr<Expression> atom();

    /**
     * @brief Parses quoted expressions
     * @return AST node for the quoted expression
     */
    std::shared_ptr<Expression> quoteExpression();

    /**
     * @brief Parses s-expressions (function calls)
     * @return AST node for the s-expression
     */
    std::shared_ptr<Expression> sexpression();

    /**
     * @brief Parses vector literals
     * @return AST node for the vector
     */
    std::shared_ptr<Expression> vector();

    /**
     * @brief Parses lambda expressions
     * @return AST node for the lambda
     * @throws ParseError if lambda syntax is invalid
     */
    std::shared_ptr<Expression> lambda();

    /**
     * @brief Parses if expressions
     * @return AST node for the if expression
     */
    std::shared_ptr<Expression> ifExpression();

    /**
     * @brief Parses define expressions
     * @return AST node for variable/procedure definition
     */
    std::shared_ptr<Expression> defineExpression();

    /**
     * @brief Parses tail expressions
     * @details Marks expressions in tail position for proper tail recursion
     * @return AST node wrapped in tail context
     */
    std::shared_ptr<Expression> tailExpression();

    /**
     * @brief Moves parser position forward
     * @return Previous token
     */
    Token advance();

    /**
     * @brief Checks if parsing is complete
     * @return true if at end of input
     */
    bool isAtEnd() const;

    /**
     * @brief Looks at current token
     * @return Current token
     */
    Token peek() const;

    /**
     * @brief Gets the last consumed token
     * @return Previous token
     */
    Token previousToken();

    /**
     * @brief Consumes token if it matches expected type
     * @param type Expected token type
     * @param message Error message if token doesn't match
     * @return Consumed token
     * @throws ParseError if token doesn't match expected type
     */
    Token consume(Tokentype type, const std::string& message);

    /**
     * @brief Checks current token type
     * @param type Token type to check
     * @return true if current token matches type
     */
    bool check(Tokentype type) const;

    /**
     * @brief Tries to match and consume current token
     * @param type Expected token type
     * @return true if token was matched and consumed
     */
    bool match(Tokentype type);

    /**
     * @brief Reports parsing error
     * @param message Error message
     */
    void error(const std::string& message);

    /**
     * @brief Reports error at specific token
     * @param token Token where error occurred
     * @param message Error message
     */
    void errorAt(const Token& token, const std::string& message);

    /** @brief Tokens queued for importing */
    std::vector<std::vector<Token>> toImport;

    /** @brief Imported expressions */
    std::optional<std::vector<Expression>> imported;

    /**
     * @brief Parses syntax-rules expressions
     * @return AST node for macro rules
     */
    std::shared_ptr<Expression> syntaxRulesExpression();

    /**
     * @brief Parses define-syntax expressions
     * @return AST node for macro definition
     */
    std::shared_ptr<Expression> defineSyntaxExpression();

    /**
     * @brief Parses let expressions
     * @return AST node for let binding
     */
    std::shared_ptr<Expression> letExpression();

    /**
     * @brief Parses letrec expressions
     * @return AST node for recursive let binding
     */
    std::shared_ptr<Expression> letRecExpression();

public:
    /**
     * @brief Default constructor
     */
    Parser() = default;

    /**
     * @brief Loads tokens for parsing
     * @param t Vector of tokens to parse
     */
    void load(const std::vector<Token>& t);

    /**
     * @brief Parses loaded tokens into AST
     * @return Optional vector of Expression pointers (nullopt if parsing fails)
     */
    std::optional<std::vector<std::shared_ptr<Expression>>> parse();

    /**
     * @brief Initializes parser with scanner for error reporting
     * @param s Shared pointer to Scanner
     */
    void initialize(std::shared_ptr<Scanner> s);

    /**
     * @brief Parses import expressions
     * @return AST node for import statement
     */
    std::shared_ptr<Expression> import();

    /**
     * @brief Parses any valid Scheme expression
     * @return AST node for the expression
     */
    std::shared_ptr<Expression> expression();

    /**
     * @brief Parses list expressions
     * @return AST node for the list
     */
    std::shared_ptr<Expression> list();
};
