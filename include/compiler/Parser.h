// Parser.h
#pragma once
#include "Expression.h"
#include <functional>
#include <memory>
#include <unordered_map>
#include <vector>

enum class Precedence {
    NONE,
    ASSIGNMENT, // =
    OR, // or
    AND, // and
    EQUALITY, // == !=
    COMPARISON, // < > <= >=
    TERM, // + -
    FACTOR, // * /
    UNARY, // ! -
    CALL, // . ()
    PRIMARY
};
class Parser {
public:
    using ParseFn = std::unique_ptr<Expression> (Parser::*)(bool canAssign);
    using InfixFn = std::unique_ptr<Expression> (Parser::*)(std::unique_ptr<Expression> left, bool canAssign);
    struct ParseRule {
        ParseFn prefix;
        InfixFn infix;
        InfixFn postfix;
        Precedence precedence;
    };

    ParseRule getRule(Tokentype type);
    Parser(std::vector<Token> tokens);
    std::unique_ptr<Expression> parse();
    bool hadError;
    bool panicMode;

private:
    std::vector<Token> tokens;

    bool isValidOperator(Tokentype type) const;
    std::unique_ptr<Expression> primary();
    size_t current;
    std::unordered_map<Tokentype, ParseRule> rules;

    void error(const std::string& message);
    void initRules();
    std::unique_ptr<Expression> literal(bool canAssign);
    std::unique_ptr<Expression> parsePrecedence(Precedence precedence);
    Token advance();
    bool isAtEnd() const;
    Token peek() const;
    Token previousToken();
    Token consume(Tokentype type, const std::string& message);
    bool check(Tokentype type) const;
    bool match(Tokentype type);

    void errorAt(const Token& token, const std::string& message);
};
