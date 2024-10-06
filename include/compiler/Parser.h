// Parser.h
#pragma once
#include "Expression.h"
#include <functional>
#include <memory>
#include <unordered_map>
#include <vector>

enum class Precedence {
    NONE = 0,
    LOWEST = 1,
    SUM = 2, // + -
    PRODUCT = 3, // * /
    PREFIX = 4 // unary -
};

class Parser {
public:
    using PrefixParseFn = std::function<std::unique_ptr<Expression>()>;
    using InfixParseFn = std::function<std::unique_ptr<Expression>(std::unique_ptr<Expression>)>;

    struct ParseRule {
        PrefixParseFn prefix;
        InfixParseFn infix;
        Precedence precedence;
    };

    Parser(std::vector<Token> tokens);
    std::unique_ptr<Expression> parse();

private:
    std::vector<Token> tokens;

    bool isValidOperator(Tokentype type) const;
    std::unique_ptr<Expression> primary();
    size_t current;
    std::unordered_map<Tokentype, ParseRule> rules;

    void initRules();
    std::unique_ptr<Expression> expression();
    std::unique_ptr<Expression> parsePrecedence(Precedence precedence);
    std::unique_ptr<Expression> number();
    std::unique_ptr<Expression> symbol();
    std::unique_ptr<Expression> list();
    ParseRule getRule(Tokentype type) const;
    Token advance();
    bool isAtEnd() const;
    Token peek() const;
    Token previous() const;
    Token consume(Tokentype type, const std::string& message);
    bool check(Tokentype type) const;
    bool match(Tokentype type);
};
