// Parser.h
#pragma once
#include "Expression.h"
#include "Scanner.h"
#include <functional>
#include <memory>
#include <unordered_map>
#include <vector>

class Parser {
public:
    Parser(std::vector<Token> tokens, Scanner* scn)
        : tokens { tokens }
        , scanner { scn } { };
    Parser()
        : scanner(nullptr)
        , current(0)
        , hadError(false)
        , panicMode(false)
    {
    }
    void load(const std::vector<Token>& newTokens)
    {
        tokens = newTokens;
        current = 0;
        hadError = false;
        panicMode = false;
    }

    void initialize(Scanner& scn)
    {
        scanner = &scn;
        current = 0;
        hadError = false;
        panicMode = false;
    }
    std::optional<std::unique_ptr<Expression>> parse();
    bool hadError;
    bool panicMode;

private:
    std::unique_ptr<Expression> expression();
    std::vector<Token> tokens;
    Scanner* scanner;
    size_t current;
    void error(const std::string& message);
    std::unique_ptr<Expression> list();

    std::unique_ptr<Expression> atom();
    Token advance();
    bool isAtEnd() const;
    Token peek() const;
    Token previousToken();
    Token consume(Tokentype type, const std::string& message);
    bool check(Tokentype type) const;
    bool match(Tokentype type);

    void errorAt(const Token& token, const std::string& message);
};
