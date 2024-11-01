#pragma once
#include "Expression.h"
#include "Scanner.h"
#include "Token.h"
#include <memory>
#include <optional>
#include <vector>

class Parser {
private:
    std::vector<Token> tokens;
    std::shared_ptr<Scanner> scanner;
    size_t current = 0;
    bool panicMode = false;

    std::shared_ptr<Expression> expression();
    std::shared_ptr<Expression> atom();
    std::shared_ptr<Expression> quoteExpression();
    std::shared_ptr<Expression> sexpression();
    std::shared_ptr<Expression> list();
    std::shared_ptr<Expression> vector();
    std::shared_ptr<Expression> lambda();
    std::shared_ptr<Expression> ifExpression();
    std::shared_ptr<Expression> defineExpression();

    Token advance();
    bool isAtEnd() const;
    Token peek() const;
    Token previousToken();
    Token consume(Tokentype type, const std::string& message);
    bool check(Tokentype type) const;
    bool match(Tokentype type);
    void error(const std::string& message);
    void errorAt(const Token& token, const std::string& message);

public:
    Parser() = default;
    void load(const std::vector<Token>& t);
    std::optional<std::vector<std::shared_ptr<Expression>>> parse();
    void initialize(std::shared_ptr<Scanner> s);
};
