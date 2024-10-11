#include "Parser.h"
#include "ParseError.h"
#include <memory>
#include <optional>
#include <stdexcept>
#include <unordered_set>

std::optional<std::unique_ptr<Expression>> Parser::parse()
{
    try {
        if (isAtEnd()) {
            return std::nullopt;
        }
        return expression();
    } catch (const ParseError& e) {
        std::cerr << e.what() << std::endl;
        return std::nullopt;
    }
}

std::unique_ptr<Expression> Parser::expression()
{
    Token token = peek();
    if (token.type == Tokentype::LEFT_PAREN) {
        return list();
    } else {
        return atom();
    }
}

std::unique_ptr<Expression> Parser::atom()
{
    Token token = advance();
    switch (token.type) {
    case Tokentype::SYMBOL:
    case Tokentype::INTEGER:
    case Tokentype::FLOAT:
    case Tokentype::STRING:
        return std::make_unique<Expression>(Expression { AtomExpression { token }, token.line });
    default:
        throw ParseError("Unexpected token", token, scanner->getLine(token.line));
    }
}
std::unique_ptr<Expression> Parser::list()
{
    Token leftParen = consume(Tokentype::LEFT_PAREN, "Expect '(' at start of list.");
    std::vector<std::unique_ptr<Expression>> elements;

    while (!check(Tokentype::RIGHT_PAREN) && !isAtEnd()) {
        auto expr = expression();
        elements.push_back(std::move(expr));
    }

    Token rightParen = consume(Tokentype::RIGHT_PAREN, "Expect ')' after list.");
    return std::make_unique<Expression>(Expression { { std::move(elements) }, leftParen.line });
}
Token Parser::advance()
{
    if (!isAtEnd())
        current++;
    return previousToken();
}

bool Parser::isAtEnd() const
{
    return peek().type == Tokentype::EOF_TOKEN;
}

Token Parser::peek() const
{
    return tokens[current];
}

Token Parser::previousToken()
{
    return tokens[current - 1];
}

Token Parser::consume(Tokentype type, const std::string& message)
{
    if (check(type))
        return advance();
    throw ParseError(message, peek(), scanner->getLine(peek().line));
}

bool Parser::check(Tokentype type) const
{
    if (isAtEnd())
        return false;
    return peek().type == type;
}

bool Parser::match(Tokentype type)
{
    if (check(type)) {
        advance();
        return true;
    }
    return false;
}

void Parser::error(const std::string& message)
{
    auto prev = previousToken();
    throw ParseError(message, prev, scanner->getLine(prev.line));
}

void Parser::errorAt(const Token& token, const std::string& message)
{
    if (panicMode)
        return;
    panicMode = true;
    throw ParseError(message, token, scanner->getLine(token.line));
}
