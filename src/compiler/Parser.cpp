#include "Parser.h"
#include "ParseError.h"
#include <iostream>

void Parser::initialize(Scanner& s)
{
    scanner = std::make_shared<Scanner>(s);
}

void Parser::load(const std::vector<Token>& t)
{
    tokens = t;
    current = 0;
    panicMode = false;
}

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
        return sexpression();
    } else {
        return atom();
    }
}

std::unique_ptr<Expression> Parser::atom()
{
    Token token = advance();
    switch (token.type) {
    case Tokentype::SYMBOL:
    case Tokentype::IDENTIFIER:
    case Tokentype::INTEGER:
    case Tokentype::FLOAT:
    case Tokentype::STRING:
    case Tokentype::TRUE:
    case Tokentype::FALSE:
    case Tokentype::DEFINE:
    case Tokentype::LAMBDA:
    case Tokentype::IF:
        return std::make_unique<Expression>(Expression { AtomExpression { token }, token.line });
    default:
        throw ParseError("Unexpected token", token, scanner->getLine(token.line));
    }
}
std::unique_ptr<Expression> Parser::sexpression()
{
    Token leftParen = consume(Tokentype::LEFT_PAREN, "Expect '(' at start of s-expression.");
    std::vector<std::unique_ptr<Expression>> elements;
    elements.push_back(expression());
    while (!check(Tokentype::RIGHT_PAREN) && !isAtEnd()) {
        elements.push_back(expression());
    }
    Token rightParen = consume(Tokentype::RIGHT_PAREN, "Expect ')' after s-expression.");
    return std::make_unique<Expression>(Expression { sExpression { std::move(elements) }, leftParen.line });
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
