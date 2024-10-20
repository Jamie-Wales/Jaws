#include "Parser.h"
#include "Error.h"
#include <iostream>

void Parser::initialize(std::shared_ptr<Scanner> s)
{
    scanner = s;
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
        e.printFormattedError();
        return std::nullopt;
    }
}

std::unique_ptr<Expression> Parser::expression()
{
    Token token = peek();
    if (token.type == Tokentype::LEFT_PAREN) {
        advance();
        if (match(Tokentype::DEFINE)) {
            return defineExpression();
        } else {
            return sexpression();
        }
    } else {
        return atom();
    }
}

std::unique_ptr<Expression> Parser::defineExpression()
{
    Token name = advance();
    if (match(Tokentype::LEFT_PAREN)) {
        size_t count = 0;
        while (!match(Tokentype::RIGHT_PAREN)) {
            count++;
        }
        std::unique_ptr<Expression> body = expression();
        return std::make_unique<Expression>(Expression {
            DefineProcedure { name, count, std::move(body) },
            name.line });
    } else {
        std::unique_ptr<Expression> value = expression();
        consume(Tokentype::RIGHT_PAREN, "Expect ')' after define expression.");
        return std::make_unique<Expression>(Expression {
            DefineExpression { std::move(name), std::move(value) },
            name.line });
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
    case Tokentype::COMPLEX:
    case Tokentype::RATIONAL:
    case Tokentype::STRING:
    case Tokentype::TRUE:
    case Tokentype::FALSE:
    case Tokentype::LAMBDA:
    case Tokentype::IF:
        return std::make_unique<Expression>(Expression { AtomExpression { token }, token.line });
    default:
        throw ParseError("Unexpected token", token, scanner->getLine(token.line));
    }
}

std::unique_ptr<Expression> Parser::sexpression()
{
    std::vector<std::unique_ptr<Expression>> elements;
    elements.push_back(expression());
    while (!check(Tokentype::RIGHT_PAREN) && !isAtEnd()) {
        elements.push_back(expression());
    }
    Token rightParen = consume(Tokentype::RIGHT_PAREN, "Expect ')' after s-expression.");
    return std::make_unique<Expression>(Expression { sExpression { std::move(elements) }, rightParen.line });
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
