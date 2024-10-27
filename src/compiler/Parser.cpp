#include "Parser.h"
#include "Error.h"

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

std::unique_ptr<Expression> Parser::vector()
{
    if (match(Tokentype::LEFT_PAREN)) {

        std::vector<std::unique_ptr<Expression>> elements;
        while (!match(Tokentype::RIGHT_PAREN)) {
            elements.push_back(expression());
        }

        return std::make_unique<Expression>(Expression {
            VectorExpression { std::move(elements) },
            previousToken().line });
    }

    throw ParseError("Expect list when defining vector", previousToken(), scanner->getLine(previousToken().line));
}

std::unique_ptr<Expression> Parser::expression()
{
    if (match(Tokentype::LEFT_PAREN)) {
        if (match(Tokentype::DEFINE)) {
            return defineExpression();
        } else {
            return sexpression();
        }
    } else if (match(Tokentype::QUOTE)) {
        if (match(Tokentype::HASH)) {
            return vector();
        }
        return list();
    } else if (match(Tokentype::HASH)) {
        return vector();
    } else {
        return atom();
    }
}

std::unique_ptr<Expression> Parser::list()
{
    std::vector<std::unique_ptr<Expression>> output = {};
    if (match(Tokentype::LEFT_PAREN)) {
        while (!match(Tokentype::RIGHT_PAREN)) {
            auto expr = expression();
            output.emplace_back(std::move(expr));
        }
    }

    return std::make_unique<Expression>(Expression {
        ListExpression {
            std::move(output) },
        previousToken().line });
}

std::unique_ptr<Expression> Parser::defineExpression()
{
    if (match(Tokentype::LEFT_PAREN)) {
        Token name = consume(Tokentype::IDENTIFIER, "Expect function name");
        std::vector<Token> parameters;
        while (!check(Tokentype::RIGHT_PAREN) && !isAtEnd()) {
            parameters.push_back(
                consume(Tokentype::IDENTIFIER, "Expect parameter name"));
        }
        consume(Tokentype::RIGHT_PAREN, "Expect ')' after parameter list");

        auto body = expression();
        return std::make_unique<Expression>(Expression {
            DefineProcedure {
                name,
                std::move(parameters),
                std::move(body) },
            name.line });
    } else {
        Token name = consume(Tokentype::IDENTIFIER, "Expect variable name");
        auto value = expression();

        return std::make_unique<Expression>(Expression {
            DefineExpression {
                name,
                std::move(value) },
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
        throw ParseError("Unexpected token in atom ", token, scanner->getLine(token.line));
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
