#include "Parser.h"
#include "ParseError.h"
#include <stdexcept>
#include <unordered_set>

Parser::Parser(std::vector<Token> tokens)
    : tokens(std::move(tokens))
    , current(0)
{
    initRules();
}

void Parser::initRules()
{
    rules[Tokentype::INTEGER] = { [this]() { return number(); }, nullptr, Precedence::NONE };
    rules[Tokentype::FLOAT] = { [this]() { return number(); }, nullptr, Precedence::NONE };
    rules[Tokentype::SYMBOL] = { [this]() { return symbol(); }, nullptr, Precedence::NONE };
    rules[Tokentype::LEFT_PAREN] = { [this]() { return list(); }, nullptr, Precedence::NONE };
}

std::unique_ptr<Expression> Parser::parse()
{
    try {
        return expression();
    } catch (const ParseError& e) {
        std::cerr << e.what() << std::endl;
        return nullptr;
    }
}

std::unique_ptr<Expression> Parser::expression()
{
    return parsePrecedence(Precedence::LOWEST);
}

std::unique_ptr<Expression> Parser::parsePrecedence(Precedence precedence)
{
    return primary();
}

std::unique_ptr<Expression> Parser::primary()
{
    if (match(Tokentype::LEFT_PAREN)) {
        return list();
    } else if (match(Tokentype::INTEGER) || match(Tokentype::FLOAT)) {
        return number();
    } else if (match(Tokentype::SYMBOL)) {
        return symbol();
    }

    throw ParseError("Expect expression.", peek());
}

std::unique_ptr<Expression> Parser::number()
{
    return make_expression(LiteralExpression(previous()), previous().line);
}

std::unique_ptr<Expression> Parser::symbol()
{
    return make_expression(LiteralExpression(previous()), previous().line);
}

bool Parser::isValidOperator(Tokentype type) const
{
    static const std::unordered_set<Tokentype> validOperators = {
        Tokentype::PLUS, Tokentype::MINUS, Tokentype::MULTIPLY, Tokentype::DIVIDE,
        Tokentype::EQUAL, Tokentype::LESS_THAN, Tokentype::GREATER_THAN
    };
    return validOperators.find(type) != validOperators.end();
}
std::unique_ptr<Expression> Parser::list()
{
    Token leftParen = previous();
    std::vector<std::unique_ptr<Expression>> elements;

    if (check(Tokentype::RIGHT_PAREN)) {
        consume(Tokentype::RIGHT_PAREN, "");
        return make_expression(PrefixExpression(Token { Tokentype::SYMBOL, "nil", leftParen.line, leftParen.column }, std::move(elements)), leftParen.line);
    }

    Token firstToken = peek();
    bool isOperator = isValidOperator(firstToken.type);
    Token op;

    if (isOperator) {
        op = advance();
    } else {
        elements.push_back(primary());
    }

    while (!check(Tokentype::RIGHT_PAREN) && !isAtEnd()) {
        elements.push_back(primary());
    }

    if (isAtEnd()) {
        throw ParseError("Unterminated list. Expected ')'.", previous());
    }

    consume(Tokentype::RIGHT_PAREN, "Expect ')' after expression.");

    if (isOperator) {
        return make_expression(PrefixExpression(op, std::move(elements)), leftParen.line);
    } else {
        // If it's not an operator, we return a regular list expression
        return make_expression(ListExpression(std::move(elements)), leftParen.line);
    }
}
Parser::ParseRule Parser::getRule(Tokentype type) const
{
    auto it = rules.find(type);
    if (it != rules.end()) {
        return it->second;
    }
    return { nullptr, nullptr, Precedence::NONE };
}

Token Parser::advance()
{
    if (!isAtEnd())
        current++;
    return previous();
}

bool Parser::isAtEnd() const
{
    return peek().type == Tokentype::EOF_TOKEN;
}

Token Parser::peek() const
{
    return tokens[current];
}

Token Parser::previous() const
{
    return tokens[current - 1];
}

Token Parser::consume(Tokentype type, const std::string& message)
{
    if (check(type))
        return advance();
    throw std::runtime_error(message);
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
