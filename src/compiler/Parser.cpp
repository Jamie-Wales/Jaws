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
    rules[Tokentype::INTEGER] = { &Parser::literal, nullptr, nullptr, Precedence::NONE };
    rules[Tokentype::STRING] = { &Parser::literal, nullptr, nullptr, Precedence::NONE };
}

std::unique_ptr<Expression> Parser::parse()
{
    return parsePrecedence(Precedence::NONE);
}

std::unique_ptr<Expression> Parser::parsePrecedence(Precedence precedence)
{
    advance();
    ParseFn prefixRule = getRule(previousToken().type).prefix;
    if (prefixRule == nullptr) {
        error("Expect expression.");
        return nullptr;
    }

    bool canAssign = precedence <= Precedence::ASSIGNMENT;
    auto left = (this->*prefixRule)(canAssign);

    while (precedence <= getRule(peek().type).precedence) {
        advance();
        InfixFn infixRule = getRule(previousToken().type).infix;
        if (infixRule != nullptr) {
            left = (this->*infixRule)(std::move(left), canAssign);
        } else {
            break;
        }
    }
    if (const InfixFn postfixRule = getRule(previousToken().type).postfix; postfixRule != nullptr) {
        left = (this->*postfixRule)(std::move(left), canAssign);
    }
    if (canAssign && match(Tokentype::EQUAL)) {
        error("Invalid assignment target.");
    }
    return left;
}

std::unique_ptr<Expression> Parser::literal(bool canAssign)
{
    if (match(Tokentype::INTEGER) || match(Tokentype::FLOAT) || match(Tokentype::SYMBOL)) {
        return make_expression(LiteralExpression(previousToken()), previousToken().line);
    }
    throw ParseError("Expect expression.", peek());
}

bool Parser::isValidOperator(Tokentype type) const
{
    static const std::unordered_set<Tokentype> validOperators = {
        Tokentype::PLUS, Tokentype::MINUS, Tokentype::MULTIPLY, Tokentype::DIVIDE,
        Tokentype::EQUAL, Tokentype::LESS_THAN, Tokentype::GREATER_THAN
    };
    return validOperators.find(type) != validOperators.end();
}

Parser::ParseRule Parser::getRule(Tokentype type)
{
    auto it = rules.find(type);
    if (it != rules.end()) {
        return it->second;
    }
    return { nullptr, nullptr, nullptr, Precedence::NONE };
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

void Parser::error(const std::string& message)
{
    errorAt(previousToken(), message);
}

void Parser::errorAt(const Token& token, const std::string& message)
{
    if (panicMode)
        return;
    panicMode = true;

    std::cerr << "[line " << token.line << "] Error";

    if (token.type == Tokentype::EOF_TOKEN) {
        std::cerr << " at end";
    } else {
        std::cerr << " at '" << token.lexeme << "'";
    }

    std::cerr << ": " << message << std::endl;
    hadError = true;
}
