#include "Parser.h"
#include "Error.h"
#include "Expression.h"
#include "Token.h"
#include "run.h"
#include <optional>

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

std::shared_ptr<Expression> Parser::import()
{
    std::vector<Token> output = {};
    while (match(Tokentype::IDENTIFIER)) {
        output.emplace_back(previousToken());
    }

    consume(Tokentype::RIGHT_PAREN, "import ends with )");
    return std::make_shared<Expression>(Expression { ImportExpression { std::move(output) }, previousToken().line });
}

std::optional<std::vector<std::shared_ptr<Expression>>> Parser::parse()
{
    auto output = std::vector<std::shared_ptr<Expression>> {};
    try {
        while (!isAtEnd()) {
            auto expr = expression();
            if (expr) {
                output.push_back(expr);
            }
        }
        return output;
    } catch (const ParseError& e) {
        e.printFormattedError();
        return std::nullopt;
    }
}

std::shared_ptr<Expression> Parser::vector()
{
    if (match(Tokentype::LEFT_PAREN)) {
        std::vector<std::shared_ptr<Expression>> elements;
        while (!match(Tokentype::RIGHT_PAREN)) {
            elements.push_back(expression());
        }

        return std::make_shared<Expression>(Expression {
            VectorExpression { std::move(elements) },
            previousToken().line });
    }

    throw ParseError("Expect list when defining vector", previousToken(), scanner->getLine(previousToken().line));
}

std::shared_ptr<Expression> Parser::tailExpression()
{
    return std::make_shared<Expression>(TailExpression { expression() }, previousToken().line);
}

std::shared_ptr<Expression> Parser::ifExpression()
{
    auto condition = expression();
    auto then = tailExpression();
    std::optional<std::shared_ptr<Expression>> elseExpr = std::nullopt;
    if (!check(Tokentype::RIGHT_PAREN)) {
        elseExpr = tailExpression();
    }
    consume(Tokentype::RIGHT_PAREN, "Expect ')' at end of if expression");

    return std::make_shared<Expression>(Expression {
        IfExpression {
            condition,
            then,
            elseExpr },
        previousToken().line });
}

std::shared_ptr<Expression> Parser::letExpression()
{
    LetExpression::Args output;
    std::optional<Token> token;
    if (match(Tokentype::IDENTIFIER)) {
        token = previousToken();
    }

    consume(Tokentype::LEFT_PAREN, "Expected '(' after let");
    int count = 1;
    while (count != 0) {
        if (match(Tokentype::LEFT_PAREN)) {
            count++;
        }
        auto name = consume(Tokentype::IDENTIFIER, "Expected identifier");
        auto value = expression();
        output.push_back({ name, value });

        while (match(Tokentype::RIGHT_PAREN)) {
            count--;
        }
    }
    std::vector<std::shared_ptr<Expression>> body;
    while (!match(Tokentype::RIGHT_PAREN)) {
        body.emplace_back(expression());
    }
    return std::make_shared<Expression>(
        LetExpression { token, output, body },
        previousToken().line);
}

std::shared_ptr<Expression> Parser::letRecExpression()
{
    throw std::runtime_error("Not implemented");
}

std::shared_ptr<Expression> Parser::expression()
{
    if (match(Tokentype::LEFT_PAREN)) {
        if (match(Tokentype::DEFINE)) {
            return defineExpression();
        } else if (match(Tokentype::LET)) {
            return letExpression();
        } else if (match(Tokentype::LETREC)) {
            return letRecExpression();
        } else if (match(Tokentype::DEFINE_SYTAX)) {
            return defineSyntaxExpression();
        } else if (match(Tokentype::SYNTAX_RULE)) {
            return syntaxRuleExpression();
        } else if (match(Tokentype::IMPORT)) {
            return import();
        } else if (match(Tokentype::LAMBDA)) {
            return lambda();
        } else if (match(Tokentype::IF)) {
            return ifExpression();
        } else if (match(Tokentype::QUOTE)) {
            auto qexpr = quoteExpression();
            consume(Tokentype::RIGHT_PAREN, "(quote expects ')' at end of expression)");
            return qexpr;
        } else {
            return sexpression();
        }
    } else if (match(Tokentype::QUOTE)) {
        return quoteExpression();
    } else if (match(Tokentype::HASH)) {
        return vector();
    } else {
        return atom();
    }
}

std::shared_ptr<Expression> Parser::defineExpression()
{
    if (match(Tokentype::LEFT_PAREN)) {
        Token name = consume(Tokentype::IDENTIFIER, "Expect function name");
        std::vector<Token> parameters;
        while (!check(Tokentype::RIGHT_PAREN) && !isAtEnd()) {
            parameters.push_back(
                consume(Tokentype::IDENTIFIER, "Expect parameter name"));
        }
        consume(Tokentype::RIGHT_PAREN, "Expect ')' after parameter list");

        std::vector<std::shared_ptr<Expression>> body;
        while (!check(Tokentype::RIGHT_PAREN)) {
            body.push_back(expression());
        }
        consume(Tokentype::RIGHT_PAREN, "Expect ')' after function definition");

        return std::make_shared<Expression>(Expression {
            DefineProcedure {
                name,
                std::move(parameters),
                std::move(body) },
            name.line });
    } else {
        Token name = consume(Tokentype::IDENTIFIER, "Expect variable name");
        auto value = expression();
        consume(Tokentype::RIGHT_PAREN, "Expect ')' after variable definition");
        return std::make_shared<Expression>(Expression {
            DefineExpression {
                name,
                value },
            name.line });
    }
}
std::shared_ptr<Expression> Parser::lambda()
{
    std::vector<Token> parameters;
    consume(Tokentype::LEFT_PAREN, "Lambda expects a parameter list");
    while (!check(Tokentype::RIGHT_PAREN) && !isAtEnd()) {
        parameters.push_back(
            consume(Tokentype::IDENTIFIER, "Expect parameter name"));
    }
    consume(Tokentype::RIGHT_PAREN, "Expect ')' after parameter list");

    std::vector<std::shared_ptr<Expression>> body;
    while (!check(Tokentype::RIGHT_PAREN)) {
        body.push_back(expression());
    }

    if (!body.empty()) {
        body.back() = std::make_shared<Expression>(
            Expression { TailExpression { body.back() },
                previousToken().line });
    }

    consume(Tokentype::RIGHT_PAREN, "Expect ')' after lambda body");
    return std::make_shared<Expression>(Expression {
        LambdaExpression {
            std::move(parameters),
            std::move(body) },
        previousToken().line });
}
std::shared_ptr<Expression> Parser::atom()
{
    Token token = advance();
    switch (token.type) {
    case Tokentype::IDENTIFIER:
    case Tokentype::INTEGER:
    case Tokentype::FLOAT:
    case Tokentype::COMPLEX:
    case Tokentype::RATIONAL:
    case Tokentype::STRING:
    case Tokentype::TRUE:
    case Tokentype::FALSE:
    case Tokentype::LAMBDA:
    case Tokentype::ARROW:
    case Tokentype::IF:
        return std::make_shared<Expression>(Expression { AtomExpression { token }, token.line });
    default:
        throw ParseError("Unexpected token in atom ", token, scanner->getLine(token.line));
    }
}

std::shared_ptr<Expression> Parser::sexpression()
{
    std::vector<std::shared_ptr<Expression>> elements = {};
    while (!match(Tokentype::RIGHT_PAREN) && !isAtEnd()) {
        elements.push_back(expression());
    }
    if (elements.size() > 0) {
        elements.back() = std::make_shared<Expression>(Expression { TailExpression { elements.back() }, previousToken().line });
    }
    return std::make_shared<Expression>(Expression { sExpression { std::move(elements) }, previousToken().line });
}

std::shared_ptr<Expression> Parser::quoteExpression()
{
    return std::make_shared<Expression>(Expression { QuoteExpression { expression() }, previousToken().line });
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

Token Parser::peek(int add) const
{
    return tokens[current + add];
}

Token Parser::previousToken()
{
    return tokens[current - 1];
}

Token Parser::consume(Tokentype type, const std::string& message)
{
    if (check(type))
        return advance();
    throw ParseError(message + " Token: " + previousToken().lexeme + " ", peek(), scanner->getLine(peek().line));
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
std::shared_ptr<Expression> Parser::syntaxRuleExpression()
{
    auto pattern = expression();
    consume(Tokentype::ARROW, "Expect => after pattern");
    auto templ = expression();
    consume(Tokentype::RIGHT_PAREN, "Expect ) after syntax-rule");
    return std::make_shared<Expression>(Expression {
        SyntaxRuleExpression { pattern, templ },
        previousToken().line });
}

std::shared_ptr<Expression> Parser::defineSyntaxExpression()
{
    Token name = consume(Tokentype::IDENTIFIER, "Expect macro name");
    auto rule = expression();
    consume(Tokentype::RIGHT_PAREN, "Expect ) after define-syntax");
    return std::make_shared<Expression>(Expression {
        DefineSyntaxExpression { name, rule },
        name.line });
}
