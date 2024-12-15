#include "parse.h"

namespace parse {

// Helper functions implementation
Token peek(const ParserState& state)
{
    return state.tokens[state.current];
}

Token peek(const ParserState& state, int ahead)
{
    return state.tokens[state.current + ahead];
}

Token previousToken(const ParserState& state)
{
    return state.tokens[state.current - 1];
}

bool isAtEnd(const ParserState& state)
{
    return peek(state).type == Tokentype::EOF_TOKEN;
}

Token advance(ParserState& state)
{
    if (!isAtEnd(state)) {
        state.current++;
    }
    return state.tokens[state.current - 1];
}

bool check(const ParserState& state, Tokentype type)
{
    if (isAtEnd(state))
        return false;
    return peek(state).type == type;
}

bool match(ParserState& state, Tokentype type)
{
    if (check(state, type)) {
        advance(state);
        return true;
    }
    return false;
}

void error(ParserState& state, const std::string& message)
{
    auto prev = previousToken(state);
    throw ParseError(message, prev, "");
}

void errorAt(ParserState& state, const Token& token, const std::string& message)
{
    if (state.panicMode)
        return;
    state.panicMode = true;
    throw ParseError(message, token, "");
}

Token consume(ParserState& state, Tokentype type, const std::string& message)
{
    if (check(state, type))
        return advance(state);
    throw ParseError(message + " Token: " + previousToken(state).lexeme, peek(state), "");
}

std::shared_ptr<Expression> parseSet(ParserState& state)
{
    auto name = consume(state, Tokentype::IDENTIFIER, "Expect Identifier for set!");
    auto expr = parseExpression(state);
    consume(state, Tokentype::RIGHT_PAREN, "Expect ')' after expression");
    return std::make_shared<Expression>(Expression { SetExpression { name, expr }, previousToken(state).line });
}
std::shared_ptr<Expression> parseExpression(ParserState& state)
{
    if (match(state, Tokentype::LEFT_PAREN)) {
        if (match(state, Tokentype::DEFINE))
            return parseDefine(state);
        if (match(state, Tokentype::LET))
            return parseLet(state);
        if (match(state, Tokentype::LETREC))
            return parseLetRec(state);
        if (match(state, Tokentype::SET))
            return parseSet(state);
        if (match(state, Tokentype::DEFINE_SYTAX))
            return parseDefineSyntax(state);
        if (match(state, Tokentype::SYNTAX_RULE))
            return parseSyntaxRules(state);
        if (match(state, Tokentype::IMPORT))
            return parseImport(state);
        if (match(state, Tokentype::LAMBDA))
            return parseLambda(state);
        if (match(state, Tokentype::IF))
            return parseIf(state);
        if (match(state, Tokentype::QUOTE)) {
            auto expr = parseQuoted(state);
            consume(state, Tokentype::RIGHT_PAREN, "Expected ')' after quoted expression");
            return expr;
        }
        return parseSExpression(state);
    }
    if (match(state, Tokentype::QUOTE))
        return parseQuoted(state);
    if (match(state, Tokentype::HASH))
        return parseVector(state);
    return parseAtom(state);
}

std::shared_ptr<Expression> parseAtom(ParserState& state)
{
    Token token = advance(state);
    switch (token.type) {
    case Tokentype::IDENTIFIER:
    case Tokentype::INTEGER:
    case Tokentype::FLOAT:
    case Tokentype::COMPLEX:
    case Tokentype::RATIONAL:
    case Tokentype::STRING:
    case Tokentype::ELLIPSIS:
    case Tokentype::TRUE:
    case Tokentype::FALSE:
    case Tokentype::LAMBDA:
    case Tokentype::ARROW:
    case Tokentype::IF:
    case Tokentype::SYNTAX_RULE:
        return std::make_shared<Expression>(Expression { AtomExpression { token }, token.line });
    default:
        throw ParseError("Unexpected token in atom", token, "");
    }
}

std::shared_ptr<Expression> parseSExpression(ParserState& state)
{
    std::vector<std::shared_ptr<Expression>> elements;
    while (!match(state, Tokentype::RIGHT_PAREN) && !isAtEnd(state)) {
        elements.push_back(parseExpression(state));
    }
    if (!elements.empty()) {
        elements.back() = std::make_shared<Expression>(
            Expression { TailExpression { elements.back() }, previousToken(state).line });
    }
    return std::make_shared<Expression>(
        Expression { sExpression { std::move(elements) }, previousToken(state).line });
}

std::shared_ptr<Expression> parseDefine(ParserState& state)
{
    if (match(state, Tokentype::LEFT_PAREN)) {
        Token name = consume(state, Tokentype::IDENTIFIER, "Expected function name");
        std::vector<Token> parameters;
        while (!check(state, Tokentype::RIGHT_PAREN) && !isAtEnd(state)) {
            parameters.push_back(consume(state, Tokentype::IDENTIFIER, "Expected parameter name"));
        }
        consume(state, Tokentype::RIGHT_PAREN, "Expected ')' after parameter list");

        std::vector<std::shared_ptr<Expression>> body;
        while (!check(state, Tokentype::RIGHT_PAREN)) {
            body.push_back(parseExpression(state));
        }
        consume(state, Tokentype::RIGHT_PAREN, "Expected ')' after function body");

        return std::make_shared<Expression>(Expression {
            DefineProcedure { name, std::move(parameters), std::move(body) },
            name.line });
    } else {
        Token name = consume(state, Tokentype::IDENTIFIER, "Expected variable name");
        auto value = parseExpression(state);
        consume(state, Tokentype::RIGHT_PAREN, "Expected ')' after variable definition");
        return std::make_shared<Expression>(Expression {
            DefineExpression { name, value },
            name.line });
    }
}

std::shared_ptr<Expression> parseLambda(ParserState& state)
{
    consume(state, Tokentype::LEFT_PAREN, "Lambda expects parameter list");
    std::vector<Token> parameters;
    while (!check(state, Tokentype::RIGHT_PAREN) && !isAtEnd(state)) {
        parameters.push_back(consume(state, Tokentype::IDENTIFIER, "Expected parameter name"));
    }
    consume(state, Tokentype::RIGHT_PAREN, "Expected ')' after parameters");

    std::vector<std::shared_ptr<Expression>> body;
    while (!check(state, Tokentype::RIGHT_PAREN)) {
        body.push_back(parseExpression(state));
    }

    if (!body.empty()) {
        body.back() = std::make_shared<Expression>(
            Expression { TailExpression { body.back() }, previousToken(state).line });
    }

    consume(state, Tokentype::RIGHT_PAREN, "Expected ')' after lambda body");
    return std::make_shared<Expression>(Expression {
        LambdaExpression { std::move(parameters), std::move(body) },
        previousToken(state).line });
}

std::shared_ptr<Expression> parseIf(ParserState& state)
{
    auto condition = parseExpression(state);
    auto then = parseTailExpression(state);
    std::optional<std::shared_ptr<Expression>> elseExpr = std::nullopt;
    if (!check(state, Tokentype::RIGHT_PAREN)) {
        elseExpr = parseTailExpression(state);
    }
    consume(state, Tokentype::RIGHT_PAREN, "Expected ')' after if expression");

    return std::make_shared<Expression>(Expression {
        IfExpression { condition, then, elseExpr },
        previousToken(state).line });
}

std::shared_ptr<Expression> parseQuoted(ParserState& state)
{
    return std::make_shared<Expression>(
        Expression { QuoteExpression { parseExpression(state) }, previousToken(state).line });
}

std::shared_ptr<Expression> parseLet(ParserState& state)
{
    LetExpression::Args output;
    std::optional<Token> token;
    if (match(state, Tokentype::IDENTIFIER)) {
        token = previousToken(state);
    }

    consume(state, Tokentype::LEFT_PAREN, "Expected '(' after let");
    int count = 1;
    while (count != 0) {
        if (match(state, Tokentype::LEFT_PAREN)) {
            count++;
        }
        auto name = consume(state, Tokentype::IDENTIFIER, "Expected identifier");
        auto value = parseExpression(state);
        output.push_back({ name, value });

        while (match(state, Tokentype::RIGHT_PAREN)) {
            count--;
        }
    }

    std::vector<std::shared_ptr<Expression>> body;
    while (!match(state, Tokentype::RIGHT_PAREN)) {
        body.emplace_back(parseExpression(state));
    }

    return std::make_shared<Expression>(
        LetExpression { token, output, body },
        previousToken(state).line);
}

std::shared_ptr<Expression> parseLetRec(ParserState& state)
{
    throw ParseError("letrec not implemented", peek(state), "");
}

std::shared_ptr<Expression> parseTailExpression(ParserState& state)
{
    return std::make_shared<Expression>(
        TailExpression { parseExpression(state) },
        previousToken(state).line);
}

std::shared_ptr<Expression> parseVector(ParserState& state)
{
    if (match(state, Tokentype::LEFT_PAREN)) {
        std::vector<std::shared_ptr<Expression>> elements;
        while (!match(state, Tokentype::RIGHT_PAREN)) {
            elements.push_back(parseExpression(state));
        }
        return std::make_shared<Expression>(Expression {
            VectorExpression { std::move(elements) },
            previousToken(state).line });
    }
    throw ParseError("Expected list when defining vector", previousToken(state), "");
}

std::shared_ptr<Expression> parseList(ParserState& state)
{
    std::vector<std::shared_ptr<Expression>> elements;
    bool isVariadic = false;
    while (!check(state, Tokentype::RIGHT_PAREN) && !isAtEnd(state)) {
        if (match(state, Tokentype::LEFT_PAREN)) {
            elements.push_back(parseList(state));
            consume(state, Tokentype::RIGHT_PAREN, "Expected ')' after nested list");
        } else if (match(state, Tokentype::ELLIPSIS)) {
            isVariadic = true;
        } else {
            Token token = advance(state);
            elements.push_back(std::make_shared<Expression>(
                Expression { AtomExpression { token }, token.line }));
        }
    }
    return std::make_shared<Expression>(
        ListExpression { std::move(elements), isVariadic },
        previousToken(state).line);
}

std::shared_ptr<Expression> parseImport(ParserState& state)
{
    std::vector<Token> output;
    while (match(state, Tokentype::IDENTIFIER)) {
        output.emplace_back(previousToken(state));
    }

    consume(state, Tokentype::RIGHT_PAREN, "import ends with )");
    return std::make_shared<Expression>(
        Expression { ImportExpression { std::move(output) }, previousToken(state).line });
}

std::shared_ptr<Expression> parseSyntaxRules(ParserState& state)
{
    consume(state, Tokentype::LEFT_PAREN, "Expect ( after syntax-rules");
    std::vector<Token> literals;
    while (!check(state, Tokentype::RIGHT_PAREN) && !isAtEnd(state)) {
        literals.push_back(consume(state, Tokentype::IDENTIFIER, "Expect literal identifier"));
    }
    consume(state, Tokentype::RIGHT_PAREN, "Expect ) after literals list");

    std::vector<std::shared_ptr<Expression>> patterns;
    std::vector<std::shared_ptr<Expression>> templates;
    while (!check(state, Tokentype::RIGHT_PAREN) && !isAtEnd(state)) {
        consume(state, Tokentype::LEFT_PAREN, "Expect ( before pattern");
        auto list = parseList(state);
        auto aslist = std::get<ListExpression>(list->as);
        for (size_t i = 0; i < aslist.elements.size() - 1; i++) {
            patterns.push_back(aslist.elements[i]);
            templates.push_back(aslist.elements[i + 1]);
        }
        consume(state, Tokentype::RIGHT_PAREN, "Expect ) after template");
    }

    consume(state, Tokentype::RIGHT_PAREN, "Expect ) after syntax-rules");
    return std::make_shared<Expression>(Expression {
        SyntaxRulesExpression {
            std::move(literals),
            std::move(patterns),
            std::move(templates) },
        previousToken(state).line });
}

std::shared_ptr<Expression> parseDefineSyntax(ParserState& state)
{
    Token name = consume(state, Tokentype::IDENTIFIER, "Expect macro name");
    auto rule = parseExpression(state);
    consume(state, Tokentype::RIGHT_PAREN, "Expect ) after define-syntax");
    return std::make_shared<Expression>(Expression {
        DefineSyntaxExpression { name, rule },
        name.line });
}

std::optional<std::vector<std::shared_ptr<Expression>>> parse(std::vector<Token> tokens)
{
    ParserState state { std::move(tokens), 0, false };

    auto output = std::vector<std::shared_ptr<Expression>> {};
    try {
        while (!isAtEnd(state)) {
            auto expr = parseExpression(state);
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

} // namespace parse
