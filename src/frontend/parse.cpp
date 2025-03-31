#include "parse.h"

namespace parse {
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

std::shared_ptr<Expression> parseDefineSyntax(ParserState& state)
{
    Token name = consume(state, Tokentype::IDENTIFIER, "Expected macro name after 'define-syntax'");
    consume(state, Tokentype::LEFT_PAREN, "Expected '(' after macro name");

    if (!match(state, Tokentype::SYNTAX_RULE)) {
        throw ParseError("Expected 'syntax-rules' in 'define-syntax'", peek(state), "");
    }

    auto syntaxRules = parseSyntaxRules(state);
    consume(state, Tokentype::RIGHT_PAREN, "Expected ')' after 'define-syntax'");

    return std::make_shared<Expression>(
        Expression { DefineSyntaxExpression { name, syntaxRules }, name.line });
}
std::shared_ptr<Expression> parseExpression(ParserState& state)
{
    if (match(state, Tokentype::LEFT_PAREN)) {
        if (match(state, Tokentype::DEFINE))
            return parseDefine(state);
        if (match(state, Tokentype::DEFINE_SYTAX))
            return parseDefineSyntax(state);
        if (match(state, Tokentype::LET))
            return parseLet(state);
        if (match(state, Tokentype::SET))
            return parseSet(state);
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
        if (match(state, Tokentype::LEFT_PAREN))
            return parseVector(state);
    return parseAtom(state);
}

std::shared_ptr<Expression> parseAtom(ParserState& state)
{
    Token token = advance(state);
    switch (token.type) {
    case Tokentype::IDENTIFIER:
    case Tokentype::INTEGER:
    case Tokentype::ELSE:
    case Tokentype::FLOAT:
    case Tokentype::COMPLEX:
    case Tokentype::RATIONAL:
    case Tokentype::STRING:
    case Tokentype::DOT:
    case Tokentype::ELLIPSIS:
    case Tokentype::TRUE:
    case Tokentype::FALSE:
    case Tokentype::LAMBDA:
    case Tokentype::ARROW:
    case Tokentype::IF:
    case Tokentype::CHAR:
    case Tokentype::SYNTAX_RULE:
        return std::make_shared<Expression>(Expression { AtomExpression { token }, token.line });
    default:
        throw ParseError("Unexpected token in atom " + token.lexeme, token, "");
    }
}

// In parse.cpp
std::shared_ptr<Expression> parseSExpression(ParserState& state)
{
    int line = peek(state).line; // Assuming peek doesn't advance

    std::vector<std::shared_ptr<Expression>> elements;
    while (!check(state, Tokentype::RIGHT_PAREN) && !isAtEnd(state)) { // Use check before consuming ')'
        elements.push_back(parseExpression(state));
    }
    consume(state, Tokentype::RIGHT_PAREN, "Expected ')' to close S-Expression"); // Consume the ')'

    // NO TAIL EXPRESSION WRAPPING HERE!
    return std::make_shared<Expression>(
        Expression { sExpression { std::move(elements) }, line }); // Use line of opening paren maybe? Or prev token?
}

std::shared_ptr<Expression> parseDefine(ParserState& state)
{
    if (match(state, Tokentype::LEFT_PAREN)) {
        Token name = consume(state, Tokentype::IDENTIFIER, "Expected function name");
        int line = name.line; // Use function name's line for the node
        std::vector<Token> parameters;
        bool isVariadic = false;
        while (!check(state, Tokentype::RIGHT_PAREN) && !isAtEnd(state)) {
            if (match(state, Tokentype::DOT)) {
                parameters.push_back(consume(state, Tokentype::IDENTIFIER, "Expected parameter name after dot"));
                isVariadic = true;
                break;
            }
            parameters.push_back(consume(state, Tokentype::IDENTIFIER, "Expected parameter name"));
        }
        consume(state, Tokentype::RIGHT_PAREN, "Expected ')' after parameter list");
        std::vector<std::shared_ptr<Expression>> body;
        while (!check(state, Tokentype::RIGHT_PAREN) && !isAtEnd(state)) { // Use check before consuming ')'
            body.push_back(parseExpression(state));
        }
        if (!body.empty()) {
            auto lastBodyExpr = body.back();
            body.back() = std::make_shared<Expression>(
                Expression { TailExpression { lastBodyExpr }, lastBodyExpr->line }); // Wrap last body expr
        }
        consume(state, Tokentype::RIGHT_PAREN, "Expected ')' after function body");
        return std::make_shared<Expression>(Expression {
            DefineProcedure { name, std::move(parameters), std::move(body), isVariadic },
            line });

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
    std::vector<Token> parameters;
    bool isVariadic = false;

    if (match(state, Tokentype::LEFT_PAREN)) {
        while (!check(state, Tokentype::RIGHT_PAREN) && !isAtEnd(state)) {
            if (match(state, Tokentype::DOT)) {
                parameters.push_back(consume(state, Tokentype::IDENTIFIER, "Expected parameter name after dot"));
                isVariadic = true;
                break;
            }
            parameters.push_back(consume(state, Tokentype::IDENTIFIER, "Expected parameter name"));
        }
        consume(state, Tokentype::RIGHT_PAREN, "Expected ')' after parameters");
    } else {
        parameters.push_back(consume(state, Tokentype::IDENTIFIER, "Expected parameter name"));
        isVariadic = true;
    }

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
        LambdaExpression { std::move(parameters), std::move(body), isVariadic },
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
    std::optional<Token> letNameToken;
    int line = previousToken(state).line;
    if (check(state, Tokentype::IDENTIFIER)) {
        letNameToken = peek(state);
        advance(state);
        line = letNameToken->line;
    }
    consume(state, Tokentype::LEFT_PAREN, "Expected '(' after 'let' or named let identifier");

    std::vector<std::pair<Token, std::shared_ptr<Expression>>> bindings;
    if (!check(state, Tokentype::RIGHT_PAREN)) {
        while (!check(state, Tokentype::RIGHT_PAREN) && !isAtEnd(state)) {
            consume(state, Tokentype::LEFT_PAREN, "Expected '(' to start binding pair in 'let'");
            Token name = consume(state, Tokentype::IDENTIFIER, "Expected identifier for binding variable in 'let'");
            auto value = parseExpression(state); // Parse the value expression
            consume(state, Tokentype::RIGHT_PAREN, "Expected ')' to end binding pair in 'let'");
            bindings.push_back({ name, value });
        }
    }
    consume(state, Tokentype::RIGHT_PAREN, "Expected ')' after 'let' bindings");

    // Parse the body expressions
    std::vector<std::shared_ptr<Expression>> body;
    while (!check(state, Tokentype::RIGHT_PAREN) && !isAtEnd(state)) {
        body.emplace_back(parseExpression(state));
    }
    if (!body.empty()) {
        auto lastBodyExpr = body.back();
        body.back() = std::make_shared<Expression>(
            Expression { TailExpression { lastBodyExpr }, lastBodyExpr->line });
    }
    consume(state, Tokentype::RIGHT_PAREN, "Expected ')' after 'let' body");
    return std::make_shared<Expression>(
        Expression { LetExpression { letNameToken, std::move(bindings), std::move(body) }, line });
}

std::shared_ptr<Expression> parseTailExpression(ParserState& state)
{
    return std::make_shared<Expression>(
        TailExpression { parseExpression(state) },
        previousToken(state).line);
}

std::shared_ptr<Expression> parseVector(ParserState& state)
{
    std::vector<std::shared_ptr<Expression>> elements;
    while (!match(state, Tokentype::RIGHT_PAREN)) {
        elements.push_back(parseExpression(state));
    }
    return std::make_shared<Expression>(Expression {
        VectorExpression { std::move(elements) },
        previousToken(state).line });
}

std::shared_ptr<Expression> parseList(ParserState& state)
{
    std::vector<std::shared_ptr<Expression>> elements;
    bool isVariadic = false;
    while (!check(state, Tokentype::RIGHT_PAREN) && !isAtEnd(state)) {
        if (match(state, Tokentype::LEFT_PAREN)) {
            elements.push_back(parseList(state));
            consume(state, Tokentype::RIGHT_PAREN, "Expected ')' after nested list");
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

template <typename... Types>
Token consume(ParserState& state, const std::string& message, Types... types)
{
    bool matches = (... || check(state, types));
    if (matches) {
        return advance(state);
    }
    throw ParseError(message + " Token: " + previousToken(state).lexeme, peek(state), "");
}

std::shared_ptr<Expression> parseImport(ParserState& state)
{
    std::vector<ImportExpression::ImportSpec> specs;

    while (!check(state, Tokentype::RIGHT_PAREN) && !isAtEnd(state)) {
        if (match(state, Tokentype::LEFT_PAREN)) {
            if (match(state, Tokentype::ONLY)) {
                auto lib = parseLibraryName(state);
                std::vector<Token> identifiers;
                while (!check(state, Tokentype::RIGHT_PAREN)) {
                    consume(state, Tokentype::IDENTIFIER, "Expected identifier in only import");
                    identifiers.push_back(previousToken(state));
                }
                consume(state, Tokentype::RIGHT_PAREN, "Expected ')' after only import");
                specs.push_back(makeOnlyImport(lib, std::move(identifiers)));
            } else if (match(state, Tokentype::EXCEPT)) {
                auto lib = parseLibraryName(state);
                std::vector<Token> identifiers;
                while (!check(state, Tokentype::RIGHT_PAREN)) {
                    consume(state, Tokentype::IDENTIFIER, "Expected identifier in except import");
                    identifiers.push_back(previousToken(state));
                }
                consume(state, Tokentype::RIGHT_PAREN, "Expected ')' after except import");
                specs.push_back(makeExceptImport(lib, std::move(identifiers)));
            } else if (match(state, Tokentype::PREFIX)) {
                auto lib = parseLibraryName(state);
                consume(state, Tokentype::IDENTIFIER, "Expected prefix identifier");
                Token prefix = previousToken(state);
                consume(state, Tokentype::RIGHT_PAREN, "Expected ')' after prefix import");
                specs.push_back(makePrefixImport(lib, prefix));
            } else if (match(state, Tokentype::RENAME)) {
                auto lib = parseLibraryName(state);
                std::vector<std::pair<Token, Token>> renames;
                while (!check(state, Tokentype::RIGHT_PAREN)) {
                    consume(state, Tokentype::LEFT_PAREN, "Expected '(' in rename pair");
                    consume(state, Tokentype::IDENTIFIER, "Expected original identifier");
                    Token orig = previousToken(state);
                    consume(state, Tokentype::IDENTIFIER, "Expected new identifier");
                    Token newName = previousToken(state);
                    consume(state, Tokentype::RIGHT_PAREN, "Expected ')' after rename pair");
                    renames.emplace_back(orig, newName);
                }
                consume(state, Tokentype::RIGHT_PAREN, "Expected ')' after rename import");
                specs.push_back(makeRenameImport(lib, std::move(renames)));
            } else {
                auto lib = parseLibraryName(state);
                specs.push_back(makeDirectImport(lib));
            }
        } else {
            consume(state, Tokentype::IDENTIFIER, "Expected library identifier");
            std::vector<std::shared_ptr<Expression>> lib;
            lib.push_back(std::make_shared<Expression>(
                Expression { AtomExpression { previousToken(state) }, previousToken(state).line }));
            specs.push_back(makeDirectImport(std::move(lib)));
        }
    }

    consume(state, Tokentype::RIGHT_PAREN, "Expected ')' after import");
    return std::make_shared<Expression>(
        Expression { ImportExpression { std::move(specs) }, previousToken(state).line });
}

std::vector<std::shared_ptr<Expression>> parseLibraryName(ParserState& state)
{
    std::vector<std::shared_ptr<Expression>> parts;

    consume(state, Tokentype::LEFT_PAREN, "Expected '(' in library name");
    while (!check(state, Tokentype::RIGHT_PAREN)) {
        if (match(state, Tokentype::IDENTIFIER) || match(state, Tokentype::INTEGER)) {
            parts.push_back(std::make_shared<Expression>(
                Expression { AtomExpression { previousToken(state) }, previousToken(state).line }));
        } else {
            error(state, "Expected identifier or version number in library name");
        }
    }
    consume(state, Tokentype::RIGHT_PAREN, "Expected ')' after library name");

    return parts;
}

void synchronize(ParserState& state)
{
    advance(state);

    while (!isAtEnd(state)) {
        if (previousToken(state).type == Tokentype::LEFT_PAREN && (check(state, Tokentype::DEFINE) || check(state, Tokentype::LET) || check(state, Tokentype::LAMBDA) || check(state, Tokentype::IF) || check(state, Tokentype::QUOTE))) {
            return;
        }
        advance(state);
    }
}

std::optional<std::vector<std::shared_ptr<Expression>>> parse(std::vector<Token> tokens)
{
    ParserState state { std::move(tokens), 0, false };
    auto output = std::vector<std::shared_ptr<Expression>> {};
    bool hadError = false;

    while (!isAtEnd(state)) {
        try {
            auto expr = parseExpression(state);
            if (expr) {
                output.push_back(expr);
            }
            state.panicMode = false;
        } catch (const ParseError& e) {
            e.printFormattedError();
            hadError = true;
            synchronize(state);
            state.panicMode = false;
        }
    }

    return hadError ? std::nullopt : std::make_optional(output);
}
std::shared_ptr<Expression> parseSyntaxRules(ParserState& state)
{
    consume(state, Tokentype::LEFT_PAREN, "Expected '(' before literals in 'syntax-rules'");
    std::vector<Token> literals;
    while (!match(state, Tokentype::RIGHT_PAREN)) {
        literals.push_back(advance(state));
    }
    std::vector<SyntaxRule> rules;
    while (!check(state, Tokentype::RIGHT_PAREN) && !isAtEnd(state)) {
        consume(state, Tokentype::LEFT_PAREN, "Expected '(' before pattern in 'syntax-rules'");
        // Use the same macro expression parser for both pattern and template
        auto pattern = parseMacroExpression(state);
        auto template_expr = parseMacroExpression(state);
        consume(state, Tokentype::RIGHT_PAREN, "Expected ')' after template in 'syntax-rules'");
        rules.push_back(SyntaxRule(pattern, template_expr));
    }

    consume(state, Tokentype::RIGHT_PAREN, "Expected ')' after 'syntax-rules'");

    return std::make_shared<Expression>(
        Expression { SyntaxRulesExpression { literals, std::move(rules) }, previousToken(state).line });
}

std::shared_ptr<Expression> parseMacroExpression(ParserState& state)
{
    if (match(state, Tokentype::LEFT_PAREN)) {
        return parseMacroSExpression(state);
    }
    if (match(state, Tokentype::QUOTE)) {
        auto expr = parseMacroExpression(state);
        return std::make_shared<Expression>(
            Expression { QuoteExpression { expr }, previousToken(state).line });
    }
    if (match(state, Tokentype::HASH)) {
        if (match(state, Tokentype::LEFT_PAREN)) {
            std::vector<std::shared_ptr<Expression>> elements;
            while (!match(state, Tokentype::RIGHT_PAREN)) {
                elements.push_back(parseMacroExpression(state));
            }
            return std::make_shared<Expression>(Expression {
                VectorExpression { std::move(elements) },
                previousToken(state).line });
        }
        throw ParseError("Expected list when defining vector", previousToken(state), "");
    }

    Token token = advance(state);
    return std::make_shared<Expression>(Expression { AtomExpression { token }, token.line });
}

std::shared_ptr<Expression> parseMacroSExpression(ParserState& state)
{
    std::vector<std::shared_ptr<Expression>> elements;
    while (!match(state, Tokentype::RIGHT_PAREN) && !isAtEnd(state)) {
        elements.push_back(parseMacroExpression(state));
    }
    return std::make_shared<Expression>(
        Expression { ListExpression { std::move(elements) }, previousToken(state).line });
}
}
