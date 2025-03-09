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
        while (!check(state, Tokentype::RIGHT_PAREN)) {
            body.push_back(parseExpression(state));
        }
        consume(state, Tokentype::RIGHT_PAREN, "Expected ')' after function body");

        return std::make_shared<Expression>(Expression {
            DefineProcedure { name, std::move(parameters), std::move(body), isVariadic },
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
    LetExpression::Args output;
    std::optional<Token> token;
    if (match(state, Tokentype::IDENTIFIER)) {
        token = previousToken(state);
    }

    consume(state, Tokentype::LEFT_PAREN, "Expected '(' after let");
    if (match(state, Tokentype::RIGHT_PAREN)) {
        std::vector<std::shared_ptr<Expression>> body;
        while (!match(state, Tokentype::RIGHT_PAREN)) {
            body.emplace_back(parseExpression(state));
        }
        return std::make_shared<Expression>(
            LetExpression { token, output, body },
            previousToken(state).line);
    }
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
