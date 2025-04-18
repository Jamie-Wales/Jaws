#include "parse.h"
#include "Token.h"
#include <iterator> // For std::make_move_iterator
#include <memory>
#include <optional>
#include <utility> // For std::move, std::pair
#include <vector>

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

template <typename... Types>
Token consume(ParserState& state, const std::string& message, Types... types)
{
    bool matches = (... || check(state, types));
    if (matches) {
        return advance(state);
    }
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
std::shared_ptr<Expression> parseBegin(ParserState& state)
{
    std::vector<std::shared_ptr<Expression>> body;
    while (!check(state, Tokentype::RIGHT_PAREN)) {
        body.push_back(parseExpression(state));
    }
    consume(state, Tokentype::RIGHT_PAREN, "Expected ')' after begin body");
    return std::make_shared<Expression>(Expression { BeginExpression { body }, previousToken(state).line });
}

std::shared_ptr<Expression> parseExpression(ParserState& state)
{
    if (match(state, Tokentype::LEFT_PAREN)) {

        if (match(state, Tokentype::BEGIN))
            return parseBegin(state);
        if (match(state, Tokentype::DEFINE))
            return parseDefine(state);
        if (match(state, Tokentype::DEFINE_SYTAX))
            return parseDefineSyntax(state);
        if (match(state, Tokentype::DEFINE_LIBRARY)) {
            return parseDefineLibrary(state);
        }
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
    int line = peek(state).line;

    std::vector<std::shared_ptr<Expression>> elements;
    while (!check(state, Tokentype::RIGHT_PAREN) && !isAtEnd(state)) {
        elements.push_back(parseExpression(state));
    }
    consume(state, Tokentype::RIGHT_PAREN, "Expected ')' to close S-Expression");

    return std::make_shared<Expression>(
        Expression { sExpression { std::move(elements) }, line });
}

std::shared_ptr<Expression> parseDefine(ParserState& state)
{
    if (match(state, Tokentype::LEFT_PAREN)) {
        Token name = consume(state, Tokentype::IDENTIFIER, "Expected function name");
        int line = name.line;
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
        while (!check(state, Tokentype::RIGHT_PAREN) && !isAtEnd(state)) {
            body.push_back(parseExpression(state));
        }
        if (!body.empty()) {
            auto lastBodyExpr = body.back();
            body.back() = std::make_shared<Expression>(
                Expression { TailExpression { lastBodyExpr }, lastBodyExpr->line });
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
            auto value = parseExpression(state);
            consume(state, Tokentype::RIGHT_PAREN, "Expected ')' to end binding pair in 'let'");
            bindings.push_back({ name, value });
        }
    }
    consume(state, Tokentype::RIGHT_PAREN, "Expected ')' after 'let' bindings");

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
    bool isVariadic = false; // This seems unused based on the code, might need revisiting
    while (!check(state, Tokentype::RIGHT_PAREN) && !isAtEnd(state)) {
        if (match(state, Tokentype::LEFT_PAREN)) {
            elements.push_back(parseList(state)); // Assuming parseList is meant for general list structures
            consume(state, Tokentype::RIGHT_PAREN, "Expected ')' after nested list");
        } else {
            Token token = advance(state);
            elements.push_back(std::make_shared<Expression>(
                Expression { AtomExpression { token }, token.line }));
        }
    }
    // Missing consumption of RIGHT_PAREN here? If called from LEFT_PAREN match
    return std::make_shared<Expression>(
        ListExpression { std::move(elements), isVariadic }, // Changed to ListExpression based on name
        previousToken(state).line);
}

std::vector<std::shared_ptr<Expression>> parseLibraryName(ParserState& state, bool expectOpeningParen)
{
    std::vector<std::shared_ptr<Expression>> parts;

    if (expectOpeningParen) {
        consume(state, Tokentype::LEFT_PAREN, "Expected '(' to start library name");
    }

    while (!check(state, Tokentype::RIGHT_PAREN) && !isAtEnd(state)) {
        if (check(state, Tokentype::IDENTIFIER) || check(state, Tokentype::INTEGER)) {
            advance(state);
            parts.push_back(std::make_shared<Expression>(
                Expression { AtomExpression { previousToken(state) }, previousToken(state).line }));
        } else {
            errorAt(state, peek(state), "Expected identifier or integer in library name part");
            break;
        }
    }
    consume(state, Tokentype::RIGHT_PAREN, "Expected ')' to end library name");

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
        auto pattern = parseMacroExpression(state);
        auto template_expr = parseMacroExpression(state);
        consume(state, Tokentype::RIGHT_PAREN, "Expected ')' after template in 'syntax-rules'");

        SyntaxRule rule(pattern, template_expr);
        rule.analyzePattern(literals); // Analyze and record pattern variables
        rules.push_back(std::move(rule));
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

std::vector<ImportExpression::ImportSpec> parseImportSpecifications(ParserState& state)
{
    std::vector<ImportExpression::ImportSpec> specs;

    while (!check(state, Tokentype::RIGHT_PAREN) && !isAtEnd(state)) {
        if (!check(state, Tokentype::LEFT_PAREN)) {
            errorAt(state, peek(state), "Expected '(' to start import set or library name");
            if (!isAtEnd(state))
                advance(state);
            continue;
        }
        advance(state); // Consume '(' of the import set

        if (match(state, Tokentype::ONLY)) {
            auto lib = parseLibraryName(state, false);
            std::vector<Token> identifiers;
            while (!check(state, Tokentype::RIGHT_PAREN)) {
                identifiers.push_back(consume(state, Tokentype::IDENTIFIER, "Expected identifier in only import"));
            }
            consume(state, Tokentype::RIGHT_PAREN, "Expected ')' after only import set identifiers");
            specs.push_back(makeOnlyImport(lib, std::move(identifiers)));

        } else if (match(state, Tokentype::EXCEPT)) {
            auto lib = parseLibraryName(state, false);
            std::vector<Token> identifiers;
            while (!check(state, Tokentype::RIGHT_PAREN)) {
                identifiers.push_back(consume(state, Tokentype::IDENTIFIER, "Expected identifier in except import"));
            }
            consume(state, Tokentype::RIGHT_PAREN, "Expected ')' after except import set identifiers");
            specs.push_back(makeExceptImport(lib, std::move(identifiers)));

        } else if (match(state, Tokentype::PREFIX)) {
            auto lib = parseLibraryName(state, false);
            Token prefix = consume(state, Tokentype::IDENTIFIER, "Expected prefix identifier");
            consume(state, Tokentype::RIGHT_PAREN, "Expected ')' after prefix identifier");
            specs.push_back(makePrefixImport(lib, prefix));

        } else if (match(state, Tokentype::RENAME)) {
            auto lib = parseLibraryName(state, false);
            std::vector<std::pair<Token, Token>> renames;
            while (!check(state, Tokentype::RIGHT_PAREN)) {
                consume(state, Tokentype::LEFT_PAREN, "Expected '(' for rename pair");
                Token orig = consume(state, Tokentype::IDENTIFIER, "Expected original identifier in rename");
                Token newName = consume(state, Tokentype::IDENTIFIER, "Expected new identifier in rename");
                consume(state, Tokentype::RIGHT_PAREN, "Expected ')' after rename pair");
                renames.emplace_back(orig, newName);
            }
            consume(state, Tokentype::RIGHT_PAREN, "Expected ')' after rename pairs");
            specs.push_back(makeRenameImport(lib, std::move(renames)));

        } else {
            auto lib = parseLibraryName(state, false);
            specs.push_back(makeDirectImport(lib));
        }
    }
    return specs;
}

bool isExportableNameToken(Tokentype type)
{
    switch (type) {
    case Tokentype::IDENTIFIER:
    case Tokentype::EXPORT:
    case Tokentype::BEGIN:
    case Tokentype::COND:
    case Tokentype::SET:
    case Tokentype::IMPORT:
    case Tokentype::DEFINE:
    case Tokentype::LAMBDA:
    case Tokentype::IF:
    case Tokentype::DEFINE_SYTAX:
    case Tokentype::DEFINE_LIBRARY:
    case Tokentype::LET:
    case Tokentype::SYNTAX_RULE:
    case Tokentype::ARROW:
    case Tokentype::ELLIPSIS:
    case Tokentype::ONLY:
    case Tokentype::EXCEPT:
    case Tokentype::PREFIX:
    case Tokentype::RENAME:
        return true;

    case Tokentype::LEFT_PAREN:
    case Tokentype::RIGHT_PAREN:
    case Tokentype::DOT:
    case Tokentype::QUOTE:
    case Tokentype::BACKQUOTE:
    case Tokentype::COMMA:
    case Tokentype::COMMA_AT:
    case Tokentype::HASH:
    case Tokentype::INTEGER:
    case Tokentype::FLOAT:
    case Tokentype::STRING:
    case Tokentype::CHAR:
    case Tokentype::RATIONAL:
    case Tokentype::COMPLEX:
    case Tokentype::TRUE:
    case Tokentype::FALSE:
    case Tokentype::WHITESPACE:
    case Tokentype::COMMENT:
    case Tokentype::EOF_TOKEN:
    case Tokentype::ERROR:
    default:
        return false;
    }
}

std::shared_ptr<Expression> parseDefineLibrary(ParserState& state)
{
    int line = previousToken(state).line;

    // Parse library name
    std::vector<std::shared_ptr<Expression>> libraryName = parseLibraryName(state);
    std::vector<HygienicSyntax> exports;
    std::vector<ImportExpression::ImportSpec> imports;
    std::vector<std::shared_ptr<Expression>> body;

    // Main loop to process export, import, and begin clauses
    while (!check(state, Tokentype::RIGHT_PAREN) && !isAtEnd(state)) {
        if (!check(state, Tokentype::LEFT_PAREN)) {
            errorAt(state, peek(state), "Expected '(' to start library declaration clause (export, import, or begin)");
            if (!isAtEnd(state))
                advance(state);
            continue;
        }

        advance(state); // Consume the '('

        if (match(state, Tokentype::EXPORT)) {
            // Improved export parsing
            while (!check(state, Tokentype::RIGHT_PAREN) && !isAtEnd(state)) {
                if (check(state, Tokentype::LEFT_PAREN)) {
                    // Handle complex export forms like (rename ...) if needed
                    errorAt(state, peek(state), "Complex export forms not yet supported");
                    advance(state);
                    int parenLevel = 1;
                    while (!isAtEnd(state) && parenLevel > 0) {
                        if (peek(state).type == Tokentype::LEFT_PAREN)
                            parenLevel++;
                        else if (peek(state).type == Tokentype::RIGHT_PAREN)
                            parenLevel--;
                        advance(state);
                    }
                } else if (isExportableNameToken(peek(state).type)) {
                    Token id = advance(state); // Consume the valid name token
                    exports.push_back(HygienicSyntax { id, {} });
                } else {
                    errorAt(state, peek(state), "Expected an identifier or exportable keyword in export list");
                    if (!isAtEnd(state))
                        advance(state);
                }
            }
            consume(state, Tokentype::RIGHT_PAREN, "Expected ')' after export list");

        } else if (match(state, Tokentype::IMPORT)) {
            std::vector<ImportExpression::ImportSpec> clauseImports = parseImportSpecifications(state);
            imports.insert(imports.end(),
                std::make_move_iterator(clauseImports.begin()),
                std::make_move_iterator(clauseImports.end()));
            consume(state, Tokentype::RIGHT_PAREN, "Expected ')' after import declaration clause");

        } else if (match(state, Tokentype::BEGIN)) {
            // NO advance here - BEGIN token was already consumed by match()

            // Parse expressions in the begin block
            while (!check(state, Tokentype::RIGHT_PAREN) && !isAtEnd(state)) {
                body.push_back(parseExpression(state));
            }

            consume(state, Tokentype::RIGHT_PAREN, "Expected ')' after begin block");
        } else {
            errorAt(state, peek(state), "Expected keyword 'export', 'import', or 'begin' after '(' in define-library");
            int parenLevel = 1;
            while (!isAtEnd(state) && parenLevel > 0) {
                if (peek(state).type == Tokentype::LEFT_PAREN)
                    parenLevel++;
                else if (peek(state).type == Tokentype::RIGHT_PAREN)
                    parenLevel--;
                advance(state);
            }
        }
    }

    consume(state, Tokentype::RIGHT_PAREN, "Expected ')' to close define-library");

    return std::make_shared<Expression>(
        DefineLibraryExpression {
            std::move(libraryName),
            std::move(exports),
            std::move(imports),
            std::move(body) },
        line);
}

std::shared_ptr<Expression> parseImport(ParserState& state)
{
    int line = previousToken(state).line;

    std::vector<ImportExpression::ImportSpec> specs = parseImportSpecifications(state);

    consume(state, Tokentype::RIGHT_PAREN, "Expected ')' after top-level import form");
    return std::make_shared<Expression>(
        Expression { ImportExpression { std::move(specs) }, line });
}

} // namespace parse
