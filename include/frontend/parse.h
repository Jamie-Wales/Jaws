#pragma once
#include "Error.h"
#include "Expression.h"
#include "Token.h"
#include <memory>
#include <optional>
#include <vector>

namespace parse {

struct ParserState {
    std::vector<Token> tokens;
    size_t current = 0;
    bool panicMode = false;
};

std::optional<std::vector<std::shared_ptr<Expression>>> parse(std::vector<Token> tokens);

std::shared_ptr<Expression> parseDefineLibrary(ParserState& state);
std::shared_ptr<Expression> parseExpression(ParserState& state);
std::shared_ptr<Expression> parseAtom(ParserState& state);
std::shared_ptr<Expression> parseSExpression(ParserState& state);
std::shared_ptr<Expression> parseSet(ParserState& state);
std::shared_ptr<Expression> parseQuoted(ParserState& state);
std::shared_ptr<Expression> parseLambda(ParserState& state);
std::shared_ptr<Expression> parseIf(ParserState& state);
std::shared_ptr<Expression> parseDefine(ParserState& state);
std::shared_ptr<Expression> parseLet(ParserState& state);
std::shared_ptr<Expression> parseLetRec(ParserState& state);
std::shared_ptr<Expression> parseVector(ParserState& state);
std::shared_ptr<Expression> parseImport(ParserState& state);
std::shared_ptr<Expression> parseDefineSyntax(ParserState& state);
std::shared_ptr<Expression> parseSyntaxRules(ParserState& state);
std::shared_ptr<Expression> parseList(ParserState& state);
std::shared_ptr<Expression> parseMacroExpression(ParserState& state);
std::shared_ptr<Expression> parseMacroSExpression(ParserState& state);

std::vector<std::shared_ptr<Expression>> parseLibraryName(ParserState& state, bool t = true);
std::shared_ptr<Expression> parseCond(ParserState& state);
std::shared_ptr<Expression> parseTailExpression(ParserState& state);

Token peek(const ParserState& state);
Token peek(const ParserState& state, int ahead);
bool isAtEnd(const ParserState& state);
Token advance(ParserState& state);
Token previousToken(const ParserState& state);
bool check(const ParserState& state, Tokentype type);
bool match(ParserState& state, Tokentype type);
Token consume(ParserState& state, Tokentype type, const std::string& message);
void error(ParserState& state, const std::string& message);
void errorAt(ParserState& state, const Token& token, const std::string& message);

} // namespace parse
