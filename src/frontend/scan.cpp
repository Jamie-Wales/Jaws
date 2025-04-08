#include "scan.h"
#include "Error.h"
#include "Token.h"
namespace scanner {

const std::unordered_map<std::string, Tokentype> keywords = {
    { "define-syntax", Tokentype::DEFINE_SYTAX },
    { "define-library", Tokentype::DEFINE_LIBRARY },
    { "define", Tokentype::DEFINE },
    { "lambda", Tokentype::LAMBDA },
    { "λ", Tokentype::LAMBDA },
    { "if", Tokentype::IF },
    { "quote", Tokentype::QUOTE },
    { "only", Tokentype::ONLY },
    { "except", Tokentype::EXCEPT },
    { "prefix", Tokentype::PREFIX },
    { "rename", Tokentype::RENAME },
    { "letrec", Tokentype::IDENTIFIER },
    { "let", Tokentype::LET },
    { "import", Tokentype::IMPORT },
    { "syntax-rules", Tokentype::SYNTAX_RULE },
    { "=>", Tokentype::ARROW },
    { "...", Tokentype::ELLIPSIS },
    { "set!", Tokentype::SET },
};

const std::vector<RegexInfo> regexPatterns = {
    { std::regex(R"(;.*)"), Tokentype::COMMENT },
    { std::regex(R"("(?:[^"\\]|\\.)*")"), Tokentype::STRING },
    { std::regex(R"(#\\(space|newline|tab|return|[a-zA-Z0-9]))"), Tokentype::CHAR },
    { std::regex(R"([+-]?(?:\d+(?:\.\d*)?|\.\d+)(?:(?:[+-](?:\d+(?:\.\d*)?|\.\d+))?i|@(?:\d+(?:\.\d*)?|\.\d+))|[+-]i)"), Tokentype::COMPLEX },
    { std::regex(R"([+-]?\d+/\d+)"), Tokentype::RATIONAL },
    { std::regex(R"([+-]?(?:\d+\.\d*|\.\d+)(?:[eE][+-]?\d+)?)"), Tokentype::FLOAT },
    { std::regex(R"([+-]?\d+)"), Tokentype::INTEGER },
    { std::regex(R"(#t|#true)"), Tokentype::TRUE },
    { std::regex(R"(#f|#false)"), Tokentype::FALSE },
    { std::regex(R"( \. )"), Tokentype::DOT },
    { std::regex(R"([a-zA-Z!$%&*+\-\./:<=>?@^_~][a-zA-Z0-9!$%&*+\-\./:<=>?@^_~]*)"), Tokentype::IDENTIFIER },
    { std::regex(R"(\|(?:[^\\|]|\\.)*\|)"), Tokentype::IDENTIFIER },
    { std::regex(R"('|quote)"), Tokentype::QUOTE },
    { std::regex(R"(\(|\[|\{)"), Tokentype::LEFT_PAREN },
    { std::regex(R"(\)|\]|\})"), Tokentype::RIGHT_PAREN },
    { std::regex(R"(\#)"), Tokentype::HASH },
    { std::regex(R"([ \t\n\r]+)"), Tokentype::WHITESPACE }
};

std::pair<ScanState, std::optional<Token>> matchNextToken(ScanState state)
{
    std::string remaining = state.input.substr(state.position);

    for (const auto& pattern : regexPatterns) {
        std::smatch match;
        if (std::regex_search(remaining, match, pattern.regex, std::regex_constants::match_continuous)) {
            std::string lexeme = match.str();
            Tokentype type = pattern.type;

            if (type == Tokentype::IDENTIFIER) {
                auto it = keywords.find(lexeme);
                if (it != keywords.end()) {
                    type = it->second;
                }
            }

            Token token { type, lexeme, state.currentLine, state.column };
            return { state, std::make_optional(token) };
        }
    }

    return { state, std::nullopt };
}

ScanState updateScanPosition(ScanState state, const std::string& lexeme)
{
    for (char c : lexeme) {
        if (c == '\n') {
            state.currentLine++;
            state.lines.push_back("");
            state.column = 1;
        } else {
            if (state.currentLine > 0 && state.currentLine <= static_cast<int>(state.lines.size())) {
                state.lines[state.currentLine - 1] += c;
            }
            state.column++;
        }
    }
    state.position += lexeme.length();

    return state;
}

std::vector<Token> tokenize(const std::string& input)
{
    std::vector<Token> tokens;
    ScanState state(input);

    while (state.position < state.input.length()) {
        auto [newState, token] = matchNextToken(state);

        if (!token) {
            throw ParseError(
                "Unexpected character",
                Token { Tokentype::ERROR, std::string(1, state.input[state.position]),
                    state.currentLine, state.column },
                state.lines[state.currentLine - 1]);
        }

        if (token->type != Tokentype::WHITESPACE && token->type != Tokentype::COMMENT) {
            tokens.push_back(*token);
        }

        state = updateScanPosition(newState, token->lexeme);
    }

    tokens.emplace_back(Tokentype::EOF_TOKEN, "", state.currentLine, state.column);
    return tokens;
}

std::string getLine(const std::vector<std::string>& lines, int lineNumber)
{
    if (lineNumber > 0 && lineNumber <= static_cast<int>(lines.size())) {
        return lines[lineNumber - 1];
    }
    return "";
}

}
