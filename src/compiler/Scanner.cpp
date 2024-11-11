#include "Scanner.h"
#include "Error.h"

const std::unordered_map<std::string, Tokentype> Scanner::keywords = {
    { "define", Tokentype::DEFINE },
    { "lambda", Tokentype::LAMBDA },
    { "if", Tokentype::IF },
    { "quote", Tokentype::QUOTE },
    { "import", Tokentype::IMPORT },
};
const std::vector<Scanner::RegexInfo> Scanner::regexPatterns = {
    { std::regex(R"(;.*)"), Tokentype::COMMENT },
    { std::regex(R"("(?:[^"\\]|\\.)*")"), Tokentype::STRING },
    { std::regex(R"([+-]?(?:\d+(?:\.\d*)?|\.\d+)(?:(?:[+-](?:\d+(?:\.\d*)?|\.\d+))?i|@(?:\d+(?:\.\d*)?|\.\d+))|[+-]i)"), Tokentype::COMPLEX },
    { std::regex(R"([+-]?\d+/\d+)"), Tokentype::RATIONAL },
    { std::regex(R"([+-]?(?:\d+\.\d*|\.\d+)(?:[eE][+-]?\d+)?)"), Tokentype::FLOAT },
    { std::regex(R"([+-]?\d+)"), Tokentype::INTEGER },
    { std::regex(R"(#t|#true)"), Tokentype::TRUE },
    { std::regex(R"(#f|#false)"), Tokentype::FALSE },
    { std::regex(R"([a-zA-Z!$%&*+\-\./:<=>?@^_~][a-zA-Z0-9!$%&*+\-\./:<=>?@^_~]*)"), Tokentype::IDENTIFIER },
    { std::regex(R"(\|(?:[^\\|]|\\.)*\|)"), Tokentype::IDENTIFIER },
    { std::regex(R"(')"), Tokentype::QUOTE },
    { std::regex(R"(\()"), Tokentype::LEFT_PAREN },
    { std::regex(R"(\))"), Tokentype::RIGHT_PAREN },
    { std::regex(R"(\#)"), Tokentype::HASH },
    { std::regex(R"([ \t\n\r]+)"), Tokentype::WHITESPACE }
};

std::vector<Token> Scanner::tokenize(const std::string& input)
{
    this->input = input;
    this->lines.clear();
    this->lines.push_back("");
    return generateTokens();
}

std::string Scanner::getLine(int lineNumber) const
{
    if (lineNumber > 0 && lineNumber <= static_cast<int>(lines.size())) {
        return lines[lineNumber - 1];
    }
    return "";
}


std::vector<Token> Scanner::generateTokens()
{
    std::vector<Token> tokens;
    position = 0;
    currentLine = 1;
    column = 1;

    while (position < input.length()) {
        if (auto token = matchToken()) {
            if (token->type != Tokentype::WHITESPACE && token->type != Tokentype::COMMENT) {
                tokens.push_back(*token);
            }
            updatePosition(token->lexeme);
        } else {
            throw ParseError("Unexpected character",
                Token { Tokentype::ERROR, std::string(1, input[position]), currentLine, column },
                lines[currentLine - 1]);
        }
    }
    tokens.emplace_back(Tokentype::EOF_TOKEN, "", currentLine, column);
    return tokens;
}

std::optional<Token> Scanner::matchToken()
{
    std::string remaining = input.substr(position);
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
            return Token { type, lexeme, currentLine, column };
        }
    }
    return std::nullopt;
}

void Scanner::updatePosition(const std::string& lexeme)
{
    for (char c : lexeme) {
        if (c == '\n') {
            ++currentLine;
            lines.push_back("");
            column = 1;
        } else {
            lines[currentLine - 1] += c;
            ++column;
        }
    }
    position += lexeme.length();
}
