#include "Scanner.h"
#include <sstream>
#include <stdexcept>
#include <string>

const std::unordered_map<std::string, Tokentype> Scanner::keywords = {
    { "define", Tokentype::DEFINE },
    { "lambda", Tokentype::LAMBDA },
    { "if", Tokentype::IF },
    { "boolean?", Tokentype::IDENTIFIER }
};
const std::vector<Scanner::RegexInfo> Scanner::regexPatterns = {
    { std::regex(R"(;.*)"), Tokentype::COMMENT },
    { std::regex(R"("(?:[^"\\]|\\.)*")"), Tokentype::STRING },
    { std::regex(R"(\d+\.\d*)"), Tokentype::FLOAT },
    { std::regex(R"(\d+)"), Tokentype::INTEGER },
    { std::regex(R"([\+\-\*/=<>][\+\-\*/=<>]*)"), Tokentype::SYMBOL },
    { std::regex(R"(#t|#true)"), Tokentype::TRUE },
    { std::regex(R"(#f|#false)"), Tokentype::FALSE },
    { std::regex(R"([a-zA-Z_][a-zA-Z0-9_+\-?!]*)"), Tokentype::IDENTIFIER },
    { std::regex(R"(')"), Tokentype::QUOTE },
    { std::regex(R"([ \t\n\r]+)"), Tokentype::WHITESPACE },
    { std::regex(R"(\()"), Tokentype::LEFT_PAREN },
    { std::regex(R"(\))"), Tokentype::RIGHT_PAREN }
};

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
            return Token { type, lexeme, line, column };
        }
    }

    return std::nullopt;
}
std::vector<Token> Scanner::tokenize(const std::string& input)
{
    this->input = input;
    splitIntoLines();
    std::vector<Token> tokens;
    position = 0;
    line = 1;
    column = 1;

    while (position < input.length()) {
        if (auto token = matchToken()) {
            if (token->type != Tokentype::WHITESPACE && token->type != Tokentype::COMMENT) {
                tokens.push_back(*token);
            }
            updatePosition(token->lexeme);
        } else {
            throw std::runtime_error("Unexpected character at line " + std::to_string(line) + ", column " + std::to_string(column) + ": " + std::string(1, input[position]));
        }
    }

    tokens.emplace_back(Tokentype::EOF_TOKEN, "", line, column);
    return tokens;
}

void Scanner::updatePosition(const std::string& lexeme)
{
    for (char c : lexeme) {
        if (c == '\n') {
            ++line;
            column = 1;
        } else {
            ++column;
        }
    }
    position += lexeme.length();
}

void Scanner::splitIntoLines()
{
    lines.clear();
    std::istringstream stream(input);
    std::string line;
    while (std::getline(stream, line)) {
        lines.push_back(line);
    }
}

std::string Scanner::getLine(int lineNumber) const
{
    if (lineNumber > 0 && lineNumber <= static_cast<int>(lines.size())) {
        return lines[lineNumber - 1];
    }
    return "";
}
