#include "Scanner.h"
#include <sstream>
#include <stdexcept>
#include <string>

const std::array<Scanner::RegexInfo, Scanner::regexPatterns.size()> Scanner::compiledRegexes = []() {
    std::array<RegexInfo, regexPatterns.size()> result;
    for (size_t i = 0; i < regexPatterns.size(); ++i) {
        result[i] = { std::regex(regexPatterns[i].first), regexPatterns[i].second };
    }
    return result;
}();

std::string Scanner::getLine(int lineNumber) const
{
    if (lineNumber > 0 && lineNumber <= static_cast<int>(lines.size())) {
        return lines[lineNumber - 1];
    }
    return "";
}

void Scanner::splitIntoLines(std::string_view input)
{
    std::istringstream stream((std::string(input)));
    std::string line;
    while (std::getline(stream, line)) {
        lines.push_back(line);
    }
}

std::vector<Token> Scanner::tokenize(const std::string& input)
{
    this->input = input;
    splitIntoLines(this->input);
    std::vector<Token> allTokens;
    auto it = this->input.begin();
    const auto end = this->input.end();
    line = 1;
    row = 1;

    while (it != end) {
        if (auto token = matchToken(it)) {
            if (token->type != Tokentype::WHITESPACE && token->type != Tokentype::COMMENT) {
                allTokens.push_back(std::move(*token));
            }
            updatePosition(token->lexeme);
        } else {
            throw std::runtime_error("Unexpected character at line " + std::to_string(line) + ", column " + std::to_string(row) + ": " + std::string(1, *it));
        }
    }
    allTokens.emplace_back(Tokentype::EOF_TOKEN, "", line, row);
    return allTokens;
}

std::optional<Token> Scanner::matchToken(std::string_view::const_iterator& it) const
{
    for (const auto& [token, type] : specialTokens) {
        if (std::string_view(&*it, std::min<size_t>(std::strlen(token), std::distance(it, input.end()))) == token) {
            auto newToken = Token { type, std::string(token), line, row };
            it += std::strlen(token);
            return newToken;
        }
    }

    for (const auto& regex_info : compiledRegexes) {
        std::match_results<std::string_view::const_iterator> match;
        if (std::regex_search(it, input.end(), match, regex_info.regex, std::regex_constants::match_continuous)) {
            std::string_view lexeme(&*match[0].first, match[0].length());
            it += lexeme.length();
            Tokentype type = regex_info.type;
            if (type == Tokentype::SYMBOL) {
                for (const auto& [keyword, keywordType] : keywords) {
                    if (lexeme == keyword) {
                        type = keywordType;
                        break;
                    }
                }
            }
            return Token { type, std::string(lexeme), line, row };
        }
    }
    return std::nullopt;
}

void Scanner::updatePosition(std::string_view lexeme)
{
    for (char c : lexeme) {
        if (c == '\n') {
            ++line;
            row = 1;
        } else {
            ++row;
        }
    }
}
