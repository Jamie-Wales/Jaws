#include "Scanner.h"
#include "Token.h"

const std::unordered_map<std::string, Tokentype> Scanner::keywords = {
    { "define", Tokentype::DEFINE },
    { "lambda", Tokentype::LAMBDA },
    { "if", Tokentype::IF }
};

const std::vector<Scanner::RegexInfo> Scanner::regexList = {
    { std::regex(R"(;.*)"), Tokentype::COMMENT },
    { std::regex(R"("(?:[^"\\]|\\.)*")"), Tokentype::STRING },
    { std::regex(R"(\d+\.\d*)"), Tokentype::FLOAT },
    { std::regex(R"(\d+)"), Tokentype::INTEGER },
    { std::regex(R"('[a-zA-Z_][a-zA-Z0-9_]*)"), Tokentype::QUOTE },
    { std::regex(R"([a-zA-Z_][a-zA-Z0-9_]*)"), Tokentype::SYMBOL },
    { std::regex(R"([ \t\n\r]+)"), Tokentype::WHITESPACE }
};

const std::vector<std::pair<std::string, Tokentype>> Scanner::operators = {
    { "(", Tokentype::LEFT_PAREN },
    { ")", Tokentype::RIGHT_PAREN },
    { "+", Tokentype::PLUS },
    { "-", Tokentype::MINUS },
    { "*", Tokentype::MULTIPLY },
    { "/", Tokentype::DIVIDE },
    { "=", Tokentype::EQUAL },
    { "<", Tokentype::LESS_THAN },
    { ">", Tokentype::GREATER_THAN }
};

std::vector<Token> Scanner::tokenize(const std::string& input)
{
    std::vector<Token> allTokens;
    auto it = input.begin();
    const auto end = input.end();
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

std::optional<Token> Scanner::matchToken(std::string::const_iterator& it) const
{
    for (const auto& [op, type] : operators) {
        if (std::string(it, std::min(it + op.length(), input.end())) == op) {
            auto token = Token { type, op, line, row };
            it += op.length();
            return token;
        }
    }

    // Then check regex patterns
    for (const auto& regex_info : regexList) {
        std::smatch match;
        if (std::regex_search(it, input.end(), match, regex_info.regex, std::regex_constants::match_continuous)) {
            const std::string lexeme = match.str();
            it += lexeme.length();
            Tokentype type = regex_info.type;
            if (type == Tokentype::SYMBOL) {
                if (const auto keywordIt = keywords.find(lexeme); keywordIt != keywords.end()) {
                    type = keywordIt->second;
                }
            }
            return Token { type, lexeme, line, row };
        }
    }
    return std::nullopt;
}

void Scanner::updatePosition(const std::string& lexeme)
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
