#pragma once
#include "Token.h"
#include <string>
#include <vector>
#include <unordered_map>
#include <regex>
#include <optional>
#include <array>

class Scanner {
public:
    Scanner() = default;
    std::vector<Token> tokenize(const std::string& input);
    std::string getLine(int lineNumber) const;

private:
    struct RegexInfo {
        std::regex regex;
        Tokentype type;
    };

    static constexpr std::array<std::pair<const char*, Tokentype>, 3> keywords = {{
        { "define", Tokentype::DEFINE },
        { "lambda", Tokentype::LAMBDA },
        { "if", Tokentype::IF }
    }};

    static constexpr std::array<std::pair<const char*, Tokentype>, 7> regexPatterns = {{
        { R"(;.*)", Tokentype::COMMENT },
        { R"("(?:[^"\\]|\\.)*")", Tokentype::STRING },
        { R"(\d+\.\d*)", Tokentype::FLOAT },
        { R"(\d+)", Tokentype::INTEGER },
        { R"('[a-zA-Z_][a-zA-Z0-9_]*)", Tokentype::QUOTE },
        { R"([a-zA-Z_+\-*/=<>][a-zA-Z0-9_+\-*/=<>]*)", Tokentype::SYMBOL },
        { R"([ \t\n\r]+)", Tokentype::WHITESPACE }
    }};

    static constexpr std::array<std::pair<const char*, Tokentype>, 2> specialTokens = {{
        { "(", Tokentype::LEFT_PAREN },
        { ")", Tokentype::RIGHT_PAREN }
    }};

    static const std::array<RegexInfo, regexPatterns.size()> compiledRegexes;

    std::vector<std::string> lines;
    std::string_view input;
    int line = 1;
    int row = 1;

    void splitIntoLines(std::string_view input);
    std::optional<Token> matchToken(std::string_view::const_iterator& it) const;
    void updatePosition(std::string_view lexeme);
};
