#pragma once

#include <string>
#include <vector>
#include <regex>
#include <optional>
#include <unordered_map>
#include "Token.h"

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

    std::string input;
    std::vector<std::string> lines;
    size_t position = 0;
    int currentLine = 1;
    int column = 1;

    static const std::unordered_map<std::string, Tokentype> keywords;
    static const std::vector<RegexInfo> regexPatterns;

    std::vector<Token> generateTokens();
    std::optional<Token> matchToken();
    void updatePosition(const std::string& lexeme);
};