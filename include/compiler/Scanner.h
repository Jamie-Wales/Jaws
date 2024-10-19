#pragma once
#include "Token.h"
#include <regex>
#include <string>
#include <unordered_map>
#include <vector>

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

    static const std::unordered_map<std::string, Tokentype> keywords;
    static const std::vector<RegexInfo> regexPatterns;

    std::vector<std::string> lines;
    std::string input;
    size_t position = 0;
    int line = 1;
    int column = 1;

    void splitIntoLines();
    std::optional<Token> matchToken();
    void updatePosition(const std::string& lexeme);
};
