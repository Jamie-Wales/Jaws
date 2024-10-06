#pragma once
#include "Token.h"
#include <optional>
#include <regex>
#include <string>
#include <unordered_map>
#include <vector>

class Scanner {
public:
    std::vector<Token> tokenize(const std::string& input);
    void addEOFToken()
    {
        Token eof = { Tokentype::EOF_TOKEN, "\0", line, row };
        tokens.push_back(eof);
    }

private:
    struct RegexInfo {
        std::regex regex;
        Tokentype type;
    };

    static const std::unordered_map<std::string, Tokentype> keywords;
    static const std::vector<RegexInfo> regexList;
    static const std::vector<std::pair<std::string, Tokentype>> operators;
    const std::string input;
    std::vector<Token> tokens;
    int line = 1;
    int row = 1;
    std::optional<Token> matchToken(std::string::const_iterator& it) const;
    void updatePosition(const std::string& lexeme);
};
