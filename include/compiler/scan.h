#pragma once
#include "Error.h"
#include "Token.h"
#include <regex>
#include <string>
#include <unordered_map>
#include <vector>

namespace scanner {

struct ScanState {
    std::string input;
    size_t position;
    int currentLine;
    int column;
    std::vector<std::string> lines;

    explicit ScanState(std::string inp)
        : input(std::move(inp))
        , position(0)
        , currentLine(1)
        , column(1)
        , lines({ "" })
    {
    }
};

struct RegexInfo {
    std::regex regex;
    Tokentype type;
};

std::pair<ScanState, std::optional<Token>> matchNextToken(ScanState state);
ScanState updateScanPosition(ScanState state, const std::string& lexeme);
std::vector<Token> tokenize(const std::string& input);
std::string getLine(const std::vector<std::string>& lines, int lineNumber);

} // namespace scanner
