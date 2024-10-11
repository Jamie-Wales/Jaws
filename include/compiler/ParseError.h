#pragma once
#include "Token.h"
#include <iostream>
#include <sstream>
#include <stdexcept>
#include <string>

class ParseError : public std::runtime_error {
public:
    ParseError(const std::string& message, const Token& token, const std::string& line)
        : std::runtime_error(formatError(message, token, line))
        , m_token(token)
        , m_line(line)
    {
    }

    void printFormattedError() const
    {
        std::cerr << what() << "\n";
        std::cerr << m_line << "\n";
        std::cerr << std::string(m_token.column - 1, ' ') << "^\n";
    }

private:
    Token m_token;
    std::string m_line;

    static std::string formatError(const std::string& message, const Token& token, const std::string& line)
    {
        std::ostringstream oss;
        oss << "Parse error at line " << token.line << ", column " << token.column
            << " (Token: " << token.lexeme << "): " << message;
        return oss.str();
    }
};
