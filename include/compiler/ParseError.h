#pragma once

#include "Token.h"
#include <sstream>
#include <stdexcept>

class ParseError : public std::runtime_error {
public:
    ParseError(const std::string& message, const Token& token)
        : std::runtime_error(formatError(message, token))
    {
    }

private:
    static std::string formatError(const std::string& message, const Token& token)
    {
        std::ostringstream oss;
        oss << "Parse error at line " << token.line << ", column " << token.column
            << " (Token: " << token.lexeme << "): " << message;
        return oss.str();
    }
};
