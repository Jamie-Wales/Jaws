#include "Expression.h"
#include "Token.h"
#include <iostream>
#include <sstream>
#include <stdexcept>
#include <string>
#include <utility>

class ParseError : public std::runtime_error {
public:
    ParseError(const std::string& message, const Token& token, std::string line)
        : std::runtime_error(formatError(message, token))
        , token(token)
        , line(std::move(line))
    {
    }

    void printFormattedError() const
    {
        std::cerr << what() << "\n";
        std::cerr << line << "\n";
        std::cerr << generateErrorIndicator() << "\n";
    }

private:
    Token token;
    std::string line;

    static std::string formatError(const std::string& message, const Token& token)
    {
        std::ostringstream oss;
        oss << "(Parse error (at line " << token.line << ")\n"
            << "(" << message << "))";
        return oss.str();
    }

    [[nodiscard]] std::string generateErrorIndicator() const
    {
        std::string indicator(token.column - 1, '-');
        indicator += ">";
        return indicator;
    }
};
class InterpreterError : public std::runtime_error {
public:
    InterpreterError(const std::string& message, std::optional<std::reference_wrapper<const Expression>> expr = std::nullopt)
        : std::runtime_error(formatError(message, expr))
        , expr(expr)
    {
    }

    void printFormattedError() const
    {
        std::cerr << what() << "\n";
        if (expr) {
            std::cerr << "\nIn expression:\n";
            expr->get().print();
            std::cerr << "\n";
        }
    }

private:
    std::optional<std::reference_wrapper<const Expression>> expr;

    static std::string formatError(const std::string& message, std::optional<std::reference_wrapper<const Expression>> expr)
    {
        std::ostringstream oss;
        if (expr) {
            oss << "Interpreter error at linew " << expr->get().line << ": ";
        } else {
            oss << "Interpreter error: ";
        }
        oss << message;
        return oss.str();
    }
};
