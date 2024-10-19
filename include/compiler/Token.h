#pragma once
#include <string>
#include <unordered_map>

enum class Tokentype {
    INTEGER,
    FLOAT,
    STRING,
    SYMBOL,
    QUOTE_SYMBOL,
    IDENTIFIER,
    LEFT_PAREN,
    RIGHT_PAREN,
    CONS,
    CAR,
    PLUS,
    MINUS,
    MULTIPLY,
    DIVIDE,
    TRUE,
    FALSE,
    EQUAL,
    LESS_THAN,
    GREATER_THAN,
    DEFINE,
    LAMBDA,
    IF,
    RATIONAL,
    COMPLEX,
    QUOTE,
    WHITESPACE,
    COMMENT,
    EOF_TOKEN,
    ERROR
};

class Token {
public:
    Tokentype type;
    std::string lexeme;
    int line;
    int column;

    Token(Tokentype type, std::string lexeme, int line, int column)
        : type(type)
        , lexeme(std::move(lexeme))
        , line(line)
        , column(column)
    {
    }

    Token() = default;
};
