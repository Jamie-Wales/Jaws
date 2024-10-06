#pragma once
#include <string>

enum class Tokentype {
    INTEGER,
    FLOAT,
    STRING,
    SYMBOL,

    LEFT_PAREN,
    RIGHT_PAREN,

    PLUS,
    MINUS,
    MULTIPLY,
    DIVIDE,

    EQUAL,
    LESS_THAN,
    GREATER_THAN,

    DEFINE,
    LAMBDA,
    IF,

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
