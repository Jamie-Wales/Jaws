#pragma once
#include <string>

enum class Tokentype {
    ELSE,
    DOT,
    ELLIPSIS,
    INTEGER,
    BEGIN,
    COND,
    SET,
    FLOAT,
    STRING,
    SYMBOL,
    IMPORT,
    QUOTE_SYMBOL,
    IDENTIFIER,
    LEFT_PAREN,
    RIGHT_PAREN,
    CONS,
    CAR,
    PLUS,
    MINUS,
    MULTIPLY,
    ONLY,
    EXCEPT,
    PREFIX,
    RENAME,
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
    HASH,
    DEFINE_SYTAX,
    SYNTAX_RULE,
    ARROW,
    LET,
    LETREC,
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
