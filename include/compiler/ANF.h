#pragma once
#include "Expression.h"
#include "Token.h"
#include <sstream>
#include <variant>
#include <vector>

namespace ir {

class ANF;

class Atom {
public:
    Token atom;
};

class Let {
public:
    std::optional<Token> name;
    std::shared_ptr<ANF> binding;
    std::shared_ptr<ANF> body;
};

class App {
public:
    Token name;
    std::vector<Token> params;
    bool is_tail;
};

class Function {
public:
    Token name;
    std::vector<Token> args;
    std::shared_ptr<ANF> body;
    bool is_primitive;
};

class If {
public:
    Token cond;
    std::shared_ptr<ANF> then;
    std::optional<std::shared_ptr<ANF>> _else;
};

class Lambda {
public:
    std::vector<Token> params;
    std::shared_ptr<ANF> body;
};
class Quote {
public:
    std::shared_ptr<Expression> expr;
};

class ANF {
public:
    using ANFTerm = std::variant<Let, Atom, Function, If, Lambda, App, Quote>;
    ANFTerm term;
    ANF(ANFTerm t)
        : term(std::move(t))
    {
    }

    void toString(std::stringstream& ss) const;
    std::string toString() const;
};

}
