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
    using ANFTerm = std::variant<Let, Atom, If, Lambda, App, Quote>;
    ANFTerm term;
    ANF(ANFTerm t)
        : term(std::move(t))
    {
    }
    void toString(std::stringstream& ss) const;
    std::string toString() const;
};

class TDefine {
public:
    Token name;
    std::shared_ptr<ANF> body;

    TDefine(Token n, std::shared_ptr<ANF> b)
        : name(std::move(n))
        , body(std::move(b))
    {
    }
    void toString(std::stringstream& ss) const
    {
    }
};

class TopLevel {
public:
    using Declaration = std::variant<TDefine, std::shared_ptr<ANF>>;
    Declaration decl;

    explicit TopLevel(Declaration d)
        : decl(std::move(d))
    {
    }

    std::string toString() const
    {
        std::stringstream ss;
        std::visit(overloaded {
                       [&](const TDefine& def) {
                           ss << "(define " << def.name.lexeme << " ";
                           def.body->toString(ss);
                           ss << ")";
                       },
                       [&](const std::shared_ptr<ANF>& exp) { exp->toString(ss); } },
            decl);
        return ss.str();
    }
};

}
