#include "ANF.h"
#include "Visit.h"
namespace ir {

void toStringInternal(const ANF& node, std::stringstream& ss);

void toStringInternal(const Quote& quote, std::stringstream& ss)
{
    ss << "(quote ";
    if (quote.expr) {
        quote.expr->toString(ss);
    } else {
        ss << "null";
    }
    ss << ")";
}

void toStringInternal(const Atom& a, std::stringstream& ss)
{
    ss << a.atom.lexeme;
}

void toStringInternal(const If& if_expr, std::stringstream& ss)
{
    ss << "(if " << if_expr.cond.lexeme << "\n  ";
    if (if_expr.then) {
        toStringInternal(*if_expr.then, ss);
    } else {
        ss << "null";
    }
    if (if_expr._else && *if_expr._else) {
        ss << "\n  ";
        toStringInternal(**if_expr._else, ss);
    }
    ss << ")";
}

static void toStringInternal(const App& app, std::stringstream& ss)
{
    ss << "(app " << app.name.lexeme;
    for (const auto& param : app.params) {
        ss << " " << param.lexeme;
    }
    ss << ")";
}

static void toStringInternal(const Let& let, std::stringstream& ss)
{
    ss << "(let (";
    if (let.name.has_value()) {
        ss << let.name->lexeme << " ";
        if (let.binding) {
            toStringInternal(*let.binding, ss);
        } else {
            ss << "()";
        }
    } else {
        if (let.binding) {
            toStringInternal(*let.binding, ss);
        } else {
            ss << "()";
        }
    }
    ss << ")\n  ";
    if (let.body) {
        toStringInternal(*let.body, ss);
    } else {
        ss << "null";
    }
    ss << ")";
}

static void toStringInternal(const Lambda& lambda, std::stringstream& ss)
{
    ss << "(lambda (";
    for (size_t i = 0; i < lambda.params.size(); i++) {
        if (i > 0) {
            ss << " ";
        }
        ss << lambda.params[i].lexeme;
    }
    ss << ")\n  ";
    if (lambda.body) {
        toStringInternal(*lambda.body, ss);
    } else {
        ss << "null";
    }
    ss << ")";
}

void toStringInternal(const ANF& node, std::stringstream& ss)
{
    std::visit(overloaded {
                   [&](const Quote& q) { toStringInternal(q, ss); },
                   [&](const Atom& a) { toStringInternal(a, ss); },
                   [&](const If& i) { toStringInternal(i, ss); },
                   [&](const App& app) { toStringInternal(app, ss); },
                   [&](const Let& l) { toStringInternal(l, ss); },
                   [&](const Lambda& lam) { toStringInternal(lam, ss); },
               },
        node.term);
}

void ANF::toString(std::stringstream& ss) const
{
    toStringInternal(*this, ss);
}

std::string ANF::toString() const
{
    std::stringstream ss;
    toString(ss);
    return ss.str();
}
};
