#include "ANF.h"
#include "Visit.h"
namespace ir {

static void toStringInternal(const ANF& node, std::stringstream& ss);

static void toStringInternal(const Quote& quote, std::stringstream& ss)
{
    ss << "(quote ";
    if (quote.expr) {
        quote.expr->toString(ss);
    } else {
        ss << "null";
    }
    ss << ")";
}

static void toStringInternal(const Atom& a, std::stringstream& ss)
{
    // Just the atom's lexeme
    ss << a.atom.lexeme;
}

static void toStringInternal(const If& if_expr, std::stringstream& ss)
{
    // Multi-line style:
    // (if cond
    //     then
    //     else)
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
    // (app f arg1 arg2 ...)
    ss << "(app " << app.name.lexeme;
    for (const auto& param : app.params) {
        ss << " " << param.lexeme;
    }
    ss << ")";
}

static void toStringInternal(const Let& let, std::stringstream& ss)
{
    // (let (name binding)
    //   body)
    ss << "(let (";
    if (let.name.has_value()) {
        ss << let.name->lexeme << " ";
        // Print the binding inline
        if (let.binding) {
            toStringInternal(*let.binding, ss);
        } else {
            ss << "()";
        }
    } else {
        // If there's no name, still show the binding:
        if (let.binding) {
            toStringInternal(*let.binding, ss);
        } else {
            ss << "()";
        }
    }
    ss << ")\n  "; // new line for the body
    if (let.body) {
        toStringInternal(*let.body, ss);
    } else {
        ss << "null";
    }
    ss << ")";
}

static void toStringInternal(const Lambda& lambda, std::stringstream& ss)
{
    // (lambda (p1 p2 ...)
    //   body)
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

// Dispatch over the ANF variant:
static void toStringInternal(const ANF& node, std::stringstream& ss)
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
