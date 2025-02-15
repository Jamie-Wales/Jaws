#include "ANF.h"
#include "Visit.h"
namespace ir {

void ANF::toString(std::stringstream& ss) const
{
    std::visit(overloaded {
                   [&](const Quote& quote) -> void {
                       ss << "(quote ";
                       if (quote.expr) {
                           quote.expr->toString(ss);
                       } else {
                           ss << "null";
                       }
                       ss << ")";
                   },
                   [&](const Atom& a) -> void {
                       ss << a.atom.lexeme;
                   },
                   [&](const If& if_expr) -> void {
                       ss << "(if " << if_expr.cond.lexeme << " ";
                       if (if_expr.then) {
                           if_expr.then->toString(ss);
                       } else {
                           ss << "null";
                       }
                       if (if_expr._else && *if_expr._else) {
                           ss << " ";
                           (*if_expr._else)->toString(ss);
                       }
                       ss << ")";
                   },
                   [&](const App& app) -> void {
                       ss << "(app " << app.name.lexeme;
                       for (const auto& param : app.params) {
                           ss << " " << param.lexeme;
                       }
                       ss << ")";
                   },
                   [&](const Let& let) -> void {
                       ss << "(let (";
                       if (let.name.has_value()) {
                           ss << let.name->lexeme << " ";
                       }
                       if (let.binding) {
                           ss << let.binding->toString();
                       } else {
                           ss << "()";
                       }
                       ss << ") ";
                       if (let.body) {
                           (*let.body)->toString(ss);
                       }
                       ss << ")";
                   },
                   [&](const Lambda& lambda) -> void {
                       ss << "(λ \n(";
                       for (size_t i = 0; i < lambda.params.size(); i++) {
                           if (i > 0)
                               ss << " ";
                           ss << lambda.params[i].lexeme;
                       }
                       ss << ") ";
                       if (lambda.body) {
                           lambda.body->toString(ss);
                       } else {
                           ss << "null";
                       }
                       ss << ")";
                   },
               },
        term);
}

std::string ANF::toString() const
{
    std::stringstream ss;
    toString(ss);
    return ss.str();
}
};
