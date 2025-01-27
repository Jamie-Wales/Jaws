#include "ANF.h"
#include "Visit.h"
namespace ir {

void ANF::toString(std::stringstream& ss) const
{
    std::visit(overloaded {
                   [&](const Quote& quote) -> void {
                       ss << "(quote ";
                       quote.expr->toString(ss);
                       ss << ")";
                   },
                   [&](const Atom& a) -> void {
                       ss << a.atom.lexeme;
                   },
                   [&](const If& if_expr) -> void {
                       ss << "(if " << if_expr.cond.lexeme << " ";
                       if_expr.then->toString(ss);
                       if (if_expr._else) {
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
                       ss << "(let";
                       if (let.name.has_value()) {
                           ss << " " << let.name->lexeme;
                       }
                       ss << " " << let.binding->toString() << " ";
                       let.body->toString(ss);
                       ss << ")";
                   },
                   [&](const Function& func) -> void {
                       ss << "(define " << func.name.lexeme << " (";
                       for (size_t i = 0; i < func.args.size(); i++) {
                           if (i > 0)
                               ss << " ";
                           ss << func.args[i].lexeme;
                       }
                       ss << ") ";
                       func.body->toString(ss);
                       ss << ")";
                   },
                   [&](const Lambda& lambda) -> void {
                       ss << "(lambda (";
                       for (size_t i = 0; i < lambda.params.size(); i++) {
                           if (i > 0)
                               ss << " ";
                           ss << lambda.params[i].lexeme;
                       }
                       ss << ") ";
                       lambda.body->toString(ss);
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
