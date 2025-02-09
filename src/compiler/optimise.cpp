#include "ANF.h"
#include "Visit.h"
#include <unordered_set>

namespace optimise {
void collectUsedVariables(const std::shared_ptr<ir::ANF>& anf, std::unordered_set<std::string>& usedList)
{
    if (!anf) {
        return;
    }

    std::visit(overloaded {
                   [&](const ir::Atom& a) {
                       if (a.atom.type == Tokentype::IDENTIFIER) {
                           usedList.insert(a.atom.lexeme);
                       }
                   },
                   [&](const ir::App& app) {
                       if (app.name.type == Tokentype::IDENTIFIER) {
                           usedList.insert(app.name.lexeme);
                       }
                       for (const auto& param : app.params) {
                           if (param.type == Tokentype::IDENTIFIER) {
                               usedList.insert(param.lexeme);
                           }
                       }
                   },
                   [&](const ir::Let& let) {
                       collectUsedVariables(let.binding, usedList);
                       collectUsedVariables(let.body, usedList);
                   },
                   [&](const ir::If& if_) {
                       usedList.insert(if_.cond.lexeme);
                       collectUsedVariables(if_.then, usedList);

                       if (if_._else) {
                           collectUsedVariables(*if_._else, usedList);
                       }
                   },
                   [&](const ir::Lambda& l) {
                       collectUsedVariables(l.body, usedList);
                   },
                   [&](const ir::Quote& q) {
                       return;
                   },
               },
        anf->term);
}

std::shared_ptr<ir::ANF> eliminateUnused(const std::shared_ptr<ir::ANF>& anf,
    const std::unordered_set<std::string>& used)
{
    if (!anf) {
        return nullptr;
    }

    return std::visit(overloaded {
                          [&](const ir::Let& let) -> std::shared_ptr<ir::ANF> {
                              auto new_binding = eliminateUnused(let.binding, used);
                              auto new_body = eliminateUnused(let.body, used);
                              if (let.name && !used.contains(let.name->lexeme)) {
                                  return new_body;
                              }
                              return std::make_shared<ir::ANF>(ir::Let {
                                  let.name,
                                  new_binding,
                                  new_body });
                          },
                          [&](const ir::If& if_expr) -> std::shared_ptr<ir::ANF> {
                              auto new_then = eliminateUnused(if_expr.then, used);

                              std::optional<std::shared_ptr<ir::ANF>> new_else;
                              if (if_expr._else) {
                                  new_else = eliminateUnused(*if_expr._else, used);
                              }

                              return std::make_shared<ir::ANF>(ir::If {
                                  if_expr.cond,
                                  new_then,
                                  new_else });
                          },
                          [&](const ir::Lambda& lambda) -> std::shared_ptr<ir::ANF> {
                              auto new_body = eliminateUnused(lambda.body, used);
                              return std::make_shared<ir::ANF>(ir::Lambda {
                                  lambda.params,
                                  new_body });
                          },
                          [&](const auto& x) -> std::shared_ptr<ir::ANF> {
                              return std::make_shared<ir::ANF>(x);
                          } },
        anf->term);
}

void dce(ir::ANF& anf)
{
    std::unordered_set<std::string> used;
    auto shared_anf = std::make_shared<ir::ANF>(anf);
    collectUsedVariables(shared_anf, used);
    auto optimized = eliminateUnused(shared_anf, used);

    if (!optimized) {
        return;
    }
    anf = *optimized;
}

std::vector<std::shared_ptr<ir::ANF>> optimise(std::vector<std::shared_ptr<ir::ANF>>& anfs)
{
    for (const auto& anf : anfs)
        if (anf) {
            dce(*anf);
        }
    return anfs;
}
}
