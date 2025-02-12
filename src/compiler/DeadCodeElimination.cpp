#include "DeadCodeElimination.h"
#include "ANF.h"
#include "Visit.h"
#include <iostream>

namespace optimise {

void DependancyGraph::addEdge(const std::string& from, const std::string& to)
{
    Edge edge = { from, to };
    outgoing[from].push_back(edge);
    incoming[to].push_back(edge);
}

void DependancyGraph::print()
{
    std::cout << "Dependency Graph:\n";
    for (const auto& [node, edges] : outgoing) {
        std::cout << node << " -> ";
        bool first = true;
        for (const auto& edge : edges) {
            if (!first)
                std::cout << ", ";
            std::cout << edge.to;
            first = false;
        }
        std::cout << "\n";
    }
}

void collectDependencies(const std::shared_ptr<ir::ANF>& anf, DependancyGraph& graph,
    const std::string& currentScope = "")
{
    if (!anf)
        return;

    std::visit(overloaded {
                   [&](const ir::Let& let) {
                       if (let.name) {
                           std::string letName = let.name->lexeme;
                           collectDependencies(let.binding, graph, letName);
                           collectDependencies(let.body, graph, letName);
                       } else {
                           collectDependencies(let.binding, graph, currentScope);
                           collectDependencies(let.body, graph, currentScope);
                       }
                   },
                   [&](const ir::App& app) {
                       if (!currentScope.empty()) {
                           // Function name depends on current scope
                           if (app.name.type == Tokentype::IDENTIFIER) {
                               graph.addEdge(currentScope, app.name.lexeme);
                           }
                           // Parameters depend on current scope
                           for (const auto& param : app.params) {
                               if (param.type == Tokentype::IDENTIFIER) {
                                   graph.addEdge(currentScope, param.lexeme);
                               }
                           }
                       }
                   },
                   [&](const ir::Lambda& lambda) {
                       collectDependencies(lambda.body, graph, currentScope);
                   },
                   [&](const ir::If& if_) {
                       if (!currentScope.empty()) {
                           graph.addEdge(currentScope, if_.cond.lexeme);
                       }
                       collectDependencies(if_.then, graph, currentScope);
                       if (if_._else) {
                           collectDependencies(*if_._else, graph, currentScope);
                       }
                   },
                   [&](const ir::Atom& atom) {
                       if (!currentScope.empty() && atom.atom.type == Tokentype::IDENTIFIER) {
                           graph.addEdge(currentScope, atom.atom.lexeme);
                       }
                   },
                   [&](const ir::Quote&) { /* quotes don't create dependencies */ },
               },
        anf->term);
}
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
    DependancyGraph dp;
    auto shared_anf = std::make_shared<ir::ANF>(anf);
    collectDependencies(shared_anf, dp);
    dp.print();

    collectUsedVariables(shared_anf, used);
    auto optimized = eliminateUnused(shared_anf, used);

    if (!optimized) {
        return;
    }
    anf = *optimized;
}

void elimatedDeadCode(std::vector<std::shared_ptr<ir::ANF>>& anfs)
{
    for (const auto& anf : anfs) {
        dce(*anf);
    }
}
}
