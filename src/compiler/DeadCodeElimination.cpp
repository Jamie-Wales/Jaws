#include "DeadCodeElimination.h"
#include "ANF.h"
#include "Visit.h"
#include <iostream>

namespace optimise {

void DependancyGraph::addEdge(const std::string& from, const std::string& to)
{
    Edge edge = { from, to };

    auto& edges = outgoing[from];
    if (std::find_if(edges.begin(), edges.end(),
            [&to](const Edge& e) { return e.to == to; })
        == edges.end()) {

        edges.push_back(edge);
        incoming[to].push_back(edge);
    }
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
    std::string currentScope = "root")
{
    if (!anf)
        return;

    std::visit(overloaded {
                   [&](const ir::Let& let) {
                       if (let.name) {
                           collectDependencies(let.binding, graph, let.name->lexeme);
                           if (!currentScope.empty()) {
                               graph.addEdge(currentScope, let.name->lexeme);
                           }
                           if (let.body)
                               collectDependencies(*let.body, graph, let.name->lexeme);
                       } else {
                           if (let.body)
                               collectDependencies(*let.body, graph, currentScope);
                       }
                   },

                   [&](const ir::App& app) {
                       for (const auto& param : app.params) {
                           graph.addEdge(app.name.lexeme, param.lexeme);
                       }
                       if (!currentScope.empty()) {
                           graph.addEdge(currentScope, app.name.lexeme);
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
                       if (!currentScope.empty()) {
                           graph.addEdge(currentScope, atom.atom.lexeme);
                       }
                   },

                   [&](const ir::Quote&) {} },
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
                              if (let.body) {
                                  auto new_body = eliminateUnused(*let.body, used);
                                  if (let.name && !used.contains(let.name->lexeme)) {
                                      return new_body;
                                  }
                                  return std::make_shared<ir::ANF>(ir::Let {
                                      let.name,
                                      new_binding,
                                      new_body });
                              }

                              return std::make_shared<ir::ANF>(ir::Let {
                                  let.name,
                                  new_binding,
                                  std::nullopt });
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

void dce(ir::ANF& anf, DependancyGraph& dp)
{
    std::unordered_set<std::string> used;
    auto shared_anf = std::make_shared<ir::ANF>(anf);
    collectDependencies(shared_anf, dp);
}

void elimatedDeadCode(std::vector<std::shared_ptr<ir::ANF>>& anfs)
{

    DependancyGraph dp;
    for (const auto& anf : anfs) {
        dce(*anf, dp);
    }
    dp.print();
}
}
