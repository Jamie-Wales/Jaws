#include "DeadCodeElimination.h"
#include "ANF.h"
#include "Visit.h"
#include <iostream>

namespace optimise {

static const std::unordered_set<std::string> sideEffects = {
    "display", "newline", "write", "read",
    "open-input-file", "open-output-file", "close-port",
    "error", "vector-set!", "list-set!", "load-library",
    "register-function", "eval", "apply", "call/cc",
    "call-with-current-continuation"
};
std::unordered_set<std::string> getLiveNodes(
    const DependancyGraph& graph,
    const std::unordered_set<std::string>& initialRoots)
{
    std::unordered_set<std::string> live = initialRoots;
    std::queue<std::string> worklist(std::deque<std::string>(initialRoots.begin(), initialRoots.end()));

    while (!worklist.empty()) {
        std::string current = worklist.front();
        worklist.pop();

        auto it = graph.outgoing.find(current);
        if (it != graph.outgoing.end()) {
            for (const auto& edge : it->second) {
                if (live.insert(edge.to).second) {
                    worklist.push(edge.to);
                }
            }
        }
    }

    return live;
}

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

std::unordered_set<std::string> getSideEffectNodes(const DependancyGraph& graph)
{
    std::unordered_set<std::string> roots;

    for (const auto& [node, edges] : graph.outgoing) {
        for (const auto& edge : edges) {
            if (sideEffects.count(edge.to) > 0) {
                roots.insert(node);
                break;
            }
        }
    }

    return roots;
}
void DependancyGraph::print()
{
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

std::vector<std::shared_ptr<ir::TopLevel>> filterTops(
    const std::vector<std::shared_ptr<ir::TopLevel>>& tops,
    const std::unordered_set<std::string>& liveNodes)
{
    std::vector<std::shared_ptr<ir::TopLevel>> filtered;

    std::copy_if(tops.begin(), tops.end(),
        std::back_inserter(filtered),
        [&liveNodes](const auto& top) {
            if (auto def = std::get_if<ir::TDefine>(&top->decl)) {
                return liveNodes.count(def->name.lexeme) > 0;
            }
            return true;
        });

    return filtered;
}

DependancyGraph filterGraph(
    const DependancyGraph& graph,
    const std::unordered_set<std::string>& liveNodes)
{
    DependancyGraph filtered;

    for (const auto& [node, edges] : graph.outgoing) {
        if (liveNodes.count(node) > 0) {
            for (const auto& edge : edges) {
                if (liveNodes.count(edge.to) > 0) {
                    filtered.addEdge(edge.from, edge.to);
                }
            }
        }
    }

    return filtered;
}

void collectDependencies(const std::shared_ptr<ir::ANF>& anf, DependancyGraph& graph,
    const std::string& currentScope)
{
    if (!anf)
        return;
    std::visit(overloaded {
                   [&](const ir::Let& let) {
                       if (let.binding) {
                           if (let.name)
                               collectDependencies(let.binding, graph, let.name->lexeme);
                           else
                               collectDependencies(let.binding, graph, currentScope);
                       }
                       if (let.body) {
                           collectDependencies(let.body, graph, currentScope);
                       }
                   },
                   [&](const ir::App& app) {
                       graph.addEdge(currentScope, app.name.lexeme);
                       for (const auto& param : app.params) {
                           graph.addEdge(currentScope, param.lexeme);
                       }
                   },
                   [&](const ir::Lambda& lambda) {
                       collectDependencies(lambda.body, graph, currentScope);
                   },
                   [&](const ir::If& if_) {
                       graph.addEdge(currentScope, if_.cond.lexeme);
                       collectDependencies(if_.then, graph, currentScope);
                       if (if_._else) {
                           collectDependencies(*if_._else, graph, currentScope);
                       }
                   },
                   [&](const ir::Atom& atom) {
                       graph.addEdge(currentScope, atom.atom.lexeme);
                   },
                   [&](const ir::Quote&) {} },
        anf->term);
}

void collectTopLevelDependencies(const ir::TopLevel& top, DependancyGraph& graph)
{
    std::visit(overloaded {
                   [&](const ir::TDefine& def) {
                       if (def.body) {
                           collectDependencies(def.body, graph, def.name.lexeme);
                       }
                   },
                   [&](const std::shared_ptr<ir::ANF>& expr) {
                       collectDependencies(expr, graph, "root");
                   } },
        top.decl);
}

void dce(std::vector<std::shared_ptr<ir::TopLevel>>& tops, bool print)
{
    DependancyGraph dp;
    for (const auto& top : tops) {
        collectTopLevelDependencies(*top, dp);
    }
    if (print) {
        std::cout << "<| DependancyGraph Pre DeadCodeElimination |>\n";
        dp.print();
        std::cout << "\n"
                  << std::endl;
    }
    std::unordered_set<std::string> initialRoots = getSideEffectNodes(dp);
    initialRoots.insert("root");

    auto liveNodes = getLiveNodes(dp, initialRoots);
    dp = filterGraph(dp, liveNodes);
    tops = filterTops(tops, liveNodes);

    if (print) {
        std::cout << "<| DependancyGraph Post DeadCodeElimination |>\n";
        dp.print();
        std::cout << "\n"
                  << std::endl;
    }
}

}
