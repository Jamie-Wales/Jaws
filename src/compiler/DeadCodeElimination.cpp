#include "DeadCodeElimination.h"
#include "ANF.h"
#include "Visit.h"
#include <sstream>

namespace optimise {

static const std::unordered_set<std::string> sideEffects = {
    "display", "newline", "write", "read",
    "open-input-file", "open-output-file", "close-port",
    "error", "vector-set!", "list-set!", "load-library",
    "register-function", "eval", "apply", "call/cc",
    "call-with-current-continuation"
};

std::string DependancyGraph::toString() const {
    std::stringstream ss;
    for (const auto& [node, edges] : outgoing) {
        ss << node << " -> ";
        bool first = true;
        for (const auto& edge : edges) {
            if (!first)
                ss << ", ";
            ss << edge.to;
            first = false;
        }
        ss << "\n";
    }
    return ss.str();
}

void DependancyGraph::addEdge(const std::string& from, const std::string& to) {
    Edge edge = { from, to };
    auto& edges = outgoing[from];
    if (std::find_if(edges.begin(), edges.end(),
            [&to](const Edge& e) { return e.to == to; })
        == edges.end()) {
        edges.push_back(edge);
        incoming[to].push_back(edge);
    }
}

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

DCEResult dce(std::vector<std::shared_ptr<ir::TopLevel>>& tops)
{
    DependancyGraph dp;
    for (const auto& top : tops) {
        collectTopLevelDependencies(*top, dp);
    }
    
    std::string preGraph = dp.toString();
    
    std::unordered_set<std::string> initialRoots = getSideEffectNodes(dp);
    initialRoots.insert("root");

    auto liveNodes = getLiveNodes(dp, initialRoots);
    auto filteredGraph = filterGraph(dp, liveNodes);
    auto filteredTops = filterTops(tops, liveNodes);
    
    std::string postGraph = filteredGraph.toString();
    
    return {
        filteredTops,
        {preGraph, postGraph}
    };
}

void collectUsedVariables(const std::shared_ptr<ir::ANF>& anf, std::unordered_set<std::string>& usedList)
{
    if (!anf)
        return;

    std::visit(overloaded {
                   [&](const ir::Let& let) {
                       if (let.binding) {
                           collectUsedVariables(let.binding, usedList);
                       }
                       if (let.body) {
                           collectUsedVariables(let.body, usedList);
                       }
                   },
                   [&](const ir::App& app) {
                       usedList.insert(app.name.lexeme);
                       for (const auto& param : app.params) {
                           usedList.insert(param.lexeme);
                       }
                   },
                   [&](const ir::Lambda& lambda) {
                       if (lambda.body) {
                           collectUsedVariables(lambda.body, usedList);
                       }
                   },
                   [&](const ir::If& if_) {
                       usedList.insert(if_.cond.lexeme);
                       collectUsedVariables(if_.then, usedList);
                       if (if_._else) {
                           collectUsedVariables(*if_._else, usedList);
                       }
                   },
                   [&](const ir::Atom& atom) {
                       usedList.insert(atom.atom.lexeme);
                   },
                   [&](const ir::Quote&) {} },
        anf->term);
}

void elimatedDeadCode(std::vector<std::shared_ptr<ir::ANF>>& anfs)
{
    std::unordered_set<std::string> used;
    for (auto& anf : anfs) {
        collectUsedVariables(anf, used);
    }
    for (auto& anf : anfs) {
        anf = eliminateUnused(anf, used);
    }
}

std::shared_ptr<ir::ANF> eliminateUnused(const std::shared_ptr<ir::ANF>& anf, const std::unordered_set<std::string>& used)
{
    if (!anf)
        return nullptr;

    return std::visit(overloaded {
                         [&](const ir::Let& let) -> std::shared_ptr<ir::ANF> {
                             if (!let.name || used.count(let.name->lexeme) > 0) {
                                 return std::make_shared<ir::ANF>(ir::Let {
                                     let.name,
                                     let.binding ? eliminateUnused(let.binding, used) : nullptr,
                                     let.body ? eliminateUnused(let.body, used) : nullptr });
                             }
                             return let.body ? eliminateUnused(let.body, used) : nullptr;
                         },
                         [&](const ir::App& app) -> std::shared_ptr<ir::ANF> {
                             return std::make_shared<ir::ANF>(app);
                         },
                         [&](const ir::Lambda& lambda) -> std::shared_ptr<ir::ANF> {
                             return std::make_shared<ir::ANF>(ir::Lambda {
                                 lambda.params,
                                 lambda.body ? eliminateUnused(lambda.body, used) : nullptr });
                         },
                         [&](const ir::If& if_) -> std::shared_ptr<ir::ANF> {
                             return std::make_shared<ir::ANF>(ir::If {
                                 if_.cond,
                                 eliminateUnused(if_.then, used),
                                 if_._else ? eliminateUnused(*if_._else, used) : nullptr });
                         },
                         [&](const ir::Atom& atom) -> std::shared_ptr<ir::ANF> {
                             return std::make_shared<ir::ANF>(atom);
                         },
                         [&](const ir::Quote& quote) -> std::shared_ptr<ir::ANF> {
                             return std::make_shared<ir::ANF>(quote);
                         } },
        anf->term);
}

}
