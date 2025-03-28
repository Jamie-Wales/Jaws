#pragma once
#include "ANF.h"
#include <memory>
#include <queue>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <utility>

namespace optimise {

class DependancyGraph {
public:
    struct Edge {
        std::string from;
        std::string to;
    };
    using EdgeMap = std::unordered_map<std::string, std::vector<Edge>>;

    void addEdge(const std::string& from, const std::string& to);
    std::string toString() const;

    EdgeMap outgoing;
    EdgeMap incoming;
};

struct DCEResult {
    std::vector<std::shared_ptr<ir::TopLevel>> optimizedTops;
    std::pair<std::string, std::string> graphs;
};

void elimatedDeadCode(std::vector<std::shared_ptr<ir::ANF>>& anfs);
void collectUsedVariables(const std::shared_ptr<ir::ANF>& anf, std::unordered_set<std::string>& usedList);
std::shared_ptr<ir::ANF> eliminateUnused(const std::shared_ptr<ir::ANF>& anf, const std::unordered_set<std::string>& used);

DCEResult dce(std::vector<std::shared_ptr<ir::TopLevel>>& tops);

}
