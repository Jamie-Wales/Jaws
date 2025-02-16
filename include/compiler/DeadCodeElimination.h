
#pragma once

#include "ANF.h"
#include <memory>
#include <unordered_set>

namespace optimise {

void dce(std::vector<std::shared_ptr<ir::TopLevel>>& tops);
void elimatedDeadCode(std::vector<std::shared_ptr<ir::ANF>>& anfs);
void collectUsedVariables(const std::shared_ptr<ir::ANF>& anf, std::unordered_set<std::string>& usedList);
std::shared_ptr<ir::ANF> eliminateUnused(const std::shared_ptr<ir::ANF>& anf, const std::unordered_set<std::string>& used);

class DependancyGraph {
public:
    struct Edge {
        std::string from;
        std::string to;
    };
    using EdgeMap = std::unordered_map<std::string, std::vector<Edge>>;
    void addEdge(const std::string& from, const std::string& to);
    void print();
    EdgeMap outgoing;
    EdgeMap incoming;
};

}
