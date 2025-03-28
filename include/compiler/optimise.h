#pragma once
#include "ANF.h"
#include <memory>
#include <string>
#include <vector>

namespace optimise {

struct OptimizationResult {
    std::vector<std::shared_ptr<ir::TopLevel>> optimizedAnf;
    std::string preDependencyGraph;
    std::string postDependencyGraph;
};

OptimizationResult optimise(std::vector<std::shared_ptr<ir::TopLevel>>& anfs);

}
