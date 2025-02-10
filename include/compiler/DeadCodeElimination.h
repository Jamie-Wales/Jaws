
#pragma once

#include "ANF.h"
#include <memory>
#include <unordered_set>

namespace optimise {

void elimatedDeadCode(std::vector<std::shared_ptr<ir::ANF>>& anfs);
void collectUsedVariables(const std::shared_ptr<ir::ANF>& anf, std::unordered_set<std::string>& usedList);
std::shared_ptr<ir::ANF> eliminateUnused(const std::shared_ptr<ir::ANF>& anf, const std::unordered_set<std::string>& used);
void dce(ir::ANF& anf);

}
