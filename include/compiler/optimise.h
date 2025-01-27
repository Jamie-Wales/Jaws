#include "ANF.h"
#include <unordered_set>

namespace optimise {

// ANF
void collectUsedVariables(const std::shared_ptr<ir::ANF>& anf, std::unordered_set<std::string>& usedList);
std::shared_ptr<ir::ANF> eliminateUnused(const std::shared_ptr<ir::ANF>& anf, const std::unordered_set<std::string>& used);
void dce(ir::ANF& anf);
std::vector<std::shared_ptr<ir::ANF>> optimise(std::vector<std::shared_ptr<ir::ANF>>& anfs);

// 3AC

}
