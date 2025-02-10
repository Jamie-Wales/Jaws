#pragma once
#include "ANF.h"
#include "Value.h"
#include <unordered_map>

namespace optimise {
using Environment = std::unordered_map<std::string, SchemeValue>;

std::vector<std::shared_ptr<ir::ANF>> optimiseConstants(std::vector<std::shared_ptr<ir::ANF>>& anfs);
std::shared_ptr<ir::ANF> constantFold(const std::shared_ptr<ir::ANF>& anf, Environment& env);
std::shared_ptr<ir::ANF> schemeValueToANF(const SchemeValue& value);

}
