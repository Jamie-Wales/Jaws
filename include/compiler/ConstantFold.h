#pragma once
#include "ANF.h"
#include <unordered_map>
#include <vector>

namespace optimise {

using NumericConstant = std::variant<int, double>;
using Constant = std::variant<NumericConstant, bool>;
std::vector<std::shared_ptr<ir::TopLevel>> optimiseConstants(std::vector<std::shared_ptr<ir::TopLevel>>& topLevels);
bool isPureFunction(const Token& func);
std::optional<NumericConstant> getNumericValue(const Token& token);
std::optional<bool> getBoolValue(const Token& token);
std::shared_ptr<ir::ANF> createConstantNode(const Constant& value, const Token& original);
std::shared_ptr<ir::ANF> constantFold(const std::shared_ptr<ir::ANF>& anf, std::unordered_map<std::string, Constant>& env);

}
