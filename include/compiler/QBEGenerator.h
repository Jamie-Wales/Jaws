#pragma once

#include "ThreeAC.h" // Assuming ThreeAC definitions are here
#include <map> // For std::unordered_map
#include <optional> // For std::optional
#include <set>
#include <sstream>
#include <string>
#include <unordered_map> // Included via <map> but good practice to be explicit
#include <vector>

namespace qbe {

struct QBEGeneratorState {
    std::ostringstream output;
    int tempCount = 0;
    int labelCount = 0;
    std::vector<std::string> pendingParams;
    std::unordered_map<std::string, std::string> tempMap;
    bool generatingFunction = false;
    std::vector<std::string> currentFuncParamNamesTAC;
};
void generateQBEIr(const tac::ThreeAddressModule& module, const std::string& outputPath);

void convertInstruction(const tac::ThreeACInstruction& instr, QBEGeneratorState& state, const std::set<std::string>& globalVars);

} // namespace qbe
