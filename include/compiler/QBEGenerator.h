#pragma once

#include "ThreeAC.h"
#include <iostream>
#include <map>
#include <unordered_set>

#include <sstream>
#include <string>
#include <unordered_map>
#include <vector>

namespace qbe {

struct QBEGeneratorState {
    std::stringstream output;
    std::vector<std::string> pendingParamsTac;
    int tempCount;
    int labelCount;
    int stringLiteralCounter;
    std::unordered_set<std::string> stringLiteralsForSymbols;
    std::unordered_map<std::string, std::string> stringLiteralMap;
    std::unordered_map<std::string, std::string> stringLiteralsForData;
    std::string currentFuncLabel;
    std::map<std::string, std::string> numberLiteralCache;
    std::string currentCallEnvSlot;
    std::vector<std::string> tacParamNames;
    std::unordered_map<std::string, std::string> varToFunctionLabel;
    std::unordered_map<std::string, size_t> functionParamCounts;
    std::string tailCallArgsSlot;
    int maxTailCallArgs = 0;
    std::string tailArgBase;
    std::unordered_map<std::string, std::string> schemeFunctionNameToLabel;

    QBEGeneratorState()
        : tempCount(0)
        , labelCount(0)
        , stringLiteralCounter(0)
    // tacParamNames will be default-initialized (empty vector)
    {
    }
};
void generateQBEIr(const tac::ThreeAddressModule& module, const std::string& outputPath);
}
