#pragma once

#include "ThreeAC.h"
#include <iostream>
#include <map>
#include <set>
#include <sstream>
#include <string>
#include <unordered_map>
#include <vector>

namespace qbe {

struct QBEGeneratorState {
    std::stringstream output;
    int tempCount = 0;
    int labelCount = 0;
    std::vector<std::string> pendingParamsTac;
    std::unordered_map<std::string, std::string> tempMap;
    bool isInsideFunction = false;
    std::string currentFuncLabel = "";

    std::set<std::string> stringLiteralsForSymbols;
    std::map<std::string, std::string> stringLiteralsForData;

    QBEGeneratorState()
        : tempCount(0)
        , labelCount(0)
        , isInsideFunction(false)
    {
    }
};

void generateQBEIr(const tac::ThreeAddressModule& module, const std::string& outputPath);

}
