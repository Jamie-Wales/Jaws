#pragma once
#include "ThreeAC.h"
#include <sstream>
#include <string>

struct QBEGeneratorState {
    std::stringstream output;
    int currentStackOffset = 0;
    std::unordered_map<std::string, int> varOffsets;

    // Helper to get unique temporary names
    int tempCounter = 0;
    std::string newTemp()
    {
        return "t" + std::to_string(tempCounter++);
    }
};

void convertInstruction(const tac::ThreeACInstruction& instr, QBEGeneratorState& state);
void generateQBEIr(const tac::ThreeAddressModule& module, const std::string& outputPath);
void handleCopy(const tac::ThreeACInstruction& instr, QBEGeneratorState& state);
void handleCall(const tac::ThreeACInstruction& instr, QBEGeneratorState& state);
void handleAlloc(const tac::ThreeACInstruction& instr, QBEGeneratorState& state);
bool isNumber(const std::string& str);
