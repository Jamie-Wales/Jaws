#pragma once

#include "ThreeAC.h"
#include <set>
#include <string>

struct QBEGeneratorState {
    std::stringstream output;
    int tempCount = 0;
    int labelCount = 0;
    std::unordered_map<std::string, std::string> tempMap;
};
// Main function to generate QBE IR from Three Address Code
void generateQBEIr(const tac::ThreeAddressModule& module, const std::string& outputPath);

// Helper function to process a single instruction
void convertInstruction(const tac::ThreeACInstruction& instr, QBEGeneratorState& state, const std::set<std::string>& globalVars);

// Handlers for different instruction types
void handleCopy(const tac::ThreeACInstruction& instr, QBEGeneratorState& state, const std::set<std::string>& globalVars);
void handleLabel(const tac::ThreeACInstruction& instr, QBEGeneratorState& state);
void handleJump(const tac::ThreeACInstruction& instr, QBEGeneratorState& state);
void handleCall(const tac::ThreeACInstruction& instr, QBEGeneratorState& state);
void handleJumpIf(const tac::ThreeACInstruction& instr, QBEGeneratorState& state, const std::set<std::string>& globalVars);
void handleJumpIfNot(const tac::ThreeACInstruction& instr, QBEGeneratorState& state, const std::set<std::string>& globalVars);
void handleAlloc(const tac::ThreeACInstruction& instr, QBEGeneratorState& state);
void handleLoad(const tac::ThreeACInstruction& instr, QBEGeneratorState& state, const std::set<std::string>& globalVars);
void handleStore(const tac::ThreeACInstruction& instr, QBEGeneratorState& state, const std::set<std::string>& globalVars);
void handleGC(const tac::ThreeACInstruction& instr, QBEGeneratorState& state);

// Helper functions
bool isNumber(const std::string& str);
std::string convertPrimitive(const std::string& primitive);
