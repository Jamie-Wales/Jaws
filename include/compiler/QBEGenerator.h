#pragma once
#include "ThreeAC.h"
#include <sstream>
#include <string>

struct QBEGeneratorState {
    std::stringstream output;
    int tempCount;
    int labelCount;
    std::unordered_map<std::string, std::string> variables;
};

void generateQBEIr(const tac::ThreeAddressModule& module, const std::string& outputPath);
void convertInstruction(const tac::ThreeACInstruction& instr, QBEGeneratorState& state);
void handleCopy(const tac::ThreeACInstruction& instr, QBEGeneratorState& state);
void handleLabel(const tac::ThreeACInstruction& instr, QBEGeneratorState& state);
void handleJump(const tac::ThreeACInstruction& instr, QBEGeneratorState& state);
void handleCall(const tac::ThreeACInstruction& instr, QBEGeneratorState& state);
void handleJumpIf(const tac::ThreeACInstruction& instr, QBEGeneratorState& state);
void handleJumpIfNot(const tac::ThreeACInstruction& instr, QBEGeneratorState& state);
void handleAlloc(const tac::ThreeACInstruction& instr, QBEGeneratorState& state);
void handleLoad(const tac::ThreeACInstruction& instr, QBEGeneratorState& state);
void handleStore(const tac::ThreeACInstruction& instr, QBEGeneratorState& state);
void handleGC(const tac::ThreeACInstruction& instr, QBEGeneratorState& state);

bool isNumber(const std::string& str);
