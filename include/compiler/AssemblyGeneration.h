#pragma once

#include "ThreeAC.h"
#include <sstream>
#include <string>
#include <unordered_map>

namespace assembly {
enum class Register {
    RAX,
    RBX,
    RCX,
    RDX,
    RSI,
    RDI,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15
};

struct AssemblyGeneratorState {
    std::stringstream output;
    std::unordered_map<std::string, Register> varToReg;
    std::unordered_map<std::string, std::string> varToStack;
    int stackOffset = 0;
};

std::string regToString(Register reg);
AssemblyGeneratorState generateAssembly(const tac::ThreeAddressModule& module);
void convertInstruction(const tac::ThreeACInstruction& instr, AssemblyGeneratorState& state);
void handleCopy(const tac::ThreeACInstruction& instr, AssemblyGeneratorState& state);
void handleCall(const tac::ThreeACInstruction& instr, AssemblyGeneratorState& state);
void handleAlloc(const tac::ThreeACInstruction& instr, AssemblyGeneratorState& state);
void handleJumpIf(const tac::ThreeACInstruction& instr, AssemblyGeneratorState& state);

};
