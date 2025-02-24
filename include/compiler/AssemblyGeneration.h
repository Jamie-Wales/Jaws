#pragma once
#include "ThreeAC.h"
#include <set>
#include <sstream>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

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
    std::unordered_set<std::string> declaredStrings;
    std::unordered_map<std::string, int> varOffsets;
    std::set<Register> usedRegisters;
    int currentStackOffset = 0;

    int getVarOffset(const std::string& var)
    {
        auto it = varOffsets.find(var);
        if (it != varOffsets.end()) {
            return it->second;
        }
        currentStackOffset += 8;
        varOffsets[var] = currentStackOffset;
        return currentStackOffset;
    }
};

std::string regToString(Register reg);
void generateAssembly(const tac::ThreeAddressModule& module, const std::string& outputPath);
bool assembleIntoExecutable(const std::string& asmPath, const std::string& exePath);

void convertInstruction(const tac::ThreeACInstruction& instr, AssemblyGeneratorState& state);
void handleCopy(const tac::ThreeACInstruction& instr, AssemblyGeneratorState& state);
void handleCall(const tac::ThreeACInstruction& instr, AssemblyGeneratorState& state);
void handleAlloc(const tac::ThreeACInstruction& instr, AssemblyGeneratorState& state);
void handleStore(const tac::ThreeACInstruction& instr, AssemblyGeneratorState& state);
void handleLoad(const tac::ThreeACInstruction& instr, AssemblyGeneratorState& state);
void handleLabel(const tac::ThreeACInstruction& instr, AssemblyGeneratorState& state);
void handleJump(const tac::ThreeACInstruction& instr, AssemblyGeneratorState& state);
void handleJumpIf(const tac::ThreeACInstruction& instr, AssemblyGeneratorState& state);
void handleJumpIfNot(const tac::ThreeACInstruction& instr, AssemblyGeneratorState& state);

bool isNumber(const std::string& str);

extern const std::vector<Register> PARAM_REGISTERS;
extern const std::vector<Register> CALLER_SAVED;
extern const std::vector<Register> CALLEE_SAVED;
}
