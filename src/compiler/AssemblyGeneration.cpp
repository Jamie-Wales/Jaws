#include "AssemblyGeneration.h"
#include <fstream>
#include <iostream>
#include <unordered_map>
#include <unordered_set>
#include <vector>

namespace assembly {

const std::vector<Register> PARAM_REGISTERS = {
    Register::RDI, Register::RSI, Register::RDX,
    Register::RCX, Register::R8, Register::R9
};

const std::vector<Register> CALLER_SAVED = {
    Register::RAX, Register::RCX, Register::RDX,
    Register::RSI, Register::RDI, Register::R8,
    Register::R9, Register::R10, Register::R11
};

const std::vector<Register> CALLEE_SAVED = {
    Register::RBX, Register::R12, Register::R13,
    Register::R14, Register::R15
};

std::string regToString(Register reg)
{
    switch (reg) {
    case Register::RAX:
        return "rax";
    case Register::RBX:
        return "rbx";
    case Register::RCX:
        return "rcx";
    case Register::RDX:
        return "rdx";
    case Register::RSI:
        return "rsi";
    case Register::RDI:
        return "rdi";
    case Register::R8:
        return "r8";
    case Register::R9:
        return "r9";
    case Register::R10:
        return "r10";
    case Register::R11:
        return "r11";
    case Register::R12:
        return "r12";
    case Register::R13:
        return "r13";
    case Register::R14:
        return "r14";
    case Register::R15:
        return "r15";
    }
    return "invalid_reg";
}

bool isNumber(const std::string& str)
{
    try {
        std::stoi(str);
        return true;
    } catch (...) {
        return false;
    }
}

std::string convertPrimitive(const std::string& primitive)
{
    static const std::unordered_map<std::string, std::string> primitiveMap = {
        { "+", "scheme_add" },
        { "-", "scheme_subtract" },
        { "*", "scheme_multiply" },
        { "/", "scheme_divide" },
        { "cons", "scheme_cons" },
        { "car", "scheme_car" },
        { "cdr", "scheme_cdr" }
    };

    auto it = primitiveMap.find(primitive);
    return it != primitiveMap.end() ? it->second : primitive;
}

void generateAssembly(const tac::ThreeAddressModule& module, const std::string& outputPath)
{
    AssemblyGeneratorState state;

    state.output << "section .text\n";
    state.output << "global main\n\n";

    // Declare external functions
    state.output << "extern alloc\n";
    state.output << "extern scheme_add\n";
    state.output << "extern scheme_subtract\n";
    state.output << "extern scheme_multiply\n";
    state.output << "extern scheme_divide\n";
    state.output << "extern scheme_cons\n";
    state.output << "extern scheme_car\n";
    state.output << "extern scheme_cdr\n\n";

    state.output << "main:\n";
    state.output << "    push rbp\n";
    state.output << "    mov rbp, rsp\n";
    state.output << "    sub rsp, 128  ; Reserve space for locals\n";

    for (auto reg : CALLEE_SAVED) {
        state.output << "    push " << regToString(reg) << "\n";
    }

    for (const auto& instr : module.instructions) {
        convertInstruction(instr, state);
    }

    state.output << "\n    ; Function epilogue\n";
    for (auto it = CALLEE_SAVED.rbegin(); it != CALLEE_SAVED.rend(); ++it) {
        state.output << "    pop " << regToString(*it) << "\n";
    }

    state.output << "    mov rsp, rbp\n";
    state.output << "    pop rbp\n";
    state.output << "    ret\n";

    std::ofstream outFile(outputPath + "/program.asm");
    if (!outFile) {
        std::cerr << "Failed to open output file: " << outputPath << std::endl;
        return;
    }
    outFile << state.output.str();
    outFile.close();
}

void handleCall(const tac::ThreeACInstruction& instr, AssemblyGeneratorState& state)
{
    if (!instr.arg1)
        return;

    int paramCount = instr.arg2 ? std::stoi(*instr.arg2) : 0;
    std::string funcName = convertPrimitive(*instr.arg1);

    // Save used caller-saved registers
    for (auto reg : state.usedRegisters) {
        if (std::find(CALLER_SAVED.begin(), CALLER_SAVED.end(), reg) != CALLER_SAVED.end()) {
            state.output << "    push " << regToString(reg) << "\n";
        }
    }

    // Align stack to 16 bytes
    state.output << "    and rsp, -16\n";

    // Load parameters into registers
    for (int i = 0; i < paramCount && i < 6; i++) {
        std::string tempVar = "_t" + std::to_string(i + 1);
        state.output << "    mov " << regToString(PARAM_REGISTERS[i])
                     << ", qword [rbp - " << state.getVarOffset(tempVar) << "]\n";
    }

    // Push remaining parameters onto stack
    for (int i = paramCount - 1; i >= 6; i--) {
        std::string tempVar = "_t" + std::to_string(i + 1);
        state.output << "    push qword [rbp - " << state.getVarOffset(tempVar) << "]\n";
    }

    // Make the function call
    state.output << "    call " << funcName << "\n";

    // Clean up stack if needed
    if (paramCount > 6) {
        state.output << "    add rsp, " << ((paramCount - 6) * 8) << "\n";
    }

    // Store result
    if (instr.result) {
        state.output << "    mov qword [rbp - " << state.getVarOffset(*instr.result) << "], rax\n";
    }

    // Restore caller-saved registers
    for (auto it = state.usedRegisters.rbegin(); it != state.usedRegisters.rend(); ++it) {
        if (std::find(CALLER_SAVED.begin(), CALLER_SAVED.end(), *it) != CALLER_SAVED.end()) {
            state.output << "    pop " << regToString(*it) << "\n";
        }
    }
}

void handleCopy(const tac::ThreeACInstruction& instr, AssemblyGeneratorState& state)
{
    if (!instr.result || !instr.arg1)
        return;

    if (isNumber(*instr.arg1)) {
        state.output << "    mov qword [rbp - " << state.getVarOffset(*instr.result)
                     << "], " << *instr.arg1 << "\n";
    } else {
        state.output << "    mov rax, qword [rbp - " << state.getVarOffset(*instr.arg1) << "]\n";
        state.output << "    mov qword [rbp - " << state.getVarOffset(*instr.result) << "], rax\n";
    }
}

void convertInstruction(const tac::ThreeACInstruction& instr, AssemblyGeneratorState& state)
{
    state.output << "\n    ; " << instr.toString(); // Comment showing original TAC

    switch (instr.op) {
    case tac::Operation::COPY:
        handleCopy(instr, state);
        break;
    case tac::Operation::CALL:
        handleCall(instr, state);
        break;
    case tac::Operation::ALLOC:
        handleAlloc(instr, state);
        break;
    case tac::Operation::STORE:
        handleStore(instr, state);
        break;
    case tac::Operation::LOAD:
        handleLoad(instr, state);
        break;
    case tac::Operation::LABEL:
        handleLabel(instr, state);
        break;
    case tac::Operation::JUMP:
        handleJump(instr, state);
        break;
    case tac::Operation::JUMP_IF:
        handleJumpIf(instr, state);
        break;
    case tac::Operation::JUMP_IF_NOT:
        handleJumpIfNot(instr, state);
        break;
    }
}

void handleAlloc(const tac::ThreeACInstruction& instr, AssemblyGeneratorState& state)
{
    if (!instr.result)
        return;

    state.output << "    push rcx\n";
    state.output << "    push rdx\n";
    state.output << "    mov rdi, 8  ; Standard allocation size\n";
    state.output << "    call alloc\n";
    state.output << "    mov qword [rbp - " << state.getVarOffset(*instr.result) << "], rax\n";
    state.output << "    pop rdx\n";
    state.output << "    pop rcx\n";
}

void handleStore(const tac::ThreeACInstruction& instr, AssemblyGeneratorState& state)
{
    if (!instr.arg1 || !instr.arg2)
        return;

    state.output << "    mov rax, qword [rbp - " << state.getVarOffset(*instr.arg1) << "]\n";
    if (isNumber(*instr.arg2)) {
        state.output << "    mov rbx, " << *instr.arg2 << "\n";
    } else {
        state.output << "    mov rbx, qword [rbp - " << state.getVarOffset(*instr.arg2) << "]\n";
    }
    state.output << "    mov qword [rax], rbx\n";
}

void handleLoad(const tac::ThreeACInstruction& instr, AssemblyGeneratorState& state)
{
    if (!instr.result || !instr.arg1)
        return;

    state.output << "    mov rax, qword [rbp - " << state.getVarOffset(*instr.arg1) << "]\n";
    state.output << "    mov rbx, qword [rax]\n";
    state.output << "    mov qword [rbp - " << state.getVarOffset(*instr.result) << "], rbx\n";
}

void handleLabel(const tac::ThreeACInstruction& instr, AssemblyGeneratorState& state)
{
    if (instr.arg1) {
        state.output << *instr.arg1 << ":\n";
    }
}

void handleJump(const tac::ThreeACInstruction& instr, AssemblyGeneratorState& state)
{
    if (instr.arg1) {
        state.output << "    jmp " << *instr.arg1 << "\n";
    }
}

void handleJumpIf(const tac::ThreeACInstruction& instr, AssemblyGeneratorState& state)
{
    if (!instr.arg1 || !instr.arg2)
        return;

    state.output << "    mov rax, qword [rbp - " << state.getVarOffset(*instr.arg1) << "]\n";
    state.output << "    cmp rax, 0\n";
    state.output << "    jne " << *instr.arg2 << "\n";
}

void handleJumpIfNot(const tac::ThreeACInstruction& instr, AssemblyGeneratorState& state)
{
    if (!instr.arg1 || !instr.arg2)
        return;

    state.output << "    mov rax, qword [rbp - " << state.getVarOffset(*instr.arg1) << "]\n";
    state.output << "    cmp rax, 0\n";
    state.output << "    je " << *instr.arg2 << "\n";
}

void handleGC(const tac::ThreeACInstruction& instr, AssemblyGeneratorState& state)
{
    state.output << "    ; GC instruction - to be implemented\n";
}

} // namespace assembly
