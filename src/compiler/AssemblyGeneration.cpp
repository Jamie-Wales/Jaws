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
    };

    auto it = primitiveMap.find(primitive);
    return it != primitiveMap.end() ? it->second : primitive;
}

void generateAssembly(const tac::ThreeAddressModule& module, const std::string& outputPath)
{
    AssemblyGeneratorState state;
    std::unordered_set<std::string> variables;
    state.output << "section .data\n";
    for (const auto& instr : module.instructions) {
        if (instr.arg1 && !isNumber(*instr.arg1) && !instr.arg1->starts_with("_t")) {
            variables.insert(*instr.arg1);
        }
    }

    for (const auto& var : variables) {
        state.output << "    str_" << var << " db \"" << var << "\", 0\n";
    }
    state.output << "\n";
    state.output << "section .text\n";
    state.output << "global main\n\n";

    state.output << "extern init_runtime\n";
    state.output << "extern cleanup_runtime\n";
    state.output << "extern gc\n";
    state.output << "extern allocate\n";
    state.output << "extern env_lookup\n";
    state.output << "extern env_define\n";
    state.output << "extern current_environment\n";
    state.output << "extern cons\n";
    state.output << "extern display\n";
    state.output << "extern intern_symbol\n\n";

    state.output << "main:\n";
    state.output << "    push rbp\n";
    state.output << "    mov rbp, rsp\n";
    state.output << "    sub rsp, 128  ; Reserve space for locals\n";
    state.output << "    call init_runtime\n";
    for (auto reg : CALLEE_SAVED) {
        state.output << "    push " << regToString(reg) << "\n";
    }
    for (const auto& instr : module.instructions) {
        convertInstruction(instr, state);
    }
    state.output << "\n    ; Zero out all local variables\n";
    state.output << "    xor rax, rax\n";
    for (const auto& [var, offset] : state.varOffsets) {
        state.output << "    mov qword [rbp - " << offset << "], rax  ; Clear " << var << "\n";
    }
    state.output << "\n    ; Function epilogue\n";
    for (auto it = CALLEE_SAVED.rbegin(); it != CALLEE_SAVED.rend(); ++it) {
        state.output << "    pop " << regToString(*it) << "\n";
    }
    state.output << "    call gc\n";
    state.output << "    mov rdi, qword [rel current_environment]\n";
    state.output << "    call cleanup_runtime\n";
    state.output << "    mov rsp, rbp\n";
    state.output << "    pop rbp\n";
    state.output << "    ret\n";
    std::ofstream outFile(outputPath);
    if (!outFile) {
        throw std::runtime_error("Failed to open output file: " + outputPath);
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

    state.output << "    ; DEBUG: Processing call instruction:\n";
    state.output << "    ; " << instr.toString();
    state.output << "    ; DEBUG: Function: " << funcName << ", param count: " << paramCount << "\n";

    state.output << "    and rsp, -16\n";
    if (instr.result) {
        // Get the current temp number
        int currTempNum = std::stoi(instr.result->substr(2));
        state.output << "    ; DEBUG: Current temp _t" << currTempNum << "\n";

        // Parameters should come from previous temps based on parameter count
        for (int i = 0; i < paramCount && i < 6; i++) {
            // For _t0 with 2 params: need currTempNum + 1 and currTempNum + 2
            // For _t3 with 1 param: need _t4
            std::string paramTemp = "_t" + std::to_string(currTempNum + i + 1);
            state.output << "    ; DEBUG: Parameter " << i << " from " << paramTemp << "\n";
            state.output << "    mov " << regToString(PARAM_REGISTERS[i])
                         << ", qword [rbp - " << state.getVarOffset(paramTemp) << "]\n";
        }
    }

    state.output << "    call " << funcName << "\n";

    if (instr.result) {
        state.output << "    ; DEBUG: Storing result in " << *instr.result << "\n";
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
    case tac::Operation::LCALL:
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
    state.output << "    call allocate\n";
    state.output << "    mov qword [rbp - " << state.getVarOffset(*instr.result) << "], rax\n";
    state.output << "    pop rdx\n";
    state.output << "    pop rcx\n";
}

void handleStore(const tac::ThreeACInstruction& instr, AssemblyGeneratorState& state)
{
    if (!instr.arg1 || !instr.arg2)
        return;

    if (!instr.arg1->starts_with("_t")) {
        // For named variables, use environment
        state.output << "    lea rdi, [rel str_" << *instr.arg1 << "]\n";
        state.output << "    call intern_symbol\n";
        state.output << "    push rax  ; Save symbol\n";

        if (isNumber(*instr.arg2)) {
            state.output << "    mov rdi, 0  ; TYPE_NUMBER\n";
            state.output << "    mov rsi, " << *instr.arg2 << "\n";
            state.output << "    call allocate\n";
        } else {
            state.output << "    mov rax, qword [rbp - " << state.getVarOffset(*instr.arg2) << "]\n";
        }
        state.output << "    push rax  ; Save value\n";

        state.output << "    mov rdi, qword [rel current_environment]\n";
        state.output << "    pop rdx  ; Value\n";
        state.output << "    pop rsi  ; Symbol\n";
        state.output << "    call env_define\n";
    } else {
        // For temp variables, use stack directly
        if (isNumber(*instr.arg2)) {
            state.output << "    mov rdi, 0  ; TYPE_NUMBER\n";
            state.output << "    mov rsi, " << *instr.arg2 << "\n";
            state.output << "    call allocate\n";
            state.output << "    mov qword [rbp - " << state.getVarOffset(*instr.arg1) << "], rax\n";
        } else {
            state.output << "    mov rax, qword [rbp - " << state.getVarOffset(*instr.arg2) << "]\n";
            state.output << "    mov qword [rbp - " << state.getVarOffset(*instr.arg1) << "], rax\n";
        }
    }
}

void handleCopy(const tac::ThreeACInstruction& instr, AssemblyGeneratorState& state)
{
    if (!instr.result || !instr.arg1)
        return;
    if (isNumber(*instr.arg1)) {
        state.output << "    mov rdi, 0  ; TYPE_NUMBER\n";
        state.output << "    mov rsi, " << *instr.arg1 << "\n";
        state.output << "    call allocate\n";
    } else if (instr.arg1->starts_with("_t") || instr.arg1->starts_with("temp")) {
        state.output << "    mov rax, qword [rbp - " << state.getVarOffset(*instr.arg1) << "]\n";
    } else {
        state.output << "    lea rdi, [rel str_" << *instr.arg1 << "]\n";
        state.output << "    call intern_symbol\n";
        state.output << "    mov rsi, rax  ; Symbol\n";
        state.output << "    mov rdi, qword [rel current_environment]\n";
        state.output << "    call env_lookup\n";
    }

    // Store the result (always on stack for temp vars)
    state.output << "    mov qword [rbp - " << state.getVarOffset(*instr.result) << "], rax\n";
}

void handleLoad(const tac::ThreeACInstruction& instr, AssemblyGeneratorState& state)
{
    if (!instr.result || !instr.arg1)
        return;

    state.output << "    mov rdi, qword [rel current_environment]\n";
    state.output << "    mov rsi, qword [rbp - " << state.getVarOffset(*instr.arg1) << "]  ; Symbol\n";
    state.output << "    call env_lookup\n";
    state.output << "    mov qword [rbp - " << state.getVarOffset(*instr.result) << "], rax\n";
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
}
