#include "AssemblyGeneration.h"

namespace assembly {

AssemblyGeneratorState generateAssembly(const tac::ThreeAddressModule& module)
{
    AssemblyGeneratorState state;

    state.output << "section .text\n";
    state.output << "global main\n";
    state.output << "extern alloc\n";
    state.output << "extern scheme_add\n\n";
    state.output << "main:\n";
    state.output << "    push rbp\n";
    state.output << "    mov rbp, rsp\n";

    for (const auto& instr : module.instructions) {
        convertInstruction(instr, state);
    }
    state.output << "    mov rsp, rbp\n";
    state.output << "    pop rbp\n";
    state.output << "    ret\n";
    return state;
}

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
    return "";
}

void convertInstruction(const tac::ThreeACInstruction& instr, AssemblyGeneratorState& state)
{
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

    case tac::Operation::LABEL:
        if (instr.arg1) {
            state.output << *instr.arg1 << ":\n";
        }
        break;

    case tac::Operation::JUMP:
        if (instr.arg1) {
            state.output << "    jmp " << *instr.arg1 << "\n";
        }
        break;

    case tac::Operation::JUMP_IF:
        handleJumpIf(instr, state);
        break;
    }
}
void handleCopy(const tac::ThreeACInstruction& instr, AssemblyGeneratorState& state)
{
    if (instr.result && instr.arg1) {
        state.output << "    mov rax, " << *instr.arg1 << "\n";
        state.output << "    mov " << *instr.result << ", rax\n";
    }
}

void handleCall(const tac::ThreeACInstruction& instr, AssemblyGeneratorState& state)
{
    if (instr.arg1 && *instr.arg1 == "+") {
        state.output << "    ; Call to scheme_add\n";
        state.output << "    mov rdi, " << "arg0" << "\n";
        state.output << "    mov rsi, " << "arg1" << "\n";
        state.output << "    call scheme_add\n";
        if (instr.result) {
            state.output << "    mov " << *instr.result << ", rax\n";
        }
    } else {
        if (instr.arg1) {
            state.output << "    call " << *instr.arg1 << "\n";
            if (instr.result) {
                state.output << "    mov " << *instr.result << ", rax\n";
            }
        }
    }
}

void handleAlloc(const tac::ThreeACInstruction& instr, AssemblyGeneratorState& state)
{
    if (instr.result) {
        state.output << "    ; Allocate memory\n";
        state.output << "    mov rdi, 8\n";
        state.output << "    call alloc\n";
        state.output << "    mov " << *instr.result << ", rax\n";
    }
}

void handleJumpIf(const tac::ThreeACInstruction& instr, AssemblyGeneratorState& state)
{
    if (instr.arg1 && instr.arg2) {
        state.output << "    cmp " << *instr.arg1 << ", 0\n";
        state.output << "    jne " << *instr.arg2 << "\n";
    }
}
}
