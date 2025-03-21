#pragma once
#include "ANF.h"
#include <optional>
#include <string>
#include <unordered_map>
#include <vector>

namespace tac {

enum class Operation {

    COPY,
    LABEL,
    JUMP,
    CALL,
    JUMP_IF,
    JUMP_IF_NOT,
    ALLOC,
    LOAD,
    STORE,
    GC
};

std::string operationToString(Operation op);

class ThreeACInstruction {
public:
    Operation op;
    std::optional<std::string> result;
    std::optional<std::string> arg1;
    std::optional<std::string> arg2;
    std::string toString() const;
    void toString(std::stringstream& ss) const;
};

class ThreeAddressModule {
public:
    std::vector<ThreeACInstruction> instructions;
    std::unordered_map<std::string, size_t> functionOffsets;
    std::string toString() const;
    void addInstr(const ThreeACInstruction instr);
};

ThreeAddressModule anfToTac(const std::vector<std::shared_ptr<ir::TopLevel>>& toplevel);
void convertANF(const std::shared_ptr<ir::ANF>& anf, ThreeAddressModule& module, std::string& result);

}
