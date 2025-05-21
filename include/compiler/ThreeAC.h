#pragma once

#include "ANF.h"
#include "Token.h"
#include <memory>
#include <optional>
#include <set>
#include <sstream>
#include <string>
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
    GC,
    PARAM,
    RETURN,
    FUNC_BEGIN,
    FUNC_END,
    ENV_LOOKUP
};

std::string operationToString(Operation op);

struct ThreeACInstruction {
    Operation op;
    std::optional<std::string> result;
    std::optional<std::string> arg1;
    std::optional<std::string> arg2;

    std::string toString() const;
    void toString(std::stringstream& ss) const;
};

struct ThreeAddressModule {
    std::vector<ThreeACInstruction> instructions;

    void addInstr(ThreeACInstruction instr);
    std::string toString() const;
};

ThreeAddressModule anfToTac(const std::vector<std::shared_ptr<ir::TopLevel>>& toplevel);

}
