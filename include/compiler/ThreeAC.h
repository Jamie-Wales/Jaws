#pragma once
#include "ANF.h"
#include <list>
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
    P_APP,
    GC,
    PARAM,
    RETURN,
    FUNC_BEGIN,
    FUNC_END,

};

std::string operationToString(Operation op);

using Scope = std::unordered_map<std::string, std::string>;
struct DeferredFunction {
    std::string label;
    std::vector<std::string> paramNames;
    std::shared_ptr<ir::ANF> body;
    Scope definitionScope; // Scope where lambda was defined (for free vars)
};
class ThreeACInstruction {
public:
    Operation op;
    std::optional<std::string> result;
    std::optional<std::string> arg1;
    std::optional<std::string> arg2;
    std::string toString() const;
    void toString(std::stringstream& ss) const;
};
struct ThreeAddressModule {
    std::vector<ThreeACInstruction> instructions;
    std::list<DeferredFunction> deferredFunctions; // <-- ADDED

    void addInstr(const ThreeACInstruction instr);
    std::string toString() const;
};

ThreeAddressModule anfToTac(const std::vector<std::shared_ptr<ir::TopLevel>>& toplevel);
void convertANF(const std::shared_ptr<ir::ANF>& anf, ThreeAddressModule& module, std::string& result);

}
