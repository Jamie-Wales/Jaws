#include "ThreeAC.h"
#include <sstream>
#include <stdexcept>
namespace tac {

std::string operationToString(Operation op)
{
    switch (op) {
    case Operation::COPY:
        return "COPY";
    case Operation::LABEL:
        return "LABEL";
    case Operation::JUMP:
        return "JUMP";
    case Operation::CALL:
        return "CALL";
    case Operation::JUMP_IF:
        return "JUMP_IF";
    case Operation::JUMP_IF_NOT:
        return "JUMP_IF_NOT";
    case Operation::ALLOC:
        return "ALLOC";
    case Operation::LOAD:
        return "LOAD";
    case Operation::STORE:
        return "STORE";
    case Operation::GC:
        return "GC";
        break;
    }
}

std::string ThreeACInstruction::toString()
{

    auto ss = std::stringstream();

    if (arg1)
        ss << *arg1 << " ";
    ss << operationToString(op) << " ";
    if (arg2)
        ss << *arg2 << " ";
    if (result)
        ss << "= " << *result;

    return ss.str();
}

void ThreeACInstruction::toString(std::stringstream& ss)
{

    if (arg1)
        ss << *arg1 << " ";
    ss << operationToString(op) << " ";
    if (arg2)
        ss << *arg2 << " ";
    if (result)
        ss << "= " << *result;
}

std::string ThreeAddressModule::toString()
{

    auto ss = std::stringstream();
    for (auto& instruction : instructions) {
        instruction.toString(ss);
    }

    return ss.str();
}
}
