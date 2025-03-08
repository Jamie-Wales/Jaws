#include "QBEGenerator.h"
#include <fstream>

std::string convertPrimitive(const std::string& primitive)
{
    if (primitive == "+")
        return "scheme_add";
    if (primitive == "-")
        return "scheme_subtract";
    if (primitive == "*")
        return "scheme_multiply";
    if (primitive == "/")
        return "scheme_divide";
    return primitive;
}

void generateQBEIr(const tac::ThreeAddressModule& module, const std::string& outputPath)
{
    QBEGeneratorState state;

    state.output << "target aarch64\n\n";
    state.output << "export function w $main() {\n";
    state.output << "@start\n";

    for (const auto& instr : module.instructions) {
        convertInstruction(instr, state);
    }

    state.output << "    ret 0\n";
    state.output << "}\n";

    std::ofstream outFile(outputPath);
    outFile << state.output.str();
}

void handleCopy(const tac::ThreeACInstruction& instr, QBEGeneratorState& state)
{
    if (!instr.result || !instr.arg1)
        return;

    if (isNumber(*instr.arg1)) {
        // For numbers, allocate a new scheme object
        state.output << "    %t" << *instr.result << "_type =l copy 0    # TYPE_NUMBER\n";
        state.output << "    %t" << *instr.result << "_val =l copy " << *instr.arg1 << "\n";
        state.output << "    %" << *instr.result << " =l call $allocate(l %t" << *instr.result << "_type, l %t" << *instr.result << "_val)\n";
    } else {
        // For variables, just copy the pointer
        state.output << "    %" << *instr.result << " =l copy %" << *instr.arg1 << "\n";
    }
}

void handleCall(const tac::ThreeACInstruction& instr, QBEGeneratorState& state)
{
    if (!instr.arg1 || !instr.result)
        return;

    int paramCount = instr.arg2 ? std::stoi(*instr.arg2) : 0;

    // Convert primitive operation names to scheme runtime functions
    std::string funcName = convertPrimitive(*instr.arg1);

    // Generate call
    state.output << "    %" << *instr.result << " =l call $" << funcName << "(";

    // Add parameters from PREVIOUS temps
    int currTemp = std::stoi(instr.result->substr(2));
    for (int i = 0; i < paramCount; i++) {
        if (i > 0)
            state.output << ", ";
        state.output << "l %_t" << (currTemp - paramCount + i);
    }
    state.output << ")\n";
}

void handleAlloc(const tac::ThreeACInstruction& instr, QBEGeneratorState& state)
{
    if (!instr.result)
        return;

    state.output << "    %t" << *instr.result << "_type =l copy 0   # TYPE_NUMBER\n";
    state.output << "    %t" << *instr.result << "_val =l copy 0    # Initial value\n";
    state.output << "    %" << *instr.result << " =l call $allocate(l %t" << *instr.result << "_type, l %t" << *instr.result << "_val)\n";
}

void convertInstruction(const tac::ThreeACInstruction& instr, QBEGeneratorState& state)
{
    state.output << "\n    # " << instr.toString();

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
    }
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
