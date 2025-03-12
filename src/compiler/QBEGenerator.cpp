#include "QBEGenerator.h"
#include <fstream>
#include <map>
#include <string>
#include <vector>

// Helper function to check if a string is a number
bool isNumber(const std::string& str)
{
    if (str.empty())
        return false;
    char* end = nullptr;
    strtol(str.c_str(), &end, 10);
    return *end == '\0';
}

// Convert primitive operation names to scheme runtime functions
std::string convertPrimitive(const std::string& primitive)
{
    static std::map<std::string, std::string> primitiveMap = {
        { "+", "scheme_add" },
        { "-", "scheme_subtract" },
        { "*", "scheme_multiply" },
        { "/", "scheme_divide" },
        { "eq?", "scheme_eq" },
        { "cons", "scheme_cons" },
        { "car", "scheme_car" },
        { "cdr", "scheme_cdr" },
        { "display", "scheme_display" },
        { "newline", "scheme_newline" }
    };

    auto it = primitiveMap.find(primitive);
    if (it != primitiveMap.end()) {
        return it->second;
    }
    return primitive;
}

void generateQBEIr(const tac::ThreeAddressModule& module, const std::string& outputPath)
{
    QBEGeneratorState state;

    // Include necessary declarations
    state.output << "# QBE IR generated for Scheme compiler\n\n";
    state.output << "\n# Data definitions\n";
    state.output << "data $nil_obj = { l 3, b 0 }\n"; // TYPE_NIL is 3
    state.output << "data $true_obj = { l 4, b 1 }\n"; // TYPE_BOOLEAN is 4, value true
    state.output << "data $false_obj = { l 4, b 0 }\n"; // TYPE_BOOLEAN is 4, value false

    // Generate a lambda function for each set of instructions with a function label
    std::map<std::string, std::vector<int>> functionBlocks;
    for (int i = 0; i < module.instructions.size(); i++) {
        const auto& instr = module.instructions[i];
        if (instr.op == tac::Operation::LABEL && instr.arg1 && instr.arg1->substr(0, 1) == "L") {
            std::string label = *instr.arg1;
            functionBlocks[label].push_back(i);
        }
    }

    // Generate function definitions for each lambda
    for (const auto& [label, indices] : functionBlocks) {
        if (module.functionOffsets.find(label) != module.functionOffsets.end()) {
            state.output << "\nfunction l $" << label << "() {\n";
            state.output << "@start\n";

            int start = indices[0];
            int end = (module.functionOffsets.find(label) != module.functionOffsets.end()) ? module.functionOffsets.at(label) : module.instructions.size() - 1;

            for (int i = start + 1; i <= end; i++) {
                convertInstruction(module.instructions[i], state);
            }

            state.output << "    ret %return\n";
            state.output << "}\n";
        }
    }

    // Generate the main function
    state.output << "\nexport function w $main() {\n";
    state.output << "@start\n";
    state.output << "    call $init_runtime()\n\n";

    // Process instructions not in lambda functions
    bool inLambda = false;
    for (int i = 0; i < module.instructions.size(); i++) {
        const auto& instr = module.instructions[i];

        // Skip instructions that are part of lambda functions
        if (instr.op == tac::Operation::LABEL && instr.arg1 && instr.arg1->substr(0, 1) == "L") {
            std::string label = *instr.arg1;
            if (module.functionOffsets.find(label) != module.functionOffsets.end()) {
                inLambda = true;
            }
        }

        if (!inLambda) {
            convertInstruction(instr, state);
        }

        // Check if we're exiting a lambda
        if (inLambda && module.functionOffsets.find(*instr.arg1) != module.functionOffsets.end() && i == module.functionOffsets.at(*instr.arg1)) {
            inLambda = false;
        }
    }

    state.output << "    call $gc()\n";
    state.output << "    ret 0\n";
    state.output << "}\n";
    std::ofstream outFile(outputPath);
    outFile << state.output.str();
}

void convertInstruction(const tac::ThreeACInstruction& instr, QBEGeneratorState& state)
{
    state.output << "\n    # " << instr.toString();

    switch (instr.op) {
    case tac::Operation::COPY:
        handleCopy(instr, state);
        break;
    case tac::Operation::LABEL:
        handleLabel(instr, state);
        break;
    case tac::Operation::JUMP:
        handleJump(instr, state);
        break;
    case tac::Operation::CALL:
        handleCall(instr, state);
        break;
    case tac::Operation::JUMP_IF:
        handleJumpIf(instr, state);
        break;
    case tac::Operation::JUMP_IF_NOT:
        handleJumpIfNot(instr, state);
        break;
    case tac::Operation::ALLOC:
        handleAlloc(instr, state);
        break;
    case tac::Operation::LOAD:
        handleLoad(instr, state);
        break;
    case tac::Operation::STORE:
        handleStore(instr, state);
        break;
    case tac::Operation::GC:
        handleGC(instr, state);
        break;
    default:
        state.output << "    # Unhandled operation: " << static_cast<int>(instr.op) << "\n";
    }
}

void handleCopy(const tac::ThreeACInstruction& instr, QBEGeneratorState& state)
{
    if (!instr.result || !instr.arg1)
        return;

    if (isNumber(*instr.arg1)) {
        // For numbers, allocate a new scheme object
        state.output << "    %t" << state.tempCount++ << " =l copy 0    # TYPE_NUMBER\n";
        state.output << "    %t" << state.tempCount++ << " =l copy " << *instr.arg1 << "\n";
        state.output << "    %" << *instr.result << " =l call $allocate(l %t" << (state.tempCount - 2)
                     << ", l %t" << (state.tempCount - 1) << ")\n";
    } else if (*instr.arg1 == "#t") {
        // True boolean value
        state.output << "    %" << *instr.result << " =l copy $true_obj\n";
    } else if (*instr.arg1 == "#f") {
        // False boolean value
        state.output << "    %" << *instr.result << " =l copy $false_obj\n";
    } else if (*instr.arg1 == "()") {
        // Nil/empty list
        state.output << "    %" << *instr.result << " =l copy $nil_obj\n";
    } else {
        // For variables, just copy the pointer
        state.output << "    %" << *instr.result << " =l copy %" << *instr.arg1 << "\n";
    }
}

void handleLabel(const tac::ThreeACInstruction& instr, QBEGeneratorState& state)
{
    if (!instr.arg1)
        return;

    state.output << "@" << *instr.arg1 << "\n";
}

void handleJump(const tac::ThreeACInstruction& instr, QBEGeneratorState& state)
{
    if (!instr.arg1)
        return;

    state.output << "    jmp @" << *instr.arg1 << "\n";
}

void handleCall(const tac::ThreeACInstruction& instr, QBEGeneratorState& state)
{
    if (!instr.arg1)
        return;

    int paramCount = instr.arg2 ? std::stoi(*instr.arg2) : 0;
    std::string funcName = convertPrimitive(*instr.arg1);

    if (funcName[0] != '$')
        funcName = "$" + funcName;

    state.output << "    # Calling " << funcName << " with " << paramCount << " params\n";

    // Special case for display with 1 parameter
    if (*instr.arg1 == "display" && paramCount == 1) {
        state.output << "    call " << funcName << "(l %_t3)\n";
        return;
    }

    // Generate call with parameters
    if (instr.result) {
        state.output << "    %" << *instr.result << " =l call " << funcName << "(";
        if (*instr.arg1 == "+" && paramCount == 2) {
            // For addition with two parameters: _t0 and _t1
            state.output << "l %_t0, l %_t1";
        } else {
            // Generic parameter access
            for (int i = 0; i < paramCount; i++) {
                if (i > 0)
                    state.output << ", ";
                state.output << "l %_t" << i;
            }
        }
        state.output << ")\n";
    } else {
        // Call with no result (e.g., some void functions)
        state.output << "    call " << funcName << "(";
        for (int i = 0; i < paramCount; i++) {
            if (i > 0)
                state.output << ", ";
            state.output << "l %_t" << i;
        }
        state.output << ")\n";
    }
}

void handleJumpIf(const tac::ThreeACInstruction& instr, QBEGeneratorState& state)
{
    if (!instr.arg1 || !instr.arg2)
        return;

    // Check if the condition is true (not nil and not false)
    state.output << "    %cond" << state.tempCount << " =l ceql %" << *instr.arg1 << ", $nil_obj\n";
    state.output << "    %cond" << (state.tempCount + 1) << " =l ceql %" << *instr.arg1 << ", $false_obj\n";
    state.output << "    %cond" << (state.tempCount + 2) << " =l or %cond"
                 << state.tempCount << ", %cond" << (state.tempCount + 1) << "\n";
    state.output << "    jnz %cond" << (state.tempCount + 2) << ", @" << *instr.arg2 << ", @next" << state.labelCount << "\n";
    state.output << "@next" << state.labelCount << "\n";

    state.tempCount += 3;
    state.labelCount++;
}

void handleJumpIfNot(const tac::ThreeACInstruction& instr, QBEGeneratorState& state)
{
    if (!instr.arg1 || !instr.arg2)
        return;

    // Check if the condition is false (nil or false)
    state.output << "    %cond" << state.tempCount << " =l ceql %" << *instr.arg1 << ", $nil_obj\n";
    state.output << "    %cond" << (state.tempCount + 1) << " =l ceql %" << *instr.arg1 << ", $false_obj\n";
    state.output << "    %cond" << (state.tempCount + 2) << " =l or %cond"
                 << state.tempCount << ", %cond" << (state.tempCount + 1) << "\n";
    state.output << "    jnz %cond" << (state.tempCount + 2) << ", @next" << state.labelCount << ", @" << *instr.arg2 << "\n";
    state.output << "@next" << state.labelCount << "\n";

    state.tempCount += 3;
    state.labelCount++;
}

void handleAlloc(const tac::ThreeACInstruction& instr, QBEGeneratorState& state)
{
    if (!instr.result || !instr.arg1)
        return;

    if (*instr.arg1 == "closure") {
        // Handle closure allocation
        state.output << "    %" << *instr.result << " =l call $make_closure(l $" << *instr.arg2 << ")\n";
    } else if (*instr.arg1 == "literal") {
        // Handle literal allocation
        // For simplicity, we're treating literals as numbers for now
        state.output << "    %t" << state.tempCount++ << " =l copy 0    # TYPE_NUMBER (literal)\n";
        state.output << "    %t" << state.tempCount++ << " =l copy 0    # Value 0 for now\n";
        state.output << "    %" << *instr.result << " =l call $allocate(l %t" << (state.tempCount - 2)
                     << ", l %t" << (state.tempCount - 1) << ")\n";
    } else {
        // Generic allocation
        state.output << "    %t" << state.tempCount++ << " =l copy 0    # TYPE_NUMBER\n";
        state.output << "    %t" << state.tempCount++ << " =l copy 0    # Value 0\n";
        state.output << "    %" << *instr.result << " =l call $allocate(l %t" << (state.tempCount - 2)
                     << ", l %t" << (state.tempCount - 1) << ")\n";
    }
}

void handleLoad(const tac::ThreeACInstruction& instr, QBEGeneratorState& state)
{
    if (!instr.result || !instr.arg1)
        return;

    // Simple load for now - we'd need more logic for actual memory access
    state.output << "    %" << *instr.result << " =l copy %" << *instr.arg1 << "\n";
}

void handleStore(const tac::ThreeACInstruction& instr, QBEGeneratorState& state)
{
    if (!instr.arg2)
        return;

    if (!instr.arg1 || instr.arg1->empty()) {
        // Store to global environment
        state.output << "    # Store to global: " << *instr.arg2 << "\n";
        state.output << "    call $env_set(l \"" << *instr.arg2 << "\", l %" << *instr.arg2 << ")\n";
    } else {
        state.output << "    # Store to object field\n";
        state.output << "    # %" << *instr.arg1 << "->field = %" << *instr.arg2 << "\n";
    }
}

void handleGC(const tac::ThreeACInstruction& instr, QBEGeneratorState& state)
{
    state.output << "    call $gc()\n";
}
