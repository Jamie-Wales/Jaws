#include "QBEGenerator.h"
#include <fstream>
#include <iostream>
#include <map>
#include <set>
#include <string>
#include <unordered_map>
#include <vector>

bool isNumber(const std::string& str)
{
    if (str.empty())
        return false;
    char* end = nullptr;
    strtol(str.c_str(), &end, 10);
    return *end == '\0';
}

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

    std::set<std::string> globalVars;
    for (const auto& instr : module.instructions) {
        if (instr.op == tac::Operation::STORE && instr.arg1) {
            if (!isNumber(*instr.arg1)) {
                globalVars.insert(*instr.arg1);
            }
        }
    }

    state.output << "# QBE IR generated for Scheme compiler\n\n";

    state.output << "# Global variable declarations\n";
    for (const auto& global : globalVars) {
        state.output << "data $" << global << "_global = { l $nil_obj }\n";
    }

    state.output << "\n# Predefined Scheme objects\n";
    state.output << "data $nil_obj = { l 3, b 0 }\n";
    state.output << "data $true_obj = { l 4, b 1 }\n";
    state.output << "data $false_obj = { l 4, b 0 }\n";

    std::map<std::string, std::vector<int>> functionBlocks;
    for (int i = 0; i < module.instructions.size(); i++) {
        const auto& instr = module.instructions[i];
        if (instr.op == tac::Operation::LABEL && instr.arg1 && instr.arg1->substr(0, 1) == "L") {
            std::string label = *instr.arg1;
            functionBlocks[label].push_back(i);
        }
    }

    for (const auto& [label, indices] : functionBlocks) {
        if (module.functionOffsets.find(label) != module.functionOffsets.end()) {
            state.output << "\nfunction l $" << label << "() {\n";
            state.output << "@start\n";

            int start = indices[0];
            int end = (module.functionOffsets.find(label) != module.functionOffsets.end()) ? module.functionOffsets.at(label) : module.instructions.size() - 1;

            for (int i = start + 1; i <= end; i++) {
                convertInstruction(module.instructions[i], state, globalVars);
            }

            state.output << "    ret %return\n";
            state.output << "}\n";
        }
    }

    state.output << "\nexport function w $main() {\n";
    state.output << "@start\n";
    state.output << "    call $init_runtime()\n\n";

    bool inLambda = false;
    for (int i = 0; i < module.instructions.size(); i++) {
        const auto& instr = module.instructions[i];

        if (instr.op == tac::Operation::LABEL && instr.arg1 && instr.arg1->substr(0, 1) == "L") {
            std::string label = *instr.arg1;
            if (module.functionOffsets.find(label) != module.functionOffsets.end()) {
                inLambda = true;
            }
        }

        if (!inLambda) {
            convertInstruction(instr, state, globalVars);
        }

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

void convertInstruction(const tac::ThreeACInstruction& instr, QBEGeneratorState& state, const std::set<std::string>& globalVars)
{
    state.output << "\n    # " << instr.toString();

    switch (instr.op) {
    case tac::Operation::COPY:
        handleCopy(instr, state, globalVars);
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
        handleJumpIf(instr, state, globalVars);
        break;
    case tac::Operation::JUMP_IF_NOT:
        handleJumpIfNot(instr, state, globalVars);
        break;
    case tac::Operation::ALLOC:
        handleAlloc(instr, state);
        break;
    case tac::Operation::LOAD:
        handleLoad(instr, state, globalVars);
        break;
    case tac::Operation::STORE:
        handleStore(instr, state, globalVars);
        break;
    case tac::Operation::GC:
        handleGC(instr, state);
        break;
    default:
        state.output << "    # Unhandled operation: " << static_cast<int>(instr.op) << "\n";
    }
}

void handleCopy(const tac::ThreeACInstruction& instr, QBEGeneratorState& state, const std::set<std::string>& globalVars)
{
    if (!instr.result || !instr.arg1)
        return;

    std::string resultVar = *instr.result;
    std::string sourceVar = *instr.arg1;

    if (globalVars.find(sourceVar) != globalVars.end()) {
        state.output << "    # Load from global variable: " << sourceVar << "\n";
        state.output << "    %" << resultVar << " =l loadl $" << sourceVar << "_global\n";
        state.tempMap[resultVar] = resultVar;
    } else if (isNumber(sourceVar)) {
        state.output << "    %t" << state.tempCount << " =l copy 0    # TYPE_NUMBER\n";
        state.output << "    %t" << (state.tempCount + 1) << " =l copy " << sourceVar << "\n";
        state.output << "    %" << resultVar << " =l call $allocate(l %t" << state.tempCount
                     << ", l %t" << (state.tempCount + 1) << ")\n";
        state.tempCount += 2;
        state.tempMap[resultVar] = resultVar;
    } else if (sourceVar == "#t") {
        state.output << "    %" << resultVar << " =l copy $true_obj\n";
        state.tempMap[resultVar] = resultVar;
    } else if (sourceVar == "#f") {
        state.output << "    %" << resultVar << " =l copy $false_obj\n";
        state.tempMap[resultVar] = resultVar;
    } else if (sourceVar == "()") {
        state.output << "    %" << resultVar << " =l copy $nil_obj\n";
        state.tempMap[resultVar] = resultVar;
    } else {
        state.output << "    %" << resultVar << " =l copy %" << sourceVar << "\n";
        state.tempMap[resultVar] = resultVar;
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

    for (int i = 0; i < 10; i++) {
        std::string tempName = "_t" + std::to_string(i);
        std::cerr << "DEBUG: Temp " << tempName << " is in map: "
                  << (state.tempMap.find(tempName) != state.tempMap.end() ? "yes" : "no") << "\n";
    }

    if (instr.result) {
        state.output << "    %" << *instr.result << " =l call " << funcName << "(";
        for (int i = 0; i < paramCount; i++) {
            if (i > 0)
                state.output << ", ";

            int paramIndex = 0;
            if (instr.result && instr.result->substr(0, 2) == "_t") {
                int resultIndex = std::stoi(instr.result->substr(2));
                paramIndex = resultIndex - paramCount + i;
                std::cerr << "DEBUG: For param " << i << ", using _t" << paramIndex << "\n";
            }

            state.output << "l %_t" << paramIndex;
        }
        state.output << ")\n";
    } else {
        state.output << "    call " << funcName << "(";
        for (int i = 0; i < paramCount; i++) {
            if (i > 0)
                state.output << ", ";
            state.output << "l %_t" << i;
        }
        state.output << ")\n";
    }

    if (instr.result) {
        state.tempMap[*instr.result] = *instr.result;
    }
}

void handleJumpIf(const tac::ThreeACInstruction& instr, QBEGeneratorState& state, const std::set<std::string>& globalVars)
{
    if (!instr.arg1 || !instr.arg2)
        return;

    std::string condVar = *instr.arg1;

    if (globalVars.find(condVar) != globalVars.end()) {
        state.output << "    # Load condition from global variable: " << condVar << "\n";
        state.output << "    %cond_val" << state.tempCount << " =l loadl $" << condVar << "_global\n";
        condVar = "cond_val" + std::to_string(state.tempCount);
        state.tempCount++;
    }

    state.output << "    %cond" << state.tempCount << " =l ceql %" << condVar << ", $nil_obj\n";
    state.output << "    %cond" << (state.tempCount + 1) << " =l ceql %" << condVar << ", $false_obj\n";
    state.output << "    %cond" << (state.tempCount + 2) << " =l or %cond"
                 << state.tempCount << ", %cond" << (state.tempCount + 1) << "\n";
    state.output << "    jnz %cond" << (state.tempCount + 2) << ", @" << *instr.arg2 << ", @next" << state.labelCount << "\n";
    state.output << "@next" << state.labelCount << "\n";

    state.tempCount += 3;
    state.labelCount++;
}

void handleJumpIfNot(const tac::ThreeACInstruction& instr, QBEGeneratorState& state, const std::set<std::string>& globalVars)
{
    if (!instr.arg1 || !instr.arg2)
        return;

    std::string condVar = *instr.arg1;

    if (globalVars.find(condVar) != globalVars.end()) {
        state.output << "    # Load condition from global variable: " << condVar << "\n";
        state.output << "    %cond_val" << state.tempCount << " =l loadl $" << condVar << "_global\n";
        condVar = "cond_val" + std::to_string(state.tempCount);
        state.tempCount++;
    }

    state.output << "    %cond" << state.tempCount << " =l ceql %" << condVar << ", $nil_obj\n";
    state.output << "    %cond" << (state.tempCount + 1) << " =l ceql %" << condVar << ", $false_obj\n";
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

    std::string resultVar = *instr.result;

    if (*instr.arg1 == "closure") {
        state.output << "    %" << resultVar << " =l call $make_closure(l $" << *instr.arg2 << ")\n";
    } else if (*instr.arg1 == "literal") {
        state.output << "    %t" << state.tempCount << " =l copy 0    # TYPE_NUMBER (literal)\n";
        state.output << "    %t" << (state.tempCount + 1) << " =l copy 0    # Value 0 for now\n";
        state.output << "    %" << resultVar << " =l call $allocate(l %t" << state.tempCount
                     << ", l %t" << (state.tempCount + 1) << ")\n";
        state.tempCount += 2;
    } else {
        state.output << "    %t" << state.tempCount << " =l copy 0    # TYPE_NUMBER\n";
        state.output << "    %t" << (state.tempCount + 1) << " =l copy 0    # Value 0\n";
        state.output << "    %" << resultVar << " =l call $allocate(l %t" << state.tempCount
                     << ", l %t" << (state.tempCount + 1) << ")\n";
        state.tempCount += 2;
    }

    // Store result in tempMap
    state.tempMap[resultVar] = resultVar;
}

void handleLoad(const tac::ThreeACInstruction& instr, QBEGeneratorState& state, const std::set<std::string>& globalVars)
{
    if (!instr.result || !instr.arg1)
        return;

    std::string resultVar = *instr.result;
    std::string srcVar = *instr.arg1;

    if (globalVars.find(srcVar) != globalVars.end()) {
        state.output << "    # Load from global variable: " << srcVar << "\n";
        state.output << "    %" << resultVar << " =l loadl $" << srcVar << "_global\n";
    } else {
        state.output << "    %" << resultVar << " =l copy %" << srcVar << "\n";
    }

    // Store result in tempMap
    state.tempMap[resultVar] = resultVar;
}

void handleStore(const tac::ThreeACInstruction& instr, QBEGeneratorState& state, const std::set<std::string>& globalVars)
{
    if (!instr.arg1 || !instr.arg2)
        return;

    std::string targetVar = *instr.arg1;
    std::string valueStr = *instr.arg2;

    if (isNumber(valueStr)) {
        state.output << "    # Create number object for: " << valueStr << "\n";
        state.output << "    %t" << state.tempCount << " =l copy 0   # TYPE_NUMBER\n";
        state.output << "    %t" << (state.tempCount + 1) << " =l copy " << valueStr << "\n";
        state.output << "    %value_obj" << state.tempCount << " =l call $allocate(l %t"
                     << state.tempCount << ", l %t" << (state.tempCount + 1) << ")\n";

        if (globalVars.find(targetVar) != globalVars.end()) {
            state.output << "    # Store to global variable: " << targetVar << "\n";
            state.output << "    storel %value_obj" << state.tempCount << ", $" << targetVar << "_global\n";
        } else {
            state.output << "    # Store to local variable: " << targetVar << "\n";
            state.output << "    %" << targetVar << " =l copy %value_obj" << state.tempCount << "\n";
            state.tempMap[targetVar] = targetVar;
        }

        state.tempCount += 2;
    } else {
        if (globalVars.find(targetVar) != globalVars.end()) {
            state.output << "    # Store variable to global: " << targetVar << "\n";
            state.output << "    storel %" << valueStr << ", $" << targetVar << "_global\n";
        } else {
            state.output << "    # Store to local variable: " << targetVar << "\n";
            state.output << "    %" << targetVar << " =l copy %" << valueStr << "\n";
            state.tempMap[targetVar] = targetVar;
        }
    }
}

void handleGC(const tac::ThreeACInstruction& instr, QBEGeneratorState& state)
{
    state.output << "    call $gc()\n";
}
