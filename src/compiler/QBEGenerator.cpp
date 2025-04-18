// compiler/QBEGenerator.cpp

#include "QBEGenerator.h" // Include the corresponding header
#include "ThreeAC.h" // Include ThreeAC definitions

#include <algorithm> // Required for std::find_if, std::replace
#include <cctype> // For isalnum
#include <fstream>
#include <iostream>
#include <map>
#include <set>
#include <sstream> // Required for istringstream and ostringstream
#include <string>
#include <unordered_map>
#include <vector>

namespace qbe {
// Helper to check if a string represents a number
bool isNumber(const std::string& str)
{
    if (str.empty())
        return false;
    char* end = nullptr;
    strtod(str.c_str(), &end);
    return end != str.c_str() && *end == '\0';
}

// Helper to split strings by a delimiter
std::vector<std::string> splitString(const std::string& str, char delimiter)
{
    std::vector<std::string> tokens;
    std::string token;
    std::istringstream tokenStream(str);
    while (std::getline(tokenStream, token, delimiter)) {
        token.erase(0, token.find_first_not_of(" \t\n\r\f\v"));
        token.erase(token.find_last_not_of(" \t\n\r\f\v") + 1);
        if (!token.empty()) {
            tokens.push_back(token);
        }
    }
    return tokens;
}

// Function to get the QBE representation of a TAC variable/literal/primitive
std::string getQbeOperand(const std::string& tacOperand, QBEGeneratorState& state, const std::set<std::string>& globalVars)
{
    if (tacOperand.empty()) {
        state.output << "    # ERROR: Empty TAC operand encountered.\n";
        return "%error_empty_operand";
    }

    static const std::unordered_map<std::string, std::string> primitiveMap = {
        { "+", "$plus" },
        { "-", "$minus" },
        { "*", "$multiply" },
        { "/", "$divide" },
        { "eq?", "$eq_pred" },
        { "cons", "$cons" },
        { "car", "$car" },
        { "cdr", "$cdr" },
        { "display", "$display" },
        { "newline", "$newline" },
    };
    auto primitiveIt = primitiveMap.find(tacOperand);
    if (primitiveIt != primitiveMap.end()) {
        return primitiveIt->second;
    }

    if (state.tempMap.count(tacOperand)) {
        return "%" + state.tempMap.at(tacOperand);
    }

    if (tacOperand == "#t")
        return "$true_obj";
    if (tacOperand == "#f")
        return "$false_obj";
    if (tacOperand == "()")
        return "$nil_obj";

    if (isNumber(tacOperand)) {
        std::string numObjTemp = "%tmp_num_" + std::to_string(state.tempCount++);
        std::string typeReg = "%t_num_type" + std::to_string(state.tempCount);
        std::string valReg = "%t_num_val" + std::to_string(state.tempCount + 1);
        state.output << "    " << typeReg << " =l copy 0 # TYPE_NUMBER\n";
        state.output << "    " << valReg << " =l copy " << tacOperand << " # Number value\n";
        state.output << "    " << numObjTemp << " =l call $allocate(l " << typeReg
                     << ", l " << valReg << ") # Allocate number obj for " << tacOperand << "\n";
        state.tempCount += 2;
        return numObjTemp;
    }

    if (tacOperand.rfind("L", 0) == 0) {
        return "$" + tacOperand;
    }

    if (tacOperand.rfind("_t", 0) == 0 || tacOperand.rfind("g", 0) == 0) {
        return "%" + tacOperand;
    }

    return tacOperand;
}

// --- Handler Functions ---
// ... (handleCopy, handleLabel, handleJump, etc. unchanged) ...
void handleCopy(const tac::ThreeACInstruction& instr, QBEGeneratorState& state, const std::set<std::string>& globalVars)
{
    if (!instr.result || !instr.arg1) {
        state.output << "    # ERROR: Invalid COPY (missing args).\n";
        return;
    }
    std::string resultVarTac = *instr.result;
    std::string sourceOperandTac = *instr.arg1;
    std::string resultRegQbe = "%" + resultVarTac;
    std::string qbeSourceOperand = getQbeOperand(sourceOperandTac, state, globalVars);

    if (qbeSourceOperand.empty() || qbeSourceOperand.find("error") != std::string::npos) {
        state.output << "    # ERROR: Cannot get QBE source for '" << sourceOperandTac << "' in COPY.\n";
        state.output << "    " << resultRegQbe << " =l copy $nil_obj # Error default\n";
    } else if (qbeSourceOperand == sourceOperandTac && qbeSourceOperand.find('%') != 0 && qbeSourceOperand.find('$') != 0) {
        state.output << "    # ERROR: Attempted COPY on variable '" << sourceOperandTac << "'. Use LOAD.\n";
        state.output << "    " << resultRegQbe << " =l copy $nil_obj # Error default\n";
    } else {
        state.output << "    " << resultRegQbe << " =l copy " << qbeSourceOperand << "\n";
    }
    state.tempMap[resultVarTac] = resultVarTac;
}
void handleLabel(const tac::ThreeACInstruction& instr, QBEGeneratorState& state)
{
    if (!instr.arg1) {
        state.output << "# ERROR: LABEL missing name.\n";
        return;
    }
    state.output << "@" << *instr.arg1 << "\n";
}
void handleJump(const tac::ThreeACInstruction& instr, QBEGeneratorState& state)
{
    if (!instr.arg1) {
        state.output << "    # ERROR: JUMP missing target.\n";
        return;
    }
    state.output << "    jmp @" << *instr.arg1 << "\n";
}
void handleParam(const tac::ThreeACInstruction& instr, QBEGeneratorState& state)
{
    if (!instr.arg1) {
        state.output << "    # ERROR: PARAM missing argument.\n";
        return;
    }
    state.pendingParams.push_back(*instr.arg1);
}
void handleReturn(const tac::ThreeACInstruction& instr, QBEGeneratorState& state, const std::set<std::string>& globalVars)
{
    if (instr.arg1) {
        std::string returnValueTac = *instr.arg1;
        std::string qbeReturnValue = getQbeOperand(returnValueTac, state, globalVars);
        if (qbeReturnValue.empty() || qbeReturnValue.find("error") != std::string::npos) {
            state.output << "    # ERROR: Cannot get QBE return value for '" << returnValueTac << "'.\n";
            state.output << "    ret $nil_obj # Error default\n";
        } else {
            state.output << "    ret " << qbeReturnValue << "\n";
        }
    } else {
        state.output << "    ret $nil_obj # Default nil return\n";
    }
}
void handleFuncBegin(const tac::ThreeACInstruction& instr, QBEGeneratorState& state)
{
    // TODO: Needs enhancement for environment frame setup (prologue)
    if (!instr.arg1 || !instr.arg2) {
        state.output << "# ERROR: FUNC_BEGIN missing args.\n";
        return;
    }
    state.generatingFunction = true;
    state.currentFuncParamNamesTAC.clear();
    state.tempMap.clear();
    std::string funcLabel = *instr.arg1;
    std::string paramNamesStr = *instr.arg2;
    std::vector<std::string> tacParamNames = splitString(paramNamesStr, ',');
    state.currentFuncParamNamesTAC = tacParamNames;

    state.output << "\nfunction l $" << funcLabel << "(";
    std::vector<std::string> qbeParamRegs;
    for (int i = 0; i < tacParamNames.size(); ++i) {
        if (i > 0)
            state.output << ", ";
        std::string qbeParam = "%p" + std::to_string(i);
        state.output << "l " << qbeParam;
        qbeParamRegs.push_back(qbeParam);
    }
    state.output << ") {\n";
    state.output << "@start_" << funcLabel << " # Function entry block\n";

    if (qbeParamRegs.size() == tacParamNames.size()) {
        for (int i = 0; i < tacParamNames.size(); ++i) {
            std::string tacParamName = tacParamNames[i];
            std::string qbeSourceReg = qbeParamRegs[i];
            std::string qbeDestReg = "%" + tacParamName;
            state.output << "    " << qbeDestReg << " =l copy " << qbeSourceReg << " # Map param " << i << " (" << tacParamName << ")\n";
            state.tempMap[tacParamName] = tacParamName;
        }
    } else {
        state.output << "    # ERROR: Mismatched param count in $" << funcLabel << ".\n";
    }
}
void handleFuncEnd(const tac::ThreeACInstruction& instr, QBEGeneratorState& state)
{
    if (!instr.arg1) {
        state.output << "# ERROR: FUNC_END missing label.\n";
        return;
    }
    // TODO: Needs enhancement for environment frame teardown (epilogue)
    state.output << "} # End of function $" << *instr.arg1 << "\n";
    state.generatingFunction = false;
    state.currentFuncParamNamesTAC.clear();
}
void handleJumpIf(const tac::ThreeACInstruction& instr, QBEGeneratorState& state, const std::set<std::string>& globalVars)
{
    if (!instr.arg1 || !instr.arg2) {
        state.output << "    # ERROR: JUMP_IF missing args.\n";
        return;
    }
    std::string condVarTac = *instr.arg1;
    std::string thenLabel = *instr.arg2;
    std::string elseLabel = "next_" + std::to_string(state.labelCount++);
    std::string qbeCondVar = getQbeOperand(condVarTac, state, globalVars);
    if (qbeCondVar.empty() || qbeCondVar.find("error") != std::string::npos) {
        state.output << "    # ERROR: Cannot get QBE condition for '" << condVarTac << "' in JUMP_IF.\n";
        state.output << "    jmp @" << elseLabel << "\n";
        state.output << "@" << elseLabel << "\n";
        return;
    }
    std::string isFalseReg = "%is_false_" + std::to_string(state.tempCount++);
    state.output << "    " << isFalseReg << " =l ceql " << qbeCondVar << ", $false_obj # 1 if #f, 0 otherwise\n";
    state.output << "    jnz " << isFalseReg << ", @" << elseLabel << ", @" << thenLabel << "\n";
    state.output << "@" << elseLabel << " # Start of path if condition was false\n";
}
void handleJumpIfNot(const tac::ThreeACInstruction& instr, QBEGeneratorState& state, const std::set<std::string>& globalVars)
{
    if (!instr.arg1 || !instr.arg2) {
        state.output << "    # ERROR: JUMP_IF_NOT missing args.\n";
        return;
    }
    std::string condVarTac = *instr.arg1;
    std::string elseLabel = *instr.arg2;
    std::string thenLabel = "next_" + std::to_string(state.labelCount++);
    std::string qbeCondVar = getQbeOperand(condVarTac, state, globalVars);
    if (qbeCondVar.empty() || qbeCondVar.find("error") != std::string::npos) {
        state.output << "    # ERROR: Cannot get QBE condition for '" << condVarTac << "' in JUMP_IF_NOT.\n";
        state.output << "    jmp @" << thenLabel << "\n";
        state.output << "@" << thenLabel << "\n";
        return;
    }
    std::string isFalseReg = "%is_false_" + std::to_string(state.tempCount++);
    state.output << "    " << isFalseReg << " =l ceql " << qbeCondVar << ", $false_obj # 1 if #f, 0 otherwise\n";
    state.output << "    jnz " << isFalseReg << ", @" << elseLabel << ", @" << thenLabel << "\n";
    state.output << "@" << thenLabel << " # Start of path if condition was true\n";
}
void handleAlloc(const tac::ThreeACInstruction& instr, QBEGeneratorState& state)
{
    if (!instr.result || !instr.arg1) {
        state.output << "    # ERROR: ALLOC missing args.\n";
        return;
    }
    std::string resultVar = *instr.result;
    std::string allocType = *instr.arg1;
    std::string resultReg = "%" + resultVar;

    if (allocType == "closure") {
        if (!instr.arg2) {
            state.output << "    # ERROR: ALLOC closure missing function label.\n";
            state.output << "    " << resultReg << " =l copy $nil_obj\n";
        } else {
            std::string funcLabel = *instr.arg2;
            state.output << "    " << resultReg << " =l call $make_closure(l $" << funcLabel << ")\n";
        }
    } else if (allocType == "literal") {
        std::string quotedString = instr.arg2 ? *instr.arg2 : "()";
        state.output << "    # TODO: Allocate literal object for: " << quotedString << "\n";
        state.output << "    " << resultReg << " =l copy $nil_obj # Placeholder\n";
    } else {
        state.output << "    # ERROR: Unhandled ALLOC type: " << allocType << "\n";
        state.output << "    " << resultReg << " =l copy $nil_obj\n";
    }
    state.tempMap[resultVar] = resultVar;
}
void handleLoad(const tac::ThreeACInstruction& instr, QBEGeneratorState& state, const std::set<std::string>& globalVars)
{
    if (!instr.result || !instr.arg1) {
        state.output << "    # ERROR: LOAD missing args.\n";
        return;
    }
    std::string resultVarTac = *instr.result;
    std::string sourceVarTac = *instr.arg1;
    std::string resultRegQbe = "%" + resultVarTac;
    std::string label_str = sourceVarTac;
    std::replace(label_str.begin(), label_str.end(), '-', '_');
    std::string symbolNameData = "$str_" + label_str;

    state.output << "    # LOAD: Look up variable '" << sourceVarTac << "'\n";
    std::string symbolObjReg = "%tmp_symbol_" + std::to_string(state.tempCount++);
    state.output << "    " << symbolObjReg << " =l call $intern_symbol(l " << symbolNameData << ") # Get symbol object\n";
    std::string envPtrReg = "%tmp_env_ptr_" + std::to_string(state.tempCount++);
    state.output << "    " << envPtrReg << " =l loadl $g_current_environment # Load current env pointer\n";
    state.output << "    " << resultRegQbe << " =l call $env_lookup(l " << envPtrReg << ", l " << symbolObjReg << ") # Look up in env\n";
    state.tempMap[resultVarTac] = resultVarTac;
}
void handleStore(const tac::ThreeACInstruction& instr, QBEGeneratorState& state, const std::set<std::string>& globalVars)
{
    if (!instr.arg1 || !instr.arg2) {
        state.output << "    # ERROR: STORE missing args.\n";
        return;
    }
    std::string targetVarTac = *instr.arg1;
    std::string valueSourceTac = *instr.arg2;
    std::string qbeValueSource = getQbeOperand(valueSourceTac, state, globalVars);
    if (qbeValueSource.empty() || qbeValueSource.find("error") != std::string::npos) {
        state.output << "    # ERROR: Cannot get QBE value for '" << valueSourceTac << "' in STORE.\n";
        return;
    }
    std::string label_str = targetVarTac;
    std::replace(label_str.begin(), label_str.end(), '-', '_');
    std::string symbolNameData = "$str_" + label_str;

    state.output << "    # STORE: Define variable '" << targetVarTac << "'\n";
    std::string symbolObjReg = "%tmp_symbol_" + std::to_string(state.tempCount++);
    state.output << "    " << symbolObjReg << " =l call $intern_symbol(l " << symbolNameData << ") # Get symbol object\n";
    std::string envPtrReg = "%tmp_env_ptr_" + std::to_string(state.tempCount++);
    state.output << "    " << envPtrReg << " =l loadl $g_current_environment # Load current env pointer\n";
    state.output << "    call $env_define(l " << envPtrReg << ", l " << symbolObjReg << ", l " << qbeValueSource << ") # Define in env\n";
}
void handleGC(const tac::ThreeACInstruction& instr, QBEGeneratorState& state)
{
    state.output << "    call $gc()\n";
}

// --- Updated handleCall ---
void handleCall(const tac::ThreeACInstruction& instr, QBEGeneratorState& state, const std::set<std::string>& globalVars)
{
    if (!instr.arg1) {
        state.output << "    # ERROR: CALL missing target.\n";
        return;
    }
    std::string funcTargetTac = *instr.arg1;
    std::string qbeFuncTargetOperand; // Final target for the 'call' instr ($L0, $plus, %code_ptr)
    std::string qbeEnvArgument = ""; // Holds 'env %reg' part, NO trailing comma

    std::string resolvedTarget = getQbeOperand(funcTargetTac, state, globalVars);

    // Case 1: Target is a primitive or direct function label ($...)
    if (resolvedTarget.rfind('$', 0) == 0) {
        qbeFuncTargetOperand = resolvedTarget;
        // No environment passed to primitives/direct labels by default
    }
    // Case 2: Target is already a temporary register (%) holding a closure/function pointer
    else if (resolvedTarget.rfind('%', 0) == 0) {
        std::string closurePtrReg = resolvedTarget;
        // Load code pointer (offset 8) and env pointer (offset 16) from closure object
        std::string codePtrAddrReg = "%tmp_code_addr_" + std::to_string(state.tempCount++);
        std::string codePtrReg = "%tmp_code_ptr_" + std::to_string(state.tempCount++);
        state.output << "    " << codePtrAddrReg << " =l add " << closurePtrReg << ", 8\n";
        state.output << "    " << codePtrReg << " =l loadl " << codePtrAddrReg << " # Load code pointer\n";
        std::string envPtrAddrReg = "%tmp_cenv_addr_" + std::to_string(state.tempCount++);
        std::string capturedEnvReg = "%tmp_cap_env_" + std::to_string(state.tempCount++);
        state.output << "    " << envPtrAddrReg << " =l add " << closurePtrReg << ", 16\n";
        state.output << "    " << capturedEnvReg << " =l loadl " << envPtrAddrReg << " # Load captured env pointer\n";

        qbeFuncTargetOperand = codePtrReg; // Call the loaded code pointer
        qbeEnvArgument = "env " + capturedEnvReg; // Pass captured env (NO trailing comma)
    }
    // Case 3: Target is a variable name (lookup needed)
    else if (resolvedTarget == funcTargetTac) {
        std::string label_str = funcTargetTac;
        std::replace(label_str.begin(), label_str.end(), '-', '_');
        std::string symbolNameData = "$str_" + label_str;
        std::string symbolObjReg = "%tmp_symbol_" + std::to_string(state.tempCount++);
        state.output << "    " << symbolObjReg << " =l call $intern_symbol(l " << symbolNameData << ")\n";
        std::string envPtrReg = "%tmp_env_ptr_" + std::to_string(state.tempCount++);
        state.output << "    " << envPtrReg << " =l loadl $g_current_environment\n";
        std::string closurePtrReg = "%tmp_func_ptr_" + std::to_string(state.tempCount++); // Holds the closure obj
        state.output << "    " << closurePtrReg << " =l call $env_lookup(l " << envPtrReg << ", l " << symbolObjReg << ")\n";

        // Load code pointer (offset 8) and env pointer (offset 16) from looked-up closure
        std::string codePtrAddrReg = "%tmp_code_addr_" + std::to_string(state.tempCount++);
        std::string codePtrReg = "%tmp_code_ptr_" + std::to_string(state.tempCount++);
        state.output << "    " << codePtrAddrReg << " =l add " << closurePtrReg << ", 8\n";
        state.output << "    " << codePtrReg << " =l loadl " << codePtrAddrReg << " # Load code pointer\n";
        std::string envPtrAddrReg = "%tmp_cenv_addr_" + std::to_string(state.tempCount++);
        std::string capturedEnvReg = "%tmp_cap_env_" + std::to_string(state.tempCount++);
        state.output << "    " << envPtrAddrReg << " =l add " << closurePtrReg << ", 16\n";
        state.output << "    " << capturedEnvReg << " =l loadl " << envPtrAddrReg << " # Load captured env pointer\n";

        qbeFuncTargetOperand = codePtrReg; // Call the loaded code pointer
        qbeEnvArgument = "env " + capturedEnvReg; // Pass captured env (NO trailing comma)
    } else {
        state.output << "    # ERROR: Unknown call target type for '" << funcTargetTac << "'.\n";
        qbeFuncTargetOperand = "$error_call_target";
    }

    // Build argument list string for regular parameters
    std::string callArgs = "";
    for (size_t i = 0; i < state.pendingParams.size(); ++i) {
        if (i > 0)
            callArgs += ", "; // Add comma separator between regular args
        std::string tacParamSource = state.pendingParams[i];
        std::string qbeParamOperand = getQbeOperand(tacParamSource, state, globalVars);
        if (qbeParamOperand.empty() || qbeParamOperand.find("error") != std::string::npos) {
            state.output << "    # ERROR: Cannot get QBE operand for param " << i << " ('" << tacParamSource << "').\n";
            callArgs += "l $nil_obj";
        } else {
            callArgs += "l " + qbeParamOperand; // Assume type 'l'
        }
    }

    // Construct the final call expression
    std::string callExpr = "call " + qbeFuncTargetOperand + "(";
    if (!qbeEnvArgument.empty()) {
        callExpr += qbeEnvArgument; // Add "env %reg"
        if (!callArgs.empty()) {
            callExpr += ", "; // Add comma ONLY if env AND regular args exist
        }
    }
    callExpr += callArgs + ")"; // Add regular args

    // Handle return value assignment
    if (instr.result) {
        std::string resultReg = "%" + *instr.result;
        state.output << "    " << resultReg << " =l " << callExpr << "\n";
        state.tempMap[*instr.result] = *instr.result;
    } else {
        state.output << "    " << callExpr << " # Side effect call\n";
    }
    state.pendingParams.clear();
}

// Main function to convert TAC module to QBE IR
void generateQBEIr(const tac::ThreeAddressModule& module, const std::string& outputPath)
{
    QBEGeneratorState state;
    std::set<std::string> globalVars;
    std::set<std::string> stringLiteralsForSymbols;

    // Pass 1: Collect required string literals for symbols
    for (const auto& instr : module.instructions) {
        if ((instr.op == tac::Operation::STORE || instr.op == tac::Operation::LOAD) && instr.arg1) {
            const std::string& name = *instr.arg1;
            if (!name.empty() && name[0] != '_' && name[0] != 'g' && name[0] != 'L' && name[0] != '#' && !isNumber(name)) {
                stringLiteralsForSymbols.insert(name);
            }
        }
        if (instr.op == tac::Operation::CALL && instr.arg1) {
            const std::string& name = *instr.arg1;
            static const std::unordered_map<std::string, std::string> primitiveMap = { { "+", "$plus" }, { "-", "$minus" }, { "*", "$multiply" }, { "/", "$divide" }, { "eq?", "$eq_pred" }, { "cons", "$cons" }, { "car", "$car" }, { "cdr", "$cdr" }, { "display", "$display" }, { "newline", "$newline" } };
            if (primitiveMap.find(name) == primitiveMap.end() && name.rfind("L", 0) != 0 && name.rfind("_t", 0) != 0 && name.rfind("g", 0) != 0 && name.rfind('%', 0) != 0 && name.rfind('$', 0) != 0) {
                stringLiteralsForSymbols.insert(name);
            }
        }
    }

    state.output << "# QBE IR generated by Scheme compiler\n\n";
    state.output << "# === Data Section ===\n\n";
    state.output << "# Predefined Scheme objects\n";
    state.output << "data $nil_obj = { l 3, b 0 }\n";
    state.output << "data $true_obj = { l 4, b 1 }\n";
    state.output << "data $false_obj = { l 4, b 0 }\n";
    // Note: $g_current_environment is external, defined in C runtime
    state.output << "\n# String literals for symbols (referenced by $intern_symbol)\n";
    for (const auto& str : stringLiteralsForSymbols) {
        std::string sanitized_str = str;
        size_t pos = 0;
        while ((pos = sanitized_str.find('\\', pos)) != std::string::npos) {
            sanitized_str.replace(pos, 1, "\\\\");
            pos += 2;
        }
        pos = 0;
        while ((pos = sanitized_str.find('"', pos)) != std::string::npos) {
            sanitized_str.replace(pos, 1, "\\\"");
            pos += 2;
        }
        std::string label_str = str;
        std::replace(label_str.begin(), label_str.end(), '-', '_'); // QBE labels use _
        state.output << "data $str_" << label_str << " = { b \"" << sanitized_str << "\", b 0 }\n";
    }
    state.output << "\n# === End of Data Section ===\n\n";

    // Emit Functions
    bool inFunction = false;
    for (int i = 0; i < module.instructions.size(); ++i) {
        const auto& instr = module.instructions[i];
        if (instr.op == tac::Operation::FUNC_BEGIN) {
            inFunction = true;
            convertInstruction(instr, state, globalVars);
        } else if (instr.op == tac::Operation::FUNC_END) {
            if (inFunction) {
                convertInstruction(instr, state, globalVars);
                inFunction = false;
            } else {
                state.output << "# WARN: FUNC_END outside function.\n";
            }
        } else if (inFunction) {
            convertInstruction(instr, state, globalVars);
        }
    }
    if (inFunction) {
        state.output << "# ERROR: Unterminated function block.\n";
    }

    // Emit Main Function
    state.output << "\nexport function w $main() { # Main entry point\n";
    state.output << "@start_main\n";
    state.output << "    call $init_runtime()\n\n";
    state.tempMap.clear();
    state.generatingFunction = false;
    for (int i = 0; i < module.instructions.size(); ++i) {
        const auto& instr = module.instructions[i];
        if (instr.op == tac::Operation::FUNC_BEGIN) {
            std::string funcLabel = instr.arg1 ? *instr.arg1 : "UNKNOWN";
            int j = i + 1;
            while (j < module.instructions.size()) {
                if (module.instructions[j].op == tac::Operation::FUNC_END && module.instructions[j].arg1 && *module.instructions[j].arg1 == funcLabel) {
                    i = j;
                    break;
                }
                j++;
            }
            if (j == module.instructions.size()) {
                state.output << "# ERROR: Unmatched FUNC_BEGIN $" << funcLabel << ".\n";
            }
            continue;
        }
        if (instr.op == tac::Operation::FUNC_END)
            continue;
        convertInstruction(instr, state, globalVars);
    }
    state.output << "\n    call $gc() # Final GC before exit\n";
    state.output << "    ret 0 # Exit code 0\n";
    state.output << "} # End of $main\n";

    // Write to File
    std::ofstream outFile(outputPath);
    if (!outFile) {
        std::cerr << "Error: Cannot open QBE output file: " << outputPath << std::endl;
        return;
    }
    outFile << state.output.str();
    outFile.close();
    if (!outFile) {
        std::cerr << "Error: Failed write to QBE output file: " << outputPath << std::endl;
    }
}

// Central dispatcher
void convertInstruction(const tac::ThreeACInstruction& instr, QBEGeneratorState& state, const std::set<std::string>& globalVars)
{
    if (instr.op != tac::Operation::LABEL) {
        state.output << "\n    # TAC: " << instr.toString() << "\n";
    }
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
        handleCall(instr, state, globalVars);
        break; // Uses updated handleCall
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
    case tac::Operation::PARAM:
        handleParam(instr, state);
        break;
    case tac::Operation::RETURN:
        handleReturn(instr, state, globalVars);
        break;
    case tac::Operation::FUNC_BEGIN:
        handleFuncBegin(instr, state);
        break;
    case tac::Operation::FUNC_END:
        handleFuncEnd(instr, state);
        break;
    default:
        state.output << "    # ERROR: Unhandled TAC operation: " << static_cast<int>(instr.op) << "\n";
    }
}

} // namespace qbe
