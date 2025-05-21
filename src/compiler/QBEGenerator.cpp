#include "QBEGenerator.h"
#include "ThreeAC.h"

#include <algorithm>
#include <cctype>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <map>
#include <set>
#include <sstream>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

namespace qbe {

std::string getQbeOperand(const std::string& tacOperand, QBEGeneratorState& state);
void handleLoad(const tac::ThreeACInstruction& instr, QBEGeneratorState& state); // Declare handleLoad

bool isNumber(const std::string& str)
{
    if (str.empty())
        return false;
    char* end = nullptr;
    strtod(str.c_str(), &end);
    return end != str.c_str() && *end == '\0';
}

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

std::string getQbeOperand(const std::string& tacOperand, QBEGeneratorState& state)
{
    if (tacOperand.empty()) {
        return "%error_empty_operand";
    }

    static const std::unordered_map<std::string, std::string> primitiveMap = {
        { "+", "$plus" }, { "-", "$minus" }, { "*", "$multiply" }, { "/", "$divide" },
        { "=", "$eq_num" }, { "eq?", "$eq_pred" }, { "eqv?", "$eqv_pred" }, { "equal?", "$equal_pred" },
        { "cons", "$cons" }, { "car", "$car" }, { "cdr", "$cdr" },
        { "display", "$display" }, { "newline", "$newline" },
        { "vector", "$make_vector" },
        { "allocate", "$allocate" }, { "make_closure", "$make_closure" },
        { "intern_symbol", "$intern_symbol" }, { "env_lookup", "$env_lookup" },
        { "env_define", "$env_define" }, { "gc", "$gc" },
        { "init_runtime", "$init_runtime" },
        { "allocate_literal", "$allocate_literal" }
    };
    auto primitiveIt = primitiveMap.find(tacOperand);
    if (primitiveIt != primitiveMap.end()) {
        return primitiveIt->second;
    }

    if (tacOperand == "#t")
        return "$true_obj";
    if (tacOperand == "#f")
        return "$false_obj";
    if (tacOperand == "()")
        return "$nil_obj";

    if (isNumber(tacOperand)) {
        std::string numObjTemp = "%tmp_num_" + std::to_string(state.tempCount++);
        int type_number_enum_value = 0;
        state.output << "    " << numObjTemp << " =l call $allocate(l "
                     << type_number_enum_value << ", l " << tacOperand
                     << ") # Allocate number obj for " << tacOperand << "\n";
        return numObjTemp;
    }

    if (tacOperand.rfind('L', 0) == 0 && tacOperand.length() > 1 && std::isdigit(tacOperand[1])) {
        return "$" + tacOperand;
    }

    if (tacOperand.rfind("_t", 0) == 0) {
        return "%" + tacOperand;
    }
    if (tacOperand.rfind("temp", 0) == 0) {
        return "%" + tacOperand;
    }

    return tacOperand;
}

void handleCopy(const tac::ThreeACInstruction& instr, QBEGeneratorState& state)
{
    if (!instr.result || !instr.arg1)
        return;
    std::string resultRegQbe = getQbeOperand(*instr.result, state);
    std::string qbeSourceOperand = getQbeOperand(*instr.arg1, state);

    if (qbeSourceOperand.empty() || qbeSourceOperand.find("error") != std::string::npos) {
        state.output << "    # ERROR: Invalid source for COPY: " << *instr.arg1 << "\n";
        state.output << "    " << resultRegQbe << " =l copy $nil_obj # Error default\n";
    } else if (qbeSourceOperand[0] == '%' || qbeSourceOperand[0] == '$') {
        state.output << "    " << resultRegQbe << " =l copy " << qbeSourceOperand << "\n";
    } else {
        state.output << "    # WARN: COPY source '" << *instr.arg1 << "' is not a temp/literal/primitive. Assuming LOAD was intended.\n";
        std::string loadedValueReg = "%tmp_copy_load_" + std::to_string(state.tempCount++);
        tac::ThreeACInstruction loadInstr = { tac::Operation::LOAD, loadedValueReg, *instr.arg1, {} };
        handleLoad(loadInstr, state);
        state.output << "    " << resultRegQbe << " =l copy " << loadedValueReg << "\n";
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

void handleParam(const tac::ThreeACInstruction& instr, QBEGeneratorState& state)
{
    if (!instr.arg1)
        return;
    state.pendingParamsTac.push_back(*instr.arg1);
}

void handleReturn(const tac::ThreeACInstruction& instr, QBEGeneratorState& state)
{
    if (instr.arg1) {
        std::string qbeReturnValue = getQbeOperand(*instr.arg1, state);
        if (qbeReturnValue.empty() || qbeReturnValue.find("error") != std::string::npos) {
            state.output << "    # ERROR: Cannot get QBE return value for '" << *instr.arg1 << "'.\n";
            state.output << "    ret $nil_obj # Error default\n";
        } else if (qbeReturnValue[0] == '%' || qbeReturnValue[0] == '$') {
            state.output << "    ret " << qbeReturnValue << "\n";
        } else {
            state.output << "    # WARN: RETURN value '" << *instr.arg1 << "' is not a temp/literal/primitive. Assuming LOAD was intended.\n";
            std::string loadedValueReg = "%tmp_ret_load_" + std::to_string(state.tempCount++);
            tac::ThreeACInstruction loadInstr = { tac::Operation::LOAD, loadedValueReg, *instr.arg1, {} };
            handleLoad(loadInstr, state);
            state.output << "    ret " << loadedValueReg << "\n";
        }
    } else {
        state.output << "    ret $nil_obj # Default nil return\n";
    }
}

void handleFuncBegin(const tac::ThreeACInstruction& instr, QBEGeneratorState& state)
{
    if (!instr.arg1) {
        state.output << "# ERROR: FUNC_BEGIN missing label.\n";
        return;
    }

    state.isInsideFunction = true;
    state.currentFuncLabel = *instr.arg1;
    state.tempMap.clear();

    std::vector<std::string> tacParamNames;
    if (instr.arg2) {
        tacParamNames = splitString(*instr.arg2, ',');
    }

    state.output << "\nfunction l $" << state.currentFuncLabel << "(env %" << state.currentFuncLabel << "_env";

    std::vector<std::string> qbeParamRegs;
    for (int i = 0; i < tacParamNames.size(); ++i) {
        state.output << ", ";
        std::string qbeParam = "%p" + std::to_string(i);
        state.output << "l " << qbeParam;
        qbeParamRegs.push_back(qbeParam);
    }
    state.output << ") {\n";
    state.output << "@start_" << state.currentFuncLabel << " # Function entry block\n";

    if (qbeParamRegs.size() == tacParamNames.size()) {
        for (int i = 0; i < tacParamNames.size(); ++i) {
            std::string tacParamName = tacParamNames[i];
            std::string qbeSourceReg = qbeParamRegs[i];
            std::string qbeDestReg = "%" + tacParamName;
            state.output << "    " << qbeDestReg << " =l copy " << qbeSourceReg << " # Map param " << i << " (" << tacParamName << ")\n";
        }
    } else {
        state.output << "    # ERROR: Mismatched param count in $" << state.currentFuncLabel << ".\n";
    }
}

void handleFuncEnd(const tac::ThreeACInstruction& instr, QBEGeneratorState& state)
{
    if (!instr.arg1 || *instr.arg1 != state.currentFuncLabel) {
        state.output << "# ERROR: FUNC_END label mismatch or missing (" << (instr.arg1 ? *instr.arg1 : "NULL") << " vs " << state.currentFuncLabel << ").\n";
    }
    state.output << "} # End of function $" << state.currentFuncLabel << "\n";
    state.isInsideFunction = false;
    state.currentFuncLabel = "";
}

void handleJumpIf(const tac::ThreeACInstruction& instr, QBEGeneratorState& state)
{
    if (!instr.arg1 || !instr.arg2)
        return;
    std::string condVarTac = *instr.arg1;
    std::string thenLabel = *instr.arg2;
    std::string elseLabel = "L_else_" + std::to_string(state.labelCount++);

    std::string qbeCondVar = getQbeOperand(condVarTac, state);

    if (qbeCondVar.empty() || qbeCondVar.find("error") != std::string::npos) {
        state.output << "    # ERROR: Invalid condition for JUMP_IF: " << condVarTac << "\n";
        return;
    } else if (qbeCondVar[0] != '%' && qbeCondVar[0] != '$') {
        // Condition is a variable name - needs LOAD first
        state.output << "    # WARN: JUMP_IF condition '" << condVarTac << "' is not a temp/literal/primitive. Assuming LOAD was intended.\n";
        std::string loadedCondReg = "%tmp_jif_load_" + std::to_string(state.tempCount++);
        tac::ThreeACInstruction loadInstr = { tac::Operation::LOAD, loadedCondReg, condVarTac, {} };
        handleLoad(loadInstr, state); // Simulate LOAD
        qbeCondVar = loadedCondReg;
    }

    std::string isFalseReg = "%is_false_" + std::to_string(state.tempCount++);
    state.output << "    " << isFalseReg << " =l ceql " << qbeCondVar << ", $false_obj # 1 if #f, 0 otherwise\n";
    state.output << "    jnz " << isFalseReg << ", @" << elseLabel << ", @" << thenLabel << "\n";
    state.output << "@" << elseLabel << " # Fallthrough path if condition was false\n";
}

void handleJumpIfNot(const tac::ThreeACInstruction& instr, QBEGeneratorState& state)
{
    if (!instr.arg1 || !instr.arg2)
        return;
    std::string condVarTac = *instr.arg1;
    std::string elseLabel = *instr.arg2;
    std::string thenLabel = "L_then_" + std::to_string(state.labelCount++);

    std::string qbeCondVar = getQbeOperand(condVarTac, state);

    if (qbeCondVar.empty() || qbeCondVar.find("error") != std::string::npos) {
        state.output << "    # ERROR: Invalid condition for JUMP_IF_NOT: " << condVarTac << "\n";
        return;
    } else if (qbeCondVar[0] != '%' && qbeCondVar[0] != '$') {
        state.output << "    # WARN: JUMP_IF_NOT condition '" << condVarTac << "' is not a temp/literal/primitive. Assuming LOAD was intended.\n";
        std::string loadedCondReg = "%tmp_jnif_load_" + std::to_string(state.tempCount++);
        tac::ThreeACInstruction loadInstr = { tac::Operation::LOAD, loadedCondReg, condVarTac, {} };
        handleLoad(loadInstr, state); // Simulate LOAD
        qbeCondVar = loadedCondReg;
    }

    std::string isFalseReg = "%is_false_" + std::to_string(state.tempCount++);
    state.output << "    " << isFalseReg << " =l ceql " << qbeCondVar << ", $false_obj # 1 if #f, 0 otherwise\n";
    state.output << "    jnz " << isFalseReg << ", @" << elseLabel << ", @" << thenLabel << "\n";
    state.output << "@" << thenLabel << " # Fallthrough path if condition was true\n";
}

void handleAlloc(const tac::ThreeACInstruction& instr, QBEGeneratorState& state)
{
    if (!instr.result || !instr.arg1)
        return;
    std::string resultReg = getQbeOperand(*instr.result, state);
    std::string allocType = *instr.arg1;

    if (allocType == "closure") {
        if (!instr.arg2)
            return;
        std::string funcLabel = *instr.arg2;
        std::string qbeFuncLabel = getQbeOperand(funcLabel, state);
        state.output << "    " << resultReg << " =l call $make_closure(l " << qbeFuncLabel << ")\n";
    } else if (allocType == "literal") {
        std::string quotedString = instr.arg2 ? *instr.arg2 : "()";
        std::string dataLabel = "$str_lit_" + std::to_string(state.tempCount++);
        state.stringLiteralsForData.insert({ dataLabel, quotedString });
        state.output << "    " << resultReg << " =l call $allocate_literal(l " << dataLabel << ") # Placeholder\n";
    } else {
        state.output << "    # ERROR: Unhandled ALLOC type: " << allocType << "\n";
        state.output << "    " << resultReg << " =l copy $nil_obj # Error default\n";
    }
}

void handleLoad(const tac::ThreeACInstruction& instr, QBEGeneratorState& state)
{
    if (!instr.result || !instr.arg1) {
        state.output << "    # ERROR: Invalid LOAD instruction (missing result or arg1).\n";
        return;
    }

    std::string resultReg = getQbeOperand(*instr.result, state);
    std::string varName = *instr.arg1;
    if (varName.empty() || varName[0] == '%' || varName[0] == '$' || isNumber(varName) || varName == "#t" || varName == "#f" || varName == "()") {
        state.output << "    # ERROR: LOAD source '" << varName << "' is not a valid variable name.\n";
        state.output << "    " << resultReg << " =l copy $nil_obj # Error default\n";
        return;
    }

    std::string label_str = varName;
    std::replace(label_str.begin(), label_str.end(), '-', '_');
    std::string symbolNameData = "$str_" + label_str;
    state.stringLiteralsForSymbols.insert(varName);

    std::string symbolObjReg = "%tmp_symbol_" + std::to_string(state.tempCount++);
    state.output << "    " << symbolObjReg << " =l call $intern_symbol(l " << symbolNameData << ") # Get symbol for '" << varName << "'\n";

    std::string envPtrReg;
    if (state.isInsideFunction) {
        envPtrReg = "%" + state.currentFuncLabel + "_env";
    } else {
        envPtrReg = "%tmp_global_env_" + std::to_string(state.tempCount++);
        state.output << "    " << envPtrReg << " =l loadl $g_current_environment # Load global env pointer\n";
    }

    state.output << "    " << resultReg << " =l call $env_lookup(l " << envPtrReg << ", l " << symbolObjReg << ") # Look up '" << varName << "'\n";
}

void handleStore(const tac::ThreeACInstruction& instr, QBEGeneratorState& state)
{
    if (!instr.arg1 || !instr.arg2)
        return;

    std::string targetVarName = *instr.arg1;
    std::string valueSourceTac = *instr.arg2;

    if (targetVarName.empty() || targetVarName[0] == '%' || targetVarName[0] == '$' || isNumber(targetVarName) || targetVarName == "#t" || targetVarName == "#f" || targetVarName == "()") {
        state.output << "    # ERROR: STORE target '" << targetVarName << "' is not a valid variable name.\n";
        return;
    }

    std::string qbeValueSource = getQbeOperand(valueSourceTac, state);
    if (qbeValueSource.empty() || qbeValueSource.find("error") != std::string::npos) {
        state.output << "    # ERROR: Invalid source for STORE: " << valueSourceTac << "\n";
        return;
    } else if (qbeValueSource[0] != '%' && qbeValueSource[0] != '$') {
        state.output << "    # WARN: STORE source '" << valueSourceTac << "' is not a temp/literal/primitive. Assuming LOAD was intended.\n";
        std::string loadedValueReg = "%tmp_store_load_" + std::to_string(state.tempCount++);
        tac::ThreeACInstruction loadInstr = { tac::Operation::LOAD, loadedValueReg, valueSourceTac, {} };
        handleLoad(loadInstr, state);
        qbeValueSource = loadedValueReg;
    }

    std::string label_str = targetVarName;
    std::replace(label_str.begin(), label_str.end(), '-', '_');
    std::string symbolNameData = "$str_" + label_str;
    state.stringLiteralsForSymbols.insert(targetVarName);

    std::string symbolObjReg = "%tmp_symbol_" + std::to_string(state.tempCount++);
    state.output << "    " << symbolObjReg << " =l call $intern_symbol(l " << symbolNameData << ") # Get symbol for '" << targetVarName << "'\n";

    std::string envPtrReg;
    if (state.isInsideFunction) {
        envPtrReg = "%" + state.currentFuncLabel + "_env";
    } else {
        envPtrReg = "%tmp_global_env_" + std::to_string(state.tempCount++);
        state.output << "    " << envPtrReg << " =l loadl $g_current_environment # Load global env pointer\n";
    }
    state.output << "    call $env_define(l " << envPtrReg << ", l " << symbolObjReg << ", l " << qbeValueSource << ") # Define '" << targetVarName << "'\n";
}

void handleGC(const tac::ThreeACInstruction& instr, QBEGeneratorState& state)
{
    state.output << "    call $gc()\n";
}

void handleCall(const tac::ThreeACInstruction& instr, QBEGeneratorState& state)
{
    if (!instr.arg1)
        return;

    std::string funcTargetTac = *instr.arg1;
    std::string qbeFuncTargetOperand;
    std::string qbeEnvArgument = "";
    bool isClosureCall = false;

    std::string resolvedTarget = getQbeOperand(funcTargetTac, state);

    if (resolvedTarget[0] == '$') {
        qbeFuncTargetOperand = resolvedTarget;
        if (resolvedTarget.rfind("$L", 0) == 0) {
            if (state.isInsideFunction) {
                qbeEnvArgument = "env %" + state.currentFuncLabel + "_env";
            } else {
                std::string globalEnvReg = "%tmp_global_env_" + std::to_string(state.tempCount++);
                state.output << "    " << globalEnvReg << " =l loadl $g_current_environment\n";
                qbeEnvArgument = "env " + globalEnvReg;
            }
            isClosureCall = true; // Treat direct label calls like closure calls regarding env passing
        }
    } else if (resolvedTarget[0] == '%') {
        std::string funcObjReg = resolvedTarget;
        std::string codePtrAddrReg = "%tmp_code_addr_" + std::to_string(state.tempCount++);
        std::string codePtrReg = "%tmp_code_ptr_" + std::to_string(state.tempCount++);
        state.output << "    " << codePtrAddrReg << " =l add " << funcObjReg << ", 8 # Offset to code ptr\n"; // Adjust offset if needed
        state.output << "    " << codePtrReg << " =l loadl " << codePtrAddrReg << " # Load code pointer\n";

        std::string envPtrAddrReg = "%tmp_cenv_addr_" + std::to_string(state.tempCount++);
        std::string capturedEnvReg = "%tmp_cap_env_" + std::to_string(state.tempCount++);
        state.output << "    " << envPtrAddrReg << " =l add " << funcObjReg << ", 16 # Offset to captured env\n"; // Adjust offset if needed
        state.output << "    " << capturedEnvReg << " =l loadl " << envPtrAddrReg << " # Load captured env pointer\n";

        qbeFuncTargetOperand = codePtrReg;
        std::string isNullEnvReg = "%tmp_is_null_env_" + std::to_string(state.tempCount++);
        state.output << "    " << isNullEnvReg << " =l ceql " << capturedEnvReg << ", 0\n";

        qbeEnvArgument = "env " + capturedEnvReg;
        isClosureCall = true;

    } else {
        state.output << "    # NOTE: Function target '" << funcTargetTac << "' is a variable. Performing LOAD.\n";
        std::string loadedFuncObjReg = "%tmp_call_load_" + std::to_string(state.tempCount++);
        tac::ThreeACInstruction loadInstr = { tac::Operation::LOAD, loadedFuncObjReg, funcTargetTac, {} };
        handleLoad(loadInstr, state); // Generate LOAD

        std::string funcObjReg = loadedFuncObjReg;
        std::string codePtrAddrReg = "%tmp_code_addr_" + std::to_string(state.tempCount++);
        std::string codePtrReg = "%tmp_code_ptr_" + std::to_string(state.tempCount++);
        state.output << "    " << codePtrAddrReg << " =l add " << funcObjReg << ", 8\n";
        state.output << "    " << codePtrReg << " =l loadl " << codePtrAddrReg << "\n";
        std::string envPtrAddrReg = "%tmp_cenv_addr_" + std::to_string(state.tempCount++);
        std::string capturedEnvReg = "%tmp_cap_env_" + std::to_string(state.tempCount++);
        state.output << "    " << envPtrAddrReg << " =l add " << funcObjReg << ", 16\n";
        state.output << "    " << capturedEnvReg << " =l loadl " << envPtrAddrReg << "\n";

        qbeFuncTargetOperand = codePtrReg;
        qbeEnvArgument = "env " + capturedEnvReg;
        isClosureCall = true;
    }

    std::string callArgs = "";
    for (size_t i = 0; i < state.pendingParamsTac.size(); ++i) {
        if (isClosureCall || i > 0) {
            callArgs += ", ";
        }
        std::string tacParamSource = state.pendingParamsTac[i];
        std::string qbeParamOperand = getQbeOperand(tacParamSource, state);

        if (qbeParamOperand.empty() || qbeParamOperand.find("error") != std::string::npos) {
            state.output << "    # ERROR: Cannot get QBE operand for param " << i << " ('" << tacParamSource << "').\n";
            callArgs += "l $nil_obj";
        } else if (qbeParamOperand[0] == '%' || qbeParamOperand[0] == '$') {
            callArgs += "l " + qbeParamOperand;
        } else {
            state.output << "    # WARN: PARAM source '" << tacParamSource << "' is not a temp/literal/primitive. Assuming LOAD was intended.\n";
            std::string loadedParamReg = "%tmp_param_load_" + std::to_string(state.tempCount++);
            tac::ThreeACInstruction loadInstr = { tac::Operation::LOAD, loadedParamReg, tacParamSource, {} };
            handleLoad(loadInstr, state); // Simulate LOAD
            callArgs += "l " + loadedParamReg;
        }
    }

    std::string callExpr = "call " + qbeFuncTargetOperand + "(";
    if (isClosureCall) {
        callExpr += qbeEnvArgument;
    }
    callExpr += callArgs + ")";

    if (instr.result) {
        std::string resultReg = getQbeOperand(*instr.result, state);
        state.output << "    " << resultReg << " =l " << callExpr << "\n";
    } else {
        state.output << "    " << callExpr << " # Side effect call\n";
    }
    state.pendingParamsTac.clear();
}

void convertInstruction(const tac::ThreeACInstruction& instr, QBEGeneratorState& state)
{
    if (instr.op != tac::Operation::LABEL) {
        state.output << "\n    # TAC: " << instr.toString() << "\n";
    }

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
    case tac::Operation::PARAM:
        handleParam(instr, state);
        break;
    case tac::Operation::RETURN:
        handleReturn(instr, state);
        break;
    case tac::Operation::FUNC_BEGIN:
        break;
    case tac::Operation::FUNC_END:
        break;
    default:
        state.output << "    # ERROR: Unhandled TAC operation: " << operationToString(instr.op) << "\n";
    }
}

void generateQBEIr(const tac::ThreeAddressModule& module, const std::string& outputPath)
{
    QBEGeneratorState state;
    state.isInsideFunction = false;

    for (const auto& instr : module.instructions) {
        if (instr.op == tac::Operation::STORE && instr.arg1) {
            std::string name = *instr.arg1;
            if (!name.empty() && name[0] != '%' && name[0] != '$' && !isNumber(name) && name != "#t" && name != "#f" && name != "()") {
                state.stringLiteralsForSymbols.insert(name);
            }
        } else if (instr.op == tac::Operation::LOAD && instr.arg1) {
            std::string name = *instr.arg1;
            if (!name.empty() && name[0] != '%' && name[0] != '$' && !isNumber(name) && name != "#t" && name != "#f" && name != "()") {
                state.stringLiteralsForSymbols.insert(name);
            }
        } else if (instr.op == tac::Operation::CALL && instr.arg1) {
            std::string name = *instr.arg1;
            if (!name.empty() && name[0] != '%' && name[0] != '$' && name.rfind('L', 0) != 0 && !isNumber(name) && name != "#t" && name != "#f" && name != "()") {
                static const std::unordered_set<std::string> primitives = { "+", "-", "*", "/", "=", "eq?", "eqv?", "equal?", "cons", "car", "cdr", "display", "newline", "vector" };
                if (primitives.find(name) == primitives.end()) {
                    state.stringLiteralsForSymbols.insert(name);
                }
            }
        } else if ((instr.op == tac::Operation::COPY || instr.op == tac::Operation::RETURN || instr.op == tac::Operation::JUMP_IF || instr.op == tac::Operation::JUMP_IF_NOT) && instr.arg1) {
            std::string name = *instr.arg1;
            if (!name.empty() && name[0] != '%' && name[0] != '$' && !isNumber(name) && name != "#t" && name != "#f" && name != "()") {
                state.stringLiteralsForSymbols.insert(name);
            }
        } else if (instr.op == tac::Operation::STORE && instr.arg2) {
            std::string name = *instr.arg2;
            if (!name.empty() && name[0] != '%' && name[0] != '$' && !isNumber(name) && name != "#t" && name != "#f" && name != "()") {
                state.stringLiteralsForSymbols.insert(name);
            }
        } else if (instr.op == tac::Operation::PARAM && instr.arg1) {
            std::string name = *instr.arg1;
            if (!name.empty() && name[0] != '%' && name[0] != '$' && !isNumber(name) && name != "#t" && name != "#f" && name != "()") {
                state.stringLiteralsForSymbols.insert(name);
            }
        }
    }

    state.output << "# QBE IR generated by Scheme compiler\n\n";
    state.output << "# === Data Section ===\n\n";
    state.output << "# Predefined Scheme objects (ensure these match runtime)\n";
    state.output << "data $nil_obj = { l 3, b 0 } # Type 3, value 0\n";
    state.output << "data $true_obj = { l 4, b 1 } # Type 4, value 1\n";
    state.output << "data $false_obj = { l 4, b 0 } # Type 4, value 0\n";

    state.output << "\n# === End of Data Section ===\n\n";

    state.output << "export function w $main() {\n";
    state.output << "@start_main\n";
    state.output << "    call $init_runtime()\n";
    state.tempMap.clear();
    state.isInsideFunction = false;

    int firstFunctionIdx = -1;
    for (int i = 0; i < module.instructions.size(); ++i) {
        const auto& instr = module.instructions[i];
        if (instr.op == tac::Operation::FUNC_BEGIN) {
            firstFunctionIdx = i;
            break;
        }
        convertInstruction(instr, state);
    }

    state.output << "\n    call $gc() # Final GC before exit\n";
    state.output << "    ret 0 # Exit code 0\n";
    state.output << "} # End of $main\n";

    if (firstFunctionIdx != -1) {
        state.output << "\n# === Function Definitions ===\n";
        for (int i = firstFunctionIdx; i < module.instructions.size(); ++i) {
            const auto& instr = module.instructions[i];

            if (instr.op == tac::Operation::FUNC_BEGIN) {
                handleFuncBegin(instr, state);
            } else if (instr.op == tac::Operation::FUNC_END) {
                handleFuncEnd(instr, state);
            } else if (state.isInsideFunction) {
                convertInstruction(instr, state);
            } else {
                state.output << "    # WARN: Instruction outside $main and function def at index " << i << ": " << instr.toString() << "\n";
            }
        }
        state.output << "# === End Function Definitions ===\n";
    } else {
        state.output << "\n# === No Function Definitions Found ===\n";
    }

    if (!state.stringLiteralsForData.empty()) {
        std::stringstream data_section;
        data_section << "\n# === Literal Data (Regenerated) ===\n";
        for (const auto& pair : state.stringLiteralsForData) {
            std::string sanitized_str = pair.second;
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
            data_section << "data " << pair.first << " = { b \"" << sanitized_str << "\", b 0 }\n";
        }
        data_section << "# === End Literal Data ===\n";
        state.output.str(data_section.str() + state.output.str()); // Example: Prepend
    }

    std::ofstream outFile(outputPath);
    if (!outFile) {
        std::cerr << "Error: Cannot open QBE output file: " << outputPath << std::endl;
        return;
    }
    outFile << state.output.str();
    outFile.close();
    if (!outFile) {
        std::cerr << "Error: Failed write to QBE output file: " << outputPath << std::endl;
    } else {
        std::cout << "Generated QBE IR to: " << outputPath << std::endl;
    }
}

}
