#include "QBEGenerator.h" // Assuming this includes ThreeAC.h, QBEGeneratorState, etc.
#include "ThreeAC.h" // For Operation enum and instruction structure
#include <algorithm>
#include <cctype> // For std::isalnum, std::isdigit
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

namespace qbe {

void handleAlloc(const tac::ThreeACInstruction& instr, QBEGeneratorState& state);
void handleFuncBegin(const tac::ThreeACInstruction& instr, QBEGeneratorState& state);
void handleReturn(const tac::ThreeACInstruction& instr, QBEGeneratorState& state);
void handleFuncEnd(const tac::ThreeACInstruction& instr, QBEGeneratorState& state);
void handleCopy(const tac::ThreeACInstruction& instr, QBEGeneratorState& state);
void handleLoad(const tac::ThreeACInstruction& instr, QBEGeneratorState& state);
void handleStore(const tac::ThreeACInstruction& instr, QBEGeneratorState& state);
void handlePrimitiveCall(const tac::ThreeACInstruction& instr, QBEGeneratorState& state);
void handleCall(const tac::ThreeACInstruction& instr, QBEGeneratorState& state);
void handleParam(const tac::ThreeACInstruction& instr, QBEGeneratorState& state);
void handleLabel(const tac::ThreeACInstruction& instr, QBEGeneratorState& state);
void handleJump(const tac::ThreeACInstruction& instr, QBEGeneratorState& state);
void handleJumpIf(const tac::ThreeACInstruction& instr, QBEGeneratorState& state);
void handleJumpIfNot(const tac::ThreeACInstruction& instr, QBEGeneratorState& state);
std::string getQbeOperand(const std::string& tacOperand, QBEGeneratorState& state, bool createSchemeObjectsForLiterals = true);
std::string prepareQbeCallArguments(QBEGeneratorState& state, const std::vector<std::string>& pendingParamsTacInput);
void handleTailCall(const tac::ThreeACInstruction& instr, QBEGeneratorState& state);
void handleTailCallSelf(const tac::ThreeACInstruction& instr, QBEGeneratorState& state);

bool isNumber(const std::string& str)
{
    if (str.empty())
        return false;
    char* end = nullptr;
    strtod(str.c_str(), &end);
    return end != str.c_str() && *end == '\0';
}

std::string sanitizeLabel(const std::string& str)
{
    std::string result;
    if (str.empty())
        return "_empty_";

    for (char c : str) {
        if (std::isalnum(c)) {
            result += c;
        } else if (c == '_') {
            result += c;
        } else {
            result += '_';
        }
    }
    if (!result.empty() && std::isdigit(result[0])) {
        result = "_" + result;
    }
    return result.empty() ? "_" : result;
}

// Utility: Escape string for QBE byte data
std::string escapeString(const std::string& str)
{
    std::string result;
    for (char c : str) {
        if (c == '"')
            result += "\\\"";
        else if (c == '\\')
            result += "\\\\";
        else if (c == '\n')
            result += "\\n";
        else if (c == '\t')
            result += "\\t";
        else
            result += c;
    }
    return result;
}

std::string getQbeOperand(const std::string& tacOperand, QBEGeneratorState& state, bool createSchemeObjectsForLiterals)
{
    if (tacOperand.empty())
        return "$nil_obj";
    if (tacOperand == "#t")
        return "$true_obj";
    if (tacOperand == "#f")
        return "$false_obj";
    if (tacOperand == "()")
        return "$nil_obj";

    if (isNumber(tacOperand)) {
        if (createSchemeObjectsForLiterals) {
            // Check if we've already created and cached this number literal's QBE temporary
            auto it = state.numberLiteralCache.find(tacOperand);
            if (it != state.numberLiteralCache.end()) {
                // state.output << "    # Using cached QBE reg for number: " << tacOperand << " -> " << it->second << "\n";
                return it->second; // Return the cached QBE temporary name (e.g., %num_lit_0)
            }

            // Not cached, so create it
            std::string numReg = "%num_lit_" + sanitizeLabel(tacOperand) + "_" + std::to_string(state.tempCount++); // Make temp name more specific
            state.output << "    " << numReg << " =l call $allocate(l 0, l " << tacOperand << ") # TYPE_NUMBER is 0\n";

            // Cache it for reuse
            state.numberLiteralCache[tacOperand] = numReg;
            // state.output << "    # Caching QBE reg for number: " << tacOperand << " -> " << numReg << "\n";
            return numReg;
        }
        return tacOperand;
    }

    if (tacOperand.length() >= 2 && tacOperand.front() == '"' && tacOperand.back() == '"') {
        std::string content = tacOperand.substr(1, tacOperand.length() - 2);
        auto it = state.stringLiteralMap.find(content);
        if (it != state.stringLiteralMap.end()) {
            return it->second;
        }
        std::string newStrLabel = "$strlit_" + std::to_string(state.stringLiteralCounter++);
        state.stringLiteralMap[content] = newStrLabel;
        state.stringLiteralsForData[newStrLabel] = content;
        return newStrLabel;
    }

    if (tacOperand[0] == '_' || tacOperand.rfind("temp", 0) == 0) {
        return "%" + tacOperand;
    }
    if (tacOperand[0] == '$' || tacOperand[0] == '@')
        return tacOperand;

    return tacOperand;
}
std::string prepareQbeCallArguments(QBEGeneratorState& state, const std::vector<std::string>& pendingParamsTacInput)
{
    std::vector<std::string> qbeArgsForCall;
    for (const auto& tacParam : pendingParamsTacInput) {
        std::string qbeOperand;
        if (isNumber(tacParam)) {
            qbeOperand = getQbeOperand(tacParam, state, true);
        } else if (tacParam.length() >= 2 && tacParam.front() == '"' && tacParam.back() == '"') {
            std::string strDataLabel = getQbeOperand(tacParam, state, false);
            std::string strObjReg = "%str_obj_" + std::to_string(state.tempCount++);
            state.output << "    " << strObjReg << " =l call $make_string(l " << strDataLabel << ")\n";
            qbeOperand = strObjReg;
        } else if (tacParam == "#t" || tacParam == "#f" || tacParam == "()") {
            qbeOperand = getQbeOperand(tacParam, state, false);
        } else if (tacParam[0] == '_' || tacParam.rfind("temp", 0) == 0) {
            qbeOperand = getQbeOperand(tacParam, state, false);
        } else {
            std::string schemeVarName = tacParam;
            std::string loadedVarReg = "%load_arg_" + std::to_string(state.tempCount++);
            state.stringLiteralsForSymbols.insert(schemeVarName);
            std::string symReg = "%sym_arg_" + std::to_string(state.tempCount++);
            state.output << "    " << symReg << " =l call $intern_symbol(l $str_" << sanitizeLabel(schemeVarName) << ")\n";

            std::string env_for_lookup_arg;
            if (!state.currentCallEnvSlot.empty() && state.currentFuncLabel != "$main") {
                env_for_lookup_arg = "%temp_env_for_arg_lookup_" + std::to_string(state.tempCount++);
                state.output << "    " << env_for_lookup_arg << " =l loadl " << state.currentCallEnvSlot << "\n";
            } else {
                env_for_lookup_arg = "%g_env_ptr_arglookup_" + std::to_string(state.tempCount++);
                state.output << "    " << env_for_lookup_arg << " =l loadl $current_environment\n";
            }
            state.output << "    " << loadedVarReg << " =l call $env_lookup(l " << env_for_lookup_arg << ", l " << symReg << ")\n";
            qbeOperand = loadedVarReg;
        }
        qbeArgsForCall.push_back("l " + qbeOperand);
    }
    std::string qbeArgListString;
    if (!qbeArgsForCall.empty()) {
        qbeArgListString = qbeArgsForCall[0];
        for (size_t i = 1; i < qbeArgsForCall.size(); ++i)
            qbeArgListString += ", " + qbeArgsForCall[i];
    }
    return qbeArgListString;
}

void handleAlloc(const tac::ThreeACInstruction& instr, QBEGeneratorState& state)
{
    if (!instr.result) {
        state.output << "    # ERROR: ALLOC missing result\n";
        return;
    }
    std::string destReg = getQbeOperand(*instr.result, state, false);

    if (instr.arg1 && *instr.arg1 == "closure") {
        if (instr.arg2) {
            state.output << "    " << destReg << " =l call $make_closure(l $" << *instr.arg2 << ") # Pass address of QBE label\n";

            // Track that this temp variable holds this function
            state.varToFunctionLabel[*instr.result] = *instr.arg2;
        } else {
            state.output << "    # ERROR: Closure ALLOC missing function label\n";
        }
    } else if (instr.arg1 && *instr.arg1 == "literal") {
        std::string literal_str = instr.arg2 ? *instr.arg2 : "nil";
        state.output << "    # TODO: QBE for ALLOC literal: " << literal_str << "\n";
    } else {
        state.output << "    # Unhandled ALLOC type in handleAlloc: " << (instr.arg1 ? *instr.arg1 : "none") << "\n";
    }
}

void handleTailCallSelf(const tac::ThreeACInstruction& instr, QBEGeneratorState& state)
{
    state.output << "    # TAIL_CALL_SELF - preparing arguments\n";

    // Evaluate all arguments and store them in the tail argument slots
    for (size_t i = 0; i < state.pendingParamsTac.size() && i < state.tacParamNames.size(); ++i) {
        const auto& tacParam = state.pendingParamsTac[i];
        std::string qbeOperand;

        if (isNumber(tacParam)) {
            qbeOperand = getQbeOperand(tacParam, state, true);
        } else if (tacParam.length() >= 2 && tacParam.front() == '"' && tacParam.back() == '"') {
            std::string strDataLabel = getQbeOperand(tacParam, state, false);
            std::string strObjReg = "%str_obj_" + std::to_string(state.tempCount++);
            state.output << "    " << strObjReg << " =l call $make_string(l " << strDataLabel << ")\n";
            qbeOperand = strObjReg;
        } else if (tacParam == "#t" || tacParam == "#f" || tacParam == "()") {
            qbeOperand = getQbeOperand(tacParam, state, false);
        } else if (tacParam[0] == '_' || tacParam.rfind("temp", 0) == 0) {
            qbeOperand = getQbeOperand(tacParam, state, false);
        } else {
            // Variable lookup
            std::string schemeVarName = tacParam;
            std::string loadedVarReg = "%load_arg_" + std::to_string(state.tempCount++);
            state.stringLiteralsForSymbols.insert(schemeVarName);
            std::string symReg = "%sym_arg_" + std::to_string(state.tempCount++);
            state.output << "    " << symReg << " =l call $intern_symbol(l $str_" << sanitizeLabel(schemeVarName) << ")\n";

            std::string env_for_lookup_arg;
            if (!state.currentCallEnvSlot.empty() && state.currentFuncLabel != "$main") {
                env_for_lookup_arg = "%temp_env_for_arg_lookup_" + std::to_string(state.tempCount++);
                state.output << "    " << env_for_lookup_arg << " =l loadl " << state.currentCallEnvSlot << "\n";
            } else {
                env_for_lookup_arg = "%g_env_ptr_arglookup_" + std::to_string(state.tempCount++);
                state.output << "    " << env_for_lookup_arg << " =l loadl $current_environment\n";
            }
            state.output << "    " << loadedVarReg << " =l call $env_lookup(l " << env_for_lookup_arg << ", l " << symReg << ")\n";
            qbeOperand = loadedVarReg;
        }

        // Store the argument in the tail argument slot
        std::string offset = std::to_string(i * 8);
        state.output << "    storel " << qbeOperand << ", " << state.tailArgBase;
        if (i > 0) {
            state.output << " +" << offset;
        }
        state.output << "\n";
    }

    // Jump to tail entry point
    state.output << "    jmp @tail_entry_" << state.currentFuncLabel << "\n";
    state.pendingParamsTac.clear();
}

void handleTailCall(const tac::ThreeACInstruction& instr, QBEGeneratorState& state)
{
    if (!instr.arg1) {
        state.output << "    # ERROR: TAIL_CALL missing function object\n";
        state.pendingParamsTac.clear();
        return;
    }

    std::string funcName = *instr.arg1;

    // Check if it's a self-call first (most important optimization)
    if (funcName == state.currentFuncLabel) {
        state.output << "    # Self-recursive tail call detected!\n";
        handleTailCallSelf(instr, state);
        return;
    }

    // Check if we're calling a known function by name
    bool isKnownFunction = state.functionParamCounts.find(funcName) != state.functionParamCounts.end();

    if (isKnownFunction) {
        // Direct tail call to a known function
        size_t expectedParams = state.functionParamCounts[funcName];
        if (state.pendingParamsTac.size() != expectedParams) {
            state.output << "    # ERROR: Parameter count mismatch for tail call to "
                         << funcName << " (expected " << expectedParams
                         << ", got " << state.pendingParamsTac.size() << ")\n";
        }

        state.output << "    # Direct tail call to known function: " << funcName << "\n";

        // Prepare arguments
        std::vector<std::string> argRegs;

        // First, we need to get the closure object for the function
        std::string closureReg = "%closure_for_" + funcName + "_" + std::to_string(state.tempCount++);
        state.stringLiteralsForSymbols.insert(funcName);
        std::string symReg = "%sym_" + funcName + "_" + std::to_string(state.tempCount++);
        state.output << "    " << symReg << " =l call $intern_symbol(l $str_" << sanitizeLabel(funcName) << ")\n";

        std::string env_for_lookup;
        if (!state.currentCallEnvSlot.empty() && state.currentFuncLabel != "$main") {
            env_for_lookup = "%temp_env_for_closure_lookup_" + std::to_string(state.tempCount++);
            state.output << "    " << env_for_lookup << " =l loadl " << state.currentCallEnvSlot << "\n";
        } else {
            env_for_lookup = "%g_env_ptr_closure_lookup_" + std::to_string(state.tempCount++);
            state.output << "    " << env_for_lookup << " =l loadl $current_environment\n";
        }
        state.output << "    " << closureReg << " =l call $env_lookup(l " << env_for_lookup << ", l " << symReg << ")\n";

        // Now prepare the actual arguments
        for (const auto& tacParam : state.pendingParamsTac) {
            std::string qbeOperand;

            if (isNumber(tacParam)) {
                qbeOperand = getQbeOperand(tacParam, state, true);
            } else if (tacParam.length() >= 2 && tacParam.front() == '"' && tacParam.back() == '"') {
                std::string strDataLabel = getQbeOperand(tacParam, state, false);
                std::string strObjReg = "%str_obj_" + std::to_string(state.tempCount++);
                state.output << "    " << strObjReg << " =l call $make_string(l " << strDataLabel << ")\n";
                qbeOperand = strObjReg;
            } else if (tacParam == "#t" || tacParam == "#f" || tacParam == "()") {
                qbeOperand = getQbeOperand(tacParam, state, false);
            } else if (tacParam[0] == '_' || tacParam.rfind("temp", 0) == 0) {
                qbeOperand = getQbeOperand(tacParam, state, false);
            } else {
                // Variable lookup
                std::string schemeVarName = tacParam;
                std::string loadedVarReg = "%load_arg_" + std::to_string(state.tempCount++);
                state.stringLiteralsForSymbols.insert(schemeVarName);
                std::string symReg = "%sym_arg_" + std::to_string(state.tempCount++);
                state.output << "    " << symReg << " =l call $intern_symbol(l $str_" << sanitizeLabel(schemeVarName) << ")\n";

                std::string env_for_lookup_arg;
                if (!state.currentCallEnvSlot.empty() && state.currentFuncLabel != "$main") {
                    env_for_lookup_arg = "%temp_env_for_arg_lookup_" + std::to_string(state.tempCount++);
                    state.output << "    " << env_for_lookup_arg << " =l loadl " << state.currentCallEnvSlot << "\n";
                } else {
                    env_for_lookup_arg = "%g_env_ptr_arglookup_" + std::to_string(state.tempCount++);
                    state.output << "    " << env_for_lookup_arg << " =l loadl $current_environment\n";
                }
                state.output << "    " << loadedVarReg << " =l call $env_lookup(l " << env_for_lookup_arg << ", l " << symReg << ")\n";
                qbeOperand = loadedVarReg;
            }
            argRegs.push_back(qbeOperand);
        }

        // Clean up our current environment
        if (!state.currentCallEnvSlot.empty()) {
            std::string currentCallEnvReg = "%temp_call_env_for_tail_restore_" + std::to_string(state.tempCount++);
            state.output << "    " << currentCallEnvReg << " =l loadl " << state.currentCallEnvSlot << "\n";
            state.output << "    call $restore_call_environment(l " << currentCallEnvReg << ")\n";
        }

        // Build the call arguments
        std::string callArgs = "l " + closureReg;
        for (const auto& arg : argRegs) {
            callArgs += ", l " + arg;
        }

        // QBE doesn't support tail calls directly
        // For calls to known functions, we still need to do call + ret
        std::string retVal = "%result_" + std::to_string(state.tempCount++);
        state.output << "    " << retVal << " =l call $" << funcName << "(" << callArgs << ")\n";
        state.output << "    ret " << retVal << "\n";

        state.pendingParamsTac.clear();
        return;
    }

    // If we get here, it's a dynamic call (function not known at compile time)
    state.output << "    # Dynamic tail call to: " << funcName << "\n";

    // We need to load the function object
    std::string funcObjQbeReg = getQbeOperand(funcName, state, false);
    if (funcObjQbeReg[0] != '%' && funcObjQbeReg[0] != '$') {
        // Need to load the function from environment
        std::string temp_load = "%load_func_" + std::to_string(state.tempCount++);
        state.stringLiteralsForSymbols.insert(funcName);
        std::string sym = "%sym_func_" + std::to_string(state.tempCount++);
        state.output << "    " << sym << " =l call $intern_symbol(l $str_" << sanitizeLabel(funcName) << ")\n";

        std::string env_for_lookup;
        if (!state.currentCallEnvSlot.empty() && state.currentFuncLabel != "$main") {
            env_for_lookup = "%temp_env_for_func_lookup_" + std::to_string(state.tempCount++);
            state.output << "    " << env_for_lookup << " =l loadl " << state.currentCallEnvSlot << "\n";
        } else {
            env_for_lookup = "%g_env_ptr_func_lookup_" + std::to_string(state.tempCount++);
            state.output << "    " << env_for_lookup << " =l loadl $current_environment\n";
        }
        state.output << "    " << temp_load << " =l call $env_lookup(l " << env_for_lookup << ", l " << sym << ")\n";
        funcObjQbeReg = temp_load;
    }

    // Prepare arguments
    std::string schemeArgs = prepareQbeCallArguments(state, state.pendingParamsTac);

    // Clean up our current environment
    if (!state.currentCallEnvSlot.empty()) {
        std::string currentCallEnvReg = "%temp_call_env_for_tail_restore_" + std::to_string(state.tempCount++);
        state.output << "    " << currentCallEnvReg << " =l loadl " << state.currentCallEnvSlot << "\n";
        state.output << "    call $restore_call_environment(l " << currentCallEnvReg << ")\n";
    }

    // Get the code pointer
    std::string codePtrReg = "%codeptr_tail_" + std::to_string(state.tempCount++);
    state.output << "    " << codePtrReg << " =l call $getCodePointer(l " << funcObjQbeReg << ")\n";

    // Build call arguments
    std::string cCallArgs = "l " + funcObjQbeReg;
    if (!schemeArgs.empty()) {
        cCallArgs += ", " + schemeArgs;
    }

    // QBE doesn't have tail call syntax, so we do a regular call + ret
    // This doesn't save stack space for dynamic calls unfortunately
    std::string retVal = "%tail_result_" + std::to_string(state.tempCount++);
    state.output << "    " << retVal << " =l call " << codePtrReg << "(" << cCallArgs << ")\n";
    state.output << "    ret " << retVal << "\n";

    state.pendingParamsTac.clear();
}

void handleFuncBegin(const tac::ThreeACInstruction& instr, QBEGeneratorState& state)
{
    if (!instr.arg1) {
        state.output << "    # ERROR: FUNC_BEGIN missing label\n";
        return;
    }
    state.currentFuncLabel = *instr.arg1;

    std::vector<std::string> schemeParamNames;
    if (instr.arg2) {
        std::stringstream ss(*instr.arg2);
        std::string name;
        while (std::getline(ss, name, ',')) {
            name.erase(0, name.find_first_not_of(" \t\n\r\f\v"));
            name.erase(name.find_last_not_of(" \t\n\r\f\v") + 1);
            if (!name.empty())
                schemeParamNames.push_back(name);
        }
    }

    state.tacParamNames = schemeParamNames;
    state.functionParamCounts[state.currentFuncLabel] = schemeParamNames.size();

    // Generate QBE function signature
    std::string qbeClosureObjParam = "%_closure_obj_" + state.currentFuncLabel;
    std::string qbeSignatureParams = "(l " + qbeClosureObjParam;

    std::vector<std::string> qbeParamTmps;
    qbeParamTmps.push_back(qbeClosureObjParam);

    for (size_t i = 0; i < schemeParamNames.size(); ++i) {
        std::string qbeParamTmpName = "%_param_" + sanitizeLabel(schemeParamNames[i]) + "_" + state.currentFuncLabel;
        qbeSignatureParams += ", l " + qbeParamTmpName;
        qbeParamTmps.push_back(qbeParamTmpName);
    }
    qbeSignatureParams += ")";

    state.output << "\nexport function w $" << state.currentFuncLabel << qbeSignatureParams << " {\n";
    state.output << "@start_" << state.currentFuncLabel << "\n";

    // Allocate space on stack for tail call arguments (if any Scheme parameters)
    state.tailArgBase = "%tail_args_" + state.currentFuncLabel;
    if (!schemeParamNames.empty()) {
        state.output << "    " << state.tailArgBase << " =l alloc8 " << schemeParamNames.size() << " # Space for tail call args (Scheme params)\n";
    }

    // Allocate stack space for the current call's environment pointer slot
    state.currentCallEnvSlot = "%fp_call_env_" + state.currentFuncLabel;
    state.output << "    " << state.currentCallEnvSlot << " =l alloc8 1 # Slot for new_call_env* (hoisted)\n";

    state.output << "    jmp @main_entry_" << state.currentFuncLabel << "\n\n";

    // Tail recursion entry point
    state.output << "@tail_entry_" << state.currentFuncLabel << " # Tail recursion entry\n";
    if (!schemeParamNames.empty()) {
        for (size_t i = 0; i < schemeParamNames.size(); ++i) {
            std::string paramQbeTemp = qbeParamTmps[i + 1];
            std::string offset = std::to_string(i * 8);
            state.output << "    " << paramQbeTemp << " =l loadl " << state.tailArgBase;
            if (i > 0) {
                state.output << " +" << offset;
            }
            state.output << " # Load tail arg " << schemeParamNames[i] << "\n";
        }
    }
    state.output << "    jmp @main_entry_" << state.currentFuncLabel << "\n\n";

    // Main function logic entry point
    state.output << "@main_entry_" << state.currentFuncLabel << " # Main function entry\n";

    // Call $setup_call_environment with the closure object
    state.output << "    %rax =l call $setup_call_environment(l " << qbeParamTmps[0] << ")\n";
    state.output << "    storel %rax, " << state.currentCallEnvSlot << "\n";

    // Bind Scheme parameters into the new call environment
    for (size_t i = 0; i < schemeParamNames.size(); ++i) {
        const std::string& schemeParamName = schemeParamNames[i];
        const std::string& qbeParamValueTemp = qbeParamTmps[i + 1];

        state.output << "    # Binding Scheme param '" << schemeParamName << "' to value from " << qbeParamValueTemp << "\n";

        std::string paramSymbolReg = "%param_sym_" + sanitizeLabel(schemeParamName) + "_" + std::to_string(state.tempCount++);
        state.stringLiteralsForSymbols.insert(schemeParamName);
        state.output << "    " << paramSymbolReg << " =l call $intern_symbol(l $str_" << sanitizeLabel(schemeParamName) << ")\n";

        std::string loadedCallEnvPointerReg = "%temp_env_for_param_bind_" + std::to_string(state.tempCount++);
        state.output << "    " << loadedCallEnvPointerReg << " =l loadl " << state.currentCallEnvSlot << "\n";

        state.output << "    call $env_define(l " << loadedCallEnvPointerReg << ", l " << paramSymbolReg << ", l " << qbeParamValueTemp << ")\n";
    }
}

void handleReturn(const tac::ThreeACInstruction& instr, QBEGeneratorState& state)
{
    std::string returnValueQbe = "$nil_obj";
    if (instr.arg1) {
        returnValueQbe = getQbeOperand(*instr.arg1, state, false);
    }

    if (!state.currentCallEnvSlot.empty()) {
        std::string currentCallEnvReg = "%temp_call_env_for_restore_" + state.currentFuncLabel;
        state.output << "    " << currentCallEnvReg << " =l loadl " << state.currentCallEnvSlot << "\n";
        state.output << "    call $restore_call_environment(l " << currentCallEnvReg << ")\n";
    } else {
        if (state.currentFuncLabel != "$main") {
            state.output << "    # WARN: No currentCallEnvSlot to restore in handleReturn for " << state.currentFuncLabel << ".\n";
        }
    }
    state.output << "    ret " << returnValueQbe << "\n";
    state.output << "}\n";
    state.currentFuncLabel = "";
    state.currentCallEnvSlot = "";
    state.tacParamNames.clear();
}

void handleFuncEnd(const tac::ThreeACInstruction& instr, QBEGeneratorState& state)
{
    if (instr.arg1) {
        state.output << "    # FUNC_END marker for: " << *instr.arg1 << "\n";
    }
}

void handleCopy(const tac::ThreeACInstruction& instr, QBEGeneratorState& state)
{
    if (!instr.result || !instr.arg1) {
        state.output << "    # ERROR: COPY missing operands\n";
        return;
    }
    std::string dest = getQbeOperand(*instr.result, state, false);
    std::string src = getQbeOperand(*instr.arg1, state, true);
    if (src[0] != '%' && src[0] != '$') {
        std::string temp_load = "%load_copy_src_" + std::to_string(state.tempCount++);
        state.stringLiteralsForSymbols.insert(*instr.arg1);
        std::string sym = "%sym_copy_src_" + std::to_string(state.tempCount++);
        state.output << "    " << sym << " =l call $intern_symbol(l $str_" << sanitizeLabel(*instr.arg1) << ")\n";

        std::string env_for_lookup;
        if (!state.currentCallEnvSlot.empty() && state.currentFuncLabel != "$main") {
            env_for_lookup = "%temp_env_for_copy_lookup_" + std::to_string(state.tempCount++);
            state.output << "    " << env_for_lookup << " =l loadl " << state.currentCallEnvSlot << "\n";
        } else {
            env_for_lookup = "%g_env_ptr_copy_lookup_" + std::to_string(state.tempCount++);
            state.output << "    " << env_for_lookup << " =l loadl $current_environment\n";
        }
        state.output << "    " << temp_load << " =l call $env_lookup(l " << env_for_lookup << ", l " << sym << ")\n";
        src = temp_load;
    }
    state.output << "    " << dest << " =l copy " << src << "\n";

    // Propagate function tracking through copies
    if (instr.arg1 && instr.result) {
        auto it = state.varToFunctionLabel.find(*instr.arg1);
        if (it != state.varToFunctionLabel.end()) {
            state.varToFunctionLabel[*instr.result] = it->second;
        }
    }
}

void handleLoad(const tac::ThreeACInstruction& instr, QBEGeneratorState& state)
{
    if (!instr.result || !instr.arg1) {
        state.output << "    # ERROR: LOAD missing operands\n";
        return;
    }
    std::string dest = getQbeOperand(*instr.result, state, false);
    std::string varName = *instr.arg1;
    state.stringLiteralsForSymbols.insert(varName);
    std::string symReg = "%sym_load_" + std::to_string(state.tempCount++);
    state.output << "    " << symReg << " =l call $intern_symbol(l $str_" << sanitizeLabel(varName) << ")\n";

    std::string env_for_lookup;
    if (!state.currentCallEnvSlot.empty() && state.currentFuncLabel != "$main") {
        env_for_lookup = "%temp_env_for_load_lookup_" + std::to_string(state.tempCount++);
        state.output << "    " << env_for_lookup << " =l loadl " << state.currentCallEnvSlot << "\n";
    } else {
        env_for_lookup = "%g_env_ptr_load_lookup_" + std::to_string(state.tempCount++);
        state.output << "    " << env_for_lookup << " =l loadl $current_environment\n";
    }
    state.output << "    " << dest << " =l call $env_lookup(l " << env_for_lookup << ", l " << symReg << ")\n";

    // If we're loading a variable that holds a function, track it
    if (instr.arg1 && instr.result) {
        auto it = state.varToFunctionLabel.find(*instr.arg1);
        if (it != state.varToFunctionLabel.end()) {
            state.varToFunctionLabel[*instr.result] = it->second;
        }
    }
}

void handleStore(const tac::ThreeACInstruction& instr, QBEGeneratorState& state)
{
    if (!instr.arg1 || !instr.arg2) {
        state.output << "    # ERROR: STORE missing operands\n";
        return;
    }
    std::string varName = *instr.arg1;
    std::string valueToStore = getQbeOperand(*instr.arg2, state, true);

    // Track if we're storing a function to a variable
    if (state.varToFunctionLabel.find(*instr.arg2) != state.varToFunctionLabel.end()) {
        state.varToFunctionLabel[varName] = state.varToFunctionLabel[*instr.arg2];
    }

    if (valueToStore[0] != '%' && valueToStore[0] != '$') {
        std::string temp_load = "%load_store_val_" + std::to_string(state.tempCount++);
        state.stringLiteralsForSymbols.insert(*instr.arg2);
        std::string sym_val = "%sym_store_val_" + std::to_string(state.tempCount++);

        std::string env_for_val_lookup;
        if (!state.currentCallEnvSlot.empty() && state.currentFuncLabel != "$main") {
            env_for_val_lookup = "%temp_env_for_store_val_lookup_" + std::to_string(state.tempCount++);
            state.output << "    " << env_for_val_lookup << " =l loadl " << state.currentCallEnvSlot << "\n";
        } else {
            env_for_val_lookup = "%g_env_ptr_store_val_lookup_" + std::to_string(state.tempCount++);
            state.output << "    " << env_for_val_lookup << " =l loadl $current_environment\n";
        }
        state.output << "    " << sym_val << " =l call $intern_symbol(l $str_" << sanitizeLabel(*instr.arg2) << ")\n";
        state.output << "    " << temp_load << " =l call $env_lookup(l " << env_for_val_lookup << ", l " << sym_val << ")\n";
        valueToStore = temp_load;
    }
    state.stringLiteralsForSymbols.insert(varName);
    std::string symRegTarget = "%sym_store_" + std::to_string(state.tempCount++);
    state.output << "    " << symRegTarget << " =l call $intern_symbol(l $str_" << sanitizeLabel(varName) << ")\n";

    std::string env_for_define;
    if (!state.currentCallEnvSlot.empty() && state.currentFuncLabel != "$main") {
        env_for_define = "%temp_env_for_store_define_" + std::to_string(state.tempCount++);
        state.output << "    " << env_for_define << " =l loadl " << state.currentCallEnvSlot << "\n";
    } else {
        env_for_define = "%g_env_ptr_store_define_" + std::to_string(state.tempCount++);
        state.output << "    " << env_for_define << " =l loadl $current_environment\n";
    }
    state.output << "    call $env_define(l " << env_for_define << ", l " << symRegTarget << ", l " << valueToStore << ")\n";
}

void handlePrimitiveCall(const tac::ThreeACInstruction& instr, QBEGeneratorState& state)
{
    if (!instr.arg1) {
        state.output << "    # ERROR: PRIMITIVE_CALL missing target\n";
        state.pendingParamsTac.clear();
        return;
    }
    std::string targetSymbol = *instr.arg1;
    std::string resultReg = instr.result ? getQbeOperand(*instr.result, state, false) : "";
    std::string args = prepareQbeCallArguments(state, state.pendingParamsTac);
    if (resultReg.empty())
        state.output << "    call " << targetSymbol << "(" << args << ")\n";
    else
        state.output << "    " << resultReg << " =l call " << targetSymbol << "(" << args << ")\n";
    state.pendingParamsTac.clear();
}

void handleCall(const tac::ThreeACInstruction& instr, QBEGeneratorState& state)
{
    if (!instr.arg1) {
        state.output << "    # ERROR: CALL missing function object\n";
        state.pendingParamsTac.clear();
        return;
    }
    std::string funcObjQbeReg = getQbeOperand(*instr.arg1, state, false);
    std::string resultReg = instr.result ? getQbeOperand(*instr.result, state, false) : "";
    std::string schemeArgs = prepareQbeCallArguments(state, state.pendingParamsTac);
    std::string codePtrReg = "%codeptr_" + std::to_string(state.tempCount++);

    state.output << "    " << codePtrReg << " =l call $getCodePointer(l " << funcObjQbeReg << ")\n";

    std::string cCallArgs = "l " + funcObjQbeReg;
    if (!schemeArgs.empty())
        cCallArgs += ", " + schemeArgs;

    if (resultReg.empty())
        state.output << "    call " << codePtrReg << "(" << cCallArgs << ")\n";
    else
        state.output << "    " << resultReg << " =l call " << codePtrReg << "(" << cCallArgs << ")\n";
    state.pendingParamsTac.clear();
}

void handleParam(const tac::ThreeACInstruction& instr, QBEGeneratorState& state)
{
    if (instr.arg1)
        state.pendingParamsTac.push_back(*instr.arg1);
    else
        state.output << "    # WARN: PARAM with no argument\n";
}

void handleLabel(const tac::ThreeACInstruction& instr, QBEGeneratorState& state)
{
    if (instr.arg1)
        state.output << "@" << *instr.arg1 << "\n";
    else
        state.output << "    # ERROR: LABEL missing name\n";
}

void handleJump(const tac::ThreeACInstruction& instr, QBEGeneratorState& state)
{
    if (instr.arg1)
        state.output << "    jmp @" << *instr.arg1 << "\n";
    else
        state.output << "    # ERROR: JUMP missing target\n";
}

void handleJumpIf(const tac::ThreeACInstruction& instr, QBEGeneratorState& state)
{
    if (!instr.arg1 || !instr.arg2) {
        state.output << "    # ERROR: JUMP_IF missing operands\n";
        return;
    }
    std::string cond = getQbeOperand(*instr.arg1, state, false);
    std::string label = *instr.arg2;
    std::string isTrue = "%istrue_" + std::to_string(state.tempCount++);
    state.output << "    " << isTrue << " =l cnel " << cond << ", $false_obj\n";
    state.output << "    jnz " << isTrue << ", @" << label << ", @fallthrough_" << state.labelCount++ << "\n";
    state.output << "@fallthrough_" << (state.labelCount - 1) << "\n";
}

void handleJumpIfNot(const tac::ThreeACInstruction& instr, QBEGeneratorState& state)
{
    if (!instr.arg1 || !instr.arg2) {
        state.output << "    # ERROR: JUMP_IF_NOT missing operands\n";
        return;
    }
    std::string cond = getQbeOperand(*instr.arg1, state, false);
    std::string label = *instr.arg2;
    std::string isFalse = "%isfalse_" + std::to_string(state.tempCount++);
    state.output << "    " << isFalse << " =l ceql " << cond << ", $false_obj\n";
    state.output << "    jnz " << isFalse << ", @" << label << ", @fallthrough_" << state.labelCount++ << "\n";
    state.output << "@fallthrough_" << (state.labelCount - 1) << "\n";
}

void convertInstruction(const tac::ThreeACInstruction& instr, QBEGeneratorState& state)
{
    state.output << "    # " << instr.toString() << "\n";
    switch (instr.op) {
    case tac::Operation::TAIL_CALL:
        handleTailCallSelf(instr, state);
        break;
    case tac::Operation::COPY:
        handleCopy(instr, state);
        break;
    case tac::Operation::LOAD:
        handleLoad(instr, state);
        break;
    case tac::Operation::STORE:
        handleStore(instr, state);
        break;
    case tac::Operation::ALLOC:
        handleAlloc(instr, state);
        break;
    case tac::Operation::PRIMITIVE_CALL:
        handlePrimitiveCall(instr, state);
        break;
    case tac::Operation::CALL:
        handleCall(instr, state);
        break;
    case tac::Operation::PARAM:
        handleParam(instr, state);
        break;
    case tac::Operation::LABEL:
        handleLabel(instr, state);
        break;
    case tac::Operation::JUMP:
        handleJump(instr, state);
        break;
    case tac::Operation::JUMP_IF:
        handleJumpIf(instr, state);
        break;
    case tac::Operation::JUMP_IF_NOT:
        handleJumpIfNot(instr, state);
        break;
    case tac::Operation::FUNC_BEGIN:
        handleFuncBegin(instr, state);
        break;
    case tac::Operation::RETURN:
        handleReturn(instr, state);
        break;
    case tac::Operation::FUNC_END:
        handleFuncEnd(instr, state);
        break;
    case tac::Operation::GC:
        state.output << "    call $gc\n";
        break;
    default:
        state.output << "    # ERROR: Unknown TAC op: " << tac::operationToString(instr.op) << "\n";
    }
}

void generateQBEIr(const tac::ThreeAddressModule& module, const std::string& outputPath)
{
    QBEGeneratorState state;
    // Pass 1: Collect data for .data section
    for (const auto& instr : module.instructions) {
        auto collect_str_lit_content_from_opt = [&](const std::optional<std::string>& s_opt) {
            if (s_opt && s_opt->length() >= 2 && s_opt->front() == '"' && s_opt->back() == '"') {
                std::string content = s_opt->substr(1, s_opt->length() - 2);
                if (state.stringLiteralMap.find(content) == state.stringLiteralMap.end()) {
                    std::string lbl = "$strlit_" + std::to_string(state.stringLiteralCounter++);
                    state.stringLiteralMap[content] = lbl;
                    state.stringLiteralsForData[lbl] = content;
                }
            }
        };
        collect_str_lit_content_from_opt(instr.arg1);
        collect_str_lit_content_from_opt(instr.arg2);
        collect_str_lit_content_from_opt(instr.result);

        auto collect_symbol_name = [&](const std::optional<std::string>& s_opt) {
            if (s_opt && !s_opt->empty() && !isNumber(*s_opt) && (*s_opt)[0] != '_' && (*s_opt)[0] != '$' && (*s_opt)[0] != '@' && *s_opt != "#t" && *s_opt != "#f" && *s_opt != "()" && !((*s_opt).length() >= 2 && (*s_opt).front() == '"' && (*s_opt).back() == '"')) {
                state.stringLiteralsForSymbols.insert(*s_opt);
            }
        };

        if (instr.op == tac::Operation::STORE || instr.op == tac::Operation::LOAD || (instr.op == tac::Operation::ENV_LOOKUP && instr.arg1)) {
            collect_symbol_name(instr.arg1);
        }
        if (instr.op == tac::Operation::STORE && instr.arg2 && !isNumber(*instr.arg2) && instr.arg2.value_or("")[0] != '_' && instr.arg2.value_or("")[0] != '"' && instr.arg2.value_or("")[0] != '#' && instr.arg2.value_or("")[0] != '(') {
            if (instr.arg2.value_or("").rfind("temp", 0) != 0) {
                collect_symbol_name(instr.arg2);
            }
        }
        if (instr.op == tac::Operation::FUNC_BEGIN && instr.arg2) {
            std::stringstream ss_params(*instr.arg2);
            std::string p_name;
            while (std::getline(ss_params, p_name, ',')) {
                p_name.erase(0, p_name.find_first_not_of(" \t\n\r\f\v"));
                p_name.erase(p_name.find_last_not_of(" \t\n\r\f\v") + 1);
                if (!p_name.empty())
                    state.stringLiteralsForSymbols.insert(p_name);
            }
        }
    }

    // Output .data section
    state.output << "# QBE IR generated by Scheme compiler\n\n";
    state.output << "# === Data Section ===\n";
    state.output << "\n# Symbol strings (for $intern_symbol)\n";
    for (const auto& s_name : state.stringLiteralsForSymbols) {
        if (!s_name.empty())
            state.output << "data $str_" << sanitizeLabel(s_name) << " = { b \"" << escapeString(s_name) << "\", b 0 }\n";
    }
    state.output << "\n# String literal data (for $make_string)\n";
    for (const auto& pair_label_content : state.stringLiteralsForData) {
        state.output << "data " << pair_label_content.first << " = { b \"" << escapeString(pair_label_content.second) << "\", b 0 }\n";
    }

    // Pass 2: Generate code for functions
    state.output << "\n# === Code Section ===\n";

    std::vector<tac::ThreeACInstruction> mainInstructions;
    std::map<std::string, std::vector<tac::ThreeACInstruction>> otherFunctionsInstructions;
    std::string currentProcessingFuncLabelForGrouping;

    for (const auto& instr : module.instructions) {
        if (instr.op == tac::Operation::FUNC_BEGIN) {
            if (instr.arg1) {
                currentProcessingFuncLabelForGrouping = *instr.arg1;
                otherFunctionsInstructions[currentProcessingFuncLabelForGrouping].push_back(instr);
            } else {
                mainInstructions.push_back(instr);
            }
        } else if (instr.op == tac::Operation::FUNC_END) {
            if (!currentProcessingFuncLabelForGrouping.empty() && instr.arg1 && *instr.arg1 == currentProcessingFuncLabelForGrouping) {
                otherFunctionsInstructions[currentProcessingFuncLabelForGrouping].push_back(instr);
            } else if (!currentProcessingFuncLabelForGrouping.empty()) {
                otherFunctionsInstructions[currentProcessingFuncLabelForGrouping].push_back(instr);
                state.output << "    # WARN: FUNC_END label mismatch or unexpected: " << instr.toString() << "\n";
            }
            currentProcessingFuncLabelForGrouping = "";
        } else if (!currentProcessingFuncLabelForGrouping.empty()) {
            otherFunctionsInstructions[currentProcessingFuncLabelForGrouping].push_back(instr);
        } else {
            mainInstructions.push_back(instr);
        }
    }

    // Generate QBE for other functions first
    for (const auto& pair_label_instrs : otherFunctionsInstructions) {
        for (const auto& instr : pair_label_instrs.second) {
            convertInstruction(instr, state);
        }
    }

    // Generate QBE for $main
    state.output << "\nexport function w $main() {\n";
    state.output << "@start_main # Main entry block\n";
    state.output << "    call $init_runtime()\n\n";
    state.currentFuncLabel = "$main";
    state.currentCallEnvSlot = "";

    for (const auto& instr : mainInstructions) {
        if (instr.op == tac::Operation::FUNC_BEGIN || instr.op == tac::Operation::FUNC_END || instr.op == tac::Operation::RETURN) {
            state.output << "    # WARN: Unexpected " << tac::operationToString(instr.op) << " in main instruction stream: " << instr.toString() << "\n";
            continue;
        }
        convertInstruction(instr, state);
    }

    state.output << "\n    call $gc() # Final GC before exit\n";
    state.output << "    ret 0\n";
    state.output << "}\n";

    std::ofstream outFile(outputPath);
    if (!outFile) {
        std::cerr << "ERROR: Failed to open QBE output file: " << outputPath << std::endl;
        return;
    }
    outFile << state.output.str();
    outFile.close();
    if (!outFile) {
        std::cerr << "ERROR: Failed to write all data to QBE output file: " << outputPath << std::endl;
    } else {
        std::cout << "Generated QBE IR to: " << outputPath << std::endl;
    }
}

}
