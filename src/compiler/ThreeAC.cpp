#include "ThreeAC.h"
#include "Visit.h"
#include <memory>
#include <numeric>
#include <optional>
#include <sstream>
#include <stdexcept>
#include <string>
#include <tuple>
#include <vector>

namespace tac {

static const std::unordered_map<std::string, std::string> PRIMITIVE_MAP = {
    { "+", "$plus" },
    { "-", "$minus" },
    { "*", "$multiply" },
    { "/", "$divide" },
    { "display", "$display" },
    { "newline", "$newline" },
    { "cons", "$cons" },
    { "car", "$car" },
    { "cdr", "$cdr" },
    { "=", "$equal" }
};

struct FunctionToProcess {
    std::string label;
    std::vector<Token> params;
    std::shared_ptr<ir::ANF> body_anf;
};

static int tempCounter = 0;
static int labelCounter = 0;

std::string generateTemp()
{
    return "_t" + std::to_string(tempCounter++);
}

std::string generateLabel()
{
    return "L" + std::to_string(labelCounter++);
}

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
    case Operation::PARAM:
        return "PARAM";
    case Operation::RETURN:
        return "RETURN";
    case Operation::FUNC_BEGIN:
        return "FUNC_BEGIN";
    case Operation::FUNC_END:
        return "FUNC_END";
    case Operation::ENV_LOOKUP:
        return "ENV_LOOKUP";
    case Operation::PRIMITIVE_CALL:
        return "PRIMITIVE_CALL";
    case Operation::TAIL_CALL:
        return "TAIL_CALL";
    case Operation::TAIL_CALL_SELF:
        return "TAIL_CALL_SELF";
    }
    return "UNKNOWN";
}

// 2. Update ThreeACInstruction::toString() - add these cases in the switch:
std::string ThreeACInstruction::toString() const
{
    auto ss = std::stringstream();
    switch (op) {
    case Operation::PARAM:
        if (arg1)
            ss << "PARAM " << *arg1;
        break;
    case Operation::RETURN:
        ss << "RETURN";
        if (arg1)
            ss << " " << *arg1;
        break;
    case Operation::FUNC_BEGIN:
        if (arg1)
            ss << "FUNC_BEGIN " << *arg1;
        if (arg2)
            ss << " " << *arg2;
        break;
    case Operation::FUNC_END:
        if (arg1)
            ss << "FUNC_END " << *arg1;
        break;
    case Operation::LABEL:
        if (arg1)
            ss << *arg1 << ":";
        break;
    case Operation::JUMP:
        ss << "JUMP ";
        if (arg1)
            ss << *arg1;
        break;
    case Operation::JUMP_IF:
        ss << "JUMP_IF ";
        if (arg1)
            ss << *arg1;
        ss << " -> ";
        if (arg2)
            ss << *arg2;
        break;
    case Operation::JUMP_IF_NOT:
        ss << "JUMP_IF_NOT ";
        if (arg1)
            ss << *arg1;
        ss << " -> ";
        if (arg2)
            ss << *arg2;
        break;
    case Operation::ALLOC:
        if (result)
            ss << *result << " = ";
        ss << *arg1;
        ss << " ALLOC";
        if (arg2)
            ss << " " << *arg2;
        break;
    case Operation::STORE:
        ss << *arg1;
        ss << " STORE ";
        if (arg2)
            ss << *arg2;
        break;
    case Operation::LOAD:
        if (result)
            ss << *result << " = ";
        ss << *arg1 << " LOAD";
        break;
    case Operation::COPY:
        if (result)
            ss << *result << " = ";
        if (arg1)
            ss << *arg1;
        ss << " COPY";
        break;
    case Operation::TAIL_CALL:
        ss << "TAIL_CALL ";
        if (arg1)
            ss << *arg1;
        if (arg2)
            ss << " (" << *arg2 << " args)";
        break;
    case Operation::TAIL_CALL_SELF:
        ss << "TAIL_CALL_SELF";
        if (arg1)
            ss << " (" << *arg1 << " args)";
        break;
    default:
        if (result)
            ss << *result << " = ";
        if (arg1)
            ss << *arg1 << " ";
        ss << operationToString(op);
        if (arg2)
            ss << " " << *arg2;
        break;
    }
    return ss.str();
}

void ThreeACInstruction::toString(std::stringstream& ss) const
{
    ss << toString() << std::endl;
}

std::string ThreeAddressModule::toString() const
{
    auto ss = std::stringstream();
    for (const auto& instruction : instructions) {
        ss << instruction.toString() << std::endl;
    }
    return ss.str();
}

void ThreeAddressModule::addInstr(ThreeACInstruction instr)
{
    instructions.push_back(std::move(instr));
}

void convertANF(
    const std::shared_ptr<ir::ANF>& anf,
    ThreeAddressModule& module,
    std::string& result,
    std::vector<FunctionToProcess>& functionsToGenerate,
    bool valueNeeded = true,
    bool isTailPosition = false,
    const std::string& currentFunctionLabel = "");

void generateFunctionTacBody(
    const FunctionToProcess& funcInfo,
    ThreeAddressModule& module,
    std::vector<FunctionToProcess>& functionsToGenerate);

bool isTempVariable(const std::string& s)
{
    return !s.empty() && s[0] == '_';
}

bool isLiteral(const std::string& s)
{
    if (s.empty())
        return false;
    return s == "#t" || s == "#f" || s == "()" || (s[0] >= '0' && s[0] <= '9') || (s[0] == '-' && s.length() > 1 && s[1] >= '0' && s[1] <= '9') || s[0] == '"';
}

void convertANF(
    const std::shared_ptr<ir::ANF>& anf,
    ThreeAddressModule& module,
    std::string& result,
    std::vector<FunctionToProcess>& functionsToGenerate,
    bool valueNeeded,
    bool isTailPosition,
    const std::string& currentFunctionLabel)
{
    if (!anf) {
        result = "()";
        return;
    }

    std::visit(overloaded {
                   [&](const ir::Let& let) {
                       std::string bindingResult;
                       // Bindings are never in tail position
                       convertANF(let.binding, module, bindingResult, functionsToGenerate,
                           let.name.has_value(), false, currentFunctionLabel);

                       if (let.name) {
                           module.addInstr({ Operation::STORE, {}, let.name->lexeme, bindingResult });
                           convertANF(let.body, module, result, functionsToGenerate,
                               valueNeeded, isTailPosition, currentFunctionLabel);
                       } else {
                           convertANF(let.body, module, result, functionsToGenerate,
                               valueNeeded, isTailPosition, currentFunctionLabel);
                       }
                   },

                   [&](const ir::Atom& atom) {
                       if (valueNeeded) {
                           if (!isTempVariable(atom.atom.lexeme) && !isLiteral(atom.atom.lexeme)) {
                               result = generateTemp();
                               module.addInstr({ Operation::LOAD, result, atom.atom.lexeme, {} });
                           } else {
                               result = atom.atom.lexeme;
                           }
                       } else {
                           result = "()";
                       }
                   },

                   [&](const ir::App& app) {
                       // Evaluate all arguments first
                       std::vector<std::string> paramSources;
                       paramSources.reserve(app.params.size());
                       for (const auto& param_token : app.params) {
                           std::string paramResult;
                           convertANF(std::make_shared<ir::ANF>(ir::Atom { param_token }),
                               module, paramResult, functionsToGenerate, true, false, currentFunctionLabel);
                           paramSources.push_back(paramResult);
                       }

                       // Emit PARAM instructions
                       for (const auto& source_name : paramSources) {
                           module.addInstr({ Operation::PARAM, {}, source_name, {} });
                       }

                       std::string originalFuncName = app.name.lexeme;
                       auto primitive_it = PRIMITIVE_MAP.find(originalFuncName);

                       if (primitive_it != PRIMITIVE_MAP.end()) {
                           // Primitive call - never a tail call
                           std::string primitiveTargetSymbol = primitive_it->second;
                           if (valueNeeded) {
                               result = generateTemp();
                               module.addInstr({ Operation::PRIMITIVE_CALL, result, primitiveTargetSymbol,
                                   std::to_string(paramSources.size()) });
                           } else {
                               module.addInstr({ Operation::PRIMITIVE_CALL, {}, primitiveTargetSymbol,
                                   std::to_string(paramSources.size()) });
                               result = "()";
                           }
                       } else {
                           // Non-primitive call (closure call)
                           // IMPORTANT: Don't create unnecessary LOAD instructions for simple identifiers
                           // This preserves function names for tail call optimization

                           std::string funcTargetSchemeObjectReg = originalFuncName;

                           // Only create a LOAD if we absolutely need to
                           // (i.e., it's already a temp variable or a literal)
                           bool needsLoad = false;

                           if (isTempVariable(originalFuncName) || originalFuncName.rfind("temp", 0) == 0 || originalFuncName[0] == '$' || isLiteral(originalFuncName)) {
                               // These are already resolved, no LOAD needed
                               funcTargetSchemeObjectReg = originalFuncName;
                           } else {
                               // For regular identifiers, we'll let the code generator handle the lookup
                               // This preserves the function name for tail call detection
                               funcTargetSchemeObjectReg = originalFuncName;
                               needsLoad = true;
                           }

                           // Check if this is a tail call
                           if (app.is_tail && isTailPosition) {
                               // Check if it's a self-call
                               bool is_self_call = (!currentFunctionLabel.empty() && originalFuncName == currentFunctionLabel);

                               if (is_self_call) {
                                   module.addInstr({ Operation::TAIL_CALL_SELF, {},
                                       std::to_string(paramSources.size()), {} });
                               } else {
                                   // For tail calls, we pass the original function name
                                   // This allows the code generator to optimize known function calls
                                   module.addInstr({ Operation::TAIL_CALL, {}, originalFuncName,
                                       std::to_string(paramSources.size()) });
                               }
                               result = ""; // Empty string indicates tail call - no value produced
                           } else {
                               // Normal call
                               if (needsLoad) {
                                   // For normal calls, we do need to load the function
                                   std::string loadedFunc = generateTemp();
                                   module.addInstr({ Operation::LOAD, loadedFunc, originalFuncName, {} });
                                   funcTargetSchemeObjectReg = loadedFunc;
                               }

                               if (valueNeeded) {
                                   result = generateTemp();
                                   module.addInstr({ Operation::CALL, result, funcTargetSchemeObjectReg,
                                       std::to_string(paramSources.size()) });
                               } else {
                                   module.addInstr({ Operation::CALL, {}, funcTargetSchemeObjectReg,
                                       std::to_string(paramSources.size()) });
                                   result = "()";
                               }
                           }
                       }
                   },

                   [&](const ir::If& ifExpr) {
                       std::string thenLabel = generateLabel();
                       std::string elseLabel = generateLabel();
                       std::string endLabel = generateLabel();

                       std::string condResult;
                       convertANF(std::make_shared<ir::ANF>(ir::Atom { ifExpr.cond }), module, condResult,
                           functionsToGenerate, true, false, currentFunctionLabel);

                       module.addInstr({ Operation::JUMP_IF_NOT, {}, condResult, elseLabel });

                       // Then branch
                       std::string thenResult;
                       convertANF(ifExpr.then, module, thenResult, functionsToGenerate,
                           valueNeeded, isTailPosition, currentFunctionLabel);

                       bool thenIsTailCall = thenResult.empty(); // Empty result means tail call

                       if (!thenIsTailCall) {
                           if (valueNeeded && !isTailPosition) {
                               std::string ifResultTemp = generateTemp();
                               module.addInstr({ Operation::COPY, ifResultTemp, thenResult, {} });
                               result = ifResultTemp;
                           } else {
                               result = thenResult;
                           }
                           module.addInstr({ Operation::JUMP, {}, endLabel, {} });
                       }

                       module.addInstr({ Operation::LABEL, {}, elseLabel, {} });

                       // Else branch
                       std::string elseResult = "()";
                       if (ifExpr._else && *ifExpr._else) {
                           convertANF(*ifExpr._else, module, elseResult, functionsToGenerate,
                               valueNeeded, isTailPosition, currentFunctionLabel);
                       }

                       bool elseIsTailCall = elseResult.empty(); // Empty result means tail call

                       if (!elseIsTailCall) {
                           if (valueNeeded && !isTailPosition && !thenIsTailCall) {
                               // Only copy to the result temp if then branch didn't already create it
                               if (result.empty() || result == "()") {
                                   result = generateTemp();
                               }
                               module.addInstr({ Operation::COPY, result, elseResult, {} });
                           } else if (!thenIsTailCall) {
                               result = elseResult;
                           }
                       }

                       // Only emit end label if at least one branch didn't tail call
                       if (!thenIsTailCall || !elseIsTailCall) {
                           module.addInstr({ Operation::LABEL, {}, endLabel, {} });
                       }

                       // If both branches tail called, we have no result
                       if (thenIsTailCall && elseIsTailCall) {
                           result = "";
                       }
                   },

                   [&](const ir::Lambda& lambda) {
                       std::string funcLabel = generateLabel();
                       functionsToGenerate.push_back({ funcLabel, lambda.params, lambda.body });

                       if (valueNeeded) {
                           result = generateTemp();
                           module.addInstr({ Operation::ALLOC, result, "closure", funcLabel });
                       } else {
                           result = "()";
                       }
                   },

                   [&](const ir::Quote& quote) {
                       if (valueNeeded) {
                           result = generateTemp();
                           std::string quotedString = quote.expr ? quote.expr->toString() : "()";
                           module.addInstr({ Operation::ALLOC, result, "literal", quotedString });
                       } else {
                           result = "()";
                       }
                   },

                   [&](const auto& _) {
                       throw std::runtime_error("Unhandled ANF variant in convertANF");
                   } },
        anf->term);
}

// Also update generateFunctionTacBody to handle empty results
void generateFunctionTacBody(
    const FunctionToProcess& funcInfo,
    ThreeAddressModule& module,
    std::vector<FunctionToProcess>& functionsToGenerate)
{
    std::vector<std::string> paramNames;
    paramNames.reserve(funcInfo.params.size());
    for (const auto& p : funcInfo.params) {
        paramNames.push_back(p.lexeme);
    }

    std::string paramNamesStr;
    if (!paramNames.empty()) {
        paramNamesStr = paramNames[0];
        for (size_t i = 1; i < paramNames.size(); ++i) {
            paramNamesStr += "," + paramNames[i];
        }
    }

    module.addInstr({ Operation::FUNC_BEGIN, {}, funcInfo.label, paramNamesStr });

    std::string bodyResult;
    convertANF(funcInfo.body_anf, module, bodyResult, functionsToGenerate,
        true, true, funcInfo.label);

    // Only generate RETURN if we didn't generate a tail call (indicated by empty result)
    if (!bodyResult.empty()) {
        module.addInstr({ Operation::RETURN, {}, bodyResult, {} });
    }

    module.addInstr({ Operation::FUNC_END, {}, funcInfo.label, {} });
}

ThreeAddressModule anfToTac(const std::vector<std::shared_ptr<ir::TopLevel>>& toplevel)
{
    ThreeAddressModule module;
    tempCounter = 0;
    labelCounter = 0;

    std::vector<FunctionToProcess> functionsToGenerate;

    for (const auto& top : toplevel) {
        if (!top)
            continue;

        std::visit(overloaded {
                       [&](const ir::TDefine& define) {
                           std::string valueResult;
                           convertANF(define.body, module, valueResult, functionsToGenerate, true, false, "");
                           module.addInstr({ Operation::STORE, {}, define.name.lexeme, valueResult });
                       },
                       [&](const std::shared_ptr<ir::ANF>& expr) {
                           std::string result_temp;
                           convertANF(expr, module, result_temp, functionsToGenerate, false, false, "");
                       } },
            top->decl);
    }

    for (size_t i = 0; i < functionsToGenerate.size(); ++i) {
        generateFunctionTacBody(functionsToGenerate[i], module, functionsToGenerate);
    }

    return module;
}

}
