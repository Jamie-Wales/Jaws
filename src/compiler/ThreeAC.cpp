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
    }
    return "UNKNOWN";
}

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
    bool valueNeeded = true);

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
    bool valueNeeded)
{
    if (!anf) {
        result = "()";
        return;
    }

    std::visit(overloaded {
                   [&](const ir::Let& let) {
                       std::string bindingResult;
                       convertANF(let.binding, module, bindingResult, functionsToGenerate, let.name.has_value());

                       if (let.name) {
                           module.addInstr({ Operation::STORE, {}, let.name->lexeme, bindingResult });
                           convertANF(let.body, module, result, functionsToGenerate, valueNeeded);
                       } else {
                           convertANF(let.body, module, result, functionsToGenerate, valueNeeded);
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
                       std::vector<std::string> paramSources;
                       paramSources.reserve(app.params.size());
                       for (const auto& param_token : app.params) {
                           std::string paramResult;

                           convertANF(std::make_shared<ir::ANF>(ir::Atom { param_token }), module, paramResult, functionsToGenerate, true);
                           paramSources.push_back(paramResult);
                       }

                       for (const auto& source_name : paramSources) {
                           module.addInstr({ Operation::PARAM, {}, source_name, {} });
                       }

                       std::string funcTarget = app.name.lexeme;

                       if (!isTempVariable(funcTarget) && funcTarget.find('$') != 0 && funcTarget.find('L') != 0) {
                           std::string loadedFunc = generateTemp();
                           module.addInstr({ Operation::LOAD, loadedFunc, funcTarget, {} });
                           funcTarget = loadedFunc;
                       }

                       if (valueNeeded) {
                           result = generateTemp();
                           module.addInstr({ Operation::CALL, result, funcTarget, std::to_string(paramSources.size()) });
                       } else {

                           module.addInstr({ Operation::CALL, {}, funcTarget, std::to_string(paramSources.size()) });
                           result = "()";
                       }
                   },

                   [&](const ir::If& ifExpr) {
                       std::string thenLabel = generateLabel();
                       std::string elseLabel = generateLabel();
                       std::string endLabel = generateLabel();

                       std::string condResult;

                       convertANF(std::make_shared<ir::ANF>(ir::Atom { ifExpr.cond }), module, condResult, functionsToGenerate, true);

                       module.addInstr({ Operation::JUMP_IF_NOT, {}, condResult, elseLabel });

                       std::string thenResult;
                       convertANF(ifExpr.then, module, thenResult, functionsToGenerate, valueNeeded);
                       std::string ifResultTemp = "()";
                       if (valueNeeded) {
                           ifResultTemp = generateTemp();
                           module.addInstr({ Operation::COPY, ifResultTemp, thenResult, {} });
                       }
                       module.addInstr({ Operation::JUMP, {}, endLabel, {} });

                       module.addInstr({ Operation::LABEL, {}, elseLabel, {} });
                       std::string elseResult = "()";
                       if (ifExpr._else && *ifExpr._else) {
                           convertANF(*ifExpr._else, module, elseResult, functionsToGenerate, valueNeeded);
                       }
                       if (valueNeeded) {

                           module.addInstr({ Operation::COPY, ifResultTemp, elseResult, {} });
                       }

                       module.addInstr({ Operation::LABEL, {}, endLabel, {} });
                       result = ifResultTemp;
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
    convertANF(funcInfo.body_anf, module, bodyResult, functionsToGenerate, true);

    module.addInstr({ Operation::RETURN, {}, bodyResult, {} });
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
                           convertANF(define.body, module, valueResult, functionsToGenerate, true);
                           module.addInstr({ Operation::STORE, {}, define.name.lexeme, valueResult });
                       },
                       [&](const std::shared_ptr<ir::ANF>& expr) {
                           std::string result_temp;
                           convertANF(expr, module, result_temp, functionsToGenerate, false);
                       } },
            top->decl);
    }

    for (size_t i = 0; i < functionsToGenerate.size(); ++i) {
        generateFunctionTacBody(functionsToGenerate[i], module, functionsToGenerate);
    }

    return module;
}

} // namespace tac
