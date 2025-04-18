#include "ThreeAC.h"
#include <sstream>

namespace tac {

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
            ss << "FUNC_BEGIN " << *arg1; // Name/Label
        if (arg2)
            ss << " " << *arg2; // Param Count
        break;
    case Operation::FUNC_END:
        if (arg1)
            ss << "FUNC_END " << *arg1; // Name/Label
        break;
    case Operation::LABEL:
        if (arg1)
            ss << *arg1 << ":"; // Common label format
        ss << operationToString(op);
        break;
    case Operation::JUMP:
        ss << "JUMP ";
        if (arg1)
            ss << *arg1; // Target Label
        break;
    case Operation::JUMP_IF:
        ss << "JUMP_IF ";
        if (arg1)
            ss << *arg1; // Condition var
        ss << " -> ";
        if (arg2)
            ss << *arg2; // Target Label
        break;
    case Operation::JUMP_IF_NOT:
        ss << "JUMP_IF_NOT ";
        if (arg1)
            ss << *arg1; // Condition var
        ss << " -> ";
        if (arg2)
            ss << *arg2; // Target Label
        break;
    default:
        if (result)
            ss << *result << " = ";
        if (arg1)
            ss << *arg1 << " ";
        ss << operationToString(op) << " ";
        if (arg2)
            ss << *arg2 << " ";
        // Trim trailing space if arg2 exists? Depends on desired format
        break;
    }
    return ss.str();
}
void ThreeACInstruction::toString(std::stringstream& ss) const
{
    if (arg1)
        ss << *arg1 << " ";
    ss << operationToString(op) << " ";
    if (arg2)
        ss << *arg2 << " ";
    if (result)
        ss << "= " << *result;
    ss << std::endl;
}

std::string ThreeAddressModule::toString() const
{
    auto ss = std::stringstream();
    for (auto& instruction : instructions) {
        instruction.toString(ss);
    }
    return ss.str();
}

void ThreeAddressModule::addInstr(const ThreeACInstruction instr)
{
    instructions.push_back(std::move(instr));
}

void convertANF(const std::shared_ptr<ir::ANF>& anf, ThreeAddressModule& module, std::string& result)
{
    if (!anf) {
        result = "/* <null_anf> */"; // Placeholder
        return;
    }

    std::visit(overloaded {
                   [&](const ir::Let& let) {
                       std::string bindingResult;
                       convertANF(let.binding, module, bindingResult);

                       if (let.name) {
                           module.addInstr({ Operation::COPY, let.name->lexeme, bindingResult, {} });
                           convertANF(let.body, module, result);
                       } else {
                           convertANF(let.body, module, result);
                       }
                   },

                   [&](const ir::Atom& atom) {
                       result = atom.atom.lexeme;
                   },

                   [&](const ir::App& app) {
                       std::vector<std::string> paramSources;
                       paramSources.reserve(app.params.size());

                       for (const auto& param_token : app.params) {
                           std::string source_name = param_token.lexeme;
                           paramSources.push_back(source_name);
                       }
                       for (const auto& source_name : paramSources) {
                           module.addInstr({ Operation::PARAM, {}, source_name, {} }); // arg1 holds the source
                       }

                       result = generateTemp(); // Temp to hold the return value
                       module.addInstr({ Operation::CALL, result, app.name.lexeme, std::to_string(paramSources.size()) });
                   },

                   [&](const ir::If& ifExpr) {
                       std::string thenLabel = generateLabel();
                       std::string elseLabel = generateLabel();
                       std::string endLabel = generateLabel();
                       module.addInstr({ Operation::JUMP_IF, {}, ifExpr.cond.lexeme, thenLabel });
                       module.addInstr({ Operation::JUMP, {}, elseLabel, {} }); // Jump to else block
                       module.addInstr({ Operation::LABEL, {}, thenLabel, {} });
                       std::string thenResult;
                       convertANF(ifExpr.then, module, thenResult);
                       result = generateTemp();
                       module.addInstr({ Operation::COPY, result, thenResult, {} });
                       module.addInstr({ Operation::JUMP, {}, endLabel, {} }); // Jump to end
                       module.addInstr({ Operation::LABEL, {}, elseLabel, {} });
                       if (ifExpr._else && *ifExpr._else) { // Check if else branch exists
                           std::string elseResult;
                           convertANF(*ifExpr._else, module, elseResult);
                           module.addInstr({ Operation::COPY, result, elseResult, {} });
                       }
                       // End label
                       module.addInstr({ Operation::LABEL, {}, endLabel, {} });
                   },
                   [&](const ir::Lambda& lambda) {
                       std::string funcLabel = generateLabel();

                       // --- Collect Parameter Names ---
                       std::vector<std::string> paramNames;
                       paramNames.reserve(lambda.params.size());
                       for (const auto& p : lambda.params) {
                           paramNames.push_back(p.lexeme);
                       }

                       // --- Join names into comma-separated string for arg2 ---
                       std::string paramNamesStr;
                       if (!paramNames.empty()) {
                           paramNamesStr = paramNames[0];
                           for (size_t i = 1; i < paramNames.size(); ++i) {
                               paramNamesStr += "," + paramNames[i];
                           }
                           // Optional: using std::accumulate
                           // paramNamesStr = std::accumulate(paramNames.begin() + 1, paramNames.end(), paramNames[0],
                           //     [](const std::string& a, const std::string& b) {
                           //         return a + ',' + b;
                           //     });
                       }

                       // --- Function Definition Start ---
                       // arg1 = function label, arg2 = comma-separated param names string
                       module.addInstr({ Operation::FUNC_BEGIN, {}, funcLabel, paramNamesStr });

                       // Convert the function body
                       std::string bodyResult;
                       convertANF(lambda.body, module, bodyResult);

                       // Generate RETURN for the result
                       module.addInstr({ Operation::RETURN, {}, bodyResult, {} });

                       // --- Function Definition End ---
                       module.addInstr({ Operation::FUNC_END, {}, funcLabel, {} });

                       // --- Closure Allocation ---
                       result = generateTemp();
                       module.addInstr({ Operation::ALLOC, result, "closure", funcLabel });
                   },

                   [&](const ir::Quote& quote) {
                       result = generateTemp();
                       std::string quotedString = quote.expr ? quote.expr->toString() : "()"; // Simplified string representation
                       module.addInstr({ Operation::ALLOC, result, "literal", quotedString }); // Example usage
                   } },
        anf->term);
}

ThreeAddressModule anfToTac(const std::vector<std::shared_ptr<ir::TopLevel>>& toplevel)
{
    ThreeAddressModule module;
    tempCounter = 0; // Reset counters for each conversion
    labelCounter = 0;

    for (const auto& top : toplevel) {
        if (!top)
            continue;

        std::visit(overloaded {
                       [&](const ir::TDefine& define) {
                           std::string result_temp; // Temp holding the value/closure
                           convertANF(define.body, module, result_temp);
                           module.addInstr({ Operation::STORE, {}, define.name.lexeme, result_temp });
                       },
                       [&](const std::shared_ptr<ir::ANF>& expr) {
                           std::string result_temp;
                           convertANF(expr, module, result_temp);
                       } },
            top->decl);
    }
    return module;
}

}
