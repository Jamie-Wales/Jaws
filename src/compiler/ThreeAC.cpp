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
    }
    return "UNKNOWN";
}

std::string ThreeACInstruction::toString() const
{
    auto ss = std::stringstream();
    if (arg1)
        ss << *arg1 << " ";
    ss << operationToString(op) << " ";
    if (arg2)
        ss << *arg2 << " ";
    if (result)
        ss << "= " << *result;
    ss << std::endl;
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
    if (anf) {
        std::visit(overloaded {
                       [&](const ir::Let& let) {
                           std::string bindingResult;
                           convertANF(let.binding, module, bindingResult);

                           if (let.name) {
                               module.addInstr({ Operation::COPY, let.name->lexeme, bindingResult, {} });
                               convertANF(let.body, module, result);
                           } else {
                               result = bindingResult;
                               convertANF(let.body, module, result);
                           }
                       },

                       [&](const ir::Atom& atom) {
                           result = atom.atom.lexeme;
                       },

                       [&](const ir::App& app) {
                           // First generate temps for all parameters
                           std::vector<std::string> paramTemps;
                           for (const auto& param : app.params) {
                               std::string paramTemp = generateTemp();
                               module.addInstr({ Operation::COPY, paramTemp, param.lexeme, {} });
                               paramTemps.push_back(paramTemp);
                           }

                           // Then generate result temp and call
                           result = generateTemp();
                           module.addInstr({ Operation::CALL, result, app.name.lexeme,
                               std::to_string(app.params.size()) });

                           if (app.is_tail) {
                               module.addInstr({ Operation::JUMP, {}, app.name.lexeme, {} });
                           }
                       },

                       [&](const ir::If& ifExpr) {
                           std::string thenLabel = generateLabel();
                           std::string elseLabel = generateLabel();
                           std::string endLabel = generateLabel();
                           result = generateTemp();
                           module.addInstr({ Operation::JUMP_IF, {}, ifExpr.cond.lexeme, thenLabel });
                           module.addInstr({ Operation::JUMP, {}, elseLabel, {} });
                           module.addInstr({ Operation::LABEL, {}, thenLabel, {} });
                           std::string thenResult;
                           convertANF(ifExpr.then, module, thenResult);
                           module.addInstr({ Operation::COPY, result, thenResult, {} });
                           module.addInstr({ Operation::JUMP, {}, endLabel, {} });
                           module.addInstr({ Operation::LABEL, {}, elseLabel, {} });
                           if (ifExpr._else) {
                               std::string elseResult;
                               convertANF(*ifExpr._else, module, elseResult);
                               module.addInstr({ Operation::COPY, result, elseResult, {} });
                           }
                           module.addInstr({ Operation::LABEL, {}, endLabel, {} });
                       },

                       [&](const ir::Lambda& lambda) {
                           std::string funcLabel = generateLabel();
                           std::string endLabel = generateLabel();
                           result = generateTemp();
                           module.addInstr({ Operation::JUMP, {}, endLabel, {} });
                           module.addInstr({ Operation::LABEL, {}, funcLabel, {} });
                           for (size_t i = 0; i < lambda.params.size(); i++) {
                               module.addInstr({ Operation::COPY, lambda.params[i].lexeme, "arg" + std::to_string(i), {} });
                           }
                           std::string bodyResult;
                           convertANF(lambda.body, module, bodyResult);
                           module.addInstr({ Operation::COPY, "return", bodyResult, {} });
                           module.functionOffsets[funcLabel] = module.instructions.size() - 1;
                           module.addInstr({ Operation::LABEL, {}, endLabel, {} });
                           module.addInstr({ Operation::ALLOC, result, "closure", {} });
                           module.addInstr({ Operation::STORE, {}, result, funcLabel });
                       },

                       [&](const ir::Quote& quote) {
                           result = generateTemp();
                           module.addInstr({ Operation::ALLOC, result, "literal", {} });
                           module.addInstr({ Operation::STORE, {}, result, quote.expr->toString() });
                       } },
            anf->term);
    }
}

ThreeAddressModule anfToTac(const std::vector<std::shared_ptr<ir::TopLevel>>& toplevel)
{
    ThreeAddressModule module;
    for (const auto& top : toplevel) {
        std::visit(overloaded {
                       [&](const ir::TDefine& define) {
                           std::string result;
                           convertANF(define.body, module, result);
                           module.addInstr({ Operation::STORE, {}, define.name.lexeme, result });
                       },
                       [&](const std::shared_ptr<ir::ANF>& expr) {
                           std::string result;
                           convertANF(expr, module, result);
                       } },
            top->decl);
    }
    return module;
}

} // namespace tac
