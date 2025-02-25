#include "ThreeAC.h"
#include <sstream>
#include <unordered_set>

namespace tac {

static int tempCounter = 0;
static int labelCounter = 0;
static std::unordered_set<std::string> builtinFunctions = { "apply", "call-with-current-continuation", "call/cc",
    "map", "for-each", "cons", "car", "cdr", "list",
    "+", "-", "*", "/", "=", "<", ">", "<=", ">=",
    "not", "and", "or", "eq?", "eqv?", "equal?", "display" };

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
    case Operation::BCALL:
        return "BCALL";
    case Operation::LCALL:
        return "LCALL";
    case Operation::JUMP_IF:
        return "JUMP_IF";
    case Operation::JUMP_IF_NOT:
        return "JUMP_IF_NOT";
    case Operation::ALLOC:
        return "ALLOC";
    case Operation::RETURN:
        return "RETURN";
    case Operation::LOAD:
        return "LOAD";
    case Operation::STORE:
        return "STORE";
    case Operation::GC:
        return "GC";
    case Operation::PUSH_PARAM:
        return "PushParam";
    case Operation::POP_PARAMS:
        return "PopParams";
    case Operation::BEGIN_FUNC:
        return "BeginFunc";
    case Operation::END_FUNC:
        return "EndFunc";
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
                           result = generateTemp();

                           // Push parameters in reverse order (right to left)
                           for (int i = app.params.size() - 1; i >= 0; i--) {
                               module.addInstr({ Operation::PUSH_PARAM, {}, app.params[i].lexeme, {} });
                           }

                           bool isBuiltin = builtinFunctions.count(app.name.lexeme) > 0;

                           if (isBuiltin) {
                               module.addInstr({ Operation::BCALL, result, app.name.lexeme,
                                   std::to_string(app.params.size()) });
                           } else {
                               // Use LCall for user-defined functions
                               module.addInstr({ Operation::LCALL, result, app.name.lexeme,
                                   std::to_string(app.params.size()) });
                           }

                           // Pop params after call
                           int paramBytes = app.params.size() * 4; // Assuming 4 bytes per parameter
                           module.addInstr({ Operation::POP_PARAMS, {}, std::to_string(paramBytes), {} });

                           if (app.is_tail) {
                               module.addInstr({ Operation::JUMP, {}, app.name.lexeme, {} });
                           }
                       },

                       [&](const ir::If& ifExpr) {
                           // Your if handling looks good, no changes needed
                           std::string thenLabel = generateLabel();
                           std::string elseLabel = generateLabel();
                           std::string endLabel = generateLabel();
                           result = generateTemp();
                           module.addInstr({ Operation::JUMP_IF_NOT, {}, ifExpr.cond.lexeme, elseLabel });
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

                           // Calculate local space needed (estimate based on params and temps)
                           int localSpace = lambda.params.size() * 4 + 16; // Basic estimate
                           module.addInstr({ Operation::BEGIN_FUNC, {}, std::to_string(localSpace), {} });

                           for (size_t i = 0; i < lambda.params.size(); i++) {
                               module.addInstr({ Operation::COPY, lambda.params[i].lexeme, "arg" + std::to_string(i), {} });
                           }

                           std::string bodyResult;
                           convertANF(lambda.body, module, bodyResult);
                           module.addInstr({ Operation::COPY, "return", bodyResult, {} });
                           module.addInstr({ Operation::END_FUNC, {}, {}, {} });
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

    // Generate a "main" section entry point
    module.addInstr({ Operation::LABEL, {}, "main", {} });
    module.addInstr({ Operation::BEGIN_FUNC, {}, "16", {} }); // Basic space for main

    for (const auto& top : toplevel) {
        std::visit(overloaded {
                       [&](const ir::TDefine& define) {
                           // Generate a unique label for this function
                           std::string funcLabel = "L_" + define.name.lexeme;

                           // Jump past function definition for now (to be processed later)
                           std::string skipLabel = generateLabel();
                           module.addInstr({ Operation::JUMP, {}, skipLabel, {} });

                           // Define the function
                           module.addInstr({ Operation::LABEL, {}, funcLabel, {} });

                           // If it's a lambda, process it as a function
                           if (define.body && std::holds_alternative<ir::Lambda>(define.body->term)) {
                               const auto& lambda = std::get<ir::Lambda>(define.body->term);

                               // Calculate stack space - params + locals + temporaries
                               int frameSize = lambda.params.size() * 4 + 16; // Estimate
                               module.addInstr({ Operation::BEGIN_FUNC, {}, std::to_string(frameSize), {} });

                               // Process parameters
                               for (size_t i = 0; i < lambda.params.size(); i++) {
                                   module.addInstr({ Operation::COPY, lambda.params[i].lexeme, "arg" + std::to_string(i), {} });
                               }
                               std::string bodyResult;
                               convertANF(lambda.body, module, bodyResult);
                               module.addInstr({ Operation::COPY, "return", bodyResult, {} });
                               module.addInstr({ Operation::RETURN, {}, {}, {} });
                               module.addInstr({ Operation::END_FUNC, {}, {}, {} });
                           } else {
                               std::string result;
                               convertANF(define.body, module, result);
                               module.addInstr({ Operation::COPY, "return", result, {} });
                               module.addInstr({ Operation::RETURN, {}, {}, {} });
                               module.addInstr({ Operation::END_FUNC, {}, {}, {} });
                           }

                           module.addInstr({ Operation::LABEL, {}, skipLabel, {} });
                           module.addInstr({ Operation::ALLOC, define.name.lexeme, "closure", {} });
                           module.addInstr({ Operation::STORE, {}, define.name.lexeme, funcLabel });
                       },

                       [&](const std::shared_ptr<ir::ANF>& expr) {
                           std::string result;
                           convertANF(expr, module, result);
                       } },
            top->decl);
    }

    // End main function
    module.addInstr({ Operation::END_FUNC, {}, {}, {} });

    return module;
}

} // namespace tac
