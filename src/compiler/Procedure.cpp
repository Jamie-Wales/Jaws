#include "Procedure.h"
#include "Error.h"
#include "Interpreter.h"
#include "Value.h"
#include <optional>
std::optional<SchemeValue> UserProcedure::operator()(Interpreter& interp,
    const std::vector<SchemeValue>& args) const
{
    if (args.size() != parameters.size()) {
        throw InterpreterError("Expected " + std::to_string(parameters.size()) + " arguments but got " + std::to_string(args.size()));
    }
    interp.scope->pushFrame();

    for (size_t i = 0; i < parameters.size(); i++) {
        interp.scope->define(parameters[i].lexeme, args[i]);
    }

    std::optional<SchemeValue> result;
    for (const auto& expr : body) {
        result = interp.interpret(expr);
        if (result && result->isProc() && result->asProc()->isTailCall()) {
            interp.scope->popFrame();
            return result;
        }
    }

    interp.scope->popFrame();
    return result;
}
