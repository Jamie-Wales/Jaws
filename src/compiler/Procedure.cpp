#include "Procedure.h"
#include "Error.h"
#include "Value.h"
#include "interpret.h"
#include <optional>

std::optional<SchemeValue> UserProcedure::operator()(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args) const
{
    if (args.size() != parameters.size()) {
        throw InterpreterError(
            "Expected " + std::to_string(parameters.size()) + " arguments but got " + std::to_string(args.size()));
    }

    state.env->pushFrame();

    // Bind arguments to parameters
    for (size_t i = 0; i < parameters.size(); i++) {
        state.env->define(parameters[i].lexeme, args[i]);
    }

    // Execute body
    std::optional<SchemeValue> result;
    for (const auto& expr : body) {
        result = interpret::interpret(state, expr);
        if (result && result->isProc() && result->asProc()->isTailCall()) {
            state.env->popFrame();
            return result;
        }
    }

    state.env->popFrame();
    return result;
}
