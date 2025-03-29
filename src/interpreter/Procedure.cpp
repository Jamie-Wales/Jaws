#include "Procedure.h"
#include "Error.h"
#include "Value.h"
#include "interpret.h"
#include <optional>

std::optional<SchemeValue> UserProcedure::operator()(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args) const
{
    size_t minArgs = parameters.size() - (isVariadic ? 1 : 0);
    if (isVariadic) {
        if (args.size() < minArgs) {
            throw InterpreterError("Too few arguments, expected at least " + std::to_string(minArgs) + ", got " + std::to_string(args.size()));
        }
    } else {
        if (args.size() != parameters.size()) {
            throw InterpreterError("Wrong number of arguments, expected " + std::to_string(parameters.size()) + ", got " + std::to_string(args.size()));
        }
    }
    auto newEnv = std::make_shared<Environment>(closure ? closure : state.rootEnv);
    size_t i = 0;
    for (; i < parameters.size() - (isVariadic ? 1 : 0); i++) {
        newEnv->define(parameters[i].lexeme, args[i]);
    }
    if (isVariadic) {
        std::list<SchemeValue> remainingArgs(args.begin() + i, args.end());
        newEnv->define(parameters.back().lexeme, SchemeValue(remainingArgs));
    }
    auto oldEnv = state.env;
    state.env = newEnv;
    std::optional<SchemeValue> result;
    for (const auto& expr : body) {
        result = interpret::interpret(state, expr);
        if (result && result->isProc() && result->asProc()->isTailCall()) {
            // No need to pop frame in new model
            state.env = oldEnv;
            return result;
        }
    }
    state.env = oldEnv;
    return result;
}
