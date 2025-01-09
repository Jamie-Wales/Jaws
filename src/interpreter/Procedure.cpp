#include "Procedure.h"
#include "Error.h"
#include "Value.h"
#include "interpret.h"
#include <optional>

std::optional<SchemeValue> UserProcedure::operator()(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args) const
{
    auto newEnv = std::make_shared<Environment>(
        closure ? closure : state.rootEnv);
    newEnv->pushFrame();
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
            newEnv->popFrame(); // Pop our frame
            state.env = oldEnv;
            return result;
        }
    }

    newEnv->popFrame(); // Pop our frame
    state.env = oldEnv;
    return result;
}
