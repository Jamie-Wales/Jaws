#include "Procedure.h"
#include "Error.h"
#include "Syntax.h"
#include "Value.h"
#include "interpret.h"
#include <algorithm>
#include <list>
#include <memory>
#include <optional>
#include <vector>

#ifdef DEBUG_LOGGING
#define DEBUG_LOG(x) std::cerr << "[DEBUG] " << x << std::endl
#else
#define DEBUG_LOG(x)
#endif

std::optional<SchemeValue> UserProcedure::operator()(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args) const
{
    return executeBody(state, args);
}

std::optional<SchemeValue> UserProcedure::executeBody(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args) const
{
    try {
        size_t minArgs = parameters.size() - (isVariadic ? 1 : 0);
        DEBUG_LOG("UserProc::executeBody START in Env@ " << state.env.get() << " | Closure Env@ " << closure.get());
        if (isVariadic) {
            if (args.size() < minArgs) {
                throw InterpreterError("User Procedure: Too few arguments, expected at least " + std::to_string(minArgs) + ", got " + std::to_string(args.size()));
            }
        } else {
            if (args.size() != parameters.size()) {
                throw InterpreterError("User Procedure: Wrong number of arguments, expected " + std::to_string(parameters.size()) + ", got " + std::to_string(args.size()));
            }
        }
        if (!closure)
            throw InterpreterError("Internal Error: executeBody called on procedure with null closure.");
        auto newEnv = closure->extend();
        DEBUG_LOG("UserProc::executeBody: Created newEnv@ " << newEnv.get() << " with parent " << closure.get());
        size_t i = 0;
        ScopeID scid = generateFreshScopeID();
        for (; i < minArgs; i++) {
            newEnv->define(parameters[i], args[i]);
        }
        if (isVariadic) {
            std::list<SchemeValue> remainingArgsList(args.begin() + i, args.end());
            auto remainingArgsPtr = std::make_shared<std::list<SchemeValue>>(std::move(remainingArgsList));
            newEnv->define(parameters.back(), SchemeValue(remainingArgsPtr));
        }
        auto oldEnv = state.env;
        state.env = newEnv;
        std::optional<SchemeValue> lastResultOpt = std::nullopt;
        if (body.empty()) {
            DEBUG_LOG("UserProc::executeBody: Empty body, returning nullopt.");
            state.env = oldEnv;
            return std::nullopt;
        }
        for (size_t exprIdx = 0; exprIdx < body.size() - 1; ++exprIdx) {
            DEBUG_LOG("UserProc::executeBody: Interpreting body expr " << exprIdx);
            interpret::interpret(state, body[exprIdx]);
        }
        DEBUG_LOG("UserProc::executeBody: Interpreting LAST body expr");
        lastResultOpt = interpret::interpret(state, body.back());
        bool tailCallWasSet = state.isTailCallPending;
        DEBUG_LOG("UserProc::executeBody: After last expr: TCO pending = " << (tailCallWasSet ? "true" : "false") << ", lastResultOpt has value = " << (lastResultOpt.has_value() ? "true" : "false"));
        if (tailCallWasSet) {
            DEBUG_LOG("UserProc::executeBody: TCO path taken, returning nullopt (Env NOT restored by this frame).");
            if (lastResultOpt) {
                DEBUG_LOG("UserProc::executeBody: ERROR! TCO requested but last expression returned a value: " << lastResultOpt->toString());
                throw InterpreterError("Internal Error: TCO state set, but interpret returned a value.");
            }
            return std::nullopt;
        } else {
            if (lastResultOpt) {
                DEBUG_LOG("UserProc::executeBody: Returning last expr result: " << lastResultOpt->toString());
            } else {
                DEBUG_LOG("UserProc::executeBody: Returning last expr result: nullopt");
            }
            state.env = oldEnv;
            return lastResultOpt;
        }
    } catch (const ContinuationInvocationException& e) {
        throw;
    }
}
