#pragma once
#include "Environment.h"
#include "Expression.h"
#include "Syntax.h"
#include "Token.h"
#include "Value.h"
#include "interpret.h"
#include <functional>
#include <memory>
#include <optional>
#include <vector>

namespace interpret {
struct InterpreterState;
}
class ContinuationInvocationException : public std::exception {
public:
    ContinuationInvocationException(interpret::InterpreterState capturedState, SchemeValue retValue)
        : state(std::move(capturedState))
        , value(std::move(retValue))
    {
    }

    const char* what() const noexcept override { return "Continuation invoked"; }

    interpret::InterpreterState state; // The state to restore
    SchemeValue value; // The value to return
};
class Procedure {
public:
    virtual std::optional<SchemeValue> operator()(interpret::InterpreterState& state,
        const std::vector<SchemeValue>& args) const
        = 0;
    virtual ~Procedure() = default;
    virtual bool isUserProcedure() const { return false; }
    virtual bool isBuiltin() const { return false; }
    virtual bool isMacro() const { return false; }
};

class BuiltInProcedure : public Procedure {
public:
    using Func = std::function<std::optional<SchemeValue>(
        interpret::InterpreterState&, const std::vector<SchemeValue>&)>;

    // Constructor now only takes the function
    explicit BuiltInProcedure(Func f)
        : func(std::move(f))
    {
    }

    bool isBuiltin() const override { return true; }

    std::optional<SchemeValue> operator()(
        interpret::InterpreterState& state,
        const std::vector<SchemeValue>& args) const override
    {
        if (!func)
            throw std::runtime_error("BuiltInProcedure: null function pointer");
        return func(state, args);
    }

private:
    Func func;
    // Removed: bool modifiesFirstArg = false;
};

class UserProcedure : public Procedure {
public:
    std::vector<HygienicSyntax> parameters;
    std::vector<std::shared_ptr<Expression>> body;
    std::shared_ptr<Environment> closure;
    bool isVariadic = false;

    UserProcedure(std::vector<HygienicSyntax> params,
        std::vector<std::shared_ptr<Expression>> b,
        std::shared_ptr<Environment> env = nullptr,
        bool variadic = false)
        : parameters(std::move(params))
        , body(std::move(b))
        , closure(env)
        , isVariadic(variadic)
    {
    }

    bool isUserProcedure() const override { return true; }
    std::optional<SchemeValue> executeBody(
        interpret::InterpreterState& state,
        const std::vector<SchemeValue>& args) const;

    std::optional<SchemeValue> operator()(
        interpret::InterpreterState& state,
        const std::vector<SchemeValue>& args) const override;
};
// In Procedure.h / Procedure.cpp
class Continuation : public Procedure {
public:
    explicit Continuation(interpret::InterpreterState capturedState)
        : state(capturedState) // Make a complete copy of the interpreter state
    {
        DEBUG_LOG("Continuation created, capturing state with Env=" << state.env.get());
    }

    std::optional<SchemeValue> operator()(
        interpret::InterpreterState& currentState, // The state when continuation is *called*
        const std::vector<SchemeValue>& args) const override
    {
        // Process arguments to get the single return value
        SchemeValue returnValue;
        if (args.size() != 1) {
            throw InterpreterError("Continuation expects exactly one argument, got " + std::to_string(args.size()));
        }
        returnValue = args[0];

        DEBUG_LOG("Continuation invoked! Throwing exception. Value=" << returnValue.toString() << ". Restoring state with Env=" << state.env.get());
        // Throw the exception with the ORIGINAL captured state and the return value
        throw ContinuationInvocationException(state, returnValue);
        // This function effectively never returns normally via std::optional
    }

    bool isBuiltin() const override { return true; }

private:
    interpret::InterpreterState state; // Complete copy of interpreter state (CAPTURED at call/cc time)
};

// Ensure ContinuationInvocationException is defined (e.g., in Procedure.h or Error.h)
