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

    interpret::InterpreterState state;
    SchemeValue value;
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

class Continuation : public Procedure {
public:
    explicit Continuation(interpret::InterpreterState capturedState)
        : state(capturedState)
    {
        DEBUG_LOG("Continuation created, capturing state with Env=" << state.env.get());
    }

    std::optional<SchemeValue> operator()(
        interpret::InterpreterState& currentState,
        const std::vector<SchemeValue>& args) const override
    {
        SchemeValue returnValue;

        if (args.size() == 0) {
            returnValue = SchemeValue();
        } else if (args.size() == 1) {
            returnValue = args[0];
        } else {
            auto multiValue = std::make_shared<MultiValue>(args);
            returnValue = SchemeValue(multiValue);
        }

        DEBUG_LOG("Continuation invoked! Throwing exception. Value=" << returnValue.toString() << ". Restoring state with Env=" << state.env.get());
        throw ContinuationInvocationException(state, returnValue);
    }

    bool isBuiltin() const override { return true; }

private:
    interpret::InterpreterState state;
};
