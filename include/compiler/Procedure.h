#pragma once
#include "Environment.h"
#include "Expression.h"
#include "Token.h"
#include "Value.h"
#include <functional>
#include <memory>
#include <vector>

namespace interpret {
struct InterpreterState;
}

class Procedure {
public:
    virtual std::optional<SchemeValue> operator()(interpret::InterpreterState& state,
        const std::vector<SchemeValue>& args) const
        = 0;
    virtual ~Procedure() = default;
    virtual bool isTailCall() const { return false; }
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
        return func(state, args);
    }

private:
    Func func;
};

class UserProcedure : public Procedure {
public:
    std::vector<Token> parameters;
    std::vector<std::shared_ptr<Expression>> body;
    std::shared_ptr<Environment> closure;
    bool isVariadic = false;

    UserProcedure(std::vector<Token> params,
        std::vector<std::shared_ptr<Expression>> b,
        std::shared_ptr<Environment> env = nullptr,
        bool variadic = false)
        : parameters(std::move(params))
        , body(std::move(b))
        , closure(env)
        , isVariadic(variadic)
    {
    }

    std::optional<SchemeValue> operator()(
        interpret::InterpreterState& state,
        const std::vector<SchemeValue>& args) const override;
};

class TailCall : public Procedure {
public:
    std::shared_ptr<Procedure> proc;
    std::vector<SchemeValue> args;

    TailCall(std::shared_ptr<Procedure> proc, std::vector<SchemeValue> args)
        : proc(std::move(proc))
        , args(std::move(args))
    {
    }

    std::optional<SchemeValue> operator()(
        interpret::InterpreterState& state,
        const std::vector<SchemeValue>&) const override
    {
        throw std::runtime_error("TailCall::operator() should not be called directly");
    }

    bool isTailCall() const override { return true; }
};
