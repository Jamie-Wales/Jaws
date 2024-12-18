#pragma once
#include "Expression.h"
#include "Token.h"
#include "Value.h"
#include <functional>
#include <memory>
#include <vector>

// Forward declare to avoid circular dependency
namespace interpret {
    struct InterpreterState;
}

class Procedure {
public:
    virtual std::optional<SchemeValue> operator()(interpret::InterpreterState& state, 
        const std::vector<SchemeValue>& args) const = 0;
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
        : func(std::move(f)) {}
        
    explicit BuiltInProcedure(std::optional<SchemeValue> (*f)(
        interpret::InterpreterState&, const std::vector<SchemeValue>&))
        : func(f) {}

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
    UserProcedure(std::vector<Token> parameters,
                  std::vector<std::shared_ptr<Expression>> body)
        : parameters(std::move(parameters))
        , body(std::move(body)) {}

    std::optional<SchemeValue> operator()(
        interpret::InterpreterState& state,
        const std::vector<SchemeValue>& args) const override;

    std::vector<Token> parameters;
    std::vector<std::shared_ptr<Expression>> body;
};

class TailCall : public Procedure {
public:
    TailCall(std::shared_ptr<Procedure> proc, std::vector<SchemeValue> args)
        : proc(std::move(proc))
        , args(std::move(args)) {}

    std::optional<SchemeValue> operator()(
        interpret::InterpreterState& state, 
        const std::vector<SchemeValue>&) const override 
    {
        throw std::runtime_error("TailCall::operator() should not be called directly");
    }

    bool isTailCall() const override { return true; }

    std::shared_ptr<Procedure> proc;
    std::vector<SchemeValue> args;
};
