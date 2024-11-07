#pragma once
#include "Expression.h"
#include "Token.h"
#include "Value.h"
#include <functional>
#include <memory>
#include <vector>
class SchemeValue;
class Interpreter;

class Procedure {
public:
    virtual std::optional<SchemeValue> operator()(Interpreter& interp, const std::vector<SchemeValue>& args) const = 0;
    virtual ~Procedure() = default;
};

class BuiltInProcedure : public Procedure {
public:
    using Func = std::function<std::optional<SchemeValue>(Interpreter&, const std::vector<SchemeValue>&)>;

    explicit BuiltInProcedure(Func f)
        : func(std::move(f))
    {
    }
    explicit BuiltInProcedure(std::optional<SchemeValue> (*f)(Interpreter&, const std::vector<SchemeValue>&))
        : func(f)
    {
    }

    std::optional<SchemeValue> operator()(Interpreter& interp, const std::vector<SchemeValue>& args) const override
    {
        return func(interp, args);
    }

private:
    Func func;
};

class UserProcedure : public Procedure {
public:
    UserProcedure(std::vector<Token> parameters,
        std::vector<std::shared_ptr<Expression>> body)
        : parameters(std::move(parameters))
        , body(std::move(body))
    {
    }
    std::optional<SchemeValue> operator()(Interpreter& interp,
        const std::vector<SchemeValue>& args) const override;
    std::vector<Token> parameters;
    std::vector<std::shared_ptr<Expression>> body;
};
