#pragma once
#include "Expression.h"
#include "Token.h"
#include <functional>
#include <memory>
#include <vector>
class SchemeValue;
class Interpreter;

class Procedure {
public:
    virtual SchemeValue operator()(Interpreter&, const std::vector<SchemeValue>&) const = 0;
    virtual ~Procedure() = default;
};

class BuiltInProcedure : public Procedure {
public:
    using Func = std::function<SchemeValue(Interpreter&, const std::vector<SchemeValue>&)>;

    explicit BuiltInProcedure(Func f);
    SchemeValue operator()(Interpreter& interp, const std::vector<SchemeValue>& args) const override;

private:
    Func func;
};

class UserProcedure : public Procedure {
public:
    UserProcedure(std::vector<Token> params, std::unique_ptr<Expression> bodyExpr)
        : paramNames(std::move(params))
        , body(std::move(bodyExpr))
    {
    }

    SchemeValue operator()(Interpreter& interp, const std::vector<SchemeValue>& args) const override;

private:
    std::vector<Token> paramNames;
    std::unique_ptr<Expression> body;
};
