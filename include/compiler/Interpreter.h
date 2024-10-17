#pragma once
#include "Expression.h"
#include "Value.h"
#include <functional>
#include <memory>
#include <optional>
#include <stdexcept>
#include <unordered_map>

class Interpreter {
private:
    std::unordered_map<std::string, SchemeValue> environment;

    class BuiltInProcedure : public SchemeValue::Procedure {
    public:
        using Func = std::function<SchemeValue(Interpreter&, const std::vector<SchemeValue>&)>;
        Func func;

        BuiltInProcedure(Func f)
            : func(std::move(f))
        {
        }

        SchemeValue operator()(Interpreter& interp, const std::vector<SchemeValue>& args) const override
        {
            return func(interp, args);
        }
    };

    std::optional<SchemeValue> define(const DefineExpression& de);
    std::optional<SchemeValue> interpretAtom(const AtomExpression& atom);
    std::optional<SchemeValue> interpretList(const ListExpression& list);
    std::optional<SchemeValue> interpretSExpression(const sExpression& se);

    std::optional<SchemeValue> defineProcedure(const DefineProcedure& se);
    static SchemeValue plus(Interpreter&, const std::vector<SchemeValue>& args);
    static SchemeValue minus(Interpreter&, const std::vector<SchemeValue>& args);

public:
    Interpreter();
    std::optional<SchemeValue> interpret(const std::unique_ptr<Expression>& e);
    SchemeValue lookupVariable(const std::string& name) const;
};
