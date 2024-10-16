#pragma once
#include "Expression.h"
#include "Value.h"
#include <functional>
#include <memory>
#include <stdexcept>
#include <unordered_map>

class Interpreter {
private:
    static std::unordered_map<std::string, SchemeValue> environment;

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

    static SchemeValue define(Interpreter& interp, const std::vector<SchemeValue>& args);
    SchemeValue interpretAtom(const AtomExpression& atom);
    SchemeValue interpretList(const ListExpression& list);
    SchemeValue interpretSExpression(const sExpression& se);

    static SchemeValue plus(Interpreter&, const std::vector<SchemeValue>& args);
    static SchemeValue minus(Interpreter&, const std::vector<SchemeValue>& args);

public:
    Interpreter();
    SchemeValue interpret(const std::unique_ptr<Expression>& e);
    SchemeValue lookupVariable(const std::string& name) const;
};
