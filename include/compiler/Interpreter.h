#pragma once
#include "Expression.h"
#include "Value.h"
#include <memory>
#include <optional>
#include <unordered_map>

class InterpreterError;

class Interpreter {
private:
    std::optional<SchemeValue> defineExpression(const DefineExpression& de, const Expression& expr);
    std::optional<SchemeValue> interpretAtom(const AtomExpression& atom, const Expression& expr);
    std::optional<SchemeValue> interpretList(const ListExpression& list, const Expression& expr);
    std::optional<SchemeValue> interpretSExpression(const sExpression& se, const Expression& expr);
    std::optional<SchemeValue> defineProcedure(DefineProcedure& dp, const Expression& e);

    // All built-in procedures should return std::optional<SchemeValue>
    static std::optional<SchemeValue> plus(Interpreter&, const std::vector<SchemeValue>& args);
    static std::optional<SchemeValue> minus(Interpreter&, const std::vector<SchemeValue>& args);
    static std::optional<SchemeValue> mult(Interpreter&, const std::vector<SchemeValue>& args);
    static std::optional<SchemeValue> div(Interpreter&, const std::vector<SchemeValue>& args);
    static std::optional<SchemeValue> less(Interpreter&, const std::vector<SchemeValue>& args);
    static std::optional<SchemeValue> greater(Interpreter&, const std::vector<SchemeValue>& args);
    static std::optional<SchemeValue> equal(Interpreter&, const std::vector<SchemeValue>& args);
    
    /* ---- Builtin Procedures  */
    static std::optional<SchemeValue> isBooleanProc(Interpreter&, const std::vector<SchemeValue>& args);
    static std::optional<SchemeValue> listProcedure(Interpreter&, const std::vector<SchemeValue>& args);
    static std::optional<SchemeValue> carProcudure(Interpreter&, const std::vector<SchemeValue>& args);
    static std::optional<SchemeValue> cdrProcedure(Interpreter&, const std::vector<SchemeValue>& args);
    static std::optional<SchemeValue> cadrProcedure(Interpreter&, const std::vector<SchemeValue>& args);
    static std::optional<SchemeValue> cons(Interpreter&, const std::vector<SchemeValue>& args);
    static std::optional<SchemeValue> length(Interpreter&, const std::vector<SchemeValue>& args);
    static std::optional<SchemeValue> append(Interpreter&, const std::vector<SchemeValue>& args);
    static std::optional<SchemeValue> reverse(Interpreter&, const std::vector<SchemeValue>& args);
    static std::optional<SchemeValue> listRef(Interpreter&, const std::vector<SchemeValue>& args);
    static std::optional<SchemeValue> listTail(Interpreter&, const std::vector<SchemeValue>& args);
    static std::optional<SchemeValue> listSet(Interpreter&, const std::vector<SchemeValue>& args);
    static std::optional<SchemeValue> read(Interpreter&, const std::vector<SchemeValue>& args);
    static std::optional<SchemeValue> write(Interpreter&, const std::vector<SchemeValue>& args);
    static std::optional<SchemeValue> display(Interpreter&, const std::vector<SchemeValue>& args);
    static std::optional<SchemeValue> newline(Interpreter&, const std::vector<SchemeValue>& args);
    static std::optional<SchemeValue> openInputFile(Interpreter&, const std::vector<SchemeValue>& args);
    static std::optional<SchemeValue> openOutputFile(Interpreter&, const std::vector<SchemeValue>& args);
    static std::optional<SchemeValue> closePort(Interpreter&, const std::vector<SchemeValue>& args);

public:
    Interpreter();
    std::unordered_map<std::string, std::optional<SchemeValue>> environment;
    std::optional<SchemeValue> interpret(const std::unique_ptr<Expression>& e);
    std::optional<SchemeValue> lookupVariable(const std::string& name) const;
};
