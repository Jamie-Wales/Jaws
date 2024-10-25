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
    static SchemeValue plus(Interpreter&, const std::vector<SchemeValue>& args);
    static SchemeValue minus(Interpreter&, const std::vector<SchemeValue>& args);
    static SchemeValue mult(Interpreter&, const std::vector<SchemeValue>& args);
    static SchemeValue div(Interpreter&, const std::vector<SchemeValue>& args);
    static SchemeValue less(Interpreter&, const std::vector<SchemeValue>& args);
    static SchemeValue greater(Interpreter&, const std::vector<SchemeValue>& args);
    static SchemeValue equal(Interpreter&, const std::vector<SchemeValue>& args);

    /* ---- Builtin Procedures  */
    static SchemeValue isBooleanProc(Interpreter&, const std::vector<SchemeValue>& args);
    static SchemeValue listProcedure(Interpreter&, const std::vector<SchemeValue>& args);
    static SchemeValue carProcudure(Interpreter&, const std::vector<SchemeValue>& args);
    static SchemeValue cdrProcedure(Interpreter&, const std::vector<SchemeValue>& args);
    static SchemeValue cadrProcedure(Interpreter&, const std::vector<SchemeValue>& args);
    static SchemeValue cons(Interpreter&, const std::vector<SchemeValue>& args);
    static SchemeValue length(Interpreter&, const std::vector<SchemeValue>& args);
    static SchemeValue append(Interpreter&, const std::vector<SchemeValue>& args);
    static SchemeValue reverse(Interpreter&, const std::vector<SchemeValue>& args);
    static SchemeValue listRef(Interpreter&, const std::vector<SchemeValue>& args);
    static SchemeValue listTail(Interpreter&, const std::vector<SchemeValue>& args);
    static SchemeValue listSet(Interpreter&, const std::vector<SchemeValue>& args);

    static SchemeValue read(Interpreter&, const std::vector<SchemeValue>& args);
    static SchemeValue write(Interpreter&, const std::vector<SchemeValue>& args);
    static SchemeValue display(Interpreter&, const std::vector<SchemeValue>& args);
    static SchemeValue newline(Interpreter&, const std::vector<SchemeValue>& args);
    static SchemeValue openInputFile(Interpreter&, const std::vector<SchemeValue>& args);
    static SchemeValue openOutputFile(Interpreter&, const std::vector<SchemeValue>& args);
    static SchemeValue closePort(Interpreter&, const std::vector<SchemeValue>& args);

public:
    Interpreter();
    std::unordered_map<std::string, SchemeValue> environment;
    std::optional<SchemeValue> interpret(const std::unique_ptr<Expression>& e);
    SchemeValue lookupVariable(const std::string& name) const;
};
