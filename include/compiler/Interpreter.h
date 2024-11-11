#pragma once
#include "Environment.h"
#include "Expression.h"
#include "Value.h"
#include <memory>
#include <optional>

class InterpreterError;

class Interpreter {
private:
    /* ---- Interpreter Functions */

    std::optional<SchemeValue> defineExpression(const DefineExpression& de, const Expression& expr);
    std::optional<SchemeValue> interpretAtom(const AtomExpression& atom, const Expression& expr);
    std::optional<SchemeValue> interpretList(const ListExpression& list, const Expression& expr);
    std::optional<SchemeValue> lambda(LambdaExpression& l, const Expression& e);
    std::optional<SchemeValue> interpretSExpression(const sExpression& se, const Expression& expr);
    std::optional<SchemeValue> defineProcedure(DefineProcedure& dp, const Expression& e);
    std::optional<SchemeValue> interpretVector(const VectorExpression& v, const Expression& e);
    std::optional<SchemeValue> ifExpression(const IfExpression& i, const Expression& e);
    std::optional<SchemeValue> interpretQuoteExpression(const QuoteExpression& qe, const Expression& e);
    std::optional<SchemeValue> interpretTailExpression(const TailExpression& t, const Expression& e);
    /* ----- Maths procedures ----- */
    static std::optional<SchemeValue> plus(Interpreter&, const std::vector<SchemeValue>& args);
    static std::optional<SchemeValue> minus(Interpreter&, const std::vector<SchemeValue>& args);
    static std::optional<SchemeValue> mult(Interpreter&, const std::vector<SchemeValue>& args);
    static std::optional<SchemeValue> div(Interpreter&, const std::vector<SchemeValue>& args);

    /* ----- boolean procs ----- */
    static std::optional<SchemeValue> less(Interpreter&, const std::vector<SchemeValue>& args);
    static std::optional<SchemeValue> greater(Interpreter&, const std::vector<SchemeValue>& args);
    static std::optional<SchemeValue> equal(Interpreter&, const std::vector<SchemeValue>& args);
    static std::optional<SchemeValue> lessOrEqual(Interpreter&, const std::vector<SchemeValue>& args);
    static std::optional<SchemeValue> greaterOrEqual(Interpreter&, const std::vector<SchemeValue>& args);
    static std::optional<SchemeValue> isBooleanProc(Interpreter&, const std::vector<SchemeValue>& args);

    /* ----- List procedures ----- */
    static std::optional<SchemeValue> map(Interpreter&, const std::vector<SchemeValue>& args);
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

    /* ----- File i/o ----- */
    static std::optional<SchemeValue> read(Interpreter&, const std::vector<SchemeValue>& args);
    static std::optional<SchemeValue> write(Interpreter&, const std::vector<SchemeValue>& args);
    static std::optional<SchemeValue> display(Interpreter&, const std::vector<SchemeValue>& args);
    static std::optional<SchemeValue> newline(Interpreter&, const std::vector<SchemeValue>& args);
    static std::optional<SchemeValue> openInputFile(Interpreter&, const std::vector<SchemeValue>& args);
    static std::optional<SchemeValue> openOutputFile(Interpreter&, const std::vector<SchemeValue>& args);
    static std::optional<SchemeValue> closePort(Interpreter&, const std::vector<SchemeValue>& args);
    static std::optional<SchemeValue> import(Interpreter& interp, const std::vector<SchemeValue>& args);
    /* Vector procedures */

    static std::optional<SchemeValue> eval(Interpreter& interp, const std::vector<SchemeValue>& args);
    static std::optional<SchemeValue> makeVector(Interpreter&, const std::vector<SchemeValue>& args);
    static std::optional<SchemeValue> vectorProcedure(Interpreter&, const std::vector<SchemeValue>& args);
    static std::optional<SchemeValue> vectorRef(Interpreter&, const std::vector<SchemeValue>& args);
    static std::optional<SchemeValue> vectorSet(Interpreter&, const std::vector<SchemeValue>& args);
    static std::optional<SchemeValue> vectorLength(Interpreter&, const std::vector<SchemeValue>& args);
    static std::optional<SchemeValue> printHelp(Interpreter& interp, const std::vector<SchemeValue>& args);
    bool tailCallInProgress;

public:
    Interpreter();

    std::shared_ptr<Environment> scope;
    std::stringstream outputStream;
    void run(const std::vector<std::shared_ptr<Expression>>& expressions);
    std::optional<SchemeValue> interpret(const std::shared_ptr<Expression>& e);
    std::optional<SchemeValue> lookupVariable(const std::string& name) const;

    bool isTailCallInProgress() const
    {
        return tailCallInProgress;
    }

    void setTailCallInProgress(bool value)
    {
        tailCallInProgress = value;
    }
};
