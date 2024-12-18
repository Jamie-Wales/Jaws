#pragma once
#include "Environment.h"
#include "Error.h"
#include "Expression.h"
#include "MacroProcessor.h"
#include "Value.h"
#include <memory>
#include <optional>
#include <sstream>

namespace interpret {

// Central interpreter state
struct InterpreterState {
    std::shared_ptr<Environment> env;
    MacroProcessor macros;
    std::stringstream output;

    InterpreterState()
        : env(std::make_shared<Environment>())
    {
    }
};

InterpreterState createInterpreter();

std::optional<SchemeValue> interpret(
    InterpreterState& state,
    const std::shared_ptr<Expression>& expr);

struct ProcedureCall {
    SchemeValue procedure;
    std::vector<SchemeValue> arguments;
};

std::optional<SchemeValue> interpretAtom(InterpreterState& state, const AtomExpression& atom);
std::optional<SchemeValue> interpretList(InterpreterState& state, const ListExpression& list);
std::optional<SchemeValue> interpretSExpression(InterpreterState& state, const sExpression& sexpr);
std::optional<SchemeValue> interpretDefine(InterpreterState& state, const DefineExpression& def);
std::optional<SchemeValue> interpretDefineProcedure(InterpreterState& state, const DefineProcedure& proc);
std::optional<SchemeValue> interpretLambda(InterpreterState& state, const LambdaExpression& lambda);
std::optional<SchemeValue> interpretIf(InterpreterState& state, const IfExpression& ifexpr);
std::optional<SchemeValue> interpretLet(InterpreterState& state, const LetExpression& let);
std::optional<SchemeValue> interpretQuote(InterpreterState& state, const QuoteExpression& quote);
std::optional<SchemeValue> interpretVector(InterpreterState& state, const VectorExpression& vec);
std::optional<SchemeValue> interpretTailCall(InterpreterState& state, const TailExpression& tail);
std::optional<SchemeValue> interpretImport(InterpreterState& state, const ImportExpression& import);
std::optional<SchemeValue> interpretSet(InterpreterState& state, const SetExpression& set);

std::optional<ProcedureCall> evaluateProcedureCall(
    InterpreterState& state,
    const sExpression& sexpr);

std::optional<SchemeValue> executeProcedure(
    InterpreterState& state,
    const SchemeValue& proc,
    const std::vector<SchemeValue>& args);

}
