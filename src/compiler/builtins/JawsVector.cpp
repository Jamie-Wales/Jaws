#include "builtins/JawsVector.h"
#include "Error.h"
namespace jaws_vec {
std::optional<SchemeValue> makeVector(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() < 1 || args.size() > 2) {
        throw InterpreterError("make-vector requires 1 or 2 arguments");
    }

    SchemeValue sizeArg = args[0];
    if (sizeArg.isExpr())
        sizeArg = expressionToValue(*sizeArg.asExpr());
    if (!sizeArg.isNumber()) {
        throw InterpreterError("make-vector: first argument must be a number");
    }
    int k = sizeArg.as<Number>().toInt();
    if (k < 0) {
        throw InterpreterError("make-vector: length must be non-negative");
    }

    SchemeValue fill = args.size() == 2 ? args[1] : SchemeValue(Number(0));
    if (fill.isExpr())
        fill = expressionToValue(*fill.asExpr());
    std::vector<SchemeValue> vec(k, fill);
    return SchemeValue(std::move(vec));
}

std::optional<SchemeValue> vectorProcedure(Interpreter&, const std::vector<SchemeValue>& args)
{
    std::vector<SchemeValue> evaluated;
    evaluated.reserve(args.size());
    for (const auto& arg : args) {
        if (arg.isExpr()) {
            evaluated.push_back(expressionToValue(*arg.asExpr()));
        } else {
            evaluated.push_back(arg);
        }
    }
    return SchemeValue(evaluated);
}

std::optional<SchemeValue> vectorRef(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() != 2) {
        throw InterpreterError("vector-ref requires exactly 2 arguments");
    }

    SchemeValue vecArg = args[0];
    if (vecArg.isExpr())
        vecArg = expressionToValue(*vecArg.asExpr());
    const auto* vec = std::get_if<std::vector<SchemeValue>>(&vecArg.value);
    if (!vec) {
        throw InterpreterError("vector-ref: first argument must be a vector");
    }

    SchemeValue indexArg = args[1];
    if (indexArg.isExpr())
        indexArg = expressionToValue(*indexArg.asExpr());
    if (!indexArg.isNumber()) {
        throw InterpreterError("vector-ref: second argument must be a number");
    }
    int index = indexArg.as<Number>().toInt();
    if (index < 0 || static_cast<size_t>(index) >= vec->size()) {
        throw InterpreterError("vector-ref: index out of bounds");
    }

    return (*vec)[index];
}

std::optional<SchemeValue> vectorSet(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() != 3) {
        throw InterpreterError("vector-set! requires exactly 3 arguments");
    }

    SchemeValue vecArg = args[0];
    if (vecArg.isExpr())
        vecArg = expressionToValue(*vecArg.asExpr());
    auto* vec = std::get_if<std::vector<SchemeValue>>(&vecArg.value);
    if (!vec) {
        throw InterpreterError("vector-set!: first argument must be a vector");
    }

    SchemeValue indexArg = args[1];
    if (indexArg.isExpr())
        indexArg = expressionToValue(*indexArg.asExpr());
    if (!indexArg.isNumber()) {
        throw InterpreterError("vector-set!: second argument must be a number");
    }
    int index = indexArg.as<Number>().toInt();
    if (index < 0 || static_cast<size_t>(index) >= vec->size()) {
        throw InterpreterError("vector-set!: index out of bounds");
    }

    SchemeValue valueArg = args[2];
    if (valueArg.isExpr())
        valueArg = expressionToValue(*valueArg.asExpr());

    std::vector<SchemeValue> newVec = *vec;
    newVec[index] = valueArg;
    return SchemeValue(std::move(newVec));
}

std::optional<SchemeValue> vectorLength(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("vector-length requires exactly 1 argument");
    }

    SchemeValue arg = args[0];
    if (arg.isExpr())
        arg = expressionToValue(*arg.asExpr());
    const auto* vec = std::get_if<std::vector<SchemeValue>>(&arg.value);
    if (!vec) {
        throw InterpreterError("vector-length: argument must be a vector");
    }
    return SchemeValue(Number(static_cast<int>(vec->size())));
}
}
