#include "builtins/JawsVector.h"
#include "Error.h"
#include "Number.h"

namespace jaws_vec {

std::optional<SchemeValue> makeVector(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args) 
{
    if (args.size() < 1 || args.size() > 2) {
        throw InterpreterError("make-vector requires 1 or 2 arguments");
    }

    auto size = args[0].ensureValue();
    if (!size.isNumber()) {
        throw InterpreterError("make-vector: first argument must be a number");
    }

    int k = size.asNumber().toInt();
    if (k < 0) {
        throw InterpreterError("make-vector: length must be non-negative");
    }

    SchemeValue fill = (args.size() == 2) 
        ? args[1].ensureValue() 
        : SchemeValue(Number(0));

    return SchemeValue(std::vector<SchemeValue>(k, fill));
}

std::optional<SchemeValue> vectorProcedure(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args) 
{
    std::vector<SchemeValue> result;
    result.reserve(args.size());
    
    for (const auto& arg : args) {
        result.push_back(arg.ensureValue());
    }
    
    return SchemeValue(std::move(result));
}

std::optional<SchemeValue> vectorRef(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args) 
{
    if (args.size() != 2) {
        throw InterpreterError("vector-ref requires exactly 2 arguments");
    }

    auto vec = args[0].ensureValue();
    if (!vec.isVector()) {
        throw InterpreterError("vector-ref: first argument must be a vector");
    }

    auto index = args[1].ensureValue();
    if (!index.isNumber()) {
        throw InterpreterError("vector-ref: second argument must be a number");
    }

    int idx = index.asNumber().toInt();
    const auto& vector = std::get<std::vector<SchemeValue>>(vec.value);
    
    if (idx < 0 || static_cast<size_t>(idx) >= vector.size()) {
        throw InterpreterError("vector-ref: index out of bounds");
    }

    return vector[idx];
}

std::optional<SchemeValue> vectorSet(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args) 
{
    if (args.size() != 3) {
        throw InterpreterError("vector-set! requires exactly 3 arguments");
    }

    auto vec = args[0].ensureValue();
    if (!vec.isVector()) {
        throw InterpreterError("vector-set!: first argument must be a vector");
    }

    auto index = args[1].ensureValue();
    if (!index.isNumber()) {
        throw InterpreterError("vector-set!: second argument must be a number");
    }

    int idx = index.asNumber().toInt();
    auto& vector = std::get<std::vector<SchemeValue>>(vec.value);
    
    if (idx < 0 || static_cast<size_t>(idx) >= vector.size()) {
        throw InterpreterError("vector-set!: index out of bounds");
    }

    // Create new vector with updated value
    std::vector<SchemeValue> newVec = vector;
    newVec[idx] = args[2].ensureValue();
    return SchemeValue(std::move(newVec));
}

std::optional<SchemeValue> vectorLength(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args) 
{
    if (args.size() != 1) {
        throw InterpreterError("vector-length requires exactly 1 argument");
    }

    auto vec = args[0].ensureValue();
    if (!vec.isVector()) {
        throw InterpreterError("vector-length: argument must be a vector");
    }

    return SchemeValue(Number(static_cast<int>(
        std::get<std::vector<SchemeValue>>(vec.value).size())));
}

} // namespace jaws_vec
