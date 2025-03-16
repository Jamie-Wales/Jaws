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

    vector[idx] = args[2].ensureValue();
    return std::nullopt;
}

std::optional<SchemeValue> vectorFill(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    if (args.size() != 2) {
        throw InterpreterError("vector-fill! requires exactly 2 arguments");
    }
    auto vec = args[0].ensureValue();
    if (!vec.isVector()) {
        throw InterpreterError("vector-fill!: first argument must be a vector");
    }
    auto fill = args[1].ensureValue();
    auto& vector = std::get<std::vector<SchemeValue>>(vec.value);

    std::fill(vector.begin(), vector.end(), fill);
    return std::nullopt;
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

std::optional<SchemeValue> vectorCopy(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    if (args.size() < 1 || args.size() > 3) {
        throw InterpreterError("vector-copy requires 1 to 3 arguments");
    }
    auto vec = args[0].ensureValue();
    if (!vec.isVector()) {
        throw InterpreterError("vector-copy: first argument must be a vector");
    }
    const auto& vector = std::get<std::vector<SchemeValue>>(vec.value);

    int start = 0;
    int end = vector.size();

    if (args.size() >= 2) {
        auto startArg = args[1].ensureValue();
        if (!startArg.isNumber()) {
            throw InterpreterError("vector-copy: start index must be a number");
        }
        start = startArg.asNumber().toInt();
        if (start < 0 || static_cast<size_t>(start) > vector.size()) {
            throw InterpreterError("vector-copy: start index out of bounds");
        }
    }

    if (args.size() >= 3) {
        auto endArg = args[2].ensureValue();
        if (!endArg.isNumber()) {
            throw InterpreterError("vector-copy: end index must be a number");
        }
        end = endArg.asNumber().toInt();
        if (end < start || static_cast<size_t>(end) > vector.size()) {
            throw InterpreterError("vector-copy: end index out of bounds");
        }
    }

    std::vector<SchemeValue> result(vector.begin() + start, vector.begin() + end);
    return SchemeValue(std::move(result));
}

std::optional<SchemeValue> vectorCopyTo(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    if (args.size() < 3 || args.size() > 5) {
        throw InterpreterError("vector-copy! requires 3 to 5 arguments");
    }

    auto target = args[0].ensureValue();
    if (!target.isVector()) {
        throw InterpreterError("vector-copy!: first argument must be a vector");
    }

    auto at = args[1].ensureValue();
    if (!at.isNumber()) {
        throw InterpreterError("vector-copy!: second argument must be a number");
    }
    int targetStart = at.asNumber().toInt();

    auto source = args[2].ensureValue();
    if (!source.isVector()) {
        throw InterpreterError("vector-copy!: third argument must be a vector");
    }

    auto& targetVector = std::get<std::vector<SchemeValue>>(target.value);
    const auto& sourceVector = std::get<std::vector<SchemeValue>>(source.value);

    int sourceStart = 0;
    int sourceEnd = sourceVector.size();

    if (args.size() >= 4) {
        auto startArg = args[3].ensureValue();
        if (!startArg.isNumber()) {
            throw InterpreterError("vector-copy!: source start index must be a number");
        }
        sourceStart = startArg.asNumber().toInt();
    }

    if (args.size() >= 5) {
        auto endArg = args[4].ensureValue();
        if (!endArg.isNumber()) {
            throw InterpreterError("vector-copy!: source end index must be a number");
        }
        sourceEnd = endArg.asNumber().toInt();
    }

    if (sourceStart < 0 || static_cast<size_t>(sourceStart) > sourceVector.size() || sourceEnd < sourceStart || static_cast<size_t>(sourceEnd) > sourceVector.size()) {
        throw InterpreterError("vector-copy!: source indices out of bounds");
    }

    if (targetStart < 0 || static_cast<size_t>(targetStart + (sourceEnd - sourceStart)) > targetVector.size()) {
        throw InterpreterError("vector-copy!: target index out of bounds");
    }

    std::copy(sourceVector.begin() + sourceStart,
        sourceVector.begin() + sourceEnd,
        targetVector.begin() + targetStart);

    return std::nullopt;
}

}
