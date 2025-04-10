#include "builtins/JawsVector.h"
#include "Error.h"
#include "Number.h"
#include "Value.h"
#include <algorithm> // For std::fill, std::copy
#include <memory>
#include <optional>
#include <vector>

namespace jaws_vec {

std::optional<SchemeValue> makeVector(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    if (args.size() < 1 || args.size() > 2) {
        throw InterpreterError("make-vector requires 1 or 2 arguments");
    }

    auto size_sv = args[0].ensureValue();
    if (!size_sv.isNumber()) {
        throw InterpreterError("make-vector: first argument must be a number");
    }

    int k = size_sv.asNumber().toInt();
    if (k < 0) {
        throw InterpreterError("make-vector: length must be non-negative");
    }

    SchemeValue fill_val = (args.size() == 2)
        ? args[1].ensureValue()
        : SchemeValue(Number(0));

    auto vector_data_ptr = std::make_shared<std::vector<SchemeValue>>(k, fill_val);
    return SchemeValue(vector_data_ptr);
}

std::optional<SchemeValue> vectorProcedure(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    auto result_ptr = std::make_shared<std::vector<SchemeValue>>();
    result_ptr->reserve(args.size());

    for (const auto& arg : args) {
        result_ptr->push_back(arg.ensureValue());
    }

    return SchemeValue(result_ptr);
}

std::optional<SchemeValue> vectorRef(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    if (args.size() != 2) {
        throw InterpreterError("vector-ref requires exactly 2 arguments");
    }

    auto vec_sv = args[0].ensureValue();
    if (!vec_sv.isVector()) {
        throw InterpreterError("vector-ref: first argument must be a vector");
    }
    auto vec_ptr = vec_sv.asSharedVector();
    if (!vec_ptr) {
        throw InterpreterError("vector-ref: operation on null vector");
    }

    auto index_sv = args[1].ensureValue();
    if (!index_sv.isNumber()) {
        throw InterpreterError("vector-ref: second argument must be a number");
    }

    int idx = index_sv.asNumber().toInt();

    if (idx < 0 || static_cast<size_t>(idx) >= vec_ptr->size()) {
        throw InterpreterError("vector-ref: index out of bounds");
    }

    return (*vec_ptr)[idx];
}

std::optional<SchemeValue> vectorSet(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args) // Assuming const args okay due to shared_ptr
{
    if (args.size() != 3) {
        throw InterpreterError("vector-set! requires exactly 3 arguments");
    }
    auto vec_sv = args[0].ensureValue();
    if (!vec_sv.isVector()) {
        throw InterpreterError("vector-set!: first argument must be a vector");
    }
    auto vec_ptr = vec_sv.asSharedVector();
    if (!vec_ptr) {
        throw InterpreterError("vector-set!: operation on null vector");
    }

    auto index_sv = args[1].ensureValue();
    if (!index_sv.isNumber()) {
        throw InterpreterError("vector-set!: second argument must be a number");
    }
    int idx = index_sv.asNumber().toInt();

    if (idx < 0 || static_cast<size_t>(idx) >= vec_ptr->size()) {
        throw InterpreterError("vector-set!: index out of bounds");
    }

    auto new_element = args[2].ensureValue();
    (*vec_ptr)[idx] = new_element;

    return std::nullopt;
}

std::optional<SchemeValue> vectorFill(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args) // Assuming const args okay due to shared_ptr
{
    if (args.size() != 2) {
        throw InterpreterError("vector-fill! requires exactly 2 arguments");
    }
    auto vec_sv = args[0].ensureValue();
    if (!vec_sv.isVector()) {
        throw InterpreterError("vector-fill!: first argument must be a vector");
    }
    auto vec_ptr = vec_sv.asSharedVector();
    if (!vec_ptr) {
        throw InterpreterError("vector-fill!: operation on null vector");
    }

    auto fill_val = args[1].ensureValue();

    std::fill(vec_ptr->begin(), vec_ptr->end(), fill_val);

    return std::nullopt;
}

std::optional<SchemeValue> vectorLength(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("vector-length requires exactly 1 argument");
    }

    auto vec_sv = args[0].ensureValue();
    if (!vec_sv.isVector()) {
        throw InterpreterError("vector-length: argument must be a vector");
    }
    auto vec_ptr = vec_sv.asSharedVector();
    if (!vec_ptr) {
        throw InterpreterError("vector-length: operation on null vector");
    }

    return SchemeValue(Number(static_cast<int>(vec_ptr->size())));
}

std::optional<SchemeValue> vectorCopy(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    if (args.size() < 1 || args.size() > 3) {
        throw InterpreterError("vector-copy requires 1 to 3 arguments");
    }
    auto source_sv = args[0].ensureValue();
    if (!source_sv.isVector()) {
        throw InterpreterError("vector-copy: first argument must be a vector");
    }
    auto source_ptr = source_sv.asSharedVector();
    if (!source_ptr) {
        throw InterpreterError("vector-copy: operation on null vector");
    }

    int start = 0;
    int end = static_cast<int>(source_ptr->size());

    if (args.size() >= 2) {
        auto startArg_sv = args[1].ensureValue();
        if (!startArg_sv.isNumber()) {
            throw InterpreterError("vector-copy: start index must be a number");
        }
        start = startArg_sv.asNumber().toInt();
        if (start < 0 || static_cast<size_t>(start) > source_ptr->size()) {
            throw InterpreterError("vector-copy: start index out of bounds");
        }
    }

    if (args.size() >= 3) {
        auto endArg_sv = args[2].ensureValue();
        if (!endArg_sv.isNumber()) {
            throw InterpreterError("vector-copy: end index must be a number");
        }
        end = endArg_sv.asNumber().toInt();
        if (end < start || static_cast<size_t>(end) > source_ptr->size()) {
            throw InterpreterError("vector-copy: end index out of bounds");
        }
    }

    auto result_ptr = std::make_shared<std::vector<SchemeValue>>(
        source_ptr->begin() + start,
        source_ptr->begin() + end);
    return SchemeValue(result_ptr);
}

std::optional<SchemeValue> vectorCopyTo(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args) // Assuming const args okay due to shared_ptr
{
    if (args.size() < 3 || args.size() > 5) {
        throw InterpreterError("vector-copy! requires 3 to 5 arguments");
    }

    auto target_sv = args[0].ensureValue();
    if (!target_sv.isVector()) {
        throw InterpreterError("vector-copy!: first argument must be a vector");
    }
    auto target_ptr = target_sv.asSharedVector();
    if (!target_ptr) {
        throw InterpreterError("vector-copy!: operation on null target vector");
    }

    auto at_sv = args[1].ensureValue();
    if (!at_sv.isNumber()) {
        throw InterpreterError("vector-copy!: second argument must be a number");
    }
    int targetStart = at_sv.asNumber().toInt();

    auto source_sv = args[2].ensureValue();
    if (!source_sv.isVector()) {
        throw InterpreterError("vector-copy!: third argument must be a vector");
    }
    auto source_ptr = source_sv.asSharedVector();
    if (!source_ptr) {
        throw InterpreterError("vector-copy!: operation on null source vector");
    }

    int sourceStart = 0;
    int sourceEnd = static_cast<int>(source_ptr->size());

    if (args.size() >= 4) {
        auto startArg_sv = args[3].ensureValue();
        if (!startArg_sv.isNumber()) {
            throw InterpreterError("vector-copy!: source start index must be a number");
        }
        sourceStart = startArg_sv.asNumber().toInt();
    }

    if (args.size() >= 5) {
        auto endArg_sv = args[4].ensureValue();
        if (!endArg_sv.isNumber()) {
            throw InterpreterError("vector-copy!: source end index must be a number");
        }
        sourceEnd = endArg_sv.asNumber().toInt();
    }

    if (sourceStart < 0 || static_cast<size_t>(sourceStart) > source_ptr->size() || sourceEnd < sourceStart || static_cast<size_t>(sourceEnd) > source_ptr->size()) {
        throw InterpreterError("vector-copy!: source indices out of bounds");
    }

    int count = sourceEnd - sourceStart;
    if (targetStart < 0 || static_cast<size_t>(targetStart + count) > target_ptr->size()) {
        throw InterpreterError("vector-copy!: target index out of bounds");
    }

    std::copy(
        source_ptr->begin() + sourceStart,
        source_ptr->begin() + sourceEnd,
        target_ptr->begin() + targetStart);

    return std::nullopt;
}

} // namespace jaws_vec
