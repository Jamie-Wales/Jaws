#include "builtins/JawsMath.h"
#include "Error.h"
#include "interpret.h"

namespace jaws_math {

std::optional<SchemeValue> plus(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    if (args.empty()) {
        return SchemeValue(Number(0));
    }

    SchemeValue result = args[0].ensureValue();
    for (size_t i = 1; i < args.size(); ++i) {
        SchemeValue curr = args[i].ensureValue();
        if (curr.isProc()) {
            curr = *interpret::executeProcedure(state, curr, args);
        }
        result = curr + result;
    }
    return result;
}

std::optional<SchemeValue> minus(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    if (args.empty()) {
        throw InterpreterError("Cannot call procedure - on empty list");
    }

    if (args.size() == 1) {
        return -args[0].ensureValue();
    }

    SchemeValue result = args[0].ensureValue();
    for (size_t i = 1; i < args.size(); ++i) {
        result = result - args[i].ensureValue();
    }
    return result;
}

std::optional<SchemeValue> mult(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    if (args.empty()) {
        return SchemeValue(Number(1));
    }

    SchemeValue result = args[0].ensureValue();
    for (size_t i = 1; i < args.size(); ++i) {
        result = result * args[i].ensureValue();
    }
    return result;
}

std::optional<SchemeValue> div(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    if (args.empty()) {
        throw InterpreterError("Cannot call procedure / on empty list");
    }

    if (args.size() == 1) {
        return SchemeValue(Number(1)) / args[0].ensureValue();
    }

    SchemeValue result = args[0].ensureValue();
    for (size_t i = 1; i < args.size(); ++i) {
        auto curr = args[i].ensureValue();
        if (curr.asNumber().isZero()) {
            throw InterpreterError("Division by zero");
        }
        result = result / curr;
    }
    return result;
}

std::optional<SchemeValue> less(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    if (args.size() < 2) {
        throw InterpreterError("< requires at least two arguments");
    }

    for (size_t i = 0; i < args.size() - 1; ++i) {
        auto curr = args[i].ensureValue();
        auto next = args[i + 1].ensureValue();

        auto comparison = curr <=> next;
        if (comparison == std::partial_ordering::unordered) {
            throw InterpreterError("Cannot compare these values");
        }
        if (!(comparison < 0)) {
            return SchemeValue(false);
        }
    }
    return SchemeValue(true);
}

std::optional<SchemeValue> greater(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    if (args.size() < 2) {
        throw InterpreterError("> requires at least two arguments");
    }

    for (size_t i = 0; i < args.size() - 1; ++i) {
        auto curr = args[i].ensureValue();
        auto next = args[i + 1].ensureValue();

        auto comparison = curr <=> next;
        if (comparison == std::partial_ordering::unordered) {
            throw InterpreterError("Cannot compare these values");
        }
        if (!(comparison > 0)) {
            return SchemeValue(false);
        }
    }
    return SchemeValue(true);
}

std::optional<SchemeValue> lessOrEqual(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    if (args.size() < 2) {
        throw InterpreterError("'<=' requires at least 2 arguments");
    }

    for (size_t i = 0; i < args.size() - 1; i++) {
        auto curr = args[i].ensureValue();
        auto next = args[i + 1].ensureValue();

        if (!curr.isNumber() || !next.isNumber()) {
            throw InterpreterError("Cannot compare non-numeric values with <=");
        }

        auto ordering = curr.asNumber() <=> next.asNumber();
        if (ordering == std::partial_ordering::unordered) {
            throw InterpreterError("Cannot compare these numeric types with <=");
        }
        if (ordering == std::partial_ordering::greater) {
            return SchemeValue(false);
        }
    }
    return SchemeValue(true);
}

std::optional<SchemeValue> greaterOrEqual(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    if (args.size() < 2) {
        throw InterpreterError("'>=' requires at least 2 arguments");
    }

    for (size_t i = 0; i < args.size() - 1; i++) {
        auto curr = args[i].ensureValue();
        auto next = args[i + 1].ensureValue();

        if (!curr.isNumber() || !next.isNumber()) {
            throw InterpreterError("Cannot compare non-numeric values with >=");
        }

        auto ordering = curr.asNumber() <=> next.asNumber();
        if (ordering == std::partial_ordering::unordered) {
            throw InterpreterError("Cannot compare these numeric types with >=");
        }
        if (ordering == std::partial_ordering::less) {
            return SchemeValue(false);
        }
    }
    return SchemeValue(true);
}

} // namespace jaws_math
