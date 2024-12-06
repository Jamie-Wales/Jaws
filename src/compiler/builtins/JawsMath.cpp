#include "builtins/JawsMath.h"
#include "Error.h"
#include "Interpreter.h"
#include <optional>
namespace jaws_math {

std::optional<SchemeValue> mult(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.empty())
        return SchemeValue(Number(1));
    SchemeValue result = args[0];
    if (result.isExpr())
        result = expressionToValue(*result.asExpr());

    for (size_t i = 1; i < args.size(); ++i) {
        SchemeValue curr = args[i];
        if (curr.isExpr())
            curr = expressionToValue(*curr.asExpr());
        result = result * curr;
    }
    return result;
}

std::optional<SchemeValue> div(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.empty())
        throw InterpreterError("Cannot call procedure / on empty list", std::nullopt);

    if (args.size() == 1) {
        SchemeValue arg = args[0];
        if (arg.isExpr())
            arg = expressionToValue(*arg.asExpr());
        return SchemeValue(Number(1)) / arg;
    }

    SchemeValue result = args[0];
    if (result.isExpr())
        result = expressionToValue(*result.asExpr());

    for (size_t i = 1; i < args.size(); ++i) {
        SchemeValue curr = args[i];
        if (curr.isExpr())
            curr = expressionToValue(*curr.asExpr());
        auto num = curr.as<Number>();
        if (num.isZero()) {
            throw InterpreterError("Division by zero", std::nullopt);
        }
        result = result / curr;
    }
    return result;
}

std::optional<SchemeValue> lessOrEqual(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() < 2) {
        throw InterpreterError("'<=' requires at least 2 arguments");
    }

    for (size_t i = 0; i < args.size() - 1; i++) {
        SchemeValue curr = args[i], next = args[i + 1];

        if (curr.isExpr())
            curr = expressionToValue(*curr.asExpr());
        if (next.isExpr())
            next = expressionToValue(*next.asExpr());

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

std::optional<SchemeValue> greaterOrEqual(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() < 2) {
        throw InterpreterError("'>=' requires at least 2 arguments");
    }

    for (size_t i = 0; i < args.size() - 1; i++) {
        SchemeValue curr = args[i], next = args[i + 1];

        if (curr.isExpr())
            curr = expressionToValue(*curr.asExpr());
        if (next.isExpr())
            next = expressionToValue(*next.asExpr());

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

std::optional<SchemeValue> plus(Interpreter& interp, const std::vector<SchemeValue>& args)
{
    if (args.empty())
        return SchemeValue(Number(0));
    SchemeValue result = args[0];
    if (result.isExpr())
        result = expressionToValue(*result.asExpr());

    for (size_t i = 1; i < args.size(); ++i) {
        SchemeValue curr = args[i];
        if (curr.isExpr())
            curr = expressionToValue(*curr.asExpr());
        if (curr.isProc())
            curr = *interp.executeProcedure(curr, args);
        result = curr + result;
    }

    return result;
}

std::optional<SchemeValue> equal(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() < 2)
        throw InterpreterError("= requires at least two arguments", std::nullopt);

    SchemeValue first = args[0];
    if (first.isExpr())
        first = expressionToValue(*first.asExpr());

    for (size_t i = 1; i < args.size(); ++i) {
        SchemeValue curr = args[i];
        if (curr.isExpr())
            curr = expressionToValue(*curr.asExpr());
        if (!(first == curr)) {
            return SchemeValue(false);
        }
    }
    return SchemeValue(true);
}

std::optional<SchemeValue> minus(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.empty())
        throw InterpreterError("Cannot call procedure - on empty list", std::nullopt);

    if (args.size() == 1) {
        SchemeValue arg = args[0];
        if (arg.isExpr())
            arg = expressionToValue(*arg.asExpr());
        return -arg;
    }

    SchemeValue result = args[0];
    if (result.isExpr())
        result = expressionToValue(*result.asExpr());

    for (size_t i = 1; i < args.size(); ++i) {
        SchemeValue curr = args[i];
        if (curr.isExpr())
            curr = expressionToValue(*curr.asExpr());
        result = result - curr;
    }
    return result;
}
std::optional<SchemeValue> greater(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() < 2)
        throw InterpreterError("> requires at least two arguments", std::nullopt);

    for (size_t i = 0; i < args.size() - 1; ++i) {
        SchemeValue curr = args[i], next = args[i + 1];
        if (curr.isExpr())
            curr = expressionToValue(*curr.asExpr());
        if (next.isExpr())
            next = expressionToValue(*next.asExpr());

        auto comparison = curr <=> next;
        if (comparison == std::partial_ordering::unordered) {
            throw InterpreterError("Cannot compare these values", std::nullopt);
        }
        if (!(comparison > 0)) {
            return SchemeValue(false);
        }
    }
    return SchemeValue(true);
}
std::optional<SchemeValue> less(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() < 2)
        throw InterpreterError("< requires at least two arguments", std::nullopt);

    for (size_t i = 0; i < args.size() - 1; ++i) {
        SchemeValue curr = args[i], next = args[i + 1];
        if (curr.isExpr())
            curr = expressionToValue(*curr.asExpr());
        if (next.isExpr())
            next = expressionToValue(*next.asExpr());

        auto comparison = curr <=> next;
        if (comparison == std::partial_ordering::unordered) {
            throw InterpreterError("Cannot compare these values", std::nullopt);
        }
        if (!(comparison < 0)) {
            return SchemeValue(false);
        }
    }
    return SchemeValue(true);
}
}
