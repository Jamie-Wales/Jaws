#include "Error.h"
#include "Interpreter.h"
#include <optional>

SchemeValue Interpreter::plus(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.empty())
        return SchemeValue(Number(0));
    SchemeValue result = args[0];
    for (size_t i = 1; i < args.size(); ++i) {
        result = result + args[i];
    }
    return result;
}

SchemeValue Interpreter::minus(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.empty())
        throw InterpreterError("Cannot call procedure - on empty list", std::nullopt);
    if (args.size() == 1)
        return -args[0];
    SchemeValue result = args[0];
    for (size_t i = 1; i < args.size(); ++i) {
        result = result - args[i];
    }
    return result;
}

SchemeValue Interpreter::isBooleanProc(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() != 1)
        throw InterpreterError("Cannot call boolean on multiple arguments", std::nullopt);
    if (std::holds_alternative<bool>(args[0].value)) {
        return SchemeValue(std::get<bool>(args[0].value));
    }
    throw InterpreterError("Arg is not a bool", std::nullopt);
}

SchemeValue Interpreter::listProcedure(Interpreter&, const std::vector<SchemeValue>& args)
{
    return SchemeValue(args);
}

SchemeValue Interpreter::carProcudure(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("CAR expects 1 argument");
    }

    auto elements = args.back().as<std::vector<SchemeValue>>();

    if (elements.size() == 0) {
        throw InterpreterError("CAR invoked on empty list");
    }

    return elements[0];
}

SchemeValue Interpreter::cdrProcedure(Interpreter&, const std::vector<SchemeValue>& args)
{

    if (args.size() != 1) {
        throw InterpreterError("CDR expects 1 argument a list");
    }

    auto elements = args.back().as<std::vector<SchemeValue>>();
    if (elements.size() < 2) {
        throw InterpreterError("CDR requires a list of atleast 2 elements");
    }
    std::vector<SchemeValue> outputs = {};
    for (int i = 1; i < elements.size(); i++) {
        outputs.emplace_back(elements[i]);
    }

    return SchemeValue(outputs);
}

SchemeValue Interpreter::cadrProcedure(Interpreter& ele, const std::vector<SchemeValue>& args)
{
    SchemeValue val = cdrProcedure(ele, args);
    val = carProcudure(ele, { args });
    return val;
}
SchemeValue Interpreter::mult(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.empty())
        return SchemeValue(Number(1));

    SchemeValue result = args[0];
    for (size_t i = 1; i < args.size(); ++i) {
        result = result * args[i];
    }
    return result;
}

SchemeValue Interpreter::div(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.empty())
        throw InterpreterError("Cannot call procedure / on empty list", std::nullopt);
    if (args.size() == 1)
        return SchemeValue(Number(1)) / args[0];
    SchemeValue result = args[0];
    for (size_t i = 1; i < args.size(); ++i) {
        auto num = args[i].as<Number>();
        if (num.isZero()) {
            throw InterpreterError("Division by zero", std::nullopt);
        }
        result = result / args[i];
    }
    return result;
}

SchemeValue Interpreter::less(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() < 2)
        throw InterpreterError("< requires at least two arguments", std::nullopt);

    for (size_t i = 0; i < args.size() - 1; ++i) {
        auto comparison = args[i] <=> args[i + 1];
        if (comparison == std::partial_ordering::unordered) {
            throw InterpreterError("Cannot compare these values", std::nullopt);
        }
        if (!(comparison < 0)) {
            return SchemeValue(false);
        }
    }
    return SchemeValue(true);
}

SchemeValue Interpreter::greater(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() < 2)
        throw InterpreterError("> requires at least two arguments", std::nullopt);

    for (size_t i = 0; i < args.size() - 1; ++i) {
        auto comparison = args[i] <=> args[i + 1];
        if (comparison == std::partial_ordering::unordered) {
            throw InterpreterError("Cannot compare these values", std::nullopt);
        }
        if (!(comparison > 0)) {
            return SchemeValue(false);
        }
    }
    return SchemeValue(true);
}

SchemeValue Interpreter::equal(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() < 2)
        throw InterpreterError("= requires at least two arguments", std::nullopt);

    for (size_t i = 1; i < args.size(); ++i) {
        if (!(args[0] == args[i])) {
            return SchemeValue(false);
        }
    }
    return SchemeValue(true);
}
