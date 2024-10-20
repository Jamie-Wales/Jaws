#include "Error.h"
#include "Interpreter.h"
#include <format>
#include <optional>
#include <stdexcept>

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
    return SchemeValue(std::holds_alternative<bool>(args[0].getValue()));
}
