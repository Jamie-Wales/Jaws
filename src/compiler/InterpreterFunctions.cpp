#include "Interpreter.h"
#include <stdexcept>
#include <format>

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
        throw std::runtime_error("'-' requires at least one argument");
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
        throw std::runtime_error(std::format("boolean? procedure expects one argument, you provided {}", args.size()));
    return SchemeValue(std::holds_alternative<bool>(args[0].getValue()));
}
