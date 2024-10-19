#include "Interpreter.h"
SchemeValue Interpreter::plus(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.empty())
        return SchemeValue(0);
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
        return SchemeValue(-args[0].as<int>());
    SchemeValue result = args[0];
    for (size_t i = 1; i < args.size(); ++i) {
        result = result + SchemeValue(-args[i].as<int>());
    }
    return result;
}

SchemeValue Interpreter::isBooleanProc(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() == 1)
        return SchemeValue(args[0].boolean());
    throw std::runtime_error(std::format("boolean? procedure expects one argument you provided {}", args.size()));
}
