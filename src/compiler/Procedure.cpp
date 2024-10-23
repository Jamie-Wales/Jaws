#include "Procedure.h"
#include "Error.h"
#include "Interpreter.h"
#include "Value.h"

BuiltInProcedure::BuiltInProcedure(Func f)
    : func(std::move(f))
{
}

SchemeValue BuiltInProcedure::operator()(Interpreter& interp,
    const std::vector<SchemeValue>& args) const
{
    return func(interp, args);
}

SchemeValue UserProcedure::operator()(Interpreter& interp,
    const std::vector<SchemeValue>& args) const
{
    if (args.size() != paramNames.size()) {
        throw InterpreterError("Expected " + std::to_string(paramNames.size()) + " arguments but got " + std::to_string(args.size()));
    }

    std::unordered_map<std::string, SchemeValue> oldBindings;
    for (const auto& param : paramNames) {
        auto it = interp.environment.find(param.lexeme);
        if (it != interp.environment.end()) {
            oldBindings[param.lexeme] = it->second;
        }
    }

    for (size_t i = 0; i < args.size(); i++) {
        interp.environment[paramNames[i].lexeme] = args[i];
    }

    auto result = interp.interpret(body);

    for (const auto& [param, value] : oldBindings) {
        interp.environment[param] = value;
    }

    for (const auto& param : paramNames) {
        if (oldBindings.find(param.lexeme) == oldBindings.end()) {
            interp.environment.erase(param.lexeme);
        }
    }

    if (!result) {
        throw InterpreterError("Procedure body returned no value");
    }
    return *result;
}
