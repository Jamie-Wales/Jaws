// JawsValues.cpp
#include "builtins/jaws_values.h"
#include "Error.h"
#include "interpret.h"

namespace jaws_values {
std::optional<SchemeValue> symbolToString(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("symbol->string requires exactly 1 argument");
    }

    const SchemeValue& value = args[0].ensureValue();

    if (!args[0].isSymbol()) {
        throw InterpreterError("symbol->string argument must be a symbol");
    }

    return SchemeValue(args[0].asSymbol());
}
std::optional<SchemeValue> schemeToString(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("scheme->string requires exactly 1 argument");
    }

    return SchemeValue(args[0].toString());
}

std::optional<SchemeValue> valuesToList(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    std::list<SchemeValue> result;
    if (!args.empty() && !args.back().isList()) {
        for (const auto& arg : args) {
            result.push_back(arg);
        }
    } else if (args.size() >= 2) {
        for (size_t i = 0; i < args.size() - 1; i++) {
            result.push_back(args[i]);
        }
        const auto& rest = args.back().asList();
        result.insert(result.end(), rest.begin(), rest.end());
    }

    return SchemeValue(std::move(result));
}

std::optional<SchemeValue> valuesToVector(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    std::vector<SchemeValue> result;
    if (!args.empty() && !args.back().isList()) {
        result = args;
    } else if (args.size() >= 2) {
        result.reserve(args.size() - 1 + args.back().asList().size());
        for (size_t i = 0; i < args.size() - 1; i++) {
            result.push_back(args[i]);
        }
        const auto& rest = args.back().asList();
        result.insert(result.end(), rest.begin(), rest.end());
    }

    return SchemeValue(std::move(result));
}

std::optional<SchemeValue> listToVector(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("list->vector requires exactly 1 argument");
    }

    if (!args[0].isList()) {
        throw InterpreterError("list->vector argument must be a list");
    }

    const auto& list = args[0].asList();
    std::vector<SchemeValue> result(list.begin(), list.end());
    return SchemeValue(std::move(result));
}

std::optional<SchemeValue> vectorToList(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("vector->list requires exactly 1 argument");
    }

    if (!args[0].isVector()) {
        throw InterpreterError("vector->list argument must be a vector");
    }

    const auto& vec = std::get<std::vector<SchemeValue>>(args[0].value);
    std::list<SchemeValue> result(vec.begin(), vec.end());
    return SchemeValue(std::move(result));
}

}
