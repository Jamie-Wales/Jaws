
#include "Error.h"
#include "Interpreter.h"
#include "Number.h"
#include <optional>

std::optional<SchemeValue> Interpreter::map(Interpreter& interp, const std::vector<SchemeValue>& args)
{
    if (args.size() < 2) {
        throw InterpreterError("MAP requires procedure and arguments");
    }
    if (!args[0].isProc()) {
        throw InterpreterError("MAP requires first argument to be procedure");
    }

    std::list<SchemeValue> result;
    std::vector<std::list<SchemeValue>::const_iterator> iters;
    std::vector<std::list<SchemeValue>::const_iterator> ends;

    size_t length = 0;
    for (size_t i = 1; i < args.size(); i++) {
        if (!args[i].isList()) {
            throw InterpreterError("MAP requires list arguments");
        }
        const auto& list = args[i].asList();
        if (i == 1) {
            length = list.size();
        } else if (list.size() != length) {
            throw InterpreterError("MAP requires lists of equal length");
        }
        iters.push_back(list.begin());
        ends.push_back(list.end());
    }

    while (iters[0] != ends[0]) {
        std::vector<SchemeValue> call_args;
        for (size_t i = 0; i < iters.size(); i++) {
            call_args.push_back(*iters[i]);
            ++iters[i];
        }

        auto procResult = args[0].call(interp, call_args);
        if (!procResult) {
            throw InterpreterError("MAP procedure must return a value");
        }
        result.push_back(*procResult);
    }

    return SchemeValue(result);
}

std::optional<SchemeValue> Interpreter::cons(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() != 2) {
        throw InterpreterError("cons requires exactly 2 arguments");
    }

    if (args[1].isList()) {
        auto list = args[1].asList();
        list.push_front(args[0]);
        return SchemeValue(list);
    }
    std::list<SchemeValue> pair;
    pair.push_back(args[0]);
    pair.push_back(args[1]);
    return SchemeValue(std::move(pair));
}

std::optional<SchemeValue> Interpreter::length(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("length requires exactly 1 argument");
    }
    if (!args[0].isList()) {
        throw InterpreterError("length: argument must be a list");
    }
    return SchemeValue(Number(static_cast<int>(args[0].asList().size())));
}

std::optional<SchemeValue> Interpreter::append(Interpreter&, const std::vector<SchemeValue>& args)
{
    std::vector<SchemeValue> result;
    for (const auto& arg : args) {
        const auto* list = std::get_if<std::list<SchemeValue>>(&arg.value);
        if (!list) {
            throw InterpreterError("APPEND arguments must be lists");
        }
        result.insert(result.end(), list->begin(), list->end());
    }
    return SchemeValue(result);
}

std::optional<SchemeValue> Interpreter::reverse(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("REVERSE requires exactly 1 argument");
    }
    const auto* list = std::get_if<std::vector<SchemeValue>>(&args[0].value);
    if (!list) {
        throw InterpreterError("REVERSE argument must be a list");
    }
    std::vector<SchemeValue> result = *list;
    std::reverse(result.begin(), result.end());
    return SchemeValue(result);
}

std::optional<SchemeValue> Interpreter::listRef(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() != 2) {
        throw InterpreterError("LIST-REF requires exactly 2 arguments");
    }
    const auto* list = std::get_if<std::vector<SchemeValue>>(&args[0].value);
    if (!list) {
        throw InterpreterError("First argument to LIST-REF must be a list");
    }
    if (!args[1].isNumber()) {
        throw InterpreterError("Second argument to LIST-REF must be a number");
    }
    int index = args[1].as<Number>().toInt();
    if (index < 0 || static_cast<size_t>(index) >= list->size()) {
        throw InterpreterError("LIST-REF index out of bounds");
    }
    return (*list)[index];
}

std::optional<SchemeValue> Interpreter::listTail(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() != 2) {
        throw InterpreterError("LIST-TAIL requires exactly 2 arguments");
    }
    const auto* list = std::get_if<std::vector<SchemeValue>>(&args[0].value);
    if (!list) {
        throw InterpreterError("First argument to LIST-TAIL must be a list");
    }
    if (!args[1].isNumber()) {
        throw InterpreterError("Second argument to LIST-TAIL must be a number");
    }
    int index = args[1].as<Number>().toInt();
    if (index < 0 || static_cast<size_t>(index) > list->size()) {
        throw InterpreterError("LIST-TAIL index out of bounds");
    }
    std::vector<SchemeValue> result(list->begin() + index, list->end());
    return SchemeValue(result);
}
