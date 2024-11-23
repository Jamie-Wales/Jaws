#include "Error.h"
#include "Interpreter.h"
#include "Number.h"
#include "Procedure.h"
#include <optional>

std::optional<SchemeValue> Interpreter::map(Interpreter& interp, const std::vector<SchemeValue>& args)
{
    if (args.size() < 2) {
        throw InterpreterError("MAP requires procedure and arguments");
    }

    SchemeValue proc = args[0];
    if (proc.isExpr()) {
        proc = expressionToValue(*proc.asExpr());
    }
    if (!proc.isProc()) {
        throw InterpreterError("MAP requires first argument to be procedure");
    }

    std::vector<std::list<SchemeValue>> lists;
    size_t length = 0;

    for (size_t i = 1; i < args.size(); i++) {
        SchemeValue arg = args[i];
        if (arg.isExpr()) {
            arg = expressionToValue(*arg.asExpr());
        }
        if (!arg.isList()) {
            throw InterpreterError("MAP requires list arguments");
        }

        lists.push_back(arg.asList());

        if (i == 1) {
            length = lists.back().size();
        } else if (lists.back().size() != length) {
            throw InterpreterError("MAP requires lists of equal length");
        }
    }

    std::list<SchemeValue> result;

    std::vector<std::list<SchemeValue>::const_iterator> iters;
    for (const auto& list : lists) {
        iters.push_back(list.begin());
    }

    while (!lists.empty() && iters[0] != lists[0].end()) {
        std::vector<SchemeValue> call_args;

        for (size_t i = 0; i < lists.size(); i++) {
            if (iters[i] != lists[i].end()) {
                call_args.push_back(*iters[i]);
                ++iters[i];
            }
        }

        auto procResult = proc.call(interp, call_args);
        if (!procResult) {
            throw InterpreterError("MAP procedure must return a value");
        }
        result.push_back(*procResult);
    }

    return SchemeValue(result);
}

std::optional<SchemeValue> Interpreter::cons(Interpreter& interp, const std::vector<SchemeValue>& args)
{
    if (args.size() != 2) {
        throw InterpreterError("cons requires exactly 2 arguments");
    }

    SchemeValue first = args[0];
    if (first.isExpr()) {
        first = expressionToValue(*first.asExpr());
    }

    SchemeValue second = args[1];
    if (second.isExpr()) {
        second = expressionToValue(*second.asExpr());
    }

    if (second.isProc()) {
        second.call(interp, std::vector<SchemeValue> { args.begin() + 2, args.end() });
    }

    if (second.isList()) {
        auto list = second.asList();
        list.push_front(first);
        return SchemeValue(list);
    }
    std::list<SchemeValue> pair;
    pair.push_back(first);
    pair.push_back(second);
    return SchemeValue(std::move(pair));
}

std::optional<SchemeValue> Interpreter::length(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("length requires exactly 1 argument");
    }

    SchemeValue arg = args[0];
    if (arg.isExpr()) {
        arg = expressionToValue(*arg.asExpr());
    }
    if (!arg.isList()) {
        throw InterpreterError("length: argument must be a list");
    }
    return SchemeValue(Number(static_cast<int>(arg.asList().size())));
}

std::optional<SchemeValue> Interpreter::append(Interpreter&, const std::vector<SchemeValue>& args)
{
    std::vector<SchemeValue> result;
    for (const auto& arg : args) {
        SchemeValue curr = arg;
        if (curr.isExpr()) {
            curr = expressionToValue(*curr.asExpr());
        }
        const auto* list = std::get_if<std::list<SchemeValue>>(&curr.value);
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

    SchemeValue arg = args[0];
    if (arg.isExpr()) {
        arg = expressionToValue(*arg.asExpr());
    }
    const auto* list = std::get_if<std::vector<SchemeValue>>(&arg.value);
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

    SchemeValue list_arg = args[0];
    if (list_arg.isExpr()) {
        list_arg = expressionToValue(*list_arg.asExpr());
    }
    const auto* list = std::get_if<std::vector<SchemeValue>>(&list_arg.value);
    if (!list) {
        throw InterpreterError("First argument to LIST-REF must be a list");
    }

    SchemeValue index_arg = args[1];
    if (index_arg.isExpr()) {
        index_arg = expressionToValue(*index_arg.asExpr());
    }
    if (!index_arg.isNumber()) {
        throw InterpreterError("Second argument to LIST-REF must be a number");
    }
    int index = index_arg.as<Number>().toInt();
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

    SchemeValue list_arg = args[0];
    if (list_arg.isExpr()) {
        list_arg = expressionToValue(*list_arg.asExpr());
    }
    const auto* list = std::get_if<std::vector<SchemeValue>>(&list_arg.value);
    if (!list) {
        throw InterpreterError("First argument to LIST-TAIL must be a list");
    }

    SchemeValue index_arg = args[1];
    if (index_arg.isExpr()) {
        index_arg = expressionToValue(*index_arg.asExpr());
    }
    if (!index_arg.isNumber()) {
        throw InterpreterError("Second argument to LIST-TAIL must be a number");
    }
    int index = index_arg.as<Number>().toInt();
    if (index < 0 || static_cast<size_t>(index) > list->size()) {
        throw InterpreterError("LIST-TAIL index out of bounds");
    }
    std::vector<SchemeValue> result(list->begin() + index, list->end());
    return SchemeValue(result);
}
