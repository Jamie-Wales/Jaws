
#include "Error.h"
#include "Interpreter.h"
#include "Number.h"
#include <optional>
namespace jaws_list {

std::optional<SchemeValue> listProcedure(Interpreter& interp, const std::vector<SchemeValue>& args)
{
    std::vector<SchemeValue> evaluated;
    evaluated.reserve(args.size());
    for (const auto& arg : args) {
        if (arg.isExpr()) {
            evaluated.push_back(expressionToValue(*arg.asExpr()));
        } else {
            evaluated.push_back(arg);
        }
    }
    return SchemeValue(std::list<SchemeValue>(evaluated.begin(), evaluated.end()));
}

std::optional<SchemeValue> carProcudure(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("CAR expects 1 argument");
    }
    SchemeValue arg = args[0];
    if (arg.isExpr())
        arg = expressionToValue(*arg.asExpr());
    auto elements = arg.as<std::list<SchemeValue>>();
    if (elements.size() == 0) {
        throw InterpreterError("CAR invoked on empty list");
    }
    return elements.front();
}

std::optional<SchemeValue> cdrProcedure(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("cdr requires 1 argument");
    }
    SchemeValue arg = args[0];
    if (arg.isExpr())
        arg = expressionToValue(*arg.asExpr());
    if (!arg.isList()) {
        throw InterpreterError("cdr requires 1 argument");
    }
    const auto& list = arg.asList();
    if (list.empty()) {
        throw InterpreterError("car: empty list");
    }
    auto cdr = arg.asList();
    auto it = cdr.begin();
    ++it;
    return SchemeValue(std::list<SchemeValue>(it, cdr.end()));
}

std::optional<SchemeValue> cadrProcedure(Interpreter& ele, const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("cadr requires exactly 1 argument");
    }
    SchemeValue arg = args[0];
    if (arg.isExpr())
        arg = expressionToValue(*arg.asExpr());
    if (!arg.isList()) {
        throw InterpreterError("cadr: argument must be a list");
    }
    const auto& list = arg.asList();
    auto it = std::next(list.begin());
    if (it == list.end()) {
        throw InterpreterError("cadr: list too short");
    }
    return *it;
}

std::optional<SchemeValue> openInputFile(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("OPEN-INPUT-FILE requires exactly 1 argument");
    }
    SchemeValue arg = args[0];
    if (arg.isExpr())
        arg = expressionToValue(*arg.asExpr());
    const auto* filename = std::get_if<std::string>(&arg.value);
    if (!filename) {
        throw InterpreterError("OPEN-INPUT-FILE argument must be a string");
    }
    auto file = std::make_shared<std::fstream>();
    file->open(*filename, std::ios::in);
    if (!file->is_open()) {
        throw InterpreterError("Could not open file: " + *filename);
    }
    return SchemeValue(Port(file, PortType::Input));
}

std::optional<SchemeValue> map(Interpreter& interp, const std::vector<SchemeValue>& args)
{
    if (args.size() < 3) {
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
        auto procResult = interp.executeProcedure(proc, args);
        if (!procResult) {
            throw InterpreterError("MAP procedure must return a value");
        }
        result.push_back(*procResult);
    }
    return SchemeValue(result);
}

std::optional<SchemeValue> cons(Interpreter& interp, const std::vector<SchemeValue>& args)
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
        auto result = interp.executeProcedure(second,
            std::vector<SchemeValue> { args.begin() + 2, args.end() });
        if (result) {
            second = *result;
        }
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
std::optional<SchemeValue> length(Interpreter&, const std::vector<SchemeValue>& args)
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

std::optional<SchemeValue> append(Interpreter&, const std::vector<SchemeValue>& args)
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

std::optional<SchemeValue> reverse(Interpreter&, const std::vector<SchemeValue>& args)
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

std::optional<SchemeValue> listRef(Interpreter&, const std::vector<SchemeValue>& args)
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

std::optional<SchemeValue> listTail(Interpreter&, const std::vector<SchemeValue>& args)
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
}
