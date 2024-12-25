#include "builtins/JawsList.h"
#include "Error.h"
#include "Number.h"
#include "Value.h"
#include "interpret.h"
#include <optional>

namespace jaws_list {

std::optional<SchemeValue> listProcedure(
    interpret::InterpreterState&,
    const std::vector<SchemeValue>& args)
{
    std::list<SchemeValue> result;
    for (const auto& arg : args) {
        result.push_back(arg.ensureValue());
    }
    return SchemeValue(std::move(result));
}

std::optional<SchemeValue> carProcudure(
    interpret::InterpreterState&,
    const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("car: requires exactly 1 argument");
    }

    auto val = args[0].ensureValue();
    if (!val.isList() || val.asList().empty()) {
        throw InterpreterError("car: argument must be a non-empty list");
    }

    return val.asList().front();
}

std::optional<SchemeValue> cdrProcedure(
    interpret::InterpreterState&,
    const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("cdr: requires exactly 1 argument");
    }
    auto val = args[0].ensureValue();
    if (!val.isList()) {
        throw InterpreterError("cdr: argument must be a list");
    }
    auto list = val.asList();
    if (list.empty()) {
        throw InterpreterError("cdr: argument cannot be empty list");
    }
    list.pop_front();
    return SchemeValue(std::move(list));
}

std::optional<SchemeValue> cadrProcedure(
    interpret::InterpreterState&,
    const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("cadr: requires exactly 1 argument");
    }

    auto val = args[0].ensureValue();
    if (!val.isList()) {
        throw InterpreterError("cadr: argument must be a list");
    }

    const auto& list = val.asList();
    auto it = std::next(list.begin());
    if (it == list.end()) {
        throw InterpreterError("cadr: list too short");
    }
    return *it;
}

std::optional<SchemeValue> map(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    if (args.size() < 2) {
        throw InterpreterError("map: requires procedure and at least one list");
    }

    auto proc = args[0].ensureValue();
    if (!proc.isProc()) {
        throw InterpreterError("map: first argument must be a procedure");
    }

    std::vector<std::list<SchemeValue>> lists;
    size_t minLength = SIZE_MAX;

    for (size_t i = 1; i < args.size(); i++) {
        auto val = args[i].ensureValue();
        if (!val.isList()) {
            throw InterpreterError("map: all arguments after procedure must be lists");
        }
        lists.push_back(val.asList());
        minLength = std::min(minLength, lists.back().size());
    }

    std::list<SchemeValue> result;
    std::vector<std::list<SchemeValue>::const_iterator> iters;
    for (const auto& list : lists) {
        iters.push_back(list.begin());
    }

    for (size_t i = 0; i < minLength; i++) {
        std::vector<SchemeValue> procArgs;
        for (auto& it : iters) {
            procArgs.push_back(*it++);
        }

        auto procResult = interpret::executeProcedure(state, proc, procArgs);
        if (!procResult) {
            throw InterpreterError("map: procedure returned no value");
        }
        result.push_back(*procResult);
    }

    return SchemeValue(std::move(result));
}

std::optional<SchemeValue> cons(
    interpret::InterpreterState&,
    const std::vector<SchemeValue>& args)
{
    if (args.size() != 2) {
        throw InterpreterError("cons: requires exactly 2 arguments");
    }

    auto first = args[0].ensureValue();
    auto second = args[1].ensureValue();

    if (second.isList()) {
        auto list = second.asList();
        list.push_front(first);
        return SchemeValue(std::move(list));
    }

    return SchemeValue(std::list<SchemeValue> { first, second });
}

std::optional<SchemeValue> length(
    interpret::InterpreterState&,
    const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("length: requires exactly 1 argument");
    }

    auto val = args[0].ensureValue();
    if (!val.isList()) {
        throw InterpreterError("length: argument must be a list");
    }

    return SchemeValue(Number(static_cast<int>(val.asList().size())));
}

std::optional<SchemeValue> append(
    interpret::InterpreterState&,
    const std::vector<SchemeValue>& args)
{
    std::list<SchemeValue> result;

    for (const auto& arg : args) {
        auto val = arg.ensureValue();
        if (!val.isList()) {
            throw InterpreterError("append: all arguments must be lists");
        }
        const auto& list = val.asList();
        result.insert(result.end(), list.begin(), list.end());
    }

    return SchemeValue(std::move(result));
}

std::optional<SchemeValue> reverse(
    interpret::InterpreterState&,
    const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("reverse: requires exactly 1 argument");
    }

    auto val = args[0].ensureValue();
    if (!val.isList()) {
        throw InterpreterError("reverse: argument must be a list");
    }

    auto list = val.asList();
    return SchemeValue(std::list<SchemeValue>(list.rbegin(), list.rend()));
}

std::optional<SchemeValue> listRef(
    interpret::InterpreterState&,
    const std::vector<SchemeValue>& args)
{
    if (args.size() != 2) {
        throw InterpreterError("list-ref: requires exactly 2 arguments");
    }

    auto list = args[0].ensureValue();
    if (!list.isList()) {
        throw InterpreterError("list-ref: first argument must be a list");
    }

    auto index = args[1].ensureValue();
    if (!index.isNumber()) {
        throw InterpreterError("list-ref: second argument must be a number");
    }

    int idx = index.asNumber().toInt();
    const auto& lst = list.asList();

    if (idx < 0 || static_cast<size_t>(idx) >= lst.size()) {
        throw InterpreterError("list-ref: index out of bounds");
    }

    auto it = lst.begin();
    std::advance(it, idx);
    return *it;
}

std::optional<SchemeValue> listTail(
    interpret::InterpreterState&,
    const std::vector<SchemeValue>& args)
{
    if (args.size() != 2) {
        throw InterpreterError("list-tail: requires exactly 2 arguments");
    }

    auto list = args[0].ensureValue();
    if (!list.isList()) {
        throw InterpreterError("list-tail: first argument must be a list");
    }

    auto index = args[1].ensureValue();
    if (!index.isNumber()) {
        throw InterpreterError("list-tail: second argument must be a number");
    }

    int idx = index.asNumber().toInt();
    const auto& lst = list.asList();

    if (idx < 0 || static_cast<size_t>(idx) >= lst.size()) {
        throw InterpreterError("list-tail: index out of bounds");
    }

    auto it = lst.begin();
    std::advance(it, idx);
    return SchemeValue(std::list<SchemeValue>(it, lst.end()));
}

std::optional<SchemeValue> listSet(
    interpret::InterpreterState&,
    const std::vector<SchemeValue>& args)
{
    if (args.size() != 3) {
        throw InterpreterError("list-set!: requires exactly 3 arguments");
    }

    auto list = args[0].ensureValue();
    if (!list.isList()) {
        throw InterpreterError("list-set!: first argument must be a list");
    }

    auto index = args[1].ensureValue();
    if (!index.isNumber()) {
        throw InterpreterError("list-set!: second argument must be a number");
    }

    int idx = index.asNumber().toInt();
    auto& lst = list.asList();

    if (idx < 0 || static_cast<size_t>(idx) >= lst.size()) {
        throw InterpreterError("list-set!: index out of bounds");
    }

    auto it = lst.begin();
    std::advance(it, idx);
    *it = args[2].ensureValue();

    return std::nullopt;
}

std::optional<SchemeValue> member(
    interpret::InterpreterState&,
    const std::vector<SchemeValue>& args)
{
    if (args.size() != 2) {
        throw InterpreterError("member: requires exactly 2 arguments");
    }

    const auto& item = args[0];
    auto list = args[1].ensureValue();
    if (!list.isList()) {
        throw InterpreterError("member: second argument must be a list");
    }

    const auto& lst = list.asList();
    auto it = lst.begin();
    while (it != lst.end()) {
        if (*it == item) {
            return SchemeValue(std::list<SchemeValue>(it, lst.end()));
        }
        ++it;
    }

    return SchemeValue(false);
}

std::optional<SchemeValue> assq(
    interpret::InterpreterState&,
    const std::vector<SchemeValue>& args)
{
    if (args.size() != 2) {
        throw InterpreterError("assq: requires exactly 2 arguments");
    }
    const auto& key = args[0].ensureValue();
    auto list = args[1].ensureValue();
    if (!list.isList()) {
        throw InterpreterError("assq: second argument must be a list");
    }
    const auto& lst = list.asList();
    for (const auto& pair : lst) {
        if (!pair.isList()) {
            throw InterpreterError("assq: elements must be pairs");
        }
        const auto& pairList = pair.asList();
        if (pairList.empty()) {
            throw InterpreterError("assq: elements must be non-empty pairs");
        }
        if (key.isSymbol() && pairList.front().isSymbol() && key.asSymbol() == pairList.front().asSymbol()) {
            return SchemeValue(pair);
        }
        if (&key == &pairList.front()) {
            return SchemeValue(pair);
        }
    }
    return SchemeValue(false);
}
}
