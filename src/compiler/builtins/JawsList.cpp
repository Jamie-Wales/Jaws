#include "builtins/JawsList.h"
#include "Error.h"
#include "Interpreter.h"
#include "Number.h"
#include "Value.h"

namespace jaws_list {

std::optional<SchemeValue> listProcedure(Interpreter&, const std::vector<SchemeValue>& args)
{
    std::list<SchemeValue> result;
    for (const auto& arg : args) {
        result.push_back(ensureSchemeValue(arg));
    }
    return SchemeValue(std::move(result));
}

std::optional<SchemeValue> carProcudure(Interpreter&, const std::vector<SchemeValue>& args)
{
    checkArgCount(args, 1, "car");
    auto val = ensureSchemeValue(args[0]);

    if (!val.isList() || val.asList().empty()) {
        throw InterpreterError("car: empty list");
    }
    return val.asList().front();
}

std::optional<SchemeValue> cdrProcedure(Interpreter&, const std::vector<SchemeValue>& args)
{
    checkArgCount(args, 1, "cdr");
    auto val = ensureSchemeValue(args[0]);

    if (!val.isList() || val.asList().empty()) {
        throw InterpreterError("cdr: empty list");
    }

    auto list = val.asList();
    auto it = list.begin();
    ++it;
    return SchemeValue(std::list<SchemeValue>(it, list.end()));
}

std::optional<SchemeValue> cadrProcedure(Interpreter&, const std::vector<SchemeValue>& args)
{
    checkArgCount(args, 1, "cadr");
    auto val = ensureSchemeValue(args[0]);

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

std::optional<SchemeValue> map(Interpreter& interp, const std::vector<SchemeValue>& args)
{
    if (args.size() < 2) {
        throw InterpreterError("map: requires procedure and at least one list");
    }

    auto proc = ensureSchemeValue(args[0]);
    if (!proc.isProc()) {
        throw InterpreterError("map: first argument must be a procedure");
    }

    // Collect and validate all input lists
    std::vector<std::list<SchemeValue>> lists;
    size_t minLength = SIZE_MAX;

    for (size_t i = 1; i < args.size(); i++) {
        auto val = ensureSchemeValue(args[i]);
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

    // Apply procedure to elements from all lists
    for (size_t i = 0; i < minLength; i++) {
        std::vector<SchemeValue> procArgs;
        for (auto& it : iters) {
            procArgs.push_back(*it++);
        }

        auto procResult = interp.executeProcedure(proc, procArgs);
        if (!procResult) {
            throw InterpreterError("map: procedure must return a value");
        }
        result.push_back(*procResult);
    }

    return SchemeValue(result);
}

std::optional<SchemeValue> cons(Interpreter&, const std::vector<SchemeValue>& args)
{
    checkArgCount(args, 2, "cons");

    auto first = ensureSchemeValue(args[0]);
    auto second = ensureSchemeValue(args[1]);

    if (second.isList()) {
        auto list = second.asList();
        list.push_front(first);
        return SchemeValue(list);
    }

    return SchemeValue(std::list<SchemeValue> { first, second });
}

std::optional<SchemeValue> length(Interpreter&, const std::vector<SchemeValue>& args)
{
    checkArgCount(args, 1, "length");
    auto val = ensureSchemeValue(args[0]);

    if (!val.isList()) {
        throw InterpreterError("length: argument must be a list");
    }
    return SchemeValue(Number(static_cast<int>(val.asList().size())));
}

std::optional<SchemeValue> append(Interpreter&, const std::vector<SchemeValue>& args)
{
    std::list<SchemeValue> result;

    for (const auto& arg : args) {
        auto val = ensureSchemeValue(arg);
        if (!val.isList()) {
            throw InterpreterError("append: all arguments must be lists");
        }
        const auto& list = val.asList();
        result.insert(result.end(), list.begin(), list.end());
    }

    return SchemeValue(std::move(result));
}

std::optional<SchemeValue> reverse(Interpreter&, const std::vector<SchemeValue>& args)
{
    checkArgCount(args, 1, "reverse");
    auto val = ensureSchemeValue(args[0]);

    if (!val.isList()) {
        throw InterpreterError("reverse: argument must be a list");
    }

    auto list = val.asList();
    return SchemeValue(std::list<SchemeValue>(list.rbegin(), list.rend()));
}

std::optional<SchemeValue> listRef(Interpreter&, const std::vector<SchemeValue>& args)
{
    checkArgCount(args, 2, "list-ref");

    auto list = ensureSchemeValue(args[0]);
    if (!list.isList()) {
        throw InterpreterError("list-ref: first argument must be a list");
    }

    auto index = ensureSchemeValue(args[1]);
    if (!index.isNumber()) {
        throw InterpreterError("list-ref: second argument must be a number");
    }

    int idx = index.asNumber().toInt();
    const auto& lst = list.asList();

    if (idx < 0 || static_cast<size_t>(idx) > lst.size() - 1) {
        throw InterpreterError("list-ref: index out of bounds");
    }

    auto it = lst.begin();
    std::advance(it, idx);
    return *it;
}

std::optional<SchemeValue> listTail(Interpreter&, const std::vector<SchemeValue>& args)
{
    checkArgCount(args, 2, "list-tail");

    auto list = ensureSchemeValue(args[0]);
    if (!list.isList()) {
        throw InterpreterError("list-tail: first argument must be a list");
    }

    auto index = ensureSchemeValue(args[1]);
    if (!index.isNumber()) {
        throw InterpreterError("list-tail: second argument must be a number");
    }

    int idx = index.asNumber().toInt();
    const auto& lst = list.asList();

    if (idx < 0 || static_cast<size_t>(idx) > lst.size() - 1) {
        throw InterpreterError("list-tail: index out of bounds");
    }

    auto it = lst.begin();
    std::advance(it, idx);
    return SchemeValue(std::list<SchemeValue>(it, lst.end()));
}

std::optional<SchemeValue> listSet(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() != 3) {
        throw InterpreterError("list-set!: requires exactly 3 arguments");
    }

    auto list = ensureSchemeValue(args[0]);
    if (!list.isList()) {
        throw InterpreterError("list-set!: first argument must be a list");
    }

    auto index = ensureSchemeValue(args[1]);
    if (!index.isNumber()) {
        throw InterpreterError("list-set!: second argument must be a number");
    }

    int idx = index.asNumber().toInt();
    auto& lst = list.asList();

    if (idx < 0 || static_cast<size_t>(idx) > lst.size() - 1) {
        throw InterpreterError("list-set!: index out of bounds");
    }

    auto it = lst.begin();
    std::advance(it, idx);
    *it = ensureSchemeValue(args[2]);

    return std::nullopt;
}

SchemeValue isPair(Interpreter& interp, const std::vector<SchemeValue>& args)
{
    checkArgCount(args, 1, "pair?");
    if (!args[0].isList()) {
        return SchemeValue(false);
    }
    const auto& lst = args[0].asList();
    return SchemeValue(!lst.empty());
}

SchemeValue member(Interpreter& interp, const std::vector<SchemeValue>& args)
{
    checkArgCount(args, 2, "member");
    const auto& item = args[0];
    if (!args[1].isList()) {
        throw InterpreterError("Second argument to member must be a list");
    }

    const auto& lst = args[1].asList();
    auto it = lst.begin();
    while (it != lst.end()) {
        if (*it == item) {
            // Create new list from this point to end
            std::list<SchemeValue> result(it, lst.end());
            return SchemeValue(result);
        }
        ++it;
    }
    return SchemeValue(false);
}

SchemeValue assq(Interpreter& interp, const std::vector<SchemeValue>& args)
{
    checkArgCount(args, 2, "assq");
    const auto& key = args[0];
    if (!args[1].isList()) {
        throw InterpreterError("Second argument to assq must be a list");
    }

    const auto& lst = args[1].asList();
    for (const auto& pair : lst) {
        if (!pair.isList()) {
            continue; // Skip non-pairs
        }
        const auto& pairList = pair.asList();
        if (pairList.empty()) {
            continue;
        }
        if (key.isSymbol() && pairList.front().isSymbol() && key.asSymbol() == pairList.front().asSymbol()) {
            return SchemeValue(pair);
        }
        if (key == pairList.front()) {
            return SchemeValue(pair);
        }
    }
    return SchemeValue(false);
}

}
