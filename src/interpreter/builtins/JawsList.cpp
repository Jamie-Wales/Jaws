#include "builtins/JawsList.h"
#include "Error.h"
#include "Number.h"
#include "Value.h"
#include "interpret.h"
#include <iterator> // For std::next
#include <list>
#include <memory>
#include <optional>
#include <vector>

namespace jaws_list {

std::optional<SchemeValue> listProcedure(
    interpret::InterpreterState&,
    const std::vector<SchemeValue>& args)
{
    auto result_ptr = std::make_shared<std::list<SchemeValue>>();
    for (const auto& arg : args) {
        result_ptr->push_back(arg.ensureValue());
    }
    return SchemeValue(result_ptr);
}

std::optional<SchemeValue> carProcudure(
    interpret::InterpreterState&,
    const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("car: requires exactly 1 argument");
    }

    auto val_sv = args[0].ensureValue();
    if (!val_sv.isList()) {
        throw InterpreterError("car: argument must be a list-like structure");
    }
    auto list_ptr = val_sv.ensureValue().asList();
    if (!list_ptr) {
        throw InterpreterError("car: operation on null list");
    }
    if (list_ptr->empty()) {
        throw InterpreterError("car: argument must be a non-empty list");
    }

    return list_ptr->front();
}

std::optional<SchemeValue> cdrProcedure(
    interpret::InterpreterState&,
    const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("cdr: requires exactly 1 argument");
    }
    auto val_sv = args[0].ensureValue();
    if (!val_sv.isList()) {
        throw InterpreterError("cdr: argument must be a list-like structure");
    }
    auto list_ptr = val_sv.asList();
    if (!list_ptr) {
        throw InterpreterError("cdr: operation on null list");
    }
    if (list_ptr->empty()) {
        throw InterpreterError("cdr: argument cannot be empty list");
    }

    auto tail_ptr = std::make_shared<std::list<SchemeValue>>(std::next(list_ptr->begin()), list_ptr->end());
    return SchemeValue(tail_ptr);
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

    auto result_ptr = std::make_shared<std::list<SchemeValue>>();

    if (second.isList()) {
        auto second_list_ptr = second.asList();
        if (!second_list_ptr) {
            throw InterpreterError("cons: operation on null list for second argument");
        }
        *result_ptr = *second_list_ptr; // Copy contents
        result_ptr->push_front(first);
    } else {
        // Represents improper list (first . second) using list convention
        result_ptr->push_back(first);
        result_ptr->push_back(second);
    }

    return SchemeValue(result_ptr);
}

std::optional<SchemeValue> length(
    interpret::InterpreterState&,
    const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("length: requires exactly 1 argument");
    }

    auto val_sv = args[0].ensureValue();
    if (!val_sv.isList()) {
        throw InterpreterError("length: argument must be a list");
    }
    auto list_ptr = val_sv.asList();
    if (!list_ptr) {
        throw InterpreterError("length: operation on null list");
    }

    return SchemeValue(Number(static_cast<int>(list_ptr->size())));
}

std::optional<SchemeValue> append(
    interpret::InterpreterState&,
    const std::vector<SchemeValue>& args)
{
    auto result_ptr = std::make_shared<std::list<SchemeValue>>();

    for (const auto& arg : args) {
        auto val_sv = arg.ensureValue();
        if (!val_sv.isList()) {
            throw InterpreterError("append: all arguments must be lists");
        }
        auto list_ptr = val_sv.asList();
        if (list_ptr) { // Only append if list is not null
            result_ptr->insert(result_ptr->end(), list_ptr->begin(), list_ptr->end());
        }
    }

    return SchemeValue(result_ptr);
}

std::optional<SchemeValue> listRef(
    interpret::InterpreterState&,
    const std::vector<SchemeValue>& args)
{
    if (args.size() != 2) {
        throw InterpreterError("list-ref: requires exactly 2 arguments");
    }

    auto list_sv = args[0].ensureValue();
    if (!list_sv.isList()) {
        throw InterpreterError("list-ref: first argument must be a list");
    }
    auto list_ptr = list_sv.asList();
    if (!list_ptr) {
        throw InterpreterError("list-ref: operation on null list");
    }

    auto index_sv = args[1].ensureValue();
    if (!index_sv.isNumber()) {
        throw InterpreterError("list-ref: second argument must be a number");
    }

    int idx = index_sv.asNumber().toInt();

    if (idx < 0 || static_cast<size_t>(idx) >= list_ptr->size()) {
        throw InterpreterError("list-ref: index out of bounds");
    }

    auto it = list_ptr->begin();
    std::advance(it, idx);
    return *it;
}

std::optional<SchemeValue> listSet(
    interpret::InterpreterState&,
    const std::vector<SchemeValue>& args) // Assuming const args okay due to shared_ptr
{
    if (args.size() != 3) {
        throw InterpreterError("list-set!: requires exactly 3 arguments");
    }

    auto list_sv = args[0].ensureValue();
    if (!list_sv.isList()) {
        throw InterpreterError("list-set!: first argument must be a list");
    }
    auto list_ptr = list_sv.asList();
    if (!list_ptr) {
        throw InterpreterError("list-set!: operation on null list");
    }

    auto index_sv = args[1].ensureValue();
    if (!index_sv.isNumber()) {
        throw InterpreterError("list-set!: second argument must be a number");
    }

    int idx = index_sv.asNumber().toInt();

    if (idx < 0 || static_cast<size_t>(idx) >= list_ptr->size()) {
        throw InterpreterError("list-set!: index out of bounds");
    }

    auto new_element = args[2].ensureValue();

    auto it = list_ptr->begin();
    std::advance(it, idx);
    *it = new_element; // Modify element within the shared list

    return std::nullopt;
}

std::optional<SchemeValue> member(
    interpret::InterpreterState&,
    const std::vector<SchemeValue>& args)
{
    if (args.size() != 2) {
        throw InterpreterError("member: requires exactly 2 arguments");
    }

    const auto& item = args[0].ensureValue(); // Item to find
    auto list_sv = args[1].ensureValue(); // List to search in
    if (!list_sv.isList()) {
        throw InterpreterError("member: second argument must be a list");
    }
    auto list_ptr = list_sv.asList();
    if (!list_ptr) {
        return SchemeValue(false);
    }

    auto it = list_ptr->begin();
    while (it != list_ptr->end()) {
        if (*it == item) {
            auto tail_ptr = std::make_shared<std::list<SchemeValue>>(it, list_ptr->end());
            return SchemeValue(tail_ptr);
        }
        ++it;
    }

    return SchemeValue(false);
}

std::optional<SchemeValue> assq(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    if (args.size() != 2) {
        throw InterpreterError("assq: requires exactly 2 arguments");
    }
    const auto& key = args[0].ensureValue();
    auto list_sv = args[1].ensureValue(); // The association list
    if (!list_sv.isList()) {
        throw InterpreterError("assq: second argument must be an association list");
    }
    auto list_ptr = list_sv.asList();
    if (!list_ptr) {
        // Assq on null list is false
        return SchemeValue(false);
    }

    for (const auto& pair_sv : *list_ptr) {
        if (!pair_sv.isList()) {
            continue;
        }
        auto inner_list_ptr = pair_sv.asList();
        if (!inner_list_ptr || inner_list_ptr->empty()) {
            continue;
        }

        const auto& current_key = inner_list_ptr->front();

        if (current_key == key) {
            return pair_sv;
        }
    }
    return SchemeValue(false);
}

} // namespace jaws_list
