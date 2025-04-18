#include "builtins/jaws_values.h"
#include "Error.h"
#include "Value.h"
#include "interpret.h"
#include <list>
#include <memory>
#include <optional>
#include <string>
#include <vector>

namespace jaws_values {

std::optional<SchemeValue> symbolToString(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("symbol->string requires exactly 1 argument");
    }
    const SchemeValue& value = args[0].ensureValue();
    if (!value.isSymbol()) {
        throw InterpreterError("symbol->string argument must be a symbol");
    }
    return SchemeValue(value.asSymbol());
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
    auto result_ptr = std::make_shared<std::list<SchemeValue>>();
    if (args.empty()) {
        return SchemeValue(result_ptr);
    }

    bool lastIsList = args.back().isList();
    size_t limit = lastIsList ? args.size() - 1 : args.size();

    for (size_t i = 0; i < limit; ++i) {
        result_ptr->push_back(args[i].ensureValue());
    }

    if (lastIsList) {
        auto last_list_ptr = args.back().asList();
        if (last_list_ptr) {
            result_ptr->insert(result_ptr->end(), last_list_ptr->begin(), last_list_ptr->end());
        }
    }

    return SchemeValue(result_ptr);
}

std::optional<SchemeValue> valuesToVector(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    auto result_ptr = std::make_shared<std::vector<SchemeValue>>();
    if (args.empty()) {
        return SchemeValue(result_ptr);
    }

    bool lastIsList = args.back().isList();
    size_t limit = lastIsList ? args.size() - 1 : args.size();
    size_t estimated_size = limit;

    if (lastIsList) {
        auto last_list_ptr = args.back().asList();
        if (last_list_ptr)
            estimated_size += last_list_ptr->size();
    }
    result_ptr->reserve(estimated_size);

    for (size_t i = 0; i < limit; ++i) {
        result_ptr->push_back(args[i].ensureValue());
    }

    if (lastIsList) {
        auto last_list_ptr = args.back().asList();
        if (last_list_ptr) {
            result_ptr->insert(result_ptr->end(), last_list_ptr->begin(), last_list_ptr->end());
        }
    }
    return SchemeValue(result_ptr);
}

std::optional<SchemeValue> listToVector(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("list->vector requires exactly 1 argument");
    }
    const auto& list_sv = args[0].ensureValue();
    if (!list_sv.isList()) {
        throw InterpreterError("list->vector argument must be a list");
    }
    auto list_ptr = list_sv.asList();
    if (!list_ptr) {
        throw InterpreterError("list->vector: operation on null list");
    }

    auto result_ptr = std::make_shared<std::vector<SchemeValue>>(list_ptr->begin(), list_ptr->end());
    return SchemeValue(result_ptr);
}

std::optional<SchemeValue> vectorToList(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("vector->list requires exactly 1 argument");
    }
    const auto& vec_sv = args[0].ensureValue();
    if (!vec_sv.isVector()) {
        throw InterpreterError("vector->list argument must be a vector");
    }
    auto vec_ptr = vec_sv.asSharedVector();
    if (!vec_ptr) {
        throw InterpreterError("vector->list: operation on null vector");
    }

    auto result_ptr = std::make_shared<std::list<SchemeValue>>(vec_ptr->begin(), vec_ptr->end());
    return SchemeValue(result_ptr);
}

std::optional<SchemeValue> charToString(interpret::InterpreterState&, const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("char->string requires exactly 1 argument");
    }
    const auto& arg0 = args[0].ensureValue();

    if (arg0.isList()) {
        auto list_ptr = arg0.asList();
        if (!list_ptr) {
            throw InterpreterError("char->string: operation on null list");
        }
        std::string to_return;
        to_return.reserve(list_ptr->size());
        for (const auto& item : *list_ptr) {
            if (!item.isChar()) {
                throw InterpreterError("char->string argument must be a list of characters");
            } else {
                to_return += item.asChar();
            }
        }
        return SchemeValue(to_return);
    }

    if (!arg0.isChar()) {
        throw InterpreterError("char->string argument must be a char or list of char");
    }
    return SchemeValue(std::string(1, arg0.asChar()));
}

std::optional<SchemeValue> stringToSymbol(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("string->symbol requires exactly 1 argument");
    }
    const SchemeValue& value = args[0].ensureValue();
    if (!value.isValue<std::string>()) {
        throw InterpreterError("string->symbol argument must be a string");
    }
    return SchemeValue(Symbol(value.as<std::string>()));
}

std::optional<SchemeValue> stringToNumber(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("string->number requires exactly 1 argument");
    }
    const SchemeValue& value = args[0].ensureValue();
    if (!value.isValue<std::string>()) {
        throw InterpreterError("string->number argument must be a string");
    }
    const std::string& str = value.as<std::string>();
    auto parsed_number = Number::fromString(str);

    if (parsed_number) {
        return SchemeValue(*parsed_number);
    } else {
        return SchemeValue(false); // Return #f on parsing failure
    }
}

std::optional<SchemeValue> charToInteger(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("char->integer requires exactly 1 argument");
    }
    const SchemeValue& value = args[0].ensureValue();
    if (!value.isChar()) {
        throw InterpreterError("char->integer argument must be a character");
    }
    int char_code = static_cast<int>(static_cast<unsigned char>(value.asChar()));
    return SchemeValue(Number(char_code));
}

std::optional<SchemeValue> intToChar(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("integer->char requires exactly 1 argument");
    }
    const SchemeValue& value = args[0].ensureValue();
    if (!value.isNumber()) {
        throw InterpreterError("integer->char argument must be a number");
    }
    Number num = value.asNumber();

    if (!num.isExact() || !num.isInteger()) {
        throw InterpreterError("integer->char requires an exact integer argument");
    }
    try {
        long long int_val = num.toInt();
        if (int_val < std::numeric_limits<unsigned char>::min() || int_val > std::numeric_limits<unsigned char>::max()) {
            throw InterpreterError("integer->char: value out of range for character");
        }

        return SchemeValue(static_cast<char>(int_val));

    } catch (const std::overflow_error& e) {
        throw InterpreterError("integer->char: number is too large to represent as an integer for char conversion");
    } catch (const std::exception& e) {
        throw InterpreterError(std::string("integer->char: error converting number: ") + e.what());
    }
}

std::optional<SchemeValue> stringToVector(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("string->vector requires exactly 1 argument");
    }
    const SchemeValue& value = args[0].ensureValue();
    if (!value.isValue<std::string>()) {
        throw InterpreterError("string->vector argument must be a string");
    }
    const std::string& str = value.as<std::string>();
    auto result_ptr = std::make_shared<std::vector<SchemeValue>>();
    result_ptr->reserve(str.length());
    for (char c : str) {
        result_ptr->push_back(SchemeValue(c));
    }
    return SchemeValue(result_ptr);
}

std::optional<SchemeValue> vectorToString(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("vector->string requires exactly 1 argument");
    }
    const SchemeValue& value = args[0].ensureValue();
    if (!value.isVector()) {
        throw InterpreterError("vector->string argument must be a vector");
    }

    auto vec_ptr = value.asSharedVector();
    if (!vec_ptr) {

        throw InterpreterError("vector->string: operation on null vector");
    }

    std::string result_str;
    result_str.reserve(vec_ptr->size());

    for (const auto& item_sv : *vec_ptr) {
        const auto& item = item_sv.ensureValue();
        if (!item.isChar()) {
            throw InterpreterError("vector->string argument must be a vector of characters");
        }
        result_str += item.asChar();
    }

    return SchemeValue(result_str);
}

} // namespace jaws_values
