#include "builtins/jaws_string.h"
#include "Error.h"
#include "Number.h"
#include <algorithm>
#include <cctype>
#include <string>

namespace jaws_string {

namespace {
    std::string getString(const SchemeValue& value, const char* funcName)
    {
        value.ensureValue();
        if (!std::holds_alternative<std::string>(value.value)) {
            throw InterpreterError(std::string(funcName) + " requires string arguments");
        }
        return std::get<std::string>(value.value);
    }

    int compareIgnoreCase(const std::string& a, const std::string& b)
    {
        std::string lowerA, lowerB;
        lowerA.resize(a.size());
        lowerB.resize(b.size());

        std::transform(a.begin(), a.end(), lowerA.begin(),
            [](unsigned char c) { return std::tolower(c); });
        std::transform(b.begin(), b.end(), lowerB.begin(),
            [](unsigned char c) { return std::tolower(c); });

        return lowerA.compare(lowerB);
    }
}

std::optional<SchemeValue> numberToString(
    interpret::InterpreterState&,
    const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("number->string requires exactly 1 argument");
    }

    if (!args[0].ensureValue().isNumber()) {
        throw InterpreterError("number->string: argument must be a number");
    }

    return SchemeValue(args[0].asNumber().toString());
}

std::optional<SchemeValue> stringEqual(
    interpret::InterpreterState&,
    const std::vector<SchemeValue>& args)
{
    if (args.size() < 2) {
        throw InterpreterError("string=? requires at least 2 arguments");
    }

    std::string first = getString(args[0], "string=?");

    for (size_t i = 1; i < args.size(); i++) {
        std::string current = getString(args[i], "string=?");
        if (first != current) {
            return SchemeValue(false);
        }
    }

    return SchemeValue(true);
}

std::optional<SchemeValue> stringLess(
    interpret::InterpreterState&,
    const std::vector<SchemeValue>& args)
{
    if (args.size() < 2) {
        throw InterpreterError("string<? requires at least 2 arguments");
    }

    for (size_t i = 0; i < args.size() - 1; i++) {
        std::string current = getString(args[i], "string<?");
        std::string next = getString(args[i + 1], "string<?");

        if (current >= next) {
            return SchemeValue(false);
        }
    }

    return SchemeValue(true);
}

std::optional<SchemeValue> stringGreater(
    interpret::InterpreterState&,
    const std::vector<SchemeValue>& args)
{
    if (args.size() < 2) {
        throw InterpreterError("string>? requires at least 2 arguments");
    }

    for (size_t i = 0; i < args.size() - 1; i++) {
        std::string current = getString(args[i], "string>?");
        std::string next = getString(args[i + 1], "string>?");

        if (current <= next) {
            return SchemeValue(false);
        }
    }

    return SchemeValue(true);
}

std::optional<SchemeValue> stringCiEqual(
    interpret::InterpreterState&,
    const std::vector<SchemeValue>& args)
{
    if (args.size() < 2) {
        throw InterpreterError("string-ci=? requires at least 2 arguments");
    }

    std::string first = getString(args[0], "string-ci=?");

    for (size_t i = 1; i < args.size(); i++) {
        std::string current = getString(args[i], "string-ci=?");
        if (compareIgnoreCase(first, current) != 0) {
            return SchemeValue(false);
        }
    }

    return SchemeValue(true);
}

std::optional<SchemeValue> stringLength(
    interpret::InterpreterState&,
    const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("string-length requires exactly 1 argument");
    }

    std::string str = getString(args[0], "string-length");
    return SchemeValue(Number(static_cast<int>(str.length())));
}

std::optional<SchemeValue> stringAppend(
    interpret::InterpreterState&,
    const std::vector<SchemeValue>& args)
{
    if (args.empty()) {
        return SchemeValue(std::string(""));
    }

    std::string result;
    for (const auto& arg : args) {
        result += getString(arg, "string-append");
    }

    return SchemeValue(result);
}

std::optional<SchemeValue> substring(
    interpret::InterpreterState&,
    const std::vector<SchemeValue>& args)
{
    if (args.size() < 2 || args.size() > 3) {
        throw InterpreterError("substring requires 2 or 3 arguments");
    }

    std::string str = getString(args[0], "substring");

    if (!args[1].isNumber()) {
        throw InterpreterError("substring: second argument must be a number");
    }
    int start = args[1].asNumber().toInt();

    if (start < 0 || static_cast<size_t>(start) > str.length()) {
        throw InterpreterError("substring: start index out of bounds");
    }

    int end;
    if (args.size() == 3) {
        if (!args[2].isNumber()) {
            throw InterpreterError("substring: third argument must be a number");
        }
        end = args[2].asNumber().toInt();

        if (end < start || static_cast<size_t>(end) > str.length()) {
            throw InterpreterError("substring: end index out of bounds");
        }
    } else {
        end = static_cast<int>(str.length());
    }

    return SchemeValue(str.substr(start, end - start));
}

std::optional<SchemeValue> stringRef(
    interpret::InterpreterState&,
    const std::vector<SchemeValue>& args)
{
    if (args.size() != 2) {
        throw InterpreterError("string-ref requires exactly 2 arguments");
    }

    std::string str = getString(args[0], "string-ref");

    if (!args[1].isNumber()) {
        throw InterpreterError("string-ref: second argument must be a number");
    }
    int index = args[1].asNumber().toInt();

    if (index < 0 || static_cast<size_t>(index) >= str.length()) {
        throw InterpreterError("string-ref: index out of bounds");
    }

    return SchemeValue(str[index]);
}

std::optional<SchemeValue> stringToList(
    interpret::InterpreterState&,
    const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("string->list requires exactly 1 argument");
    }

    std::string str = getString(args[0], "string->list");
    std::list<SchemeValue> result;

    for (char c : str) {
        result.push_back(SchemeValue(c));
    }

    return SchemeValue(std::make_shared<std::list<SchemeValue>>(std::move(result)));
}

std::optional<SchemeValue> listToString(
    interpret::InterpreterState&,
    const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("list->string requires exactly 1 argument");
    }

    if (!args[0].isList()) {
        throw InterpreterError("list->string: argument must be a list");
    }

    const auto& lst = args[0].asList();
    std::string result;

    for (const auto& item : *lst) {
        if (!std::holds_alternative<char>(item.value)) {
            throw InterpreterError("list->string: list must contain only characters");
        }
        result += std::get<char>(item.value);
    }

    return SchemeValue(result);
}

std::optional<SchemeValue> stringCopy(
    interpret::InterpreterState&,
    const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("string-copy requires exactly 1 argument");
    }

    std::string str = getString(args[0], "string-copy");
    return SchemeValue(str);
}

std::optional<SchemeValue> stringUpcase(
    interpret::InterpreterState&,
    const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("string-upcase requires exactly 1 argument");
    }

    std::string str = getString(args[0], "string-upcase");
    std::string result = str;

    std::transform(result.begin(), result.end(), result.begin(),
        [](unsigned char c) { return std::toupper(c); });

    return SchemeValue(result);
}

std::optional<SchemeValue> stringDowncase(
    interpret::InterpreterState&,
    const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("string-downcase requires exactly 1 argument");
    }

    std::string str = getString(args[0], "string-downcase");
    std::string result = str;

    std::transform(result.begin(), result.end(), result.begin(),
        [](unsigned char c) { return std::tolower(c); });

    return SchemeValue(result);
}

} // namespace jaws_string
