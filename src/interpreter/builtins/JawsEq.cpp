#include "builtins/JawsEq.h"
#include "Error.h"
#include "interpret.h"
namespace jaws_eq {

std::optional<SchemeValue> less(
    interpret::InterpreterState&,
    const std::vector<SchemeValue>& args)
{
    if (args.size() < 2) {
        throw InterpreterError("< requires at least two arguments");
    }

    for (size_t i = 0; i < args.size() - 1; ++i) {
        SchemeValue curr = args[i].ensureValue();
        SchemeValue next = args[i + 1].ensureValue();
        auto comparison = curr <=> next;

        if (comparison == std::partial_ordering::unordered) {
            throw InterpreterError("Cannot compare these values");
        }
        if (!(comparison < 0)) {
            return SchemeValue(false);
        }
    }
    return SchemeValue(true);
}

std::optional<SchemeValue> greater(
    interpret::InterpreterState&,
    const std::vector<SchemeValue>& args)
{
    if (args.size() < 2) {
        throw InterpreterError("> requires at least two arguments");
    }

    for (size_t i = 0; i < args.size() - 1; ++i) {
        SchemeValue curr = args[i].ensureValue();
        SchemeValue next = args[i + 1].ensureValue();
        auto comparison = curr <=> next;

        if (comparison == std::partial_ordering::unordered) {
            throw InterpreterError("Cannot compare these values");
        }
        if (!(comparison > 0)) {
            return SchemeValue(false);
        }
    }
    return SchemeValue(true);
}

std::optional<SchemeValue> equal(
    interpret::InterpreterState&,
    const std::vector<SchemeValue>& args)
{
    if (args.size() < 2) {
        throw InterpreterError("= requires at least two arguments");
    }

    SchemeValue first = args[0].ensureValue();
    for (size_t i = 1; i < args.size(); ++i) {
        if (!(first == args[i].ensureValue())) {
            return SchemeValue(false);
        }
    }
    return SchemeValue(true);
}

std::optional<SchemeValue> isProcedure(
    interpret::InterpreterState&,
    const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("procedure?: expected 1 argument");
    }
    return SchemeValue(args[0].isProc());
}

std::optional<SchemeValue> isPair(
    interpret::InterpreterState&,
    const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("pair?: expected 1 argument");
    }
    return SchemeValue(args[0].ensureValue().isList() && !args[0].ensureValue().asList().empty());
}

std::optional<SchemeValue> isList(
    interpret::InterpreterState&,
    const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("list?: expected 1 argument");
    }
    return SchemeValue(args[0].isList());
}

std::optional<SchemeValue> isVector(
    interpret::InterpreterState&,
    const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("vector?: expected 1 argument");
    }
    return SchemeValue(std::holds_alternative<std::vector<SchemeValue>>(args[0].value));
}

std::optional<SchemeValue> isSymbol(
    interpret::InterpreterState&,
    const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("symbol?: expected 1 argument");
    }

    return SchemeValue(args[0].ensureValue().isSymbol());
}

std::optional<SchemeValue> isNumber(
    interpret::InterpreterState&,
    const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("number?: expected 1 argument");
    }
    return SchemeValue(args[0].isNumber());
}

std::optional<SchemeValue> isString(
    interpret::InterpreterState&,
    const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("string?: expected 1 argument");
    }
    return SchemeValue(std::holds_alternative<std::string>(args[0].value));
}

std::optional<SchemeValue> isPort(
    interpret::InterpreterState&,
    const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("port?: expected 1 argument");
    }
    return SchemeValue(args[0].isPort());
}

std::optional<SchemeValue> isNull(
    interpret::InterpreterState&,
    const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("null?: expected 1 argument");
    }
    return SchemeValue(args[0].ensureValue().isList() && args[0].ensureValue().asList().empty());
}

std::optional<SchemeValue> isEq(
    interpret::InterpreterState&,
    const std::vector<SchemeValue>& args)
{
    if (args.size() != 2) {
        throw InterpreterError("eq?: expected 2 arguments");
    }
    auto args0 = args[0].ensureValue();
    auto args1 = args[1].ensureValue();
    return SchemeValue(&args0 == &args1);
}

std::optional<SchemeValue> isEqv(
    interpret::InterpreterState&,
    const std::vector<SchemeValue>& args)
{
    if (args.size() != 2) {
        throw InterpreterError("eqv?: expected 2 arguments");
    }
    return SchemeValue(args[0].ensureValue() == args[1].ensureValue());
}

std::optional<SchemeValue> isBooleanProc(
    interpret::InterpreterState&,
    const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("boolean?: expected 1 argument");
    }
    return SchemeValue(std::holds_alternative<bool>(args[0].value));
}

std::optional<SchemeValue> isChar(interpret::InterpreterState&, const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("char?: expected 1 argument");
    }
    return SchemeValue(std::holds_alternative<char>(args[0].value));
}

} // namespace jaws_eq
