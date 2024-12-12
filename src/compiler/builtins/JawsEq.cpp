#include "builtins/JawsEq.h"
#include "Error.h"
#include "Interpreter.h"
namespace jaws_eq {

std::optional<SchemeValue> less(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() < 2)
        throw InterpreterError("< requires at least two arguments", std::nullopt);

    for (size_t i = 0; i < args.size() - 1; ++i) {
        SchemeValue curr = args[i].ensureValue(), next = args[i + 1].ensureValue();
        auto comparison = curr <=> next;
        if (comparison == std::partial_ordering::unordered) {
            throw InterpreterError("Cannot compare these values", std::nullopt);
        }
        if (!(comparison < 0)) {
            return SchemeValue(false);
        }
    }
    return SchemeValue(true);
}

std::optional<SchemeValue> greater(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() < 2)
        throw InterpreterError("> requires at least two arguments", std::nullopt);

    for (size_t i = 0; i < args.size() - 1; ++i) {
        SchemeValue curr = args[i].ensureValue(), next = args[i + 1].ensureValue();
        auto comparison = curr <=> next;
        if (comparison == std::partial_ordering::unordered) {
            throw InterpreterError("Cannot compare these values", std::nullopt);
        }
        if (!(comparison > 0)) {
            return SchemeValue(false);
        }
    }
    return SchemeValue(true);
}

std::optional<SchemeValue> equal(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() < 2)
        throw InterpreterError("= requires at least two arguments", std::nullopt);

    SchemeValue first = args[0].ensureValue();

    for (size_t i = 1; i < args.size(); ++i) {
        SchemeValue curr = args[i].ensureValue();
        if (!(first == curr)) {
            return SchemeValue(false);
        }
    }
    return SchemeValue(true);
}

std::optional<SchemeValue> isBooleanProc(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() != 1)
        throw InterpreterError("Cannot call boolean on multiple arguments", std::nullopt);
    SchemeValue arg = args[0].ensureValue();
    if (std::holds_alternative<bool>(arg.value)) {
        return SchemeValue(std::get<bool>(arg.value));
    }
    throw InterpreterError("Arg is not a bool", std::nullopt);
}
std::optional<SchemeValue> isProcedure(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() != 1)
        throw std::runtime_error("procedure?: expected 1 argument");
    return SchemeValue(args[0].isProc());
}

std::optional<SchemeValue> isPair(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() != 1)
        throw std::runtime_error("pair?: expected 1 argument");
    return SchemeValue(args[0].isList() && !args[0].asList().empty());
}

std::optional<SchemeValue> isList(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() != 1)
        throw std::runtime_error("list?: expected 1 argument");
    return SchemeValue(args[0].isList());
}

std::optional<SchemeValue> isVector(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() != 1)
        throw std::runtime_error("vector?: expected 1 argument");
    return SchemeValue(std::holds_alternative<std::vector<SchemeValue>>(args[0].value));
}

std::optional<SchemeValue> isSymbol(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() != 1)
        throw std::runtime_error("symbol?: expected 1 argument");
    return SchemeValue(args[0].isSymbol());
}

std::optional<SchemeValue> isNumber(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() != 1)
        throw std::runtime_error("number?: expected 1 argument");
    return SchemeValue(args[0].isNumber());
}

std::optional<SchemeValue> isString(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() != 1)
        throw std::runtime_error("string?: expected 1 argument");
    return SchemeValue(std::holds_alternative<std::string>(args[0].value));
}

std::optional<SchemeValue> isPort(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() != 1)
        throw std::runtime_error("port?: expected 1 argument");
    return SchemeValue(args[0].isPort());
}

std::optional<SchemeValue> isNull(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() != 1)
        throw std::runtime_error("null?: expected 1 argument");
    return SchemeValue(args[0].isList() && args[0].asList().empty());
}

std::optional<SchemeValue> isEq(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() != 2)
        throw std::runtime_error("eq?: expected 2 arguments");
    return SchemeValue(&args[0] == &args[1]);
}

std::optional<SchemeValue> isEqv(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() != 2)
        throw std::runtime_error("eqv?: expected 2 arguments");
    return SchemeValue(args[0] == args[1]);
}
}
