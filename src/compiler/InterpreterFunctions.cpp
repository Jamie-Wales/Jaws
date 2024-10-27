#include "Error.h"
#include "Interpreter.h"
#include "Port.h"
#include <optional>

std::optional<SchemeValue> Interpreter::plus(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.empty())
        return SchemeValue(Number(0));
    SchemeValue result = args[0];
    for (size_t i = 1; i < args.size(); ++i) {
        result = result + args[i];
    }
    return result;
}

std::optional<SchemeValue> Interpreter::minus(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.empty())
        throw InterpreterError("Cannot call procedure - on empty list", std::nullopt);
    if (args.size() == 1)
        return -args[0];
    SchemeValue result = args[0];
    for (size_t i = 1; i < args.size(); ++i) {
        result = result - args[i];
    }
    return result;
}

std::optional<SchemeValue> Interpreter::isBooleanProc(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() != 1)
        throw InterpreterError("Cannot call boolean on multiple arguments", std::nullopt);
    if (std::holds_alternative<bool>(args[0].value)) {
        return SchemeValue(std::get<bool>(args[0].value));
    }
    throw InterpreterError("Arg is not a bool", std::nullopt);
}

std::optional<SchemeValue> Interpreter::listProcedure(Interpreter& interp, const std::vector<SchemeValue>& args)
{
    return SchemeValue(std::list<SchemeValue>(args.begin(), args.end()));
}

std::optional<SchemeValue> Interpreter::carProcudure(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("CAR expects 1 argument");
    }
    auto elements = args.back().as<std::vector<SchemeValue>>();
    if (elements.size() == 0) {
        throw InterpreterError("CAR invoked on empty list");
    }
    return elements[0];
}

std::optional<SchemeValue> Interpreter::cdrProcedure(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("car requires exactly 1 argument");
    }
    if (!args[0].isList()) {
        throw InterpreterError("car: argument must be a list");
    }
    const auto& list = args[0].asList();
    if (list.empty()) {
        throw InterpreterError("car: empty list");
    }
    return list.front();
}

std::optional<SchemeValue> Interpreter::cadrProcedure(Interpreter& ele, const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("cadr requires exactly 1 argument");
    }
    if (!args[0].isList()) {
        throw InterpreterError("cadr: argument must be a list");
    }
    const auto& list = args[0].asList();
    auto it = std::next(list.begin());
    if (it == list.end()) {
        throw InterpreterError("cadr: list too short");
    }
    return *it;
}

std::optional<SchemeValue> Interpreter::mult(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.empty())
        return SchemeValue(Number(1));
    SchemeValue result = args[0];
    for (size_t i = 1; i < args.size(); ++i) {
        result = result * args[i];
    }
    return result;
}

std::optional<SchemeValue> Interpreter::div(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.empty())
        throw InterpreterError("Cannot call procedure / on empty list", std::nullopt);
    if (args.size() == 1)
        return SchemeValue(Number(1)) / args[0];
    SchemeValue result = args[0];
    for (size_t i = 1; i < args.size(); ++i) {
        auto num = args[i].as<Number>();
        if (num.isZero()) {
            throw InterpreterError("Division by zero", std::nullopt);
        }
        result = result / args[i];
    }
    return result;
}

std::optional<SchemeValue> Interpreter::less(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() < 2)
        throw InterpreterError("< requires at least two arguments", std::nullopt);
    for (size_t i = 0; i < args.size() - 1; ++i) {
        auto comparison = args[i] <=> args[i + 1];
        if (comparison == std::partial_ordering::unordered) {
            throw InterpreterError("Cannot compare these values", std::nullopt);
        }
        if (!(comparison < 0)) {
            return SchemeValue(false);
        }
    }
    return SchemeValue(true);
}

std::optional<SchemeValue> Interpreter::greater(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() < 2)
        throw InterpreterError("> requires at least two arguments", std::nullopt);
    for (size_t i = 0; i < args.size() - 1; ++i) {
        auto comparison = args[i] <=> args[i + 1];
        if (comparison == std::partial_ordering::unordered) {
            throw InterpreterError("Cannot compare these values", std::nullopt);
        }
        if (!(comparison > 0)) {
            return SchemeValue(false);
        }
    }
    return SchemeValue(true);
}

std::optional<SchemeValue> Interpreter::equal(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() < 2)
        throw InterpreterError("= requires at least two arguments", std::nullopt);
    for (size_t i = 1; i < args.size(); ++i) {
        if (!(args[0] == args[i])) {
            return SchemeValue(false);
        }
    }
    return SchemeValue(true);
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
        const auto* list = std::get_if<std::vector<SchemeValue>>(&arg.value);
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

std::optional<SchemeValue> Interpreter::openInputFile(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("OPEN-INPUT-FILE requires exactly 1 argument");
    }
    const auto* filename = std::get_if<std::string>(&args[0].value);
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

std::optional<SchemeValue> Interpreter::openOutputFile(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("OPEN-OUTPUT-FILE requires exactly 1 argument");
    }
    const auto* filename = std::get_if<std::string>(&args[0].value);
    if (!filename) {
        throw InterpreterError("OPEN-OUTPUT-FILE argument must be a string");
    }
    auto file = std::make_shared<std::fstream>();
    file->open(*filename, std::ios::out);
    if (!file->is_open()) {
        throw InterpreterError("Could not open file: " + *filename);
    }
    return SchemeValue(Port(file, PortType::Output));
}

std::optional<SchemeValue> Interpreter::read(Interpreter& interp, const std::vector<SchemeValue>& args)
{
    if (args.size() > 1) {
        throw InterpreterError("READ accepts at most 1 argument");
    }
    std::istream* input;
    if (args.empty()) {
        input = &std::cin;
    } else {
        const auto* port = std::get_if<Port>(&args[0].value);
        if (!port || port->type != PortType::Input || !port->isOpen()) {
            throw InterpreterError("READ argument must be an open input port");
        }
        input = port->file.get();
    }
    std::string line;
    if (!std::getline(*input, line)) {
        throw InterpreterError("READ: End of file or error");
    }
    return SchemeValue(line);
}

std::optional<SchemeValue> Interpreter::write(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() < 1 || args.size() > 2) {
        throw InterpreterError("WRITE requires 1 or 2 arguments");
    }
    std::ostream* output;
    if (args.size() == 1) {
        output = &std::cout;
    } else {
        const auto* port = std::get_if<Port>(&args[1].value);
        if (!port || port->type != PortType::Output || !port->isOpen()) {
            throw InterpreterError("WRITE second argument must be an open output port");
        }
        output = port->file.get();
    }
    *output << args[0].toString();
    return std::nullopt;
}

std::optional<SchemeValue> Interpreter::display(Interpreter& interp, const std::vector<SchemeValue>& args)
{
    if (args.size() < 1 || args.size() > 2) {
        throw InterpreterError("DISPLAY requires 1 or 2 arguments");
    }
    std::ostream* output;
    if (args.size() == 1) {
        interp.outputStream << args[0].toString();
        return std::nullopt;
    } else {
        const auto* port = std::get_if<Port>(&args[1].value);
        if (!port || port->type != PortType::Output || !port->isOpen()) {
            throw InterpreterError("DISPLAY second argument must be an open output port");
        }
        output = port->file.get();
    }
    if (const auto* str = std::get_if<std::string>(&args[0].value)) {
        *output << *str;
    } else {
        *output << args[0].toString();
    }
    return std::nullopt;
}

std::optional<SchemeValue> Interpreter::newline(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() > 1) {
        throw InterpreterError("NEWLINE accepts at most 1 argument");
    }
    std::ostream* output;
    if (args.empty()) {
        output = &std::cout;
    } else {
        const auto* port = std::get_if<Port>(&args[0].value);
        if (!port || port->type != PortType::Output || !port->isOpen()) {
            throw InterpreterError("NEWLINE argument must be an open output port");
        }
        output = port->file.get();
    }
    *output << std::endl;
    return std::nullopt;
}

std::optional<SchemeValue> Interpreter::closePort(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("CLOSE-PORT requires exactly 1 argument");
    }
    const auto* port = std::get_if<Port>(&args[0].value);
    if (!port) {
        throw InterpreterError("CLOSE-PORT argument must be a port");
    }
    if (port->isOpen()) {
        port->close();
    }
    return std::nullopt;
}
std::optional<SchemeValue> Interpreter::makeVector(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() < 1 || args.size() > 2) {
        throw InterpreterError("make-vector requires 1 or 2 arguments");
    }
    if (!args[0].isNumber()) {
        throw InterpreterError("make-vector: first argument must be a number");
    }
    int k = args[0].as<Number>().toInt();
    if (k < 0) {
        throw InterpreterError("make-vector: length must be non-negative");
    }
    SchemeValue fill = args.size() == 2 ? args[1] : SchemeValue(Number(0));
    std::vector<SchemeValue> vec(k, fill);
    return SchemeValue(std::move(vec));
}

std::optional<SchemeValue> Interpreter::vectorProcedure(Interpreter&, const std::vector<SchemeValue>& args)
{
    return SchemeValue(args);
}

std::optional<SchemeValue> Interpreter::vectorRef(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() != 2) {
        throw InterpreterError("vector-ref requires exactly 2 arguments");
    }
    const auto* vec = std::get_if<std::vector<SchemeValue>>(&args[0].value);
    if (!vec) {
        throw InterpreterError("vector-ref: first argument must be a vector");
    }
    if (!args[1].isNumber()) {
        throw InterpreterError("vector-ref: second argument must be a number");
    }
    int index = args[1].as<Number>().toInt();
    if (index < 0 || static_cast<size_t>(index) >= vec->size()) {
        throw InterpreterError("vector-ref: index out of bounds");
    }

    return (*vec)[index];
}
std::optional<SchemeValue> Interpreter::vectorSet(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() != 3) {
        throw InterpreterError("vector-set! requires exactly 3 arguments");
    }
    auto* vec = std::get_if<std::vector<SchemeValue>>(&args[0].value);
    if (!vec) {
        throw InterpreterError("vector-set!: first argument must be a vector");
    }
    if (!args[1].isNumber()) {
        throw InterpreterError("vector-set!: second argument must be a number");
    }
    int index = args[1].as<Number>().toInt();
    if (index < 0 || static_cast<size_t>(index) >= vec->size()) {
        throw InterpreterError("vector-set!: index out of bounds");
    }
    std::vector<SchemeValue> newVec = *vec;
    newVec[index] = args[2];
    return SchemeValue(std::move(newVec));
}

std::optional<SchemeValue> Interpreter::vectorLength(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("vector-length requires exactly 1 argument");
    }
    const auto* vec = std::get_if<std::vector<SchemeValue>>(&args[0].value);
    if (!vec) {
        throw InterpreterError("vector-length: argument must be a vector");
    }
    return SchemeValue(Number(static_cast<int>(vec->size())));
}

std::optional<SchemeValue> Interpreter::printHelp(Interpreter& interp, const std::vector<SchemeValue>& args)
{

    interp.outputStream << "Available commands:\n"
                        << "  exit       - Exit the Jaws REPL\n"
                        << "  help       - Display this help message\n"
                        << "\nBasic Jaws syntax:\n"
                        << "  Numbers    - Integers (e.g., 42) or floating-point (e.g., 3.14)\n"
                        << "  Strings    - Enclosed in double quotes (e.g., \"Hello, Jaws!\")\n"
                        << "  Lists      - Enclosed in parentheses (e.g., (+ 1 2))\n"
                        << "  Symbols    - Identifiers for variables and functions\n"
                        << "\nBuilt-in functions:\n"
                        << "  +          - Addition (e.g., (+ 1 2 3))\n"
                        << "  define     - Define variables (e.g., (define x 10))\n"
                        << "  if         - Conditional execution (e.g., (if (> x 0) \"positive\" \"non-positive\"))\n"
                        << "\nEnter Scheme expressions to evaluate them" << std::endl;

    return std::nullopt;
}
