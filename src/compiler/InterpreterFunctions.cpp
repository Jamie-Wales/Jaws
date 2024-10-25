#include "Error.h"
#include "Interpreter.h"
#include "Port.h"
#include <optional>

SchemeValue Interpreter::plus(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.empty())
        return SchemeValue(Number(0));
    SchemeValue result = args[0];
    for (size_t i = 1; i < args.size(); ++i) {
        result = result + args[i];
    }
    return result;
}

SchemeValue Interpreter::minus(Interpreter&, const std::vector<SchemeValue>& args)
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

SchemeValue Interpreter::isBooleanProc(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() != 1)
        throw InterpreterError("Cannot call boolean on multiple arguments", std::nullopt);
    if (std::holds_alternative<bool>(args[0].value)) {
        return SchemeValue(std::get<bool>(args[0].value));
    }
    throw InterpreterError("Arg is not a bool", std::nullopt);
}

SchemeValue Interpreter::listProcedure(Interpreter&, const std::vector<SchemeValue>& args)
{
    return SchemeValue(args);
}

SchemeValue Interpreter::carProcudure(Interpreter&, const std::vector<SchemeValue>& args)
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

SchemeValue Interpreter::cdrProcedure(Interpreter&, const std::vector<SchemeValue>& args)
{

    if (args.size() != 1) {
        throw InterpreterError("CDR expects 1 argument a list");
    }

    auto elements = args.back().as<std::vector<SchemeValue>>();
    if (elements.size() < 2) {
        throw InterpreterError("CDR requires a list of atleast 2 elements");
    }
    std::vector<SchemeValue> outputs = {};
    for (int i = 1; i < elements.size(); i++) {
        outputs.emplace_back(elements[i]);
    }

    return SchemeValue(outputs);
}

SchemeValue Interpreter::cadrProcedure(Interpreter& ele, const std::vector<SchemeValue>& args)
{
    SchemeValue val = cdrProcedure(ele, args);
    val = carProcudure(ele, { args });
    return val;
}
SchemeValue Interpreter::mult(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.empty())
        return SchemeValue(Number(1));

    SchemeValue result = args[0];
    for (size_t i = 1; i < args.size(); ++i) {
        result = result * args[i];
    }
    return result;
}

SchemeValue Interpreter::div(Interpreter&, const std::vector<SchemeValue>& args)
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

SchemeValue Interpreter::less(Interpreter&, const std::vector<SchemeValue>& args)
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

SchemeValue Interpreter::greater(Interpreter&, const std::vector<SchemeValue>& args)
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

SchemeValue Interpreter::equal(Interpreter&, const std::vector<SchemeValue>& args)
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

SchemeValue Interpreter::cons(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() != 2) {
        throw InterpreterError("CONS requires exactly 2 arguments");
    }

    std::vector<SchemeValue> result;
    result.push_back(args[0]);

    if (auto* list = std::get_if<std::vector<SchemeValue>>(&args[1].value)) {
        result.insert(result.end(), list->begin(), list->end());
    } else {
        // If not a list, just add the element
        result.push_back(args[1]);
    }

    return SchemeValue(result);
}

SchemeValue Interpreter::length(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("LENGTH requires exactly 1 argument");
    }

    const auto* list = std::get_if<std::vector<SchemeValue>>(&args[0].value);
    if (!list) {
        throw InterpreterError("LENGTH argument must be a list");
    }

    return SchemeValue(Number(static_cast<int>(list->size())));
}

SchemeValue Interpreter::append(Interpreter&, const std::vector<SchemeValue>& args)
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

SchemeValue Interpreter::reverse(Interpreter&, const std::vector<SchemeValue>& args)
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

SchemeValue Interpreter::listRef(Interpreter&, const std::vector<SchemeValue>& args)
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

SchemeValue Interpreter::listTail(Interpreter&, const std::vector<SchemeValue>& args)
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

SchemeValue Interpreter::openInputFile(Interpreter&, const std::vector<SchemeValue>& args)
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

SchemeValue Interpreter::openOutputFile(Interpreter&, const std::vector<SchemeValue>& args)
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

SchemeValue Interpreter::read(Interpreter& interp, const std::vector<SchemeValue>& args)
{
    if (args.size() > 1) {
        throw InterpreterError("READ accepts at most 1 argument");
    }

    std::istream* input;
    if (args.empty()) {
        input = &std::cin; // Default to standard input
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

SchemeValue Interpreter::write(Interpreter&, const std::vector<SchemeValue>& args)
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
    return SchemeValue();
}

SchemeValue Interpreter::display(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() < 1 || args.size() > 2) {
        throw InterpreterError("DISPLAY requires 1 or 2 arguments");
    }

    std::ostream* output;
    if (args.size() == 1) {
        output = &std::cout;
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
    return SchemeValue();
}

SchemeValue Interpreter::closePort(Interpreter&, const std::vector<SchemeValue>& args)
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
    return SchemeValue();
}
SchemeValue Interpreter::newline(Interpreter&, const std::vector<SchemeValue>& args)
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
    return SchemeValue();
}
