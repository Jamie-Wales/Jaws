#include "builtins/JawsIO.h"
#include "Error.h"
#include "Interpreter.h"
namespace jaws_io {
std::optional<SchemeValue> openOutputFile(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("OPEN-OUTPUT-FILE requires exactly 1 argument");
    }
    SchemeValue arg = args[0];
    if (arg.isExpr())
        arg = expressionToValue(*arg.asExpr());
    const auto* filename = std::get_if<std::string>(&arg.value);
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
std::optional<SchemeValue> read(Interpreter& interp, const std::vector<SchemeValue>& args)
{
    if (args.size() > 1) {
        throw InterpreterError("READ accepts at most 1 argument");
    }

    std::istream* input;
    if (args.empty()) {
        input = &std::cin;
    } else {
        SchemeValue arg = args[0];
        if (arg.isExpr()) {
            arg = expressionToValue(*arg.asExpr());
        }
        const auto* port = std::get_if<Port>(&arg.value);
        if (!port || port->type != PortType::Input || !port->isOpen()) {
            throw InterpreterError("READ argument must be an open input port");
        }
        input = port->get();
    }

    std::string content;
    if (input == &std::cin) {
        if (!std::getline(*input, content)) {
            throw InterpreterError("READ: End of file or error");
        }
    } else {
        std::string line;
        while (std::getline(*input, line)) {
            content += line + "\n";
            if (line.find_first_not_of(" \t\n") != std::string::npos) {
                break;
            }
        }
        if (content.empty()) {
            throw InterpreterError("READ: End of file or error");
        }
    }

    auto tokens = interp.s->tokenize(content);
    interp.p->load(tokens);
    auto opt = interp.p->parse();
    if (opt && !opt->empty()) {
        auto ele = (*opt)[0];
        return SchemeValue(ele);
    }
    return std::nullopt;
}

std::optional<SchemeValue> write(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() < 1 || args.size() > 2) {
        throw InterpreterError("WRITE requires 1 or 2 arguments");
    }

    SchemeValue arg = args[0];
    if (arg.isExpr()) {
        arg = expressionToValue(*arg.asExpr());
    }

    std::ostream* output;
    if (args.size() == 1) {
        output = &std::cout;
    } else {
        SchemeValue portArg = args[1];
        if (portArg.isExpr()) {
            portArg = expressionToValue(*portArg.asExpr());
        }
        const auto* port = std::get_if<Port>(&portArg.value);
        if (!port || port->type != PortType::Output || !port->isOpen()) {
            throw InterpreterError("WRITE second argument must be an open output port");
        }
        output = port->getOutput();
    }
    *output << arg.toString();
    return std::nullopt;
}
std::optional<SchemeValue> display(Interpreter& interp, const std::vector<SchemeValue>& args)
{
    if (args.size() < 1 || args.size() > 2) {
        throw InterpreterError("DISPLAY requires 1 or 2 arguments");
    }

    SchemeValue arg = args[0];
    if (arg.isExpr())
        arg = expressionToValue(*arg.asExpr());

    if (args.size() == 1) {
        interp.outputStream << arg.toString() << std::endl;
        return std::nullopt;
    }

    SchemeValue portArg = args[1];
    if (portArg.isExpr())
        portArg = expressionToValue(*portArg.asExpr());
    const auto* port = std::get_if<Port>(&portArg.value);
    if (!port || port->type != PortType::Output || !port->isOpen()) {
        throw InterpreterError("DISPLAY second argument must be an open output port");
    }
    std::ostream* output = port->getOutput();

    if (const auto* str = std::get_if<std::string>(&arg.value)) {
        *output << *str;
    } else {
        *output << arg.toString();
    }
    return std::nullopt;
}

std::optional<SchemeValue> newline(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() > 1) {
        throw InterpreterError("NEWLINE accepts at most 1 argument");
    }
    std::ostream* output;
    if (args.empty()) {
        output = &std::cout;
    } else {
        SchemeValue arg = args[0];
        if (arg.isExpr())
            arg = expressionToValue(*arg.asExpr());
        const auto* port = std::get_if<Port>(&arg.value);
        if (!port || port->type != PortType::Output || !port->isOpen()) {
            throw InterpreterError("NEWLINE argument must be an open output port");
        }
        output = port->getOutput();
    }
    *output << std::endl;
    return std::nullopt;
}

std::optional<SchemeValue> closePort(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("CLOSE-PORT requires exactly 1 argument");
    }
    SchemeValue arg = args[0];
    if (arg.isExpr())
        arg = expressionToValue(*arg.asExpr());
    const auto* port = std::get_if<Port>(&arg.value);
    if (!port) {
        throw InterpreterError("CLOSE-PORT argument must be a port");
    }
    if (port->isOpen()) {
        port->close();
    }
    return std::nullopt;
}
}
