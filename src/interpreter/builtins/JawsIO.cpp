#include "builtins/JawsIO.h"
#include "Error.h"
#include "Port.h"
#include "parse.h"
#include "scan.h"
#include <fstream>
#include <iostream>

namespace jaws_io {

namespace {
    std::ostream* getOutputStream(const std::vector<SchemeValue>& args, size_t portArgIndex = 0)
    {
        if (args.size() <= portArgIndex) {
            return &std::cout;
        }

        auto portVal = args[portArgIndex].ensureValue();
        const auto* port = std::get_if<Port>(&portVal.value);
        if (!port || port->type != PortType::Output || !port->isOpen()) {
            throw InterpreterError("Output port argument must be an open output port");
        }
        return port->getOutput();
    }

    std::string getStringArg(const SchemeValue& arg, const char* funcName)
    {
        auto val = arg.ensureValue();
        const auto* str = std::get_if<std::string>(&val.value);
        if (!str) {
            throw InterpreterError(std::string(funcName) + " argument must be a string");
        }
        return *str;
    }
}

std::optional<SchemeValue> openOutputFile(
    interpret::InterpreterState&,
    const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("open-output-file requires exactly 1 argument");
    }

    std::string filename = getStringArg(args[0], "open-output-file");
    auto file = std::make_shared<std::fstream>();
    file->open(filename, std::ios::out);

    if (!file->is_open()) {
        throw InterpreterError("Could not open file: " + filename);
    }

    return SchemeValue(Port(file, PortType::Output));
}

std::optional<SchemeValue> openInputFile(
    interpret::InterpreterState&,
    const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("open-input-file requires exactly 1 argument");
    }

    std::string filename = getStringArg(args[0], "open-input-file");
    auto file = std::make_shared<std::fstream>();
    file->open(filename, std::ios::in);

    if (!file->is_open()) {
        throw InterpreterError("Could not open file: " + filename);
    }

    return SchemeValue(Port(file, PortType::Input));
}

std::optional<SchemeValue> read(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    if (args.size() > 1) {
        throw InterpreterError("read accepts at most 1 argument");
    }

    // Get input stream
    std::istream* input = &std::cin;
    if (!args.empty()) {
        auto val = args[0].ensureValue();
        const auto* port = std::get_if<Port>(&val.value);
        if (!port || port->type != PortType::Input || !port->isOpen()) {
            throw InterpreterError("read argument must be an open input port");
        }
        input = port->get();
    }

    // Read content
    std::string content;
    if (input == &std::cin) {
        if (!std::getline(*input, content)) {
            throw InterpreterError("read: End of file or error");
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
            throw InterpreterError("read: End of file or error");
        }
    }

    // Parse the content
    auto tokens = scanner::tokenize(content);
    auto expressions = parse::parse(std::move(tokens));

    if (expressions && !expressions->empty()) {
        return SchemeValue((*expressions)[0]);
    }
    return std::nullopt;
}

std::optional<SchemeValue> write(
    interpret::InterpreterState&,
    const std::vector<SchemeValue>& args)
{
    if (args.size() < 1 || args.size() > 2) {
        throw InterpreterError("write requires 1 or 2 arguments");
    }

    auto val = args[0].ensureValue();
    auto* output = getOutputStream(args, 1);
    *output << val.toString();

    return std::nullopt;
}

std::optional<SchemeValue> display(
    interpret::InterpreterState&,
    const std::vector<SchemeValue>& args)
{
    if (args.size() < 1 || args.size() > 2) {
        throw InterpreterError("display requires 1 or 2 arguments");
    }

    auto val = args[0].ensureValue();
    if (args.size() == 1) {
        if (const auto* str = std::get_if<std::string>(&val.value)) {
            std::cout << *str << std::flush; // Added flush here
        } else {
            std::cout << val.toString() << std::flush; // Added flush here
        }
        return std::nullopt;
    }

    auto* output = getOutputStream(args, 1);
    if (const auto* str = std::get_if<std::string>(&val.value)) {
        *output << *str << std::flush; // Added flush here
    } else {
        *output << val.toString() << std::flush; // Added flush here
    }

    return std::nullopt;
}

std::optional<SchemeValue> error(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    if (args.empty()) {
        throw InterpreterError("error requires at least 1 argument");
    }
    std::string message = "";
    for (size_t i = 0; i < args.size(); ++i) {
        auto val = args[i].ensureValue();
        if (const auto* str = std::get_if<std::string>(&val.value)) {
            message += *str;
        } else {
            message += val.toString();
        }
        if (i < args.size() - 1) {
            message += " ";
        }
    }
    throw InterpreterError(message);
}
std::optional<SchemeValue> newline(
    interpret::InterpreterState&,
    const std::vector<SchemeValue>& args)
{
    if (args.size() > 1) {
        throw InterpreterError("newline accepts at most 1 argument");
    }

    auto* output = getOutputStream(args);
    *output << std::endl;

    return std::nullopt;
}

std::optional<SchemeValue> closePort(
    interpret::InterpreterState&,
    const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("close-port requires exactly 1 argument");
    }

    auto val = args[0].ensureValue();
    const auto* port = std::get_if<Port>(&val.value);
    if (!port) {
        throw InterpreterError("close-port argument must be a port");
    }

    if (port->isOpen()) {
        port->close();
    }

    return std::nullopt;
}

std::optional<SchemeValue> socketServer(interpret::InterpreterState& state, const std::vector<SchemeValue>& args)
{
    if (args.size() < 1 || args.size() > 2) {
        throw InterpreterError("socket-server requires 1 or 2 arguments");
    }

    if (!args[0].ensureValue().isNumber()) {
        throw InterpreterError("socket-server: port argument must be a number");
    }
    int port = args[0].ensureValue().asNumber().toInt();

    int backlog = 5;
    if (args.size() == 2) {
        if (!args[1].ensureValue().isNumber()) {
            throw InterpreterError("socket-server: backlog argument must be a number");
        }
        backlog = args[1].ensureValue().asNumber().toInt();
    }
    try {
        auto p = Port::createServerSocket(port, backlog);
        return SchemeValue(p);
    } catch (const std::exception& e) {
        throw InterpreterError("socket-server: " + std::string(e.what()));
    }
}

std::optional<SchemeValue> socketConnect(interpret::InterpreterState& state, const std::vector<SchemeValue>& args)
{
    if (args.size() != 2) {
        throw InterpreterError("socket-connect requires exactly 2 arguments");
    }

    if (!args[0].ensureValue().isValue<std::string>()) {
        throw InterpreterError("socket-connect: host argument must be a string");
    }

    if (!args[1].ensureValue().isValue<Number>()) {
        throw InterpreterError("socket-connect: port argument must be a number");
    }

    const auto host = args[0].ensureValue().getValue<std::string>();
    const auto port = args[1].ensureValue().getValue<Number>().toInt();
    try {
        return SchemeValue(Port::connectToServer(host, port));
    } catch (InterpreterError& e) {
        throw e;
    } catch (const std::exception& e) {
        throw InterpreterError("socket-connect: " + std::string(e.what()));
    }
}

} // namespace jaws_io
