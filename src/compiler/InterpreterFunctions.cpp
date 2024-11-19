#include "Error.h"
#include "Interpreter.h"
#include "Number.h"
#include "Port.h"
#include <optional>

std::optional<SchemeValue> Interpreter::lessOrEqual(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() < 2) {
        throw InterpreterError("'<=' requires at least 2 arguments");
    }

    for (size_t i = 0; i < args.size() - 1; i++) {
        SchemeValue curr = args[i], next = args[i + 1];

        if (curr.isExpr())
            curr = expressionToValue(*curr.asExpr());
        if (next.isExpr())
            next = expressionToValue(*next.asExpr());

        if (!curr.isNumber() || !next.isNumber()) {
            throw InterpreterError("Cannot compare non-numeric values with <=");
        }

        auto ordering = curr.asNumber() <=> next.asNumber();
        if (ordering == std::partial_ordering::unordered) {
            throw InterpreterError("Cannot compare these numeric types with <=");
        }
        if (ordering == std::partial_ordering::greater) {
            return SchemeValue(false);
        }
    }
    return SchemeValue(true);
}

std::optional<SchemeValue> Interpreter::eval(Interpreter& interp, const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("Eval expects one argument");
    }
    SchemeValue arg = args[0];
    if (arg.isExpr()) {
        return interp.interpret(arg.asExpr());
    }

    return arg;
}

std::optional<SchemeValue> Interpreter::greaterOrEqual(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() < 2) {
        throw InterpreterError("'>=' requires at least 2 arguments");
    }

    for (size_t i = 0; i < args.size() - 1; i++) {
        SchemeValue curr = args[i], next = args[i + 1];

        if (curr.isExpr())
            curr = expressionToValue(*curr.asExpr());
        if (next.isExpr())
            next = expressionToValue(*next.asExpr());

        if (!curr.isNumber() || !next.isNumber()) {
            throw InterpreterError("Cannot compare non-numeric values with >=");
        }

        auto ordering = curr.asNumber() <=> next.asNumber();
        if (ordering == std::partial_ordering::unordered) {
            throw InterpreterError("Cannot compare these numeric types with >=");
        }
        if (ordering == std::partial_ordering::less) {
            return SchemeValue(false);
        }
    }
    return SchemeValue(true);
}

std::optional<SchemeValue> Interpreter::plus(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.empty())
        return SchemeValue(Number(0));
    SchemeValue result = args[0];
    if (result.isExpr())
        result = expressionToValue(*result.asExpr());

    for (size_t i = 1; i < args.size(); ++i) {
        SchemeValue curr = args[i];
        if (curr.isExpr())
            curr = expressionToValue(*curr.asExpr());
        result = result + curr;
    }
    return result;
}

std::optional<SchemeValue> Interpreter::minus(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.empty())
        throw InterpreterError("Cannot call procedure - on empty list", std::nullopt);

    if (args.size() == 1) {
        SchemeValue arg = args[0];
        if (arg.isExpr())
            arg = expressionToValue(*arg.asExpr());
        return -arg;
    }

    SchemeValue result = args[0];
    if (result.isExpr())
        result = expressionToValue(*result.asExpr());

    for (size_t i = 1; i < args.size(); ++i) {
        SchemeValue curr = args[i];
        if (curr.isExpr())
            curr = expressionToValue(*curr.asExpr());
        result = result - curr;
    }
    return result;
}

std::optional<SchemeValue> Interpreter::isBooleanProc(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() != 1)
        throw InterpreterError("Cannot call boolean on multiple arguments", std::nullopt);
    SchemeValue arg = args[0];
    if (arg.isExpr())
        arg = expressionToValue(*arg.asExpr());
    if (std::holds_alternative<bool>(arg.value)) {
        return SchemeValue(std::get<bool>(arg.value));
    }
    throw InterpreterError("Arg is not a bool", std::nullopt);
}

std::optional<SchemeValue> Interpreter::listProcedure(Interpreter& interp, const std::vector<SchemeValue>& args)
{
    std::vector<SchemeValue> evaluated;
    evaluated.reserve(args.size());
    for (const auto& arg : args) {
        if (arg.isExpr()) {
            evaluated.push_back(expressionToValue(*arg.asExpr()));
        } else {
            evaluated.push_back(arg);
        }
    }
    return SchemeValue(std::list<SchemeValue>(evaluated.begin(), evaluated.end()));
}

std::optional<SchemeValue> Interpreter::carProcudure(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("CAR expects 1 argument");
    }
    SchemeValue arg = args[0];
    if (arg.isExpr())
        arg = expressionToValue(*arg.asExpr());
    auto elements = arg.as<std::list<SchemeValue>>();
    if (elements.size() == 0) {
        throw InterpreterError("CAR invoked on empty list");
    }
    return elements.front();
}

std::optional<SchemeValue> Interpreter::cdrProcedure(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("car requires exactly 1 argument");
    }
    SchemeValue arg = args[0];
    if (arg.isExpr())
        arg = expressionToValue(*arg.asExpr());
    if (!arg.isList()) {
        throw InterpreterError("car: argument must be a list");
    }
    const auto& list = arg.asList();
    if (list.empty()) {
        throw InterpreterError("car: empty list");
    }
    return SchemeValue(std::list<SchemeValue>(list.begin()++, list.end()));
}

std::optional<SchemeValue> Interpreter::cadrProcedure(Interpreter& ele, const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("cadr requires exactly 1 argument");
    }
    SchemeValue arg = args[0];
    if (arg.isExpr())
        arg = expressionToValue(*arg.asExpr());
    if (!arg.isList()) {
        throw InterpreterError("cadr: argument must be a list");
    }
    const auto& list = arg.asList();
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
    if (result.isExpr())
        result = expressionToValue(*result.asExpr());

    for (size_t i = 1; i < args.size(); ++i) {
        SchemeValue curr = args[i];
        if (curr.isExpr())
            curr = expressionToValue(*curr.asExpr());
        result = result * curr;
    }
    return result;
}

std::optional<SchemeValue> Interpreter::div(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.empty())
        throw InterpreterError("Cannot call procedure / on empty list", std::nullopt);

    if (args.size() == 1) {
        SchemeValue arg = args[0];
        if (arg.isExpr())
            arg = expressionToValue(*arg.asExpr());
        return SchemeValue(Number(1)) / arg;
    }

    SchemeValue result = args[0];
    if (result.isExpr())
        result = expressionToValue(*result.asExpr());

    for (size_t i = 1; i < args.size(); ++i) {
        SchemeValue curr = args[i];
        if (curr.isExpr())
            curr = expressionToValue(*curr.asExpr());
        auto num = curr.as<Number>();
        if (num.isZero()) {
            throw InterpreterError("Division by zero", std::nullopt);
        }
        result = result / curr;
    }
    return result;
}

std::optional<SchemeValue> Interpreter::less(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() < 2)
        throw InterpreterError("< requires at least two arguments", std::nullopt);

    for (size_t i = 0; i < args.size() - 1; ++i) {
        SchemeValue curr = args[i], next = args[i + 1];
        if (curr.isExpr())
            curr = expressionToValue(*curr.asExpr());
        if (next.isExpr())
            next = expressionToValue(*next.asExpr());

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

std::optional<SchemeValue> Interpreter::greater(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() < 2)
        throw InterpreterError("> requires at least two arguments", std::nullopt);

    for (size_t i = 0; i < args.size() - 1; ++i) {
        SchemeValue curr = args[i], next = args[i + 1];
        if (curr.isExpr())
            curr = expressionToValue(*curr.asExpr());
        if (next.isExpr())
            next = expressionToValue(*next.asExpr());

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

std::optional<SchemeValue> Interpreter::equal(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() < 2)
        throw InterpreterError("= requires at least two arguments", std::nullopt);

    SchemeValue first = args[0];
    if (first.isExpr())
        first = expressionToValue(*first.asExpr());

    for (size_t i = 1; i < args.size(); ++i) {
        SchemeValue curr = args[i];
        if (curr.isExpr())
            curr = expressionToValue(*curr.asExpr());
        if (!(first == curr)) {
            return SchemeValue(false);
        }
    }
    return SchemeValue(true);
}

std::optional<SchemeValue> Interpreter::openInputFile(Interpreter&, const std::vector<SchemeValue>& args)
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

std::optional<SchemeValue> Interpreter::openOutputFile(Interpreter&, const std::vector<SchemeValue>& args)
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
std::optional<SchemeValue> Interpreter::read(Interpreter& interp, const std::vector<SchemeValue>& args)
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
        // For string streams and files, read until we have a complete expression
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

std::optional<SchemeValue> Interpreter::write(Interpreter&, const std::vector<SchemeValue>& args)
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
std::optional<SchemeValue> Interpreter::display(Interpreter& interp, const std::vector<SchemeValue>& args)
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

std::optional<SchemeValue> Interpreter::newline(Interpreter&, const std::vector<SchemeValue>& args)
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

std::optional<SchemeValue> Interpreter::closePort(Interpreter&, const std::vector<SchemeValue>& args)
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

std::optional<SchemeValue> Interpreter::makeVector(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() < 1 || args.size() > 2) {
        throw InterpreterError("make-vector requires 1 or 2 arguments");
    }

    SchemeValue sizeArg = args[0];
    if (sizeArg.isExpr())
        sizeArg = expressionToValue(*sizeArg.asExpr());
    if (!sizeArg.isNumber()) {
        throw InterpreterError("make-vector: first argument must be a number");
    }
    int k = sizeArg.as<Number>().toInt();
    if (k < 0) {
        throw InterpreterError("make-vector: length must be non-negative");
    }

    SchemeValue fill = args.size() == 2 ? args[1] : SchemeValue(Number(0));
    if (fill.isExpr())
        fill = expressionToValue(*fill.asExpr());
    std::vector<SchemeValue> vec(k, fill);
    return SchemeValue(std::move(vec));
}

std::optional<SchemeValue> Interpreter::vectorProcedure(Interpreter&, const std::vector<SchemeValue>& args)
{
    std::vector<SchemeValue> evaluated;
    evaluated.reserve(args.size());
    for (const auto& arg : args) {
        if (arg.isExpr()) {
            evaluated.push_back(expressionToValue(*arg.asExpr()));
        } else {
            evaluated.push_back(arg);
        }
    }
    return SchemeValue(evaluated);
}

std::optional<SchemeValue> Interpreter::vectorRef(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() != 2) {
        throw InterpreterError("vector-ref requires exactly 2 arguments");
    }

    SchemeValue vecArg = args[0];
    if (vecArg.isExpr())
        vecArg = expressionToValue(*vecArg.asExpr());
    const auto* vec = std::get_if<std::vector<SchemeValue>>(&vecArg.value);
    if (!vec) {
        throw InterpreterError("vector-ref: first argument must be a vector");
    }

    SchemeValue indexArg = args[1];
    if (indexArg.isExpr())
        indexArg = expressionToValue(*indexArg.asExpr());
    if (!indexArg.isNumber()) {
        throw InterpreterError("vector-ref: second argument must be a number");
    }
    int index = indexArg.as<Number>().toInt();
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

    SchemeValue vecArg = args[0];
    if (vecArg.isExpr())
        vecArg = expressionToValue(*vecArg.asExpr());
    auto* vec = std::get_if<std::vector<SchemeValue>>(&vecArg.value);
    if (!vec) {
        throw InterpreterError("vector-set!: first argument must be a vector");
    }

    SchemeValue indexArg = args[1];
    if (indexArg.isExpr())
        indexArg = expressionToValue(*indexArg.asExpr());
    if (!indexArg.isNumber()) {
        throw InterpreterError("vector-set!: second argument must be a number");
    }
    int index = indexArg.as<Number>().toInt();
    if (index < 0 || static_cast<size_t>(index) >= vec->size()) {
        throw InterpreterError("vector-set!: index out of bounds");
    }

    SchemeValue valueArg = args[2];
    if (valueArg.isExpr())
        valueArg = expressionToValue(*valueArg.asExpr());

    std::vector<SchemeValue> newVec = *vec;
    newVec[index] = valueArg;
    return SchemeValue(std::move(newVec));
}

std::optional<SchemeValue> Interpreter::vectorLength(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("vector-length requires exactly 1 argument");
    }

    SchemeValue arg = args[0];
    if (arg.isExpr())
        arg = expressionToValue(*arg.asExpr());
    const auto* vec = std::get_if<std::vector<SchemeValue>>(&arg.value);
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
std::optional<SchemeValue> Interpreter::isProcedure(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() != 1)
        throw std::runtime_error("procedure?: expected 1 argument");
    return SchemeValue(args[0].isProc());
}

std::optional<SchemeValue> Interpreter::isPair(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() != 1)
        throw std::runtime_error("pair?: expected 1 argument");
    return SchemeValue(args[0].isList() && !args[0].asList().empty());
}

std::optional<SchemeValue> Interpreter::isList(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() != 1)
        throw std::runtime_error("list?: expected 1 argument");
    return SchemeValue(args[0].isList());
}

std::optional<SchemeValue> Interpreter::isVector(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() != 1)
        throw std::runtime_error("vector?: expected 1 argument");
    return SchemeValue(std::holds_alternative<std::vector<SchemeValue>>(args[0].value));
}

std::optional<SchemeValue> Interpreter::isSymbol(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() != 1)
        throw std::runtime_error("symbol?: expected 1 argument");
    return SchemeValue(args[0].isSymbol());
}

std::optional<SchemeValue> Interpreter::isNumber(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() != 1)
        throw std::runtime_error("number?: expected 1 argument");
    return SchemeValue(args[0].isNumber());
}

std::optional<SchemeValue> Interpreter::isString(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() != 1)
        throw std::runtime_error("string?: expected 1 argument");
    return SchemeValue(std::holds_alternative<std::string>(args[0].value));
}

std::optional<SchemeValue> Interpreter::isPort(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() != 1)
        throw std::runtime_error("port?: expected 1 argument");
    return SchemeValue(args[0].isPort());
}

std::optional<SchemeValue> Interpreter::isNull(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() != 1)
        throw std::runtime_error("null?: expected 1 argument");
    return SchemeValue(args[0].isList() && args[0].asList().empty());
}

std::optional<SchemeValue> Interpreter::isEq(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() != 2)
        throw std::runtime_error("eq?: expected 2 arguments");
    return SchemeValue(&args[0] == &args[1]);
}

std::optional<SchemeValue> Interpreter::isEqv(Interpreter&, const std::vector<SchemeValue>& args)
{
    if (args.size() != 2)
        throw std::runtime_error("eqv?: expected 2 arguments");
    return SchemeValue(args[0] == args[1]);
}

std::optional<SchemeValue> Interpreter::apply(Interpreter& interp, const std::vector<SchemeValue>& args)
{
    if (args.size() < 2)
        throw std::runtime_error("apply: expected at least 2 arguments");

    if (!args[0].isProc())
        throw std::runtime_error("apply: first argument must be a procedure");

    std::vector<SchemeValue> procArgs;
    for (size_t i = 1; i < args.size() - 1; i++) {
        procArgs.push_back(args[i]);
    }

    const auto& lastArg = args.back();
    if (!lastArg.isList())
        throw std::runtime_error("apply: last argument must be a list");

    for (const auto& elem : lastArg.asList()) {
        procArgs.push_back(elem);
    }

    return args[0].call(interp, procArgs);
}
