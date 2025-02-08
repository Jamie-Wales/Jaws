#include "builtins/JawsHof.h"
#include "Error.h"
#include "Procedure.h"
#include "parse.h"
#include "scan.h"

namespace jaws_hof {

std::optional<SchemeValue> eval(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("eval: expects exactly one argument");
    }

    const SchemeValue& arg = args[0];
    if (arg.isExpr()) {
        return interpret::interpret(state, arg.asExpr());
    }

    try {
        std::string valueStr = arg.toString();

        std::vector<Token> tokens = scanner::tokenize(valueStr);
        auto expressions = parse::parse(std::move(tokens));
        if (!expressions || expressions->empty()) {
            throw InterpreterError("eval: failed to parse value: " + valueStr);
        }
        return interpret::interpret(state, (*expressions)[0]);
    } catch (const std::exception& e) {
        throw InterpreterError("eval: error evaluating expression: " + std::string(e.what()));
    }
}

std::optional<SchemeValue> printHelp(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    std::cout << "Available commands:\n"
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
              << "\nEnter Scheme expressions to evaluate them\n";

    return std::nullopt;
}

std::optional<SchemeValue> apply(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    if (args.size() < 2) {
        throw InterpreterError("apply: expected at least 2 arguments");
    }

    if (!args[0].isProc()) {
        throw InterpreterError("apply: first argument must be a procedure");
    }

    std::vector<SchemeValue> procArgs;
    for (size_t i = 1; i < args.size() - 1; i++) {
        procArgs.push_back(args[i]);
    }
    const auto& lastArg = args.back();
    if (!lastArg.isList()) {
        throw InterpreterError("apply: last argument must be a list");
    }

    // Add list elements to arguments
    for (const auto& elem : lastArg.asList()) {
        procArgs.push_back(elem);
    }

    return interpret::executeProcedure(state, args[0], procArgs);
}

std::optional<SchemeValue> callCC(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("call/cc: expects exactly one argument");
    }

    const auto& proc = args[0];
    if (!proc.isProc()) {
        throw InterpreterError("call/cc: argument must be a procedure");
    }
    auto cont = std::make_shared<Continuation>(
        [&state](SchemeValue value) -> std::optional<SchemeValue> {
            return value;
        });
    std::vector<SchemeValue> contArgs = { SchemeValue(cont) };
    return interpret::executeProcedure(state, proc, contArgs);
}
} // namespace jaws_hof
