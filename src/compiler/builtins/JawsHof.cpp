#include "builtins/JawsHof.h"
#include "Error.h"
#include "Interpreter.h"
namespace jaws_hof {

std::optional<SchemeValue> eval(Interpreter& interp, const std::vector<SchemeValue>& args)
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

std::optional<SchemeValue> printHelp(Interpreter& interp, const std::vector<SchemeValue>& args)
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

std::optional<SchemeValue> apply(Interpreter& interp, const std::vector<SchemeValue>& args)
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
    return interp.executeProcedure(args[0], procArgs);
}
}
