#include "builtins/JawsHof.h"
#include "Error.h"
#include "Procedure.h"
#include "Value.h"
#include "interpret.h"
#include "parse.h"
#include "scan.h"
#include <algorithm>
#include <functional>
#include <iostream>
#include <limits>
#include <list>
#include <memory>
#include <optional>
#include <string>
#include <vector>

namespace jaws_hof {

std::optional<SchemeValue> eval(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("eval: expects exactly one argument");
    }

    const SchemeValue& arg = args[0].ensureValue();

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

        std::optional<SchemeValue> eval_result;
        for (const auto& expr : *expressions) {
            eval_result = interpret::interpret(state, expr);
        }
        return eval_result;

    } catch (const std::exception& e) {
        throw InterpreterError("eval: error evaluating expression: " + std::string(e.what()));
    }
}

std::optional<SchemeValue> printHelp(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    std::cout << "Available commands:\n"
              << "  exit        - Exit the Jaws REPL\n"
              << "  help        - Display this help message\n"
              << "\nBasic Jaws syntax:\n"
              << "  Numbers     - Integers (e.g., 42) or floating-point (e.g., 3.14)\n"
              << "  Strings     - Enclosed in double quotes (e.g., \"Hello, Jaws!\")\n"
              << "  Lists       - Enclosed in parentheses (e.g., (+ 1 2))\n"
              << "  Symbols     - Identifiers for variables and functions\n"
              << "\nBuilt-in functions:\n"
              << "  +           - Addition (e.g., (+ 1 2 3))\n"
              << "  define      - Define variables (e.g., (define x 10))\n"
              << "  if          - Conditional execution (e.g., (if (> x 0) \"positive\" \"non-positive\"))\n"
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

    const auto& proc_sv = args[0].ensureValue();
    if (!proc_sv.isProc()) {
        throw InterpreterError("apply: first argument must be a procedure");
    }

    std::vector<SchemeValue> procArgs;
    procArgs.reserve(args.size());

    for (size_t i = 1; i < args.size() - 1; i++) {
        procArgs.push_back(args[i].ensureValue());
    }

    const auto& lastArg_sv = args.back().ensureValue();
    if (!lastArg_sv.isList()) {
        throw InterpreterError("apply: last argument must be a list");
    }

    auto last_list_ptr = lastArg_sv.asList();
    if (!last_list_ptr) {
        throw InterpreterError("apply: operation on null list for last argument");
    }

    procArgs.insert(procArgs.end(), last_list_ptr->begin(), last_list_ptr->end());

    return interpret::executeProcedure(state, proc_sv, procArgs);
}

std::optional<SchemeValue> callCC(
    interpret::InterpreterState& state, // Passed by reference
    const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("call/cc: expects exactly one argument");
    }
    const auto& proc_sv = args[0].ensureValue();
    if (!proc_sv.isProc()) {
        throw InterpreterError("call/cc: argument must be a procedure");
    }

    auto cont_proc_ptr = std::make_shared<Continuation>(state); // 'state' is copied here
    DEBUG_LOG("call/cc: Created continuation object.");
    std::vector<SchemeValue> contArgs = { SchemeValue(cont_proc_ptr) };
    try {
        DEBUG_LOG("call/cc: Calling user procedure with continuation.");
        std::optional<SchemeValue> normal_result = interpret::executeProcedure(state, proc_sv, contArgs);
        DEBUG_LOG("call/cc: User procedure returned normally.");
        return normal_result;
    } catch (const ContinuationInvocationException& e) {
        DEBUG_LOG("call/cc: Caught ContinuationInvocationException. Restoring state and returning value.");

        state = e.state;
        DEBUG_LOG("call/cc: State restored. Returning value: " << e.value.toString());
        return e.value;
    }
}
std::optional<SchemeValue> map(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    if (args.size() < 2) {
        throw InterpreterError("map: requires procedure and at least one list");
    }

    auto proc_sv = args[0].ensureValue();
    if (!proc_sv.isProc()) {
        throw InterpreterError("map: first argument must be a procedure");
    }

    std::vector<std::shared_ptr<std::list<SchemeValue>>> list_ptrs;
    size_t minLength = std::numeric_limits<size_t>::max();

    for (size_t i = 1; i < args.size(); i++) {
        auto list_sv = args[i].ensureValue();
        if (!list_sv.isList()) {
            throw InterpreterError("map: all arguments after procedure must be lists");
        }
        auto list_ptr = list_sv.asList();
        if (!list_ptr) {
            throw InterpreterError("map: operation on null list");
        }
        list_ptrs.push_back(list_ptr);
        minLength = std::min(minLength, list_ptr->size());
    }

    auto result_list_data = std::make_shared<std::list<SchemeValue>>();
    std::vector<std::list<SchemeValue>::const_iterator> iters;
    iters.reserve(list_ptrs.size());
    for (const auto& lp : list_ptrs) {
        iters.push_back(lp->begin());
    }

    for (size_t i = 0; i < minLength; i++) {
        std::vector<SchemeValue> procArgs;
        procArgs.reserve(iters.size());
        for (auto& it : iters) {
            procArgs.push_back(*it++);
        }

        auto procResult = interpret::executeProcedure(state, proc_sv, procArgs);
        if (procResult) {
            result_list_data->push_back(*procResult);
        } else {
            throw InterpreterError("map: procedure call did not return a value");
        }
    }

    return SchemeValue(result_list_data);
}

std::optional<SchemeValue> values(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    // Create a MultiValue object to hold the arguments
    auto multiValue = std::make_shared<MultiValue>(args);
    return SchemeValue(multiValue);
}

// Implementation of call-with-values
std::optional<SchemeValue> callWithValues(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    if (args.size() != 2) {
        throw InterpreterError("call-with-values: expects exactly two arguments (producer and consumer)");
    }

    const auto& producer = args[0].ensureValue();
    const auto& consumer = args[1].ensureValue();

    if (!producer.isProc() || !consumer.isProc()) {
        throw InterpreterError("call-with-values: both arguments must be procedures");
    }

    // Call the producer with no arguments
    std::vector<SchemeValue> noArgs;
    auto producerResult = interpret::executeProcedure(state, producer, noArgs);

    if (!producerResult) {
        // If producer returns no values, pass empty args to consumer
        return interpret::executeProcedure(state, consumer, noArgs);
    }

    // Extract values from the producer result
    std::vector<SchemeValue> consumerArgs;

    if (producerResult->isMultiValue()) {
        // If the result is a MultiValue object, unpack it
        auto multiValue = producerResult->asMultiValue();
        consumerArgs = multiValue->values;
    } else {
        // If it's a single value, pass it as the only argument
        consumerArgs.push_back(*producerResult);
    }

    // Call the consumer with the extracted values
    return interpret::executeProcedure(state, consumer, consumerArgs);
}

} // namespace jaws_hof
