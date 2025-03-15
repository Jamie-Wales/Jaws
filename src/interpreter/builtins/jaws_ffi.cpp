#include "builtins/jaws_ffi.h"
#include "FFI.h"
#include <algorithm>

namespace jaws_ffi {

std::optional<SchemeValue> loadLibrary(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    if (args.size() != 2) {
        throw InterpreterError("load-library requires exactly 2 arguments");
    }

    if (!std::holds_alternative<std::string>(args[0].value) || !std::holds_alternative<std::string>(args[1].value)) {
        throw InterpreterError("load-library expects two string arguments: library-name and library-path");
    }

    const std::string& name = std::get<std::string>(args[0].value);
    const std::string& path = std::get<std::string>(args[1].value);

    try {
        FFIManager::instance().loadLibrary(name, path);
        return std::nullopt;
    } catch (const std::exception& e) {
        throw InterpreterError("Failed to load library: " + std::string(e.what()));
    }
}

std::optional<SchemeValue> registerFunction(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    // Check minimum required arguments
    if (args.size() < 4) {
        throw InterpreterError("register-function requires at least 4 arguments: library-name, function-name, scheme-name, return-type [arg-types...]");
    }

    // Validate argument types
    for (const auto& arg : args) {
        if (!std::holds_alternative<std::string>(arg.value)) {
            throw InterpreterError("All arguments to register-function must be strings");
        }
    }

    // Extract required arguments
    const std::string& libName = std::get<std::string>(args[0].value);
    const std::string& funcName = std::get<std::string>(args[1].value);
    const std::string& schemeName = std::get<std::string>(args[2].value);
    const std::string& retType = std::get<std::string>(args[3].value);

    // Validate return type
    std::vector<std::string> validTypes = { "void", "int", "double", "string" };
    if (std::find(validTypes.begin(), validTypes.end(), retType) == validTypes.end()) {
        throw InterpreterError("Invalid return type: " + retType + ". Supported types are: void, int, double, string");
    }

    // Extract optional argument types
    std::vector<std::string> argTypes;
    for (size_t i = 4; i < args.size(); i++) {
        std::string argType = std::get<std::string>(args[i].value);

        // Validate argument typejaws_ffi
        if (std::find(validTypes.begin(), validTypes.end(), argType) == validTypes.end()) {
            if (argType != "void") { // void is invalid as an argument type
                throw InterpreterError("Invalid argument type: " + argType + ". Supported types are: int, double, string");
            }
        }

        argTypes.push_back(argType);
    }

    try {
        SchemeValue wrappedFunc = FFIManager::instance().wrapCFunction(
            libName, funcName, retType, argTypes);

        state.env->define(schemeName, wrappedFunc);

        return std::nullopt;
    } catch (const std::exception& e) {
        throw InterpreterError("Failed to register function: " + std::string(e.what()));
    }
}

std::optional<SchemeValue> registerWrapper(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    if (args.size() != 2) {
        throw InterpreterError("register-wrapper requires exactly 2 arguments: signature and wrapper-procedure");
    }

    if (!std::holds_alternative<std::string>(args[0].value)) {
        throw InterpreterError("First argument to register-wrapper must be a signature string");
    }
    const std::string& signature = std::get<std::string>(args[0].value);

    if (!args[1].isProc()) {
        throw InterpreterError("Second argument to register-wrapper must be a procedure");
    }

    throw InterpreterError("register-wrapper is not fully implemented yet");

    return std::nullopt;
}

} // namespace jaws_ffi
