#include "builtins/jaws_ffi.h"
#include "FFI.h"

namespace jaws_ffi {
std::optional<SchemeValue> loadLibrary(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    if (args.size() != 2 || !args[0].isValue<std::string>() || !args[1].isValue<std::string>()) {
        throw InterpreterError("load-library expects two string arguments: name and path");
    }

    FFIManager::instance().loadLibrary(args[0].as<std::string>(), args[1].as<std::string>());
    return std::nullopt;
}

std::optional<SchemeValue> registerFunction(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    if (args.size() != 4 || !args[0].isValue<std::string>() || !args[1].isValue<std::string>() || !args[2].isValue<std::string>() || !args[3].isValue<std::string>()) {
        throw InterpreterError("register-function expects four string arguments: library-name, function-name, scheme-name, return-type");
    }

    const std::string& libName = args[0].as<std::string>();
    const std::string& funcName = args[1].as<std::string>();
    const std::string& schemeName = args[2].as<std::string>();
    const std::string& retType = args[3].as<std::string>();

    auto wrappedFunc = FFIManager::instance().wrapCFunction(libName, funcName, retType);
    state.env->define(schemeName, wrappedFunc);
    return std::nullopt;
}
}
