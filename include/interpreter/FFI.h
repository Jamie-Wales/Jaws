#pragma once
#include "DynamicLibrary.h"
#include "Error.h"
#include "Procedure.h"
#include "Value.h"
#include <dlfcn.h>
#include <functional>
#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

using FFIFunction = std::function<std::optional<SchemeValue>(
    interpret::InterpreterState&, const std::vector<SchemeValue>&)>;

class FFIManager {
public:
    static FFIManager& instance();
    void loadLibrary(const std::string& name, const std::string& path);
    void* getSymbol(const std::string& libName, const std::string& funcName);
    void registerWrapper(const std::string& signature, std::function<FFIFunction(void*)> wrapper);
    SchemeValue wrapCFunction(
        const std::string& libName,
        const std::string& funcName,
        const std::string& retType,
        const std::vector<std::string>& argTypes = {});
    static int schemeToInt(const SchemeValue& value);
    static double schemeToDouble(const SchemeValue& value);
    static const char* schemeToString(const SchemeValue& value);

private:
    std::unordered_map<std::string, std::unique_ptr<DynamicLibrary>> libraries;
    std::unordered_map<std::string, std::function<FFIFunction(void*)>> wrappers;

    FFIManager();
    FFIManager(const FFIManager&) = delete;
    FFIManager& operator=(const FFIManager&) = delete;

    // Register default function wrappers for common signatures
    void registerDefaultWrappers();
};
