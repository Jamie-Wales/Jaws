
#pragma once
#include "DynamicLibrary.h"
#include "Error.h"
#include "Procedure.h"
#include <dlfcn.h>
#include <string>

using CIntFunc = int (*)(void);
using CDoubleFunc = double (*)(void);
using CStringFunc = const char* (*)(void);
using CVoidFunc = void (*)(void);

class FFIManager {
public:
    static FFIManager& instance() {
        static FFIManager inst;
        return inst;
    }

    void loadLibrary(const std::string& name, const std::string& path) {
        libraries[name] = std::make_unique<DynamicLibrary>(path);
    }

    template<typename T>
    T getFunction(const std::string& libName, const std::string& funcName) {
        auto it = libraries.find(libName);
        if (it == libraries.end()) {
            throw InterpreterError("Library not found: " + libName);
        }
        return it->second->getSymbol<T>(funcName);
    }

    SchemeValue wrapCFunction(const std::string& libName, const std::string& funcName, const std::string& retType) {
        if (retType == "int") {
            auto func = getFunction<CIntFunc>(libName, funcName);
            return SchemeValue(std::make_shared<BuiltInProcedure>(
                [func](interpret::InterpreterState&, const std::vector<SchemeValue>&) {
                    return SchemeValue(Number(func()));
                }));
        }
        else if (retType == "double") {
            auto func = getFunction<CDoubleFunc>(libName, funcName);
            return SchemeValue(std::make_shared<BuiltInProcedure>(
                [func](interpret::InterpreterState&, const std::vector<SchemeValue>&) {
                    return SchemeValue(Number(func()));
                }));
        }
        else if (retType == "string") {
            auto func = getFunction<CStringFunc>(libName, funcName);
            return SchemeValue(std::make_shared<BuiltInProcedure>(
                [func](interpret::InterpreterState&, const std::vector<SchemeValue>&) {
                    return SchemeValue(std::string(func()));
                }));
        }
        else if (retType == "void") {
            auto func = getFunction<CVoidFunc>(libName, funcName);
            return SchemeValue(std::make_shared<BuiltInProcedure>(
                [func](interpret::InterpreterState&, const std::vector<SchemeValue>&) {
                    func();
                    return std::optional<SchemeValue>(std::nullopt);
                }));
        }
        throw InterpreterError("Unsupported return type: " + retType);
    }

private:
    std::unordered_map<std::string, std::unique_ptr<DynamicLibrary>> libraries;

    FFIManager() = default;
    FFIManager(const FFIManager&) = delete;
    FFIManager& operator=(const FFIManager&) = delete;
};
