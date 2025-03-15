
#include "FFI.h"

FFIManager& FFIManager::instance()
{
    static FFIManager inst;
    return inst;
}

void FFIManager::loadLibrary(const std::string& name, const std::string& path)
{
    libraries[name] = std::make_unique<DynamicLibrary>(path);
}

void* FFIManager::getSymbol(const std::string& libName, const std::string& funcName)
{
    auto it = libraries.find(libName);
    if (it == libraries.end()) {
        throw InterpreterError("Library not found: " + libName);
    }
    return it->second->getSymbol<void*>(funcName);
}

void FFIManager::registerWrapper(const std::string& signature, std::function<FFIFunction(void*)> wrapper)
{
    wrappers[signature] = wrapper;
}

SchemeValue FFIManager::wrapCFunction(
    const std::string& libName,
    const std::string& funcName,
    const std::string& retType,
    const std::vector<std::string>& argTypes)
{
    void* funcPtr = getSymbol(libName, funcName);
    if (!funcPtr) {
        throw InterpreterError("Function not found: " + funcName);
    }

    std::string signature = retType + "(";
    for (size_t i = 0; i < argTypes.size(); i++) {
        if (i > 0)
            signature += ",";
        signature += argTypes[i];
    }
    signature += ")";

    auto it = wrappers.find(signature);
    if (it == wrappers.end()) {
        throw InterpreterError("No wrapper registered for signature: " + signature);
    }

    FFIFunction wrappedFunc = it->second(funcPtr);
    return SchemeValue(std::make_shared<BuiltInProcedure>(wrappedFunc));
}

int FFIManager::schemeToInt(const SchemeValue& value)
{
    if (!value.isNumber()) {
        throw InterpreterError("Expected a number, got: " + value.toString());
    }
    return static_cast<int>(value.asNumber().toInt());
}

double FFIManager::schemeToDouble(const SchemeValue& value)
{
    if (!value.isNumber()) {
        throw InterpreterError("Expected a number, got: " + value.toString());
    }
    return value.asNumber().asFloat();
}

const char* FFIManager::schemeToString(const SchemeValue& value)
{
    static thread_local std::string storage;
    if (std::holds_alternative<std::string>(value.value)) {
        storage = std::get<std::string>(value.value);
    } else {
        storage = value.toString();
    }
    return storage.c_str();
}

FFIManager::FFIManager()
{
    registerDefaultWrappers();
}

void FFIManager::registerDefaultWrappers()
{
    // void()
    registerWrapper("void()", [](void* ptr) -> FFIFunction {
        auto func = reinterpret_cast<void (*)()>(ptr);
        return [func](interpret::InterpreterState&, const std::vector<SchemeValue>& args) -> std::optional<SchemeValue> {
            checkArgCount(args, 0, "FFI function");
            func();
            return std::nullopt;
        };
    });

    // int()
    registerWrapper("int()", [](void* ptr) -> FFIFunction {
        auto func = reinterpret_cast<int (*)()>(ptr);
        return [func](interpret::InterpreterState&, const std::vector<SchemeValue>& args) -> std::optional<SchemeValue> {
            checkArgCount(args, 0, "FFI function");
            int result = func();
            return SchemeValue(Number(result));
        };
    });

    // double()
    registerWrapper("double()", [](void* ptr) -> FFIFunction {
        auto func = reinterpret_cast<double (*)()>(ptr);
        return [func](interpret::InterpreterState&, const std::vector<SchemeValue>& args) -> std::optional<SchemeValue> {
            checkArgCount(args, 0, "FFI function");
            double result = func();
            return SchemeValue(Number(result));
        };
    });

    // string()
    registerWrapper("string()", [](void* ptr) -> FFIFunction {
        auto func = reinterpret_cast<const char* (*)()>(ptr);
        return [func](interpret::InterpreterState&, const std::vector<SchemeValue>& args) -> std::optional<SchemeValue> {
            checkArgCount(args, 0, "FFI function");
            const char* result = func();
            return SchemeValue(std::string(result ? result : ""));
        };
    });

    // void(int)
    registerWrapper("void(int)", [](void* ptr) -> FFIFunction {
        auto func = reinterpret_cast<void (*)(int)>(ptr);
        return [func](interpret::InterpreterState&, const std::vector<SchemeValue>& args) -> std::optional<SchemeValue> {
            checkArgCount(args, 1, "FFI function");
            func(schemeToInt(args[0]));
            return std::nullopt;
        };
    });

    // int(int)
    registerWrapper("int(int)", [](void* ptr) -> FFIFunction {
        auto func = reinterpret_cast<int (*)(int)>(ptr);
        return [func](interpret::InterpreterState&, const std::vector<SchemeValue>& args) -> std::optional<SchemeValue> {
            checkArgCount(args, 1, "FFI function");
            int result = func(schemeToInt(args[0]));
            return SchemeValue(Number(result));
        };
    });

    // double(double)
    registerWrapper("double(double)", [](void* ptr) -> FFIFunction {
        auto func = reinterpret_cast<double (*)(double)>(ptr);
        return [func](interpret::InterpreterState&, const std::vector<SchemeValue>& args) -> std::optional<SchemeValue> {
            checkArgCount(args, 1, "FFI function");
            double result = func(schemeToDouble(args[0]));
            return SchemeValue(Number(result));
        };
    });

    // string(string)
    registerWrapper("string(string)", [](void* ptr) -> FFIFunction {
        auto func = reinterpret_cast<const char* (*)(const char*)>(ptr);
        return [func](interpret::InterpreterState&, const std::vector<SchemeValue>& args) -> std::optional<SchemeValue> {
            checkArgCount(args, 1, "FFI function");
            const char* result = func(schemeToString(args[0]));
            return SchemeValue(std::string(result ? result : ""));
        };
    });

    // void(double)
    registerWrapper("void(double)", [](void* ptr) -> FFIFunction {
        auto func = reinterpret_cast<void (*)(double)>(ptr);
        return [func](interpret::InterpreterState&, const std::vector<SchemeValue>& args) -> std::optional<SchemeValue> {
            checkArgCount(args, 1, "FFI function");
            func(schemeToDouble(args[0]));
            return std::nullopt;
        };
    });

    // void(string)
    registerWrapper("void(string)", [](void* ptr) -> FFIFunction {
        auto func = reinterpret_cast<void (*)(const char*)>(ptr);
        return [func](interpret::InterpreterState&, const std::vector<SchemeValue>& args) -> std::optional<SchemeValue> {
            checkArgCount(args, 1, "FFI function");
            func(schemeToString(args[0]));
            return std::nullopt;
        };
    });

    // int(double)
    registerWrapper("int(double)", [](void* ptr) -> FFIFunction {
        auto func = reinterpret_cast<int (*)(double)>(ptr);
        return [func](interpret::InterpreterState&, const std::vector<SchemeValue>& args) -> std::optional<SchemeValue> {
            checkArgCount(args, 1, "FFI function");
            int result = func(schemeToDouble(args[0]));
            return SchemeValue(Number(result));
        };
    });

    // int(string)
    registerWrapper("int(string)", [](void* ptr) -> FFIFunction {
        auto func = reinterpret_cast<int (*)(const char*)>(ptr);
        return [func](interpret::InterpreterState&, const std::vector<SchemeValue>& args) -> std::optional<SchemeValue> {
            checkArgCount(args, 1, "FFI function");
            int result = func(schemeToString(args[0]));
            return SchemeValue(Number(result));
        };
    });

    // double(int)
    registerWrapper("double(int)", [](void* ptr) -> FFIFunction {
        auto func = reinterpret_cast<double (*)(int)>(ptr);
        return [func](interpret::InterpreterState&, const std::vector<SchemeValue>& args) -> std::optional<SchemeValue> {
            checkArgCount(args, 1, "FFI function");
            double result = func(schemeToInt(args[0]));
            return SchemeValue(Number(result));
        };
    });

    // double(string)
    registerWrapper("double(string)", [](void* ptr) -> FFIFunction {
        auto func = reinterpret_cast<double (*)(const char*)>(ptr);
        return [func](interpret::InterpreterState&, const std::vector<SchemeValue>& args) -> std::optional<SchemeValue> {
            checkArgCount(args, 1, "FFI function");
            double result = func(schemeToString(args[0]));
            return SchemeValue(Number(result));
        };
    });

    // Two argument functions

    // int(int,int)
    registerWrapper("int(int,int)", [](void* ptr) -> FFIFunction {
        auto func = reinterpret_cast<int (*)(int, int)>(ptr);
        return [func](interpret::InterpreterState&, const std::vector<SchemeValue>& args) -> std::optional<SchemeValue> {
            checkArgCount(args, 2, "FFI function");
            int result = func(schemeToInt(args[0]), schemeToInt(args[1]));
            return SchemeValue(Number(result));
        };
    });

    // double(double,double)
    registerWrapper("double(double,double)", [](void* ptr) -> FFIFunction {
        auto func = reinterpret_cast<double (*)(double, double)>(ptr);
        return [func](interpret::InterpreterState&, const std::vector<SchemeValue>& args) -> std::optional<SchemeValue> {
            checkArgCount(args, 2, "FFI function");
            double result = func(schemeToDouble(args[0]), schemeToDouble(args[1]));
            return SchemeValue(Number(result));
        };
    });

    // string(string,string)
    registerWrapper("string(string,string)", [](void* ptr) -> FFIFunction {
        auto func = reinterpret_cast<const char* (*)(const char*, const char*)>(ptr);
        return [func](interpret::InterpreterState&, const std::vector<SchemeValue>& args) -> std::optional<SchemeValue> {
            checkArgCount(args, 2, "FFI function");
            const char* result = func(schemeToString(args[0]), schemeToString(args[1]));
            return SchemeValue(std::string(result ? result : ""));
        };
    });

    // void(int,int)
    registerWrapper("void(int,int)", [](void* ptr) -> FFIFunction {
        auto func = reinterpret_cast<void (*)(int, int)>(ptr);
        return [func](interpret::InterpreterState&, const std::vector<SchemeValue>& args) -> std::optional<SchemeValue> {
            checkArgCount(args, 2, "FFI function");
            func(schemeToInt(args[0]), schemeToInt(args[1]));
            return std::nullopt;
        };
    });

    // void(double,double)
    registerWrapper("void(double,double)", [](void* ptr) -> FFIFunction {
        auto func = reinterpret_cast<void (*)(double, double)>(ptr);
        return [func](interpret::InterpreterState&, const std::vector<SchemeValue>& args) -> std::optional<SchemeValue> {
            checkArgCount(args, 2, "FFI function");
            func(schemeToDouble(args[0]), schemeToDouble(args[1]));
            return std::nullopt;
        };
    });

    // void(string,string)
    registerWrapper("void(string,string)", [](void* ptr) -> FFIFunction {
        auto func = reinterpret_cast<void (*)(const char*, const char*)>(ptr);
        return [func](interpret::InterpreterState&, const std::vector<SchemeValue>& args) -> std::optional<SchemeValue> {
            checkArgCount(args, 2, "FFI function");
            func(schemeToString(args[0]), schemeToString(args[1]));
            return std::nullopt;
        };
    });

    // Mixed type two-argument functions

    // int(int,double)
    registerWrapper("int(int,double)", [](void* ptr) -> FFIFunction {
        auto func = reinterpret_cast<int (*)(int, double)>(ptr);
        return [func](interpret::InterpreterState&, const std::vector<SchemeValue>& args) -> std::optional<SchemeValue> {
            checkArgCount(args, 2, "FFI function");
            int result = func(schemeToInt(args[0]), schemeToDouble(args[1]));
            return SchemeValue(Number(result));
        };
    });

    // int(double,int)
    registerWrapper("int(double,int)", [](void* ptr) -> FFIFunction {
        auto func = reinterpret_cast<int (*)(double, int)>(ptr);
        return [func](interpret::InterpreterState&, const std::vector<SchemeValue>& args) -> std::optional<SchemeValue> {
            checkArgCount(args, 2, "FFI function");
            int result = func(schemeToDouble(args[0]), schemeToInt(args[1]));
            return SchemeValue(Number(result));
        };
    });

    // double(int,double)
    registerWrapper("double(int,double)", [](void* ptr) -> FFIFunction {
        auto func = reinterpret_cast<double (*)(int, double)>(ptr);
        return [func](interpret::InterpreterState&, const std::vector<SchemeValue>& args) -> std::optional<SchemeValue> {
            checkArgCount(args, 2, "FFI function");
            double result = func(schemeToInt(args[0]), schemeToDouble(args[1]));
            return SchemeValue(Number(result));
        };
    });

    // double(double,int)
    registerWrapper("double(double,int)", [](void* ptr) -> FFIFunction {
        auto func = reinterpret_cast<double (*)(double, int)>(ptr);
        return [func](interpret::InterpreterState&, const std::vector<SchemeValue>& args) -> std::optional<SchemeValue> {
            checkArgCount(args, 2, "FFI function");
            double result = func(schemeToDouble(args[0]), schemeToInt(args[1]));
            return SchemeValue(Number(result));
        };
    });
    registerWrapper("void(int,string)", [](void* ptr) -> FFIFunction {
        auto func = reinterpret_cast<void (*)(int, const char*)>(ptr);
        return [func](interpret::InterpreterState&, const std::vector<SchemeValue>& args) -> std::optional<SchemeValue> {
            checkArgCount(args, 2, "FFI function");
            func(schemeToInt(args[0]), schemeToString(args[1]));
            return std::nullopt;
        };
    });
    registerWrapper("string(int)", [](void* ptr) -> FFIFunction {
        auto func = reinterpret_cast<const char* (*)(int)>(ptr);
        return [func](interpret::InterpreterState&, const std::vector<SchemeValue>& args) -> std::optional<SchemeValue> {
            checkArgCount(args, 1, "FFI function");
            const char* result = func(schemeToInt(args[0]));
            return SchemeValue(std::string(result ? result : ""));
        };
    });
    registerWrapper("string(int,int)", [](void* ptr) -> FFIFunction {
        auto func = reinterpret_cast<const char* (*)(int, int)>(ptr);
        return [func](interpret::InterpreterState&, const std::vector<SchemeValue>& args) -> std::optional<SchemeValue> {
            checkArgCount(args, 2, "FFI function");
            const char* result = func(schemeToInt(args[0]), schemeToInt(args[1]));
            return SchemeValue(std::string(result ? result : ""));
        };
    });

    // int(string,int)
    registerWrapper("int(string,int)", [](void* ptr) -> FFIFunction {
        auto func = reinterpret_cast<int (*)(const char*, int)>(ptr);
        return [func](interpret::InterpreterState&, const std::vector<SchemeValue>& args) -> std::optional<SchemeValue> {
            checkArgCount(args, 2, "FFI function");
            int result = func(schemeToString(args[0]), schemeToInt(args[1]));
            return SchemeValue(Number(result));
        };
    });

    // int(int,string)
    registerWrapper("int(int,string)", [](void* ptr) -> FFIFunction {
        auto func = reinterpret_cast<int (*)(int, const char*)>(ptr);
        return [func](interpret::InterpreterState&, const std::vector<SchemeValue>& args) -> std::optional<SchemeValue> {
            checkArgCount(args, 2, "FFI function");
            int result = func(schemeToInt(args[0]), schemeToString(args[1]));
            return SchemeValue(Number(result));
        };
    });

    // void(string,int)
    registerWrapper("void(string,int)", [](void* ptr) -> FFIFunction {
        auto func = reinterpret_cast<void (*)(const char*, int)>(ptr);
        return [func](interpret::InterpreterState&, const std::vector<SchemeValue>& args) -> std::optional<SchemeValue> {
            checkArgCount(args, 2, "FFI function");
            func(schemeToString(args[0]), schemeToInt(args[1]));
            return std::nullopt;
        };
    });

    // Three argument functions that might be useful

    // void(int,int,int)
    registerWrapper("void(int,int,int)", [](void* ptr) -> FFIFunction {
        auto func = reinterpret_cast<void (*)(int, int, int)>(ptr);
        return [func](interpret::InterpreterState&, const std::vector<SchemeValue>& args) -> std::optional<SchemeValue> {
            checkArgCount(args, 3, "FFI function");
            func(schemeToInt(args[0]), schemeToInt(args[1]), schemeToInt(args[2]));
            return std::nullopt;
        };
    });

    // int(int,int,int)
    registerWrapper("int(int,int,int)", [](void* ptr) -> FFIFunction {
        auto func = reinterpret_cast<int (*)(int, int, int)>(ptr);
        return [func](interpret::InterpreterState&, const std::vector<SchemeValue>& args) -> std::optional<SchemeValue> {
            checkArgCount(args, 3, "FFI function");
            int result = func(schemeToInt(args[0]), schemeToInt(args[1]), schemeToInt(args[2]));
            return SchemeValue(Number(result));
        };
    });

    // void(int,string,string)
    registerWrapper("void(int,string,string)", [](void* ptr) -> FFIFunction {
        auto func = reinterpret_cast<void (*)(int, const char*, const char*)>(ptr);
        return [func](interpret::InterpreterState&, const std::vector<SchemeValue>& args) -> std::optional<SchemeValue> {
            checkArgCount(args, 3, "FFI function");
            func(schemeToInt(args[0]), schemeToString(args[1]), schemeToString(args[2]));
            return std::nullopt;
        };
    });
    // Additional wrapper for void(int,int,string)
    registerWrapper("void(int,int,string)", [](void* ptr) -> FFIFunction {
        auto func = reinterpret_cast<void (*)(int, int, const char*)>(ptr);
        return [func](interpret::InterpreterState&, const std::vector<SchemeValue>& args) -> std::optional<SchemeValue> {
            checkArgCount(args, 3, "FFI function");
            func(schemeToInt(args[0]), schemeToInt(args[1]), schemeToString(args[2]));
            return std::nullopt;
        };
    });

    // Additional wrapper for void(int,int,int,int,int)
    registerWrapper("void(int,int,int,int,int)", [](void* ptr) -> FFIFunction {
        auto func = reinterpret_cast<void (*)(int, int, int, int, int)>(ptr);
        return [func](interpret::InterpreterState&, const std::vector<SchemeValue>& args) -> std::optional<SchemeValue> {
            checkArgCount(args, 5, "FFI function");
            func(schemeToInt(args[0]), schemeToInt(args[1]), schemeToInt(args[2]),
                schemeToInt(args[3]), schemeToInt(args[4]));
            return std::nullopt;
        };
    });

    // int(int,int,int,int)
    registerWrapper("int(int,int,int,int)", [](void* ptr) -> FFIFunction {
        auto func = reinterpret_cast<int (*)(int, int, int, int)>(ptr);
        return [func](interpret::InterpreterState&, const std::vector<SchemeValue>& args) -> std::optional<SchemeValue> {
            checkArgCount(args, 4, "FFI function");
            int result = func(schemeToInt(args[0]), schemeToInt(args[1]),
                schemeToInt(args[2]), schemeToInt(args[3]));
            return SchemeValue(Number(result));
        };
    });
    registerWrapper("void(int,int,int,int,int,int)", [](void* ptr) -> FFIFunction {
        auto func = reinterpret_cast<void (*)(int, int, int, int, int, int)>(ptr);
        return [func](interpret::InterpreterState&, const std::vector<SchemeValue>& args) -> std::optional<SchemeValue> {
            checkArgCount(args, 6, "FFI function");
            func(schemeToInt(args[0]), schemeToInt(args[1]),
                schemeToInt(args[2]), schemeToInt(args[3]),
                schemeToInt(args[4]), schemeToInt(args[5]));
            return std::nullopt;
        };
    });
    // int(int,int,string)
    registerWrapper("int(int,int,string)", [](void* ptr) -> FFIFunction {
        auto func = reinterpret_cast<int (*)(int, int, const char*)>(ptr);
        return [func](interpret::InterpreterState&, const std::vector<SchemeValue>& args) -> std::optional<SchemeValue> {
            checkArgCount(args, 3, "FFI function");
            int result = func(schemeToInt(args[0]), schemeToInt(args[1]), schemeToString(args[2]));
            return SchemeValue(Number(result));
        };
    });
}
