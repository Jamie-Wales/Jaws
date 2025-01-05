#pragma once
#include "Error.h"
#include <dlfcn.h>
#include <string>

class DynamicLibrary {
public:
    explicit DynamicLibrary(const std::string& path)
    {
        handle = dlopen(path.c_str(), RTLD_LAZY);
        if (!handle) {
            throw InterpreterError("Failed to load library: " + std::string(dlerror()));
        }
    }

    ~DynamicLibrary()
    {
        if (handle) {
            dlclose(handle);
        }
    }

    template <typename T>
    T getSymbol(const std::string& name)
    {
        void* sym = dlsym(handle, name.c_str());
        if (!sym) {
            throw InterpreterError("Failed to find symbol: " + name + " - " + std::string(dlerror()));
        }
        return reinterpret_cast<T>(sym);
    }

private:
    void* handle;
};
