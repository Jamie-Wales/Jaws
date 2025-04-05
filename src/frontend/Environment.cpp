#include "Environment.h"
#include "Error.h"
// #define DEBUG_LOGGING
#ifdef DEBUG_LOGGING
#define DEBUG_LOG(x) std::cerr << "[DEBUG] " << x << "\n"
#else
#define DEBUG_LOG(x)
#endif

Environment::Environment()
    : parent(nullptr)
{
    DEBUG_LOG("Created root Environment @ " << this);
}

Environment::Environment(std::shared_ptr<Environment> parent)
    : parent(parent)
{
    DEBUG_LOG("Created child Environment @ " << this << " with parent @ " << parent.get());
}

void Environment::define(const std::string& name, const SchemeValue& value)
{
    DEBUG_LOG("Environment::define '" << name << "' in Env @ " << this);
    std::lock_guard<std::mutex> lock(mutex);
    variables[name] = value;
}

void Environment::set(const std::string& name, const SchemeValue& value)
{
    DEBUG_LOG("Environment::set trying to set '" << name << "' in Env @ " << this);
    {
        std::lock_guard<std::mutex> lock(mutex);
        auto it = variables.find(name);
        if (it != variables.end()) {
            DEBUG_LOG("  Found '" << name << "' in current Env, updating value");
            it->second = value;
            return;
        }
    }
    
    if (parent) {
        DEBUG_LOG("  Not found in current Env, trying parent @ " << parent.get());
        parent->set(name, value);
        return;
    }
    
    DEBUG_LOG("  Error: Variable '" << name << "' not found in any environment");
    throw InterpreterError("Unbound variable " + name);
}

std::optional<SchemeValue> Environment::get(const std::string& name) const
{
    DEBUG_LOG("Environment::get searching for '" << name << "' in Env @ " << this);
    {
        std::lock_guard<std::mutex> lock(mutex);
        auto it = variables.find(name);
        if (it != variables.end()) {
            DEBUG_LOG("  Found '" << name << "' in current Env! Value: " << it->second.toString());
            return it->second;
        }
    }
    
    if (parent) {
        DEBUG_LOG("  Not found in current Env, checking parent @ " << parent.get());
        return parent->get(name);
    }
    
    DEBUG_LOG("  '" << name << "' not found in any environment");
    return std::nullopt;
}

std::shared_ptr<Environment> Environment::extend()
{
    DEBUG_LOG("Creating extended Environment from Env @ " << this);
    return std::make_shared<Environment>(shared_from_this());
}

std::shared_ptr<Environment> Environment::copy() const
{
    DEBUG_LOG("Copying Environment @ " << this);
    auto newEnv = std::make_shared<Environment>(parent);
    
    {
        std::lock_guard<std::mutex> lock(mutex);
        newEnv->variables = variables;
    }
    
    DEBUG_LOG("  Created copy @ " << newEnv.get());
    return newEnv;
}

void Environment::printEnv() const
{
    std::cerr << "Environment @ " << this << ":\n";
    
    {
        std::lock_guard<std::mutex> lock(mutex);
        for (const auto& [name, value] : variables) {
            std::cerr << "  " << name << " -> " << value.toString() << "\n";
        }
    }
    
    if (parent) {
        std::cerr << "Parent ";
        parent->printEnv();
    }
}
