#include "Environment.h"
#include "Error.h"
#include "Syntax.h"
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

void Environment::define(const HygienicSyntax& name, const SchemeValue& value)
{
    std::lock_guard<std::mutex> lock(mutex);
    variables[name] = value;
}

void Environment::set(const HygienicSyntax& name, const SchemeValue& value)
{

    std::lock_guard<std::mutex> lock(mutex);
    auto it = variables.find(name);
    if (it != variables.end()) {
        DEBUG_LOG("  Found exact match in current Env, updating value");
        it->second = value;
        return;
    }

    // Look for compatible variable to set
    for (auto& [bindingSyntax, storedValue] : variables) {
        if (bindingSyntax.token.lexeme == name.token.lexeme) {
            if (!parent && bindingSyntax.context.marks.empty()) {
                DEBUG_LOG("  Global unmarked binding can be set");
                storedValue = value;
                return;
            }

            // Check if identifier's marks contain all of binding's marks
            bool containsAllMarks = true;
            for (const auto& bindingMark : bindingSyntax.context.marks) {
                if (name.context.marks.find(bindingMark) == name.context.marks.end()) {
                    containsAllMarks = false;
                    break;
                }
            }

            if (containsAllMarks) {
                DEBUG_LOG("  Identifier contains all binding marks, updating value");
                storedValue = value;
                return;
            }

            DEBUG_LOG("  Marks not compatible, continuing search");
        }
    }

    // If not found, check parent environment
    if (parent) {
        DEBUG_LOG("  No compatible match in current Env, trying parent @ " << parent.get());
        parent->set(name, value);
        return;
    }

    DEBUG_LOG("  Error: Variable '" << name.token.lexeme << "' not found in any environment");
    throw InterpreterError("Unbound variable " + name.token.lexeme);
}
// Helper to check if one set of marks contains all marks from another
bool containsAllMarks(const std::set<ScopeID>& container, const std::set<ScopeID>& contained)
{
    for (const auto& mark : contained) {
        if (container.find(mark) == container.end()) {
            return false;
        }
    }
    return true;
}

std::optional<SchemeValue> Environment::get(const HygienicSyntax& id) const
{
    DEBUG_LOG("Environment::get searching for '" << id.token.lexeme << "' with marks [");
    std::lock_guard<std::mutex> lock(mutex);

    auto it = variables.find(id);
    if (it != variables.end()) {
        DEBUG_LOG("  Found exact match in current Env! Value: " << it->second.toString());
        return it->second;
    }

    // Look for compatible bindings
    for (const auto& [bindingSyntax, value] : variables) {
        if (bindingSyntax.token.lexeme == id.token.lexeme) {
            if (!parent && bindingSyntax.context.marks.empty()) {
                DEBUG_LOG("  Global unmarked binding is accessible");
                return value;
            }

            // Check if looking-up identifier contains all marks from binding
            bool hasAllBindingMarks = true;
            for (const auto& mark : bindingSyntax.context.marks) {
                if (id.context.marks.find(mark) == id.context.marks.end()) {
                    hasAllBindingMarks = false;
                    DEBUG_LOG("    Missing mark " << mark << " from binding");
                    break;
                }
            }

            if (hasAllBindingMarks) {
                DEBUG_LOG("  Access granted - identifier has all binding marks");
                return value;
            }

            DEBUG_LOG("  Access denied - identifier missing required marks");
        }
    }

    // If not found, check parent environment
    if (parent) {
        DEBUG_LOG("  No compatible match in current Env @ " << this);
        DEBUG_LOG("  Checking parent @ " << parent.get());
        return parent->get(id);
    }

    DEBUG_LOG("  Not found in any environment");
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
            std::cerr << "  " << name.token.lexeme << " with marks [";
            for (const auto& mark : name.context.marks) {
                std::cerr << mark << " ";
            }
            std::cerr << "] -> " << value.toString() << "\n";
        }
    }
    if (parent) {
        std::cerr << "Parent ";
        parent->printEnv();
    }
}
