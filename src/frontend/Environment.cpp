#include "Environment.h"
#include "Error.h"
#include "Syntax.h"
#include <algorithm>

#ifdef DEBUG_LOGGING
#define DEBUG_LOG(x) std::cerr << "(ENV) " << x << "\n"
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

namespace {

bool isSubset(const std::set<ScopeID>& subset, const std::set<ScopeID>& superset)
{
    return std::includes(superset.begin(), superset.end(), subset.begin(), subset.end());
}

// Sets-of-scopes resolution: a binding is a candidate when its scopes are a
// subset of the reference's, and the candidate with the most scopes wins.
template <typename Map>
auto resolveInFrame(Map& variables, const HygienicSyntax& id) -> decltype(&variables.begin()->second)
{
    decltype(&variables.begin()->second) best = nullptr;
    size_t bestSize = 0;
    bool ambiguous = false;

    for (auto& [binding, value] : variables) {
        if (binding.token.lexeme != id.token.lexeme || !isSubset(binding.context.marks, id.context.marks))
            continue;
        size_t size = binding.context.marks.size();
        if (!best || size > bestSize) {
            best = &value;
            bestSize = size;
            ambiguous = false;
        } else if (size == bestSize) {
            ambiguous = true;
        }
    }

    if (ambiguous)
        throw InterpreterError("Ambiguous reference to " + id.token.lexeme);
    return best;
}

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

    if (auto* binding = resolveInFrame(variables, name)) {
        *binding = value;
        return;
    }

    if (parent) {
        DEBUG_LOG("  No compatible match in current Env, trying parent @ " << parent.get());
        parent->set(name, value);
        return;
    }

    DEBUG_LOG("  Error: Variable '" << name.token.lexeme << "' not found in any environment");
    throw InterpreterError("Unbound variable " + name.token.lexeme);
}

std::optional<SchemeValue> Environment::get(const HygienicSyntax& id) const
{
    std::lock_guard<std::mutex> lock(mutex);

    auto it = variables.find(id);
    if (it != variables.end())
        return it->second;

    if (auto* binding = resolveInFrame(variables, id))
        return *binding;

    if (parent)
        return parent->get(id);
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
