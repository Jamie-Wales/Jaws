#include "MacroEnvironment.h"
#include <iostream>

// #define DEBUG_LOGGING
#ifdef DEBUG_LOGGING
#define DEBUG_LOG(x) std::cerr << "[DEBUG] " << x << std::endl
#else
#define DEBUG_LOG(x)
#endif

namespace pattern {

MacroEnvironment::MacroEnvironment()
    : parent(nullptr)
{
}

MacroEnvironment::MacroEnvironment(std::shared_ptr<MacroEnvironment> parent)
    : parent(parent)
{
}

void MacroEnvironment::defineMacro(const std::string& name, std::shared_ptr<Expression> macro)
{
    macros[name] = std::move(macro);
}

std::optional<std::shared_ptr<Expression>> MacroEnvironment::getMacroDefinition(const std::string& name) const
{
    DEBUG_LOG("Looking for macro: " << name << " in environment " << this)
    auto it = macros.find(name);
    if (it != macros.end()) {
        DEBUG_LOG("Found macro: " << name << " in environment " << this)
        return it->second;
    }
    if (parent) {
        DEBUG_LOG("Checking parent environment " << parent.get() << " for macro: " << name)
        return parent->getMacroDefinition(name);
    }
    DEBUG_LOG("Macro not found: " << name)
    return std::nullopt;
}

bool MacroEnvironment::isMacro(const std::string& name) const
{
    return getMacroDefinition(name).has_value();
}

std::shared_ptr<MacroEnvironment> MacroEnvironment::extend()
{
    return std::make_shared<MacroEnvironment>(shared_from_this());
}

}
