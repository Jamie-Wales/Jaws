#include "MacroEnvironment.h"
// No need for #include <mutex> anymore

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
    auto it = macros.find(name);
    if (it != macros.end()) {
        return it->second;
    }
    if (parent) {
        return parent->getMacroDefinition(name);
    }
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
