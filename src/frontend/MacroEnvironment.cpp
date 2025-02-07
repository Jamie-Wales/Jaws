#include "MacroEnvironment.h"

namespace pattern {

void MacroEnvironment::defineMacro(const std::string& name, std::shared_ptr<Expression> macro)
{
    macros[name] = std::move(macro);
}

bool MacroEnvironment::isMacro(const std::string& name) const
{
    return macros.find(name) != macros.end();
}

std::optional<std::shared_ptr<Expression>> MacroEnvironment::getMacroDefinition(const std::string& name) const
{
    auto it = macros.find(name);
    if (it != macros.end()) {
        return it->second;
    }
    return std::nullopt;
}

} // namespace pattern
