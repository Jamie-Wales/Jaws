#pragma once
#include <map>
#include <memory>
#include <optional>
#include <string>

// Forward declaration
struct Expression;

namespace pattern {

class MacroEnvironment : public std::enable_shared_from_this<MacroEnvironment> {
public:
    MacroEnvironment();
    MacroEnvironment(std::shared_ptr<MacroEnvironment> parent);

    void defineMacro(const std::string& name, std::shared_ptr<Expression> macro);

    bool isMacro(const std::string& name) const;
    std::optional<std::shared_ptr<Expression>> getMacroDefinition(const std::string& name) const;

    std::shared_ptr<MacroEnvironment> extend();

private:
    std::shared_ptr<MacroEnvironment> parent;
    std::map<std::string, std::shared_ptr<Expression>> macros;
};

}
