#pragma once
#include "Syntax.h"
#include "Value.h"
#include <iostream>
#include <memory>
#include <mutex>
#include <optional>
#include <string>
#include <unordered_map>

class Environment : public std::enable_shared_from_this<Environment> {
private:
    std::shared_ptr<Environment> parent;
    std::unordered_map<HygienicSyntax, SchemeValue> variables;
    mutable std::mutex mutex;

public:
    Environment();
    Environment(std::shared_ptr<Environment> parent);
    void define(const HygienicSyntax& name, const SchemeValue& value);
    void set(const HygienicSyntax& name, const SchemeValue& value);
    std::optional<SchemeValue> get(const HygienicSyntax& name) const;
    std::shared_ptr<Environment> extend();
    std::shared_ptr<Environment> copy() const;
    void printEnv() const;
    std::shared_ptr<Environment> getParent() const { return parent; }
};
