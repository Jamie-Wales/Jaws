#pragma once
#include "Value.h"
#include <memory>
#include <optional>
#include <string>
#include <tbb/concurrent_unordered_map.h>
#include <iostream>

class Environment : public std::enable_shared_from_this<Environment> {
private:
    std::shared_ptr<Environment> parent;
    tbb::concurrent_unordered_map<std::string, SchemeValue> variables;

public:
    Environment();
    Environment(std::shared_ptr<Environment> parent);
    void define(const std::string& name, const SchemeValue& value);
    void set(const std::string& name, const SchemeValue& value);
    std::optional<SchemeValue> get(const std::string& name) const;
    std::shared_ptr<Environment> extend();
    std::shared_ptr<Environment> copy() const;
    void printEnv() const;
    std::shared_ptr<Environment> getParent() const { return parent; }
};
