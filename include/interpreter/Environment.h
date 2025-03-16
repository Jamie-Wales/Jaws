#pragma once
#include "Value.h"
#include <deque>
#include <memory>
#include <optional>
#include <string>
#include <unordered_map>

struct Frame {
    std::unordered_map<std::string, SchemeValue> bound;
};

class Environment {
public:
    std::shared_ptr<Environment> enclosing;
    std::deque<std::shared_ptr<Frame>> frames;
    Environment();
    Environment(std::shared_ptr<Environment> parent);
    void pushFrame();
    void popFrame();
    void define(const std::string& name, const SchemeValue& value);
    void set(const std::string& name, const SchemeValue& value);
    std::optional<SchemeValue> get(const std::string& name) const;
    std::shared_ptr<Environment> copy() const;
};
