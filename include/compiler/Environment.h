#pragma once
#include "Value.h"
#include <iostream>
#include <stdexcept>
#include <unordered_map>
class Environment {
public:
    struct Frame {
        std::unordered_map<std::string, SchemeValue> bound;
    };
    Environment(std::shared_ptr<Environment> enclosing = nullptr)
        : frames({ std::make_shared<Frame>() })
        , enclosing(enclosing)
    {
    }
    void replaceFrame()
    {
        if (frames.size() > 1) {
            frames.back().reset(new Frame());
        } else {
            throw std::runtime_error("Cannot replace the global frame");
        }
    }
    void pushFrame();
    void popFrame();
    void define(const std::string& name, const SchemeValue& value);
    std::optional<SchemeValue> get(const std::string& name) const;
    void set(const std::string& name, const SchemeValue& value);

private:
    std::list<std::shared_ptr<Frame>> frames;
    std::shared_ptr<Environment> enclosing;
};
