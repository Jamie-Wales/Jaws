#include "Environment.h"
#include <iostream>

void Environment::pushFrame()
{
    frames.push_front(std::make_shared<Frame>());
}

void Environment::popFrame()
{
    if (frames.empty()) {
        throw InterpreterError("Trying to pop from empty environment");
    }
    frames.pop_front();
}

Environment::Environment()
    : enclosing(nullptr)
{
    pushFrame();
}

Environment::Environment(std::shared_ptr<Environment> parent)
    : enclosing(parent)
{
}

void Environment::define(const std::string& name, const SchemeValue& value)
{
    frames.front()->bound[name] = value;
}

void Environment::set(const std::string& name, const SchemeValue& value)
{
    for (auto& frame : frames) {
        auto it = frame->bound.find(name);
        if (it != frame->bound.end()) {
            it->second = value;
            return;
        }
    }

    if (enclosing) {
        enclosing->set(name, value);
        return;
    }

    frames.front()->bound[name] = value;
}

std::optional<SchemeValue> Environment::get(const std::string& name) const
{
    for (const auto& frame : frames) {
        auto it = frame->bound.find(name);
        if (it != frame->bound.end()) {
            return it->second;
        }
    }

    if (enclosing) {
        return enclosing->get(name);
    }

    return std::nullopt;
}
