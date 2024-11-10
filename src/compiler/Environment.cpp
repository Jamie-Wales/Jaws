#include "Environment.h"
#include <iostream>
void Environment::pushFrame()
{
    frames.push_front(std::make_shared<Frame>());
    std::cout << "Pushing frame now at " << frames.size() << std::endl;
}

void Environment::popFrame()
{
    frames.pop_front();
    std::cout << "Popping frame now at " << frames.size() << std::endl;
}
void Environment::define(const std::string& name, const SchemeValue& value)
{
    frames.front().get()->bound[name] = value;
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
void Environment::set(const std::string& name, const SchemeValue& value)
{
    auto currentFrame = frames.front().get();
    auto it = currentFrame->bound.find(name);
    if (it != currentFrame->bound.end()) {
        it->second = value;
        return;
    }

    if (enclosing) {
        auto found = enclosing->get(name);
        if (found) {
            enclosing->set(name, value);
            return;
        }
    }

    currentFrame->bound[name] = value;
}
