#include "Environment.h"
#include <iostream>
#include <stdexcept>

void Environment::pushFrame()
{
    frames.push_front(std::make_shared<Frame>());
    std::cout << "Pushing frame now at " << frames.size() << std::endl;
}

void Environment::popFrame()
{
    if (frames.size() <= 1) {
        throw std::runtime_error("Cannot pop global frame");
    }
    frames.pop_front();
    std::cout << "Popping frame now at " << frames.size() << std::endl;
}

void Environment::define(const std::string& name, const SchemeValue& value)
{
    std::cout << "Defining " << name << " in top frame" << std::endl;
    frames.front()->bound[name] = value;
}

std::optional<SchemeValue> Environment::get(const std::string& name) const
{
    for (const auto& frame : frames) {
        auto it = frame->bound.find(name);
        if (it != frame->bound.end()) {
            std::cout << "Found " << name << " in frame" << std::endl;
            return it->second;
        }
    }
    if (enclosing) {
        std::cout << "Looking in enclosing environment for " << name << std::endl;
        return enclosing->get(name);
    }

    std::cout << "Failed to find " << name << std::endl;
    return std::nullopt;
}

void Environment::set(const std::string& name, const SchemeValue& value)
{
    auto currentFrame = frames.front();
    auto it = currentFrame->bound.find(name);
    if (it != currentFrame->bound.end()) {
        std::cout << "Setting " << name << " in current frame" << std::endl;
        it->second = value;
        return;
    }
    if (enclosing) {
        auto found = enclosing->get(name);
        if (found) {
            std::cout << "Setting " << name << " in enclosing environment" << std::endl;
            enclosing->set(name, value);
            return;
        }
    }

    std::cout << "Creating new binding for " << name << " in current frame" << std::endl;
    currentFrame->bound[name] = value;
}

void Environment::printCurrentFrame() const
{
    std::cout << "Frame contents:" << std::endl;
    for (const auto& [name, value] : frames.front()->bound) {
        std::cout << "  " << name << " = " << value.toString() << std::endl;
    }
}
