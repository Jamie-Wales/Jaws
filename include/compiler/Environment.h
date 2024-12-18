#pragma once
#include "Value.h"
#include <list>
#include <memory>
#include <unordered_map>

class Environment {
public:
    struct Frame {
        std::unordered_map<std::string, SchemeValue> bound;
    };

    Environment()
        : frames({ std::make_shared<Frame>() })
        , enclosing(nullptr)
    {
        pushFrame();
    }

    Environment(const Environment& other)
        : frames(other.frames)
        , enclosing(other.enclosing)
    {
    }

    Environment(std::shared_ptr<Frame> capturedFrame, std::shared_ptr<Environment> parent)
        : frames({ capturedFrame })
        , enclosing(parent)
    {
    }

    void printCurrentFrame() const;
    size_t getFrameCount() const { return frames.size(); }
    std::shared_ptr<Frame> getCurrentFrame() const { return frames.front(); }
    void define(const std::string& name, const SchemeValue& value);
    std::optional<SchemeValue> get(const std::string& name) const;
    void set(const std::string& name, const SchemeValue& value);
    void pushFrame();
    void popFrame();

private:
    std::list<std::shared_ptr<Frame>> frames;
    std::shared_ptr<Environment> enclosing;
};
