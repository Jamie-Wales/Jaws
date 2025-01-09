#pragma once
#include "Visit.h"
#include <fstream>
#include <memory>
#include <sstream>
#include <variant>

enum class PortType {
    Input,
    Output
};

class Port {
public:
    std::variant<std::shared_ptr<std::fstream>, std::shared_ptr<std::stringstream>> stream;
    PortType type;

    // Constructor for file streams
    Port(std::shared_ptr<std::fstream> f, PortType t)
        : stream(std::move(f))
        , type(t)
    {
    }

    // Constructor for string streams
    explicit Port(const std::string& s)
        : stream(std::make_shared<std::stringstream>(s))
        , type(PortType::Input)
    {
    }

    explicit Port(std::stringstream&& ss)
        : stream(std::make_shared<std::stringstream>(std::move(ss)))
        , type(PortType::Input)
    {
    }

    bool isOpen() const
    {
        return std::visit(overloaded {
                              [](const std::shared_ptr<std::fstream>& f) { return f && f->is_open(); },
                              [](const std::shared_ptr<std::stringstream>&) { return true; } },
            stream);
    }

    void close() const
    {
        std::visit(overloaded {
                       [](const std::shared_ptr<std::fstream>& f) {
                           if (f && f->is_open())
                               f->close();
                       },
                       [](const std::shared_ptr<std::stringstream>&) { /* Nothing to close */ } },
            stream);
    }

    std::istream* get() const
    {
        return std::visit(overloaded {
                              [](const std::shared_ptr<std::fstream>& f) -> std::istream* { return f.get(); },
                              [](const std::shared_ptr<std::stringstream>& s) -> std::istream* { return s.get(); } },
            stream);
    }

    std::ostream* getOutput() const
    {
        return std::visit(overloaded {
                              [](const std::shared_ptr<std::fstream>& f) -> std::ostream* { return f.get(); },
                              [](const std::shared_ptr<std::stringstream>& s) -> std::ostream* { return s.get(); } },
            stream);
    }

    PortType getType() const { return type; }
};
