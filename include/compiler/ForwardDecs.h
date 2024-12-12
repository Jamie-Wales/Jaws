#pragma once
#include <string>
class Expression;
class SchemeValue;
class Interpreter;
class Procedure;

struct Symbol {
    std::string name;
    explicit Symbol(std::string n)
        : name(std::move(n))
    {
    }
    bool operator==(const Symbol& other) const { return name == other.name; }
};
