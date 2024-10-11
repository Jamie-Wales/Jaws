#pragma once
#include <string>
#include <variant>
#include <vector>
#include <stdexcept>

class SchemeValue {
public:
    using Value = std::variant<int, double, std::string, bool, std::vector<SchemeValue>>;
    Value value;

    SchemeValue() : value(0) {} 
    SchemeValue(Value v) : value(std::move(v)) {}
    SchemeValue(const SchemeValue&) = default;
    SchemeValue(SchemeValue&&) noexcept = default;
    SchemeValue& operator=(const SchemeValue&) = default;
    SchemeValue& operator=(SchemeValue&&) noexcept = default;
    std::string toString() const;
    bool isTrue() const;
    SchemeValue operator+(const SchemeValue& other) const;
    template <typename T>
    T as() const
    {
        if (std::holds_alternative<T>(value)) {
            return std::get<T>(value);
        }
        throw std::runtime_error("Type mismatch in SchemeValue");
    }
};
