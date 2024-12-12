// Value.h
#pragma once
#include "Error.h"
#include "Number.h"
#include "Port.h"
#include "ValueTraits.h"
#include <compare>
#include <list>
#include <memory>
#include <string>
#include <variant>
#include <vector>

class Procedure;
class Interpreter;
class Expression;

class SchemeValue {
public:
    using Value = std::variant<
        Number,
        bool,
        std::string,
        Symbol,
        std::list<SchemeValue>,
        std::vector<SchemeValue>,
        std::shared_ptr<Procedure>,
        Port,
        std::shared_ptr<Expression>>;
    SchemeValue(SchemeValue&& other) noexcept
        : value(std::move(other.value))
    {
    }
    SchemeValue ensureValue() const
    {
        return std::visit([](const auto& val) -> SchemeValue {
            return ValueTrait<std::decay_t<decltype(val)>>::toValue(val);
        },
            value);
    }
    SchemeValue& operator=(SchemeValue&& other) noexcept
    {
        value = std::move(other.value);
        return *this;
    }
    SchemeValue(const SchemeValue& other)
        : value(other.value)
    {
    }

    SchemeValue& operator=(const SchemeValue& other)
    {
        if (this != &other) {
            value = other.value;
        }
        return *this;
    }
    SchemeValue();
    explicit SchemeValue(Value v);

    bool isPort() const;
    bool isNumber() const;
    Number asNumber() const;
    bool isSymbol() const;
    bool isProc() const;
    bool isExpr() const;
    bool isVector() const;
    std::shared_ptr<Expression> asExpr() const;
    std::shared_ptr<Procedure> asProc() const;
    bool isList() const;
    const std::list<SchemeValue>& asList() const;
    std::list<SchemeValue>& asList();

    std::string asSymbol() const;
    bool isTrue() const;

    std::string toString() const;

    SchemeValue operator+(const SchemeValue& other) const;
    SchemeValue operator-(const SchemeValue& other) const;
    SchemeValue operator*(const SchemeValue& other) const;
    SchemeValue operator/(const SchemeValue& other) const;
    SchemeValue operator-() const;

    std::partial_ordering operator<=>(const SchemeValue& other) const;
    bool operator==(const SchemeValue& other) const;

    template <typename T>
    T as() const
    {
        return std::get<T>(value);
    }
    template <typename T>
    bool isValue() const
    {
        return std::holds_alternative<T>(value);
    }
    Value value;
    template <typename T>
    T getValue() const
    {
        if (std::holds_alternative<T>(value)) {
            return std::get<T>(value);
        } else {
            throw std::runtime_error("Incorrect type for getValue");
        }
    }
};

void checkArgCount(const std::vector<SchemeValue>& args, size_t expected, const char* name);
SchemeValue expressionToValue(const Expression& expr);
std::shared_ptr<Expression> valueToExpression(const SchemeValue& val, Interpreter& interp);
SchemeValue ensureSchemeValue(const SchemeValue& val);
