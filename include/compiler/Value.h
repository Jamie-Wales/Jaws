// Value.h
#pragma once
#include "Number.h"
#include "Parser.h"
#include "Port.h"
#include "Scanner.h"
#include <compare>
#include <list>
#include <memory>
#include <optional>
#include <string>
#include <variant>
#include <vector>

class Procedure;
class Interpreter;
class Expression;

struct Symbol {
    std::string name;
    explicit Symbol(std::string n)
        : name(std::move(n))
    {
    }
    bool operator==(const Symbol& other) const { return name == other.name; }
};

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

SchemeValue expressionToValue(const Expression& expr);
std::shared_ptr<Expression> valueToExpression(const SchemeValue& val, Interpreter& interp);
