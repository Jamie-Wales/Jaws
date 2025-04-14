// Value.h
#pragma once
#include "MultiValue.h"
#include "Number.h"
#include "Port.h"
#include "Thread.h"
#include "ValueTraits.h"
#include <compare>
#include <list>
#include <memory>
#include <stdexcept>
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
        std::shared_ptr<std::list<SchemeValue>>,
        std::shared_ptr<std::vector<SchemeValue>>,
        std::shared_ptr<Procedure>,
        Port,
        std::shared_ptr<ThreadHandle>,
        std::shared_ptr<MutexHandle>,
        std::shared_ptr<ConditionVarHandle>,
        std::shared_ptr<Expression>,
        std::shared_ptr<MultiValue>,
        char>;

    SchemeValue(SchemeValue&& other) noexcept
        : value(std::move(other.value))
    {
    }

    std::shared_ptr<std::vector<SchemeValue>> asSharedVector() const;
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

    bool isChar() const
    {

        return std::holds_alternative<char>(value);
    }

    bool asChar() const
    {

        if (isChar())
            return std::get<char>(value);

        throw std::runtime_error("Invalid call to convert char");
    }
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
    std::shared_ptr<std::list<SchemeValue>> asList() const;

    std::shared_ptr<std::list<SchemeValue>> asSharedList() const;
    bool isThread() const
    {
        return std::holds_alternative<std::shared_ptr<ThreadHandle>>(value);
    }

    bool isMutex() const
    {
        return std::holds_alternative<std::shared_ptr<MutexHandle>>(value);
    }

    bool isConditionVar() const
    {
        return std::holds_alternative<std::shared_ptr<ConditionVarHandle>>(value);
    }
    std::shared_ptr<ThreadHandle> asThread() const
    {
        if (!isThread()) {
            throw std::runtime_error("Value is not a thread");
        }
        return std::get<std::shared_ptr<ThreadHandle>>(value);
    }

    std::shared_ptr<MutexHandle> asMutex() const
    {
        if (!isMutex()) {
            throw std::runtime_error("Value is not a mutex");
        }
        return std::get<std::shared_ptr<MutexHandle>>(value);
    }

    std::shared_ptr<ConditionVarHandle> asConditionVar() const
    {
        if (!isConditionVar()) {
            throw std::runtime_error("Value is not a condition variable");
        }
        return std::get<std::shared_ptr<ConditionVarHandle>>(value);
    }
    std::string asSymbol() const;
    bool isTrue() const;

    std::string toString() const;

    bool isMultiValue() const
    {
        return std::holds_alternative<std::shared_ptr<MultiValue>>(value);
    }

    std::shared_ptr<MultiValue> asMultiValue() const
    {
        if (!isMultiValue()) {
            throw std::runtime_error("Value is not a multiple values object");
        }
        return std::get<std::shared_ptr<MultiValue>>(value);
    }
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
std::shared_ptr<Expression> valueToExpression(const SchemeValue& val);
SchemeValue ensureSchemeValue(const SchemeValue& val);
