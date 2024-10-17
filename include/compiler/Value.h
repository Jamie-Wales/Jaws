#pragma once
#include <memory>
#include <string>
#include <variant>
#include <vector>

class Interpreter;

class SchemeValue {
public:
    class Procedure {
    public:
        virtual SchemeValue operator()(Interpreter&, const std::vector<SchemeValue>&) const = 0;
        virtual ~Procedure() = default;
    };

    struct Symbol {
        std::string name;
        explicit Symbol(std::string n)
            : name(std::move(n))
        {
        }
    };

    using Value = std::variant<
        int,
        double,
        bool,
        std::string,
        Symbol,
        std::vector<SchemeValue>,
        std::shared_ptr<Procedure>>;

    Value value;

    SchemeValue();
    SchemeValue(Value v);

    bool isProc() const;
    bool isSymbol() const;
    SchemeValue call(Interpreter& interp, const std::vector<SchemeValue>& args) const;
    bool isTrue() const;
    std::string toString() const;
    SchemeValue operator+(const SchemeValue& other) const;
    std::string asSymbol() const;

    template <typename T>
    T as() const
    {
        if (std::holds_alternative<T>(value)) {
            return std::get<T>(value);
        }
        throw std::runtime_error("Type mismatch in SchemeValue");
    }
};
