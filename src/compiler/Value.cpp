#include "Value.h"
#include "Interpreter.h"
#include <stdexcept>

SchemeValue::SchemeValue()
    : value(0)
{
}

SchemeValue::SchemeValue(Value v)
    : value(std::move(v))
{
}

bool SchemeValue::isProc() const
{
    return std::holds_alternative<std::shared_ptr<Procedure>>(value);
}

bool SchemeValue::isSymbol() const
{
    return std::holds_alternative<Symbol>(value);
}

SchemeValue SchemeValue::call(Interpreter& interp, const std::vector<SchemeValue>& args) const
{
    if (isProc()) {
        return (*std::get<std::shared_ptr<Procedure>>(value))(interp, args);
    }
    throw std::runtime_error("Attempt to call non-procedure value");
}

bool SchemeValue::isTrue() const
{
    return std::visit(overloaded {
                          [](int arg) { return arg != 0; },
                          [](double arg) { return arg != 0.0; },
                          [](bool arg) { return arg; },
                          [](const std::string& arg) { return !arg.empty(); },
                          [](const Symbol& arg) { return true; },
                          [](const std::vector<SchemeValue>& arg) { return !arg.empty(); },
                          [](const std::shared_ptr<Procedure>&) { return true; } },
        value);
}

std::string SchemeValue::toString() const
{
    return std::visit(overloaded {
                          [](int arg) { return std::to_string(arg); },
                          [](double arg) { return std::to_string(arg); },
                          [](bool arg) { return arg ? std::string("#t") : "#f"; },
                          [](const std::string& arg) { return std::string("\"" + arg + "\""); },
                          [](const Symbol& arg) { return arg.name; },
                          [](const std::vector<SchemeValue>& arg) {
                              std::string result = "(";
                              for (size_t i = 0; i < arg.size(); ++i) {
                                  if (i > 0)
                                      result += " ";
                                  result += arg[i].toString();
                              }
                              result += ")";
                              return result;
                          },
                          [](const std::shared_ptr<Procedure>&) { return std::string("<procedure>"); } },
        value);
}

SchemeValue SchemeValue::operator+(const SchemeValue& other) const
{
    return std::visit(overloaded {
                          [](int a, int b) { return SchemeValue(a + b); },
                          [](int a, double b) { return SchemeValue(static_cast<double>(a) + b); },
                          [](double a, int b) { return SchemeValue(a + static_cast<double>(b)); },
                          [](double a, double b) { return SchemeValue(a + b); },
                          [](const std::string& a, const std::string& b) { return SchemeValue(a + b); },
                          [](const auto&, const auto&) -> SchemeValue {
                              throw std::runtime_error("Invalid types for addition");
                          } },
        value, other.value);
}

std::string SchemeValue::asSymbol() const
{
    if (const auto* sym = std::get_if<Symbol>(&value)) {
        return sym->name;
    }
    throw std::runtime_error("Value is not a symbol");
}
