#include <Value.h>
#include <stdexcept>
#include <string>
#include <variant>
#include <visit.h>

std::string SchemeValue::toString() const
{
    return std::visit(overloaded {
                          [](int arg) { return std::to_string(arg); },
                          [](double arg) { return std::to_string(arg); },
                          [](const std::string& arg) { return "\"" + arg + "\""; },
                          [](bool arg) { return arg ? std::string("#t") : std::string("#f"); },
                          [](const std::vector<SchemeValue>& arg) {
                              std::string result = "(";
                              for (size_t i = 0; i < arg.size(); ++i) {
                                  if (i > 0)
                                      result += " ";
                                  result += arg[i].toString();
                              }
                              return result + ")";
                          } },
        value);
}

bool SchemeValue::isTrue() const
{
    return std::visit(overloaded {
                          [](int arg) { return arg != 0; },
                          [](double arg) { return arg != 0.0; },
                          [](const std::string& arg) { return !arg.empty(); },
                          [](bool arg) { return arg; },
                          [](const std::vector<SchemeValue>& arg) { return !arg.empty(); } },
        value);
}

SchemeValue SchemeValue::operator+(const SchemeValue& other) const
{
    return std::visit(overloaded {
                          [](int a, int b) -> SchemeValue { return { a + b }; },
                          [](int a, double b) -> SchemeValue { return { a + b }; },
                          [](double a, int b) -> SchemeValue { return { a + b }; },
                          [](double a, double b) -> SchemeValue { return { a + b }; },
                          [](const std::string& a, const std::string& b) -> SchemeValue { return { a + b }; },
                          [](const auto&, const auto&) -> SchemeValue {
                              throw std::runtime_error("Invalid types for addition");
                          } },
        value, other.value);
}
