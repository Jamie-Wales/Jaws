#include "Value.h"
#include "Interpreter.h"
#include "Procedure.h"
#include "Visit.h"
#include <stdexcept>

SchemeValue::SchemeValue()
    : value(Number(0))
{
}

SchemeValue::SchemeValue(Value v)
    : value(std::move(v))
{
}

bool SchemeValue::isPort() const
{
    return std::holds_alternative<Port>(value);
}
bool SchemeValue::isProc() const
{
    return std::holds_alternative<std::shared_ptr<Procedure>>(value);
}

bool SchemeValue::isSymbol() const
{
    return std::holds_alternative<Symbol>(value);
}

bool SchemeValue::isNumber() const
{
    return std::holds_alternative<Number>(value);
}

std::optional<SchemeValue> SchemeValue::call(Interpreter& interp, const std::vector<SchemeValue>& args) const
{
    if (isProc()) {
        auto ele = std::get<std::shared_ptr<Procedure>>(value);
        if (ele)
            return (*ele)(interp, args);
    }
    throw std::runtime_error("Attempt to call non-procedure value");
}
bool SchemeValue::isTrue() const
{
    return std::visit(overloaded {
                          [](const Number& arg) { return !arg.isZero(); },
                          [](const std::string& arg) { return !arg.empty(); },
                          [](bool arg) { return arg; },
                          [](const Symbol&) { return true; },
                          [](const std::vector<SchemeValue>& arg) { return !arg.empty(); },
                          [](const std::shared_ptr<Procedure>&) { return true; },
                          [](const Port& p) { return p.isOpen(); } },
        value);
}

std::string SchemeValue::toString() const
{
    return std::visit(overloaded {
                          [](const Number& arg) { return arg.toString(); },
                          [](const std::string& arg) { return std::string("\"" + arg + "\""); },
                          [](bool arg) { return arg ? std::string("#t") : "#f"; },
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
                          [](const std::shared_ptr<Procedure>&) { return std::string("<procedure>"); },
                          [](const Port& p) {
                              return std::string(p.type == PortType::Input ? "<input-port>" : "<output-port>");
                          } },
        value);
}
SchemeValue SchemeValue::operator+(const SchemeValue& other) const
{
    return std::visit(overloaded {
                          [](const Number& a, const Number& b) {
                              return SchemeValue(a + b);
                          },
                          [](const std::string& a, const std::string& b) {
                              return SchemeValue(a + b);
                          },
                          [](const auto&, const auto&) -> SchemeValue {
                              throw std::runtime_error("Invalid types for addition");
                          } },
        value, other.value);
}

SchemeValue SchemeValue::operator-(const SchemeValue& other) const
{
    return std::visit(overloaded {
                          [](const Number& a, const Number& b) {
                              return SchemeValue(a - b);
                          },
                          [](const auto&, const auto&) -> SchemeValue {
                              throw std::runtime_error("Invalid types for subtraction");
                          } },
        value, other.value);
}

SchemeValue SchemeValue::operator*(const SchemeValue& other) const
{
    return std::visit(overloaded {
                          [](const Number& a, const Number& b) {
                              return SchemeValue(a * b);
                          },
                          [](const auto&, const auto&) -> SchemeValue {
                              throw std::runtime_error("Invalid types for multiplication");
                          } },
        value, other.value);
}

SchemeValue SchemeValue::operator/(const SchemeValue& other) const
{
    return std::visit(overloaded {
                          [](const Number& a, const Number& b) {
                              if (b.isZero()) {
                                  throw std::runtime_error("Division by zero");
                              }
                              return SchemeValue(a / b);
                          },
                          [](const auto&, const auto&) -> SchemeValue {
                              throw std::runtime_error("Invalid types for division");
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

SchemeValue SchemeValue::operator-() const
{
    return std::visit(overloaded {
                          [](const Number& n) { return SchemeValue(-n); },
                          [](const auto&) -> SchemeValue {
                              throw std::runtime_error("Unary minus not supported for this type");
                          } },
        value);
}

std::partial_ordering SchemeValue::operator<=>(const SchemeValue& other) const
{
    return std::visit(overloaded {
                          [](const Number& a, const Number& b) -> std::partial_ordering {
                              try {
                                  return a <=> b;
                              } catch (const std::runtime_error&) {
                                  return std::partial_ordering::unordered;
                              }
                          },

                          [](const std::string& a, const std::string& b) -> std::partial_ordering {
                              if (auto cmp = a <=> b; cmp != 0) {
                                  return cmp < 0 ? std::partial_ordering::less : cmp > 0 ? std::partial_ordering::greater
                                                                                         : std::partial_ordering::equivalent;
                              }
                              return std::partial_ordering::equivalent;
                          },

                          [](bool a, bool b) -> std::partial_ordering {
                              if (a == b)
                                  return std::partial_ordering::equivalent;
                              return a < b ? std::partial_ordering::less : std::partial_ordering::greater;
                          },

                          [](const Symbol& a, const Symbol& b) -> std::partial_ordering {
                              if (auto cmp = a.name <=> b.name; cmp != 0) {
                                  return cmp < 0 ? std::partial_ordering::less : cmp > 0 ? std::partial_ordering::greater
                                                                                         : std::partial_ordering::equivalent;
                              }
                              return std::partial_ordering::equivalent;
                          },

                          [](const std::vector<SchemeValue>& a, const std::vector<SchemeValue>& b) -> std::partial_ordering {
                              auto minSize = std::min(a.size(), b.size());
                              for (size_t i = 0; i < minSize; ++i) {
                                  if (auto cmp = a[i] <=> b[i]; cmp != std::partial_ordering::equivalent) {
                                      return cmp;
                                  }
                              }
                              if (a.size() < b.size())
                                  return std::partial_ordering::less;
                              if (a.size() > b.size())
                                  return std::partial_ordering::greater;
                              return std::partial_ordering::equivalent;
                          },
                          [](const std::shared_ptr<Procedure>&, const std::shared_ptr<Procedure>&) -> std::partial_ordering {
                              return std::partial_ordering::unordered;
                          },
                          [](const Port&, const Port&) -> std::partial_ordering {
                              return std::partial_ordering::unordered;
                          },

                          [](const auto& a, const auto& b) -> std::partial_ordering {
                              auto typeOrder = [](const auto& val) -> int {
                                  using T = std::decay_t<decltype(val)>;
                                  if constexpr (std::is_same_v<T, bool>)
                                      return 0;
                                  else if constexpr (std::is_same_v<T, Number>)
                                      return 1;
                                  else if constexpr (std::is_same_v<T, Symbol>)
                                      return 2;
                                  else if constexpr (std::is_same_v<T, std::string>)
                                      return 3;
                                  else if constexpr (std::is_same_v<T, std::vector<SchemeValue>>)
                                      return 4;
                                  else if constexpr (std::is_same_v<T, std::shared_ptr<Procedure>>)
                                      return 5;
                                  else if constexpr (std::is_same_v<T, Port>)
                                      return 6;
                                  else
                                      return 7;
                              };

                              int orderA = typeOrder(a);
                              int orderB = typeOrder(b);
                              if (orderA < orderB)
                                  return std::partial_ordering::less;
                              if (orderA > orderB)
                                  return std::partial_ordering::greater;
                              return std::partial_ordering::equivalent;
                          } },
        value, other.value);
}

bool SchemeValue::operator==(const SchemeValue& other) const
{
    return std::visit(overloaded {
                          [](const Number& a, const Number& b) -> bool {
                              try {
                                  return (a <=> b) == std::partial_ordering::equivalent;
                              } catch (const std::runtime_error&) {
                                  return false;
                              }
                          },

                          [](const std::string& a, const std::string& b) -> bool {
                              return a == b;
                          },

                          [](bool a, bool b) -> bool {
                              return a == b;
                          },

                          [](const Symbol& a, const Symbol& b) -> bool {
                              return a.name == b.name;
                          },

                          [](const std::vector<SchemeValue>& a, const std::vector<SchemeValue>& b) -> bool {
                              if (a.size() != b.size())
                                  return false;
                              for (size_t i = 0; i < a.size(); ++i) {
                                  if (!(a[i] == b[i]))
                                      return false;
                              }
                              return true;
                          },

                          [](const std::shared_ptr<Procedure>& a, const std::shared_ptr<Procedure>& b) -> bool {
                              return a == b;
                          },

                          [](const auto&, const auto&) -> bool {
                              return false;
                          } },
        value, other.value);
}
