#include "Value.h"
#include "Interpreter.h"
#include "Procedure.h"
#include "Visit.h"
#include <stdexcept>

SchemeValue expressionToValue(const Expression& expr)
{
    return std::visit(overloaded {

                          [](const QuoteExpression& a) -> SchemeValue {
                              return SchemeValue(a.expression);
                          },

                          [](const AtomExpression& a) -> SchemeValue {
                              switch (a.value.type) {
                              case Tokentype::IDENTIFIER:
                              case Tokentype::SYMBOL:
                              case Tokentype::QUOTE_SYMBOL:
                                  return SchemeValue(Symbol { a.value.lexeme });

                              case Tokentype::INTEGER: {
                                  return SchemeValue(Number(std::stoi(a.value.lexeme)));
                              }

                              case Tokentype::FLOAT: {
                                  return SchemeValue(Number(std::stod(a.value.lexeme)));
                              }

                              case Tokentype::RATIONAL: {
                                  size_t slashPos = a.value.lexeme.find('/');
                                  if (slashPos != std::string::npos) {
                                      int num = std::stoi(a.value.lexeme.substr(0, slashPos));
                                      int den = std::stoi(a.value.lexeme.substr(slashPos + 1));
                                      return SchemeValue(Number(Number::Rational(num, den)));
                                  }
                                  throw std::runtime_error("Invalid rational format");
                              }

                              case Tokentype::COMPLEX: {
                                  size_t plusPos = a.value.lexeme.find('+');
                                  size_t iPos = a.value.lexeme.find('i');
                                  if (plusPos != std::string::npos && iPos != std::string::npos) {
                                      double real = std::stod(a.value.lexeme.substr(0, plusPos));
                                      double imag = std::stod(a.value.lexeme.substr(plusPos + 1, iPos - plusPos - 1));
                                      return SchemeValue(Number(Number::ComplexType(real, imag)));
                                  }
                                  throw std::runtime_error("Invalid complex format");
                              }

                              case Tokentype::STRING:
                                  return SchemeValue(a.value.lexeme);

                              case Tokentype::TRUE:
                                  return SchemeValue(true);

                              case Tokentype::FALSE:
                                  return SchemeValue(false);

                              case Tokentype::PLUS:
                                  return SchemeValue(Symbol { "+" });
                              case Tokentype::MINUS:
                                  return SchemeValue(Symbol { "-" });
                              case Tokentype::MULTIPLY:
                                  return SchemeValue(Symbol { "*" });
                              case Tokentype::DIVIDE:
                                  return SchemeValue(Symbol { "/" });
                              case Tokentype::EQUAL:
                                  return SchemeValue(Symbol { "=" });
                              case Tokentype::LESS_THAN:
                                  return SchemeValue(Symbol { "<" });
                              case Tokentype::GREATER_THAN:
                                  return SchemeValue(Symbol { ">" });
                              case Tokentype::CONS:
                                  return SchemeValue(Symbol { "cons" });
                              case Tokentype::CAR:
                                  return SchemeValue(Symbol { "car" });
                              case Tokentype::DEFINE:
                                  return SchemeValue(Symbol { "define" });
                              case Tokentype::LAMBDA:
                                  return SchemeValue(Symbol { "lambda" });
                              case Tokentype::IF:
                                  return SchemeValue(Symbol { "if" });
                              case Tokentype::QUOTE:
                                  return SchemeValue(Symbol { "quote" });

                              default:
                                  throw std::runtime_error("Unexpected token type in expressionToValue: " + a.value.lexeme);
                              }
                          },

                          [](const sExpression& s) -> SchemeValue {
                              std::list<SchemeValue> values;
                              for (const auto& elem : s.elements) {
                                  values.push_back(expressionToValue(*elem));
                              }
                              return SchemeValue(std::move(values));
                          },

                          [](const VectorExpression& v) -> SchemeValue {
                              std::vector<SchemeValue> values;
                              values.reserve(v.elements.size());
                              for (const auto& elem : v.elements) {
                                  values.push_back(expressionToValue(*elem));
                              }
                              return SchemeValue(std::move(values));
                          },

                          [](const DefineExpression& d) -> SchemeValue {
                              std::list<SchemeValue> values;
                              values.push_back(SchemeValue(Symbol { "define" }));
                              values.push_back(SchemeValue(Symbol { d.name.lexeme }));
                              values.push_back(expressionToValue(*d.value));
                              return SchemeValue(std::move(values));
                          },

                          [](const DefineProcedure& d) -> SchemeValue {
                              std::list<SchemeValue> values;
                              values.push_back(SchemeValue(Symbol { "define" }));

                              std::list<SchemeValue> params;
                              params.push_back(SchemeValue(Symbol { d.name.lexeme }));
                              for (const auto& param : d.parameters) {
                                  params.push_back(SchemeValue(Symbol { param.lexeme }));
                              }
                              values.push_back(SchemeValue(std::move(params)));
                              for (auto p : d.body) {
                                  values.push_back(SchemeValue(expressionToValue(*p)));
                              }

                              return SchemeValue(std::move(values));
                          },

                          [](const LambdaExpression& l) -> SchemeValue {
                              std::list<SchemeValue> values;
                              values.push_back(SchemeValue(Symbol { "lambda" }));

                              std::list<SchemeValue> params;
                              for (const auto& param : l.parameters) {
                                  params.push_back(SchemeValue(Symbol { param.lexeme }));
                              }
                              values.push_back(SchemeValue(std::move(params)));
                              for (auto p : l.body) {
                                  values.push_back(expressionToValue(*p));
                              }

                              return SchemeValue(std::move(values));
                          },

                          [](const IfExpression& i) -> SchemeValue {
                              std::list<SchemeValue> values;
                              values.push_back(SchemeValue(Symbol { "if" }));
                              values.push_back(expressionToValue(*i.condition));
                              values.push_back(expressionToValue(*i.then));
                              if (i.el) {
                                  values.push_back(expressionToValue(**i.el));
                              }
                              return SchemeValue(std::move(values));
                          },

                          [](const auto& a) -> SchemeValue {
                              throw std::runtime_error("Invald Expression");
                          },
                      },
        expr.as);
}
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

bool SchemeValue::isExpr() const
{
    return std::holds_alternative<std::shared_ptr<Expression>>(value);
}

bool SchemeValue::isSymbol() const
{
    return std::holds_alternative<Symbol>(value);
}

bool SchemeValue::isNumber() const
{
    return std::holds_alternative<Number>(value);
}

Number SchemeValue::asNumber() const
{
    if (!std::holds_alternative<Number>(value)) {
        throw std::runtime_error("Value is not Number");
    }
    return std::get<Number>(value);
}

std::shared_ptr<Procedure> SchemeValue::asProc() const
{
    if (!isProc()) {
        throw std::runtime_error("Value is not procedure");
    }
    return std::get<std::shared_ptr<Procedure>>(value);
}

std::shared_ptr<Expression> SchemeValue::asExpr() const
{
    if (!isExpr()) {
        throw std::runtime_error("Value is not procedure");
    }
    return std::get<std::shared_ptr<Expression>>(value);
}

bool SchemeValue::isList() const
{
    return std::holds_alternative<std::list<SchemeValue>>(value);
}

const std::list<SchemeValue>& SchemeValue::asList() const
{
    if (!isList()) {
        throw std::runtime_error("Value is not a list");
    }
    return std::get<std::list<SchemeValue>>(value);
}

std::list<SchemeValue>& SchemeValue::asList()
{
    if (!isList()) {
        throw std::runtime_error("Value is not a list");
    }
    return std::get<std::list<SchemeValue>>(value);
}
std::optional<SchemeValue> SchemeValue::call(Interpreter& interp, const std::vector<SchemeValue>& args) const
{
    if (!isProc()) {
        throw std::runtime_error("Attempt to call non-procedure value: " + toString());
    }

    auto proc = asProc();
    if (!proc) {
        throw std::runtime_error("Null procedure pointer");
    }

    std::optional<SchemeValue> result;

    std::shared_ptr<Procedure> currentProc = proc;
    std::vector<SchemeValue> currentArgs = args;

    while (true) {
        result = currentProc->operator()(interp, currentArgs);

        if (!result) {
            return std::nullopt;
        }

        if (result->isProc() && result->asProc()->isTailCall()) {
            auto tailCallProc = result->asProc();
            auto tailCall = std::dynamic_pointer_cast<TailCall>(tailCallProc);
            if (!tailCall) {
                throw std::runtime_error("Expected TailCall procedure");
            }

            currentProc = tailCall->proc;
            currentArgs = tailCall->args;
        } else {
            return result;
        }
    }
}

bool SchemeValue::isTrue() const
{
    return std::visit(overloaded {
                          [](const Number& arg) { return !arg.isZero(); },
                          [](const std::shared_ptr<Expression> e) { return true; },
                          [](const std::string& arg) { return !arg.empty(); },
                          [](bool arg) { return arg; },
                          [](const Symbol&) { return true; },
                          [](const std::list<SchemeValue> l) { return l.size() != 0; },
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
                              std::string result = "#(";
                              for (size_t i = 0; i < arg.size(); i++) {
                                  if (i > 0)
                                      result += " ";
                                  result += arg[i].toString();
                              }
                              result += ")";
                              return result;
                          },
                          [](const std::list<SchemeValue>& arg) {
                              std::string result = "(";
                              int i = 0;
                              for (auto& val : arg) {
                                  if (i > 0)
                                      result += " ";
                                  result += val.toString();
                                  i++;
                              }
                              result += ")";
                              return result;
                          },
                          [](const std::shared_ptr<Procedure>&) { return std::string("<procedure>"); },
                          [](const Port& p) {
                              return std::string(p.type == PortType::Input ? "<input-port>" : "<output-port>");
                          },
                          [](const std::shared_ptr<Expression>& e) {
                              return expressionToValue(*e).toString();
                          },
                      },
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
