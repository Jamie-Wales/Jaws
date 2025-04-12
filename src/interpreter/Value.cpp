#include "Value.h"
#include "Expression.h"
#include "Procedure.h"
#include "Visit.h"
#include "parse.h"
#include "scan.h"
#include <stdexcept>
#include <variant>

void checkArgCount(const std::vector<SchemeValue>& args, size_t expected, const char* name)
{
    if (args.size() != expected) {
        throw InterpreterError(std::string(name) + " requires exactly " + std::to_string(expected) + " argument(s)");
    }
}

SchemeValue ensureSchemeValue(const SchemeValue& val)
{
    return val.ensureValue();
}
std::shared_ptr<Expression> valueToExpression(const SchemeValue& value)
{
    std::string valueStr = value.toString();
    std::vector<Token> tokens = scanner::tokenize(valueStr);
    auto expressions = parse::parse(tokens);
    if (!expressions || expressions->empty()) {
        throw std::runtime_error("Failed to parse value: " + valueStr);
    }
    return (*expressions)[0];
}

SchemeValue expressionToValue(const Expression& expr)
{
    return std::visit(overloaded {
                          [&](const QuoteExpression& a) -> SchemeValue {
                              if (!a.expression)
                                  return SchemeValue(); // Handle '()
                              return SchemeValue(a.expression);
                          },
                          [&](const AtomExpression& a) -> SchemeValue {
                              switch (a.value.token.type) {
                              case Tokentype::IDENTIFIER:
                              case Tokentype::SYMBOL:
                              case Tokentype::QUOTE_SYMBOL:
                                  return SchemeValue(Symbol { a.value.token.lexeme });
                              case Tokentype::INTEGER:
                                  return SchemeValue(Number(std::stoi(a.value.token.lexeme)));
                              case Tokentype::FLOAT:
                                  return SchemeValue(Number(std::stod(a.value.token.lexeme)));
                              case Tokentype::RATIONAL: {
                                  size_t slashPos = a.value.token.lexeme.find('/');
                                  if (slashPos != std::string::npos) {
                                      int num = std::stoi(a.value.token.lexeme.substr(0, slashPos));
                                      int den = std::stoi(a.value.token.lexeme.substr(slashPos + 1));
                                      return SchemeValue(Number(Number::Rational(num, den)));
                                  }
                                  throw std::runtime_error("Invalid rational format");
                              }
                              case Tokentype::COMPLEX: {
                                  std::string complexStr = a.value.token.lexeme;
                                  size_t iPos = complexStr.find('i');
                                  if (iPos == std::string::npos) {
                                      return SchemeValue(Number(std::stod(complexStr)));
                                  }
                                  complexStr = complexStr.substr(0, iPos); // Remove 'i'
                                  size_t plusPos = complexStr.rfind('+');
                                  size_t minusPos = complexStr.rfind('-'); // Find last sign
                                  double real = 0.0;
                                  double imag = 0.0;

                                  if (complexStr.empty()) { // Just "i"
                                      imag = 1.0;
                                  } else if (plusPos == std::string::npos && (minusPos == std::string::npos || minusPos == 0)) { // Pure imaginary like "2i" or "-2i"
                                      imag = std::stod(complexStr);
                                  } else { // Has real and imaginary part like "1+2" or "1-2" or "-1-2" etc.
                                      size_t signPos = (plusPos != std::string::npos) ? plusPos : minusPos;
                                      real = std::stod(complexStr.substr(0, signPos));
                                      imag = std::stod(complexStr.substr(signPos));
                                  }
                                  return SchemeValue(Number(Number::ComplexType(real, imag)));
                              }
                              case Tokentype::STRING:
                                  return SchemeValue(a.value.token.lexeme.substr(1, a.value.token.lexeme.length() - 2));
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
                              case Tokentype::ELLIPSIS:
                                  return SchemeValue(Symbol { "..." });
                              case Tokentype::ELSE:
                                  return SchemeValue(Symbol { "else" });
                              case Tokentype::QUOTE:
                                  return SchemeValue(Symbol { "quote" });
                              default:
                                  throw std::runtime_error("Unexpected token type in expressionToValue: " + a.value.token.lexeme);
                              }
                          },
                          [&](const sExpression& s) -> SchemeValue {
                              auto values_list_ptr = std::make_shared<std::list<SchemeValue>>();
                              for (const auto& elem : s.elements) {
                                  values_list_ptr->push_back(expressionToValue(*elem));
                              }
                              return SchemeValue(values_list_ptr);
                          },
                          [&](const ListExpression& l) -> SchemeValue {
                              auto values_list_ptr = std::make_shared<std::list<SchemeValue>>();
                              for (const auto& elem : l.elements) {
                                  values_list_ptr->push_back(expressionToValue(*elem));
                              }
                              return SchemeValue(values_list_ptr);
                          },
                          [&](const VectorExpression& v) -> SchemeValue {
                              auto values_vec_ptr = std::make_shared<std::vector<SchemeValue>>();
                              values_vec_ptr->reserve(v.elements.size());
                              for (const auto& elem : v.elements) {
                                  values_vec_ptr->push_back(expressionToValue(*elem));
                              }
                              return SchemeValue(std::move(values_vec_ptr));
                          },
                          [&](const LetExpression& l) -> SchemeValue {
                              auto values_list_ptr = std::make_shared<std::list<SchemeValue>>();
                              values_list_ptr->push_back(SchemeValue(Symbol { "let" }));
                              if (l.name) {
                                  values_list_ptr->push_back(SchemeValue(Symbol { l.name->token.lexeme }));
                              }
                              auto bindings_list = std::make_shared<std::list<SchemeValue>>();
                              for (const auto& binding : l.arguments) {
                                  auto binding_pair = std::make_shared<std::list<SchemeValue>>();
                                  binding_pair->push_back(SchemeValue(Symbol { binding.first.token.lexeme }));
                                  binding_pair->push_back(expressionToValue(*binding.second));
                                  bindings_list->push_back(SchemeValue(binding_pair));
                              }
                              values_list_ptr->push_back(SchemeValue(bindings_list));
                              for (const auto& expr : l.body) {
                                  values_list_ptr->push_back(expressionToValue(*expr));
                              }

                              return SchemeValue(values_list_ptr);
                          },
                          [&](const QuasiQuoteExpression& e) -> SchemeValue {
                              auto values_list_ptr = std::make_shared<std::list<SchemeValue>>();
                              values_list_ptr->push_back(SchemeValue(Symbol { "quasiquote" }));
                              values_list_ptr->push_back(expressionToValue(*e.value));
                              return SchemeValue(values_list_ptr);
                          },

                          [&](const UnquoteExpression& e) -> SchemeValue {
                              // Create a list with 'unquote' and the expression
                              auto values_list_ptr = std::make_shared<std::list<SchemeValue>>();
                              values_list_ptr->push_back(SchemeValue(Symbol { "unquote" }));
                              values_list_ptr->push_back(expressionToValue(*e.value));
                              return SchemeValue(values_list_ptr);
                          },

                          [&](const SpliceExpression& e) -> SchemeValue {
                              // Create a list with 'unquote-splicing' and the expression
                              auto values_list_ptr = std::make_shared<std::list<SchemeValue>>();
                              values_list_ptr->push_back(SchemeValue(Symbol { "unquote-splicing" }));
                              values_list_ptr->push_back(expressionToValue(*e.value));
                              return SchemeValue(values_list_ptr);
                          },
                          [&](const DefineExpression& d) -> SchemeValue {
                              auto values_list_ptr = std::make_shared<std::list<SchemeValue>>();
                              values_list_ptr->push_back(SchemeValue(Symbol { "define" }));
                              values_list_ptr->push_back(SchemeValue(Symbol { d.name.token.lexeme }));
                              values_list_ptr->push_back(expressionToValue(*d.value));
                              return SchemeValue(values_list_ptr);
                          },

                          [&](const DefineProcedure& d) -> SchemeValue {
                              auto values_list_ptr = std::make_shared<std::list<SchemeValue>>();
                              values_list_ptr->push_back(SchemeValue(Symbol { "define" }));
                              auto param_list = std::make_shared<std::list<SchemeValue>>();
                              param_list->push_back(SchemeValue(Symbol { d.name.token.lexeme }));
                              for (const auto& param : d.parameters) {
                                  param_list->push_back(SchemeValue(Symbol { param.token.lexeme }));
                              }

                              values_list_ptr->push_back(SchemeValue(param_list));
                              for (const auto& expr : d.body) {
                                  values_list_ptr->push_back(expressionToValue(*expr));
                              }

                              return SchemeValue(values_list_ptr);
                          },

                          [&](const LambdaExpression& l) -> SchemeValue {
                              auto values_list_ptr = std::make_shared<std::list<SchemeValue>>();
                              values_list_ptr->push_back(SchemeValue(Symbol { "lambda" }));
                              auto param_list = std::make_shared<std::list<SchemeValue>>();
                              for (const auto& param : l.parameters) {
                                  param_list->push_back(SchemeValue(Symbol { param.token.lexeme }));
                              }
                              values_list_ptr->push_back(SchemeValue(param_list));
                              for (const auto& expr : l.body) {
                                  values_list_ptr->push_back(expressionToValue(*expr));
                              }
                              return SchemeValue(values_list_ptr);
                          },

                          [&](const IfExpression& i) -> SchemeValue {
                              auto values_list_ptr = std::make_shared<std::list<SchemeValue>>();
                              values_list_ptr->push_back(SchemeValue(Symbol { "if" }));
                              values_list_ptr->push_back(expressionToValue(*i.condition));
                              values_list_ptr->push_back(expressionToValue(*i.then));
                              if (i.el) {
                                  values_list_ptr->push_back(expressionToValue(**i.el));
                              }

                              return SchemeValue(values_list_ptr);
                          },

                          [&](const SetExpression& s) -> SchemeValue {
                              auto values_list_ptr = std::make_shared<std::list<SchemeValue>>();
                              values_list_ptr->push_back(SchemeValue(Symbol { "set!" }));
                              values_list_ptr->push_back(SchemeValue(Symbol { s.identifier.token.lexeme }));
                              values_list_ptr->push_back(expressionToValue(*s.value));
                              return SchemeValue(values_list_ptr);
                          },

                          [&](const TailExpression& e) -> SchemeValue {
                              // Just convert the inner expression
                              return expressionToValue(*e.expression);
                          },

                          [&](const ImportExpression& i) -> SchemeValue {
                              auto values_list_ptr = std::make_shared<std::list<SchemeValue>>();
                              values_list_ptr->push_back(SchemeValue(Symbol { "import" }));
                              for (const auto& spec : i.imports) {
                                  auto lib_list = std::make_shared<std::list<SchemeValue>>();
                                  for (const auto& part : spec.library) {
                                      lib_list->push_back(expressionToValue(*part));
                                  }
                                  values_list_ptr->push_back(SchemeValue(lib_list));
                              }

                              return SchemeValue(values_list_ptr);
                          },

                          [&](const DefineSyntaxExpression& ds) -> SchemeValue {
                              auto values_list_ptr = std::make_shared<std::list<SchemeValue>>();
                              values_list_ptr->push_back(SchemeValue(Symbol { "define-syntax" }));
                              values_list_ptr->push_back(SchemeValue(Symbol { ds.name.token.lexeme }));
                              values_list_ptr->push_back(expressionToValue(*ds.rule));
                              return SchemeValue(values_list_ptr);
                          },

                          [&](const SyntaxRulesExpression& s) -> SchemeValue {
                              auto values_list_ptr = std::make_shared<std::list<SchemeValue>>();
                              values_list_ptr->push_back(SchemeValue(Symbol { "syntax-rules" }));
                              auto literals_list = std::make_shared<std::list<SchemeValue>>();
                              for (const auto& lit : s.literals) {
                                  literals_list->push_back(SchemeValue(Symbol { lit.lexeme }));
                              }
                              values_list_ptr->push_back(SchemeValue(literals_list));
                              for (const auto& rule : s.rules) {
                                  auto rule_list = std::make_shared<std::list<SchemeValue>>();
                                  rule_list->push_back(expressionToValue(*rule.pattern));
                                  rule_list->push_back(expressionToValue(*rule.template_expr));
                                  values_list_ptr->push_back(SchemeValue(rule_list));
                              }

                              return SchemeValue(values_list_ptr);
                          },
                          [&](const auto& a) -> SchemeValue {
                              throw std::runtime_error("Invalid Expression type in expressionToValue");
                          } },
        expr.as);
}
SchemeValue::SchemeValue()
    : value(std::make_shared<std::list<SchemeValue>>())
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
    if (!isNumber()) {
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
        throw std::runtime_error("Value is not Expression pointer");
    }
    return std::get<std::shared_ptr<Expression>>(value);
}

bool SchemeValue::isList() const
{
    return std::holds_alternative<std::shared_ptr<std::list<SchemeValue>>>(value);
}

std::shared_ptr<std::list<SchemeValue>> SchemeValue::asList() const
{
    if (!isList()) {
        throw std::runtime_error("Value is not a list representation");
    }
    return std::get<std::shared_ptr<std::list<SchemeValue>>>(value);
}

bool SchemeValue::isTrue() const
{
    return std::visit(overloaded {
                          [](const Number& arg) { return !arg.isZero(); },
                          [](const std::shared_ptr<Expression>& e) { return true; },
                          [](const std::string& arg) { return !arg.empty(); },
                          [](bool arg) { return arg; },
                          [](const Symbol&) { return true; },
                          [](const std::shared_ptr<std::list<SchemeValue>>& l) { return l && !l->empty(); },
                          [](const std::shared_ptr<std::vector<SchemeValue>>& v) { return v && !v->empty(); },
                          [](const std::shared_ptr<Procedure>&) { return true; },
                          [](const std::shared_ptr<ThreadHandle>&) { return true; },
                          [](const std::shared_ptr<MutexHandle>&) { return true; },
                          [](const std::shared_ptr<ConditionVarHandle>&) { return true; },
                          [](const Port& p) { return p.isOpen(); },
                          [](char c) { return true; }, // All chars are true
                      },
        value);
}

std::string SchemeValue::toString() const
{
    return std::visit(overloaded {
                          [](const Number& arg) { return arg.toString(); },
                          [](const std::string& arg) { return std::format("\"{}\"", arg); },
                          [](bool arg) { return arg ? std::string("#t") : "#f"; },
                          [](const Symbol& arg) { return arg.name; },
                          [](const std::shared_ptr<std::vector<SchemeValue>>& vec_ptr) {
                              if (!vec_ptr)
                                  return std::string("#(<null-vector>)");
                              std::string result = "#(";
                              bool first = true;
                              for (const auto& val : *vec_ptr) {
                                  if (!first)
                                      result += " ";
                                  result += val.toString();
                                  first = false;
                              }
                              result += ")";
                              return result;
                          },
                          [](const std::shared_ptr<std::list<SchemeValue>>& list_ptr) {
                              if (!list_ptr)
                                  return std::string("(<null-list>)");
                              if (list_ptr->empty())
                                  return std::string("()");
                              std::string result = "(";
                              bool first = true;
                              for (const auto& val : *list_ptr) {
                                  if (!first)
                                      result += " ";
                                  result += val.toString();
                                  first = false;
                              }
                              // This basic version doesn't handle improper lists / dotted pairs properly
                              result += ")";
                              return result;
                          },
                          [](const std::shared_ptr<Procedure>&) { return std::string("<procedure>"); },
                          [](const Port& p) {
                              return std::string(p.type == PortType::Input ? "<input-port>" : "<output-port>");
                          },
                          [](char c) {
                              if (c == ' ')
                                  return std::string("#\\space");
                              if (c == '\n')
                                  return std::string("#\\newline");
                              return std::string("#\\") + c;
                          },
                          [](const std::shared_ptr<Expression>& e) {
                              return e ? e->toString() : "<null-expression>";
                          },
                          [](const std::shared_ptr<ThreadHandle>&) { return std::string("<thread>"); },
                          [](const std::shared_ptr<MutexHandle>&) { return std::string("<mutex>"); },
                          [](const std::shared_ptr<ConditionVarHandle>&) { return std::string("<condition-variable>"); },
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

bool SchemeValue::isVector() const
{
    return std::holds_alternative<std::shared_ptr<std::vector<SchemeValue>>>(value);
}

std::shared_ptr<std::vector<SchemeValue>> SchemeValue::asSharedVector() const
{
    if (!isVector()) {
        throw std::runtime_error("Value is not a vector representation");
    }
    return std::get<std::shared_ptr<std::vector<SchemeValue>>>(value);
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
    if (isSymbol()) {
        return std::get<Symbol>(value).name;
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
    if (value.index() != other.value.index()) {
        return value.index() <=> other.value.index();
    }

    return std::visit(overloaded {
                          [](const Number& a, const Number& b) -> std::partial_ordering {
                              try {
                                  return a <=> b;
                              } catch (...) {
                                  return std::partial_ordering::unordered;
                              }
                          },
                          [](const std::string& a, const std::string& b) -> std::partial_ordering { return a <=> b; },
                          [](bool a, bool b) -> std::partial_ordering { return a <=> b; },
                          [](const Symbol& a, const Symbol& b) -> std::partial_ordering { return a.name <=> b.name; },
                          [](const std::shared_ptr<std::vector<SchemeValue>>& a_ptr, const std::shared_ptr<std::vector<SchemeValue>>& b_ptr) -> std::partial_ordering {
                              if (!a_ptr && !b_ptr)
                                  return std::partial_ordering::equivalent;
                              if (!a_ptr)
                                  return std::partial_ordering::less; // null < non-null
                              if (!b_ptr)
                                  return std::partial_ordering::greater; // non-null > null

                              const auto& a = *a_ptr;
                              const auto& b = *b_ptr;
                              auto minSize = std::min(a.size(), b.size());
                              for (size_t i = 0; i < minSize; ++i) {
                                  if (auto cmp = a[i] <=> b[i]; cmp != std::partial_ordering::equivalent) {
                                      return cmp;
                                  }
                              }
                              return a.size() <=> b.size();
                          },
                          [](const std::shared_ptr<std::list<SchemeValue>>& a_ptr, const std::shared_ptr<std::list<SchemeValue>>& b_ptr) -> std::partial_ordering {
                              if (!a_ptr && !b_ptr)
                                  return std::partial_ordering::equivalent;
                              if (!a_ptr)
                                  return std::partial_ordering::less;
                              if (!b_ptr)
                                  return std::partial_ordering::greater;

                              auto it_a = a_ptr->begin();
                              auto it_b = b_ptr->begin();
                              while (it_a != a_ptr->end() && it_b != b_ptr->end()) {
                                  if (auto cmp = (*it_a) <=> (*it_b); cmp != std::partial_ordering::equivalent) {
                                      return cmp;
                                  }
                                  ++it_a;
                                  ++it_b;
                              }
                              if (it_a == a_ptr->end() && it_b == b_ptr->end())
                                  return std::partial_ordering::equivalent;
                              if (it_a == a_ptr->end())
                                  return std::partial_ordering::less; // a is shorter
                              return std::partial_ordering::greater; // b is shorter
                          },
                          [](const std::shared_ptr<Procedure>& a, const std::shared_ptr<Procedure>& b) -> std::partial_ordering {
                              if (a == b)
                                  return std::partial_ordering::equivalent;
                              return std::partial_ordering::unordered; // Procedures generally unordered unless identical
                          },
                          [](const Port&, const Port&) -> std::partial_ordering { return std::partial_ordering::unordered; }, // Ports generally unordered
                          [](char a, char b) -> std::partial_ordering { return a <=> b; },
                          [](const std::shared_ptr<Expression>& a, const std::shared_ptr<Expression>& b) -> std::partial_ordering {
                              if (a == b)
                                  return std::partial_ordering::equivalent;
                              // Deep comparison might be possible but complex, treat as unordered for now
                              return std::partial_ordering::unordered;
                          },
                          [](const std::shared_ptr<ThreadHandle>& a, const std::shared_ptr<ThreadHandle>& b) -> std::partial_ordering {
                              if (a == b)
                                  return std::partial_ordering::equivalent;
                              return std::partial_ordering::unordered;
                          },
                          [](const std::shared_ptr<MutexHandle>& a, const std::shared_ptr<MutexHandle>& b) -> std::partial_ordering {
                              if (a == b)
                                  return std::partial_ordering::equivalent;
                              return std::partial_ordering::unordered;
                          },
                          [](const std::shared_ptr<ConditionVarHandle>& a, const std::shared_ptr<ConditionVarHandle>& b) -> std::partial_ordering {
                              if (a == b)
                                  return std::partial_ordering::equivalent;
                              return std::partial_ordering::unordered;
                          },
                          [](const auto&, const auto&) -> std::partial_ordering {
                              return std::partial_ordering::unordered; // Default for unhandled or cross-type
                          } },
        value, other.value);
}

bool SchemeValue::operator==(const SchemeValue& other) const
{
    if (value.index() != other.value.index()) {
        return false;
    }

    return std::visit(overloaded {
                          [](const Number& a, const Number& b) -> bool {
                              try {
                                  return (a <=> b) == std::partial_ordering::equivalent;
                              } catch (...) {
                                  return false;
                              }
                          },
                          [](const std::string& a, const std::string& b) -> bool { return a == b; },
                          [](bool a, bool b) -> bool { return a == b; },
                          [](const Symbol& a, const Symbol& b) -> bool { return a.name == b.name; },
                          [](const std::shared_ptr<std::vector<SchemeValue>>& a_ptr, const std::shared_ptr<std::vector<SchemeValue>>& b_ptr) -> bool {
                              if (a_ptr == b_ptr)
                                  return true;
                              if (!a_ptr || !b_ptr)
                                  return false;
                              const auto& a = *a_ptr;
                              const auto& b = *b_ptr;
                              if (a.size() != b.size())
                                  return false;
                              for (size_t i = 0; i < a.size(); ++i) {
                                  if (!(a[i] == b[i]))
                                      return false;
                              }
                              return true;
                          },
                          [](const std::shared_ptr<std::list<SchemeValue>>& a_ptr, const std::shared_ptr<std::list<SchemeValue>>& b_ptr) -> bool {
                              if (a_ptr == b_ptr)
                                  return true;
                              if (!a_ptr || !b_ptr)
                                  return false;
                              if (a_ptr->size() != b_ptr->size())
                                  return false;
                              auto it_a = a_ptr->begin();
                              auto it_b = b_ptr->begin();
                              while (it_a != a_ptr->end()) {
                                  if (!(*it_a == *it_b))
                                      return false;
                                  ++it_a;
                                  ++it_b;
                              }
                              return true;
                          },
                          [](const std::shared_ptr<Procedure>& a, const std::shared_ptr<Procedure>& b) -> bool { return a == b; },
                          [](const Port& a, const Port& b) -> bool { return &a == &b; },
                          [](char a, char b) -> bool { return a == b; },
                          [](const std::shared_ptr<Expression>& a, const std::shared_ptr<Expression>& b) -> bool {
                              if (a == b)
                                  return true;
                              if (!a || !b)
                                  return false;
                              return a->toString() == b->toString();
                          },
                          [](const std::shared_ptr<ThreadHandle>& a, const std::shared_ptr<ThreadHandle>& b) -> bool { return a == b; },
                          [](const std::shared_ptr<MutexHandle>& a, const std::shared_ptr<MutexHandle>& b) -> bool { return a == b; },
                          [](const std::shared_ptr<ConditionVarHandle>& a, const std::shared_ptr<ConditionVarHandle>& b) -> bool { return a == b; },
                          [](const auto&, const auto&) -> bool { return false; } },
        value, other.value);
}
