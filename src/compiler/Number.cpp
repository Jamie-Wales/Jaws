#include "Number.h"
#include "Visit.h"
#include <cmath>
#include <format>
#include <limits>
#include <numeric>
#include <stdexcept>

Number::Rational::Rational(int n, int d)
    : numerator(n)
    , denominator(d)
{
    if (denominator == 0)
        throw std::runtime_error("Denominator cannot be zero");
    simplify();
}

void Number::Rational::simplify()
{
    if (denominator < 0) {
        numerator = -numerator;
        denominator = -denominator;
    }
    int gcd = std::gcd(std::abs(numerator), denominator);
    numerator /= gcd;
    denominator /= gcd;
}

Number::Number(int i)
    : value(i)
{
}
Number::Number(double d)
    : value(d)
{
}
Number::Number(const Rational& r)
    : value(r)
{
}
Number::Number(const ComplexType& c)
    : value(c)
{
}

bool Number::isNumber() const { return true; }

bool Number::isComplex() const
{
    return std::visit(overloaded {
                          [](const ComplexType&) { return true; },
                          [](const auto&) { return false; } },
        value);
}

bool Number::isReal() const
{
    return std::visit(overloaded {
                          [](const ComplexType& c) { return c.imag() == 0; },
                          [](const auto&) { return true; } },
        value);
}

bool Number::isRational() const
{
    return std::visit(overloaded {
                          [](int) { return true; },
                          [](const Rational&) { return true; },
                          [](double d) { return std::floor(d) == d; },
                          [](const ComplexType&) { return false; } },
        value);
}

bool Number::isInteger() const
{
    return std::visit(overloaded {
                          [](int) { return true; },
                          [](double d) { return std::floor(d) == d; },
                          [](const Rational& r) { return r.numerator % r.denominator == 0; },
                          [](const ComplexType& c) { return c.imag() == 0 && std::floor(c.real()) == c.real(); } },
        value);
}

bool Number::isZero() const
{
    return std::visit(overloaded {
                          [](int i) { return i == 0; },
                          [](double d) { return d == 0.0; },
                          [](const Rational& r) { return r.numerator == 0; },
                          [](const ComplexType& c) { return c.real() == 0 && c.imag() == 0; } },
        value);
}
std::string Number::toString() const
{
    return std::visit(overloaded {
                          [](int i) { return std::format("{}", i); },
                          [](double d) { return std::format("{:.6g}", d); },
                          [](const Rational& r) { return std::format("{}/{}", r.numerator, r.denominator); },
                          [](const ComplexType& c) {
                              if (c.imag() == 0) {
                                  return std::format("{:.6g}", c.real());
                              } else if (c.real() == 0) {
                                  return std::format("{:.6g}i", c.imag());
                              } else if (c.imag() < 0) {
                                  return std::format("{:.6g}{:.6g}i", c.real(), c.imag());
                              } else {
                                  return std::format("{:.6g}+{:.6g}i", c.real(), c.imag());
                              }
                          } },
        value);
}
Number Number::operator+(const Number& other) const
{
    return std::visit(overloaded {
                          [](int a, int b) -> Number {
                              long long result = static_cast<long long>(a) + b;
                              if (result > std::numeric_limits<int>::max() || result < std::numeric_limits<int>::min()) {
                                  return static_cast<double>(result);
                              }
                              return static_cast<int>(result);
                          },
                          [](int a, const Rational& b) -> Number {
                              return Rational(a * b.denominator + b.numerator, b.denominator);
                          },
                          [](int a, double b) -> Number { return static_cast<double>(a) + b; },
                          [](int a, const ComplexType& b) -> Number { return ComplexType(a + b.real(), b.imag()); },

                          // Rational combinations
                          [](const Rational& a, int b) -> Number {
                              return Rational(a.numerator + b * a.denominator, a.denominator);
                          },
                          [](const Rational& a, const Rational& b) -> Number {
                              return Rational(a.numerator * b.denominator + b.numerator * a.denominator,
                                  a.denominator * b.denominator);
                          },
                          [](const Rational& a, double b) -> Number {
                              return static_cast<double>(a.numerator) / a.denominator + b;
                          },
                          [](const Rational& a, const ComplexType& b) -> Number {
                              return ComplexType(static_cast<double>(a.numerator) / a.denominator + b.real(), b.imag());
                          },

                          // Double combinations
                          [](double a, int b) -> Number { return a + static_cast<double>(b); },
                          [](double a, const Rational& b) -> Number {
                              return a + static_cast<double>(b.numerator) / b.denominator;
                          },
                          [](double a, double b) -> Number { return a + b; },
                          [](double a, const ComplexType& b) -> Number { return ComplexType(a + b.real(), b.imag()); },

                          // Complex combinations
                          [](const ComplexType& a, int b) -> Number { return ComplexType(a.real() + b, a.imag()); },
                          [](const ComplexType& a, const Rational& b) -> Number {
                              return ComplexType(a.real() + static_cast<double>(b.numerator) / b.denominator, a.imag());
                          },
                          [](const ComplexType& a, double b) -> Number { return ComplexType(a.real() + b, a.imag()); },
                          [](const ComplexType& a, const ComplexType& b) -> Number { return a + b; },

                          [](const auto& a, const auto& b) -> Number {
                              throw std::runtime_error("Unsupported types for addition");
                          } },
        value, other.value);
}

Number Number::operator-(const Number& other) const
{
    return std::visit(overloaded {
                          // Integer combinations
                          [](int a, int b) -> Number {
                              long long result = static_cast<long long>(a) - b;
                              if (result > std::numeric_limits<int>::max() || result < std::numeric_limits<int>::min()) {
                                  return static_cast<double>(result);
                              }
                              return static_cast<int>(result);
                          },
                          [](int a, const Rational& b) -> Number {
                              return Rational(a * b.denominator - b.numerator, b.denominator);
                          },
                          [](int a, double b) -> Number { return static_cast<double>(a) - b; },
                          [](int a, const ComplexType& b) -> Number { return ComplexType(a - b.real(), -b.imag()); },

                          [](const Rational& a, int b) -> Number {
                              return Rational(a.numerator - b * a.denominator, a.denominator);
                          },
                          [](const Rational& a, const Rational& b) -> Number {
                              return Rational(a.numerator * b.denominator - b.numerator * a.denominator,
                                  a.denominator * b.denominator);
                          },
                          [](const Rational& a, double b) -> Number {
                              return static_cast<double>(a.numerator) / a.denominator - b;
                          },
                          [](const Rational& a, const ComplexType& b) -> Number {
                              return ComplexType(static_cast<double>(a.numerator) / a.denominator - b.real(), -b.imag());
                          },

                          [](double a, int b) -> Number { return a - static_cast<double>(b); },
                          [](double a, const Rational& b) -> Number {
                              return a - static_cast<double>(b.numerator) / b.denominator;
                          },
                          [](double a, double b) -> Number { return a - b; },
                          [](double a, const ComplexType& b) -> Number { return ComplexType(a - b.real(), -b.imag()); },

                          [](const ComplexType& a, int b) -> Number { return ComplexType(a.real() - b, a.imag()); },
                          [](const ComplexType& a, const Rational& b) -> Number {
                              return ComplexType(a.real() - static_cast<double>(b.numerator) / b.denominator, a.imag());
                          },
                          [](const ComplexType& a, double b) -> Number { return ComplexType(a.real() - b, a.imag()); },
                          [](const ComplexType& a, const ComplexType& b) -> Number { return a - b; },

                          [](const auto& a, const auto& b) -> Number {
                              throw std::runtime_error("Unsupported types for subtraction");
                          } },
        value, other.value);
}

Number Number::operator*(const Number& other) const
{
    return std::visit(overloaded {
                          [](int a, int b) -> Number {
                              long long result = static_cast<long long>(a) * b;
                              if (result > std::numeric_limits<int>::max() || result < std::numeric_limits<int>::min()) {
                                  return static_cast<double>(result);
                              }
                              return static_cast<int>(result);
                          },
                          [](int a, const Rational& b) -> Number {
                              return Rational(a * b.numerator, b.denominator);
                          },
                          [](int a, double b) -> Number { return static_cast<double>(a) * b; },
                          [](int a, const ComplexType& b) -> Number { return ComplexType(a * b.real(), a * b.imag()); },

                          [](const Rational& a, int b) -> Number {
                              return Rational(a.numerator * b, a.denominator);
                          },
                          [](const Rational& a, const Rational& b) -> Number {
                              return Rational(a.numerator * b.numerator, a.denominator * b.denominator);
                          },
                          [](const Rational& a, double b) -> Number {
                              return static_cast<double>(a.numerator) / a.denominator * b;
                          },
                          [](const Rational& a, const ComplexType& b) -> Number {
                              double aValue = static_cast<double>(a.numerator) / a.denominator;
                              return ComplexType(aValue * b.real(), aValue * b.imag());
                          },

                          [](double a, int b) -> Number { return a * static_cast<double>(b); },
                          [](double a, const Rational& b) -> Number {
                              return a * static_cast<double>(b.numerator) / b.denominator;
                          },
                          [](double a, double b) -> Number { return a * b; },
                          [](double a, const ComplexType& b) -> Number { return ComplexType(a * b.real(), a * b.imag()); },

                          [](const ComplexType& a, int b) -> Number { return ComplexType(a.real() * b, a.imag() * b); },
                          [](const ComplexType& a, const Rational& b) -> Number {
                              double bValue = static_cast<double>(b.numerator) / b.denominator;
                              return ComplexType(a.real() * bValue, a.imag() * bValue);
                          },
                          [](const ComplexType& a, double b) -> Number { return ComplexType(a.real() * b, a.imag() * b); },
                          [](const ComplexType& a, const ComplexType& b) -> Number { return a * b; },

                          [](const auto& a, const auto& b) -> Number {
                              throw std::runtime_error("Unsupported types for multiplication");
                          } },
        value, other.value);
}

Number Number::operator/(const Number& other) const
{
    return std::visit(overloaded {
                          // Integer combinations
                          [](int a, int b) -> Number {
                              if (b == 0)
                                  throw std::runtime_error("Division by zero");
                              if (a % b == 0)
                                  return a / b;
                              return Rational(a, b);
                          },
                          [](int a, const Rational& b) -> Number {
                              if (b.numerator == 0)
                                  throw std::runtime_error("Division by zero");
                              return Rational(a * b.denominator, b.numerator);
                          },
                          [](int a, double b) -> Number {
                              if (b == 0)
                                  throw std::runtime_error("Division by zero");
                              return static_cast<double>(a) / b;
                          },
                          [](int a, const ComplexType& b) -> Number {
                              if (b == ComplexType(0, 0))
                                  throw std::runtime_error("Division by zero");
                              return ComplexType(a) / b;
                          },

                          // Rational combinations
                          [](const Rational& a, int b) -> Number {
                              if (b == 0)
                                  throw std::runtime_error("Division by zero");
                              return Rational(a.numerator, a.denominator * b);
                          },
                          [](const Rational& a, const Rational& b) -> Number {
                              if (b.numerator == 0)
                                  throw std::runtime_error("Division by zero");
                              return Rational(a.numerator * b.denominator, a.denominator * b.numerator);
                          },
                          [](const Rational& a, double b) -> Number {
                              if (b == 0)
                                  throw std::runtime_error("Division by zero");
                              return static_cast<double>(a.numerator) / (a.denominator * b);
                          },
                          [](const Rational& a, const ComplexType& b) -> Number {
                              if (b == ComplexType(0, 0))
                                  throw std::runtime_error("Division by zero");
                              double aValue = static_cast<double>(a.numerator) / a.denominator;
                              return ComplexType(aValue) / b;
                          },

                          // Double combinations
                          [](double a, int b) -> Number {
                              if (b == 0)
                                  throw std::runtime_error("Division by zero");
                              return a / static_cast<double>(b);
                          },
                          [](double a, const Rational& b) -> Number {
                              if (b.numerator == 0)
                                  throw std::runtime_error("Division by zero");
                              return a / (static_cast<double>(b.numerator) / b.denominator);
                          },
                          [](double a, double b) -> Number {
                              if (b == 0)
                                  throw std::runtime_error("Division by zero");
                              return a / b;
                          },
                          [](double a, const ComplexType& b) -> Number {
                              if (b == ComplexType(0, 0))
                                  throw std::runtime_error("Division by zero");
                              return ComplexType(a) / b;
                          },

                          // Complex combinations
                          [](const ComplexType& a, int b) -> Number {
                              if (b == 0)
                                  throw std::runtime_error("Division by zero");
                              return ComplexType(a.real() / b, a.imag() / b);
                          },
                          [](const ComplexType& a, const Rational& b) -> Number {
                              if (b.numerator == 0)
                                  throw std::runtime_error("Division by zero");
                              double bValue = static_cast<double>(b.numerator) / b.denominator;
                              return a / ComplexType(bValue, 0);
                          },
                          [](const ComplexType& a, double b) -> Number {
                              if (b == 0)
                                  throw std::runtime_error("Division by zero");
                              return ComplexType(a.real() / b, a.imag() / b);
                          },
                          [](const ComplexType& a, const ComplexType& b) -> Number {
                              if (b == ComplexType(0, 0))
                                  throw std::runtime_error("Division by zero");
                              double denominator = b.real() * b.real() + b.imag() * b.imag();
                              double real = (a.real() * b.real() + a.imag() * b.imag()) / denominator;
                              double imag = (a.imag() * b.real() - a.real() * b.imag()) / denominator;
                              return ComplexType(real, imag);
                          },
                          [](const auto& a, const auto& b) -> Number {
                              throw std::runtime_error("Unsupported types for division");
                          } },
        value, other.value);
}
Number Number::operator-() const
{
    return std::visit(overloaded {
                          [](int a) -> Number {
                              if (a == std::numeric_limits<int>::min()) {
                                  // #TODO handle edge cases - int min
                                  return static_cast<double>(-(static_cast<long long>(a)));
                              }
                              return -a;
                          },
                          [](double a) -> Number {
                              return -a;
                          },
                          [](const Rational& r) -> Number {
                              return Rational(-r.numerator, r.denominator);
                          },
                          [](const ComplexType& c) -> Number {
                              return ComplexType(-c.real(), -c.imag());
                          } },
        value);
}
