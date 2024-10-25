#include "Number.h"
#include "Visit.h"
#include <cmath>
#include <compare>
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
                          [](const ComplexType& c) {
                              return c.imag() == 0 && std::floor(c.real()) == c.real();
                          } },
        value);
}

bool Number::isInteger() const
{
    return std::visit(overloaded {
                          [](int) { return true; },
                          [](double d) { return std::floor(d) == d; },
                          [](const Rational& r) { return r.numerator % r.denominator == 0; },
                          [](const ComplexType& c) {
                              return c.imag() == 0 && std::floor(c.real()) == c.real();
                          } },
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

std::partial_ordering Number::operator<=>(const Number& other) const
{
    return std::visit(overloaded {
                          [](int a, int b) -> std::partial_ordering { return a <=> b; },

                          [](const Rational& a, const Rational& b) -> std::partial_ordering {
                              return (static_cast<long long>(a.numerator) * b.denominator) <=> (static_cast<long long>(b.numerator) * a.denominator);
                          },
                          [](const Rational& a, int b) -> std::partial_ordering {
                              return a.numerator <=> (static_cast<long long>(b) * a.denominator);
                          },
                          [](int a, const Rational& b) -> std::partial_ordering {
                              return (static_cast<long long>(a) * b.denominator) <=> b.numerator;
                          },

                          [](double a, double b) -> std::partial_ordering {
                              if (std::isnan(a) || std::isnan(b))
                                  return std::partial_ordering::unordered;
                              return a <=> b;
                          },
                          [](double a, int b) -> std::partial_ordering { return a <=> static_cast<double>(b); },
                          [](int a, double b) -> std::partial_ordering { return static_cast<double>(a) <=> b; },
                          [](double a, const Rational& b) -> std::partial_ordering {
                              return a <=> (static_cast<double>(b.numerator) / b.denominator);
                          },
                          [](const Rational& a, double b) -> std::partial_ordering {
                              return (static_cast<double>(a.numerator) / a.denominator) <=> b;
                          },

                          [](const ComplexType&, const ComplexType&) -> std::partial_ordering {
                              throw std::runtime_error("Complex numbers cannot be ordered");
                          },
                          [](const ComplexType&, const auto&) -> std::partial_ordering {
                              throw std::runtime_error("Complex numbers cannot be ordered");
                          },
                          [](const auto&, const ComplexType&) -> std::partial_ordering {
                              throw std::runtime_error("Complex numbers cannot be ordered");
                          },

                          [](const auto&, const auto&) -> std::partial_ordering {
                              throw std::runtime_error("Invalid numeric comparison");
                          } },
        value, other.value);
}

bool Number::operator==(const Number& other) const
{
    return std::visit(overloaded {
                          [](int a, int b) { return a == b; },

                          [](const Rational& a, const Rational& b) {
                              return a.numerator * b.denominator == b.numerator * a.denominator;
                          },
                          [](const Rational& a, int b) {
                              return a.numerator == b * a.denominator;
                          },
                          [](int a, const Rational& b) {
                              return a * b.denominator == b.numerator;
                          },
                          [](double a, double b) {
                              if (std::isnan(a) || std::isnan(b))
                                  return false;
                              return a == b;
                          },
                          [](double a, int b) { return a == static_cast<double>(b); },
                          [](int a, double b) { return static_cast<double>(a) == b; },
                          [](double a, const Rational& b) {
                              return a == static_cast<double>(b.numerator) / b.denominator;
                          },
                          [](const Rational& a, double b) {
                              return static_cast<double>(a.numerator) / a.denominator == b;
                          },

                          [](const ComplexType& a, const ComplexType& b) {
                              return a.real() == b.real() && a.imag() == b.imag();
                          },
                          [](const ComplexType& a, int b) {
                              return a.imag() == 0 && a.real() == static_cast<double>(b);
                          },
                          [](int a, const ComplexType& b) {
                              return b.imag() == 0 && static_cast<double>(a) == b.real();
                          },
                          [](const ComplexType& a, double b) {
                              return a.imag() == 0 && a.real() == b;
                          },
                          [](double a, const ComplexType& b) {
                              return b.imag() == 0 && a == b.real();
                          },
                          [](const ComplexType& a, const Rational& b) {
                              return a.imag() == 0 && a.real() == static_cast<double>(b.numerator) / b.denominator;
                          },
                          [](const Rational& a, const ComplexType& b) {
                              return b.imag() == 0 && static_cast<double>(a.numerator) / a.denominator == b.real();
                          },

                          [](const auto&, const auto&) {
                              return false;
                          } },
        value, other.value);
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
                          [](int a, double b) -> Number {
                              return static_cast<double>(a) + b;
                          },
                          [](int a, const ComplexType& b) -> Number {
                              return ComplexType(a + b.real(), b.imag());
                          },

                          [](const Rational& a, int b) -> Number {
                              return Rational(a.numerator + b * a.denominator, a.denominator);
                          },
                          [](const Rational& a, const Rational& b) -> Number {
                              return Rational(
                                  a.numerator * b.denominator + b.numerator * a.denominator,
                                  a.denominator * b.denominator);
                          },
                          [](const Rational& a, double b) -> Number {
                              return static_cast<double>(a.numerator) / a.denominator + b;
                          },
                          [](const Rational& a, const ComplexType& b) -> Number {
                              double aVal = static_cast<double>(a.numerator) / a.denominator;
                              return ComplexType(aVal + b.real(), b.imag());
                          },

                          [](double a, int b) -> Number {
                              return a + static_cast<double>(b);
                          },
                          [](double a, const Rational& b) -> Number {
                              return a + static_cast<double>(b.numerator) / b.denominator;
                          },
                          [](double a, double b) -> Number {
                              return a + b;
                          },
                          [](double a, const ComplexType& b) -> Number {
                              return ComplexType(a + b.real(), b.imag());
                          },

                          // Complex combinations
                          [](const ComplexType& a, int b) -> Number {
                              return ComplexType(a.real() + b, a.imag());
                          },
                          [](const ComplexType& a, const Rational& b) -> Number {
                              return ComplexType(
                                  a.real() + static_cast<double>(b.numerator) / b.denominator,
                                  a.imag());
                          },
                          [](const ComplexType& a, double b) -> Number {
                              return ComplexType(a.real() + b, a.imag());
                          },
                          [](const ComplexType& a, const ComplexType& b) -> Number {
                              return a + b;
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
                          [](int a, double b) -> Number {
                              return static_cast<double>(a) - b;
                          },
                          [](int a, const ComplexType& b) -> Number {
                              return ComplexType(a - b.real(), -b.imag());
                          },

                          // Rational combinations
                          [](const Rational& a, int b) -> Number {
                              return Rational(a.numerator - b * a.denominator, a.denominator);
                          },
                          [](const Rational& a, const Rational& b) -> Number {
                              return Rational(
                                  a.numerator * b.denominator - b.numerator * a.denominator,
                                  a.denominator * b.denominator);
                          },
                          [](const Rational& a, double b) -> Number {
                              return static_cast<double>(a.numerator) / a.denominator - b;
                          },
                          [](const Rational& a, const ComplexType& b) -> Number {
                              double aVal = static_cast<double>(a.numerator) / a.denominator;
                              return ComplexType(aVal - b.real(), -b.imag());
                          },

                          // Double combinations
                          [](double a, int b) -> Number {
                              return a - static_cast<double>(b);
                          },
                          [](double a, const Rational& b) -> Number {
                              return a - static_cast<double>(b.numerator) / b.denominator;
                          },
                          [](double a, double b) -> Number {
                              return a - b;
                          },
                          [](double a, const ComplexType& b) -> Number {
                              return ComplexType(a - b.real(), -b.imag());
                          },

                          [](const ComplexType& a, int b) -> Number {
                              return ComplexType(a.real() - b, a.imag());
                          },
                          [](const ComplexType& a, const Rational& b) -> Number {
                              return ComplexType(
                                  a.real() - static_cast<double>(b.numerator) / b.denominator,
                                  a.imag());
                          },
                          [](const ComplexType& a, double b) -> Number {
                              return ComplexType(a.real() - b, a.imag());
                          },
                          [](const ComplexType& a, const ComplexType& b) -> Number {
                              return a - b;
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
                          [](int a, double b) -> Number {
                              return static_cast<double>(a) * b;
                          },
                          [](int a, const ComplexType& b) -> Number {
                              return ComplexType(a * b.real(), a * b.imag());
                          },

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
                              double aVal = static_cast<double>(a.numerator) / a.denominator;
                              return ComplexType(aVal * b.real(), aVal * b.imag());
                          },
                          [](double a, int b) -> Number {
                              return a * static_cast<double>(b);
                          },
                          [](double a, const Rational& b) -> Number {
                              return a * static_cast<double>(b.numerator) / b.denominator;
                          },
                          [](double a, double b) -> Number {
                              return a * b;
                          },
                          [](double a, const ComplexType& b) -> Number {
                              return ComplexType(a * b.real(), a * b.imag());
                          },
                          [](const ComplexType& a, int b) -> Number {
                              return ComplexType(a.real() * b, a.imag() * b);
                          },
                          [](const ComplexType& a, const Rational& b) -> Number {
                              double bVal = static_cast<double>(b.numerator) / b.denominator;
                              return ComplexType(a.real() * bVal, a.imag() * bVal);
                          },
                          [](const ComplexType& a, double b) -> Number {
                              return ComplexType(a.real() * b, a.imag() * b);
                          },
                          [](const ComplexType& a, const ComplexType& b) -> Number {
                              return a * b;
                          } },
        value, other.value);
}
int Number::toInt() const
{
    return std::visit(overloaded {
                          [](int a) -> int {
                              return a;
                          },
                          [](const Rational& r) -> int {
                              if (r.denominator == 1) {
                                  return r.numerator;
                              }
                              return r.numerator / r.denominator;
                          },
                          [](double d) -> int {
                              if (d > std::numeric_limits<int>::max() || d < std::numeric_limits<int>::min()) {
                                  throw std::runtime_error("Double value too large for integer conversion");
                              }
                              if (d != std::floor(d)) {
                                  throw std::runtime_error("Cannot convert non-integer to integer");
                              }
                              return static_cast<int>(d);
                          },
                          [](const ComplexType& c) -> int {
                              if (c.imag() != 0.0) {
                                  throw std::runtime_error("Cannot convert complex number with imaginary part to integer");
                              }
                              if (c.real() > std::numeric_limits<int>::max() || c.real() < std::numeric_limits<int>::min()) {
                                  throw std::runtime_error("Complex real part too large for integer conversion");
                              }
                              if (c.real() != std::floor(c.real())) {
                                  throw std::runtime_error("Cannot convert non-integer complex to integer");
                              }
                              return static_cast<int>(c.real());
                          } },
        value);
}
Number Number::operator/(const Number& other) const
{
    if (other.isZero()) {
        throw std::runtime_error("Division by zero");
    }

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
                              return static_cast<double>(a) / b;
                          },
                          [](int a, const ComplexType& b) -> Number {
                              double denominator = b.real() * b.real() + b.imag() * b.imag();
                              return ComplexType(
                                  (a * b.real()) / denominator,
                                  (-a * b.imag()) / denominator);
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
                              return Rational(
                                  a.numerator * b.denominator,
                                  a.denominator * b.numerator);
                          },
                          [](const Rational& a, double b) -> Number {
                              return static_cast<double>(a.numerator) / (a.denominator * b);
                          },
                          [](const Rational& a, const ComplexType& b) -> Number {
                              double aVal = static_cast<double>(a.numerator) / a.denominator;
                              double denominator = b.real() * b.real() + b.imag() * b.imag();
                              return ComplexType(
                                  (aVal * b.real()) / denominator,
                                  (-aVal * b.imag()) / denominator);
                          },

                          [](double a, int b) -> Number {
                              return a / static_cast<double>(b);
                          },
                          [](double a, const Rational& b) -> Number {
                              return a / (static_cast<double>(b.numerator) / b.denominator);
                          },
                          [](double a, double b) -> Number {
                              return a / b;
                          },
                          [](double a, const ComplexType& b) -> Number {
                              double denominator = b.real() * b.real() + b.imag() * b.imag();
                              return ComplexType(
                                  (a * b.real()) / denominator,
                                  (-a * b.imag()) / denominator);
                          },

                          // Complex combinations
                          [](const ComplexType& a, const ComplexType& b) -> Number {
                              double denominator = b.real() * b.real() + b.imag() * b.imag();
                              return ComplexType(
                                  (a.real() * b.real() + a.imag() * b.imag()) / denominator,
                                  (a.imag() * b.real() - a.real() * b.imag()) / denominator);
                          },
                          [](const ComplexType& a, int b) -> Number {
                              return ComplexType(a.real() / b, a.imag() / b);
                          },
                          [](const ComplexType& a, const Rational& b) -> Number {
                              double bVal = static_cast<double>(b.numerator) / b.denominator;
                              return ComplexType(a.real() / bVal, a.imag() / bVal);
                          },
                          [](const ComplexType& a, double b) -> Number {
                              return ComplexType(a.real() / b, a.imag() / b);
                          } },
        value, other.value);
}

Number Number::operator-() const
{
    return std::visit(overloaded {
                          [](int a) -> Number {
                              if (a == std::numeric_limits<int>::min()) {
                                  return static_cast<double>(-(static_cast<long long>(a)));
                              }
                              return -a;
                          },
                          [](const Rational& r) -> Number {
                              return Rational(-r.numerator, r.denominator);
                          },
                          [](double d) -> Number {
                              return -d;
                          },
                          [](const ComplexType& c) -> Number {
                              return ComplexType(-c.real(), -c.imag());
                          } },
        value);
}

std::string Number::toString() const
{
    return std::visit(overloaded {
                          [](int i) {
                              return std::format("{}", i);
                          },
                          [](const Rational& r) {
                              return std::format("{}/{}", r.numerator, r.denominator);
                          },
                          [](double d) {
                              return std::format("{:.6g}", d);
                          },
                          [](const ComplexType& c) {
                              if (c.imag() == 0) {
                                  return std::format("{:.6g}", c.real());
                              }
                              if (c.real() == 0) {
                                  return std::format("{:.6g}i", c.imag());
                              }
                              if (c.imag() < 0) {
                                  return std::format("{:.6g}{:.6g}i", c.real(), c.imag());
                              }
                              return std::format("{:.6g}+{:.6g}i", c.real(), c.imag());
                          } },
        value);
}
