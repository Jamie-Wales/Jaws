#include "Number.h"
#include "Visit.h"
#include <cmath>
#include <compare>
#include <format>
#include <limits>
#include <numeric>
#include <stdexcept>

using std::nullopt;

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

double Number::asFloat() const
{
    return std::visit(overloaded {
                          [](int i) -> double {
                              return static_cast<double>(i);
                          },
                          [](const Rational& r) -> double {
                              return static_cast<double>(r.numerator) / r.denominator;
                          },
                          [](double d) -> double {
                              return d;
                          },
                          [](const ComplexType& c) -> double {
                              if (c.imag() != 0.0) {
                                  throw std::runtime_error("Inexact->exact: complex number cannot be coerced to real");
                              }
                              return c.real();
                          } },
        value);
}

bool Number::asBoolean() const
{
    return !isZero();
}

Number::Rational Number::asRational() const
{
    return std::visit(overloaded {
                          [](int i) -> Rational {
                              return Rational(i, 1);
                          },
                          [](const Rational& r) -> Rational {
                              return r;
                          },
                          [](double d) -> Rational {
                              if (std::isnan(d) || std::isinf(d)) {
                                  throw std::runtime_error("Inexact->exact: no exact representation for NaN or infinity");
                              }

                              if (std::floor(d) == d) {
                                  if (d > std::numeric_limits<int>::max() || d < std::numeric_limits<int>::min()) {
                                      throw std::runtime_error("Inexact->exact: number too large for exact representation");
                                  }
                                  return Rational(static_cast<int>(d), 1);
                              }

                              double value = d;
                              int num = 0, den = 1;
                              int prevNum = 1, prevDen = 0;
                              int i = 0;
                              double epsilon = 1e-10;

                              while (i++ < 100) {
                                  int intPart = static_cast<int>(value);
                                  num = intPart * den + prevNum;
                                  prevNum = den;
                                  den = num;
                                  prevDen = num;

                                  double fractPart = value - intPart;
                                  if (std::abs(fractPart) < epsilon)
                                      break;
                                  if (fractPart == 0.0)
                                      break;
                                  value = 1.0 / fractPart;
                              }

                              return Rational(num, den);
                          },
                          [](const ComplexType& c) -> Rational {
                              if (c.imag() != 0.0) {
                                  throw std::runtime_error("Inexact->exact: complex number cannot be coerced to rational");
                              }
                              return Number(c.real()).asRational();
                          } },
        value);
}
std::string trim(const std::string& str)
{
    size_t first = str.find_first_not_of(" \t\n\r\f\v");
    if (std::string::npos == first) {
        return str; // empty or all whitespace
    }
    size_t last = str.find_last_not_of(" \t\n\r\f\v");
    return str.substr(first, (last - first + 1));
}

std::optional<Number> Number::fromString(std::string input)
{

    std::string str = trim(input); // Trim whitespace

    if (str.empty()) {
        return std::nullopt;
    }

    // --- 1. Complex Number Check ---
    // Look for 'i' suffix, but not as the only character.
    if (str.length() > 1 && str.back() == 'i') {
        std::string num_part = str.substr(0, str.length() - 1);
        if (num_part.empty())
            return std::nullopt; // Should not happen if str.length() > 1

        size_t last_plus = num_part.find_last_of('+');
        size_t last_minus = num_part.find_last_of('-');

        size_t split_pos = std::string::npos;
        if (last_plus != std::string::npos && last_plus != 0) {
            split_pos = last_plus;
        }
        if (last_minus != std::string::npos && last_minus != 0) {
            // Take the later sign if both '+' and '-' exist after position 0
            if (split_pos == std::string::npos || last_minus > split_pos) {
                split_pos = last_minus;
            }
        }

        double real = 0.0;
        double imag = 0.0;
        std::size_t chars_processed = 0;

        try {
            if (split_pos == std::string::npos) {
                if (num_part == "+" || num_part.empty()) {
                    imag = 1.0;
                    chars_processed = num_part.length();
                } else if (num_part == "-") {
                    imag = -1.0;
                    chars_processed = num_part.length();
                } else {
                    imag = std::stod(num_part, &chars_processed);
                }
                if (chars_processed != num_part.length())
                    return std::nullopt;
                real = 0.0;

            } else {
                std::string real_str = num_part.substr(0, split_pos);
                std::string imag_str = num_part.substr(split_pos); // Includes sign e.g "+2", "-5.6", "+", "-"
                real = std::stod(real_str, &chars_processed);
                if (chars_processed != real_str.length())
                    return std::nullopt;
                if (imag_str == "+") {
                    imag = 1.0;
                    chars_processed = imag_str.length();
                } else if (imag_str == "-") {
                    imag = -1.0;
                    chars_processed = imag_str.length();
                } else {
                    imag = std::stod(imag_str, &chars_processed);
                }
                if (chars_processed != imag_str.length())
                    return std::nullopt;
            }
            return Number(ComplexType(real, imag));

        } catch (const std::invalid_argument&) {
            return std::nullopt;
        } catch (const std::out_of_range&) {
            return std::nullopt;
        } catch (...) {
            return std::nullopt;
        }
    }
    size_t slash_pos = str.find('/');
    if (slash_pos != std::string::npos && slash_pos != 0 && slash_pos != str.length() - 1 && str.find('/', slash_pos + 1) == std::string::npos) // Only one slash
    {
        std::string num_str = str.substr(0, slash_pos);
        std::string den_str = str.substr(slash_pos + 1);
        std::size_t chars_processed_num = 0;
        std::size_t chars_processed_den = 0;
        try {
            int num = std::stoi(num_str, &chars_processed_num);
            int den = std::stoi(den_str, &chars_processed_den);
            if (chars_processed_num != num_str.length() || chars_processed_den != den_str.length()) {
                return std::nullopt;
            }
            if (den == 0) {
                return std::nullopt;
            }
            return Number(Rational(num, den));
        } catch (const std::invalid_argument&) {
            return std::nullopt;
        } catch (const std::out_of_range&) {
            return std::nullopt;
        } catch (const std::runtime_error&) {
            return std::nullopt;
        } catch (...) {
            return std::nullopt;
        }
    }
    bool maybe_float = str.find_first_of(".eE") != std::string::npos;
    std::size_t chars_processed = 0;

    if (maybe_float) {
        try {
            double val = std::stod(str, &chars_processed);
            // Ensure the entire string was consumed
            if (chars_processed != str.length())
                return std::nullopt;
            return Number(val);
        } catch (const std::invalid_argument&) {
            return nullopt;
        } catch (const std::out_of_range&) {
            return std::nullopt;
        } catch (...) {
            return std::nullopt;
        }
    } else {
        try {
            int val = std::stoi(str, &chars_processed);
            if (chars_processed != str.length())
                return std::nullopt;
            return Number(val);
        } catch (const std::invalid_argument&) {
            return std::nullopt;
        } catch (const std::out_of_range&) {
            return std::nullopt;
        } catch (...) {
            return std::nullopt;
        }
    }

    return std::nullopt;
}

Number::ComplexType Number::asComplex() const
{
    return std::visit(overloaded {
                          [](int i) -> ComplexType {
                              return ComplexType(static_cast<double>(i), 0.0);
                          },
                          [](const Rational& r) -> ComplexType {
                              return ComplexType(static_cast<double>(r.numerator) / r.denominator, 0.0);
                          },
                          [](double d) -> ComplexType {
                              return ComplexType(d, 0.0);
                          },
                          [](const ComplexType& c) -> ComplexType {
                              return c;
                          } },
        value);
}

bool Number::isExact() const
{
    return std::visit(overloaded {
                          [](int) -> bool { return true; },
                          [](const Rational&) -> bool { return true; },
                          [](double) -> bool { return false; },
                          [](const ComplexType& c) -> bool {
                              return std::floor(c.real()) == c.real() && std::floor(c.imag()) == c.imag();
                          } },
        value);
}

bool Number::isInexact() const
{
    return !isExact();
}

bool Number::isEqv(const Number& other) const
{
    if (isExact() != other.isExact())
        return false;
    return *this == other;
}
