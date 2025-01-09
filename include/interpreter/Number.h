#pragma once
#include <compare>
#include <complex>
#include <string>
#include <variant>

class Number {
public:
    struct Rational {
        int numerator;
        int denominator;
        Rational(int n, int d);
        void simplify();
    };

    using ComplexType = std::complex<double>;
    std::variant<int, Rational, double, ComplexType> value;

    Number(int i);
    Number(double d);
    Number(const Rational& r);
    Number(const ComplexType& c);

    bool isNumber() const;
    bool isZero() const;
    bool isComplex() const;
    bool isReal() const;
    bool isRational() const;
    bool isInteger() const;
    bool isExact() const;
    bool isInexact() const;
    bool isFloat() const;
    double asFloat() const;
    bool asBoolean() const;
    Rational asRational() const;
    ComplexType asComplex() const;
    int toInt() const;
    int asInteger() const;
    bool isEqv(const Number& other) const;
    std::string toString() const;
    Number operator+(const Number& other) const;
    Number operator-(const Number& other) const;
    Number operator-() const;
    Number operator*(const Number& other) const;
    Number operator/(const Number& other) const;
    std::partial_ordering operator<=>(const Number& other) const;
    bool operator==(const Number& other) const;
};
