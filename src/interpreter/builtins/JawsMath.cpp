#include "builtins/JawsMath.h"
#include "Error.h"
#include "interpret.h"
#include <cmath>
#include <complex>
#include <random>

namespace jaws_math {
inline void checkSingleArg(const std::vector<SchemeValue>& args, const char* procName)
{
    if (args.size() != 1) {
        throw InterpreterError(std::string(procName) + ": requires exactly 1 argument");
    }
}
std::optional<SchemeValue> plus(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    if (args.empty()) {
        return SchemeValue(Number(0));
    }

    SchemeValue result = args[0].ensureValue();
    for (size_t i = 1; i < args.size(); ++i) {
        SchemeValue curr = args[i].ensureValue();
        if (curr.isProc()) {
            curr = *interpret::executeProcedure(state, curr, args);
        }
        result = curr + result;
    }
    return result;
}

std::optional<SchemeValue> minus(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    if (args.empty()) {
        throw InterpreterError("Cannot call procedure - on empty list");
    }

    if (args.size() == 1) {
        return -args[0].ensureValue();
    }

    SchemeValue result = args[0].ensureValue();
    for (size_t i = 1; i < args.size(); ++i) {
        result = result - args[i].ensureValue();
    }
    return result;
}

std::optional<SchemeValue> mult(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    if (args.empty()) {
        return SchemeValue(Number(1));
    }

    SchemeValue result = args[0].ensureValue();
    for (size_t i = 1; i < args.size(); ++i) {
        result = result * args[i].ensureValue();
    }
    return result;
}

std::optional<SchemeValue> div(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    if (args.empty()) {
        throw InterpreterError("Cannot call procedure / on empty list");
    }

    if (args.size() == 1) {
        return SchemeValue(Number(1)) / args[0].ensureValue();
    }

    SchemeValue result = args[0].ensureValue();
    for (size_t i = 1; i < args.size(); ++i) {
        auto curr = args[i].ensureValue();
        if (curr.asNumber().isZero()) {
            throw InterpreterError("Division by zero");
        }
        result = result / curr;
    }
    return result;
}

std::optional<SchemeValue> less(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    if (args.size() < 2) {
        throw InterpreterError("< requires at least two arguments");
    }

    for (size_t i = 0; i < args.size() - 1; ++i) {
        auto curr = args[i].ensureValue();
        auto next = args[i + 1].ensureValue();

        auto comparison = curr <=> next;
        if (comparison == std::partial_ordering::unordered) {
            throw InterpreterError("Cannot compare these values");
        }
        if (!(comparison < 0)) {
            return SchemeValue(false);
        }
    }
    return SchemeValue(true);
}

std::optional<SchemeValue> greater(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    if (args.size() < 2) {
        throw InterpreterError("> requires at least two arguments");
    }

    for (size_t i = 0; i < args.size() - 1; ++i) {
        auto curr = args[i].ensureValue();
        auto next = args[i + 1].ensureValue();

        auto comparison = curr <=> next;
        if (comparison == std::partial_ordering::unordered) {
            throw InterpreterError("Cannot compare these values");
        }
        if (!(comparison > 0)) {
            return SchemeValue(false);
        }
    }
    return SchemeValue(true);
}

std::optional<SchemeValue> lessOrEqual(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    if (args.size() < 2) {
        throw InterpreterError("'<=' requires at least 2 arguments");
    }

    for (size_t i = 0; i < args.size() - 1; i++) {
        auto curr = args[i].ensureValue();
        auto next = args[i + 1].ensureValue();

        if (!curr.isNumber() || !next.isNumber()) {
            throw InterpreterError("Cannot compare non-numeric values with <=");
        }

        auto ordering = curr.asNumber() <=> next.asNumber();
        if (ordering == std::partial_ordering::unordered) {
            throw InterpreterError("Cannot compare these numeric types with <=");
        }
        if (ordering == std::partial_ordering::greater) {
            return SchemeValue(false);
        }
    }
    return SchemeValue(true);
}

std::optional<SchemeValue> greaterOrEqual(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    if (args.size() < 2) {
        throw InterpreterError("'>=' requires at least 2 arguments");
    }

    for (size_t i = 0; i < args.size() - 1; i++) {
        auto curr = args[i].ensureValue();
        auto next = args[i + 1].ensureValue();

        if (!curr.isNumber() || !next.isNumber()) {
            throw InterpreterError("Cannot compare non-numeric values with >=");
        }

        auto ordering = curr.asNumber() <=> next.asNumber();
        if (ordering == std::partial_ordering::unordered) {
            throw InterpreterError("Cannot compare these numeric types with >=");
        }
        if (ordering == std::partial_ordering::less) {
            return SchemeValue(false);
        }
    }
    return SchemeValue(true);
}

std::optional<SchemeValue> equal(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    if (args.size() < 2) {
        throw InterpreterError("'=' requires at least 2 arguments");
    }

    for (size_t i = 0; i < args.size() - 1; i++) {
        auto curr = args[i].ensureValue();
        auto next = args[i + 1].ensureValue();

        if (!curr.isNumber() || !next.isNumber()) {
            throw InterpreterError("Cannot compare non-numeric values with =");
        }

        if (!(curr == next)) {
            return SchemeValue(false);
        }
    }
    return SchemeValue(true);
}

std::optional<SchemeValue> quotient(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    if (args.size() != 2) {
        throw InterpreterError("quotient requires exactly 2 arguments");
    }

    auto n = args[0].ensureValue();
    auto d = args[1].ensureValue();

    if (!n.isNumber() || !d.isNumber()) {
        throw InterpreterError("quotient requires numeric arguments");
    }

    Number num = n.asNumber();
    Number den = d.asNumber();

    if (den.isZero()) {
        throw InterpreterError("quotient: division by zero");
    }

    if (num.isInteger() && den.isInteger()) {
        int n_int = num.toInt();
        int d_int = den.toInt();
        return SchemeValue(Number(n_int / d_int));
    }

    double result = std::floor(num.asFloat() / den.asFloat());
    return SchemeValue(Number(result));
}

std::optional<SchemeValue> remainder(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    if (args.size() != 2) {
        throw InterpreterError("remainder requires exactly 2 arguments");
    }

    auto n = args[0].ensureValue();
    auto d = args[1].ensureValue();

    if (!n.isNumber() || !d.isNumber()) {
        throw InterpreterError("remainder requires numeric arguments");
    }

    Number num = n.asNumber();
    Number den = d.asNumber();

    if (den.isZero()) {
        throw InterpreterError("remainder: division by zero");
    }

    // Calculate remainder
    if (num.isInteger() && den.isInteger()) {
        int n_int = num.toInt();
        int d_int = den.toInt();
        return SchemeValue(Number(n_int % d_int));
    }

    // Non-integer remainder
    double n_val = num.asFloat();
    double d_val = den.asFloat();
    double q = std::floor(n_val / d_val);
    return SchemeValue(Number(n_val - q * d_val));
}

std::optional<SchemeValue> exactToInexact(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("exact->inexact requires exactly 1 argument");
    }
    const auto& val = args[0].ensureValue();
    if (!val.isNumber()) {
        throw InterpreterError("exact->inexact requires a number argument");
    }
    Number result = val.asNumber();
    return SchemeValue(Number(result.asFloat())); // Convert to float (inexact)
}

std::optional<SchemeValue> inexactToExact(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("inexact->exact requires exactly 1 argument");
    }
    const auto& val = args[0].ensureValue();
    if (!val.isNumber()) {
        throw InterpreterError("inexact->exact requires a number argument");
    }
    Number result = val.asNumber();
    return SchemeValue(result.asRational()); // Convert to rational (exact)
}

std::optional<SchemeValue> realPart(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("real-part requires exactly 1 argument");
    }

    auto val = args[0].ensureValue();
    if (!val.isNumber()) {
        throw InterpreterError("real-part requires a number argument");
    }

    Number::ComplexType complex = val.asNumber().asComplex();
    return SchemeValue(Number(complex.real()));
}

std::optional<SchemeValue> imagPart(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("imag-part requires exactly 1 argument");
    }

    auto val = args[0].ensureValue();
    if (!val.isNumber()) {
        throw InterpreterError("imag-part requires a number argument");
    }

    Number::ComplexType complex = val.asNumber().asComplex();
    return SchemeValue(Number(complex.imag()));
}

std::optional<SchemeValue> makeRectangular(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    if (args.size() != 2) {
        throw InterpreterError("make-rectangular requires exactly 2 arguments");
    }

    auto real = args[0].ensureValue();
    auto imag = args[1].ensureValue();

    if (!real.isNumber() || !imag.isNumber()) {
        throw InterpreterError("make-rectangular requires number arguments");
    }

    double r = real.asNumber().asFloat();
    double i = imag.asNumber().asFloat();

    return SchemeValue(Number(Number::ComplexType(r, i)));
}

std::optional<SchemeValue> sqrt(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("sqrt requires exactly 1 argument");
    }

    auto val = args[0].ensureValue();
    if (!val.isNumber()) {
        throw InterpreterError("sqrt requires a number argument");
    }

    Number n = val.asNumber();
    if (n.isReal() && n.asFloat() < 0) {
        // Handle negative real numbers by returning complex
        return SchemeValue(Number(Number::ComplexType(0, std::sqrt(-n.asFloat()))));
    } else if (n.isComplex()) {
        Number::ComplexType c = n.asComplex();
        std::complex<double> result = std::sqrt(c);
        return SchemeValue(Number(Number::ComplexType(result.real(), result.imag())));
    } else {
        return SchemeValue(Number(std::sqrt(n.asFloat())));
    }
}

std::optional<SchemeValue> exp(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("exp requires exactly 1 argument");
    }

    auto val = args[0].ensureValue();
    if (!val.isNumber()) {
        throw InterpreterError("exp requires a number argument");
    }

    Number n = val.asNumber();
    if (n.isComplex()) {
        Number::ComplexType c = n.asComplex();
        std::complex<double> result = std::exp(c);
        return SchemeValue(Number(Number::ComplexType(result.real(), result.imag())));
    } else {
        return SchemeValue(Number(std::exp(n.asFloat())));
    }
}

std::optional<SchemeValue> log(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    if (args.size() < 1 || args.size() > 2) {
        throw InterpreterError("log requires 1 or 2 arguments");
    }

    auto val = args[0].ensureValue();
    if (!val.isNumber()) {
        throw InterpreterError("log requires number arguments");
    }

    if (args.size() == 2) {
        auto base = args[1].ensureValue();
        if (!base.isNumber()) {
            throw InterpreterError("log requires number arguments");
        }

        double x = val.asNumber().asFloat();
        double b = base.asNumber().asFloat();

        if (x <= 0 || b <= 0 || b == 1) {
            throw InterpreterError("log: domain error");
        }

        return SchemeValue(Number(std::log(x) / std::log(b)));
    }

    Number n = val.asNumber();
    if (n.isComplex()) {
        Number::ComplexType c = n.asComplex();
        std::complex<double> result = std::log(c);
        return SchemeValue(Number(Number::ComplexType(result.real(), result.imag())));
    } else {
        double x = n.asFloat();
        if (x <= 0) {
            throw InterpreterError("log: domain error");
        }
        return SchemeValue(Number(std::log(x)));
    }
}

std::optional<SchemeValue> sin(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("sin requires exactly 1 argument");
    }

    auto val = args[0].ensureValue();
    if (!val.isNumber()) {
        throw InterpreterError("sin requires a number argument");
    }

    Number n = val.asNumber();
    if (n.isComplex()) {
        Number::ComplexType c = n.asComplex();
        std::complex<double> result = std::sin(c);
        return SchemeValue(Number(Number::ComplexType(result.real(), result.imag())));
    } else {
        return SchemeValue(Number(std::sin(n.asFloat())));
    }
}

std::optional<SchemeValue> cos(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    if (args.size() != 1) {
        throw InterpreterError("cos requires exactly 1 argument");
    }

    auto val = args[0].ensureValue();
    if (!val.isNumber()) {
        throw InterpreterError("cos requires a number argument");
    }

    Number n = val.asNumber();
    if (n.isComplex()) {
        Number::ComplexType c = n.asComplex();
        std::complex<double> result = std::cos(c);
        return SchemeValue(Number(Number::ComplexType(result.real(), result.imag())));
    } else {
        return SchemeValue(Number(std::cos(n.asFloat())));
    }
}

std::optional<SchemeValue> atan(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    if (args.size() == 1) {
        auto val = args[0].ensureValue();
        if (!val.isNumber()) {
            throw InterpreterError("atan requires a number argument");
        }

        Number n = val.asNumber();
        if (n.isComplex()) {
            Number::ComplexType c = n.asComplex();
            std::complex<double> result = std::atan(c);
            return SchemeValue(Number(Number::ComplexType(result.real(), result.imag())));
        } else {
            return SchemeValue(Number(std::atan(n.asFloat())));
        }
    } else if (args.size() == 2) {
        // This is atan2(y, x)
        auto y = args[0].ensureValue();
        auto x = args[1].ensureValue();

        if (!y.isNumber() || !x.isNumber()) {
            throw InterpreterError("atan requires number arguments");
        }

        Number ny = y.asNumber();
        Number nx = x.asNumber();

        if (ny.isComplex() || nx.isComplex()) {
            throw InterpreterError("atan: complex arguments not supported for two-argument form");
        }

        double y_val = ny.asFloat();
        double x_val = nx.asFloat();

        return SchemeValue(Number(std::atan2(y_val, x_val)));
    } else {
        throw InterpreterError("atan requires 1 or 2 arguments");
    }
}

static std::mt19937 random_engine(std::random_device {}());

std::optional<SchemeValue> random(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    if (args.empty()) {
        std::uniform_real_distribution<double> dist(0.0, 1.0);
        return SchemeValue(Number(dist(random_engine)));
    } else if (args.size() == 1) {
        auto n = args[0].ensureValue();
        if (!n.isNumber() || !n.asNumber().isInteger()) {
            throw InterpreterError("random: argument must be a positive integer");
        }

        int max = n.asNumber().toInt();
        if (max <= 0) {
            throw InterpreterError("random: argument must be a positive integer");
        }

        std::uniform_int_distribution<int> dist(0, max - 1);
        return SchemeValue(Number(dist(random_engine)));
    } else {
        throw InterpreterError("random requires 0 or 1 arguments");
    }
}

std::optional<SchemeValue> isInteger(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    checkSingleArg(args, "integer?");
    const auto& val = args[0].ensureValue();
    bool result = val.isNumber() && val.asNumber().isInteger();
    return SchemeValue(result);
}

// rational?
std::optional<SchemeValue> isRational(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    checkSingleArg(args, "rational?");
    const auto& val = args[0].ensureValue();

    bool result = val.isNumber() && val.asNumber().isRational();
    return SchemeValue(result);
}

// real?
std::optional<SchemeValue> isReal(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    checkSingleArg(args, "real?");
    const auto& val = args[0].ensureValue();
    bool result = val.isNumber() && val.asNumber().isReal();
    return SchemeValue(result);
}

// complex?
std::optional<SchemeValue> isComplex(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    checkSingleArg(args, "complex?");
    const auto& val = args[0].ensureValue().asNumber().isComplex();
    return SchemeValue(val);
}

// exact?
std::optional<SchemeValue> isExact(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    checkSingleArg(args, "exact?");
    const auto& val = args[0].ensureValue();

    bool result = val.isNumber() && val.asNumber().isExact();
    return SchemeValue(result);
}

// inexact?
std::optional<SchemeValue> isInexact(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args)
{
    checkSingleArg(args, "inexact?");
    const auto& val = args[0].ensureValue();

    bool result = val.isNumber() && val.asNumber().isInexact();
    return SchemeValue(result);
}

}
