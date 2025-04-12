#pragma once
#include "Value.h"
#include "interpret.h"
#include <optional>
#include <vector>

namespace jaws_math {
// Basic arithmetic
std::optional<SchemeValue> plus(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args);

std::optional<SchemeValue> minus(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args);

std::optional<SchemeValue> mult(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args);

std::optional<SchemeValue> div(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args);

// Comparison operations
std::optional<SchemeValue> less(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args);

std::optional<SchemeValue> greater(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args);

std::optional<SchemeValue> equal(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args);

std::optional<SchemeValue> lessOrEqual(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args);

std::optional<SchemeValue> greaterOrEqual(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args);

// Integer division operations
std::optional<SchemeValue> quotient(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args);

std::optional<SchemeValue> remainder(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args);

// Type conversions
std::optional<SchemeValue> exactToInexact(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args);

std::optional<SchemeValue> inexactToExact(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args);

// Complex number core operations
std::optional<SchemeValue> realPart(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args);

std::optional<SchemeValue> imagPart(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args);

std::optional<SchemeValue> makeRectangular(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args);

// Core transcendental functions
std::optional<SchemeValue> sqrt(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args);

std::optional<SchemeValue> exp(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args);

std::optional<SchemeValue> log(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args);

// Support functions
std::optional<SchemeValue> sin(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args);

std::optional<SchemeValue> cos(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args);

std::optional<SchemeValue> atan(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args);

std::optional<SchemeValue> random(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args);

std::optional<SchemeValue> isInteger(interpret::InterpreterState& state, const std::vector<SchemeValue>& args);
std::optional<SchemeValue> isRational(interpret::InterpreterState& state, const std::vector<SchemeValue>& args);
std::optional<SchemeValue> isReal(interpret::InterpreterState& state, const std::vector<SchemeValue>& args);
std::optional<SchemeValue> isComplex(interpret::InterpreterState& state, const std::vector<SchemeValue>& args);
std::optional<SchemeValue> isExact(interpret::InterpreterState& state, const std::vector<SchemeValue>& args);
std::optional<SchemeValue> isInexact(interpret::InterpreterState& state, const std::vector<SchemeValue>& args);
} // namespace jaws_math
