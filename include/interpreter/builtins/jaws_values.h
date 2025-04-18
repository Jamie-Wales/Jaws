#pragma once
#include "Error.h"
#include "Value.h"
#include <optional>
#include <vector>

namespace interpret {
struct InterpreterState;
}

namespace jaws_values {

std::optional<SchemeValue> schemeToString(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args);

std::optional<SchemeValue> valuesToList(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args);

std::optional<SchemeValue> valuesToVector(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args);

// Convert between lists and vectors
std::optional<SchemeValue> listToVector(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args);

std::optional<SchemeValue> vectorToList(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args);

std::optional<SchemeValue> symbolToString(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args);

std::optional<SchemeValue> charToString(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args);

std::optional<SchemeValue> stringToSymbol(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args);

std::optional<SchemeValue> stringToNumber(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args);

std::optional<SchemeValue> charToInteger(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args);

std::optional<SchemeValue> intToChar(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args);

std::optional<SchemeValue> stringToVector(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args);

std::optional<SchemeValue> vectorToString(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args);
} // namespace jaws_values
