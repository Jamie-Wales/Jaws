#pragma once
#include "Value.h"
#include "interpret.h"
#include <optional>
#include <vector>

namespace jaws_vec {

std::optional<SchemeValue> makeVector(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args);

std::optional<SchemeValue> vectorProcedure(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args);

std::optional<SchemeValue> vectorRef(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args);

std::optional<SchemeValue> vectorSet(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args);

std::optional<SchemeValue> vectorLength(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args);

std::optional<SchemeValue> vectorFill(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args);

std::optional<SchemeValue> vectorCopy(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args);

std::optional<SchemeValue> vectorCopyTo(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args);

}
