#pragma once
#include "interpret.h"
#include "Value.h"
#include <optional>
#include <vector>

namespace jaws_io {

std::optional<SchemeValue> openOutputFile(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args);

std::optional<SchemeValue> openInputFile(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args);

std::optional<SchemeValue> read(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args);

std::optional<SchemeValue> write(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args);

std::optional<SchemeValue> display(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args);

std::optional<SchemeValue> newline(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args);

std::optional<SchemeValue> closePort(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args);

} // namespace jaws_io
