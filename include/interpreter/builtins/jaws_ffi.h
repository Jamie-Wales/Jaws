#pragma once
#include "Value.h"
#include "interpret.h"
#include <optional>
#include <vector>

namespace jaws_ffi {
std::optional<SchemeValue> loadLibrary(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args);

std::optional<SchemeValue> registerFunction(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args);
}
