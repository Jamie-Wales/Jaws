#pragma once
#include "interpret.h"
#include "Value.h"
#include <optional>
#include <vector>

namespace jaws_hof {

std::optional<SchemeValue> eval(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args);

std::optional<SchemeValue> printHelp(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args);

std::optional<SchemeValue> apply(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args);

} 

