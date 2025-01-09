#pragma once
#include "Value.h"
#include "interpret.h"
#include <optional>

namespace jaws_macro {
std::optional<SchemeValue> definePrimitive(
    interpret::InterpreterState& state,
    const std::vector<SchemeValue>& args);

}
