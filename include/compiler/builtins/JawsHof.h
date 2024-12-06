#pragma once
#include "Value.h"
namespace jaws_hof {
std::optional<SchemeValue> eval(Interpreter& interp, const std::vector<SchemeValue>& args);
std::optional<SchemeValue> printHelp(Interpreter& interp, const std::vector<SchemeValue>& args);
std::optional<SchemeValue> apply(Interpreter&, const std::vector<SchemeValue>& args);
}
