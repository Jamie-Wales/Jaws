#pragma once
#include "Value.h"
namespace jaws_vec {
std::optional<SchemeValue> makeVector(Interpreter&, const std::vector<SchemeValue>& args);
std::optional<SchemeValue> vectorProcedure(Interpreter&, const std::vector<SchemeValue>& args);
std::optional<SchemeValue> vectorRef(Interpreter&, const std::vector<SchemeValue>& args);
std::optional<SchemeValue> vectorSet(Interpreter&, const std::vector<SchemeValue>& args);
std::optional<SchemeValue> vectorLength(Interpreter&, const std::vector<SchemeValue>& args);
}
