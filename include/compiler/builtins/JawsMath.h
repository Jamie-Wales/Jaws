#pragma once
#include <optional>

class Interpreter;
class SchemeValue;
namespace jaws_math {
std::optional<SchemeValue> plus(Interpreter&, const std::vector<SchemeValue>& args);
std::optional<SchemeValue> minus(Interpreter&, const std::vector<SchemeValue>& args);
std::optional<SchemeValue> mult(Interpreter&, const std::vector<SchemeValue>& args);
std::optional<SchemeValue> div(Interpreter&, const std::vector<SchemeValue>& args);
std::optional<SchemeValue> less(Interpreter&, const std::vector<SchemeValue>& args);
std::optional<SchemeValue> greater(Interpreter&, const std::vector<SchemeValue>& args);
std::optional<SchemeValue> equal(Interpreter&, const std::vector<SchemeValue>& args);
std::optional<SchemeValue> lessOrEqual(Interpreter&, const std::vector<SchemeValue>& args);
std::optional<SchemeValue> greaterOrEqual(Interpreter&, const std::vector<SchemeValue>& args);
}
