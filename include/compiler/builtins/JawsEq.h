#pragma once
#include <optional>

class Interpreter;
class SchemeValue;
namespace jaws_eq {
std::optional<SchemeValue> isProcedure(Interpreter&, const std::vector<SchemeValue>& args);
std::optional<SchemeValue> isPair(Interpreter&, const std::vector<SchemeValue>& args);
std::optional<SchemeValue> isList(Interpreter&, const std::vector<SchemeValue>& args);
std::optional<SchemeValue> isVector(Interpreter&, const std::vector<SchemeValue>& args);
std::optional<SchemeValue> isSymbol(Interpreter&, const std::vector<SchemeValue>& args);
std::optional<SchemeValue> isNumber(Interpreter&, const std::vector<SchemeValue>& args);
std::optional<SchemeValue> isString(Interpreter&, const std::vector<SchemeValue>& args);
std::optional<SchemeValue> isPort(Interpreter&, const std::vector<SchemeValue>& args);
std::optional<SchemeValue> isNull(Interpreter&, const std::vector<SchemeValue>& args);
std::optional<SchemeValue> isEq(Interpreter&, const std::vector<SchemeValue>& args);
std::optional<SchemeValue> isEqv(Interpreter&, const std::vector<SchemeValue>& args);
std::optional<SchemeValue> isBooleanProc(Interpreter&, const std::vector<SchemeValue>& args);
}
