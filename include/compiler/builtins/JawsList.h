#pragma once
#include <optional>

class Interpreter;
class SchemeValue;
namespace jaws_list {
std::optional<SchemeValue> map(Interpreter&, const std::vector<SchemeValue>& args);
std::optional<SchemeValue> listProcedure(Interpreter&, const std::vector<SchemeValue>& args);
std::optional<SchemeValue> carProcudure(Interpreter&, const std::vector<SchemeValue>& args);
std::optional<SchemeValue> cdrProcedure(Interpreter&, const std::vector<SchemeValue>& args);
std::optional<SchemeValue> cadrProcedure(Interpreter&, const std::vector<SchemeValue>& args);
std::optional<SchemeValue> cons(Interpreter&, const std::vector<SchemeValue>& args);
std::optional<SchemeValue> length(Interpreter&, const std::vector<SchemeValue>& args);
std::optional<SchemeValue> append(Interpreter&, const std::vector<SchemeValue>& args);
std::optional<SchemeValue> reverse(Interpreter&, const std::vector<SchemeValue>& args);
std::optional<SchemeValue> listRef(Interpreter&, const std::vector<SchemeValue>& args);
std::optional<SchemeValue> listTail(Interpreter&, const std::vector<SchemeValue>& args);
std::optional<SchemeValue> listSet(Interpreter&, const std::vector<SchemeValue>& args);
}
