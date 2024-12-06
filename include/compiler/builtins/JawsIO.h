#pragma once
#include "Value.h"

namespace jaws_io {
std::optional<SchemeValue> write(Interpreter&, const std::vector<SchemeValue>& args);
std::optional<SchemeValue> display(Interpreter&, const std::vector<SchemeValue>& args);
std::optional<SchemeValue> newline(Interpreter&, const std::vector<SchemeValue>& args);
std::optional<SchemeValue> openInputFile(Interpreter&, const std::vector<SchemeValue>& args);
std::optional<SchemeValue> openOutputFile(Interpreter&, const std::vector<SchemeValue>& args);
std::optional<SchemeValue> closePort(Interpreter&, const std::vector<SchemeValue>& args);
std::optional<SchemeValue> read(Interpreter&, const std::vector<SchemeValue>& args);
std::optional<SchemeValue> import(Interpreter& interp, const std::vector<SchemeValue>& args);
}
