#pragma once
#include "interpret.h"
#include "Value.h"
#include "Error.h"

namespace jaws_eq {

std::optional<SchemeValue> less(
    interpret::InterpreterState&, 
    const std::vector<SchemeValue>& args);

std::optional<SchemeValue> greater(
    interpret::InterpreterState&, 
    const std::vector<SchemeValue>& args);

std::optional<SchemeValue> equal(
    interpret::InterpreterState&, 
    const std::vector<SchemeValue>& args);

std::optional<SchemeValue> isProcedure(
    interpret::InterpreterState&, 
    const std::vector<SchemeValue>& args);

std::optional<SchemeValue> isPair(
    interpret::InterpreterState&, 
    const std::vector<SchemeValue>& args);

std::optional<SchemeValue> isList(
    interpret::InterpreterState&, 
    const std::vector<SchemeValue>& args);

std::optional<SchemeValue> isVector(
    interpret::InterpreterState&, 
    const std::vector<SchemeValue>& args);

std::optional<SchemeValue> isSymbol(
    interpret::InterpreterState&, 
    const std::vector<SchemeValue>& args);

std::optional<SchemeValue> isNumber(
    interpret::InterpreterState&, 
    const std::vector<SchemeValue>& args);

std::optional<SchemeValue> isString(
    interpret::InterpreterState&, 
    const std::vector<SchemeValue>& args);

std::optional<SchemeValue> isPort(
    interpret::InterpreterState&, 
    const std::vector<SchemeValue>& args);

std::optional<SchemeValue> isNull(
    interpret::InterpreterState&, 
    const std::vector<SchemeValue>& args);

std::optional<SchemeValue> isEq(
    interpret::InterpreterState&, 
    const std::vector<SchemeValue>& args);

std::optional<SchemeValue> isEqv(
    interpret::InterpreterState&, 
    const std::vector<SchemeValue>& args);

std::optional<SchemeValue> isBooleanProc(
    interpret::InterpreterState&, 
    const std::vector<SchemeValue>& args);

} // namespace jaws_eq
