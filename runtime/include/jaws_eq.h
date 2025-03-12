
#pragma once
#include "gc.h"
#include "types.h"

SchemeObject* is_null(SchemeObject* obj);
SchemeObject is_pair(SchemeObject* obj);
SchemeObject is_symbol(SchemeObject* obj);
SchemeObject is_number(SchemeObject* obj);
