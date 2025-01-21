#pragma once

#include "util.h"
#include <stdbool.h>
#include <stdlib.h>

typedef uint64_t value_t;

value_t make_integer(int64_t integer);
int64_t get_integer(value_t integer);
bool is_integer(value_t v);
