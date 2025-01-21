
#include "../include/value.h"
#include <stdbool.h>
value_t make_integer(int64_t integer)
{
    return (integer << TAG_BITS) | TAG_INTEGER;
}

int64_t get_integer(value_t v)
{
    return ((int64_t)v) >> TAG_BITS;
}

bool is_integer(value_t v)
{
    return (v & TAG_MASK) == TAG_INTEGER;
}
