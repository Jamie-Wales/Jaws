
#include "MultiValue.h"
#include "Value.h"
#include <utility>

MultiValue::MultiValue(std::vector<SchemeValue> vals)
    : values(std::move(vals))
{
}
