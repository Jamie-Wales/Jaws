#include "jaws_eq.h"
#include "types.h"

SchemeObject* is_null(SchemeObject* obj)
{
    return allocate(TYPE_BOOL, is_nil(obj));
}
