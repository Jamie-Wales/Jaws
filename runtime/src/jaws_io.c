#include "../include/jaws_io.h"
#include "../include/gc.h"
#include <stdio.h>

// Function should be named scheme_display to match QBE
void scheme_display(SchemeObject* obj)
{
    printf("%s\n", to_string(obj));
}

// Return type should be SchemeObject*, not int64_t
SchemeObject* scheme_multiply(SchemeObject* a, SchemeObject* b)
{
    int64_t result = a->value.number * b->value.number;
    return allocate(TYPE_NUMBER, result);
}
