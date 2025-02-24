#include "../include/jaws_io.h"
#include <stdio.h>

void display(SchemeObject* obj)
{
    printf("%s\n", to_string(obj));
}

int64_t scheme_multiply(SchemeObject* a, SchemeObject* b)
{
    return a->value.number * b->value.number;
}
