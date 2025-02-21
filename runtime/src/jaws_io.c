#include "../include/jaws_io.h"
#include <stdio.h>

void display(SchemeObject* obj)
{
    printf("%s", to_string(obj));
}
