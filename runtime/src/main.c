#include "../include/value.h"
#include <stdio.h>

int main(void)
{

    value_t a = make_integer(10);

    if (is_integer(a)) {
        printf("%lld\n", get_integer(a));
    }
}
