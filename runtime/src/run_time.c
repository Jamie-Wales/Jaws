#include "../include/run_time.h"
#include <stdlib.h>

// Number operations
int64_t scheme_add(int64_t a, int64_t b, int64_t c)
{
    return a + b + c;
}

int64_t scheme_subtract(int64_t a, int64_t b)
{
    return a - b;
}

int64_t scheme_multiply(int64_t a, int64_t b)
{
    return a * b;
}

int64_t scheme_divide(int64_t a, int64_t b)
{
    if (b == 0)
        return 0; // Simple error handling for now
    return a / b;
}

// Memory allocation
void* alloc(size_t size)
{
    return malloc(size);
}

// List operations
void* scheme_cons(void* car, void* cdr)
{
    void** pair = alloc(2 * sizeof(void*));
    if (pair) {
        pair[0] = car;
        pair[1] = cdr;
    }
    return pair;
}

void* scheme_car(void* pair)
{
    return pair ? ((void**)pair)[0] : NULL;
}

void* scheme_cdr(void* pair)
{
    return pair ? ((void**)pair)[1] : NULL;
}
