#ifndef JAWS_RUNTIME_H
#define JAWS_RUNTIME_H

#include <stddef.h>
#include <stdint.h>

// Number operations
int64_t scheme_add(int64_t a, int64_t b, int64_t c);
int64_t scheme_subtract(int64_t a, int64_t b);
int64_t scheme_multiply(int64_t a, int64_t b);
int64_t scheme_divide(int64_t a, int64_t b);

// Memory allocation
void* alloc(size_t size);

// List operations
void* scheme_cons(void* car, void* cdr);
void* scheme_car(void* pair);
void* scheme_cdr(void* pair);

#endif // JAWS_RUNTIME_H
