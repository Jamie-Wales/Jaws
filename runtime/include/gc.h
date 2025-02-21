#pragma once
#include "types.h"

// gc
void init_runtime();
extern void mark_object(SchemeObject* obj);
extern void mark_roots();
extern void sweep_compact();
void gc();

// allocation
SchemeObject* alloc_object();
SchemeObject* allocate(SchemeType type, int64_t immediate);
SchemeObject* allocate_pair(SchemeObject* car, SchemeObject* cdr);
