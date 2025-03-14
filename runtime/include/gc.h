#pragma once
#include "env.h"
#include "types.h"

extern void init_runtime();
extern void mark_object(SchemeObject* obj);
extern void mark_roots();
extern void sweep_compact();
extern void gc();
extern SchemeObject* alloc_object();
extern SchemeObject* allocate(SchemeType type, int64_t immediate);
extern SchemeObject* allocate_pair(SchemeObject* car, SchemeObject* cdr);
