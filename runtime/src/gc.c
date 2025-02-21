#include "../include/gc.h"
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define HEAP_OBJECT_COUNT 100
#define INITIAL_HEAP (sizeof(SchemeObject) * HEAP_OBJECT_COUNT)
static void* heap;
static size_t heap_size;
static size_t used;

SchemeObject* alloc_object()
{
    size_t size = sizeof(SchemeObject);
    if (used + size > heap_size) {
        gc();
        if (used + size > heap_size) {
            fprintf(stderr, "Out of memory!\n");
            exit(1);
        }
    }

    SchemeObject* obj = (SchemeObject*)((char*)heap + used);
    used += size;
    return obj;
}

SchemeObject* allocate(SchemeType type, int64_t immediate)
{
    SchemeObject* obj = alloc_object();
    obj->type = type;

    switch (type) {
    case TYPE_NUMBER:
        obj->value.number = immediate;
        break;
    case TYPE_SYMBOL:
        obj->value.symbol = (const char*)immediate;
        break;
    case TYPE_PAIR:
        obj->value.pair.car = (SchemeObject*)immediate;
        obj->value.pair.cdr = NULL;
        break;
    case TYPE_NIL:
        break;
    default:
        return NULL;
    }

    return obj;
}

SchemeObject* allocate_pair(SchemeObject* car, SchemeObject* cdr)
{
    SchemeObject* obj = alloc_object();
    obj->type = TYPE_PAIR;
    obj->value.pair.car = car;
    obj->value.pair.cdr = cdr;
    return obj;
}

void init_runtime()
{
    heap = malloc(INITIAL_HEAP);
    heap_size = INITIAL_HEAP;
    used = 0;
}

extern void mark_object(SchemeObject* obj)
{
    if (!obj || ((char*)obj < (char*)heap) || ((char*)obj >= (char*)heap + used))
        return;
    if (obj->type & 0x80)
        return;

    obj->type |= 0x80;
    switch (obj->type) {
    case TYPE_PAIR:
        mark_object(obj->value.pair.car);
        mark_object(obj->value.pair.cdr);
        break;
    }
}

extern void mark_roots()
{
    void* stack_top;
    void* stack_bottom;
    asm("movq %%rbp, %0" : "=r"(stack_bottom));
    asm("movq %%rsp, %0" : "=r"(stack_top));

    for (void** p = stack_top; p <= (void**)stack_bottom; p++) {
        SchemeObject* obj = (SchemeObject*)*p;
        if ((char*)obj >= (char*)heap && (char*)obj < (char*)heap + used) {
            mark_object(obj);
        }
    }
}

extern void sweep_compact()
{
    SchemeObject* new_end = (SchemeObject*)heap;
    SchemeObject* current = (SchemeObject*)heap;

    while ((char*)current < (char*)heap + used) {
        if (current->type & 0x80) {
            current->type &= 0x7F;

            if (current != new_end) {
                memmove(new_end, current, sizeof(SchemeObject));
                if (current->type == TYPE_PAIR) {
                    if ((char*)current->value.pair.car >= (char*)heap && (char*)current->value.pair.car < (char*)heap + used) {
                        ptrdiff_t offset = (char*)new_end - (char*)current;
                        new_end->value.pair.car = (SchemeObject*)((char*)current->value.pair.car + offset);
                    }
                    if ((char*)current->value.pair.cdr >= (char*)heap && (char*)current->value.pair.cdr < (char*)heap + used) {
                        ptrdiff_t offset = (char*)new_end - (char*)current;
                        new_end->value.pair.cdr = (SchemeObject*)((char*)current->value.pair.cdr + offset);
                    }
                }
            }
            new_end = (SchemeObject*)((char*)new_end + sizeof(SchemeObject));
        }
        current = (SchemeObject*)((char*)current + sizeof(SchemeObject));
    }

    used = (char*)new_end - (char*)heap;
}

void gc()
{
    mark_roots();
    sweep_compact();
}
