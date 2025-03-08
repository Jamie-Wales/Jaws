// gc.c
#include "../include/gc.h"
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define HEAP_OBJECT_COUNT 100
#define MIN_HEAP_SIZE (sizeof(SchemeObject) * 50)
#define INITIAL_HEAP (sizeof(SchemeObject) * HEAP_OBJECT_COUNT)
#define HEAP_GROWTH_FACTOR 2
#define HEAP_SHRINK_THRESHOLD 0.25
#define DEBUG_GC

static void* heap;
static size_t heap_size;
static size_t used;
void print_heap()
{
    printf("\n=== HEAP CONTENTS ===\n");
    printf("Total heap size: %zu bytes\n", heap_size);
    printf("Used: %zu bytes\n", used);

    SchemeObject* current = (SchemeObject*)heap;
    size_t count = 0;

    while ((char*)current < (char*)heap + used) {
        printf("\nObject %zu at %p: %s",
            count++,
            (void*)current,
            to_string(current));
        current = (SchemeObject*)((char*)current + sizeof(SchemeObject));
    }

    printf("\n===================\n\n");
}
static void try_shrink_heap()
{
    size_t min_required = used * 2;
    if (min_required < MIN_HEAP_SIZE)
        min_required = MIN_HEAP_SIZE;

    if (heap_size > min_required) {
        void* new_heap = realloc(heap, min_required);
        if (new_heap) {
            heap = new_heap;
            heap_size = min_required;
#ifdef DEBUG_GC
            printf("Heap shrunk to %zu bytes\n", heap_size);
#endif
        }
    }
}

static void grow_heap()
{
    size_t new_size = heap_size * HEAP_GROWTH_FACTOR;
    void* new_heap = realloc(heap, new_size);
    if (!new_heap) {
        fprintf(stderr, "Out of memory: failed to grow heap to %zu bytes!\n", new_size);
        exit(1);
    }
    heap = new_heap;
    heap_size = new_size;
#ifdef DEBUG_GC
    printf("Heap grown to %zu bytes\n", heap_size);
#endif
}

SchemeObject* alloc_object()
{
    size_t size = sizeof(SchemeObject);
    if (used + size > heap_size) {
        gc();
        if (used + size > heap_size) {
            grow_heap();
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
    if (!heap) {
        fprintf(stderr, "Failed to initialize heap!\n");
        exit(1);
    }
    heap_size = INITIAL_HEAP;
    used = 0;
    init_global_environment();

#ifdef DEBUG_GC
    printf("Runtime initialized with heap size %zu bytes\n", heap_size);
#endif
}

void cleanup_runtime()
{
    if (SYMBOL_TABLE) {
        hashmap_destroy(SYMBOL_TABLE->symbols);
        free(SYMBOL_TABLE);
        SYMBOL_TABLE = NULL;
    }
    free(heap);
    heap = NULL;
    heap_size = 0;
    used = 0;
}

void mark_object(SchemeObject* obj)
{
    if (!obj || ((char*)obj < (char*)heap) || ((char*)obj >= (char*)heap + used)) {
        return;
    }
    if (obj->type & 0x80) {
        return;
    }
    obj->type |= 0x80;

    switch (obj->type & 0x7F) {
    case TYPE_PAIR:
        mark_object(obj->value.pair.car);
        mark_object(obj->value.pair.cdr);
        break;
    case TYPE_SYMBOL:
        break;
    }
}

void mark_environment(SchemeEnvironment* env)
{
    if (!env)
        return;
    for (size_t i = 0; i < env->bindings->capacity; i++) {
        Entry* entry = env->bindings->entries[i];
        while (entry != NULL) {
            mark_object(entry->key);
            if (entry->value) {
                mark_object(entry->value);
            }
            entry = entry->next;
        }
    }
}

void mark_roots()
{
#ifdef DEBUG_GC
    printf("Mark roots\n");
#endif

    SchemeEnvironment* env = current_environment;
    while (env != NULL) {
        mark_environment(env);
        env = env->enclosing;
    }

    if (GLOBAL_ENVIRONMENT) {
        mark_environment(GLOBAL_ENVIRONMENT);
    }
    void* stack_top;
    void* stack_bottom;
    asm("mov %0, fp" : "=r"(stack_bottom));
    asm("mov %0, sp" : "=r"(stack_top));
    for (void** p = stack_top; p <= (void**)stack_bottom; p++) {
        SchemeObject* obj = (SchemeObject*)*p;
        if ((char*)obj >= (char*)heap && (char*)obj < (char*)heap + used) {
#ifdef DEBUG_GC
            printf("Found root object at %p\n", (void*)p);
#endif
            mark_object(obj);
        }
    }
}

void sweep_compact()
{
    SchemeObject* new_end = (SchemeObject*)heap;
    SchemeObject* current = (SchemeObject*)heap;

#ifdef DEBUG_GC
    printf("Current heap usage %zu bytes\n", used);
    size_t live_count = 0;
#endif

    while ((char*)current < (char*)heap + used) {
        if (current->type & 0x80) {
#ifdef DEBUG_GC
            live_count++;
#endif
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

    size_t new_used = (char*)new_end - (char*)heap;

#ifdef DEBUG_GC
    printf("Live objects: %zu\n", live_count);
    printf("New heap usage: %zu bytes\n", new_used);
    printf("Freed: %zu bytes\n", used - new_used);
#endif

    used = new_used;
    if ((double)used / heap_size < HEAP_SHRINK_THRESHOLD) {
        try_shrink_heap();
    }

#ifdef DEBUG_GC
    printf("Heap utilization: %.1f%%\n", (used * 100.0) / heap_size);
#endif
}

void gc()
{
#ifdef DEBUG_GC
    printf("\nGarbage collector called\n");
#endif

    mark_environment(current_environment);
    mark_environment(GLOBAL_ENVIRONMENT);

#ifdef DEBUG_GC
    printf("\nBefore sweep:\n");
    print_heap();
#endif
    sweep_compact();

#ifdef DEBUG_GC
    printf("\nAfter sweep:\n");
    print_heap();
#endif
}
