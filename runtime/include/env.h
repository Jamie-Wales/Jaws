// env.h
#pragma once
#include "types.h"
#include <stddef.h>
#include <stdio.h>

typedef struct Entry {
    SchemeObject* key;
    SchemeObject* value;
    struct Entry* next;
} Entry;

typedef struct HashMap {
    Entry** entries;
    size_t capacity;
    size_t count;
} HashMap;

typedef struct SymbolTable {
    HashMap* symbols;
} SymbolTable;

typedef struct SchemeEnvironment {
    struct SchemeEnvironment* enclosing;
    HashMap* bindings;
} SchemeEnvironment;

SchemeObject* intern_symbol(const char* name);
SchemeObject* env_lookup(SchemeEnvironment* env, SchemeObject* symbol);
void env_define(SchemeEnvironment* env, SchemeObject* symbol, SchemeObject* value);
void init_global_environment(void);
SchemeEnvironment* new_environment(SchemeEnvironment* enclosing);
void cleanup_environment(SchemeEnvironment* env);
HashMap* hashmap_new(void);
void hashmap_destroy(HashMap* map);
