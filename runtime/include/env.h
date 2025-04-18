// include/env.h
#pragma once
#include "types.h" // Assumes types.h includes stdint.h, stdbool.h etc.
#include <stddef.h>
#include <stdio.h>

// --- HashMap Structures ---
// Used for Environment Bindings (Symbol* -> Value*)
// Also used (inefficiently) for Symbol Table (Symbol* -> NULL, lookup via strcmp)
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

// --- Environment Structure ---
typedef struct SchemeEnvironment {
    struct SchemeEnvironment* enclosing; // Pointer to outer scope
    HashMap* bindings; // Binds SchemeObject* symbols to SchemeObject* values
} SchemeEnvironment;

extern SchemeEnvironment* g_current_environtment;
extern SchemeEnvironment* current_environment;
extern HashMap* global_symbol_table;

void push_current_environment();

void pop_current_environment();
void set_current_environment(SchemeEnvironment* new_env);
void init_symbol_table(void);
void init_global_environment(void);
void destroy_symbol_table(void);
void cleanup_environment(SchemeEnvironment* env);

SchemeObject* intern_symbol(const char* name);

SchemeEnvironment* new_environment(SchemeEnvironment* enclosing);
SchemeObject* env_lookup(SchemeEnvironment* env, SchemeObject* symbol);
void env_define(SchemeEnvironment* env, SchemeObject* symbol, SchemeObject* value);
