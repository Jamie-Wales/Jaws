// env.c
#include "../include/env.h"
#include "../include/gc.h"
#include "../include/types.h"
#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// #define DEBUG_GC

#define INITIAL_CAPACITY 16
#define TABLE_MAX_LOAD 0.75f
#define ENV_STACK_SIZE 1024

SchemeEnvironment *current_environment = NULL;
SchemeEnvironment *g_current_environment = NULL;
HashMap *global_symbol_table = NULL;
static SchemeEnvironment *environment_stack[ENV_STACK_SIZE];
static int environment_stack_top = -1;

// --- Hash Functions ---
static uint32_t hash_symbol_ptr(const SchemeObject *key) {
  return (uint32_t)((uintptr_t)key >> 3);
}
static uint32_t hash_string(const char *str) {
  uint32_t hash = 5381;
  int c;
  while ((c = (unsigned char)*str++)) {
    hash = ((hash << 5) + hash) + c;
  }
  return hash;
}

// --- HashMap Implementation ---
static void hashmap_resize(HashMap *map, size_t new_capacity) {
  if (!map) {
    fprintf(stderr, "ERROR: hashmap_resize called with NULL map\n");
    return;
  }
  if (new_capacity < INITIAL_CAPACITY)
    new_capacity = INITIAL_CAPACITY;
  printf("DEBUG: Resizing hashmap %p from %zu to %zu\n", (void *)map,
         map->capacity, new_capacity);
  Entry **new_entries = calloc(new_capacity, sizeof(Entry *));
  if (!new_entries) {
    fprintf(stderr, "ERROR: calloc failed in hashmap_resize.\n");
    return;
  }
  for (size_t i = 0; i < map->capacity; i++) {
    Entry *entry = map->entries[i];
    while (entry != NULL) {
      Entry *next = entry->next;
      if (!entry->key) {
        fprintf(stderr, "ERROR: NULL key found during resize for map %p\n",
                (void *)map);
        entry = next;
        continue;
      }
      uint32_t hash = hash_symbol_ptr(entry->key) % new_capacity;
      entry->next = new_entries[hash];
      new_entries[hash] = entry;
      entry = next;
    }
  }
  free(map->entries);
  map->entries = new_entries;
  map->capacity = new_capacity;
  printf("DEBUG: Hashmap resize complete for %p\n", (void *)map);
}

HashMap *hashmap_new(void) {
  printf("DEBUG: hashmap_new called\n");
  HashMap *map = malloc(sizeof(HashMap));
  if (!map) {
    fprintf(stderr, "FATAL: Failed to allocate HashMap struct\n");
    exit(1);
  }
  printf("DEBUG: hashmap_new: map struct allocated at %p\n", (void *)map);
  map->capacity = INITIAL_CAPACITY;
  map->count = 0;
  map->entries = calloc(INITIAL_CAPACITY, sizeof(Entry *));
  if (!map->entries) {
    fprintf(stderr, "FATAL: Failed to allocate HashMap entries array\n");
    free(map);
    exit(1);
  }
  printf("DEBUG: hashmap_new: entries array allocated at %p for map %p\n",
         (void *)map->entries, (void *)map);
  printf("DEBUG: hashmap_new returning map %p\n", (void *)map);
  return map;
}
void hashmap_destroy(HashMap *map) { /* ... same ... */
  if (!map)
    return;
  for (size_t i = 0; i < map->capacity; i++) {
    Entry *entry = map->entries[i];
    while (entry != NULL) {
      Entry *next = entry->next;
      free(entry);
      entry = next;
    }
  }
  free(map->entries);
  free(map);
}

// --- Updated hashmap_get ---
static SchemeObject *hashmap_get(HashMap *map, SchemeObject *key) {
  printf("DEBUG: hashmap_get(map=%p, key=%p)\n", (void *)map, (void *)key);
  if (!map || !key) {
    printf("DEBUG: hashmap_get: NULL map or key\n");
    return NULL;
  }
  if (!map->entries) {
    fprintf(stderr,
            "ERROR: hashmap_get called with NULL map->entries for map %p!\n",
            (void *)map);
    return NULL;
  }
  printf("DEBUG: hashmap_get: map capacity=%zu, count=%zu, entries=%p\n",
         map->capacity, map->count, (void *)map->entries);

  uint32_t hash = hash_symbol_ptr(key) % map->capacity;
  printf("DEBUG: hashmap_get: Calculated hash=%u for key=%p (capacity=%zu)\n",
         hash, (void *)key, map->capacity);

  if (hash >= map->capacity) {
    fprintf(stderr,
            "ERROR: hashmap_get: Calculated hash %u out of bounds for capacity "
            "%zu!\n",
            hash, map->capacity);
    return NULL;
  }

  printf("DEBUG: hashmap_get: Accessing map->entries[%u] at address %p\n", hash,
         (void *)&(map->entries[hash]));
  Entry *entry = map->entries[hash]; // Potential crash point
  printf("DEBUG: hashmap_get: Initial entry in bucket %u is %p\n", hash,
         (void *)entry);

  while (entry != NULL) {
    printf("DEBUG: hashmap_get: Checking entry %p (key=%p)\n", (void *)entry,
           (void *)entry->key);
    if (entry->key == key) {
      printf("DEBUG: hashmap_get: Found key %p, returning value %p\n",
             (void *)key, (void *)entry->value);
      return entry->value;
    }
    entry = entry->next;
  }
  printf("DEBUG: hashmap_get: Key %p not found in bucket %u\n", (void *)key,
         hash);
  return NULL;
}

static void
hashmap_put(HashMap *map, SchemeObject *key,
            SchemeObject *value) { /* ... same detailed prints as before ... */
  printf("DEBUG: hashmap_put(map=%p, key=%p, value=%p)\n", (void *)map,
         (void *)key, (void *)value);
  if (!map || !key) {
    fprintf(stderr, "ERROR: hashmap_put called with NULL map or key.\n");
    return;
  }
  if (!map->entries) {
    fprintf(stderr,
            "ERROR: hashmap_put called with NULL map->entries for map %p!\n",
            (void *)map);
    return;
  }
  printf("DEBUG: hashmap_put: map capacity=%zu, count=%zu, entries=%p\n",
         map->capacity, map->count, (void *)map->entries);
  if ((float)(map->count + 1) / map->capacity >= TABLE_MAX_LOAD) {
    hashmap_resize(map, map->capacity * 2);
  }
  uint32_t hash = hash_symbol_ptr(key) % map->capacity;
  printf("DEBUG: hashmap_put: Calculated hash=%u for key=%p (capacity=%zu)\n",
         hash, (void *)key, map->capacity);
  if (hash >= map->capacity) {
    fprintf(stderr,
            "ERROR: hashmap_put: Calculated hash %u out of bounds for capacity "
            "%zu!\n",
            hash, map->capacity);
    return;
  }
  printf("DEBUG: hashmap_put: Accessing map->entries[%u] at address %p\n", hash,
         (void *)&(map->entries[hash]));
  Entry *entry = map->entries[hash];
  printf("DEBUG: hashmap_put: Initial entry in bucket %u is %p\n", hash,
         (void *)entry);
  while (entry != NULL) {
    if (entry->key == key) {
      entry->value = value;
      printf("DEBUG: hashmap_put: Updated existing key %p\n", (void *)key);
      return;
    }
    entry = entry->next;
  }
  Entry *new_entry = malloc(sizeof(Entry));
  if (!new_entry) {
    fprintf(stderr, "ERROR: Failed to allocate hashmap entry\n");
    return;
  }
  printf("DEBUG: hashmap_put: Allocated new entry struct at %p\n",
         (void *)new_entry);
  new_entry->key = key;
  new_entry->value = value;
  new_entry->next = map->entries[hash];
  map->entries[hash] = new_entry;
  map->count++;
  printf("DEBUG: hashmap_put: Inserted new key %p\n", (void *)key);
}

// --- Symbol Table Implementation ---
void init_symbol_table() { /* ... same ... */
  if (!global_symbol_table) {
    global_symbol_table = hashmap_new();
    printf("DEBUG: Global symbol table initialized.\n");
  } else {
    printf("WARN: Symbol table already initialized.\n");
  }
}
void destroy_symbol_table() { /* ... same ... */
  if (!global_symbol_table)
    return;
  printf("DEBUG: Destroying global symbol table.\n");
  for (size_t i = 0; i < global_symbol_table->capacity; i++) {
    Entry *entry = global_symbol_table->entries[i];
    while (entry != NULL) {
      Entry *next = entry->next;
      if (entry->key && entry->key->type == TYPE_SYMBOL &&
          entry->key->value.symbol) {
        free((void *)entry->key->value.symbol);
      }
      free(entry);
      entry = next;
    }
  }
  free(global_symbol_table->entries);
  free(global_symbol_table);
  global_symbol_table = NULL;
}
SchemeObject *intern_symbol(const char *name) { /* ... same ... */
  if (!global_symbol_table) {
    fprintf(stderr, "ERROR: Symbol table not initialized!\n");
    return SCHEME_NIL;
  }
  if (!name) {
    fprintf(stderr, "ERROR: Attempted to intern NULL symbol name.\n");
    return SCHEME_NIL;
  }
  for (size_t i = 0; i < global_symbol_table->capacity; ++i) {
    Entry *entry = global_symbol_table->entries[i];
    while (entry != NULL) {
      if (entry->key && entry->key->type == TYPE_SYMBOL &&
          entry->key->value.symbol != NULL &&
          strcmp(entry->key->value.symbol, name) == 0) {
        return entry->key;
      }
      entry = entry->next;
    }
  }
  SchemeObject *new_symbol = alloc_object();
  if (!new_symbol) {
    fprintf(stderr, "ERROR: Failed to allocate memory for new symbol '%s'\n",
            name);
    return SCHEME_NIL;
  }
  new_symbol->type = TYPE_SYMBOL;
  char *name_copy = strdup(name);
  if (!name_copy) {
    fprintf(stderr, "ERROR: Failed to duplicate string for symbol '%s'\n",
            name);
    return SCHEME_NIL;
  }
  new_symbol->value.symbol = name_copy;
  hashmap_put(global_symbol_table, new_symbol, NULL);
  return new_symbol;
}

// --- Environment Implementation ---
// --- Updated new_environment ---
SchemeEnvironment *new_environment(SchemeEnvironment *enclosing) {
  printf("DEBUG: new_environment called, enclosing=%p\n", (void *)enclosing);
  SchemeEnvironment *env = malloc(sizeof(SchemeEnvironment));
  if (!env) {
    fprintf(stderr, "FATAL: Failed to allocate SchemeEnvironment struct\n");
    exit(1);
  }
  printf("DEBUG: new_environment: env struct allocated at %p\n", (void *)env);
  env->enclosing = enclosing;
  env->bindings = hashmap_new();
  if (!env->bindings) {
    fprintf(stderr, "FATAL: hashmap_new returned NULL in new_environment!\n");
    free(env);
    exit(1);
  }
  printf("DEBUG: new_environment returning env %p with bindings %p\n",
         (void *)env, (void *)env->bindings);
  return env;
}

// --- Updated env_lookup ---
SchemeObject *env_lookup(SchemeEnvironment *env, SchemeObject *symbol) {
  printf("DEBUG: env_lookup(env=%p, symbol=%p (\"%s\"))\n", (void *)env,
         (void *)symbol,
         symbol ? (symbol->value.symbol ? symbol->value.symbol : "?")
                : "?"); // Print args
  if (!symbol || (symbol->type != TYPE_SYMBOL)) {
    fprintf(stderr, "ERROR: Attempted env lookup with non-symbol key.\n");
    return SCHEME_NIL;
  }
  if (!env) {
    fprintf(stderr, "ERROR: env_lookup called with NULL environment.\n");
    return SCHEME_NIL;
  }

  SchemeEnvironment *current = env;
  while (current != NULL) {
    printf("DEBUG: env_lookup: Checking env %p\n", (void *)current);
    if (!current->bindings) {
      fprintf(stderr,
              "ERROR: env_lookup encountered NULL bindings for env %p!\n",
              (void *)current);
      return SCHEME_NIL;
    }
    printf("DEBUG: env_lookup: Calling hashmap_get for bindings %p\n",
           (void *)current->bindings);
    SchemeObject *value =
        hashmap_get(current->bindings, symbol); // Calls updated hashmap_get
    if (value != NULL) {
      printf("DEBUG: env_lookup: Found symbol in env %p, returning value %p\n",
             (void *)current, (void *)value);
      return value;
    }
    printf("DEBUG: env_lookup: Symbol not in env %p, checking enclosing %p\n",
           (void *)current, (void *)current->enclosing);
    current = current->enclosing;
  }

  // Symbol not found in any environment
  if (symbol && symbol->value.symbol) {
    fprintf(stderr, "WARN: Unbound variable: %s\n", symbol->value.symbol);
  } else {
    fprintf(stderr, "WARN: Unbound variable: (invalid symbol object %p)\n",
            (void *)symbol);
  }
  printf("DEBUG: env_lookup: Symbol not found, returning SCHEME_NIL\n");
  return SCHEME_NIL;
}

// --- Updated env_define ---
void env_define(SchemeEnvironment *env, SchemeObject *symbol,
                SchemeObject *value) {
  printf("DEBUG: env_define(env=%p, symbol=%p (\"%s\"), value=%p)\n",
         (void *)env, (void *)symbol,
         symbol ? (symbol->value.symbol ? symbol->value.symbol : "?") : "?",
         (void *)value);
  if (!env) {
    fprintf(stderr, "ERROR: env_define called with NULL environment.\n");
    return;
  }
  if (!symbol || (symbol->type != TYPE_SYMBOL)) {
    fprintf(stderr, "ERROR: Attempted env define with non-symbol key.\n");
    return;
  }
  if (!env->bindings) {
    fprintf(stderr,
            "ERROR: env_define called with NULL env->bindings for env %p!\n",
            (void *)env);
    return;
  }
  printf("DEBUG: env_define: Calling hashmap_put with bindings map %p\n",
         (void *)env->bindings);
  hashmap_put(env->bindings, symbol, value); // Calls updated hashmap_put
  printf("DEBUG: env_define: Returned from hashmap_put\n");
}

// --- Environment Stack Management ---
void push_current_environment() { /* ... same ... */
  if (environment_stack_top >= ENV_STACK_SIZE - 1) {
    fprintf(stderr, "FATAL: Environment stack overflow!\n");
    exit(1);
  }
  environment_stack[++environment_stack_top] = current_environment;
#ifdef DEBUG_GC
  printf("DEBUG: Pushed env %p onto stack (new top %d)\n",
         (void *)current_environment, environment_stack_top);
#endif
}
void set_current_environment(SchemeEnvironment *new_env) { /* ... same ... */
  current_environment = new_env;
  g_current_environment = new_env;
#ifdef DEBUG_GC
  printf(
      "DEBUG: Set current environment to %p (updated $g_current_environment)\n",
      (void *)new_env);
#endif
}
void pop_current_environment() { /* ... same ... */
  if (environment_stack_top < 0) {
    fprintf(stderr, "FATAL: Environment stack underflow!\n");
    exit(1);
  }
  current_environment = environment_stack[environment_stack_top--];
  g_current_environment = current_environment;
#ifdef DEBUG_GC
  printf("DEBUG: Popped env %p from stack (new top %d), set as current\n",
         (void *)current_environment, environment_stack_top);
#endif
}

// --- Initialization ---
void init_runtime_environment_and_symbols() {
  init_symbol_table();
  init_global_environment();
}
void init_global_environment(void) { /* ... same ... */
  if (current_environment) {
    printf("WARN: Global environment already initialized?\n");
    return;
  }
  if (!global_symbol_table) {
    fprintf(
        stderr,
        "ERROR: Symbol table must be initialized before global environment!\n");
    init_symbol_table();
  }
  current_environment = new_environment(NULL);
  printf("DEBUG: Global environment created at %p.\n",
         (void *)current_environment);
  g_current_environment = current_environment;
  printf("DEBUG: Set g_current_environment (at %p) to %p\n",
         (void *)&g_current_environment, (void *)g_current_environment);
  printf("DEBUG: Defining primitives in global environment...\n");
  extern SchemeObject *plus(SchemeObject * a, SchemeObject * b);
  extern SchemeObject *multiply(SchemeObject * a, SchemeObject * b);
  extern void display(SchemeObject * obj);
  extern void newline(void);
  extern SchemeObject *make_function(void *code, SchemeEnvironment *env);
  env_define(current_environment, intern_symbol("+"),
             make_function((void *)&plus, NULL));
  env_define(current_environment, intern_symbol("*"),
             make_function((void *)&multiply, NULL));
  env_define(current_environment, intern_symbol("display"),
             make_function((void *)&display, NULL));
  env_define(current_environment, intern_symbol("newline"),
             make_function((void *)&newline, NULL));
  printf("DEBUG: Global environment initialization complete.\n");
}
void cleanup_environment(SchemeEnvironment *env) { /* ... same ... */
  if (!env)
    return;
  hashmap_destroy(env->bindings);
  free(env);
}
