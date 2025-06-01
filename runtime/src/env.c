#include "../include/env.h"
#include "../include/gc.h"
#include "../include/types.h"
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Add this line (or compile with -DDEBUG_ENV) to enable debug prints
// #define DEBUG_ENV

#define INITIAL_CAPACITY 16
#define TABLE_MAX_LOAD 0.75f

SchemeEnvironment *current_environment = NULL;

SchemeEnvironment *g_current_environment = NULL;
HashMap *global_symbol_table = NULL;

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

static void hashmap_resize(HashMap *map, size_t new_capacity) {
  if (new_capacity < INITIAL_CAPACITY)
    new_capacity = INITIAL_CAPACITY;
  Entry **new_entries = calloc(new_capacity, sizeof(Entry *));
  if (!new_entries) {
    fprintf(stderr, "ERROR: Failed to allocate memory for hashmap resize.\n");
    return;
  }
  for (size_t i = 0; i < map->capacity; i++) {
    Entry *entry = map->entries[i];
    while (entry != NULL) {
      Entry *next = entry->next;
      uint32_t hash = hash_symbol_ptr(entry->key) % new_capacity;
      entry->next = new_entries[hash];
      new_entries[hash] = entry;
      entry = next;
    }
  }
  free(map->entries);
  map->entries = new_entries;
  map->capacity = new_capacity;
}

HashMap *hashmap_new(void) {
  HashMap *map = malloc(sizeof(HashMap));
  if (!map) {
    fprintf(stderr, "Failed to allocate hashmap\n");
    exit(1);
  }
  map->capacity = INITIAL_CAPACITY;
  map->count = 0;
  map->entries = calloc(INITIAL_CAPACITY, sizeof(Entry *));
  if (!map->entries) {
    fprintf(stderr, "Failed to allocate hashmap entries\n");
    free(map);
    exit(1);
  }
  return map;
}
void hashmap_destroy(HashMap *map) {
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
static SchemeObject *hashmap_get(HashMap *map, SchemeObject *key) {
  if (!map || !key)
    return NULL;
  uint32_t hash = hash_symbol_ptr(key) % map->capacity;
  Entry *entry = map->entries[hash];
  while (entry != NULL) {
    if (entry->key == key) {
      return entry->value;
    }
    entry = entry->next;
  }
  return NULL;
}

static void hashmap_put(HashMap *map, SchemeObject *key, SchemeObject *value) {
#ifdef DEBUG_ENV
  printf("DEBUG_HP_PUT: ENTERED. map: %p, key: %p (type: %d), value: %p (type: "
         "%d)\n",
         (void *)map, (void *)key, key ? (int)(key->type & ~0x80) : -1,
         (void *)value, value ? (int)(value->type & ~0x80) : -1);
  if (key && (key->type & ~0x80) == TYPE_SYMBOL) {
    printf("DEBUG_HP_PUT: key is SYMBOL: \"%s\"\n",
           key->value.symbol ? key->value.symbol : "NULL_STR_IN_KEY");
  }
  fflush(stdout);
#endif

  if (!map || !key) {
    fprintf(stderr, "ERROR_HP_PUT: map or key is NULL. map: %p, key: %p.\n",
            (void *)map, (void *)key);
    fflush(stderr);
    return;
  }

  if ((float)(map->count + 1) / map->capacity >= TABLE_MAX_LOAD) {
#ifdef DEBUG_ENV
    printf("DEBUG_HP_PUT: Resizing hashmap %p from capacity %zu for key %s\n",
           (void *)map, map->capacity, key->value.symbol);
    fflush(stdout);
#endif
    hashmap_resize(map, map->capacity * 2);
#ifdef DEBUG_ENV
    printf("DEBUG_HP_PUT: Resize complete for hashmap %p, new capacity %zu\n",
           (void *)map, map->capacity);
    fflush(stdout);
#endif
  }

  uint32_t hash_val = hash_symbol_ptr(key) % map->capacity;
#ifdef DEBUG_ENV
  printf("DEBUG_HP_PUT: Calculated hash: %u for key %s (%p)\n", hash_val,
         key->value.symbol, (void *)key);
  fflush(stdout);
#endif

  Entry *entry = map->entries[hash_val];
#ifdef DEBUG_ENV
  printf("DEBUG_HP_PUT: Initial entry for bucket %u: %p\n", hash_val,
         (void *)entry);
  fflush(stdout);
#endif

  int loop_iter = 0;
  while (entry != NULL) {
#ifdef DEBUG_ENV
    printf("DEBUG_HP_PUT: Loop iter %d, current entry: %p\n", loop_iter,
           (void *)entry);
    fflush(stdout);
#endif

    if (entry == (void *)0x2) {
      fprintf(
          stderr,
          "FATAL_HP_PUT: 'entry' pointer is 0x2 before dereferencing key!\n");
      fflush(stderr);
      exit(EXIT_FAILURE);
    }
    if (!entry->key) {
      fprintf(stderr, "FATAL_HP_PUT: entry %p has NULL key!\n", (void *)entry);
      fflush(stderr);
      exit(EXIT_FAILURE);
    }
#ifdef DEBUG_ENV
    printf("DEBUG_HP_PUT: Comparing entry->key: %p (name: %s) with target key: "
           "%p (name: %s)\n",
           (void *)entry->key,
           (entry->key->value.symbol ? entry->key->value.symbol
                                     : "NULL_STR_IN_ENTRY_KEY"),
           (void *)key,
           (key->value.symbol ? key->value.symbol : "NULL_STR_IN_TARGET_KEY"));
    fflush(stdout);
#endif

    if (entry->key == key) {
#ifdef DEBUG_ENV
      printf("DEBUG_HP_PUT: Key found. Updating value.\n");
      fflush(stdout);
#endif
      entry->value = value;
      return;
    }

    if (entry->next == (void *)0x2) {
      fprintf(stderr, "FATAL_HP_PUT: 'entry->next' pointer is 0x2 before "
                      "'entry = entry->next'!\n");
      fflush(stderr);
      exit(EXIT_FAILURE);
    }
#ifdef DEBUG_ENV
    printf("DEBUG_HP_PUT: Moving to entry->next: %p\n", (void *)entry->next);
    fflush(stdout);
#endif
    entry = entry->next;
    loop_iter++;
  }

#ifdef DEBUG_ENV
  printf("DEBUG_HP_PUT: Key NOT found. Allocating new entry.\n");
  fflush(stdout);
#endif
  Entry *new_entry = (Entry *)malloc(sizeof(Entry));
#ifdef DEBUG_ENV
  printf("DEBUG_HP_PUT: malloc for new_entry returned: %p\n",
         (void *)new_entry);
  fflush(stdout);
#endif

  if (!new_entry) {
    fprintf(stderr, "ERROR_HP_PUT: Failed to malloc new_entry\n");
    fflush(stderr);
    return;
  }
  new_entry->key = key;
  new_entry->value = value;
  new_entry->next = map->entries[hash_val];
  map->entries[hash_val] = new_entry;
  map->count++;
#ifdef DEBUG_ENV
  printf("DEBUG_HP_PUT: New entry %p (key=%s, val=%p, next=%p) added to bucket "
         "%u. Count=%zu\n",
         (void *)new_entry, key->value.symbol, (void *)value,
         (void *)new_entry->next, hash_val, map->count);
  fflush(stdout);
#endif
}

void init_symbol_table() {
  if (!global_symbol_table) {
    global_symbol_table = hashmap_new();
#ifdef DEBUG_ENV
    printf("DEBUG: Global symbol table initialized.\n");
#endif
  } else {
    printf("WARN: Symbol table already initialized.\n");
  }
}
void destroy_symbol_table() {
  if (!global_symbol_table)
    return;
#ifdef DEBUG_ENV
  printf("DEBUG: Destroying global symbol table.\n");
#endif
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
SchemeObject *intern_symbol(const char *name) {
#ifdef DEBUG_ENV
  printf("interning %s\n", name);
  fflush(stdout);
#endif
  if (!global_symbol_table) {
    fprintf(stderr, "ERROR: Symbol table not initialized! Call "
                    "init_symbol_table() first.\n");
    return SCHEME_NIL;
  }
  if (!name) {
    fprintf(stderr, "ERROR: Attempted to intern NULL symbol name.\n");
    return SCHEME_NIL;
  }
  for (size_t i = 0; i < global_symbol_table->capacity; i++) {
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
    // Note: new_symbol is allocated from GC heap, will be collected if not
    // used.
    return SCHEME_NIL;
  }
  new_symbol->value.symbol = name_copy;
  hashmap_put(global_symbol_table, new_symbol, NULL);
  return new_symbol;
}

SchemeEnvironment *new_environment(SchemeEnvironment *enclosing) {
  SchemeEnvironment *env =
      (SchemeEnvironment *)malloc(sizeof(SchemeEnvironment));
  if (!env) {
    fprintf(stderr, "Failed to allocate environment\n");
    exit(1);
  }
  env->enclosing = enclosing;
  env->bindings = hashmap_new();
  return env;
}
SchemeObject *env_lookup(SchemeEnvironment *env, SchemeObject *symbol) {
  if (!symbol || (symbol->type != TYPE_SYMBOL)) {
    fprintf(
        stderr,
        "ERROR: Attempted env lookup with non-symbol key (key=%p, type=%d).\n",
        (void *)symbol, symbol ? (int)symbol->type : -1);
    return SCHEME_NIL;
  }
  SchemeEnvironment *current = env;
  while (current != NULL) {
    SchemeObject *value = hashmap_get(current->bindings, symbol);
    if (value != NULL) {
      return value;
    }
    current = current->enclosing;
  }
  if (symbol && symbol->value.symbol) {
    // This is a runtime condition, not necessarily a debug print, so kept it.
    fprintf(stderr, "WARN: Unbound variable: %s\n", symbol->value.symbol);
  } else {
    fprintf(stderr, "WARN: Unbound variable: (invalid symbol object %p)\n",
            (void *)symbol);
  }
  return SCHEME_NIL;
}
void env_define(SchemeEnvironment *env, SchemeObject *symbol,
                SchemeObject *value) {
  if (!env) {
    fprintf(stderr, "ERROR: env_define called with NULL environment.\n");
    return;
  }
  if (!symbol || (symbol->type != TYPE_SYMBOL)) {
    fprintf(
        stderr,
        "ERROR: Attempted env define with non-symbol key (key=%p, type=%d).\n",
        (void *)symbol, symbol ? (int)symbol->type : -1);
    return;
  }
  hashmap_put(env->bindings, symbol, value);
}

void init_runtime_environment_and_symbols() {
  init_symbol_table();
  init_global_environment();
}
void init_global_environment(void) {
  if (current_environment) {
    printf("WARN: Global environment already initialized?\n"); // This is a
                                                               // WARN, kept.
    return;
  }
  if (!global_symbol_table) {
    fprintf(
        stderr,
        "ERROR: Symbol table must be initialized before global environment!\n");
    init_symbol_table();
  }

  current_environment = new_environment(NULL);
#ifdef DEBUG_ENV
  printf("DEBUG: Global environment created at %p.\n",
         (void *)current_environment);
#endif

  g_current_environment = current_environment;
#ifdef DEBUG_ENV
  printf("DEBUG: Set g_current_environment (at %p) to %p\n",
         (void *)&g_current_environment, (void *)g_current_environment);

  printf("DEBUG: Defining primitives in global environment...\n");
#endif

  extern SchemeObject *plus(SchemeObject * a, SchemeObject * b);
  extern SchemeObject *multiply(SchemeObject * a, SchemeObject * b);
  extern void display(SchemeObject * obj);
  extern void newline(void);
  extern SchemeObject *make_function(void *code, SchemeEnvironment *env);
  extern SchemeObject *equal(SchemeObject * a, SchemeObject * b);
  env_define(current_environment, intern_symbol("+"),
             make_function((void *)&plus, NULL));
  env_define(current_environment, intern_symbol("*"),
             make_function((void *)&multiply, NULL));
  env_define(current_environment, intern_symbol("display"),
             make_function((void *)&display, NULL));
  env_define(current_environment, intern_symbol("newline"),
             make_function((void *)&newline, NULL));
  env_define(current_environment, intern_symbol("="),
             make_function((void *)&equal, NULL));

#ifdef DEBUG_ENV
  printf("DEBUG: Global environment initialization complete.\n");
#endif
}
void cleanup_environment(SchemeEnvironment *env) {
  if (!env)
    return;
  hashmap_destroy(env->bindings);
  free(env);
}
