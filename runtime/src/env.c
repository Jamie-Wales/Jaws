#include "../include/env.h"
#include "../include/gc.h"
#include "../include/types.h"
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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
  if (!map || !key) {
    fprintf(stderr,
            "ERROR: hashmap_put called with NULL map (%p) or key (%p).\n",
            (void *)map, (void *)key);
    if (!key)
      fprintf(stderr, "       (Value being put was: %p)\n", (void *)value);
    return;
  }
  if ((float)(map->count + 1) / map->capacity >= TABLE_MAX_LOAD) {
    hashmap_resize(map, map->capacity * 2);
    uint32_t hash_after_resize = hash_symbol_ptr(key) % map->capacity;
    Entry *entry_after_resize = map->entries[hash_after_resize];
    while (entry_after_resize != NULL) {
      if (entry_after_resize->key == key) {
        entry_after_resize->value = value;
        return;
      }
      entry_after_resize = entry_after_resize->next;
    }
  }
  uint32_t hash = hash_symbol_ptr(key) % map->capacity;
  Entry *entry = map->entries[hash];
  while (entry != NULL) {
    if (entry->key == key) {
      entry->value = value;
      return;
    }
    entry = entry->next;
  }
  Entry *new_entry = malloc(sizeof(Entry));
  if (!new_entry) {
    fprintf(stderr, "ERROR: Failed to allocate hashmap entry\n");
    return;
  }
  new_entry->key = key;
  new_entry->value = value;
  new_entry->next = map->entries[hash];
  map->entries[hash] = new_entry;
  map->count++;
}

void init_symbol_table() {
  if (!global_symbol_table) {
    global_symbol_table = hashmap_new();
    printf("DEBUG: Global symbol table initialized.\n");
  } else {
    printf("WARN: Symbol table already initialized.\n");
  }
}
void destroy_symbol_table() {
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
SchemeObject *intern_symbol(const char *name) {
  if (!global_symbol_table) {
    fprintf(stderr, "ERROR: Symbol table not initialized! Call "
                    "init_symbol_table() first.\n");
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

SchemeEnvironment *new_environment(SchemeEnvironment *enclosing) {
  SchemeEnvironment *env = malloc(sizeof(SchemeEnvironment));
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
void cleanup_environment(SchemeEnvironment *env) {
  if (!env)
    return;
  hashmap_destroy(env->bindings);
  free(env);
}
