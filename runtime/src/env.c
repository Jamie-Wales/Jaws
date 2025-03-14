// env.c
#include "../include/env.h"
#include "../include/gc.h"
#include <stdlib.h>
#include <string.h>

#define INITIAL_CAPACITY 75
#define TABLE_MAX_LOAD 100

static uint32_t hash_ptr(const void* ptr)
{
    return (uint32_t)((uintptr_t)ptr >> 3);
}

static void hashmap_resize(HashMap* map, size_t new_capacity)
{
    Entry** new_entries = calloc(new_capacity, sizeof(Entry*));
    for (size_t i = 0; i < map->capacity; i++) {
        Entry* entry = map->entries[i];
        while (entry != NULL) {
            Entry* next = entry->next;
            uint32_t hash = hash_ptr(entry->key) % new_capacity;
            entry->next = new_entries[hash];
            new_entries[hash] = entry;
            entry = next;
        }
    }
    free(map->entries);
    map->entries = new_entries;
    map->capacity = new_capacity;
}

HashMap* hashmap_new(void)
{
    HashMap* map = malloc(sizeof(HashMap));
    if (!map) {
        fprintf(stderr, "Failed to allocate hashmap\n");
        exit(1);
    }
    map->capacity = INITIAL_CAPACITY;
    map->count = 0;
    map->entries = calloc(INITIAL_CAPACITY, sizeof(Entry*));
    if (!map->entries) {
        fprintf(stderr, "Failed to allocate hashmap entries\n");
        free(map);
        exit(1);
    }
    return map;
}

void hashmap_destroy(HashMap* map)
{
    if (!map)
        return;
    for (size_t i = 0; i < map->capacity; i++) {
        Entry* entry = map->entries[i];
        while (entry != NULL) {
            Entry* next = entry->next;
            free(entry);
            entry = next;
        }
    }
    free(map->entries);
    free(map);
}

static SchemeObject* hashmap_get(HashMap* map, SchemeObject* key)
{
    uint32_t hash = hash_ptr(key) % map->capacity;
    Entry* entry = map->entries[hash];
    while (entry != NULL) {
        if (entry->key == key) {
            return entry->value;
        }
        entry = entry->next;
    }
    return NULL;
}

static void hashmap_put(HashMap* map, SchemeObject* key, SchemeObject* value)
{
    if (map->count >= map->capacity * TABLE_MAX_LOAD) {
        hashmap_resize(map, map->capacity * 2);
    }

    uint32_t hash = hash_ptr(key) % map->capacity;
    Entry* entry = map->entries[hash];

    while (entry != NULL) {
        if (entry->key == key) {
            entry->value = value;
            return;
        }
        entry = entry->next;
    }

    entry = malloc(sizeof(Entry));
    if (!entry) {
        fprintf(stderr, "Failed to allocate hashmap entry\n");
        exit(1);
    }
    entry->key = key;
    entry->value = value;
    entry->next = map->entries[hash];
    map->entries[hash] = entry;
    map->count++;
}

SchemeObject* intern_symbol(const char* name)
{
    return NULL;
}

SchemeObject* env_lookup(SchemeEnvironment* env, SchemeObject* symbol)
{
    while (env != NULL) {
        SchemeObject* value = hashmap_get(env->bindings, symbol);
        if (value != NULL) {
            return value;
        }
        env = env->enclosing;
    }
    return NULL;
}

void env_define(SchemeEnvironment* env, SchemeObject* symbol, SchemeObject* value)
{
    hashmap_put(env->bindings, symbol, value);
}

SchemeEnvironment* new_environment(SchemeEnvironment* enclosing)
{
    SchemeEnvironment* env = malloc(sizeof(SchemeEnvironment));
    if (!env) {
        fprintf(stderr, "Failed to allocate environment\n");
        exit(1);
    }
    env->enclosing = enclosing;
    env->bindings = hashmap_new();
    return env;
}

void init_global_environment(void)
{
}
void cleanup_environment(SchemeEnvironment* env)
{
    if (!env)
        return;
    hashmap_destroy(env->bindings);
    free(env);
}
