#include "../include/gc.h"
#include "../include/env.h"
#include "../include/types.h"

#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define HEAP_OBJECT_COUNT 200
#define MIN_HEAP_SIZE (sizeof(SchemeObject) * 50)
#define INITIAL_HEAP (sizeof(SchemeObject) * HEAP_OBJECT_COUNT)
#define HEAP_GROWTH_FACTOR 1.25
#define HEAP_SHRINK_THRESHOLD 0.25

#define GC_MARK_BIT 0x80

#ifndef MAX
#define MAX(a, b) ((a) > (b) ? (a) : (b))
#endif

static void *heap = NULL;
static size_t heap_size = 0;
static size_t used = 0;
static void *stack_bottom_approx = NULL;

static size_t last_gc_used = 0;
static int gc_count = 0;

typedef struct {
  SchemeObject **entries;
  size_t capacity;
  size_t mask;
} ForwardingMap;

static ForwardingMap *forwarding_map_create(size_t estimated_items) {
  size_t cap = 1;
  if (estimated_items == 0)
    estimated_items = 1;
  while (cap < estimated_items)
    cap <<= 1;
  if (cap == 0)
    cap = 1;

  ForwardingMap *map = (ForwardingMap *)malloc(sizeof(ForwardingMap));
  if (!map) {
    perror("Failed to allocate ForwardingMap structure");
    return NULL;
  }
  map->entries = (SchemeObject **)calloc(cap * 2, sizeof(SchemeObject *));
  if (!map->entries) {
    perror("Failed to allocate entries for ForwardingMap");
    free(map);
    return NULL;
  }
  map->capacity = cap;
  map->mask = cap - 1;
  return map;
}

static void forwarding_map_destroy(ForwardingMap *map) {
  if (map) {
    free(map->entries);
    free(map);
  }
}

static void forwarding_map_put(ForwardingMap *map, SchemeObject *old_addr,
                               SchemeObject *new_addr) {
  if (!map || !map->entries)
    return;
  if (!old_addr) {
    return;
  }
  size_t hash = ((uintptr_t)old_addr >> 3) & map->mask;
  size_t probe_count = 0;

  while (map->entries[hash] != NULL && map->entries[hash] != old_addr) {
    hash = (hash + 1) & map->mask;
    probe_count++;
    if (probe_count >= map->capacity) {
      fprintf(stderr,
              "Error: ForwardingMap full or collision loop in put for old_addr "
              "%p.\n",
              (void *)old_addr);
      return;
    }
  }
  map->entries[hash] = old_addr;
  map->entries[hash + map->capacity] = new_addr;
}

static SchemeObject *forwarding_map_get(ForwardingMap *map,
                                        SchemeObject *old_addr) {
  if (!map || !map->entries || !old_addr)
    return old_addr;

  size_t hash = ((uintptr_t)old_addr >> 3) & map->mask;
  size_t probe_count = 0;

  while (map->entries[hash] != NULL) {
    if (map->entries[hash] == old_addr) {
      return map->entries[hash + map->capacity];
    }
    hash = (hash + 1) & map->mask;
    probe_count++;
    if (probe_count >= map->capacity) {
      break;
    }
  }
  return old_addr;
}

static void try_shrink_heap();
static void grow_heap();
void mark_object(SchemeObject *obj);
void mark_environment(SchemeEnvironment *env);
void mark_roots();
void sweep_compact();

extern char *to_string(SchemeObject *object);
extern SchemeEnvironment *g_current_environment;
extern HashMap *global_symbol_table;

static size_t calculate_heap_size(size_t current_heap_size,
                                  size_t current_used_after_gc) {
  size_t min_size = MAX(current_used_after_gc * 2, MIN_HEAP_SIZE);
  if (min_size == 0 && current_used_after_gc == 0)
    min_size = MIN_HEAP_SIZE;

  double allocation_since_last_gc = 0;
  if (gc_count > 0 && current_used_after_gc >= last_gc_used) {
    allocation_since_last_gc = (double)(current_used_after_gc - last_gc_used);
  }

  double growth_factor = HEAP_GROWTH_FACTOR;

  size_t target_size_based_on_growth =
      (size_t)(current_heap_size * growth_factor);
  if (target_size_based_on_growth <= current_heap_size &&
      current_heap_size > 0) {
    target_size_based_on_growth =
        current_heap_size + MAX(INITIAL_HEAP, current_used_after_gc);
  }

  size_t calculated_target = MAX(min_size, target_size_based_on_growth);

#ifdef DEBUG_GC
  printf("DEBUG: calculate_heap_size: current_used_after_gc=%zu, "
         "last_gc_used=%zu, gc_count=%d, alloc_since_last_gc=%.0f, "
         "growth_factor=%.1f, calculated_target=%zu\n",
         current_used_after_gc, last_gc_used, gc_count,
         allocation_since_last_gc, growth_factor, calculated_target);
#endif
  return calculated_target;
}

void print_heap() {
#ifdef DEBUG_GC
  printf("\n=== HEAP CONTENTS ===\n");
  printf("Total heap size: %zu bytes\n", heap_size);
  printf("Used: %zu bytes\n", used);
  SchemeObject *current = (SchemeObject *)heap;
  size_t count = 0;
  uintptr_t current_heap_end = (uintptr_t)heap + used;
  while ((uintptr_t)current < current_heap_end) {
    SchemeType base_type = current->type & ~GC_MARK_BIT;
    bool is_marked = (current->type & GC_MARK_BIT) != 0;

    char temp_str_val[256] = "<INVALID_OR_UNPRINTABLE>";
    if (base_type == TYPE_NUMBER) {
      snprintf(temp_str_val, sizeof(temp_str_val), "%lld",
               current->value.number);
    } else if (base_type == TYPE_SYMBOL && current->value.symbol != NULL) {
      strncpy(temp_str_val, current->value.symbol, sizeof(temp_str_val) - 1);
      temp_str_val[sizeof(temp_str_val) - 1] = '\0';
    } else if (base_type == TYPE_NIL) {
      strcpy(temp_str_val, "()");
    } else if (base_type == TYPE_BOOL) {
      strcpy(temp_str_val, current->value.boolean ? "#t" : "#f");
    } else if (base_type == TYPE_FUNCTION) {
      strcpy(temp_str_val, "#<procedure>");
    } else if (base_type == TYPE_PAIR) {
      snprintf(temp_str_val, sizeof(temp_str_val), "Pair@%p", (void *)current);
    }

    printf("Object %zu at %p: Type=%d%s Value=%s\n", count++, (void *)current,
           (int)base_type, is_marked ? " (M)" : "", temp_str_val);

    if (sizeof(SchemeObject) == 0) {
      fprintf(stderr, "ERROR: sizeof(SchemeObject) is zero in print_heap!\n");
      break;
    }
    current = (SchemeObject *)((char *)current + sizeof(SchemeObject));
  }
  printf("===================\n\n");
  fflush(stdout);
#endif
}

static void try_shrink_heap() {
  size_t new_potential_size = MAX(used * 2, MIN_HEAP_SIZE);
  if (new_potential_size < MIN_HEAP_SIZE)
    new_potential_size = MIN_HEAP_SIZE;

  if (heap_size > MIN_HEAP_SIZE && heap_size > new_potential_size &&
      (double)used / heap_size < HEAP_SHRINK_THRESHOLD) {
#ifdef DEBUG_GC
    printf("DEBUG: Attempting heap shrink from %zu to %zu (used %zu, threshold "
           "%.2f)\n",
           heap_size, new_potential_size, used, HEAP_SHRINK_THRESHOLD);
#endif
    void *new_heap = realloc(heap, new_potential_size);
    if (new_heap || new_potential_size == 0) {
      heap = new_heap;
      heap_size = new_potential_size;
#ifdef DEBUG_GC
      printf("DEBUG: Heap shrunk to %zu bytes\n", heap_size);
#endif
    } else {
      fprintf(stderr,
              "WARN: realloc failed during heap shrink (ignored, keeping "
              "current size %zu)\n",
              heap_size);
    }
  }
}

static void grow_heap() {
  size_t new_size = calculate_heap_size(heap_size, used);
  if (new_size <= heap_size && used > 0) {
    new_size = MAX(heap_size + INITIAL_HEAP, used * 2);
    if (new_size < MIN_HEAP_SIZE)
      new_size = MIN_HEAP_SIZE;
#ifdef DEBUG_GC
    printf("DEBUG: grow_heap: calculated_heap_size was not larger. Using "
           "fallback growth to %zu\n",
           new_size);
#endif
  } else if (used == 0 && new_size < INITIAL_HEAP) {
    new_size = INITIAL_HEAP;
  }

#ifdef DEBUG_GC
  printf("DEBUG: Attempting heap grow from %zu to %zu (live data: %zu)\n",
         heap_size, new_size, used);
#endif
  void *new_heap = realloc(heap, new_size);
  if (!new_heap && new_size > 0) {
    fprintf(stderr, "FATAL: Out of memory! Failed to grow heap to %zu bytes!\n",
            new_size);
    exit(EXIT_FAILURE);
  }
  heap = new_heap;
  heap_size = new_size;
  last_gc_used = used;
#ifdef DEBUG_GC
  printf("DEBUG: Heap grown to %zu bytes. last_gc_used updated to %zu.\n",
         heap_size, last_gc_used);
#endif
}

SchemeObject *alloc_object() {
  size_t object_size = sizeof(SchemeObject);
  if (object_size == 0) {
    fprintf(stderr, "FATAL: sizeof(SchemeObject) is 0!\n");
    exit(EXIT_FAILURE);
  }

  if (used + object_size > heap_size) {
#ifdef DEBUG_GC
    printf("DEBUG: Heap full (used %zu + object_size %zu > heap_size %zu). "
           "Triggering GC.\n",
           used, object_size, heap_size);
#endif
    gc();
    if (used + object_size > heap_size) {
#ifdef DEBUG_GC
      printf("DEBUG: Heap still full after GC (used %zu). Attempting to grow "
             "heap.\n",
             used);
#endif
      grow_heap();
      if (used + object_size > heap_size) {
        fprintf(stderr,
                "FATAL: Out of memory even after GC and heap growth! Cannot "
                "allocate object of size %zu. Heap size: %zu, Used: %zu\n",
                object_size, heap_size, used);
        exit(EXIT_FAILURE);
      }
    }
  }
  SchemeObject *obj = (SchemeObject *)((char *)heap + used);
  memset(obj, 0, object_size);
  used += object_size;
#ifdef DEBUG_GC

#endif
  return obj;
}

SchemeObject *allocate(SchemeType type, int64_t immediate_val) {

  SchemeObject *obj = alloc_object();
  obj->type = type & ~GC_MARK_BIT;

  switch (obj->type) {
  case TYPE_NUMBER:
    obj->value.number = immediate_val;
    break;
  case TYPE_SYMBOL:
    fprintf(stderr, "WARN: Direct allocation of TYPE_SYMBOL via allocate() is "
                    "unusual. Use intern_symbol.\n");
    obj->value.symbol = (const char *)immediate_val;
    break;
  case TYPE_PAIR:
    fprintf(stderr, "WARN: Direct allocation of TYPE_PAIR via allocate() is "
                    "unusual. Use allocate_pair.\n");
    obj->value.pair.car = NULL;
    obj->value.pair.cdr = NULL;
    break;
  case TYPE_BOOL:
    obj->value.boolean = (immediate_val != 0);
    break;
  case TYPE_FUNCTION:
    obj->value.function.code = NULL;
    obj->value.function.env = NULL;
    break;
  case TYPE_STRING:
    fprintf(stderr, "WARN: Direct allocation of TYPE_STRING via allocate() is "
                    "unusual. Use make_string.\n");
    obj->value.string = (const char *)immediate_val;
    break;
  case TYPE_NIL:
    break;
  default:
    fprintf(stderr, "ERROR: Invalid type %d passed to allocate\n", obj->type);
    obj->type = TYPE_NIL;
    return obj;
  }
  return obj;
}

SchemeObject *allocate_pair(SchemeObject *car, SchemeObject *cdr) {
  SchemeObject *obj = alloc_object();
  obj->type = TYPE_PAIR;
  obj->value.pair.car = car;
  obj->value.pair.cdr = cdr;
  return obj;
}

void debug_check_struct_sizes() {
#ifdef DEBUG_RUNTIME
  printf("\nDEBUG: STRUCT SIZE INFORMATION...\n");
  printf("DEBUG: sizeof(SchemeObject) = %zu\n", sizeof(SchemeObject));
  printf("DEBUG: Heap address: %p\n", heap);
  printf("DEBUG: STRUCT CHECK COMPLETE\n\n");
  fflush(stdout);
#endif
}

extern void init_symbol_table(void);
extern void init_global_environment(void);

void init_runtime() {
  volatile uintptr_t dummy_on_stack_for_bottom;
  stack_bottom_approx = (void *)&dummy_on_stack_for_bottom;

  if (!heap) {
    heap = malloc(INITIAL_HEAP);
    if (!heap) {
      fprintf(stderr, "FATAL: Failed to initialize heap with malloc!\n");
      exit(EXIT_FAILURE);
    }
    heap_size = INITIAL_HEAP;
    used = 0;
    last_gc_used = 0;
    gc_count = 0;
  }

#ifdef DEBUG_RUNTIME
  printf("Runtime initialized with heap size %zu bytes\n", heap_size);
  printf("Approx stack bottom: %p\n", stack_bottom_approx);
#endif
  debug_check_struct_sizes();

#ifdef DEBUG_RUNTIME
  printf("DEBUG: Initializing symbol table and global environment...\n");
  fflush(stdout);
#endif
  init_symbol_table();
  init_global_environment();
#ifdef DEBUG_RUNTIME
  printf("DEBUG: Symbol table and global environment initialized.\n");
  fflush(stdout);
#endif
}

void cleanup_runtime() {
  if (heap) {
    free(heap);
    heap = NULL;
  }
  heap_size = 0;
  used = 0;
  stack_bottom_approx = NULL;
#ifdef DEBUG_RUNTIME
  printf("DEBUG: Runtime cleaned up (heap freed).\n");
#endif
}

void mark_object(SchemeObject *obj) {
  if (!obj || (uintptr_t)obj < (uintptr_t)heap ||
      (uintptr_t)obj >= ((uintptr_t)heap + used)) {
    return;
  }

  if (((uintptr_t)obj - (uintptr_t)heap) % sizeof(SchemeObject) != 0) {
    return;
  }

  if (obj->type & GC_MARK_BIT) {
    return;
  }

  obj->type |= GC_MARK_BIT;

  SchemeType base_type = obj->type & ~GC_MARK_BIT;
  switch (base_type) {
  case TYPE_PAIR:
    mark_object(obj->value.pair.car);
    mark_object(obj->value.pair.cdr);
    break;
  case TYPE_FUNCTION:
    mark_environment(obj->value.function.env);
    break;
  case TYPE_STRING:
    break;
  case TYPE_NUMBER:
  case TYPE_BOOL:
  case TYPE_NIL:
  case TYPE_SYMBOL:
  default:
    break;
  }
}

void mark_environment(SchemeEnvironment *env) {
  SchemeEnvironment *current_env_iter = env;
  while (current_env_iter != NULL) {
    if (current_env_iter->bindings) {
      for (size_t i = 0; i < current_env_iter->bindings->capacity; i++) {
        Entry *entry = current_env_iter->bindings->entries[i];
        while (entry != NULL) {
          mark_object(entry->key);
          mark_object(entry->value);
          entry = entry->next;
        }
      }
    }
    current_env_iter = current_env_iter->enclosing;
  }
}

void mark_roots() {
#ifdef DEBUG_GC
  printf("DEBUG: Marking roots...\n");
  fflush(stdout);
#endif
  mark_environment(g_current_environment);

  if (global_symbol_table) {
    for (size_t i = 0; i < global_symbol_table->capacity; i++) {
      Entry *entry = global_symbol_table->entries[i];
      while (entry != NULL) {
        mark_object(entry->key);
        entry = entry->next;
      }
    }
  }

  void *stack_top_approx;
  volatile uintptr_t dummy_on_stack_top;
  stack_top_approx = (void *)&dummy_on_stack_top;

  if (!stack_bottom_approx) {
    fprintf(stderr,
            "ERROR: Stack bottom not initialized for GC! Cannot scan stack.\n");
    return;
  }

#ifdef DEBUG_GC
  printf("DEBUG: Scanning stack conservatively from approx top %p to approx "
         "bottom %p\n",
         stack_top_approx, stack_bottom_approx);
  fflush(stdout);
#endif

  uintptr_t scan_start, scan_end;
  if ((uintptr_t)stack_top_approx < (uintptr_t)stack_bottom_approx) {
    scan_start = (uintptr_t)stack_top_approx;
    scan_end = (uintptr_t)stack_bottom_approx;
  } else {
    scan_start = (uintptr_t)stack_bottom_approx;
    scan_end = (uintptr_t)stack_top_approx;
  }

  scan_start = scan_start & ~(sizeof(void *) - 1);

  uintptr_t heap_start_addr = (uintptr_t)heap;
  uintptr_t heap_end_addr = heap_start_addr + used;

  for (uintptr_t p = scan_start; p < scan_end; p += sizeof(void *)) {
    SchemeObject *potential_obj_ptr = *(SchemeObject **)p;
    if ((uintptr_t)potential_obj_ptr >= heap_start_addr &&
        (uintptr_t)potential_obj_ptr < heap_end_addr) {
      mark_object(potential_obj_ptr);
    }
  }
#ifdef DEBUG_GC
  printf("DEBUG: Stack scan complete.\n");
  fflush(stdout);
#endif
}

void sweep_compact() {
  uintptr_t current_heap_phys_end = (uintptr_t)heap + used;
  size_t live_object_count = 0;

  if (used == 0) {
#ifdef DEBUG_GC
    printf("DEBUG: Sweep/Compact: Heap is empty.\n");
#endif
    return;
  }
  if (sizeof(SchemeObject) == 0) {
    fprintf(stderr, "FATAL: sizeof(SchemeObject) is 0 in sweep_compact!\n");
    return;
  }

  size_t estimated_live_objects = used / sizeof(SchemeObject);
  ForwardingMap *fwd_map = forwarding_map_create(
      estimated_live_objects > 0 ? estimated_live_objects : 1);
  if (!fwd_map) {
    fprintf(stderr,
            "ERROR: Failed to create forwarding map. Aborting compaction.\n");
    uintptr_t scan_ptr_unmark = (uintptr_t)heap;
    while (scan_ptr_unmark < current_heap_phys_end) {
      SchemeObject *obj_to_unmark = (SchemeObject *)scan_ptr_unmark;
      obj_to_unmark->type &= ~GC_MARK_BIT;
      scan_ptr_unmark += sizeof(SchemeObject);
    }
    return;
  }

  uintptr_t scan_ptr = (uintptr_t)heap;
  uintptr_t write_ptr = (uintptr_t)heap;

  while (scan_ptr < current_heap_phys_end) {
    SchemeObject *current_obj = (SchemeObject *)scan_ptr;
    if (current_obj->type & GC_MARK_BIT) {
      forwarding_map_put(fwd_map, current_obj, (SchemeObject *)write_ptr);
      write_ptr += sizeof(SchemeObject);
      live_object_count++;
    }
    scan_ptr += sizeof(SchemeObject);
  }
#ifdef DEBUG_GC
  printf("DEBUG: Sweep/Compact Pass 1: New addresses calculated. %zu live "
         "objects.\n",
         live_object_count);
#endif

  scan_ptr = (uintptr_t)heap;
  while (scan_ptr < current_heap_phys_end) {
    SchemeObject *current_obj_at_old_loc = (SchemeObject *)scan_ptr;
    if (current_obj_at_old_loc->type & GC_MARK_BIT) {
      SchemeType base_type = current_obj_at_old_loc->type & ~GC_MARK_BIT;
      switch (base_type) {
      case TYPE_PAIR:
        current_obj_at_old_loc->value.pair.car =
            forwarding_map_get(fwd_map, current_obj_at_old_loc->value.pair.car);
        current_obj_at_old_loc->value.pair.cdr =
            forwarding_map_get(fwd_map, current_obj_at_old_loc->value.pair.cdr);
        break;
      case TYPE_FUNCTION: {
        SchemeEnvironment *fn_env = current_obj_at_old_loc->value.function.env;
        SchemeEnvironment *env_iter = fn_env;
        while (env_iter != NULL) {
          if (env_iter->bindings) {
            for (size_t k = 0; k < env_iter->bindings->capacity; k++) {
              Entry *map_entry = env_iter->bindings->entries[k];
              while (map_entry != NULL) {
                map_entry->key = forwarding_map_get(fwd_map, map_entry->key);
                map_entry->value =
                    forwarding_map_get(fwd_map, map_entry->value);
                map_entry = map_entry->next;
              }
            }
          }
          env_iter = env_iter->enclosing;
        }
      } break;
      default:
        break;
      }
    }
    scan_ptr += sizeof(SchemeObject);
  }

  SchemeEnvironment *env_chain_iter = g_current_environment;
  while (env_chain_iter != NULL) {
    if (env_chain_iter->bindings) {
      for (size_t k = 0; k < env_chain_iter->bindings->capacity; k++) {
        Entry *map_entry = env_chain_iter->bindings->entries[k];
        while (map_entry != NULL) {
          map_entry->key = forwarding_map_get(fwd_map, map_entry->key);
          map_entry->value = forwarding_map_get(fwd_map, map_entry->value);
          map_entry = map_entry->next;
        }
      }
    }
    env_chain_iter = env_chain_iter->enclosing;
  }
  if (global_symbol_table) {
    for (size_t i = 0; i < global_symbol_table->capacity; i++) {
      Entry *entry = global_symbol_table->entries[i];
      while (entry != NULL) {
        entry->key = forwarding_map_get(fwd_map, entry->key);
        entry = entry->next;
      }
    }
  }

#ifdef DEBUG_GC
  printf("DEBUG: Sweep/Compact Pass 2 & 2.1: Pointers updated (live objects & "
         "roots).\n");
#endif

  scan_ptr = (uintptr_t)heap;
  while (scan_ptr < current_heap_phys_end) {
    SchemeObject *current_obj_at_old_loc = (SchemeObject *)scan_ptr;
    if (current_obj_at_old_loc->type & GC_MARK_BIT) {
      SchemeObject *new_addr =
          forwarding_map_get(fwd_map, current_obj_at_old_loc);
      if (new_addr && new_addr != current_obj_at_old_loc) {
        if ((uintptr_t)new_addr >= (uintptr_t)heap &&
            ((uintptr_t)new_addr + sizeof(SchemeObject)) <=
                ((uintptr_t)heap + heap_size)) {
          memmove(new_addr, current_obj_at_old_loc, sizeof(SchemeObject));
        } else {
          fprintf(stderr, "ERROR GC: new_addr %p out of bounds for memmove!\n",
                  (void *)new_addr);
        }
      }
      SchemeObject *final_location =
          (new_addr && (uintptr_t)new_addr >= (uintptr_t)heap)
              ? new_addr
              : current_obj_at_old_loc;
      final_location->type &= ~GC_MARK_BIT;
    }
    scan_ptr += sizeof(SchemeObject);
  }

#ifdef DEBUG_GC
  printf(
      "DEBUG: Sweep/Compact Pass 3: Moving objects and unmarking complete.\n");
#endif

  size_t new_used_bytes = live_object_count * sizeof(SchemeObject);
#ifdef DEBUG_GC
  printf("DEBUG: Sweep/Compact finished. Live objects: %zu (%zu bytes)\n",
         live_object_count, new_used_bytes);
  printf("DEBUG: Old heap usage: %zu bytes. New heap usage: %zu bytes\n", used,
         new_used_bytes);
  if (used >= new_used_bytes) {
    printf("DEBUG: Freed (approx): %zu bytes\n", used - new_used_bytes);
  }
#endif
  used = new_used_bytes;

  forwarding_map_destroy(fwd_map);

  if (heap_size > MIN_HEAP_SIZE && used > 0 && heap_size > 0 &&
      (double)used / heap_size < HEAP_SHRINK_THRESHOLD) {
    try_shrink_heap();
  }
#ifdef DEBUG_GC
  if (heap_size > 0)
    printf("DEBUG: Heap utilization: %.1f%%\n", (used * 100.0) / heap_size);
  else
    printf("DEBUG: Heap size is 0, utilization undefined.\n");
#endif
}

void gc() {
#ifdef DEBUG_GC
  printf("\n--- GC START (Count: %d) ---\n", gc_count + 1);
  fflush(stdout);
#endif
  gc_count++;

  mark_roots();

#ifdef DEBUG_GC
  printf("--- Marking Complete (GC Count: %d) ---\n", gc_count);
  print_heap();
  fflush(stdout);
#endif

  sweep_compact();

#ifdef DEBUG_GC
  printf("--- GC Complete (GC Count: %d) ---\n", gc_count);
  print_heap();
  fflush(stdout);
#endif
}

void *getCodePointer(SchemeObject *obj) {
#ifdef DEBUG_RUNTIME

#endif

  if (obj && (obj->type & ~GC_MARK_BIT) == TYPE_FUNCTION) {
    return obj->value.function.code;
  }
  return NULL;
}

SchemeEnvironment *setup_call_environment(SchemeObject *closure_obj) {
#ifdef DEBUG_RUNTIME

#endif

  if (!closure_obj) {
    fprintf(
        stderr,
        "FATAL ERROR: setup_call_environment - received NULL closure_obj.\n");
    exit(EXIT_FAILURE);
  }

  if ((closure_obj->type & ~GC_MARK_BIT) != TYPE_FUNCTION) {
    fprintf(stderr,
            "FATAL ERROR: setup_call_environment - closure_obj is not a "
            "function. Type: %d\n",
            (int)(closure_obj->type & ~GC_MARK_BIT));
    return NULL;
  }

  SchemeEnvironment *captured_env = closure_obj->value.function.env;
#ifdef DEBUG_RUNTIME

#endif

  SchemeEnvironment *call_env = new_environment(captured_env);
  g_current_environment = call_env;
#ifdef DEBUG_RUNTIME

#endif
  return call_env;
}

void restore_call_environment(SchemeEnvironment *active_call_env_from_setup) {
#ifdef DEBUG_RUNTIME

#endif

  if (!active_call_env_from_setup) {
#ifdef DEBUG_RUNTIME

#endif
    if (g_current_environment != NULL) {
      g_current_environment = g_current_environment->enclosing;
    }
    return;
  }
  SchemeEnvironment *env_to_restore_to = active_call_env_from_setup->enclosing;
#ifdef DEBUG_RUNTIME

#endif
  g_current_environment = env_to_restore_to;
}
