// gc.c
#include "../include/gc.h"
#include "../include/env.h" // Need this for mark_environment, SchemeEnvironment, HashMap
#include <stddef.h>
#include <stdint.h> // For uintptr_t
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define HEAP_OBJECT_COUNT 100
#define MIN_HEAP_SIZE (sizeof(SchemeObject) * 50)
#define INITIAL_HEAP (sizeof(SchemeObject) * HEAP_OBJECT_COUNT)
#define HEAP_GROWTH_FACTOR 2
#define HEAP_SHRINK_THRESHOLD 0.25
#define DEBUG_GC
#define GC_MARK_BIT                                                            \
  0x80 // Define the mark bit (e.g., most significant bit of type)

// --- Global GC State ---
static void *heap = NULL;
static size_t heap_size = 0;
static size_t used = 0;
static void *stack_bottom_approx = NULL;

// Structure to hold forwarding address mapping during GC
typedef struct {
  SchemeObject *old_address;
  SchemeObject *new_address;
} ForwardingInfo;

// --- Forward Declarations ---
static void try_shrink_heap();
static void grow_heap();
void mark_object(SchemeObject *obj);
void mark_environment(SchemeEnvironment *env);
void mark_roots();
void sweep_compact(); // Now implements 3-pass compaction
extern char *to_string(SchemeObject *object);
extern SchemeEnvironment *current_environment;
extern HashMap *global_symbol_table;

void print_heap() {
  printf("\n=== HEAP CONTENTS ===\n");
  printf("Total heap size: %zu bytes\n", heap_size);
  printf("Used: %zu bytes\n", used);
  SchemeObject *current = (SchemeObject *)heap;
  size_t count = 0;
  uintptr_t current_heap_end = (uintptr_t)heap + used;
  while ((uintptr_t)current < current_heap_end) {
    // Check type validity before printing to avoid crashing on potential
    // garbage
    SchemeType base_type = current->type & ~GC_MARK_BIT;
    bool is_marked = (current->type & GC_MARK_BIT) != 0;
    const char *type_str = "<INVALID>";
    // Simple type check based on known enum values (adjust if needed)
    if (base_type >= TYPE_NUMBER && base_type <= TYPE_BOOL) {
      type_str = to_string(current); // Use existing to_string
    }
    printf("\nObject %zu at %p: Type=%d%s Value=%s", count++, (void *)current,
           (int)base_type, is_marked ? " (M)" : "", type_str);
    if (type_str != current->value.symbol &&
        base_type != TYPE_SYMBOL) { // Avoid double free if to_string returns
                                    // internal symbol pointer
      // free(type_str); // Only free if to_string allocates fresh memory
    }
    current = (SchemeObject *)((char *)current + sizeof(SchemeObject));
  }
  printf("\n===================\n\n");
}

static void try_shrink_heap() {
  size_t min_required =
      used > 0 ? used * 2 : MIN_HEAP_SIZE; // Ensure minimum size
  if (min_required < MIN_HEAP_SIZE)
    min_required = MIN_HEAP_SIZE;

  if (heap_size > min_required) {
#ifdef DEBUG_GC
    printf("DEBUG: Attempting heap shrink from %zu to %zu (used %zu)\n",
           heap_size, min_required, used);
#endif
    void *new_heap = realloc(heap, min_required);
    if (new_heap || min_required == 0) { // realloc(ptr, 0) is like free(ptr)
      heap = new_heap;
      heap_size = min_required;
#ifdef DEBUG_GC
      printf("DEBUG: Heap shrunk to %zu bytes\n", heap_size);
#endif
    } else {
      fprintf(stderr, "WARN: realloc failed during heap shrink (ignored)\n");
    }
  }
}

static void grow_heap() {
  size_t new_size =
      (heap_size == 0) ? INITIAL_HEAP : heap_size * HEAP_GROWTH_FACTOR;
  if (new_size < MIN_HEAP_SIZE)
    new_size = MIN_HEAP_SIZE; // Ensure minimum size on first grow too
#ifdef DEBUG_GC
  printf("DEBUG: Attempting heap grow from %zu to %zu\n", heap_size, new_size);
#endif
  void *new_heap = realloc(heap, new_size);
  if (!new_heap) {
    fprintf(stderr, "FATAL: Out of memory! Failed to grow heap to %zu bytes!\n",
            new_size);
    exit(1);
  }
  heap = new_heap;
  heap_size = new_size;
#ifdef DEBUG_GC
  printf("DEBUG: Heap grown to %zu bytes\n", heap_size);
#endif
}

SchemeObject *alloc_object() {
  size_t size = sizeof(SchemeObject);
  if (used + size > heap_size) {
#ifdef DEBUG_GC
    printf(
        "DEBUG: Heap full trigger GC (used %zu + size %zu > heap_size %zu)\n",
        used, size, heap_size);
#endif
    gc();
    if (used + size > heap_size) {
#ifdef DEBUG_GC
      printf("DEBUG: Growing heap after GC (used %zu + size %zu > heap_size "
             "%zu)\n",
             used, size, heap_size);
#endif
      grow_heap();
      if (used + size > heap_size) {
        fprintf(stderr,
                "FATAL: Out of memory even after GC and heap growth!\n");
        exit(1);
      }
    }
  }
  SchemeObject *obj = (SchemeObject *)((char *)heap + used);
  memset(obj, 0, sizeof(SchemeObject));
  used += size;
  // printf("DEBUG: Allocated object at %p, used now %zu\n", (void*)obj, used);
  // // Optional
  return obj;
}

SchemeObject *allocate(SchemeType type, int64_t immediate) {
  // printf("DEBUG: Entering allocate(type=%d, immediate=%lld)\n", type,
  // immediate);
  SchemeObject *obj = alloc_object();
  if (!obj) {
    fprintf(stderr, "ERROR: alloc_object returned NULL in allocate!\n");
    return NULL;
  }
  obj->type = type & ~GC_MARK_BIT; // Ensure type is clean on allocation
  switch (obj->type) {
  case TYPE_NUMBER:
    obj->value.number = immediate;
    break;
  case TYPE_SYMBOL:
    fprintf(stderr, "WARN: Direct allocation of TYPE_SYMBOL attempted.\n");
    obj->value.symbol = (const char *)immediate;
    break;
  case TYPE_PAIR:
    fprintf(stderr, "WARN: Direct allocation of TYPE_PAIR attempted.\n");
    obj->value.pair.car = (SchemeObject *)immediate;
    obj->value.pair.cdr = SCHEME_NIL;
    break;
  case TYPE_BOOL:
    obj->value.boolean = (immediate != 0);
    break;
  case TYPE_FUNCTION:
    obj->value.function.code = NULL;
    obj->value.function.env = NULL;
    break;
  case TYPE_NIL:
    break;
  default:
    fprintf(stderr, "ERROR: Invalid type %d passed to allocate\n", obj->type);
    return NULL;
  }
  // printf("DEBUG: Exiting allocate(), returning object at %p\n", obj);
  return obj;
}
SchemeObject *allocate_pair(SchemeObject *car, SchemeObject *cdr) {
  SchemeObject *obj = alloc_object();
  if (!obj)
    return NULL;
  obj->type = TYPE_PAIR;
  obj->value.pair.car = car;
  obj->value.pair.cdr = cdr;
  return obj;
}

void debug_check_struct_sizes() {
  // ... (same as before) ...
  printf("\nDEBUG: STRUCT SIZE INFORMATION...\n");
  printf("DEBUG: sizeof(SchemeObject) = %zu\n", sizeof(SchemeObject));
  printf("DEBUG: Heap address: %p\n", heap);
  printf("DEBUG: STRUCT CHECK COMPLETE\n\n");
}

extern void init_symbol_table(void);
extern void init_global_environment(void);

void init_runtime() {
  volatile char dummy_on_stack;
  stack_bottom_approx = (void *)&dummy_on_stack;
  heap = malloc(INITIAL_HEAP);
  if (!heap) {
    fprintf(stderr, "Failed to initialize heap!\n");
    exit(1);
  }
  heap_size = INITIAL_HEAP;
  used = 0;
#ifdef DEBUG_GC
  printf("Runtime initialized with heap size %zu bytes\n", heap_size);
  printf("Approx stack bottom: %p\n", stack_bottom_approx);
  debug_check_struct_sizes();
#endif
  printf("DEBUG: Initializing symbol table and global environment...\n");
  init_symbol_table();
  init_global_environment();
  printf("DEBUG: Symbol table and global environment initialized.\n");
}

void cleanup_runtime() {
  // destroy_symbol_table(); // If implemented
  free(heap);
  heap = NULL;
  heap_size = 0;
  used = 0;
}

void mark_object(SchemeObject *obj) {
  if (!obj || (uintptr_t)obj < (uintptr_t)heap ||
      (uintptr_t)obj >= (uintptr_t)heap + used)
    return;
  if (((uintptr_t)obj - (uintptr_t)heap) % sizeof(SchemeObject) != 0)
    return; // Alignment check
  if (obj->type & GC_MARK_BIT)
    return; // Already marked
  obj->type |= GC_MARK_BIT;
  switch (obj->type & ~GC_MARK_BIT) {
  case TYPE_PAIR:
    mark_object(obj->value.pair.car);
    mark_object(obj->value.pair.cdr);
    break;
  case TYPE_FUNCTION:
    mark_environment(obj->value.function.env);
    break;
  // Add other types with heap pointers here (e.g., vectors)
  default:
    break; // No heap pointers in number, bool, nil, symbol (string is malloc'd)
  }
}

void mark_environment(SchemeEnvironment *env) {
  SchemeEnvironment *current_env = env;
  while (current_env != NULL) {
    if (current_env->bindings) {
      for (size_t i = 0; i < current_env->bindings->capacity; i++) {
        Entry *entry = current_env->bindings->entries[i];
        while (entry != NULL) {
          mark_object(entry->key);   // Mark symbol
          mark_object(entry->value); // Mark value
          entry = entry->next;
        }
      }
    }
    current_env = current_env->enclosing; // Move to enclosing scope
  }
}

void mark_roots() {
#ifdef DEBUG_GC
  printf("DEBUG: Marking roots...\n");
#endif
  mark_environment(current_environment);
  if (global_symbol_table) {
    for (size_t i = 0; i < global_symbol_table->capacity; i++) {
      Entry *entry = global_symbol_table->entries[i];
      while (entry != NULL) {
        mark_object(entry->key);
        entry = entry->next;
      }
    }
  }
  // 2. Mark Stack (Conservative Scan)
  void *stack_top_approx;
  volatile char dummy_on_stack_top;
  stack_top_approx = (void *)&dummy_on_stack_top;
  if (!stack_bottom_approx) {
    fprintf(stderr, "ERROR: Stack bottom not initialized for GC!\n");
    return;
  }
#ifdef DEBUG_GC
  printf("DEBUG: Scanning stack conservatively from approx top %p to approx "
         "bottom %p\n",
         stack_top_approx, stack_bottom_approx);
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
  uintptr_t heap_start = (uintptr_t)heap;
  uintptr_t heap_end = heap_start + used;
  for (uintptr_t p = scan_start; p < scan_end; p += sizeof(void *)) {
    SchemeObject *potential_obj = *(SchemeObject **)p;
    if ((uintptr_t)potential_obj >= heap_start &&
        (uintptr_t)potential_obj < heap_end) {
      mark_object(potential_obj);
    }
  }
#ifdef DEBUG_GC
  printf("DEBUG: Stack scan complete.\n");
#endif
}

// --- Helper function to look up the new address of an object ---
SchemeObject *get_forwarding_address(ForwardingInfo *map, size_t map_size,
                                     SchemeObject *old_addr) {
  // Simple linear search for demonstration; use hash map for better performance
  for (size_t i = 0; i < map_size; ++i) {
    if (map[i].old_address == old_addr) {
      return map[i].new_address;
    }
  }
  return old_addr;
}

// --- REVISED sweep_compact with 3 Passes ---
void sweep_compact() {
  uintptr_t scan_ptr = (uintptr_t)heap;
  uintptr_t write_ptr = (uintptr_t)heap;
  uintptr_t current_heap_end = (uintptr_t)heap + used;
  size_t live_count = 0;
  size_t max_objects = heap_size / sizeof(SchemeObject);
  ForwardingInfo *forwarding_map = malloc(max_objects * sizeof(ForwardingInfo));
  if (!forwarding_map) {
    fprintf(stderr,
            "ERROR: Failed to allocate forwarding map for GC compaction!\n");
    while (scan_ptr < current_heap_end) {
      SchemeObject *current_obj = (SchemeObject *)scan_ptr;
      if (current_obj->type & GC_MARK_BIT) {
        current_obj->type &= ~GC_MARK_BIT;
      }
      scan_ptr += sizeof(SchemeObject);
    }
    return; // Exit sweep_compact
  }

#ifdef DEBUG_GC
  printf("DEBUG: Sweep/Compact Pass 1: Calculating new addresses...\n");
#endif

  scan_ptr = (uintptr_t)heap;
  while (scan_ptr < current_heap_end) {
    SchemeObject *current_obj = (SchemeObject *)scan_ptr;
    if (current_obj->type & GC_MARK_BIT) { // Is it live?
      forwarding_map[live_count].old_address = current_obj;
      forwarding_map[live_count].new_address = (SchemeObject *)write_ptr;
      live_count++;
      write_ptr +=
          sizeof(SchemeObject); // Advance write pointer for next live object
    }
    scan_ptr += sizeof(SchemeObject);
  }

#ifdef DEBUG_GC
  printf("DEBUG: Sweep/Compact Pass 1: Found %zu live objects.\n", live_count);
#endif

#ifdef DEBUG_GC
  printf("DEBUG: Sweep/Compact Pass 2: Updating pointers...\n");
#endif

  // Pass 2: Update internal pointers using the forwarding map
  // Iterate through the *live* objects using the map we just built
  for (size_t i = 0; i < live_count; ++i) {
    SchemeObject *current_obj =
        forwarding_map[i].old_address; // Get object from old location
    SchemeType base_type = current_obj->type & ~GC_MARK_BIT;

    switch (base_type) {
    case TYPE_PAIR: {
      SchemeObject *old_car = current_obj->value.pair.car;
      SchemeObject *old_cdr = current_obj->value.pair.cdr;
      // Only update if pointer is non-null and within heap bounds (before
      // compaction)
      if (old_car && (uintptr_t)old_car >= (uintptr_t)heap &&
          (uintptr_t)old_car < current_heap_end) {
        current_obj->value.pair.car =
            get_forwarding_address(forwarding_map, live_count, old_car);
      }
      if (old_cdr && (uintptr_t)old_cdr >= (uintptr_t)heap &&
          (uintptr_t)old_cdr < current_heap_end) {
        current_obj->value.pair.cdr =
            get_forwarding_address(forwarding_map, live_count, old_cdr);
      }
      break;
    }
    case TYPE_FUNCTION: {
      SchemeEnvironment *old_env = current_obj->value.function.env;
      // Environments themselves aren't GC'd here, but we need to update
      // pointers *within* them This is tricky. Let's assume env pointers don't
      // need updating *by the compactor* unless SchemeEnvironment structs are
      // ALSO on the Scheme heap. If they are malloc'd separately (as they are
      // now), their address doesn't change. We *do* need to update pointers
      // *inside* the environment bindings, but mark_environment -> mark_object
      // -> recursive marking handles reachability. The update pass needs to fix
      // pointers *to* heap objects *from* heap objects. Pointers from malloc'd
      // envs to heap objects don't need updating here. Pointers *within*
      // closures (the env field) pointing to *other heap objects* would need
      // updating if SchemeEnvironment itself was heap-allocated by our GC.
      // Let's assume for now env pointers don't point to objects that move
      // *within this heap*.
      break;
    }
    // Add cases for other types with heap pointers (e.g., vectors)
    default:
      break; // No pointers to update in numbers, bools, symbols, nil
    }
    // We can unmark here or in Pass 3
    // current_obj->type &= ~GC_MARK_BIT;
  }

#ifdef DEBUG_GC
  printf("DEBUG: Sweep/Compact Pass 3: Moving objects...\n");
#endif

  // Pass 3: Move objects to their new locations
  for (size_t i = 0; i < live_count; ++i) {
    SchemeObject *old_addr = forwarding_map[i].old_address;
    SchemeObject *new_addr = forwarding_map[i].new_address;

    // Unmark the object (if not done in pass 2)
    old_addr->type &= ~GC_MARK_BIT;

    // Move if necessary (memmove handles overlapping regions)
    if (old_addr != new_addr) {
      memmove(new_addr, old_addr, sizeof(SchemeObject));
    }
  }

  // Update heap usage
  size_t new_used = live_count * sizeof(SchemeObject);

#ifdef DEBUG_GC
  printf("DEBUG: Sweep/Compact finished. Live objects: %zu (%zu bytes)\n",
         live_count, new_used);
  printf("DEBUG: New heap usage: %zu bytes\n", new_used);
  printf("DEBUG: Freed (approx): %zu bytes\n", used - new_used);
#endif

  used = new_used;

  // Free the temporary map
  free(forwarding_map);

  // Try shrinking if utilization is low
  if (heap_size > MIN_HEAP_SIZE &&
      (double)used / heap_size < HEAP_SHRINK_THRESHOLD) {
    try_shrink_heap();
  }
#ifdef DEBUG_GC
  if (heap_size > 0)
    printf("DEBUG: Heap utilization: %.1f%%\n", (used * 100.0) / heap_size);
#endif
}

void gc() {
#ifdef DEBUG_GC
  printf("\n--- GC START ---\n");
#endif

  mark_roots();

#ifdef DEBUG_GC
  printf("--- Marking Complete ---\n");
#endif

  sweep_compact(); // Use the new 3-pass compacting version

#ifdef DEBUG_GC
  printf("--- GC Complete ---\n");
#endif
}
