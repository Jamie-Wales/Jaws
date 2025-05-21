#include "../include/types.h"
#include "../include/gc.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static char *to_string_pair(SchemeObject *pair, int *depth) {
  if (*depth > 100)
    return strdup("...");
  char *car_str;
  char *cdr_str;
  char *result;

  (*depth)++;
  if (pair->value.pair.car) {
    car_str = to_string(pair->value.pair.car);
  } else {
    car_str = strdup("()");
  }

  if (pair->value.pair.cdr && pair->value.pair.cdr->type == TYPE_PAIR) {
    cdr_str = to_string_pair(pair->value.pair.cdr, depth);
    result = malloc(strlen(car_str) + strlen(cdr_str) + 4);
    sprintf(result, "%s %s", car_str, cdr_str);
  } else {
    cdr_str = to_string(pair->value.pair.cdr);
    result = malloc(strlen(car_str) + strlen(cdr_str) + 4);
    sprintf(result, "%s . %s", car_str, cdr_str);
  }

  free(car_str);
  if (cdr_str)
    free(cdr_str);
  (*depth)--;

  return result;
}

SchemeObject *make_closure(void *code) {
  SchemeEnvironment *captured_env = current_environment;

  SchemeObject *obj = alloc_object();
  if (!obj)
    return NULL;

  obj->type = TYPE_FUNCTION;
  obj->value.function.code = code;
  obj->value.function.env = captured_env;

  printf("DEBUG: Created closure %p with code %p and captured env %p\n",
         (void *)obj, code, (void *)captured_env);

  return obj;
}

SchemeObject *make_function(void *code, struct SchemeEnvironment *env) {
  SchemeObject *obj = alloc_object();
  if (!obj)
    return NULL;

  obj->type = TYPE_FUNCTION;
  obj->value.function.code = code;
  obj->value.function.env = env;
  return obj;
}

char *to_string(SchemeObject *object) {
  if (!object)
    return strdup("()");

  char buffer[1024];
  char *result;

  switch (object->type & 0x7F) {
  case TYPE_NUMBER:
    snprintf(buffer, sizeof(buffer), "%lld", object->value.number);
    return strdup(buffer);

  case TYPE_PAIR: {
    int depth = 0;
    result = malloc(strlen("(") + strlen(to_string_pair(object, &depth)) +
                    strlen(")") + 1);
    sprintf(result, "(%s)", to_string_pair(object, &depth));
    return result;
  }

  case TYPE_SYMBOL:
    if (object->value.symbol) {
      return strdup(object->value.symbol);
    }
    return strdup("<invalid-symbol>");

  case TYPE_NIL:
    return strdup("()");

  case TYPE_FUNCTION:
    return strdup("#<procedure>");

  default:
    return strdup("<unknown-type>");
  }
}
int is_nil(SchemeObject *obj) { return obj == SCHEME_NIL || obj == NULL; }
/* #TODO: Implement this function */
void call_closure(SchemeObject *func, SchemeObject **args, int arg_count) {
  return;
}
