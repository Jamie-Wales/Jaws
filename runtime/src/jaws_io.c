#include "../include/jaws_io.h"
#include "../include/gc.h"
#include "../include/jaws_eq.h"
#include <stdio.h>

// #define DEBUG_JAWS_IO

SchemeObject *display(SchemeObject *obj) {

#ifdef DEBUG_JAWS_IO
  printf("\nDEBUG: Entering scheme_display(obj=%p)\n", obj);
#endif

  if (!obj) {
#ifdef DEBUG_JAWS_IO
    printf("DEBUG: NULL object pointer\n");
#endif
    printf("()\n"); // Functional output
    return SCHEME_NIL;
  }

#ifdef DEBUG_JAWS_IO
  printf("DEBUG: Object is at address: %p\n", (void *)obj);
  printf("DEBUG: Trying to access object type\n");
#endif

  int raw_type = obj->type;
#ifdef DEBUG_JAWS_IO
  printf("DEBUG: Raw type value: %d\n", raw_type);
#endif

  int masked_type = raw_type & 0x7F;
#ifdef DEBUG_JAWS_IO
  printf("DEBUG: Masked type: %d\n", masked_type);
  printf("DEBUG: About to access value based on type %d\n", masked_type);
#endif

  switch (masked_type) {
  case TYPE_STRING: // Assuming TYPE_STRING is defined and distinct
#ifdef DEBUG_JAWS_IO
    printf("DEBUG: String type\n");
#endif
    if (obj->value.string) {
      printf("%s\n", obj->value.string); // Functional output
    } else {
      printf("<invalid-string-ptr>\n"); // Functional output
    }
    break;
  case TYPE_NUMBER:
#ifdef DEBUG_JAWS_IO
    printf("DEBUG: Accessing number value\n");
#endif
    int64_t num = obj->value.number;
#ifdef DEBUG_JAWS_IO
    printf("DEBUG: Number value is %lld\n", num);
#endif
    printf("%lld\n", num); // Functional output
    break;
  case TYPE_SYMBOL:
#ifdef DEBUG_JAWS_IO
    printf("DEBUG: Accessing symbol pointer\n");
#endif
    const char *symbol = obj->value.symbol;
#ifdef DEBUG_JAWS_IO
    printf("DEBUG: Symbol pointer is %p\n", symbol);
#endif
    if (symbol) {
#ifdef DEBUG_JAWS_IO
      printf("DEBUG: Symbol content: %s\n", symbol);
#endif
      printf("%s\n", symbol); // Functional output
    } else {
#ifdef DEBUG_JAWS_IO
      printf("DEBUG: NULL symbol\n");
#endif
      printf("<invalid-symbol>\n"); // Functional output
    }
    break;
  case TYPE_PAIR:
#ifdef DEBUG_JAWS_IO
    printf("DEBUG: Pair type - calling to_string()\n");
#endif
    {
      char *str =
          to_string(obj); // to_string itself might have its own debug prints
#ifdef DEBUG_JAWS_IO
      printf("DEBUG: to_string returned %p\n", str);
#endif
      if (str) {
        printf("%s\n", str); // Functional output
        free(str);
      } else {
        printf("<error-in-to-string>\n"); // Functional output (error condition)
      }
    }
    break;
  case TYPE_NIL:
#ifdef DEBUG_JAWS_IO
    printf("DEBUG: NIL type\n");
#endif
    printf("()\n"); // Functional output
    break;
  case TYPE_FUNCTION:
#ifdef DEBUG_JAWS_IO
    printf("DEBUG: Function type\n");
#endif
    printf("#<procedure>\n"); // Functional output
    break;
  default:
#ifdef DEBUG_JAWS_IO
    printf("DEBUG: Unknown type %d\n", masked_type);
#endif
    printf("<unknown-type>\n"); // Functional output
  }

#ifdef DEBUG_JAWS_IO
  printf("DEBUG: Exiting scheme_display()\n");
#endif

  return SCHEME_NIL;
}

SchemeObject *equal(SchemeObject *a, SchemeObject *b) {
  return a->value.number == b->value.number ? s_true() : s_false();
}

SchemeObject *plus(SchemeObject *a, SchemeObject *b) {
  int64_t result = a->value.number + b->value.number;
  return allocate(TYPE_NUMBER, result);
}

SchemeObject *multiply(SchemeObject *a, SchemeObject *b) {
  int64_t result = a->value.number * b->value.number;
  return allocate(TYPE_NUMBER, result);
}
SchemeObject *newline() {
  printf("%s", "\n"); // Functional output
  return SCHEME_NIL;
}
