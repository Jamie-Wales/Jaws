#include "../include/jaws_io.h"
#include "../include/gc.h"
#include <stdio.h>

void scheme_display(SchemeObject* obj)
{
    printf("\nDEBUG: Entering scheme_display(obj=%p)\n", obj);

    if (!obj) {
        printf("DEBUG: NULL object pointer\n");
        printf("()\n");
        return;
    }

    printf("DEBUG: Object is at address: %p\n", (void*)obj);
    printf("DEBUG: Trying to access object type\n");

    int raw_type = obj->type;
    printf("DEBUG: Raw type value: %d\n", raw_type);

    int masked_type = raw_type & 0x7F;
    printf("DEBUG: Masked type: %d\n", masked_type);

    // Now directly access the value based on type
    printf("DEBUG: About to access value based on type %d\n", masked_type);

    switch (masked_type) {
    case TYPE_NUMBER:
        printf("DEBUG: Accessing number value\n");
        int64_t num = obj->value.number;
        printf("DEBUG: Number value is %lld\n", num);
        printf("%lld\n", num);
        break;
    case TYPE_SYMBOL:
        printf("DEBUG: Accessing symbol pointer\n");
        const char* symbol = obj->value.symbol;
        printf("DEBUG: Symbol pointer is %p\n", symbol);
        if (symbol) {
            printf("DEBUG: Symbol content: %s\n", symbol);
            printf("%s\n", symbol);
        } else {
            printf("DEBUG: NULL symbol\n");
            printf("<invalid-symbol>\n");
        }
        break;
    case TYPE_PAIR:
        printf("DEBUG: Pair type - calling to_string()\n");
        {
            char* str = to_string(obj);
            printf("DEBUG: to_string returned %p\n", str);
            if (str) {
                printf("%s\n", str);
                free(str);
            } else {
                printf("<error-in-to-string>\n");
            }
        }
        break;
    case TYPE_NIL:
        printf("DEBUG: NIL type\n");
        printf("()\n");
        break;
    case TYPE_FUNCTION:
        printf("DEBUG: Function type\n");
        printf("#<procedure>\n");
        break;
    default:
        printf("DEBUG: Unknown type %d\n", masked_type);
        printf("<unknown-type>\n");
    }

    printf("DEBUG: Exiting scheme_display()\n");
}

// Return type should be SchemeObject*, not int64_t
SchemeObject* scheme_add(SchemeObject* a, SchemeObject* b)
{
    int64_t result = a->value.number + b->value.number;
    return allocate(TYPE_NUMBER, result);
}
void scheme_newline()
{
    printf("%s", "\n");
}
