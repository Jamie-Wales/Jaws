#pragma once

#include <stdint.h>
#include <stdlib.h>
typedef enum {
    TYPE_NUMBER,
    TYPE_PAIR,
    TYPE_SYMBOL,
    TYPE_NIL,
    TYPE_FUNCTION
} SchemeType;

typedef struct SchemeObject {
    SchemeType type;
    union {
        int64_t number;
        struct {
            struct SchemeObject* car;
            struct SchemeObject* cdr;
        } pair;
        const char* symbol;
    } value;
} SchemeObject;

static SchemeObject nil_obj = { TYPE_NIL };
#define SCHEME_NIL (&nil_obj)

char* to_string(SchemeObject* object);
int is_nil(SchemeObject* obj);
