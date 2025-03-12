#pragma once

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
typedef enum {
    TYPE_NUMBER,
    TYPE_PAIR,
    TYPE_SYMBOL,
    TYPE_NIL,
    TYPE_FUNCTION,
    TYPE_BOOL
} SchemeType;

typedef struct {
    void* code;
    struct SchemeEnvironment* env;
} SchemeFunction;

typedef struct SchemeObject {
    SchemeType type;
    union {
        int64_t number;
        bool boolean;
        struct {
            struct SchemeObject* car;
            struct SchemeObject* cdr;
        } pair;
        const char* symbol;
        SchemeFunction function;
    } value;
} SchemeObject;

static SchemeObject nil_obj = { TYPE_NIL };
#define SCHEME_NIL (&nil_obj)

char* to_string(SchemeObject* object);
int is_nil(SchemeObject* obj);
SchemeObject* make_function(void* code, struct SchemeEnvironment* env);
SchemeObject* make_closure(void* code);
void call_closure(SchemeObject* func, SchemeObject** args, int arg_count);
