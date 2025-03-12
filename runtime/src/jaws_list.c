#include "../include/jaws_list.h"
#include "../include/gc.h"

SchemeObject* scheme_cons(SchemeObject* car, SchemeObject* cdr)
{
    SchemeObject* pair = alloc_object();

    pair->type = TYPE_PAIR;
    pair->value.pair.car = car;
    pair->value.pair.cdr = cdr;

    return pair;
}
