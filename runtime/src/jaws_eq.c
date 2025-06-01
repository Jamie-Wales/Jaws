#include "../include/jaws_eq.h"
#include "../include/types.h"

SchemeObject *is_null(SchemeObject *obj) {
  return allocate(TYPE_BOOL, is_nil(obj));
}

SchemeObject *s_true() { return &true_obj; };

SchemeObject *s_false() { return &false_obj; };
