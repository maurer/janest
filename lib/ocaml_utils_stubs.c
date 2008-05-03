#include "ocaml_utils.h"

/* Exceptions */

void raise_with_two_args(value tag, value arg1, value arg2)
{
  value v_exc;

  Begin_roots3(tag, arg1, arg2);
    v_exc = caml_alloc_small(3, 0);
    Field(v_exc, 0) = tag;
    Field(v_exc, 1) = arg1;
    Field(v_exc, 2) = arg2;
  End_roots();

  caml_raise(v_exc);
}
