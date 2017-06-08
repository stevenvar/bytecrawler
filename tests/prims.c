#include <caml/mlvalues.h>

CAMLprim value begin_loop()
{
  return Val_unit;
}

CAMLprim value end_loop ()
{
  return Val_unit;
}
