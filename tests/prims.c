#include <caml/mlvalues.h>

CAMLprim value random_bool()
{
  return Val_int(1);
}

CAMLprim value begin_loop()
{
  return Val_unit;
}

CAMLprim value end_loop ()
{
  return Val_unit;
}
