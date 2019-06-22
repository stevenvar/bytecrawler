/* #include <caml/values.h> */
#define OCAML_VIRTUAL_ARCH 16
#include "/usr/local/include/omicrob/vm/values.h"

value begin_loop()
{
  return Val_unit;
}

value end_loop ()
{
  return Val_unit;
}
