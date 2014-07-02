#include <unistd.h>
#include "caml/mlvalues.h"

CAMLprim value nb_processors (value unit) {
  long v;
  #ifdef _SC_NPROCESSORS_ONLN
  v = sysconf (_SC_NPROCESSORS_ONLN);
  #else
  v = 1;
  #endif
  return Val_long ((v<=0)?1:v);
}
