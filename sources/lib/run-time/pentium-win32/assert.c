#include <windows.h>
#include "mpslib.h"

void __cdecl _assert (
        void *expr,
        void *filename,
        unsigned lineno
        )
{
  mps_lib_abort();
}
