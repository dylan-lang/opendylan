#include <stddef.h>
#include <stdlib.h>

#include "llvm-runtime.h"

// primitive-exit-application
void primitive_exit_application(DSINT status)
{
  exit(status);
}
