#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>

#include "llvm-runtime.h"

#ifdef GC_USE_BOEHM
#include <gc/gc.h>
#endif

void _Init_Run_Time(void)
{
  static int initp = 0;
  if (!initp) {
    initp = 1;

    // register our dylan-level atexit mechanism
    atexit(call_application_exit_functions_internal);

    // set up signal handlers
#ifdef OPEN_DYLAN_PLATFORM_UNIX
#ifdef SIGPIPE
    signal(SIGPIPE, SIG_IGN);
#endif
#endif

#ifdef GC_USE_BOEHM
    GC_INIT();
#endif

    primitive_initialize_thread_variables();
  }
}
