/* Support for handling exceptions in Dylan (other than MM traps) */
/* Currently, we just handle stack overflows arithmetic exceptions  */

/* ---*** TODO: Find out how to trap stack overflows on Linux */

extern int inside_dylan_ffi_barrier();
extern void dylan_stack_overflow_handler(PVOID base_address, int size, DWORD protection);
extern void dylan_integer_overflow_handler();
extern void dylan_integer_divide_0_handler();
extern void dylan_float_divide_0_handler();
extern void dylan_float_overflow_handler();
extern void dylan_float_underflow_handler();


/* Linux exception handling:  Setup a signal handler for SIGFPE (floating point exceptions)
   using the SA_SIGINFO extension to sigaction.  Use of this flag passes two extra arguments
   to the signal handler.  One of those arguments is a siginfo structure which contains
   details on the exception.  In particular, it classifies the exception (e.g., integer
   overflow, float overflow, etc.)  We use this classification to invoke the appropriate
   Dylan handler */

/* ---*** TODO: Find out how to trap stack overflows and add an appropriate handler */


#define EXCEPTION_PREAMBLE() \
  struct sigaction oldFPEHandler; \
  void doFPEHandler (int signum, siginfo_t * si, void * p) { \
    DylanFPEHandler(oldFPEHandler.sa_handler, signum, si, p); \
  } \
  oldFPEHandler.sa_handler = (__sighandler_t)doFPEHandler; \
  EstablishDylanExceptionHandlers(&oldFPEHandler); \
  {

#define EXCEPTION_POSTAMBLE() \
  } \
  RemoveDylanExceptionHandlers(&oldFPEHandler);

typedef void (* SIG_SIGINFO)(int, siginfo_t *, void *);

static void DylanFPEHandler (__sighandler_t, int, siginfo_t *, void *);

static void EstablishDylanExceptionHandlers (struct sigaction * oldFPEHandler)
{
  struct sigaction newFPEHandler;

  newFPEHandler.sa_handler = oldFPEHandler->sa_handler;
  sigemptyset(&newFPEHandler.sa_mask);
  newFPEHandler.sa_flags = SA_SIGINFO;
  sigaction(SIGFPE, &newFPEHandler, oldFPEHandler);
}

static void RemoveDylanExceptionHandlers (struct sigaction * oldFPEHandler)
{
  sigaction(SIGFPE, oldFPEHandler, NULL);
}

static void DylanFPEHandler (__sighandler_t oldHandler, int signum, siginfo_t * si, void * p)
{
  if (inside_dylan_ffi_barrier() == 0) { }

  else {
    switch (si->si_code) {
    case FPE_INTDIV:
      dylan_integer_divide_0_handler();
      break;
    case FPE_INTOVF:
      dylan_integer_overflow_handler();
      break;
    case FPE_FLTDIV:
      dylan_float_divide_0_handler();
      break;
    case FPE_FLTOVF:
      dylan_float_overflow_handler();
      break;
    case FPE_FLTUND:
      dylan_float_underflow_handler();
      break;
    default:
      break;
    }
  }

  // Here iff we should invoke the previous handler ...
  if (oldHandler == SIG_DFL) {
    signal(signum, SIG_DFL);
    raise(signum);
  }
  else
    // ---*** NOTE: What if the old handler isn't expecting this calling sequence?
    (*(SIG_SIGINFO)oldHandler)(signum, si, p);

  return;
}
