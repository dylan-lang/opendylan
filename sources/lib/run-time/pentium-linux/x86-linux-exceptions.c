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


/* Linux exception handling:  Setup a signal handler for SIGFPE (floating point exceptions).
   We rely on the fact that Linux passes a second argument containing the error context. */

/* ---*** NOTE: On x86 Linux, our use of the INT 4 and INTO instructions to raise an
                integer overflow exception isn't properly converted into a SIGFPE signal.
                Presumably, the hardware trap vector isn't setup properly and, as a result,
                we get a SIGSEGV signal instead.  So, on x86 Linux only, we establish
                a SIGSEGV handler which checks the trap number in the sigcontext structure
                to see if we were invoked for an integer overflow. */

/* ---*** TODO: Find out how to trap stack overflows and add an appropriate handler */


#define EXCEPTION_PREAMBLE() \
  struct sigaction oldFPEHandler; \
  struct sigaction oldSEGVHandler; \
  void doFPEHandler (int signum, struct sigcontext sc) { \
    DylanFPEHandler(oldFPEHandler.sa_handler, signum, sc); \
  } \
  void doSEGVHandler (int signum, struct sigcontext sc) { \
    DylanSEGVHandler(oldSEGVHandler.sa_handler, signum, sc); \
  } \
  oldFPEHandler.sa_handler = (sighandler_t)doFPEHandler; \
  oldSEGVHandler.sa_handler = (sighandler_t)doSEGVHandler; \
  EstablishDylanExceptionHandlers(&oldFPEHandler, &oldSEGVHandler); \
  {

#define EXCEPTION_POSTAMBLE() \
  } \
  RemoveDylanExceptionHandlers(&oldFPEHandler, &oldSEGVHandler);

typedef void (* SIG_SIGCONTEXT)(int, struct sigcontext);

static void DylanFPEHandler (sighandler_t, int, struct sigcontext);
static void DylanSEGVHandler (sighandler_t, int, struct sigcontext);

static void EstablishDylanExceptionHandlers (struct sigaction * oldFPEHandler,
					     struct sigaction * oldSEGVHandler)
{
  struct sigaction newFPEHandler;
  struct sigaction newSEGVHandler;

  sigset_t set, oldset;

  newFPEHandler.sa_handler = oldFPEHandler->sa_handler;
  sigemptyset(&newFPEHandler.sa_mask);
  newFPEHandler.sa_flags = 0;
  sigaction(SIGFPE, &newFPEHandler, oldFPEHandler);

#if 0
  newSEGVHandler.sa_handler = oldSEGVHandler->sa_handler;
  sigemptyset(&newSEGVHandler.sa_mask);
  newSEGVHandler.sa_flags = 0;
  sigaction(SIGSEGV, &newSEGVHandler, oldSEGVHandler);
#endif

  sigemptyset(&set);
  sigaddset(&set, SIGPIPE);
  sigprocmask(SIG_BLOCK, &set, &oldset);
}

static void RemoveDylanExceptionHandlers (struct sigaction * oldFPEHandler,
					  struct sigaction * oldSEGVHandler)
{
  sigaction(SIGFPE, oldFPEHandler, NULL);
#if 0
  sigaction(SIGSEGV, oldSEGVHandler, NULL);
#endif
}


/* ---*** NOTE: There doesn't appear to be an include file that defines these constants! */

#define TRAP_INTEGER_DIVZERO   0
#define TRAP_INTEGER_OVERFLOW  4
#define TRAP_FLOAT_EXCEPTION  16

#define FS_INVALID_OP          1
#define FS_DENORMAL_OPERAND    2
#define FS_DIVZERO             4
#define FS_OVERFLOW            8
#define FS_UNDERFLOW          16
#define FS_INEXACT            32
#define FS_FSTACK_OVERFLOW    64
#define FS_ALL_EXCEPTIONS \
  (FS_INVALID_OP | FS_DENORMAL_OPERAND | FS_DIVZERO | FS_OVERFLOW | FS_UNDERFLOW \
   | FS_INEXACT | FS_FSTACK_OVERFLOW)

__inline
void RestoreFPState (struct _fpstate * fp)
{
  fp->sw &= ~(long)FS_ALL_EXCEPTIONS;           /* Reset all exception flags */
  asm ("movl %0,%%eax\n\tfldenv 0(%%eax)" : : "g" (fp) : "ax");
  return;
}

static void DylanFPEHandler (sighandler_t oldHandler, int signum, struct sigcontext sc)
{
  if (inside_dylan_ffi_barrier() == 0) { }

  else {
    switch (sc.trapno) {
    case TRAP_INTEGER_DIVZERO:
      RestoreFPState(sc.fpstate);
      dylan_integer_divide_0_handler();
      break;                                    /* Should never get here ... */              
    case TRAP_INTEGER_OVERFLOW:
      RestoreFPState(sc.fpstate);
      dylan_integer_overflow_handler();
      break;                                    /* Should never get here ... */              
    case TRAP_FLOAT_EXCEPTION:
      {
	long state = sc.fpstate->sw;
	if (state & FS_DIVZERO) {
	  RestoreFPState(sc.fpstate);
	  dylan_float_divide_0_handler();       /* Should never return ... */
	} else if (state & FS_OVERFLOW) {
	  RestoreFPState(sc.fpstate);
	  dylan_float_overflow_handler();       /* Should never return ... */
	} else if (state & FS_UNDERFLOW) {
	  RestoreFPState(sc.fpstate);
	  dylan_float_underflow_handler();      /* Should never return ... */
	}
      }
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
    (*(SIG_SIGCONTEXT)oldHandler)(signum, sc);

  return;
}


static void DylanSEGVHandler (sighandler_t oldHandler, int signum, struct sigcontext sc)
{
  if (inside_dylan_ffi_barrier() == 0) { }

  else if (sc.trapno == TRAP_INTEGER_OVERFLOW) {
    RestoreFPState(sc.fpstate);
    dylan_integer_overflow_handler();           /* Should never return ... */
  }

  // Here iff we should invoke the previous handler ...
  if (oldHandler == SIG_DFL) {
    signal(signum, SIG_DFL);
    raise(signum);
  }
  else
    // ---*** NOTE: What if the old handler isn't expecting this calling sequence?
    (*(SIG_SIGCONTEXT)oldHandler)(signum, sc);

  return;
}
