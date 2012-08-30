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

#include <sys/signal.h>
#include <ucontext.h>
#include <ieeefp.h>
#include <fenv.h>

#define EXCEPTION_PREAMBLE() \
  struct sigaction oldFPEHandler; \
  struct sigaction oldSEGVHandler; \
  EstablishDylanExceptionHandlers(&oldFPEHandler, &oldSEGVHandler); \
  {

#define EXCEPTION_POSTAMBLE() \
  } \
  RemoveDylanExceptionHandlers(&oldFPEHandler, &oldSEGVHandler);

static void DylanFPEHandler (int sig, siginfo_t *info, void *sc);
static void DylanSEGVHandler (int sig, siginfo_t *info, void *sc);

static void EstablishDylanExceptionHandlers (struct sigaction * oldFPEHandler,
					     struct sigaction * oldSEGVHandler)
{
  struct sigaction newFPEHandler;
  struct sigaction newSEGVHandler;

  sigset_t set, oldset;

  sigfillset(&newFPEHandler.sa_mask);
  newFPEHandler.sa_sigaction = DylanFPEHandler;
  newFPEHandler.sa_flags = SA_SIGINFO;
  sigaction(SIGFPE, &newFPEHandler, oldFPEHandler);

#if 0
  sigfillset(&newFPEHandler.sa_mask);
  newSEGVHandler.sa_sigaction = DylanFSEGVandler;
  newSEGVHandler.sa_flags = SA_SIGINFO;
  sigaction(SIGFPE, &newSEGVHandler, oldSEGVHandler);
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

__inline
void RestoreFPState ()
{
  fpresetsticky(fpgetsticky());
  fpsetmask(FP_X_INV | FP_X_DZ | FP_X_OFL);
  return;
}

static void DylanFPEHandler (int sig, siginfo_t *info, void *uap)
{
  if (inside_dylan_ffi_barrier() == 0) { }

  else {
    ucontext_t *uc = (ucontext_t *) uap;

    switch (info->si_code) {
    case FPE_INTDIV:
      RestoreFPState();
      uc->uc_mcontext.mc_eip = (long) dylan_integer_divide_0_handler;
      break;
      
    case FPE_INTOVF:
      RestoreFPState();
      uc->uc_mcontext.mc_eip = (long) dylan_integer_overflow_handler;
      break;
      
    case FPE_FLTDIV:
      RestoreFPState();
      uc->uc_mcontext.mc_eip = (long) dylan_float_divide_0_handler;
      break;
      
    case FPE_FLTOVF:
      RestoreFPState();
      uc->uc_mcontext.mc_eip = (long) dylan_float_overflow_handler;
      break;

    case FPE_FLTUND:
      RestoreFPState();
      uc->uc_mcontext.mc_eip = (long) dylan_float_underflow_handler;
      break;
      
    default:
      break;
    }
  }
}


static void DylanSEGVHandler (int sig, siginfo_t *info, void *sc)
{
  if (inside_dylan_ffi_barrier() == 0) { }

  // else if (sc.trapno == TRAP_INTEGER_OVERFLOW) {
  //  RestoreFPState(sc.fpstate);
  //  dylan_integer_overflow_handler();           /* Should never return ... */
  //}

  // Here iff we should invoke the previous handler ...
  //if (oldHandler == SIG_DFL) {
  //  signal(signum, SIG_DFL);
  //  raise(signum);
  //}
  //else
    // ---*** NOTE: What if the old handler isn't expecting this calling sequence?
  //  (*(SIG_SIGCONTEXT)oldHandler)(signum, sc);

  return;
}
