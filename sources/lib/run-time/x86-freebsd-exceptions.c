/* Support for handling exceptions in Dylan (other than MM traps) */
/* Currently, we just handle stack overflows arithmetic exceptions  */

extern int inside_dylan_ffi_barrier();
extern void dylan_stack_overflow_handler(void *base_address, int size, unsigned long protection);
extern void dylan_integer_overflow_handler();
extern void dylan_integer_divide_0_handler();
extern void dylan_float_divide_0_handler();
extern void dylan_float_overflow_handler();
extern void dylan_float_underflow_handler();

extern void walkstack();

/* FreeBSD exception handling:  Setup a signal handler for SIGFPE (floating point exceptions).
   We rely on the fact that FreeBSD passes a second argument containing the error context. */

/* ---*** TODO: Find out how to trap stack overflows and add an appropriate handler */

#include <sys/signal.h>
#include <ucontext.h>
#include <ieeefp.h>
#include <fenv.h>

#define EXCEPTION_PREAMBLE() \
  struct sigaction oldFPEHandler; \
  struct sigaction oldSEGVHandler; \
  struct sigaction oldTRAPHandler; \
EstablishDylanExceptionHandlers(&oldFPEHandler, &oldSEGVHandler, &oldTRAPHandler);      \
  {

#define EXCEPTION_POSTAMBLE() \
  } \
  RemoveDylanExceptionHandlers(&oldFPEHandler, &oldSEGVHandler, &oldTRAPHandler);

static void DylanFPEHandler (int sig, siginfo_t *info, void *sc);
static void DylanSEGVHandler (int sig, siginfo_t *info, void *sc);
static void DylanTRAPHandler (int sig, siginfo_t *info, void *sc);

static void EstablishDylanExceptionHandlers (struct sigaction * oldFPEHandler,
                                             struct sigaction * oldSEGVHandler,
                                             struct sigaction * oldTRAPHandler)
{
  struct sigaction newFPEHandler;
  struct sigaction newSEGVHandler;
  struct sigaction newTRAPHandler;

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

  sigfillset(&newTRAPHandler.sa_mask);
  newTRAPHandler.sa_sigaction = DylanTRAPHandler;
  newTRAPHandler.sa_flags = SA_SIGINFO;
  sigaction(SIGTRAP, &newTRAPHandler, oldTRAPHandler);

  signal(SIGPIPE, SIG_IGN);
}

static void RemoveDylanExceptionHandlers (struct sigaction * oldFPEHandler,
                                          struct sigaction * oldSEGVHandler,
                                          struct sigaction * oldTRAPHandler)
{
  sigaction(SIGFPE, oldFPEHandler, NULL);
#if 0
  sigaction(SIGSEGV, oldSEGVHandler, NULL);
#endif
  sigaction(SIGTRAP, oldTRAPHandler, NULL);
}

static __inline
void RestoreFPState ()
{
  fpresetsticky(fpgetsticky());
  fpsetmask(FP_X_INV | FP_X_DZ | FP_X_OFL);
  return;
}

static void DylanFPEHandler (int sig, siginfo_t *info, void *uap)
{
  if (inside_dylan_ffi_barrier() == 0) {
  } else {
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

static void DylanTRAPHandler (int sig, siginfo_t *info, void *sc)
{
  walkstack();
}
