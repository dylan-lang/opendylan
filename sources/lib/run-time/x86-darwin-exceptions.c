/* Support for handling exceptions in Dylan (other than MM traps) */

extern int inside_dylan_ffi_barrier();
extern void dylan_integer_overflow_handler();
extern void dylan_integer_divide_0_handler();
extern void dylan_float_divide_0_handler();
extern void dylan_float_overflow_handler();
extern void dylan_float_underflow_handler();


/* ---*** TODO: Implement all of this */

#include <sys/signal.h>
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

static void EstablishDylanExceptionHandlers (struct sigaction * oldFPEHandler,
                                             struct sigaction * oldSEGVHandler)
{
  struct sigaction newFPEHandler;

  unused(oldSEGVHandler);

  sigemptyset(&newFPEHandler.sa_mask);
  newFPEHandler.sa_sigaction = DylanFPEHandler;
  newFPEHandler.sa_flags = SA_SIGINFO;
  sigaction(SIGFPE, &newFPEHandler, oldFPEHandler);

  signal(SIGPIPE, SIG_IGN);
}

static void RemoveDylanExceptionHandlers (struct sigaction * oldFPEHandler,
                                          struct sigaction * oldSEGVHandler)
{
  unused(oldSEGVHandler);
  sigaction(SIGFPE, oldFPEHandler, NULL);
}

static void DylanFPEHandler (int sig, siginfo_t *info, void *uap)
{
  unused(sig);
  unused(info);
  unused(uap);
  if (inside_dylan_ffi_barrier() != 0) {
    // Fill this in ...
  }
}
