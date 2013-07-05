/* Support for handling exceptions in Dylan (other than MM traps) */
/* Currently, we just handle stack overflows arithmetic exceptions  */

/* ---*** TODO: Find out how to trap stack overflows on Linux */

#include <stdlib.h>
#include <unistd.h>
#include <stddef.h>
#include <stdio.h>
#include <signal.h>
#include <sys/ucontext.h>
#include <ucontext.h>
#include <fpu_control.h>

extern int inside_dylan_ffi_barrier();
extern void dylan_stack_overflow_handler(void *base_address, int size, unsigned long protection);
extern void dylan_integer_overflow_handler();
extern void dylan_integer_divide_0_handler();
extern void dylan_float_divide_0_handler();
extern void dylan_float_overflow_handler();
extern void dylan_float_underflow_handler();

extern void walkstack();

/* Linux exception handling: Setup signal handlers for SIGFPE
 * (floating point exceptions), SIGSEGV (segmentation violation), and
 * SIGTRAP (breakpoint trap).
 *
 * ---*** NOTE: On x86 Linux, our use of the INT 4 and INTO
 *        instructions to raise an integer overflow exception actually
 *        results in a SIGSEGV signal being raised. Though this
 *        doesn't really make much sense, that is how things are
 *        defined in the SysV x86 ABI. In our SIGSEGV handler we check
 *        for this case, and chain to an outer SIGSEGV handler if a
 *        memory access error is involved.
 */

/* ---*** TODO: Find out how to trap stack overflows and add an
          appropriate handler
 */

#define INT_OPCODE  0xCD        /* x86 INT instruction */
#define INTO_OPCODE 0xCE        /* x86 INTO instruction */

/* FPU Control Word mask enabling exceptions for divide-by-zero,
 * overflow, and underflow
 */
#define DYLAN_FPU_CW (_FPU_DEFAULT              \
                      & ~(_FPU_MASK_ZM          \
                          | _FPU_MASK_OM        \
                          | _FPU_MASK_UM))

static unsigned exception_handler_level = 0;
static struct sigaction outer_FPEHandler;
static struct sigaction outer_SEGVHandler;
static struct sigaction outer_TRAPHandler;

#define EXCEPTION_PREAMBLE()                           \
  {                                                    \
    if (exception_handler_level++ == 0) {              \
      EstablishDylanExceptionHandlers();               \
    }

#define EXCEPTION_POSTAMBLE()                          \
    if (--exception_handler_level == 0) {              \
      RemoveDylanExceptionHandlers();                  \
    }                                                  \
  }

static void DylanFPEHandler (int sig, siginfo_t *info, void *sc);
static void DylanSEGVHandler (int sig, siginfo_t *info, void *sc);
static void DylanTRAPHandler (int sig, siginfo_t *info, void *sc);

static void EstablishDylanExceptionHandlers(void)
{
  struct sigaction newFPEHandler;
  struct sigaction newSEGVHandler;
  struct sigaction newTRAPHandler;

  unsigned short cw;

  sigemptyset(&newFPEHandler.sa_mask);
  newFPEHandler.sa_sigaction = DylanFPEHandler;
  newFPEHandler.sa_flags = SA_SIGINFO;
  sigaction(SIGFPE, &newFPEHandler, &outer_FPEHandler);

  sigemptyset(&newSEGVHandler.sa_mask);
  newSEGVHandler.sa_sigaction = DylanSEGVHandler;
  newSEGVHandler.sa_flags = SA_SIGINFO;
  sigaction(SIGSEGV, &newSEGVHandler, &outer_SEGVHandler);

  sigemptyset(&newTRAPHandler.sa_mask);
  newTRAPHandler.sa_sigaction = DylanTRAPHandler;
  newTRAPHandler.sa_flags = SA_SIGINFO;
  sigaction(SIGTRAP, &newTRAPHandler, &outer_TRAPHandler);

  signal(SIGPIPE, SIG_IGN);

  /* Set the FPU control word */
  cw = DYLAN_FPU_CW;
  _FPU_SETCW(cw);
}

static void RemoveDylanExceptionHandlers (void)
{
  sigaction(SIGFPE, &outer_FPEHandler, NULL);
  sigaction(SIGSEGV, &outer_SEGVHandler, NULL);
  sigaction(SIGTRAP, &outer_TRAPHandler, NULL);
}

static __inline
void RestoreFPState (ucontext_t *uc)
{
  unsigned long int cw = DYLAN_FPU_CW;
  if (uc->uc_mcontext.fpregs) {
    uc->uc_mcontext.fpregs->cw = cw;
  }
  _FPU_SETCW(cw);
}

static void DylanFPEHandler (int sig, siginfo_t *info, void *uap)
{
  unused(sig);
  if (inside_dylan_ffi_barrier() == 0) {
  } else {
    ucontext_t *uc = (ucontext_t *) uap;

    switch (info->si_code) {
    case FPE_INTDIV:
      RestoreFPState(uc);
      uc->uc_mcontext.gregs[REG_EIP] = (greg_t) dylan_integer_divide_0_handler;
      break;

    case FPE_INTOVF:
      RestoreFPState(uc);
      uc->uc_mcontext.gregs[REG_EIP] = (long) dylan_integer_overflow_handler;
      break;

    case FPE_FLTDIV:
      RestoreFPState(uc);
      uc->uc_mcontext.gregs[REG_EIP] = (long) dylan_float_divide_0_handler;
      break;

    case FPE_FLTOVF:
      RestoreFPState(uc);
      uc->uc_mcontext.gregs[REG_EIP] = (long) dylan_float_overflow_handler;
      break;

    case FPE_FLTUND:
      RestoreFPState(uc);
      uc->uc_mcontext.gregs[REG_EIP] = (long) dylan_float_underflow_handler;
      break;

    default:
      break;
    }
  }
}

static void DylanSEGVHandler (int sig, siginfo_t *info, void *uap)
{
  if (inside_dylan_ffi_barrier() == 0) {
  } else {
    ucontext_t *uc = (ucontext_t *) uap;
    const unsigned char *eip
      = (const unsigned char *)uc->uc_mcontext.gregs[REG_EIP];

    switch (info->si_code) {
    case SEGV_MAPERR:
    case SEGV_ACCERR:
      break;

    default:
      if (eip[-1] == INTO_OPCODE
          || (eip[-2] == INT_OPCODE && eip[-1] == 0x04)) {
        uc->uc_mcontext.gregs[REG_EIP] = (long) dylan_integer_overflow_handler;
        return;
      }
      break;
    }
  }

  assert(outer_SEGVHandler.sa_flags & SA_SIGINFO);
  (*outer_SEGVHandler.sa_sigaction)(sig, info, uap);
}

static void DylanTRAPHandler (int sig, siginfo_t *info, void *uap)
{
  walkstack();
  assert(outer_TRAPHandler.sa_flags & SA_SIGINFO);
  (*outer_TRAPHandler.sa_sigaction)(sig, info, uap);
}
