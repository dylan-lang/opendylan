#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>

#include "llvm-runtime.h"

#include <signal.h>
#include <sys/signal.h>
#include <sys/ucontext.h>

#include <fenv.h>
//#pragma STDC FENV_ACCESS ON

void primitive_reset_float_environment(void)
{
  feclearexcept(FE_ALL_EXCEPT);
#if defined OPEN_DYLAN_PLATFORM_FREEBSD || defined OPEN_DYLAN_PLATFORM_LINUX
  feenableexcept(FE_DIVBYZERO | FE_OVERFLOW | FE_UNDERFLOW);
#elif defined OPEN_DYLAN_PLATFORM_DARWIN \
  && (defined OPEN_DYLAN_ARCH_X86 || defined OPEN_DYLAN_ARCH_X86_64)
  fenv_t fenv;
  fegetenv(&fenv);
  fenv.__control &= ~(FE_DIVBYZERO | FE_OVERFLOW | FE_UNDERFLOW);
  fenv.__mxcsr &= ~((FE_DIVBYZERO | FE_OVERFLOW | FE_UNDERFLOW) << 7);
  fesetenv(&fenv);
#endif
}

#if defined OPEN_DYLAN_PLATFORM_FREEBSD
#include <ucontext.h>
static inline void SetIP(void *uap, uintptr_t ip)
{
  ucontext_t *uc = (ucontext_t *) uap;
#if defined OPEN_DYLAN_ARCH_X86
  // Simulate the effect of a calll instruction
  uintptr_t *esp = (uintptr_t *) uc->uc_mcontext.mc_esp;
  *--esp = uc->uc_mcontext.mc_eip;
  uc->uc_mcontext.mc_esp = (uintptr_t) esp;
  uc->uc_mcontext.mc_eip = ip;
#elif defined OPEN_DYLAN_ARCH_X86_64
  uintptr_t *rsp = (uintptr_t *) uc->uc_mcontext.mc_rsp;
  *--rsp = uc->uc_mcontext.mc_rip;
  uc->uc_mcontext.mc_rsp = (uintptr_t) rsp;
  uc->uc_mcontext.mc_rip = ip;
#else
#error Unsupported FreeBSD arch
#endif
}
#elif defined OPEN_DYLAN_PLATFORM_LINUX
#include <sys/ucontext.h>
#include <ucontext.h>
static inline void SetIP(void *uap, uintptr_t ip)
{
  ucontext_t *uc = (ucontext_t *) uap;
#if OPEN_DYLAN_ARCH_X86
  uintptr_t *esp = (uintptr_t *) uc->uc_mcontext.gregs[REG_ESP];
  *--esp = uc->uc_mcontext.gregs[REG_EIP];
  uc->uc_mcontext.gregs[REG_ESP] = (greg_t) esp;
  uc->uc_mcontext.gregs[REG_EIP] = (greg_t) ip;
#else
#error Unsupported Linux arch
#endif
}
#elif !defined OPEN_DYLAN_PLATFORM_DARWIN
#error No definition for SetIP provided
#endif

#if defined OPEN_DYLAN_PLATFORM_FREEBSD || defined OPEN_DYLAN_PLATFORM_LINUX
static struct sigaction oldfpehandler;

static void DylanFPEHandler (int sig, siginfo_t *info, void *uap)
{
  switch (info->si_code) {
  case FPE_INTDIV:
    SetIP(uap, (uintptr_t) &dylan_integer_divide_by_0_error);
    break;

  case FPE_INTOVF:
    SetIP(uap, (uintptr_t) &dylan_integer_overflow_error);
    break;

  case FPE_FLTDIV:
    SetIP(uap, (uintptr_t) &dylan_float_divide_by_0_error);
    break;

  case FPE_FLTOVF:
    SetIP(uap, (uintptr_t) &dylan_float_overflow_error);
    break;

  case FPE_FLTUND:
    SetIP(uap, (uintptr_t) &dylan_float_underflow_error);
    break;

  default:
    abort();
    break;
  }
}

#if defined OPEN_DYLAN_PLATFORM_LINUX \
  && (defined OPEN_DYLAN_ARCH_X86 || defined OPEN_DYLAN_ARCH_X86_64)
#define INT_OPCODE  0xCD        /* x86 INT instruction */
#define INTO_OPCODE 0xCE        /* x86 INTO instruction */

static struct sigaction oldsegvhandler;

static void DylanSEGVHandler (int sig, siginfo_t *info, void *uap)
{
  ucontext_t *uc = (ucontext_t *) uap;
  const unsigned char *eip
    = (const unsigned char *) uc->uc_mcontext.gregs[REG_EIP];
  switch (info->si_code) {
  case SEGV_MAPERR:
  case SEGV_ACCERR:
    break;

  default:
    if (eip[-1] == INTO_OPCODE || (eip[-2] == INT_OPCODE && eip[-1] == 0x04)) {
      SetIP(uap, (uintptr_t) &dylan_integer_overflow_error);
    }
  }
}
#endif

void EstablishDylanExceptionHandlers(void)
{
  struct sigaction fpehandler;
  sigemptyset(&fpehandler.sa_mask);
  fpehandler.sa_sigaction = DylanFPEHandler;
  fpehandler.sa_flags = SA_SIGINFO;
  sigaction(SIGFPE, &fpehandler, &oldfpehandler);

#if defined OPEN_DYLAN_PLATFORM_LINUX
  struct sigaction segvhandler;
  sigemptyset(&segvhandler.sa_mask);
  segvhandler.sa_sigaction = DylanSEGVHandler;
  segvhandler.sa_flags = SA_SIGINFO;
  sigaction(SIGSEGV, &segvhandler, &oldsegvhandler);
#endif

  primitive_reset_float_environment();
}

void RemoveDylanExceptionHandlers(void)
{
  sigaction(SIGFPE, &oldfpehandler, NULL);
}

#elif defined OPEN_DYLAN_PLATFORM_DARWIN

void EstablishDylanExceptionHandlers(void)
{
  // FIXME
}

void RemoveDylanExceptionHandlers(void)
{
  // FIXME
}

#endif
