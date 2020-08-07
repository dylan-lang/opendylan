#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>

#include "llvm-runtime.h"
#include "stack-walker.h"

#include <unistd.h>
#include <signal.h>
#include <sys/signal.h>
#include <sys/ucontext.h>

#include <fenv.h>
//#pragma STDC FENV_ACCESS ON

void primitive_reset_float_environment(void)
{
  feclearexcept(FE_ALL_EXCEPT);
#if defined OPEN_DYLAN_PLATFORM_FREEBSD || defined OPEN_DYLAN_PLATFORM_LINUX
#if defined OPEN_DYLAN_ARCH_X86
  // Clear the x87 stack
  __asm__ volatile("0:\n"
                   "  fnstsw %%ax\n"        // Retrieve x87 status word
                   "  andw $0x3800, %%ax\n" // Mask out TOP
                   "  je 1f\n"              // Done if stack is empty
                   "  fstp %%st(0)\n"       // Drop top of stack
                   "  jmp 0b\n"
                   "1:"
                   : /* no outputs */
                   : /* no inputs */
                   : "cc", "eax", "st");
#endif
  feenableexcept(FE_DIVBYZERO | FE_OVERFLOW | FE_UNDERFLOW | FE_INVALID);
#elif defined OPEN_DYLAN_PLATFORM_DARWIN \
  && (defined OPEN_DYLAN_ARCH_X86 || defined OPEN_DYLAN_ARCH_X86_64)
  fenv_t fenv;
  fegetenv(&fenv);
  fenv.__control &= ~(FE_DIVBYZERO | FE_OVERFLOW | FE_UNDERFLOW | FE_INVALID);
  fenv.__mxcsr &= ~((FE_DIVBYZERO | FE_OVERFLOW | FE_UNDERFLOW | FE_INVALID) << 7);
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
#elif defined OPEN_DYLAN_ARCH_X86_64
  uintptr_t *rsp = (uintptr_t *) uc->uc_mcontext.gregs[REG_RSP];
  *--rsp = uc->uc_mcontext.gregs[REG_RIP];
  uc->uc_mcontext.gregs[REG_RSP] = (uintptr_t) rsp;
  uc->uc_mcontext.gregs[REG_RIP] = ip;
#elif defined OPEN_DYLAN_ARCH_AARCH64
  uc->uc_mcontext.regs[30] = uc->uc_mcontext.pc; // Link register
  uc->uc_mcontext.pc = ip;
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

  case FPE_FLTINV:
    SetIP(uap, (uintptr_t) &dylan_float_invalid_error);
    break;

  case FPE_FLTOVF:
    SetIP(uap, (uintptr_t) &dylan_float_overflow_error);
    break;

  case FPE_FLTUND:
    SetIP(uap, (uintptr_t) &dylan_float_underflow_error);
    break;

  default:
    fprintf(stderr, "Unhandled FPE code %d\n", info->si_code);
    abort();
    break;
  }
}

#if defined OPEN_DYLAN_PLATFORM_LINUX \
  && (defined OPEN_DYLAN_ARCH_X86 || defined OPEN_DYLAN_ARCH_X86_64)
#define INT_OPCODE  0xCD        /* x86 INT instruction */
#define INTO_OPCODE 0xCE        /* x86 INTO instruction */

#if defined(OPEN_DYLAN_ARCH_X86)
#define IP_REGISTER REG_EIP
#endif
#if defined(OPEN_DYLAN_ARCH_X86_64)
#define IP_REGISTER REG_RIP
#endif

static struct sigaction oldsegvhandler;

static void DylanSEGVHandler (int sig, siginfo_t *info, void *uap)
{
  ucontext_t *uc = (ucontext_t *) uap;
  const unsigned char *eip
    = (const unsigned char *) uc->uc_mcontext.gregs[IP_REGISTER];
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

static void DylanTRAPHandler (int sig, siginfo_t *info, void *uap)
{
  dylan_dump_callstack(uap);
  _exit(127);
}

void EstablishDylanExceptionHandlers(void)
{
  struct sigaction traphandler;
  sigemptyset(&traphandler.sa_mask);
  traphandler.sa_sigaction = DylanTRAPHandler;
  traphandler.sa_flags = SA_SIGINFO;
  sigaction(SIGTRAP, &traphandler, NULL);

  struct sigaction fpehandler;
  sigemptyset(&fpehandler.sa_mask);
  fpehandler.sa_sigaction = DylanFPEHandler;
  fpehandler.sa_flags = SA_SIGINFO;
  sigaction(SIGFPE, &fpehandler, &oldfpehandler);

#if defined OPEN_DYLAN_PLATFORM_LINUX \
  && (defined OPEN_DYLAN_ARCH_X86 || defined OPEN_DYLAN_ARCH_X86_64)
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

// References:
// - http://www.mikeash.com/pyblog/friday-qa-2013-01-11-mach-exception-handlers.html
// - Memory Pool System sources, code/protxc.c

#include <stddef.h>
#include <pthread.h>

#define UNW_LOCAL_ONLY
#include <libunwind.h>

#include <mach/mach.h>

#include "mach_exc.h"

#if defined OPEN_DYLAN_ARCH_X86_64
#define THREAD_STATE_FLAVOR x86_THREAD_STATE64
#elif defined OPEN_DYLAN_ARCH_X86
#define THREAD_STATE_FLAVOR x86_THREAD_STATE32
#endif

#define STACK_ALIGNMENT 16

static mach_port_t exception_port = MACH_PORT_NULL;

void *catcher(void *dummy)
{
  extern boolean_t mach_exc_server(mach_msg_header_t *InHeadP,
                                   mach_msg_header_t *OutHeadP);

  pthread_setname_np("Dylan exception catcher");
  mach_msg_server(mach_exc_server, 2048, exception_port, 0);
  abort();
}

// We won't receive this since we're not asking for it
kern_return_t catch_mach_exception_raise
    (mach_port_t exception_port,
     mach_port_t thread,
     mach_port_t task,
     exception_type_t exception,
     mach_exception_data_t code,
     mach_msg_type_number_t codeCnt)
{
  return KERN_FAILURE;
}

// We won't receive this since we're not asking for it
kern_return_t catch_mach_exception_raise_state
    (mach_port_t exception_port,
     exception_type_t exception,
     const mach_exception_data_t code,
     mach_msg_type_number_t codeCnt,
     int *flavor,
     const thread_state_t old_state,
     mach_msg_type_number_t old_stateCnt,
     thread_state_t new_state,
     mach_msg_type_number_t *new_stateCnt)
{
  return KERN_FAILURE;
}

kern_return_t catch_mach_exception_raise_state_identity
    (mach_port_t exception_port,
     mach_port_t thread,
     mach_port_t task,
     exception_type_t exception,
     mach_exception_data_t code,
     mach_msg_type_number_t codeCnt,
     int *flavor,
     thread_state_t old_state,
     mach_msg_type_number_t old_stateCnt,
     thread_state_t new_state,
     mach_msg_type_number_t *new_stateCnt)
{
  uintptr_t handler;
  if (exception == EXC_ARITHMETIC) {
    switch (code[0]) {
    case EXC_I386_DIV:
      handler = (uintptr_t) &dylan_integer_divide_by_0_error;
      break;

    case EXC_I386_INTO:
      handler = (uintptr_t) &dylan_integer_overflow_error;
      break;

    case EXC_I386_EXTERR:
    case EXC_I386_SSEEXTERR:
      // code[1] contains the floating point status word
      if (code[1] & FE_DIVBYZERO) {
        handler = (uintptr_t) &dylan_float_divide_by_0_error;
      }
      else if (code[1] & FE_INVALID) {
        handler = (uintptr_t) &dylan_float_invalid_error;
      }
      else if (code[1] & FE_OVERFLOW) {
        handler = (uintptr_t) &dylan_float_overflow_error;
      }
      else if (code[1] & FE_UNDERFLOW) {
        handler = (uintptr_t) &dylan_float_underflow_error;
      }
      else {
        fprintf(stderr, "Unhandled exception: exception=%d code=%lld/%lld\n",
                exception, code[0], code[1]);
        abort();
      }
      break;

    default:
      fprintf(stderr, "Unhandled exception: exception=%d code=%lld/%lld\n",
              exception, code[0], code[1]);
      abort();
    }
  }
  else if (exception == EXC_BREAKPOINT) {
    x86_thread_state64_t *ts = (x86_thread_state64_t *) old_state;
    unw_context_t uc;
    memset(&uc, 0, sizeof uc);
#if defined OPEN_DYLAN_ARCH_X86_64
    // Both of these structures begin with the x86_64 general registers
    memcpy(&uc, ts, sizeof(uintptr_t) * 18);
#else
#error No thread state to unwind context conversion
#endif
    dylan_dump_callstack(&uc);
    _exit(127);
  }
  else {
    fprintf(stderr, "Unhandled exception: exception=%d code=%lld/%lld\n",
            exception, code[0], code[1]);
    abort();
  }

#if defined OPEN_DYLAN_ARCH_X86
  x86_thread_state32_t *ts = (x86_thread_state32_t *) old_state;
  uintptr_t *esp = (uintptr_t *) (ts->__esp & -STACK_ALIGNMENT);
  *--esp = ts->__eip;
  ts->__esp = (uintptr_t) esp;
  ts->__eip = handler;
#elif defined OPEN_DYLAN_ARCH_X86_64
  x86_thread_state64_t *ts = (x86_thread_state64_t *) old_state;
  uintptr_t *rsp = (uintptr_t *) (ts->__rsp & -STACK_ALIGNMENT);
  *--rsp = ts->__rip;
  ts->__rsp = (uintptr_t) rsp;
  ts->__rip = handler;
#else
#error No definition for catch_mach_exception_raise_state_identity provided
#endif
  *new_stateCnt = old_stateCnt;
  memcpy(new_state, old_state, old_stateCnt * sizeof(natural_t));

  return KERN_SUCCESS;
}

void EstablishDylanExceptionHandlers(void)
{
  if (exception_port == MACH_PORT_NULL) {
    // Need a port we can receive exceptions on
    kern_return_t rc
      = mach_port_allocate(mach_task_self(), MACH_PORT_RIGHT_RECEIVE,
                           &exception_port);
    if (rc != KERN_SUCCESS) {
      mach_error("mach_port_allocate send", rc);
      abort();
    }

    // Need to be able to send on it too
    rc = mach_port_insert_right(mach_task_self(), exception_port, exception_port,
                                MACH_MSG_TYPE_MAKE_SEND);
    if (rc != KERN_SUCCESS) {
      mach_error("mach_port_insert_right", rc);
      abort();
    }

    // Spawn a thread to serve exception requests
    pthread_attr_t attr;
    pthread_attr_init(&attr);
    pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
    pthread_t thread;
    int prc = pthread_create(&thread, &attr, catcher, NULL);
    pthread_attr_destroy(&attr);
    if (prc != 0) {
      fprintf(stderr, "%s: pthread_create returned %d\n",
              __func__, prc);
      abort();
    }
  }

  // Check if there is an existing EXC_BREAKPOINT handler (i.e. a
  // debugger) and if so don't request it on this exception port
  exception_mask_t exception_mask = EXC_MASK_ARITHMETIC|EXC_MASK_BREAKPOINT;
  {
    mach_msg_type_number_t count = 0;
    exception_mask_t masks[32];
    mach_port_t ports[32];
    exception_behavior_t behaviors[32];
    thread_state_flavor_t flavors[32];
    kern_return_t rc
      = task_get_exception_ports(mach_task_self(), EXC_MASK_BREAKPOINT,
                                 masks, &count, ports, behaviors, flavors);
    if (rc == KERN_SUCCESS) {
      for (mach_msg_type_number_t i = 0; i < count; ++i) {
        if (MACH_PORT_VALID(ports[i])) {
          exception_mask &= ~EXC_MASK_BREAKPOINT;
          break;
        }
      }
    }
  }

  // Set this thread's exception port
  kern_return_t rc
    = thread_set_exception_ports(mach_thread_self(),
                                 exception_mask,
                                 exception_port,
                                 EXCEPTION_STATE_IDENTITY|MACH_EXCEPTION_CODES,
                                 THREAD_STATE_FLAVOR);
  if (rc != KERN_SUCCESS) {
    mach_error("thread_set_exception_ports", rc);
    abort();
  }

  primitive_reset_float_environment();
}

void RemoveDylanExceptionHandlers(void)
{
  // Nothing to do (thread is about to exit)
}

#endif
