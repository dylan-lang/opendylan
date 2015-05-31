#include <dlfcn.h>
#include <stdio.h>

void print_frame(void *ip)
{
#ifdef OPEN_DYLAN_PLATFORM_UNIX
  Dl_info info;
  int rc;

  rc = dladdr(ip, &info);
  if (!rc||(!info.dli_sname && !info.dli_fname)) {
    printf("0x%lx (unknown)\n", (long)ip);
  } else {
    if (!info.dli_sname) {
      printf("0x%lx (%s)\n", (long)ip, info.dli_fname);
    } else {
      printf("%s+%ld (%s)\n",
             info.dli_sname,
             (long)ip - (long)info.dli_saddr,
             info.dli_fname);
    }
  }
#endif
}

// this is our stack walker, used in SIGTRAP
#if defined(OPEN_DYLAN_PLATFORM_DARWIN)

#include <unwind.h>

static _Unwind_Reason_Code frame_func(struct _Unwind_Context *context, void *arg)
{
  void *ip = (void*)_Unwind_GetIP(context);
  print_frame(ip);
  return(_URC_NO_REASON);
}

void dylan_dump_callstack(void)
{
  _Unwind_Backtrace(frame_func, NULL);
}

#elif defined(OPEN_DYLAN_ARCH_X86) && \
      (defined(OPEN_DYLAN_PLATFORM_LINUX) || \
       defined(OPEN_DYLAN_PLATFORM_FREEBSD))

static long getebp (void)
{
    long ebp;
    asm("mov (%%ebp), %0"
        :"=r"(ebp));
    return ebp;
    return 0;
}

void dylan_dump_callstack(void)
{
  long ebp = getebp();
  while (ebp) {
    long eip = *((long *)ebp + 1);
    print_frame((void*)eip);
    ebp = *((long*)ebp);
  }
}

#else

void dylan_dump_callstack(void)
{
}

#endif
