#include <dlfcn.h>
#include <stdio.h>

// this is our stack walker, used in SIGTRAP
#if defined(OPEN_DYLAN_PLATFORM_DARWIN)

#include <unwind.h>

static _Unwind_Reason_Code frame_func(struct _Unwind_Context *context, void *arg)
{
  void *ip = (void*)_Unwind_GetIP(context);
  Dl_info info;
  dladdr(ip, &info);
  printf("  %p %s\n", ip, info.dli_sname);
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
  long eip;
  int rc;
  Dl_info info;

  while (ebp) {
    eip = *((long *)ebp + 1);
    rc = dladdr((void*)eip, &info);
    if (!rc||(!info.dli_sname && !info.dli_fname)) {
      printf("0x%lx (unknown)\n", eip);
    } else {
      if (!info.dli_sname) {
        printf("0x%lx (%s)\n", eip, info.dli_fname);
      } else {
        printf("%s+%ld (%s)\n",
               info.dli_sname,
               eip - (long)info.dli_saddr,
               info.dli_fname);
      }
    }
    ebp = *((long*)ebp);
  }
}

#else

void dylan_dump_callstack(void)
{
}

#endif
