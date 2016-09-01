#include "thread-utils.h"

#if defined(OPEN_DYLAN_PLATFORM_LINUX)
#include <unistd.h>
#include <sys/prctl.h>
#include <sys/syscall.h>
#include <pthread.h>
#elif defined(OPEN_DYLAN_PLATFORM_DARWIN)
#include <pthread.h>
#elif defined(OPEN_DYLAN_PLATFORM_FREEBSD)
#include <pthread.h>
#include <pthread_np.h>
#elif defined(OPEN_DYLAN_PLATFORM_NETBSD)
#include <lwp.h>
#elif defined(OPEN_DYLAN_PLATFORM_OPENBSD)
#include <unistd.h>
#include <sys/syscall.h>
#elif defined(OPEN_DYLAN_PLATFORM_WINDOWS)
#include <windows.h>
#endif

uint64_t dylan_current_thread_id(void)
{
#if defined(OPEN_DYLAN_PLATFORM_LINUX)
  return syscall(SYS_gettid);
#elif defined(OPEN_DYLAN_PLATFORM_DARWIN)
  uint64_t tid;
  pthread_threadid_np(pthread_self(), &tid);
  return tid;
#elif defined(OPEN_DYLAN_PLATFORM_FREEBSD)
  return pthread_getthreadid_np();
#elif defined(OPEN_DYLAN_PLATFORM_NETBSD)
  return _lwp_self();
#elif defined(OPEN_DYLAN_PLATFORM_OPENBSD)
  return syscall(SYS_getthrid);
#elif defined(OPEN_DYLAN_PLATFORM_WINDOWS)
  return GetCurrentThreadId();
#else
  #error dylan_current_thread_id is not yet implemented.
#endif
  return 0;
}

void dylan_set_current_thread_name(const char *name) {
#ifdef OPEN_DYLAN_PLATFORM_LINUX
  /* gdb shows this, so set it too */
  prctl(PR_SET_NAME, (unsigned long)name, 0, 0, 0);
  extern int pthread_setname_np(pthread_t, const char*) __attribute__((weak));
  if (pthread_setname_np) {
    pthread_setname_np(pthread_self(), name);
  }
#endif
#ifdef OPEN_DYLAN_PLATFORM_FREEBSD
  pthread_set_name_np(pthread_self(), name);
#endif
#ifdef OPEN_DYLAN_PLATFORM_DARWIN
  pthread_setname_np(name);
#endif
}
