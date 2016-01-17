#include "thread-utils.h"

#if defined(OPEN_DYLAN_PLATFORM_LINUX) || defined(OPEN_DYLAN_PLATFORM_OPENBSD)
#include <unistd.h>
#include <sys/syscall.h>
#elif defined(OPEN_DYLAN_PLATFORM_DARWIN)
#include <pthread.h>
#elif defined(OPEN_DYLAN_PLATFORM_FREEBSD)
#include <pthread_np.h>
#elif defined(OPEN_DYLAN_PLATFORM_NETBSD)
#include <lwp.h>
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
