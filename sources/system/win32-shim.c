#include <windows.h>

int system_concurrent_thread_count(void)
{
  SYSTEM_INFO sysinfo;
  GetSystemInfo(&sysinfo);
  return (int) sysinfo.dwNumberOfProcessors;
}
