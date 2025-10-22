#include <sys/types.h>
#include <sys/sysctl.h>
#include <stdlib.h>
#include <unistd.h>

uint application_filename_length () {
  int mib[4] = { CTL_KERN, KERN_PROC, KERN_PROC_PATHNAME, -1 };
  size_t len;
  sysctl(mib, 4, NULL, &len, NULL, 0);
  return len;
}

uint application_filename_name (char* buffer, size_t len) {
  int mib[4] = { CTL_KERN, KERN_PROC, KERN_PROC_PATHNAME, -1 };
  sysctl(mib, 4, buffer, &len, NULL, 0);
  return len;
}
