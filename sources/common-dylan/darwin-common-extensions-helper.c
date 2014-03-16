#include <limits.h>
#include <mach-o/dyld.h>
#include <stdlib.h>

extern void *MMAllocMisc(size_t size);
extern void MMFreeMisc(void *p, size_t size);

unsigned int application_filename_name (char* buffer, size_t capacity) {
  char buf[PATH_MAX];
  uint32_t bufferSize = PATH_MAX;
  char *p = buf;

  if (-1 == _NSGetExecutablePath(p, &bufferSize)) {
    p = MMAllocMisc(bufferSize);
    _NSGetExecutablePath(p, &bufferSize);
  }

  char *real = realpath(p, NULL);
  size_t realSize = real ? strlen(real) : 0;

  if ((NULL != buffer) && (NULL != real) && (realSize < capacity)) {
    memcpy(buffer, real, realSize);
  }

  if (NULL != real) {
    free(real);
  }

  if (p != buf) {
    MMFreeMisc(p, bufferSize);
  }

  return realSize;
}

/* We'll do the work twice, but that's the easiest way to make sure
 * we handle the length correctly.
 */
unsigned int application_filename_length () {
  return application_filename_name(NULL, 0) + 1;
}
