#define _XOPEN_SOURCE 600L

#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <string.h>

int io_errno(void)
{
  return errno;
}

char *io_strerror(int errnum, char *buffer, size_t size)
{
#if defined(__GLIBC__) && defined(_GNU_SOURCE)
  // GNU-specific
  return strerror_r(errnum, buffer, size);
#else
  // POSIX/XSI standard
  strerror_r(errnum, buffer, size);
  return buffer;
#endif
}

long io_lseek (int fd, long offset, int whence) {
  return lseek(fd, offset, whence);
}

int io_fd_positionable(int fd) {
  struct stat st;
  if (fstat(fd, &st) < 0) {
    return -1;
  }

  return (st.st_mode & S_IFMT) == S_IFREG;
}
