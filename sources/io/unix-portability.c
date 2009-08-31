#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

int io_errno(void)
{
  return errno;
}

long io_lseek (int fd, long offset, int whence) {
  return lseek(fd, offset, whence);
}

int io_fd_info(int fd) {
  struct stat st;
  if(fstat(fd, &st) < 0)
    return -1;

  return st.st_blksize | ((st.st_mode & S_IFMT) == S_IFREG);
}
