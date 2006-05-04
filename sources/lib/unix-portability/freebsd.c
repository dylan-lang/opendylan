#include <unistd.h>

int mylseek (int fildes, int offset, int whence) {
  return lseek(fildes, offset, whence);
}
