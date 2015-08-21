#include <stdio.h>
#include <stdlib.h>

/* Plinth additions */

void mps_lib_abort(void)
{
  fflush(stdout);
  abort();
}

int mps_lib_fputs_(const char *s, int end, FILE *stream)
{
  // We know that on Unix, stream is just a FILE*.
  int i = 0;
  char c;
  while ((i < end) && (c = s[i++])) {
    fputc(c, (FILE *)stream);
  }
  return 1;
}
