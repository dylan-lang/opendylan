#include <assert.h>
#include <time.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <pthread.h>

void mps_lib_abort(void)
{
  fflush(stdout);
  abort();
}

/* Thread Local storage
 *
 * Implement this in C for invocation by HARP, just in
 * case the pthreads APIs are implemented as macros -
 * (which they might be according to the doc).
 */
int tlv_create_key(void)
{
  pthread_key_t key;
  int res = pthread_key_create(&key, NULL);
  assert(res == 0);
  return (int)key;
}


void tlv_destroy_key(int key)
{
  int res = pthread_key_delete((pthread_key_t)key);
  assert(res == 0);
}


void *tlv_get_value(int key)
{
  return pthread_getspecific((pthread_key_t)key);
}


void tlv_set_value(int key, void *value)
{
  int res = pthread_setspecific((pthread_key_t)key, value);
  assert(res == 0);
}

/* Plinth additions */

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
