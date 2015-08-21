#include <assert.h>
#include <pthread.h>

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
