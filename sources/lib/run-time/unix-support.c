#include <assert.h>
#include <time.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <pthread.h>
#include <dlfcn.h>

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

// this is our stack walker, used in SIGTRAP
#ifdef OPEN_DYLAN_ARCH_X86
static long getebp () {
    long ebp;
    asm("mov (%%ebp), %0"
        :"=r"(ebp));
    return ebp;
    return 0;
}
#endif

void walkstack() {
#ifdef OPEN_DYLAN_ARCH_X86
  long ebp = getebp();
  long eip;
  int rc;
  Dl_info info;

  while (ebp) {
    eip = *((long *)ebp + 1);
    rc = dladdr((void*)eip, &info);
    if (!rc||(!info.dli_sname && !info.dli_fname)) {
      printf("0x%lx (unknown)\n", eip);
    } else {
      if (!info.dli_sname) {
        printf("0x%lx (%s)\n", eip, info.dli_fname);
      } else {
        printf("%s+%ld (%s)\n",
               info.dli_sname,
               eip - (long)info.dli_saddr,
               info.dli_fname);
      }
    }
    ebp = *((long*)ebp);
  }
#endif
}
