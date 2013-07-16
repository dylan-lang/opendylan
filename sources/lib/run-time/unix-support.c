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


/* Support for Dylan timer primitives */

typedef void* D;

#define define_SOV(_name, _size) \
  typedef struct _sov##_name { \
    D class; \
    D size; \
    D data[_size]; \
  } _name;

define_SOV(SOV, 1);

static __inline__ D*  vector_data(SOV* vector) { return(vector->data); }

#define ITAG 1
#define I(n) ((D)((((unsigned long)(n))<<2)|ITAG))

extern void *make_dylan_vector(int n);

static struct rusage start, stop;

void c_primitive_start_timer()
{
  getrusage(RUSAGE_SELF, &start);
}

D c_primitive_stop_timer()
{
  getrusage(RUSAGE_SELF, &stop);

  stop.ru_utime.tv_usec -= start.ru_utime.tv_usec;
  stop.ru_utime.tv_sec -= start.ru_utime.tv_sec;

  if (stop.ru_utime.tv_usec < 0) {
    stop.ru_utime.tv_usec += 1000000;
    stop.ru_utime.tv_sec -= 1;
  }

  {
    SOV* value = make_dylan_vector(2);
    D* data = (D*)vector_data(value);
    data[0] = I(stop.ru_utime.tv_sec);
    data[1] = I(stop.ru_utime.tv_usec);
    return((D)value);
  }
}

// this is our stack walker, used in SIGTRAP
static long getebp () {
    long ebp;
    asm("mov (%%ebp), %0"
        :"=r"(ebp));
    return ebp;
}

void walkstack() {
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
}
