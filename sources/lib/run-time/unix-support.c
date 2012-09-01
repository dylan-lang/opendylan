#define _GNU_SOURCE

#include <time.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <sys/resource.h>

void mps_lib_abort(void)
{ 
  fflush(stdout);
  abort();
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
