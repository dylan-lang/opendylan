#define _GNU_SOURCE

#include <time.h>
#include <stdio.h>
#ifndef FREEBSD 
#include <malloc.h>
#else
#include <stdlib.h>
#endif
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

__inline__ D*  vector_data(SOV* vector) { return(vector->data); }

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

  { SOV* value = make_dylan_vector(2);
    D* data = (D*)vector_data(value);
    data[0] = I(stop.ru_utime.tv_sec);
    data[1] = I(stop.ru_utime.tv_usec);
    return((D)value); 
}}


/* Needed to keep code generated for applications happy ... */

int TargcT;
char **TargvT[];


/* Check for expiration of the Dylan runtime library */

#define RUN_TIME_API

/* Expiration date supplied by the makefile must be in the form YYYYMMYY
   and represents the last day that the runtime may be used ... */

#ifdef EXPIRATION
long encodedExpiration = EXPIRATION;
#else
long encodedExpiration = -1;
#endif

#define expirationMessageTemplate \
  "This test version of the Functional Developer runtime expired on DD-MMM-YYYY."
#define expirationMessageGeneric  \
  "This test version of the Functional Developer runtime has expired."

static char* MonthNames[13] = {"", "Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                   "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"};

static void insert_integer(long n, long nDigits, char *p) {
  long i, digit;
  for (i = 0; i < nDigits; i++) {
    digit = n % 10;
    n = n / 10;
    *p-- = digit + '0';
  }
}

static void insert_month(long m, char *p) {
  long i;
  for (i = 0; i < 3; i++)
    *p-- = MonthNames[m][2 - i];
}

RUN_TIME_API
void check_runtime_library_expiration_date() {
  time_t simpleNow;
  struct tm now;
  char *expirationMessage, *originalExpirationMessage;
  long expirationMessageSize, i;
  long encodedNow, expirationYear, expirationMonth, expirationDay;

  if (encodedExpiration > 0) {
    simpleNow = time(NULL);
    localtime_r(&simpleNow, &now);
    encodedNow = (10000 * (now.tm_year + 1900)) + (100 * (now.tm_mon + 1)) + now.tm_mday;
    if (encodedNow > encodedExpiration) {
      expirationMessageSize = sizeof(expirationMessageTemplate);
      expirationMessage = malloc(expirationMessageSize + 1);
      if (expirationMessage != NULL) {
	originalExpirationMessage = expirationMessageTemplate;
	for (i = 0; i < expirationMessageSize; i++)
	  expirationMessage[i] = originalExpirationMessage[i];
	expirationYear = encodedExpiration / 10000;
	expirationMonth = (encodedExpiration % 10000) / 100;
	expirationDay = encodedExpiration % 100;
	insert_integer(expirationYear, 4, &expirationMessage[expirationMessageSize - 3]);
	insert_month(expirationMonth, &expirationMessage[expirationMessageSize - 8]);
	insert_integer(expirationDay, 2, &expirationMessage[expirationMessageSize - 12]);
	fputs(expirationMessage, stderr);
      } else  
	fputs(expirationMessageGeneric, stderr);
      fputc('\n', stderr);
      exit(-1);
    }
  }

  return;
}
