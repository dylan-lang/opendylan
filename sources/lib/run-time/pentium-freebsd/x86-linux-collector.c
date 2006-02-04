/* INTERIM DYLAN RUN-TIME SYSTEM INTERFACE
 *
 * $HopeName: D-lib-pentium-run-time!boehm-collector.c(trunk.1) $
 * Copyright (C) 1996 Functional Objects, Inc. All rights reserved
 */

#define _GNU_SOURCE

/* flag to select Linux */
#define LINUX_PLATFORM
#define X86_LINUX_PLATFORM
//#define BOEHM_GC

#include <sys/signal.h>

#include "collector.c"


/* HACK -- Implement GC_malloc and GC_free
 *
 * This should not be necessary as nobody should be 
 * calling them. But the date library (amongst others)
 * currently does. Such libraries should be changed to call
 * MMAllocMisc and MMFreeMisc instead - otherwise Dylan will
 * never be able to interoperate with Boehm.
 */

#ifndef BOEHM_GC

void *GC_malloc(size_t size)
{
  return mps__malloc(size);
}

void GC_free(void *old)
{
  mps__free(old);
}

#endif





/* Thread Local storage 
 *
 * Implement this is C for invocatiuon by HARP, just in 
 * case the pthreads APIs are implemented as macros -
 * (which they might be according to the doc).
 */


extern int tlv_create_key(void)
{
  pthread_key_t key;
  int res = pthread_key_create(&key, NULL);
  assert(res == 0);
  return (int)key;
}


extern void tlv_destroy_key(int key)
{
  int res = pthread_key_delete((pthread_key_t)key);
  assert(res == 0);
}


extern void *tlv_get_value(int key)
{
  return pthread_getspecific((pthread_key_t)key);
}


extern void tlv_set_value(int key, void *value)
{
  int res = pthread_setspecific((pthread_key_t)key, value);
  assert(res == 0);
}



/* Heap stat stuff */

void clear_wrapper_stats ()
{
}


void display_wrapper_stats ()
{
}

void add_stat_for_object (void *object, void* wrapper, int size)
{
}

typedef struct wrapper_stats_s *wrapper_stats_t;

typedef struct wrapper_stats_s {
  void *wrapper_address;
  int  usage_count;
  int  usage_size;
} wrapper_stats_s;

void display_stats_for_memory_usage ()
{
}

void display_wrapper_breakpoints()
{
}

#include "break.c"

