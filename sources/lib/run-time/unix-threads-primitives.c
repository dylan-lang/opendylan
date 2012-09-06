/*
 * File:    unix-threads-primitives.c
 * Author:  Tony Mann
 * Copyright: 1996 The Harlequin Group Limited. All rights reserved.
 *
 * A description of the implementation of the primitives in this file can be
 * found in D-doc-design-runtime!win32-thread-portability.text
 */

#define _GNU_SOURCE

#include <assert.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <stddef.h>
#include <errno.h>
#include <time.h>
#include <sys/types.h>

#ifdef GC_USE_BOEHM
#include <gc/gc.h>
#endif

#include "unix-types.h"

#include "unix-threads-primitives.h"

#define ignore(x) (void)x

/*****************************************************************************/
/* GLOBAL VARIABLE DECLARATIONS                                              */
/*                                                                           */
/* There is an implementation constraint that other parts of the Dylan       */
/* runtime system must ensure that TLV vectors are static (by referencing    */
/* them from ambiguous roots). Hence they may also be referenced from C data */
/* structures. The variable default_tlv_vector is now defined externally     */
/* so that it can be an ambiguous root.                                      */
/*                                                                           */
/*****************************************************************************/

/* A large negative number used to indicate that a thread is in the process
   of growing the TLV vector */
static const long TLV_GROW = -2000000;

#ifdef C_TESTING
  DWORD TlsIndexThread;
  DWORD TlsIndexThreadHandle;
  DWORD TlsIndexThreadVector;
  TLV_VECTOR default_tlv_vector = NULL;
#else
/*****************************************************************************/
/* Provided by the HARP runtime                                              */
/*****************************************************************************/
  extern TLV_VECTOR default_tlv_vector;
#endif

static int TLV_vector_offset = 3*sizeof(Z);

/* This is referenced from Dylan code */
long tlv_writer_counter = 0;

static define_CRITICAL_SECTION(tlv_vector_list_lock);
static TLV_VECTOR_LIST  tlv_vector_list;

static const size_t linksize = sizeof(struct tlv_vector_list_element);


/*****************************************************************************/
/* LOCAL FUNCTION DECLARATIONS                                               */
/*****************************************************************************/

static void  initialize_threads_primitives(void);

static void       grow_all_tlv_vectors(int newsize);
static TLV_VECTOR grow_tlv_vector(TLV_VECTOR vector, int newsize);

static void  copy_tlv_vector(TLV_VECTOR destination, TLV_VECTOR source);
static void  update_tlv_vectors(int newindex, Z value);
static void  add_tlv_vector(pthread_t newthread, TLV_VECTOR tlv_vector);
static int   remove_tlv_vector(pthread_t thread);


/*****************************************************************************/
/* EXTERNAL FUNCTIONS                                                        */
/*****************************************************************************/

/* Increment the 32-bit value pointed to by var. Prevents other threads from
 * using the value simultaneously.
 * Returns: the new incremented value
 */
static inline
long internal_InterlockedIncrement(long *var)
{
  return __sync_add_and_fetch(var, 1);
}

/* Decrement the 32-bit value pointed to by var. Prevents other threads from
 * using the value simultaneously
 * Returns: the new decremented value
 */
static inline
long internal_InterlockedDecrement(long *var)
{
  return __sync_sub_and_fetch(var, 1);
}

/* Atomically compares the destination and compare values, and stores the
 * exchange value in the destination if they are equal (otherwise does
 * nothing). Returns the initial value of the destination.
 */
static inline
long internal_InterlockedCompareExchange(long *destination, long exchange,
                                          long compare)
{
  return __sync_val_compare_and_swap(destination, compare, exchange);
}

#ifdef C_TESTING
/*****************************************************************************/
/* Implementation for C tests                                                */
/*****************************************************************************/

void *make_dylan_vector(int n)
{
  return (malloc((n+2) * sizeof(Z)));
}

void *get_tlv_vector(void)
{
  return (void *)(TlsGetValue(TlsIndexThreadVector));
}

void set_tlv_vector(void *vector)
{
  TlsSetValue(TlsIndexThreadVector, vector);
}

void *get_current_thread()
{
  return (void *)(TlsGetValue(TlsIndexThread));
}

void set_current_thread(void *thread)
{
  TlsSetValue(TlsIndexThread, thread);
}

void *get_current_thread_handle()
{
  return (void *)(TlsGetValue(TlsIndexThreadHandle));
}

void set_current_thread_handle(void *handle)
{
  TlsSetValue(TlsIndexThreadHandle, handle);
}

/* This is the starting function for the new thread. It calls the
 * dylan trampoline function which we rely on to initialise the thread.
 */
DWORD WINAPI
dylan_thread_trampoline(void **arg)
{
  trampoline_body(arg[0], 0);
  return 0;
}

void *MMAllocMisc(size_t size)
{
  return malloc(size);
}

void MMFreeMisc(void *old, size_t size)
{
  free(old);
}

#else
/*****************************************************************************/
/* Provided by the HARP runtime                                              */
/*****************************************************************************/

extern void *call_first_dylan_function(void *func, int num_args, ...);
extern void *make_dylan_vector(int n);
extern void *get_tlv_vector(void);
extern void  set_tlv_vector(void *vector);
extern void *get_current_thread();
extern void  set_current_thread(void *thread);
extern pthread_t get_current_thread_handle();
extern void  set_current_thread_handle(pthread_t handle);
extern void *get_current_teb();
extern int   dylan_init_thread(void **rReturn, void *(*f)(void *, size_t),
                               void *p, size_t s);
extern void *dylan_thread_trampoline(void *thread);

extern void *MMAllocMisc(size_t size);
extern void MMFreeMisc(void *old, size_t size);


#endif


THREADS_RUN_TIME_API
void primitive_write_thread_variable_internal()
{
  do {
    if (internal_InterlockedDecrement(&tlv_writer_counter) < 0) {
      pthread_mutex_lock(&tlv_vector_list_lock);
      pthread_mutex_unlock(&tlv_vector_list_lock);
    }
  } while(internal_InterlockedIncrement(&tlv_writer_counter) < 0);
}


extern void *dylan__malloc__ambig(size_t size);
extern void *dylan_false;


/*****************************************************************************/
/* THREAD PRIMITIVES                                                         */
/*****************************************************************************/


/* 1 */
THREADS_RUN_TIME_API  ZINT
primitive_make_thread(DTHREAD *newthread, D_NAME name,
                      ZINT zpriority, ZFN func, BOOL synchronize)
{
  int    priority = (int)zpriority >> 2;
  int status;
  DTHREAD **newthread_ptr;

  ignore(priority);

  newthread_ptr = (DTHREAD **)(dylan__malloc__ambig(4));
  newthread_ptr[0] = newthread;

  assert(newthread != NULL);
  assert(IS_ZINT(zpriority));
  assert(func != NULL);


  // dylan_thread_trampoline is the starting function for the new thread. 
  // It calls the dylan trampoline fucntion which we rely on to initialise 
  // the thread


  newthread->handle2 = func;
  status = pthread_create((pthread_t *)(&newthread->handle1), NULL, 
			  dylan_thread_trampoline, 
			  (void*)newthread_ptr);
  

  if (status != 0) {
    MSG1("make-thread: pthread_create returned error %d\n", status);
    return CREATE_ERROR;
  }

  // Ignore the priority for now @@@@#!"£$
  return OK;
}

void *
trampoline_body(void *arg, size_t ignore)
{
  DTHREAD *thread;
  ZFN    dylan_trampoline;

  assert(arg != NULL);

  thread = (DTHREAD *)arg;
  dylan_trampoline = (ZFN)thread->handle2;

#ifdef C_TESTING
  primitive_initialize_current_thread(thread);
  (*dylan_trampoline)(NULL, 0);  // method for C tests only
#else
  call_first_dylan_function((void *)dylan_trampoline, 0);
#endif

  remove_tlv_vector((pthread_t)thread->handle1);
  return 0;
}


/* 2 */
THREADS_RUN_TIME_API  ZINT
primitive_destroy_thread(DTHREAD *thread)
{
  pthread_t pThread;
  int status;

  assert(thread != NULL);

  if (thread->handle1 != NULL) {
    pThread = (pthread_t)(thread->handle1);
    
    status = pthread_detach(pThread);
    if (status != 0) {
      MSG2("pthread_detach %p failed with error %d\n",
	   (void *) pThread, status);
    }
  }

  return OK;
}


/* 3 */
THREADS_RUN_TIME_API  ZINT
primitive_thread_join_single(DTHREAD *thread)
{
  pthread_t pThread;
  int status;
  
  assert(thread != NULL);
  pThread = (pthread_t)(thread->handle1);

  status = pthread_join(pThread, NULL);
  if (status != 0) {
    MSG2("thread-join-single: pthread_join %p returned error %d\n",
	 (void *) pThread, status);
    return GENERAL_ERROR;
  }
  
  thread->handle1 = NULL;
  return OK;
}


/* 4 */
THREADS_RUN_TIME_API  Z
primitive_thread_join_multiple(SOV *thread_vector)
{ 
  // @@@@#!"£$ NOT PROPERLY IMPLEMENTED
  // Just join on the first thread
  DTHREAD ** threads = (DTHREAD **)(thread_vector->data);
  DTHREAD *thread1 = *threads;
  ZINT sres = primitive_thread_join_single(thread1);
  assert(sres == OK);
  return (Z)thread1;
}


/* 5 */
THREADS_RUN_TIME_API  void
primitive_thread_yield(void)
{
  // Causes thread to give up its remaining time slice
  primitive_sleep(I(0));
}


/* 6. */
THREADS_RUN_TIME_API  Z
primitive_current_thread(void)
{
  Z thread;

  thread = get_current_thread();
  assert(thread != NULL);
  return(thread);
}
 

/* 7 */
THREADS_RUN_TIME_API  ZINT
primitive_wait_for_simple_lock(CONTAINER *lock)
{
  SIMPLELOCK  *slock;
  pthread_t    hThread;
  int status;

  assert(lock != NULL);
  assert(lock->handle != NULL);
  slock = (SIMPLELOCK*)lock->handle;

  // Check that thread doesn't already own the lock
  // Allow for pre-initialization
  hThread = get_current_thread_handle();
  if (slock->owner == hThread && hThread != 0) {
    MSG0("wait-for-simple-lock: Error. Already own the lock\n");
    return ALREADY_LOCKED;
  }

  status = pthread_mutex_lock(&slock->mutex);
  if (status != 0) {
    MSG0("wait-for-simple-lock: Error returned by pthread_mutex_lock.\n");
    return GENERAL_ERROR;
  }

  slock->owner = hThread;

  return OK;
}


THREADS_RUN_TIME_API  ZINT
primitive_wait_for_recursive_lock(CONTAINER *lock)
{
  RECURSIVELOCK  *rlock;
  pthread_t       hThread;
  int status;

  assert(lock != NULL);
  assert(lock->handle != NULL);
  rlock = (RECURSIVELOCK*)lock->handle;

  // Check if we already own the lock
  hThread = get_current_thread_handle();
  if (rlock->owner == hThread) {
    assert(rlock->recursion_count > 0);
    rlock->recursion_count++;
    return OK;
  }

  status = pthread_mutex_lock(&rlock->mutex);
  if (status != 0) {
    MSG0("wait-for-recursive-lock: Error returned by pthread_mutex_lock.\n");
    return GENERAL_ERROR;
  }

  assert(rlock->recursion_count == 0);
  rlock->recursion_count = 1;
  rlock->owner = hThread;
  return OK;
}


/* 9 */
THREADS_RUN_TIME_API  ZINT
primitive_wait_for_semaphore(CONTAINER *lock)
{
  SEMAPHORE *semaphore = (SEMAPHORE*)lock->handle;

  assert(lock != NULL);
  assert(semaphore != NULL);

  while (sem_wait(&semaphore->sema) != 0) {
    if (errno != EINTR) {
      MSG0("wait-for-simple-lock: Error returned by sem_wait.\n");
      return GENERAL_ERROR;
    }
  }
  return OK;
}


/* 10 */
THREADS_RUN_TIME_API  ZINT
primitive_wait_for_notification(CONTAINER *notif, CONTAINER *lock)
{
  NOTIFICATION  *notification;
  SIMPLELOCK  *slock;
  pthread_t    hThread;

  assert(notif != NULL);
  assert(notif->handle != NULL);
  assert(lock != NULL);
  assert(lock->handle != NULL);

  notification = (NOTIFICATION*)notif->handle;
  slock = (SIMPLELOCK*)lock->handle;

  // make sure thread owns the simple lock
  hThread = get_current_thread_handle();
  if (slock->owner != hThread) {
    MSG0("wait-for-notification: Don't own associated lock\n");
    return NOT_LOCKED;
  }
  // We're about to lose the lock - so drop ownership
  slock->owner = 0;

  if (pthread_cond_wait(&notification->cond, &slock->mutex) != 0) {
    MSG0("wait-for-simple-lock: Error returned by pthread_mutex_lock.\n");
    return GENERAL_ERROR;
  }

  // We should now own the mutex. Register our ownership.
  assert(slock->owner == 0);
  slock->owner = hThread;
  return OK;
}


/* 11 */
THREADS_RUN_TIME_API ZINT
primitive_wait_for_simple_lock_timed(CONTAINER *lock, ZINT zmilsecs)
{
  int timeout = zmilsecs >> 2;
  int timeleft = timeout;
  int sleeptime = 100;
  int status;
  SIMPLELOCK  *slock;
  pthread_t    hThread;

  assert(lock != NULL);
  assert(lock->handle != NULL);
  assert(IS_ZINT(zmilsecs));
  slock = (SIMPLELOCK*)lock->handle;

  // Check that thread doesn't already own the lock
  hThread = get_current_thread_handle();
  if (slock->owner == hThread) {
    MSG0("wait-for-simple-lock: Error. Already own the lock\n");
    return ALREADY_LOCKED;
  }

  while ((status = pthread_mutex_trylock(&slock->mutex)) != 0) {
    switch (status) {
    case EBUSY:
      if (timeleft <= 0) {
	MSG1("wait-for-simple-lock-timed(%p): Timeout waiting for lock\n",
	     timeout);
	return TIMEOUT;
      } else {
	primitive_sleep(I(sleeptime));
	timeleft -= sleeptime;
	break;
      }
    default:
      MSG0("wait-for-simple-lock-timed: Error returned by pthread_mutex_trylock.\n");
      return GENERAL_ERROR;
    }
  }
  slock->owner = hThread;
  return OK;
}


/* 12 */

THREADS_RUN_TIME_API ZINT
primitive_wait_for_recursive_lock_timed(CONTAINER *lock, ZINT zmilsecs)
{
  int timeout = zmilsecs >> 2;
  int timeleft = timeout;
  int sleeptime = 100;
  int status;
  RECURSIVELOCK  *rlock;
  pthread_t       hThread;

  assert(lock != NULL);
  assert(lock->handle != NULL);
  assert(IS_ZINT(zmilsecs));
  rlock = (RECURSIVELOCK*)lock->handle;

  // Check if we already own the lock
  hThread = get_current_thread_handle();
  if (rlock->owner == hThread) {
    assert(rlock->recursion_count > 0);
    rlock->recursion_count++;
    return OK;
  }

  while ((status = pthread_mutex_trylock(&rlock->mutex)) != 0) {
    switch (status) {
    case EBUSY:
      if (timeleft <= 0) {
	MSG1("wait-for-recursive-lock-timed(%p): Timeout waiting for lock\n",
	     timeout);
	return TIMEOUT;
      } else {
	primitive_sleep(I(sleeptime));
	timeleft -= sleeptime;
	break;
      }
    default:
      MSG0("wait-for-recursive-lock-timed: Error returned by pthread_mutex_trylock.\n");
      return GENERAL_ERROR;
    }
  }
  assert(rlock->recursion_count == 0);
  rlock->recursion_count = 1;
  rlock->owner = hThread;
  return OK;
}


/* 13 */
THREADS_RUN_TIME_API  ZINT
primitive_wait_for_semaphore_timed(CONTAINER *lock, ZINT zmilsecs)
{
  int timeout = zmilsecs >> 2;
  int timeleft = timeout;
  int sleeptime = 100;
  SEMAPHORE  *semaphore;

  assert(lock != NULL);
  assert(lock->handle != NULL);
  assert(IS_ZINT(zmilsecs));

  semaphore = (SEMAPHORE*)lock->handle;

  while (sem_trywait(&semaphore->sema) != 0) {
    switch (errno) {
    case EAGAIN:
    case EINTR:
      if (timeleft <= 0) {
	MSG1("wait-for-semaphore-timed(%p): Timeout waiting for lock\n",
	     timeout);
	return TIMEOUT;
      } else {
	primitive_sleep(I(sleeptime));
	timeleft -= sleeptime;
	break;
      }
    default:
      MSG0("wait-for-simple-lock-timed: Error returned by pthread_mutex_trylock.\n");
      return GENERAL_ERROR;
    }
  }
  return OK;
}


/* 14 */
THREADS_RUN_TIME_API  ZINT
primitive_wait_for_notification_timed(CONTAINER *notif, CONTAINER *lock,
				      ZINT zmilsecs)
{
  NOTIFICATION  *notification;
  SIMPLELOCK  *slock;
  pthread_t hThread;
  struct timespec timespec;
  int status, milsecs, secs;

  assert(notif != NULL);
  assert(notif->handle != NULL);
  assert(lock != NULL);
  assert(lock->handle != NULL);
  assert(IS_ZINT(zmilsecs));

  notification = (NOTIFICATION*)notif->handle;
  slock = (SIMPLELOCK*)lock->handle;
  milsecs = zmilsecs >> 2;

  // Manage timeouts at pretty low precision
  secs = milsecs / 1000;
  if (secs == 0) secs++;
  timespec.tv_sec = secs + time(NULL);
  timespec.tv_nsec = 0;

  // make sure thread owns the simple lock
  hThread = get_current_thread_handle();
  if (slock->owner != hThread) {
    MSG0("wait-for-notification: Don't own associated lock\n");
    return NOT_LOCKED;
  }
  // We're about to lose the lock - so drop ownership
  slock->owner = 0;

  status = pthread_cond_timedwait(&notification->cond, &slock->mutex, &timespec);
  switch (status) {
  case 0 :
    // We should now own the mutex. Register our ownership.
    assert(slock->owner == 0);
    slock->owner = hThread;
    return OK;
  case ETIMEDOUT:
    // We should now own the mutex. Register our ownership.
    assert(slock->owner == 0);
    slock->owner = hThread;
    return TIMEOUT;
  default:
    MSG0("wait-for-notification: Error returned by pthread_cond_timedwait.\n");
    return GENERAL_ERROR;
  }
}



/* 15 */
THREADS_RUN_TIME_API  ZINT
primitive_release_simple_lock(CONTAINER *lock)
{
  SIMPLELOCK  *slock;
  pthread_t hThread;
  int status;

  assert(lock != NULL);
  assert(lock->handle != NULL);
  slock = (SIMPLELOCK*)lock->handle;
  hThread = get_current_thread_handle();

  if (slock->owner != hThread) {
    MSG0("release-simple-lock: Error, don't own the lock\n");
    return NOT_LOCKED;
  }

  slock->owner = 0;
  status = pthread_mutex_unlock(&slock->mutex);
  if (status != 0) {
    MSG0("release-simple-lock: Error returned by pthread_mutex_unlock.\n");
    return GENERAL_ERROR;
  }
  return OK;
}


/* 16 */
THREADS_RUN_TIME_API  ZINT
primitive_release_recursive_lock(CONTAINER *lock)
{
  RECURSIVELOCK  *rlock;
  pthread_t hThread;
  int status;

  assert(lock != NULL);
  assert(lock->handle != NULL);
  rlock = (RECURSIVELOCK*)lock->handle;
  hThread = get_current_thread_handle();

  if (rlock->owner != hThread) {
    MSG0("release-recursive-lock: Error, don't own the lock\n");
    return NOT_LOCKED;
  }

  if (--rlock->recursion_count == 0) {
    // Give up the lock
    rlock->owner = 0;
    status = pthread_mutex_unlock(&rlock->mutex);
    if (status != 0) {
      MSG0("release-recursive-lock: Error returned by pthread_mutex_unlock.\n");
      return GENERAL_ERROR;
    }
  }

  return OK;
}



/* 17 */
THREADS_RUN_TIME_API  ZINT
primitive_release_semaphore(CONTAINER *lock)
{
  SEMAPHORE  *semaphore;

  assert(lock != NULL);
  assert(lock->handle != NULL);

  semaphore = (SEMAPHORE*)lock->handle;
  if (sem_post(&semaphore->sema) != 0) {
    MSG0("release-semaphore: ReleaseSemphore error. Assume count exceeded\n");
    return COUNT_EXCEEDED;
  }
  return OK;
}


/* 18 */
THREADS_RUN_TIME_API  ZINT
primitive_release_notification(CONTAINER *notif, CONTAINER *lock)
{
  NOTIFICATION * notification;
  SIMPLELOCK   * slock;
  pthread_t hThread;

  assert(notif != NULL);
  assert(notif->handle != NULL);
  assert(lock != NULL);
  assert(lock->handle != NULL);

  notification = (NOTIFICATION*)notif->handle;
  slock = (SIMPLELOCK*)lock->handle;

  // make sure thread owns the simple lock
  hThread = get_current_thread_handle();
  if (slock->owner != hThread) {
    MSG0("release-notification: Don't own associated lock\n");
    return NOT_LOCKED;
  }

  if (pthread_cond_signal(&notification->cond) != 0) {
    MSG0("release-notification: error from pthread_cond_signal\n");
    return GENERAL_ERROR;
  }

  return OK;
}


/* 19 */
THREADS_RUN_TIME_API  ZINT
primitive_release_all_notification(CONTAINER *notif, CONTAINER *lock)
{
  NOTIFICATION * notification;
  SIMPLELOCK   * slock;
  pthread_t hThread;

  assert(notif != NULL);
  assert(notif->handle != NULL);
  assert(lock != NULL);
  assert(lock->handle != NULL);

  notification = (NOTIFICATION*)notif->handle;
  slock = (SIMPLELOCK*)lock->handle;

  // make sure thread owns the simple lock
  hThread = get_current_thread_handle();
  if (slock->owner != hThread) {
    MSG0("release-all-notification: Don't own associated lock\n");
    return NOT_LOCKED;
  }

  if (pthread_cond_broadcast(&notification->cond) != 0) {
    MSG0("release-all-notification: error from pthread_cond_broadcast");
    return GENERAL_ERROR;
  }

  return OK;
}


/* 20 */   
THREADS_RUN_TIME_API ZINT 
primitive_make_recursive_lock(CONTAINER *lock, D_NAME name)
{
  RECURSIVELOCK *rlock;
  pthread_mutexattr_t attr;
  int res;

  assert(lock != NULL);

  rlock = MMAllocMisc(sizeof(RECURSIVELOCK));
  if (rlock == NULL) {
    MSG0("make-recursive-lock: malloc failed\n");
    return GENERAL_ERROR;
  }

  rlock->owner = 0;
  rlock->recursion_count = 0;

  res = pthread_mutexattr_init(&attr);
  if(res != 0) return GENERAL_ERROR;
  res = pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_ERRORCHECK);
  if(res != 0) return GENERAL_ERROR;
  res = pthread_mutex_init(&rlock->mutex, &attr);
  if(res != 0) return GENERAL_ERROR;
  res = pthread_mutexattr_destroy(&attr);
  if(res != 0) return GENERAL_ERROR;

  lock->handle = rlock;
  return OK;
}


/* 21 */
THREADS_RUN_TIME_API  ZINT
primitive_destroy_recursive_lock(CONTAINER *lock)
{
  RECURSIVELOCK *rlock;

  assert(lock != NULL);
  assert(lock->handle != NULL);

  rlock = lock->handle;
  if (pthread_mutex_destroy(&rlock->mutex) != 0) {
    MSG0("destroy-recursive-lock: pthread_mutex_destroy returned error\n");
    return GENERAL_ERROR;
  }
  MMFreeMisc(rlock, sizeof(RECURSIVELOCK));
  return OK;
}



/* 22 */
THREADS_RUN_TIME_API ZINT 
primitive_make_simple_lock(CONTAINER *lock, D_NAME name)
{
  SIMPLELOCK *slock;
  pthread_mutexattr_t attr;
  int res;

  assert(lock != NULL);

  slock = MMAllocMisc(sizeof(SIMPLELOCK));
  if (slock == NULL) {
    MSG0("make-simple-lock: malloc failed\n");
    return GENERAL_ERROR;
  }

  slock->owner = 0;

  res = pthread_mutexattr_init(&attr);
  if(res != 0) return GENERAL_ERROR;
  res = pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_ERRORCHECK);
  if(res != 0) return GENERAL_ERROR;
  res = pthread_mutex_init(&slock->mutex, &attr);
  if(res != 0) return GENERAL_ERROR;
  res = pthread_mutexattr_destroy(&attr);
  if(res != 0) return GENERAL_ERROR;

  lock->handle = slock;
  return OK;
}

   
/* 23 */
THREADS_RUN_TIME_API  ZINT
primitive_destroy_simple_lock(CONTAINER *lock)
{
  SIMPLELOCK *slock;

  assert(lock != NULL);
  assert(lock->handle != NULL);

  slock = (SIMPLELOCK*)lock->handle;
  if (pthread_mutex_destroy(&slock->mutex) != 0) {
    MSG0("destroy-simple-lock: pthread_mutex_destroy returned error\n");
    return GENERAL_ERROR;
  }
  MMFreeMisc(slock, sizeof(SIMPLELOCK));
  return OK;
}


/* 24 */
THREADS_RUN_TIME_API  ZINT
primitive_owned_simple_lock(CONTAINER *lock)
{
  pthread_t    hThread;
  SIMPLELOCK  *slock;

  assert(lock != NULL);
  assert(lock->handle != NULL);

  slock = lock->handle;

  hThread = get_current_thread_handle();
  if (slock->owner == hThread)
    return((ZINT)I(1)); // owned
  else
    return((ZINT)I(0)); // not owned
}


/* 25 */
THREADS_RUN_TIME_API  ZINT
primitive_owned_recursive_lock(CONTAINER *lock)
{
  pthread_t       hThread;
  RECURSIVELOCK  *rlock;

  assert(lock != NULL);
  assert(lock->handle != NULL);

  rlock = lock->handle;

  hThread = get_current_thread_handle();
  if (rlock->owner == hThread)
    return((ZINT)I(1)); // owned
  else
    return((ZINT)I(0)); // not owned
}

  
/* 26 */
THREADS_RUN_TIME_API  ZINT
primitive_make_semaphore(CONTAINER *lock, D_NAME name,
                    ZINT zinitial, ZINT zmax)
{
  SEMAPHORE  *semaphore;
  int   initial = zinitial >> 2;
  int   max   = zmax >> 2;

  ignore(max);

  assert(lock != NULL);
  assert(IS_ZINT(zinitial));
  assert(IS_ZINT(zmax));

  semaphore = MMAllocMisc(sizeof(SEMAPHORE));
  if (semaphore == NULL) 
    goto generalError;

  if(sem_init(&semaphore->sema, 0, initial) == -1)
    goto generalError;

  lock->handle = semaphore;
  return OK;

generalError:
  MSG0("make-semaphore: failed\n");
  return GENERAL_ERROR;
}


/* 27 */
THREADS_RUN_TIME_API  ZINT
primitive_destroy_semaphore(CONTAINER *lock)
{
  SEMAPHORE *semaphore;

  assert(lock != NULL);
  assert(lock->handle != NULL);

  semaphore = (SEMAPHORE*)lock->handle;

  if (sem_destroy(&semaphore->sema) == -1) {
    MSG0("destroy-semaphore: sem_destroy returned error\n");
    return GENERAL_ERROR;
  }

  MMFreeMisc(semaphore, sizeof(SEMAPHORE));
  return OK;
}


/* 28 */
THREADS_RUN_TIME_API  ZINT
primitive_make_notification(CONTAINER *notif, D_NAME name)
{
  NOTIFICATION *notification;
  int res;

  assert(notif != NULL);

  notification = MMAllocMisc(sizeof(NOTIFICATION));
  if (notification == NULL) {
    MSG0("make-notification: malloc failed\n");
    return GENERAL_ERROR;
  }

  res = pthread_cond_init(&notification->cond, NULL);
  if(res != 0) return GENERAL_ERROR;

  notif->handle = notification;
  return OK;
}


/* 29 */
THREADS_RUN_TIME_API  ZINT
primitive_destroy_notification(CONTAINER *notif)
{
  NOTIFICATION *notification;

  assert(notif != NULL);
  assert(notif->handle != NULL);

  notification = (NOTIFICATION*)notif->handle;
  if (pthread_cond_destroy(&notification->cond) != 0) {
    MSG0("destroy-notification: pthread_cond_destroy returned error\n");
    return GENERAL_ERROR;
  }
  MMFreeMisc(notification, sizeof(NOTIFICATION));
  return OK;
}


/* 30 */
THREADS_RUN_TIME_API  void
primitive_sleep(ZINT zmilsecs)
{
  long milsecs = zmilsecs >> 2;
  struct timespec req, rem;

  assert(IS_ZINT(zmilsecs));
  req.tv_sec = milsecs / 1000;
  req.tv_nsec = (milsecs % 1000) * 1000000;
  while(nanosleep(&req, &rem)) {
    if (errno == EINTR) {
      req = rem;
    } else {
      MSG0("sleep: error in nanosleep\n");
      return;
    }
  }   
}


/* 31 */
/*
Z
primitive_assign_atomic_memory(void * * location, Z newval)
{
}
*/

/* 32 */
/*
ZINT
primitive_conditional_update_memory(void * * location, Z newval, Z oldval)
{
}
*/


/* 33 */
THREADS_RUN_TIME_API  void *
primitive_allocate_thread_variable(Z value)
{
  int     variable_offset, size, limit;
  Z      *destination;

  pthread_mutex_lock(&tlv_vector_list_lock);

  // Get offset into to TLV vector for the new variable
  variable_offset = TLV_vector_offset;

  // increment offset for the next new variable
  TLV_vector_offset += sizeof(Z);

  // First check if we need to grow the TLV vectors
  size = (int)(*((Z *)(default_tlv_vector + sizeof(Z)))) >> 2;
  limit = (size+2) * sizeof(Z);
  if (variable_offset >= limit)
    grow_all_tlv_vectors(size+size);  // double the size each time we grow

  // Put the variable's default value in the default TLV vector
  destination = (Z *)(default_tlv_vector + variable_offset);
  *destination = value;

  // Update all the active thread TLV vectors with the default value
  update_tlv_vectors(variable_offset, value);

  // Finished
  pthread_mutex_unlock(&tlv_vector_list_lock);

  // return the offset into the TLV vector (an integer, not a pointer)
  return((void *)variable_offset);
}


/* Grow all TLV vectors
 */
void grow_all_tlv_vectors(int newsize)
{
  TLV_VECTOR_LIST list;
  TLV_VECTOR new_default;

  // Wait for thread variable writes to finish
  while(internal_InterlockedCompareExchange(&tlv_writer_counter, TLV_GROW, 0)
	!= 0);

  // Grow the default vector
  new_default = make_dylan_vector(newsize);
  copy_tlv_vector(new_default, default_tlv_vector);
  default_tlv_vector = new_default;

  // Grow each vector in the active thread list
  list = tlv_vector_list;
  while(list != NULL) {
    list->tlv_vector = grow_tlv_vector(list->tlv_vector, newsize);
    list = list->next;
  }

  // Let writes proceed again
  while(internal_InterlockedCompareExchange(&tlv_writer_counter, 0, TLV_GROW)
	!= TLV_GROW);
}


/* Grow a single TLV vector
 */
TLV_VECTOR grow_tlv_vector(TLV_VECTOR vector, int newsize)
{
  BYTE       *teb;
  TLV_VECTOR  new_vector;

  // allocate a new vector and copy the values in the old across
  new_vector = make_dylan_vector(newsize);
  copy_tlv_vector(new_vector, vector);

#ifndef C_TESTING
  // put the new TLV vector in the TEB
  teb = (BYTE *)(*((Z *)(vector + 2*sizeof(Z))));
  *((void **)(teb + 4)) = new_vector;
#endif

  // return the new vector
  return(new_vector);
}


/* Copy a tlv vector. Assumes the destination vector is at least as large
 * as the source vector.
*/
void copy_tlv_vector(TLV_VECTOR destination, TLV_VECTOR source)
{
  Z   *p_source, *p_destination;
  int  i, limit;

  // limit = number of bytes in the source vector
  limit = ((int)(*((Z *)(source + sizeof(Z)))) >> 2) + 2;
  limit *= sizeof(Z);

  for (i = 2*sizeof(Z); i<limit; i += sizeof(Z)) {
    p_destination = (Z *)(destination + i);
    p_source = (Z *)(source + i);
    *p_destination = *p_source;
  }
}


/* Add a new variable to all the TLV vectors in the active thread list.
 * Assumes the vectors do not need to be grown. Also, the calling function
 * must be in the tlv_vector_list_lock Critical Section.
 */
void
update_tlv_vectors(int offset, Z value)
{
  TLV_VECTOR_LIST list = tlv_vector_list;
  Z *destination;

  while (list != NULL) {
    destination = (Z *)(list->tlv_vector + offset);
    *destination = value;
    list = list->next;
  }
}


/* 34 */
THREADS_RUN_TIME_API  Z
primitive_read_thread_variable(void *variable_handle)
{
  TLV_VECTOR tlv_vector;
  Z         *source;
  Z          result;
  int        offset;

  // The variable handle is the byte offset where the variable's value is
  // stored in the TLV.
  offset = (int)variable_handle;
  tlv_vector = get_tlv_vector();
  source = (Z *)(tlv_vector + offset);
  result = *source;
  return(result);
}


/* 35 */
THREADS_RUN_TIME_API  Z
primitive_write_thread_variable(void *variable_handle, Z new_value)
{
  TLV_VECTOR tlv_vector;
  Z         *destination;
  int        offset;

  // If another thread is growing the TLV vectors, wait till it's finished
  if (internal_InterlockedIncrement(&tlv_writer_counter) < 0)
    primitive_write_thread_variable_internal();

  // The variable handle is the byte offset where the variable's value is
  // stored in the TLV.
  offset = (int)variable_handle;
  tlv_vector = get_tlv_vector();
  destination = (Z *)(tlv_vector + offset);
  *destination = new_value;

  // Indicate that the write has finished
  internal_InterlockedDecrement(&tlv_writer_counter);

  return(new_value);
}

/* 36 */
THREADS_RUN_TIME_API  void
primitive_initialize_current_thread(DTHREAD *thread, BOOL synchronize)
{
  pthread_t   hThread;
  TLV_VECTOR  tlv_vector;
  Z          *destination;
  int         size;

  /* @@@@#!"£$ no support for "synchronized" threads */
  assert(thread != NULL);

  // race conditions mean handle may not be set up yet by father thread in pthread_create,
  // so do it here explicitly.
  hThread = pthread_self();
  thread->handle1 = (HANDLE)hThread;

  // Put the thread object and handle in the TEB for later use
  set_current_thread(thread);
  set_current_thread_handle(hThread);

  pthread_mutex_lock(&tlv_vector_list_lock);

  // Now set up a vector for the Dylan thread variables
  size = (int)(*((Z *)(default_tlv_vector + sizeof(Z)))) >> 2;
  tlv_vector = make_dylan_vector(size);
  set_tlv_vector(tlv_vector);

  // Initialise the vector with the values from the default vector
  copy_tlv_vector(tlv_vector, default_tlv_vector);

#ifndef C_TESTING
  // Put the TEB in the first slot of the vector
  destination = (Z *)(tlv_vector + 2*sizeof(Z));
  *destination = get_current_teb();
#endif

  // Add thread to active thread list
  add_tlv_vector(hThread, tlv_vector);

  pthread_mutex_unlock(&tlv_vector_list_lock);

  // Clear the handle2 slot in the thread object
  // (which contained the address of the starting function)
  thread->handle2 = dylan_false;
}


/* 36a */
THREADS_RUN_TIME_API  void
primitive_initialize_special_thread(DTHREAD *thread)
{
  assert(thread != NULL);

  // Do we need to initialise?
  if (default_tlv_vector == NULL)
    initialize_threads_primitives();

  primitive_initialize_current_thread(thread, FALSE);
}

/* 36b */
THREADS_RUN_TIME_API  void
primitive_detach_thread(DTHREAD *thread)
{
  // do nothing
  assert(thread != NULL);
}


/* 37 */
THREADS_RUN_TIME_API  ZINT
primitive_unlock_simple_lock(CONTAINER *lock)
{
  SIMPLELOCK *slock;

  assert(lock != NULL);
  assert(lock->handle != NULL);

  slock = lock->handle;
  if (slock->owner == 0) {
    /* nothing to do - lock already released */
    return OK;
  }
  return primitive_release_simple_lock(lock);
}


/* 38 */
THREADS_RUN_TIME_API  ZINT
primitive_unlock_recursive_lock(CONTAINER *lock)
{
  RECURSIVELOCK *rlock;
  ZINT res;

  assert(lock != NULL);
  assert(lock->handle != NULL);

  rlock = lock->handle;
  if (rlock->owner == 0) {
    // nothing to do - lock already released
    assert(rlock->recursion_count == 0);
    return OK;
  }

  while(rlock->recursion_count > 0) {
    res = primitive_release_recursive_lock(lock);
    if (res != OK)
      return res;
  }
  return OK;
}


/* This function is called to initialise the primitives
 */
void initialize_threads_primitives()
{
  default_tlv_vector = make_dylan_vector(TLV_VECTOR_INITIAL_SIZE);
  assert(default_tlv_vector != NULL);
  initialize_CRITICAL_SECTION(&tlv_vector_list_lock);
  tlv_vector_list  = NULL;

#ifdef C_TESTING
  TlsIndexThread = TlsAlloc();
  TlsIndexThreadHandle = TlsAlloc();
  TlsIndexThreadVector = TlsAlloc();
#endif
}


/* add_tlv_vector adds a new thread to the active thread vector list.
 * Assumes the thread vector has already been initialised.
 */
void
add_tlv_vector(pthread_t hThread, TLV_VECTOR tlv_vector)
{
  TLV_VECTOR_LIST new_element = MMAllocMisc(linksize);

  assert(new_element != NULL);

  // protect list updates so they don't interfere with each other
  pthread_mutex_lock(&tlv_vector_list_lock);

  // initialise the new element and put it on the front of the list
  new_element->hThread = hThread;
  new_element->tlv_vector = tlv_vector; 
  new_element->next = tlv_vector_list;
  tlv_vector_list = new_element;

  pthread_mutex_unlock(&tlv_vector_list_lock);
}


/* A thread calls remove_tlv_vector just before it terminates. The function
 * removes the thread from the list of active threads.
 */
int
remove_tlv_vector(pthread_t hThread)
{
  TLV_VECTOR_LIST last, current;

  if (tlv_vector_list == NULL)  // empty list
    return(1);

  // protect list updates so they don't interfere with each other
  pthread_mutex_lock(&tlv_vector_list_lock);

  last = tlv_vector_list;
  if (tlv_vector_list->hThread == hThread) {
    // matches first entry in list
    tlv_vector_list = tlv_vector_list->next;
#ifdef C_TESTING
    MMFreeMisc(last->tlv_vector, linksize);
    MMFreeMisc(last, linksize);
#endif

    pthread_mutex_unlock(&tlv_vector_list_lock);
    return(0);
  }

  current = tlv_vector_list->next;
  while (current != NULL) {
    if (current->hThread == hThread) {
      // found the right entry, so cut it out
      last->next = current->next;
#ifdef C_TESTING
      MMFreeMisc(current->tlv_vector, linksize);
      MMFreeMisc(current, linksize);
#endif
      // Finished
      pthread_mutex_unlock(&tlv_vector_list_lock);
      return(0);
    }
    else {
      last = current;
      current = current->next;
    }
  }

  // Reached the end of the list without finding thread's entry
  pthread_mutex_unlock(&tlv_vector_list_lock);
  return(1);
}
