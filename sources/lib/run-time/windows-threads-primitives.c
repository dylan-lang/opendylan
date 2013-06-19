/*
 * File:    windows-threads-primitives.c
 * Author:  Keith Dennison
 * Copyright: 1996 Functional Objects, Inc. All rights reserved.
 *
 * A description of the implementation of the primitives in this file can be
 * found in D-doc-design-runtime!win32-thread-portability.text
 */

/*
#define THREAD_AWARE_C_LIBS
*/

#include <assert.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <stddef.h>
#include <errno.h>
#include <time.h>
#include <process.h>
#include <sys/types.h>
#include <windows.h>

#include "windows-threads-primitives.h"


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

int        TLV_vector_offset = 3*sizeof(Z);

__declspec( dllexport )
PVOID      tlv_writer_counter = 0;

CRITICAL_SECTION tlv_vector_list_lock;
TLV_VECTOR_LIST  tlv_vector_list;

size_t linksize = sizeof(struct tlv_vector_list_element);


/*****************************************************************************/
/* LOCAL FUNCTION DECLARATIONS                                               */
/*****************************************************************************/

static void  initialize_threads_primitives(void);

static void       grow_all_tlv_vectors(int newsize);
static TLV_VECTOR grow_tlv_vector(TLV_VECTOR vector, int newsize);

static void  copy_tlv_vector(TLV_VECTOR destination, TLV_VECTOR source);
static void  update_tlv_vectors(int newindex, Z value);
static void  add_tlv_vector(HANDLE newthread, TLV_VECTOR tlv_vector);
static int   remove_tlv_vector(HANDLE thread);

static int   priority_map(int);

static LONG  internal_InterlockedIncrement(LPLONG);
static LONG  internal_InterlockedDecrement(LPLONG);
static PVOID internal_InterlockedCompareExchange(PVOID *, PVOID, PVOID);


/*****************************************************************************/
/* EXTERNAL FUNCTIONS                                                        */
/*****************************************************************************/

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
extern void *get_current_thread_handle();
extern void  set_current_thread_handle(void *handle);
extern void *get_current_teb();
extern int   dylan_init_thread(void **rReturn, void *(*f)(void *, size_t),
                               void *p, size_t s);
extern DWORD WINAPI  dylan_thread_trampoline(void **thread);

extern void *MMAllocMisc(size_t size);
extern void MMFreeMisc(void *old, size_t size);


#endif


/*****************************************************************************/
/* THREAD PRIMITIVES INTERNALS                                               */
/*                                                                           */
/* These functions are called both by the inlined versions of the primitives */
/* and the non-inlined versions which follow below.                          */
/*****************************************************************************/

THREADS_RUN_TIME_API
ZINT primitive_wait_for_simple_lock_internal(SIMPLELOCK *slock, HANDLE hThread)
{
  // Check that thread doesn't already own the lock
  if (slock->owner == hThread) {
    slock->lock_count--;
    MSG0("wait-for-simple-lock: Error. Already own the lock\n");
    return ALREADY_LOCKED;
  }

  // Someone else already has the lock, so wait till they release it
  if (WaitForSingleObject(slock->semaphore, INFINITE) != WAIT_OBJECT_0) {
    MSG0("wait-for-simple-lock: Error returned by WaitForSingleObject.\n");
    return GENERAL_ERROR;
  }

  slock->owner = hThread;
  return OK;
}


THREADS_RUN_TIME_API
ZINT primitive_wait_for_simple_lock_timed_internal(SIMPLELOCK *slock,
                                                   HANDLE hThread,
                                                   ZINT ztimeout)
{
  int  timeout = ztimeout >> 2;

  if (slock->owner == hThread) {
    slock->lock_count--;
    MSG0("wait-for-simple-lock-timed: Error already own the lock\n");
    return ALREADY_LOCKED;
  }

  switch (WaitForSingleObject(slock->semaphore, timeout)) {
  case WAIT_OBJECT_0:
    slock->owner = hThread;
    break;

  case WAIT_TIMEOUT:
    MSG2("wait-for-simple-lock-timed(%p, %p): Timeout waiting for lock\n",
         hThread, timeout);
    return TIMEOUT;

  default:
    MSG2("wait-for-simple-lock-timed(%p, %p): WaitForSingleObject error\n",
         hThread, timeout);
    return GENERAL_ERROR;
  }
  return OK;
}


THREADS_RUN_TIME_API
ZINT primitive_release_simple_lock_internal(SIMPLELOCK *slock)
{
  LONG junk;

  if (ReleaseSemaphore(slock->semaphore, 1, &junk) == FALSE) {
    MSG0("release-simple-lock: error releasing semaphore\n");
    return GENERAL_ERROR;
  }
  return OK;
}


THREADS_RUN_TIME_API
ZINT primitive_wait_for_recursive_lock_internal(RECURSIVELOCK *rlock,
                                                HANDLE hThread)
{
  if (WaitForSingleObject(rlock->semaphore, INFINITE) != WAIT_OBJECT_0) {
    MSG0("wait-for-simple-lock: Error returned by WaitForSingleObject.\n");
    return GENERAL_ERROR;
  }
  rlock->owner = hThread;
  rlock->recursion_count = 1;
  return OK;
}


THREADS_RUN_TIME_API
ZINT primitive_wait_for_recursive_lock_timed_internal(RECURSIVELOCK *rlock,
                                                      HANDLE hThread,
                                                      ZINT ztimeout)
{
  int timeout = ztimeout >> 2;

  switch (WaitForSingleObject(rlock->semaphore, timeout)) {
  case WAIT_OBJECT_0:
    rlock->owner = hThread;
    rlock->recursion_count = 1;
    break;

  case WAIT_TIMEOUT:
    MSG0("wait-for-recursive-lock-timed: Timeout waiting for lock\n");
    return TIMEOUT;

  case WAIT_FAILED:
    MSG0("wait-for-recursive-lock-timed: WaitForSingleObject error\n");
    return GENERAL_ERROR;
  }
  return OK;
}


THREADS_RUN_TIME_API
ZINT primitive_release_recursive_lock_internal(RECURSIVELOCK *rlock)
{
  LONG junk;

  if (ReleaseSemaphore(rlock->semaphore, 1, &junk) == FALSE) {
    MSG0("release-recursive-lock: error releasing semaphore\n");
    return GENERAL_ERROR;
  }
  return OK;
}


THREADS_RUN_TIME_API
void primitive_write_thread_variable_internal()
{
  do {
    if (internal_InterlockedDecrement((LPLONG)(&tlv_writer_counter)) < 0) {
      EnterCriticalSection(&tlv_vector_list_lock);
      LeaveCriticalSection(&tlv_vector_list_lock);
    }
  } while (internal_InterlockedIncrement((LPLONG)(&tlv_writer_counter)) < 0);
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
  HANDLE hThread;
  HANDLE  *  events;
  DWORD  idThread;
  int    priority = (int)zpriority >> 2;
  DTHREAD **newthread_ptr;

  newthread_ptr = (DTHREAD **)(dylan__malloc__ambig(4));
  newthread_ptr[0] = newthread;

  assert(newthread != NULL);
  assert(IS_ZINT(zpriority));
  assert(func != NULL);


  if (synchronize) {
    // Events are used to signal when the thread has completed initialisation
    // and when the go-ahead is received from the debugger to conclude initialisation

    events = (HANDLE *)MMAllocMisc(sizeof(HANDLE) * 2);
    events[0] = CreateEvent(NULL, FALSE, FALSE, NULL);
    events[1] = CreateEvent(NULL, FALSE, FALSE, NULL);
    newthread->handle1 = events;
    newthread->handle2 = func;
  }


  // dylan_thread_trampoline is the starting function for the new thread.
  // It calls the dylan trampoline function which we rely on to initialise
  // the thread
  {
    DWORD creationFlag;

    if (synchronize) creationFlag = 0;
    else creationFlag = CREATE_SUSPENDED;

#ifdef THREAD_AWARE_C_LIBS
  hThread = (HANDLE)_beginthreadex(NULL, 0, dylan_thread_trampoline, newthread_ptr,
                                   creationFlag, &idThread);
#else
  hThread = CreateThread(NULL, 0, dylan_thread_trampoline, (LPVOID)newthread_ptr,
                         creationFlag, &idThread);
#endif

  }

  if (hThread == NULL) {
    MSG0("make-thread: CreateThread returned error\n");
    return CREATE_ERROR;
  }

  if (synchronize) {
    // Now wait for the new thread to complete initialisation
    if (WaitForSingleObject(events[0], INFINITE) != WAIT_OBJECT_0) {
      MSG0("make-thread: error waiting for thread initialize event\n");
      return GENERAL_ERROR;
    }

    // Don't need the event any more
    if (CloseHandle(events[0]) == FALSE) {
      MSG0("make-thread: error closing event handle\n");
      return GENERAL_ERROR;
    }
  } else {
   newthread->handle1 = hThread;
   newthread->handle2 = func;
  }

  // Map priority level to win32 equivalent and set the thread's priority
  priority = priority_map(priority);
  if (SetThreadPriority(hThread, priority) == FALSE) {
    MSG0("make-thread: SetThreadPriority returned error\n");
    return PRIORITY_ERROR;
  }

  if (synchronize) {
    // Don't need the thread's handle any more - it has its own handle
    if (CloseHandle(hThread) == FALSE) {
      MSG0("make-thread: error closing thread handle\n");
      return GENERAL_ERROR;
    }
  } else {
    // Now resume the new thread
    if (ResumeThread(hThread) == 0xFFFFFFFF) {
      MSG0("make-thread: ResumeThread returned error\n");
      return GENERAL_ERROR;
    }
  }

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

  remove_tlv_vector(thread->handle1);
  return 0;
}


/* 2 */
THREADS_RUN_TIME_API  ZINT
primitive_destroy_thread(DTHREAD *thread)
{
  assert(thread != NULL);

  if (CloseHandle((HANDLE)(thread->handle1)) == FALSE) {
    MSG0("primitive_destroy_thread: CloseHandle returned error\n");
    return GENERAL_ERROR;
  }
  return OK;
}


/* 3 */
THREADS_RUN_TIME_API  ZINT
primitive_thread_join_single(DTHREAD *thread)
{
  HANDLE hThread;

  assert(thread != NULL);

  hThread = (HANDLE)(thread->handle1);
  if (WaitForSingleObject(hThread, INFINITE) != WAIT_OBJECT_0) {
    MSG0("thread-join-single: WaitForSingleObject returned error\n");
    return GENERAL_ERROR;
  }
  return OK;
}


/* 4 */
THREADS_RUN_TIME_API  Z
primitive_thread_join_multiple(SOV *thread_vector)
{
  HANDLE  *  handles;
  DTHREAD ** threads;
  int    i, result, size;

  assert(thread_vector != NULL);
  assert(IS_ZINT(thread_vector->size));

  size  = ((int)(thread_vector->size)) >> 2;
  threads = (DTHREAD **)(thread_vector->data);
  handles = (HANDLE *)MMAllocMisc(sizeof(HANDLE) * size);

  if (handles == NULL) {
    MSG0("thread-join-multiple: malloc failed\n");
    return (Z)GENERAL_ERROR;
  }
  for (i = 0; i < size; i++)
    handles[i] = (HANDLE)(threads[i]->handle1);

  result = WaitForMultipleObjects(size,       // number of threads */
                                  handles,    // their OS handles */
                                  FALSE,      // wait for one thread to finish
                                  INFINITE);  // no timeout
  MMFreeMisc(handles, sizeof(HANDLE) * size);
  result -= WAIT_OBJECT_0;
  if (result < 0 || result >= size) {
    MSG0("thread-join-multiple: WaitForSingleObject returned error\n");
    return (Z)GENERAL_ERROR;
  }
  return(thread_vector->data[result]);  // thread object which finished
}


/* 5 */
THREADS_RUN_TIME_API  void
primitive_thread_yield(void)
{
  // Causes thread to give up its remaining time slice
  Sleep(0);
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
  HANDLE       hThread;

  assert(lock != NULL);
  assert(lock->handle != NULL);

  slock = lock->handle;
  hThread = get_current_thread_handle();

  if (internal_InterlockedIncrement(&slock->lock_count) == 0) {
    slock->owner = hThread;
    return OK;
  }
  return primitive_wait_for_simple_lock_internal(slock, hThread);
}


/* 8 */
THREADS_RUN_TIME_API  ZINT
primitive_wait_for_recursive_lock(CONTAINER *lock)
{
  HANDLE      hThread;
  RECURSIVELOCK  *rlock;

  assert(lock != NULL);
  assert(lock->handle != NULL);

  rlock = lock->handle;
  hThread = get_current_thread_handle();

  if (rlock->owner == hThread) {
    rlock->recursion_count++;
  } else if (internal_InterlockedIncrement(&rlock->lock_count) == 0) {
    rlock->owner = hThread;
    rlock->recursion_count = 1;
  } else {
    return primitive_wait_for_recursive_lock_internal(rlock, hThread);
  }
  return OK;
}


/* 9 */
THREADS_RUN_TIME_API  ZINT
primitive_wait_for_semaphore(CONTAINER *lock)
{
  HANDLE hSemaphore;

  assert(lock != NULL);
  assert(lock->handle != NULL);

  hSemaphore = lock->handle;
  if (WaitForSingleObject(hSemaphore, INFINITE) != WAIT_OBJECT_0) {
    MSG0("wait-for-semaphore: WaitForSingleObject returned error\n");
    return GENERAL_ERROR;
  }
  return OK;
}


/* 10 */
THREADS_RUN_TIME_API  ZINT
primitive_wait_for_notification(CONTAINER *notif, CONTAINER *lock)
{
  NOTIFICATION  *notification;
  SIMPLELOCK  *slock;
  int      owned, tmp1, tmp2;

  assert(notif != NULL);
  assert(notif->handle != NULL);
  assert(lock != NULL);
  assert(lock->handle != NULL);

  notification = notif->handle;
  slock = lock->handle;

  // make sure thread owns the simple lock
  owned = (int)primitive_owned_simple_lock(lock) >> 2;
  if (owned == 0) {
    MSG0("wait-for-notification: Don't own associated lock\n");
    return NOT_LOCKED;
  }

  if (WaitForSingleObject(notification->anti_notifier, INFINITE)
       != WAIT_OBJECT_0) {
    MSG0("wait-for-notification: error waiting for anti-notifier\n");
    return GENERAL_ERROR;
  }
  internal_InterlockedIncrement(&notification->count);
  if (primitive_release_simple_lock(lock) != OK) {
    MSG0("wait-for-notification: error releasing lock\n");
    return GENERAL_ERROR;
  }
  do {
    if (WaitForSingleObject(notification->notifier, INFINITE)
        != WAIT_OBJECT_0) {
      MSG0("wait-for-notification: error waiting for notifier\n");
      return GENERAL_ERROR;
    }
    tmp1 = (int)internal_InterlockedCompareExchange
                  ((PVOID *)(&notification->target), (PVOID)1, (PVOID)0);
  } while (tmp1 == 1);
  tmp2 = internal_InterlockedDecrement(&notification->count);
  if ((tmp1 != -1) || (tmp2 <= 0)) {
    // know it's not a release-all with more threads to be woken up
    if (ResetEvent(notification->notifier) == FALSE ||
      SetEvent(notification->anti_notifier) == FALSE) {
      MSG0("wait-for-notification: error (re)setting (anti)notifier\n");
      return GENERAL_ERROR;
    }
  }
  if (primitive_wait_for_simple_lock(lock) != OK) {
    MSG0("wait-for-notification: error while reclaiming lock\n");
    return GENERAL_ERROR;
  }
  return OK;
}


/* 11 */
THREADS_RUN_TIME_API ZINT
primitive_wait_for_simple_lock_timed(CONTAINER *lock, ZINT zmilsecs)
{
  HANDLE       hThread;
  DWORD        milsecs;
  SIMPLELOCK  *slock;

  assert(lock != NULL);
  assert(lock->handle != NULL);
  assert(IS_ZINT(zmilsecs));

  hThread = get_current_thread_handle();
  milsecs = zmilsecs >> 2;
  slock = lock->handle;

  if (internal_InterlockedIncrement(&slock->lock_count) == 0) {
    slock->owner = hThread;
    return OK;
  }
  return primitive_wait_for_simple_lock_timed_internal(slock, hThread,
                                                       zmilsecs);
}


/* 12 */
THREADS_RUN_TIME_API  ZINT
primitive_wait_for_recursive_lock_timed(CONTAINER *lock, ZINT zmilsecs)
{
  HANDLE      hThread;
  DWORD       milsecs;
  RECURSIVELOCK  *rlock;

  assert(lock != NULL);
  assert(lock->handle != NULL);
  assert(IS_ZINT(zmilsecs));

  milsecs = zmilsecs >> 2;
  rlock = lock->handle;
  hThread = get_current_thread_handle();

  if (rlock->owner == hThread) {
    rlock->recursion_count++;
  } else if (internal_InterlockedIncrement(&rlock->lock_count) == 0) {
    rlock->owner = hThread;
    rlock->recursion_count = 1;
  } else {
    return primitive_wait_for_recursive_lock_timed_internal(rlock, hThread,
                                                            zmilsecs);
  }
  return OK;
}


/* 13 */
THREADS_RUN_TIME_API  ZINT
primitive_wait_for_semaphore_timed(CONTAINER *lock, ZINT zmilsecs)
{
  HANDLE  hSemaphore;
  DWORD   milsecs;

  assert(lock != NULL);
  assert(lock->handle != NULL);
        assert(IS_ZINT(zmilsecs));

  hSemaphore = lock->handle;
  milsecs = zmilsecs >> 2;
  switch (WaitForSingleObject(hSemaphore, milsecs)) {
  case WAIT_OBJECT_0:
    break;

  case WAIT_TIMEOUT:
    MSG0("wait-for-semaphore-timed: Timeout waiting for semaphore\n");
    return TIMEOUT;

  default:
    MSG0("wait-for-semaphore-timed: WaitForSingleObject returned error\n");
    return GENERAL_ERROR;
  }
  return OK;
}


/* 14 */
THREADS_RUN_TIME_API  ZINT
primitive_wait_for_notification_timed(CONTAINER *notif, CONTAINER *lock,
                           ZINT zmilsecs)
{
  DWORD      start, current;
  NOTIFICATION  *notification;
  SIMPLELOCK  *slock;
  int      milsecs, owned, timeout, tmp1, tmp2;

  assert(notif != NULL);
  assert(notif->handle != NULL);
  assert(lock != NULL);
  assert(lock->handle != NULL);
  assert(IS_ZINT(zmilsecs));

  notification = notif->handle;
  slock = lock->handle;
  milsecs = zmilsecs >> 2;
  start = GetTickCount();

  owned = (int)primitive_owned_simple_lock(lock) >> 2;
  if (owned == 0) {
    MSG0("wait-for-notification-timed: Don't own associated lock\n");
    return NOT_LOCKED;
  }

  switch (WaitForSingleObject(notification->anti_notifier, milsecs)) {
  case WAIT_TIMEOUT:
    MSG0("wait-for-notification-timed: Timeout waiting for anti-notifier\n");
    return TIMEOUT;
  case WAIT_FAILED:
    MSG0("wait-for-notification-timed: Error waiting for anti-notifier\n");
    return GENERAL_ERROR;
  }
  internal_InterlockedIncrement(&notification->count);
  primitive_release_simple_lock(lock);
  do {
    current = GetTickCount();
    timeout = milsecs - (current - start);
    switch (WaitForSingleObject(notification->notifier, timeout)) {
    case WAIT_TIMEOUT:
      MSG0("wait-for-notification-timed: Timeout waiting for notifier\n");
      tmp2 = internal_InterlockedDecrement(&notification->count);
      if (WaitForSingleObject(notification->notifier, 0) == WAIT_OBJECT_0 &&
          tmp2 <= 0) {
        ResetEvent(notification->notifier);
        SetEvent(notification->anti_notifier);
      }
      primitive_wait_for_simple_lock(lock);
      return TIMEOUT;
    case WAIT_FAILED:
      MSG0("wait-for-notification-timed: error waiting for notifier\n");
      return GENERAL_ERROR;
    }
    tmp1 = (int)internal_InterlockedCompareExchange
                  ((PVOID *)(&notification->target), (PVOID)1, (PVOID)0);
  } while (tmp1 == 1);
  tmp2 = internal_InterlockedDecrement(&notification->count);
  if ((tmp1 != -1) || (tmp2 <= 0)) {
    // know it's not a release-all
    if (ResetEvent(notification->notifier) == FALSE ||
        SetEvent(notification->anti_notifier) == FALSE) {
      MSG0("wait-for-notification-timed: error (re)setting (anti)notifier\n");
      return GENERAL_ERROR;
    }
  }
  if (primitive_wait_for_simple_lock(lock) != OK) {
    MSG0("wait-for-notification-timed: error while reclaiming lock\n");
    return GENERAL_ERROR;
  }
  return OK;
}


/* 15 */
THREADS_RUN_TIME_API  ZINT
primitive_release_simple_lock(CONTAINER *lock)
{
  SIMPLELOCK  *slock;
  HANDLE     hThread;
  int      decRes;

  assert(lock != NULL);
  assert(lock->handle != NULL);

  slock = lock->handle;
  hThread = get_current_thread_handle();

  if (slock->owner != hThread) {
    MSG0("release-simple-lock: Error, don't own the lock\n");
    return NOT_LOCKED;
  }

  slock->owner = 0;
  decRes = internal_InterlockedDecrement(&slock->lock_count);
  if (decRes >= 0) {
    return primitive_release_simple_lock_internal(slock);
  }
  return OK;
}


/* 16 */
THREADS_RUN_TIME_API  ZINT
primitive_release_recursive_lock(CONTAINER *lock)
{
  RECURSIVELOCK  *rlock;
  HANDLE          hThread;
  int             decRes;

  assert(lock != NULL);
  assert(lock->handle != NULL);

  rlock = lock->handle;
  hThread = get_current_thread_handle();

  if (rlock->owner != hThread) {
    MSG0("release-recursive-lock: Error, don't own the lock\n");
    return NOT_LOCKED;
  }

  if (--rlock->recursion_count == 0) {
    // Give up the lock
    rlock->owner = 0;
    decRes = internal_InterlockedDecrement(&rlock->lock_count);
    if (decRes >= 0) {
      return primitive_release_recursive_lock_internal(rlock);
    }
  }

  return OK;
}


/* 17 */
THREADS_RUN_TIME_API  ZINT
primitive_release_semaphore(CONTAINER *lock)
{
  HANDLE  hSemaphore;

  assert(lock != NULL);
  assert(lock->handle != NULL);

  hSemaphore = lock->handle;
  if (ReleaseSemaphore(hSemaphore, (LONG)1, NULL) == FALSE) {
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
  int      owned;

  assert(notif != NULL);
  assert(notif->handle != NULL);
  assert(lock != NULL);
  assert(lock->handle != NULL);

  notification = notif->handle;
  slock = lock->handle;
  owned = (int)primitive_owned_simple_lock(lock) >> 2;
  if (owned == 0) {
    MSG0("release-notification: Don't own associated lock\n");
    return NOT_LOCKED;
  }

  if (WaitForSingleObject(notification->anti_notifier, INFINITE)
      != WAIT_OBJECT_0) {
    MSG0("release-notification: error waiting for anti-notifier\n");
    return GENERAL_ERROR;
  }

  // Check that there are threads waiting to be released
  if (notification->count > 0) {
    notification->target = 0;  // release one thread only
    if (ResetEvent(notification->anti_notifier) == FALSE ||
          SetEvent(notification->notifier) == FALSE) {
      MSG0("release-notification: error (re)setting (anti)notifier\n");
      return GENERAL_ERROR;
    }
  }
  return OK;
}


/* 19 */
THREADS_RUN_TIME_API  ZINT
primitive_release_all_notification(CONTAINER *notif, CONTAINER *lock)
{
  NOTIFICATION  *notification;
  SIMPLELOCK  *slock;
  int      owned;

  assert(notif != NULL);
  assert(notif->handle != NULL);
  assert(lock != NULL);
  assert(lock->handle != NULL);

  notification = notif->handle;
  slock = lock->handle;
  owned = (int)primitive_owned_simple_lock(lock) >> 2;
  if (owned == 0) {
    MSG0("release-all-notification: Don't own associated lock\n");
    return NOT_LOCKED;
  }

  if (WaitForSingleObject(notification->anti_notifier, INFINITE)
      != WAIT_OBJECT_0) {
    MSG0("release-all-notification: error waiting for anti-notifier\n");
    return GENERAL_ERROR;
  }

  // Check that there are threads waiting to be released
  if (notification->count > 0) {
    notification->target = -1;  // indicates a release-all
    if (ResetEvent(notification->anti_notifier) == FALSE ||
        SetEvent(notification->notifier) == FALSE) {
      MSG0("release-all-notification: error (re)setting (anti)notifier\n");
      return GENERAL_ERROR;
    }
  }
  return OK;
}


/* 20 */
THREADS_RUN_TIME_API  ZINT
primitive_make_recursive_lock(CONTAINER *lock, D_NAME name)
{
  RECURSIVELOCK *rlock;

  assert(lock != NULL);

  rlock = MMAllocMisc(sizeof(RECURSIVELOCK));
  if (rlock == NULL) {
    MSG0("make-recursive-lock: malloc failed\n");
    return GENERAL_ERROR;
  }

  rlock->lock_count = -1;
  rlock->semaphore = CreateSemaphore(NULL, 0, 1, NULL);
  if (rlock->semaphore == NULL) {
    MSG0("make-recursive-lock: error creating semaphore\n");
    return GENERAL_ERROR;
  }
  rlock->owner = 0;
  rlock->recursion_count = 0;
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
  if (CloseHandle(rlock->semaphore) == FALSE) {
    MSG0("destroy-recursive-lock: CloseHandle returned error\n");
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

  assert(lock != NULL);

  slock = MMAllocMisc(sizeof(SIMPLELOCK));
  if (slock == NULL) {
    MSG0("make-simple-lock: malloc failed\n");
    return GENERAL_ERROR;
  }

  slock->lock_count = -1;
  slock->semaphore = CreateSemaphore(NULL, 0, 1, NULL);
  if (slock->semaphore == NULL) {
    MSG0("make-simple-lock: error creating semaphore\n");
    return GENERAL_ERROR;
  }
  slock->owner = 0;
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

  slock = lock->handle;
  if (CloseHandle(slock->semaphore) == FALSE) {
    MSG0("destroy-simple-lock: CloseHandle returned error\n");
    return GENERAL_ERROR;
  }
  MMFreeMisc(slock, sizeof(SIMPLELOCK));
  return OK;
}


/* 24 */
THREADS_RUN_TIME_API  ZINT
primitive_owned_simple_lock(CONTAINER *lock)
{
  HANDLE     hThread;
  SIMPLELOCK  *slock;

  assert(lock != NULL);
  assert(lock->handle != NULL);

  slock = lock->handle;

  hThread = get_current_thread_handle();
  if (slock->owner == hThread) {
    return((ZINT)I(1)); // owned
  } else {
    return((ZINT)I(0)); // not owned
  }
}


/* 25 */
THREADS_RUN_TIME_API  ZINT
primitive_owned_recursive_lock(CONTAINER *lock)
{
  HANDLE      hThread;
  RECURSIVELOCK  *rlock;

  assert(lock != NULL);
  assert(lock->handle != NULL);

  rlock = lock->handle;

  hThread = get_current_thread_handle();
  if (rlock->owner == hThread) {
    return((ZINT)I(1)); // owned
  } else {
    return((ZINT)I(0)); // not owned
  }
}


/* 26 */
THREADS_RUN_TIME_API  ZINT
primitive_make_semaphore(CONTAINER *lock, D_NAME name,
                    ZINT zinitial, ZINT zmax)
{
  HANDLE  hSemaphore;
  int   initial = zinitial >> 2;
  int   max   = zmax >> 2;

  assert(lock != NULL);
  assert(IS_ZINT(zinitial));
  assert(IS_ZINT(zmax));

  hSemaphore = CreateSemaphore(NULL, initial, max, NULL);
  if (hSemaphore == NULL) {
    MSG0("make-semaphore: CreateSemaphore returned error\n");
    return GENERAL_ERROR;
  }
  lock->handle = hSemaphore;
  return OK;
}


/* 27 */
THREADS_RUN_TIME_API  ZINT
primitive_destroy_semaphore(CONTAINER *lock)
{
  assert(lock != NULL);
  assert(lock->handle != NULL);

  if (CloseHandle((HANDLE)(lock->handle)) == FALSE) {
    MSG0("destroy-semaphore: CloseHandle returned error\n");
    return GENERAL_ERROR;
  }
  return OK;
}


/* 28 */
THREADS_RUN_TIME_API  ZINT
primitive_make_notification(CONTAINER *notif, D_NAME name)
{
  NOTIFICATION *notification;

  assert(notif != NULL);

  notification = MMAllocMisc(sizeof(NOTIFICATION));
  if (notification == NULL) {
    MSG0("make-notification: malloc returned error\n");
    return GENERAL_ERROR;
  }
  notification->notifier = CreateEvent(NULL, TRUE, FALSE, NULL);
  notification->anti_notifier = CreateEvent(NULL, TRUE, TRUE, NULL);
  if (notification->notifier == NULL || notification->anti_notifier == NULL) {
    MSG0("make-notification: error creating (anti)notification event(s)\n");
    return GENERAL_ERROR;
  }
  notification->target = 1;
  notification->count = 0;
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

  notification = notif->handle;
  if (CloseHandle(notification->notifier) == FALSE ||
      CloseHandle(notification->anti_notifier) == FALSE) {
    MSG0("destroy-notification: error closing (anti)notifier event(s)\n");
    return GENERAL_ERROR;
  }
  MMFreeMisc(notification, sizeof(NOTIFICATION));
  return OK;
}


/* 30 */
THREADS_RUN_TIME_API  void
primitive_sleep(ZINT zmilsecs)
{
  DWORD milsecs = zmilsecs >> 2;

  assert(IS_ZINT(zmilsecs));
  Sleep(milsecs);
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

  EnterCriticalSection(&tlv_vector_list_lock);

  // Get offset into to TLV vector for the new variable
  variable_offset = TLV_vector_offset;

  // increment offset for the next new variable
  TLV_vector_offset += sizeof(Z);

  // First check if we need to grow the TLV vectors
  size = (int)(*((Z *)(default_tlv_vector + sizeof(Z)))) >> 2;
  limit = (size+2) * sizeof(Z);
  if (variable_offset >= limit) {
    grow_all_tlv_vectors(size+size);  // double the size each time we grow
  }

  // Put the variable's default value in the default TLV vector
  destination = (Z *)(default_tlv_vector + variable_offset);
  *destination = value;

  // Update all the active thread TLV vectors with the default value
  update_tlv_vectors(variable_offset, value);

  // Finished
  LeaveCriticalSection(&tlv_vector_list_lock);

  // return the offset into the TLV vector (an integer, not a pointer)
  return((void *)variable_offset);
}


/* Grow all TLV vectors
 */
void grow_all_tlv_vectors(newsize)
{
  TLV_VECTOR_LIST list;
  TLV_VECTOR new_default;

  // Wait for thread variable writes to finish
  while (internal_InterlockedCompareExchange(&tlv_writer_counter, TLV_GROW, 0)
         != 0);

  // Grow the default vector
  new_default = make_dylan_vector(newsize);
  copy_tlv_vector(new_default, default_tlv_vector);
  default_tlv_vector = new_default;

  // Grow each vector in the active thread list
  list = tlv_vector_list;
  while (list != NULL) {
    list->tlv_vector = grow_tlv_vector(list->tlv_vector, newsize);
    list = list->next;
  }

  // Let writes proceed again
  while (internal_InterlockedCompareExchange(&tlv_writer_counter, 0, TLV_GROW)
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
  if (internal_InterlockedIncrement((LPLONG)(&tlv_writer_counter)) < 0) {
    primitive_write_thread_variable_internal();
  }

  // The variable handle is the byte offset where the variable's value is
  // stored in the TLV.
  offset = (int)variable_handle;
  tlv_vector = get_tlv_vector();
  destination = (Z *)(tlv_vector + offset);
  *destination = new_value;

  // Indicate that the write has finished
  internal_InterlockedDecrement((LPLONG)(&tlv_writer_counter));

  return(new_value);
}


/* 36 */
THREADS_RUN_TIME_API  void
primitive_initialize_current_thread(DTHREAD *thread, BOOL synchronize)
{
  HANDLE      hThread, hProcess;
  HANDLE  *   events;
  TLV_VECTOR  tlv_vector;
  Z          *destination;
  int         size;

  assert(thread != NULL);

  if (synchronize) {
    events = thread->handle1;
    // Do we need to initialise?
    if (default_tlv_vector == NULL) {
      initialize_threads_primitives();
    }

    /* Get a handle for the current thread: GetCurrentThread() returns a
       special value which can only be used by a thread to refer to itself.
       We need a handle which other threads can use to refer to the current
       thread.
    */
    hProcess = GetCurrentProcess();
    DuplicateHandle(hProcess, GetCurrentThread(), hProcess, &hThread,
                    0, FALSE, DUPLICATE_SAME_ACCESS);
  } else {
    hThread = thread->handle1;
  }

  // Put the thread object and handle in the TEB for later use
  set_current_thread(thread);
  set_current_thread_handle(hThread);

  EnterCriticalSection(&tlv_vector_list_lock);

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

  LeaveCriticalSection(&tlv_vector_list_lock);

  // Clear the handle2 slot in the thread object
  // (which contained the address of the starting function)
  thread->handle2 = dylan_false;

  if (synchronize) {
    thread->handle1 = events[1];

    // Signal creating thread that we've finished initialisation
    SetEvent(events[0]);

    // Now wait for the creating thread and debugger
    if (WaitForSingleObject(events[1], INFINITE) != WAIT_OBJECT_0) {
      MSG0("initialize-thread: error waiting for debugger event\n");
      // return GENERAL_ERROR;
    }
    // Don't need the event any more
    if (CloseHandle(events[1]) == FALSE) {
      MSG0("initialize-thread: error closing event handle\n");
      // return GENERAL_ERROR;
    }

    thread->handle1 = hThread;

    MMFreeMisc(events, sizeof(HANDLE) * 2);
  }
}


/* 36a */
THREADS_RUN_TIME_API  void
primitive_initialize_special_thread(DTHREAD *thread)
{
  HANDLE  hProcess;

  assert(thread != NULL);

  // Do we need to initialise?
  if (default_tlv_vector == NULL) {
    initialize_threads_primitives();
  }

  // Get a handle for the current thread: GetCurrentThread() returns a
  // special value which can only be used by a thread to refer to itself.
  // We need a handle which other threads can use to refer to the current
  // thread.
  hProcess = GetCurrentProcess();
  DuplicateHandle(hProcess, GetCurrentThread(), hProcess, &thread->handle1,
                  0, FALSE, DUPLICATE_SAME_ACCESS);
  primitive_initialize_current_thread(thread, FALSE);
}

/* 36b */
THREADS_RUN_TIME_API  void
primitive_detach_thread(DTHREAD * thread)
{
  // do nothing for Win32
}





/* 37 */
THREADS_RUN_TIME_API  ZINT
primitive_unlock_simple_lock(CONTAINER *lock)
{
  SIMPLELOCK *slock;
  LONG    junk;
  int     decRes;

  assert(lock != NULL);
  assert(lock->handle != NULL);

  slock = lock->handle;
  if (slock->owner == 0) {
    /* nothing to do - lock already released */
    assert(slock->lock_count == -1);
    return OK;
  }
  slock->owner = 0;
  decRes = internal_InterlockedDecrement(&slock->lock_count);
  if (decRes >= 0) {
    if (ReleaseSemaphore(slock->semaphore, 1, &junk) == FALSE) {
      MSG0("unlock_simple_lock: error releasing semaphore\n");
      return GENERAL_ERROR;
    }
  }
  return OK;
}


/* 38 */
THREADS_RUN_TIME_API  ZINT
primitive_unlock_recursive_lock(CONTAINER *lock)
{
  RECURSIVELOCK *rlock;
  LONG           junk;

  assert(lock != NULL);
  assert(lock->handle != NULL);

  rlock = lock->handle;
  if (rlock->owner == 0) {
    // nothing to do - lock already released
    assert(rlock->lock_count == -1);
    assert(rlock->recursion_count == 0);
    return OK;
  }

  rlock->recursion_count = 0;
  rlock->owner = 0;
  if (internal_InterlockedDecrement(&rlock->lock_count) >= 0) {
    if (ReleaseSemaphore(rlock->semaphore, 1, &junk) == FALSE) {
      MSG0("unlock-recursive-lock: error releasing semaphore\n");
      return GENERAL_ERROR;
    }
  }
  return OK;
}


/* This function is called to initialise the primitives
 */
void initialize_threads_primitives()
{
  default_tlv_vector = make_dylan_vector(TLV_VECTOR_INITIAL_SIZE);
  assert(default_tlv_vector != NULL);
  InitializeCriticalSection(&tlv_vector_list_lock);
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
add_tlv_vector(HANDLE hThread, TLV_VECTOR tlv_vector)
{
  TLV_VECTOR_LIST new_element = MMAllocMisc(linksize);

  assert(new_element != NULL);

  // protect list updates so they don't interfere with each other
  EnterCriticalSection(&tlv_vector_list_lock);

  // initialise the new element and put it on the front of the list
  new_element->hThread = hThread;
  new_element->tlv_vector = tlv_vector;
  new_element->next = tlv_vector_list;
  tlv_vector_list = new_element;

  LeaveCriticalSection(&tlv_vector_list_lock);
}


/* A thread calls remove_tlv_vector just before it terminates. The function
 * removes the thread from the list of active threads.
 */
int
remove_tlv_vector(HANDLE hThread)
{
  TLV_VECTOR_LIST last, current;

  if (tlv_vector_list == NULL) { // empty list
    return(1);
  }

  // protect list updates so they don't interfere with each other
  EnterCriticalSection(&tlv_vector_list_lock);

  last = tlv_vector_list;
  if (tlv_vector_list->hThread == hThread) {
    // matches first entry in list
    tlv_vector_list = tlv_vector_list->next;
#ifdef C_TESTING
    MMFreeMisc(last->tlv_vector, linksize);
    MMFreeMisc(last, linksize);
#endif

    LeaveCriticalSection(&tlv_vector_list_lock);
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
      LeaveCriticalSection(&tlv_vector_list_lock);
      return(0);
    } else {
      last = current;
      current = current->next;
    }
  }

  // Reached the end of the list without finding thread's entry
  LeaveCriticalSection(&tlv_vector_list_lock);
  return(1);
}


/* The priority_map function maps dylan thread priorities to windows priorities
 * as below:
 *
 *   Dylan Priorities        windows priority
 *      < -1249            THREAD_PRIORITY_IDLE
 *    -1249 to -750       THREAD_PRIORITY_LOWEST
 *     -749 to -250    THREAD_PRIORITY_BELOW_NORMAL
 *     -250 to  249       THREAD_PRIORITY_NORMAL
 *    250 to  749      THREAD_PRIORITY_ABOVE_NORMAL
 *    750 to 1249         THREAD_PRIORITY_HIGHEST
 *      > 1249         THREAD_PRIORITY_TIME_CRITICAL
 */
int priority_map(int dylan_priority)
{
  int priority;

  if (dylan_priority < 0) {
    if (dylan_priority < -1249) {
      priority = THREAD_PRIORITY_IDLE;
    } else {
      priority = (dylan_priority - 250) / 500;
    }
  } else {
    if (dylan_priority > 1249) {
      priority = THREAD_PRIORITY_TIME_CRITICAL;
    } else {
      priority = (dylan_priority + 250) / 500;
    }
  }

  return (priority);
}



/*  We implement our own versions of InterlockedIncrement, InterlockedDecrement
 *  and InterlockedCompareExchange for efficiency reasons, and also because
 *  InterlockedCompareExchange is not available in Windows 95.
 */

/* Disable 'no return value' warning for the following three functions */
#pragma warning( disable : 4035 )

/* Increment the 32-bit value pointed to by var. Prevents other threads from
 * using the value simultaneously.
 * Returns: zero if the result of the increment was 0
 *      a value less than zero if the result of the increment was < 0
 *      a value greater than zero if the result of the increment was > 0
 */
LONG internal_InterlockedIncrement(LPLONG var)
{
  __asm {
    mov           ecx,var
    mov           eax,0x00000001
    lock xadd     dword ptr [ecx],eax
    inc           eax
  }
}

/* Decrement the 32-bit value pointed to by var. Prevents other threads from
 * using the value simultaneously
 * Returns: zero if the result of the decrement was 0
 *      a value less than zero if the result was < 0
 *      a value greater than zero if the result was > 0
 */
LONG internal_InterlockedDecrement(LPLONG var)
{
  __asm {
    mov           ecx,var
    mov           eax,0xffffffff
    lock xadd     dword ptr [ecx],eax
    dec           eax
  }
}

/* Atomically compares the destination and compare values, and stores the
 * exchange value in the destination if they are equal (otherwise does
 * nothing). Returns the initial value of the destination.
 */
PVOID internal_InterlockedCompareExchange(PVOID *destination, PVOID exchange,
                                          PVOID compare)
{
  __asm {
    mov           ecx,destination
    mov           edx,exchange
    mov           eax,compare
    lock cmpxchg  dword ptr [ecx],edx
  }
}
