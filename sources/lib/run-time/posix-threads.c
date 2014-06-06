/*
 * File:   posix-threads.c
 * Author: Keith Dennison
 * Copyright (c) 1999 Functional Objects, Inc. All rights reserved.
 *
 * A description of the implementation of the primitives in this file can be
 * found in D-doc-design-runtime!win32-thread-portability.text
 */

#include "posix-threads.h"

#include "trace.h"

#include <errno.h>
#include <assert.h>
#include <stddef.h>
#include <stdlib.h>
#include <inttypes.h>

#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include <limits.h>

#if defined(GC_USE_BOEHM)
#  include <gc/gc.h>
#  include <gc/gc_pthread_redirects.h>
#elif defined(GC_USE_MALLOC)
#  define GC_FREE free
#  define GC_MALLOC malloc
#  define GC_MALLOC_UNCOLLECTABLE malloc
#endif

#include <sys/time.h>

#ifdef OPEN_DYLAN_PLATFORM_LINUX
/* get prctl */
#include <sys/prctl.h>
#endif
#ifdef OPEN_DYLAN_PLATFORM_FREEBSD
/* get pthread_set_name_np */
#include <pthread_np.h>
#endif


static void timespec_add_msecs(struct timespec *tp, long msecs) {
  long secs = msecs / 1000;
  msecs = msecs % 1000;
  tp->tv_sec += secs;
  tp->tv_nsec += msecs * 1000000L;
  if (tp->tv_nsec >= 1000000000L) {
    tp->tv_sec += tp->tv_nsec / 1000000000L;
    tp->tv_nsec = tp->tv_nsec % 1000000000L;
  }
}

#ifdef HAVE_POSIX_TIMERS
static void timespec_current(struct timespec *tp) {
  clock_gettime(CLOCK_REALTIME, tp);
}
#else
static void timespec_current(struct timespec *tp) {
  struct timeval tv;
  gettimeofday(&tv, NULL);
  tp->tv_sec = tv.tv_sec;
  tp->tv_nsec = tv.tv_usec * 1000;
}
#endif


#if defined(HAVE_POSIX_THREADS) && defined(HAVE_POSIX_TIMEOUTS)
#define our_pthread_mutex_timedlock pthread_mutex_timedlock
#else
/* emulate pthread_mutex_timedlock when not provided */
static int our_pthread_mutex_timedlock(pthread_mutex_t *mutex,
                                       struct timespec *end) {
  int res;
  struct timespec cur;
  do {
    /* get current time */
    timespec_current(&cur);
    /* time out when appropriate */
    if ((cur.tv_sec > end->tv_sec)
        || (cur.tv_sec == end->tv_sec
            && cur.tv_nsec >= end->tv_nsec)) {
      return ETIMEDOUT;
    }
    /* do a trylock */
    res = pthread_mutex_trylock(mutex);
    /* wait for 100 usec if busy */
    if (res == EBUSY) {
      struct timespec delay;
      delay.tv_sec = 0;
      delay.tv_nsec = 100 * 1000;
      nanosleep(&delay, &delay);
      continue;
    }
    /* else, there was an error or success */
    if (res) {
      /* pass other error codes */
      return res;
    } else {
      /* lock acquired */
      return 0;
    }
  } while (1);
}
#endif


#define ignore(x) (void)x

#ifndef _DEBUG      /* For Release builds */
#define  MSG0(msg)                          ((void)0)
#define  MSG1(msg, arg1)                    ((void)0)
#else               /* For Debug builds */
#define  MSG0(msg)                          printf(msg)
#define  MSG1(msg, arg1)                    printf(msg, arg1)
#endif

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

extern OBJECT KPfalseVKi;

static pthread_mutex_t thread_join_lock = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t thread_exit_event = PTHREAD_COND_INITIALIZER;

static TLV_VECTOR       default_tlv_vector = NULL;

static pthread_mutex_t  tlv_vector_list_lock = PTHREAD_MUTEX_INITIALIZER;
static TLV_VECTOR_LIST  tlv_vector_list;

static long tlv_writer_counter = 0;

static size_t  TLV_vector_offset = 2;


/*****************************************************************************/
/* LOCAL FUNCTION DECLARATIONS                                               */
/*****************************************************************************/

void  initialize_threads_primitives(void);
static void *make_tlv_vector(size_t);
static int   priority_map(int);

static TLV_VECTOR grow_tlv_vector(TLV_VECTOR vector, size_t newsize);
static void grow_all_tlv_vectors(size_t newsize);
static void  copy_tlv_vector(TLV_VECTOR destination, TLV_VECTOR source);
static void update_tlv_vectors(size_t offset, D value);
static void add_tlv_vector(DTHREAD *thread, TEB *teb, TLV_VECTOR tlv_vector);
static int remove_tlv_vector(DTHREAD *thread);


/* TEB management */

#ifdef USE_PTHREAD_TLS
pthread_key_t teb_key;

PURE_FUNCTION TEB* get_teb(void)
{
  return (TEB*)pthread_getspecific(teb_key);
}

static void set_teb(TEB* teb)
{
  pthread_setspecific(teb_key, (void*)teb);
}

static void initialize_teb_key(void)
{
  pthread_key_create(&teb_key, NULL);
}
#else
__thread TEB *dylan_teb;

static void set_teb(TEB* new_teb)
{
  dylan_teb = new_teb;
}

static void initialize_teb_key(void)
{
}
#endif

static TEB* make_teb(void)
{
  TEB* teb = (TEB*)GC_MALLOC_UNCOLLECTABLE(sizeof(TEB));

  memset(teb, 0, sizeof(TEB));

  teb->uwp_frame = &teb->top_uwp_frame;

  set_teb(teb);

  return teb;
}

static void free_teb(void)
{
  TEB* teb = get_teb();

  set_teb(NULL);

  GC_FREE(teb);
}


/* TEB accessors */

PURE_FUNCTION static inline void *get_tlv_vector(void)
{
  return get_teb()->tlv_vector;
}

static inline void set_tlv_vector(void *vector)
{
  get_teb()->tlv_vector = vector;
}

PURE_FUNCTION static inline void *get_current_thread(void)
{
  return get_teb()->thread;
}

static inline void set_current_thread(void *thread)
{
  get_teb()->thread = thread;
}

PURE_FUNCTION static inline void *get_current_thread_handle(void)
{
  return get_teb()->thread_handle;
}

static inline void set_current_thread_handle(void *handle)
{
  get_teb()->thread_handle = handle;
}


/* TLV management */

static void *make_tlv_vector(size_t n)
{
  D *vector;
  size_t size;

  // compute actual (byte) size
  size = (n + 2) * sizeof(D);

  // fill int the vector
  vector = GC_MALLOC_UNCOLLECTABLE(size);
  memset(vector, 0, size);
  vector[0] = NULL;
  vector[1] = I(n);

  // done
  return vector;
}

static void free_tlv_vector(D *vector)
{
  GC_FREE(vector);
}


/* Grow a single TLV vector
 */
static TLV_VECTOR grow_tlv_vector(TLV_VECTOR vector, size_t newsize)
{
  TLV_VECTOR  new_vector;

  trace_tlv("Growing vector %p", vector);

  // allocate a new vector and copy the values in the old across
  new_vector = make_tlv_vector(newsize);
  copy_tlv_vector(new_vector, vector);

  // return the new vector
  return(new_vector);
}


static void grow_all_tlv_vectors(size_t newsize)
{
  TLV_VECTOR_LIST list;
  TLV_VECTOR new_default;

  trace_tlv("Growing all vectors to size %zd", newsize);

  // Wait until we are the only writer
  while (atomic_cas(&tlv_writer_counter, TLV_GROW, 0) != 0);

  // Grow the default vector
  new_default = make_tlv_vector(newsize);
  copy_tlv_vector(new_default, default_tlv_vector);
  default_tlv_vector = new_default;

  // Grow each vector in the active thread list
  list = tlv_vector_list;
  while (list != NULL) {
    list->tlv_vector = grow_tlv_vector(list->tlv_vector, newsize);
    list->teb->tlv_vector = list->tlv_vector;
    list = list->next;
  }

  // Let writes proceed again
  while (atomic_cas(&tlv_writer_counter, 0, TLV_GROW) != TLV_GROW);
}


/* Copy a tlv vector. Assumes the destination vector is at least as large
 * as the source vector.
 */
void copy_tlv_vector(TLV_VECTOR destination, TLV_VECTOR source)
{
  size_t i, limit;

  limit = ((size_t)(source[1]) >> 2) + 2;
  for (i = 2; i<limit; i++)
    destination[i] = source[i];
}


/* Add a new variable to all the TLV vectors in the active thread list.
 * Assumes the vectors do not need to be grown. Also, the calling function
 * must be in the tlv_vector_list_lock Critical Section.
 */
static void
update_tlv_vectors(size_t offset, D value)
{
  TLV_VECTOR_LIST list = tlv_vector_list;
  D *destination;

  trace_tlv("Propagating default of offset %zd with value %p", offset, value);

  while (list != NULL) {
    destination = (D *)(list->tlv_vector + offset);
    *destination = value;
    list = list->next;
  }
}


/* add_tlv_vector adds a new thread to the active thread vector list.
 * Assumes the thread vector has already been initialised.
 */
static void
add_tlv_vector(DTHREAD *thread, TEB *teb, TLV_VECTOR tlv_vector)
{
  TLV_VECTOR_LIST new_element = GC_MALLOC_UNCOLLECTABLE(sizeof(struct tlv_vector_list_element));

  assert(new_element != NULL);

  memset(new_element, 0, sizeof(struct tlv_vector_list_element));

  trace_tlv("Adding vector %p for thread %p", tlv_vector, thread);

  // initialise the new element and put it on the front of the list
  new_element->thread = thread;
  new_element->teb = teb;
  new_element->tlv_vector = tlv_vector;
  new_element->next = tlv_vector_list;
  tlv_vector_list = new_element;
}


/* A thread calls remove_tlv_vector just before it terminates. The function
 * removes the thread from the list of active threads.
 */
static int
remove_tlv_vector(DTHREAD *thread)
{
  TLV_VECTOR_LIST last, current;

  trace_tlv("Removing vector for thread %p", thread);

  if (tlv_vector_list == NULL)  // empty list
    return(1);

  last = tlv_vector_list;
  if (tlv_vector_list->thread == thread) {
    // matches first entry in list
    tlv_vector_list = tlv_vector_list->next;
    free_tlv_vector(last->tlv_vector);
    GC_FREE(last);
    return(0);
  }

  current = tlv_vector_list->next;
  while (current != NULL) {
    if (current->thread == thread) {
      // found the right entry, so cut it out
      last->next = current->next;
      free_tlv_vector(current->tlv_vector);
      GC_FREE(current);
      return(0);
    }
    else {
      last = current;
      current = current->next;
    }
  }

  // Reached the end of the list without finding thread's entry
  return(1);
}

/*
 * Set up the TLV vector for the given thread.
 *
 * Note that THREAD may be NULL in case of the initial thread,
 * which does not need this marker since its vectors will
 * never be removed.
 */
static void setup_tlv_vector(DTHREAD *thread)
{
  TEB         *teb;
  TLV_VECTOR   tlv_vector;
  size_t    size;

  trace_tlv("Setting up TLV vector for thread %p", thread);

  teb = get_teb();

  pthread_mutex_lock(&tlv_vector_list_lock);

  tlv_vector = get_tlv_vector();

  if (!tlv_vector) {
    // Now set up a vector for the Dylan thread variables
    size = (size_t)(default_tlv_vector[1]) >> 2;
    tlv_vector = make_tlv_vector(size);
    set_tlv_vector(tlv_vector);

    // Initialise the vector with the values from the default vector
    copy_tlv_vector(tlv_vector, default_tlv_vector);

    // Add thread to active thread list
    add_tlv_vector(thread, teb, tlv_vector);
  }

  pthread_mutex_unlock(&tlv_vector_list_lock);
}

/*
 * Called once from _Init_Run_Time() to initialize globally.
 */
void initialize_threads_primitives(void)
{
  trace_threads("Initializing thread primitives");

  // Set up vector of default values for thread variables
  default_tlv_vector = make_tlv_vector(TLV_VECTOR_INITIAL_SIZE);

  initialize_teb_key();

  // Allocate the TEB for the initial thread
  make_teb();

  // Initialize the TLV vector for the initial thread
  setup_tlv_vector(NULL);
}


/*****************************************************************************/
/* THREAD PRIMITIVES                                                         */
/*****************************************************************************/

static void set_current_thread_name(const char *name) {
#ifdef OPEN_DYLAN_PLATFORM_LINUX
  /* gdb shows this, so set it too */
  prctl(PR_SET_NAME, (unsigned long)name, 0, 0, 0);
  extern int pthread_setname_np(pthread_t, const char*) __attribute__((weak));
  if (pthread_setname_np) {
    pthread_setname_np(pthread_self(), name);
  }
#endif
#ifdef OPEN_DYLAN_PLATFORM_FREEBSD
  pthread_set_name_np(pthread_self(), name);
#endif
#ifdef OPEN_DYLAN_PLATFORM_DARWIN
  pthread_setname_np(name);
#endif
}

static void *trampoline (void *arg)
{
  D        result, f;
  DTHREAD *thread = (DTHREAD *)arg;
  THREAD  *rthread;

  assert(thread != NULL);


  rthread = (THREAD*)(thread->handle2);

  rthread->teb = make_teb();

  f = rthread->function;

  if (rthread->name) {
    const char *raw = primitive_string_as_raw(rthread->name);
    trace_threads("Thread %p has name \"%s\"", thread, raw);
    set_current_thread_name(raw);
  }

  setup_tlv_vector(thread);

  trace_threads("Thread %p starts function %p", thread, f);

  result = CALL0(f);

  trace_threads("Thread %p returned %p", thread, result);

  remove_tlv_vector(thread);

  pthread_mutex_lock(&thread_join_lock);
  thread->handle1 = (void *)((uintptr_t)thread->handle1 | COMPLETED);
  pthread_cond_broadcast(&thread_exit_event);
  pthread_mutex_unlock(&thread_join_lock);

  free_teb();

  return result;
}


/* 1 */
D primitive_make_thread(D t, D n, D p, D f, DBOOL s)
{
  DTHREAD *thread = (DTHREAD *)t;
  ZINT     zpriority = (ZINT)p;

  THREAD*             rthread;
  pthread_attr_t      attr;
  // struct sched_param  param;
  // int                 priority = (int)zpriority >> 2;

  ignore(s);

  assert(thread != NULL);
  assert(IS_ZINT(zpriority));
  assert(f != NULL);

  rthread = primitive_allocate(sizeof(THREAD));
  rthread->name = n;
  rthread->function = f;

  thread->handle1 = 0;       // runtime thread flags
  thread->handle2 = rthread; // runtime thread object

  // param.sched_priority = priority_map(priority);

  if (pthread_attr_init(&attr)) {
    MSG0("make-thread: error attr_init\n");
    return CREATE_ERROR;
  }

  if (pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED)) {
    MSG0("make-thread: error attr_setdetachstate\n");
    return CREATE_ERROR;
  }

//  if (pthread_attr_setschedparam(&attr, &param)) {
//    MSG0("make-thread: error set_schedparam\n");
//    return CREATE_ERROR;
//  }

  if (pthread_create(&rthread->tid, &attr, trampoline, thread)) {
    MSG0("make-thread: error creating thread\n");
    return CREATE_ERROR;
  }

  if (pthread_attr_destroy(&attr)) {
    MSG0("make-thread: error destroy\n");
    return CREATE_ERROR;
  }

  return OK;
}


/* 2 */
D primitive_destroy_thread(D t)
{
  DTHREAD    *thread = (DTHREAD *)t;

  assert(thread != NULL);

  return OK;
}


/* 3 */
D primitive_thread_join_single(D t)
{
  volatile DTHREAD *thread = t;
  uintptr_t         state, completed;

  assert(thread != NULL);

  if (pthread_mutex_lock(&thread_join_lock) != 0) {
    MSG0("thread-join-single: error obtaining thread join lock\n");
    return GENERAL_ERROR;
  }

  state = (uintptr_t)thread->handle1;
  if (state & MARKED || state & JOINED) {
    pthread_mutex_unlock(&thread_join_lock);
    MSG0("thread-join-single: duplicate join error\n");
    return GENERAL_ERROR;
  }

  thread->handle1 = (void *)(state | MARKED);
  completed = state & COMPLETED;
  while (!completed) {
    if (pthread_cond_wait(&thread_exit_event, &thread_join_lock)) {
      MSG0("thread-join-single: error waiting for thread exit event\n");
      return GENERAL_ERROR;
    }
    completed = (uintptr_t)thread->handle1 & COMPLETED;
  }

  thread->handle1 = (void *)((uintptr_t)thread->handle1 ^ (JOINED | MARKED));

  if (pthread_mutex_unlock(&thread_join_lock) != 0) {
    MSG0("thread-join-single: error releasing thread join lock\n");
    return GENERAL_ERROR;
  }

  return OK;
}


/* 4 */
D primitive_thread_join_multiple(D v)
{
  SOV               *thread_vector = v;
  volatile DTHREAD **threads;
  volatile DTHREAD  *joined_thread = NULL;
  unsigned int       i;
  uintptr_t          size, state;

  assert(thread_vector != NULL);
  assert(IS_ZINT(thread_vector->size));

  size  = ((uintptr_t)(thread_vector->size)) >> 2;
  threads = (volatile DTHREAD **)(thread_vector->data);

  if (pthread_mutex_lock(&thread_join_lock)) {
    MSG0("thread-join-multiple: error obtaining thread join lock\n");
    return GENERAL_ERROR;
  }

  /* Make sure none of the threads is already
   * part of a join operation
   */
  for (i = 0; i < size; i++) {
    state = (uintptr_t)threads[i]->handle1;
    if (state & MARKED || state & JOINED) {
      MSG0("thread-join-multiple: duplicate join error\n");
      return GENERAL_ERROR;
    }
  }

  /* Now mark the threads as being part of a join
   */
  for (i = 0; i < size; i++) {
    state = (uintptr_t)threads[i]->handle1;
    threads[i]->handle1 = (void *)(state | MARKED);
  }

  for (i = 0; i < size; i++) {
    state = (uintptr_t)threads[i]->handle1;
    if (state & COMPLETED) {
      joined_thread = threads[i];
      break;
    }
  }

  while (joined_thread == NULL) {
    if (pthread_cond_wait(&thread_exit_event, &thread_join_lock)) {
      MSG0("thread-join-multiple: error waiting for thread exit event\n");
      return GENERAL_ERROR;
    }
    for (i = 0; i < size; i++) {
      if ((uintptr_t)threads[i]->handle1 & COMPLETED) {
        joined_thread = threads[i];
        break;
      }
    }
  }

  state = (uintptr_t)joined_thread->handle1;
  joined_thread->handle1 = (void *)(state | JOINED);

  for (i = 0; i < size; i++) {
    state = (uintptr_t)threads[i]->handle1;
    threads[i]->handle1 = (void *)(state ^ MARKED);
  }

  if (pthread_mutex_unlock(&thread_join_lock) != 0) {
    MSG0("thread-join-multiple: error releasing thread join lock\n");
    return GENERAL_ERROR;
  }

  return (DTHREAD*)joined_thread;
}

/* 4.5 */
void primitive_detach_thread(D t)
{
  DTHREAD    *thread = (DTHREAD *)t;

  assert(thread != NULL);
}

/* 5 */
void primitive_thread_yield(void)
{
  // Causes thread to give up its remaining time slice
  sched_yield();
}


/* 6. */
D primitive_current_thread(void)
{
  return get_current_thread();
}


/* 7 */
D primitive_wait_for_simple_lock(D l)
{
  CONTAINER   *lock = (CONTAINER *)l;
  SIMPLELOCK  *slock;
  TEB         *teb;

  assert(lock != NULL);
  assert(lock->handle != NULL);

  teb = get_teb();
  slock = lock->handle;

  int res = pthread_mutex_lock(&slock->mutex);
  if (res == EDEADLK) {
    return ALREADY_LOCKED;
  }
  if (res) {
    MSG0("wait-for-simple-lock: Error locking mutex\n");
    return GENERAL_ERROR;
  }

  slock->owner = teb;

  return OK;
}


/* 8 */
D primitive_wait_for_recursive_lock(D l)
{
  CONTAINER     *lock = (CONTAINER *)l;
  RECURSIVELOCK *rlock;
  TEB           *teb;

  assert(lock != NULL);
  assert(lock->handle != NULL);

  teb = get_teb();
  rlock = lock->handle;

  int res = pthread_mutex_lock(&rlock->mutex);
  if (res == EDEADLK) {
    return ALREADY_LOCKED;
  }
  if (res) {
    MSG0("wait-for-recursive-lock: Error locking mutex\n");
    return GENERAL_ERROR;
  }

  atomic_increment(&rlock->count);

  rlock->owner = teb;

  return OK;
}


/* 9 */
D primitive_wait_for_semaphore(D l)
{
  CONTAINER  *lock = (CONTAINER *)l;
  SEMAPHORE  *semaphore;

  assert(lock != NULL);
  assert(lock->handle != NULL);

  semaphore = lock->handle;

#ifdef HAVE_POSIX_SEMAPHORES
  int res = sem_wait(&semaphore->semaphore);
  if (res) {
    MSG0("wait-for-semaphore: sem_wait returned error\n");
    return GENERAL_ERROR;
  }
#else
  if (pthread_mutex_lock(&semaphore->mutex)) {
    MSG0("wait-for-semaphore: pthread_mutex_lock returned error\n");
    return GENERAL_ERROR;
  }

  while (semaphore->count <= 0) {
    pthread_cond_wait(&semaphore->cond, &semaphore->mutex);
  }

  semaphore->count--;

  if (pthread_mutex_unlock(&semaphore->mutex)) {
    MSG0("wait-for-semaphore: pthread_mutex_unlock returned error\n");
    return GENERAL_ERROR;
  }
#endif

  return OK;
}


/* 10 */
D primitive_wait_for_notification(D n, D l)
{
  CONTAINER     *notif = (CONTAINER *)n;
  CONTAINER     *lock = (CONTAINER *)l;
  NOTIFICATION  *notification;
  SIMPLELOCK    *slock;
  int            error;

  assert(notif != NULL);
  assert(notif->handle != NULL);
  assert(lock != NULL);
  assert(lock->handle != NULL);

  notification = notif->handle;
  slock = lock->handle;

  error = pthread_cond_wait(&notification->cond, &slock->mutex);
  if (error) {
    MSG0("wait-for-notification: Error claiming associated lock");
    return GENERAL_ERROR;
  }

  return OK;
}


/* 11 */
D primitive_wait_for_simple_lock_timed(D l, D ms)
{
  CONTAINER   *lock = (CONTAINER *)l;
  ZINT         zmilsecs = (ZINT)ms;
  SIMPLELOCK  *slock;
  TEB         *teb;
  long         milsecs;
  struct timespec end;

  assert(lock != NULL);
  assert(lock->handle != NULL);
  assert(IS_ZINT(zmilsecs));

  teb = get_teb();
  slock = lock->handle;

  timespec_current(&end);
  milsecs = zmilsecs >> 2;
  timespec_add_msecs(&end, milsecs);

  int res = our_pthread_mutex_timedlock(&slock->mutex, &end);
  if (res == ETIMEDOUT) {
    return TIMEOUT;
  }
  if (res == EDEADLK) {
    return ALREADY_LOCKED;
  }
  if (res) {
    MSG0("wait-for-simple-lock-timed: Error unlocking mutex\n");
    return GENERAL_ERROR;
  }

  slock->owner = teb;

  return OK;
}


/* 12 */
D primitive_wait_for_recursive_lock_timed(D l, D ms)
{
  CONTAINER       *lock = (CONTAINER *)l;
  ZINT             zmilsecs = (ZINT)ms;
  RECURSIVELOCK   *rlock;
  TEB             *teb;
  long             milsecs;
  struct timespec  end;

  assert(lock != NULL);
  assert(lock->handle != NULL);
  assert(IS_ZINT(zmilsecs));

  teb = get_teb();
  rlock = lock->handle;

  timespec_current(&end);
  milsecs = zmilsecs >> 2;
  timespec_add_msecs(&end, milsecs);

  int res = our_pthread_mutex_timedlock(&rlock->mutex, &end);
  if (res == ETIMEDOUT) {
    return TIMEOUT;
  }
  if (res == EDEADLK) {
    return ALREADY_LOCKED;
  }
  if (res) {
    MSG0("wait-for-recursive-lock-timed: Error unlocking mutex\n");
    return GENERAL_ERROR;
  }

  atomic_increment(&rlock->count);

  rlock->owner = teb;

  return OK;
}


/* 13 */
D primitive_wait_for_semaphore_timed(D l, D m)
{
  CONTAINER  *lock = (CONTAINER *)l;
  ZINT        zmilsecs = (ZINT)m;
  SEMAPHORE  *semaphore;
  long        milsecs;
  struct timespec end;

  assert(lock != NULL);
  assert(lock->handle != NULL);
  assert(IS_ZINT(zmilsecs));

  timespec_current(&end);
  milsecs = zmilsecs >> 2;
  timespec_add_msecs(&end, milsecs);

  semaphore = lock->handle;

#ifdef HAVE_POSIX_SEMAPHORES
  int res = sem_timedwait(&semaphore->semaphore, &end);
  if (res == ETIMEDOUT) {
    return TIMEOUT;
  }
  if (res) {
    MSG0("wait-for-semaphore: sem_timedwait returned error\n");
    return GENERAL_ERROR;
  }
#else
  if (pthread_mutex_lock(&semaphore->mutex)) {
    MSG0("wait-for-semaphore: pthread_mutex_lock returned error\n");
    return GENERAL_ERROR;
  }

  while (semaphore->count <= 0) {
    int res = pthread_cond_timedwait(&semaphore->cond, &semaphore->mutex, &end);
    if (res == ETIMEDOUT) {
      return TIMEOUT;
    }
  }

  semaphore->count--;

  if (pthread_mutex_unlock(&semaphore->mutex)) {
    MSG0("wait-for-semaphore: pthread_mutex_unlock returned error\n");
    return GENERAL_ERROR;
  }
#endif

  return OK;
}


/* 14 */
D primitive_wait_for_notification_timed(D n, D l, D m)
{
  CONTAINER     *notif = (CONTAINER *)n;
  CONTAINER     *lock = (CONTAINER *)l;
  ZINT           zmilsecs = (ZINT)m;
  NOTIFICATION  *notification;
  SIMPLELOCK    *slock;
  ZINT           milsecs;
  struct timespec end;

  assert(notif != NULL);
  assert(notif->handle != NULL);
  assert(lock != NULL);
  assert(lock->handle != NULL);
  assert(IS_ZINT(zmilsecs));

  notification = notif->handle;
  slock = lock->handle;

  timespec_current(&end);
  milsecs = zmilsecs >> 2;
  timespec_add_msecs(&end, milsecs);

  int res = pthread_cond_timedwait(&notification->cond, &slock->mutex, &end);
  if (res == ETIMEDOUT) {
    return TIMEOUT;
  }
  if (res) {
    MSG0("wait-for-notification-timed: error\n");
    return GENERAL_ERROR;
  }

  return OK;
}


/* 15 */
D primitive_release_simple_lock(D l)
{
  CONTAINER   *lock = (CONTAINER *)l;
  SIMPLELOCK  *slock;

  assert(lock != NULL);
  assert(lock->handle != NULL);

  slock = lock->handle;

  slock->owner = 0;

  int res = pthread_mutex_unlock(&slock->mutex);
  if (res == EPERM) {
    return NOT_LOCKED;
  }
  if (res) {
    MSG0("release-simple-lock: error signalling cond\n");
    return GENERAL_ERROR;
  }

  return OK;
}


/* 16 */
D primitive_release_recursive_lock(D l)
{
  CONTAINER      *lock = (CONTAINER *)l;
  RECURSIVELOCK  *rlock;

  assert(lock != NULL);
  assert(lock->handle != NULL);

  rlock = lock->handle;

  int res = pthread_mutex_unlock(&rlock->mutex);
  if (res == EPERM) {
    return NOT_LOCKED;
  }
  if (res) {
    MSG0("release-simple-lock: error signalling cond\n");
    return GENERAL_ERROR;
  }

  atomic_decrement(&rlock->count);

  return OK;
}


/* 17 */
D primitive_release_semaphore(D l)
{
  CONTAINER  *lock = (CONTAINER *)l;
  SEMAPHORE  *semaphore;

  assert(lock != NULL);
  assert(lock->handle != NULL);

  semaphore = lock->handle;

#ifdef HAVE_POSIX_SEMAPHORES
  int res = sem_post(&semaphore->semaphore);
  if (res) {
    MSG0("release-semaphore: sem_post returned error\n");
    return GENERAL_ERROR;
  }
#else
  if (pthread_mutex_lock(&semaphore->mutex)) {
    MSG0("release-semaphore: pthread_mutex_lock returned error\n");
    return GENERAL_ERROR;
  }

  if (semaphore->count >= semaphore->max_count) {
    MSG0("release-semaphore: count exceeded\n");
    return COUNT_EXCEEDED;
  }

  semaphore->count++;

  if (pthread_mutex_unlock(&semaphore->mutex)
      || pthread_cond_signal(&semaphore->cond)) {
    MSG0("release-semaphore: error releasing semaphore\n");
    return GENERAL_ERROR;
  }
#endif

  return OK;
}


/* 18 */
D primitive_release_notification(D n, D l)
{
  CONTAINER     *notif = (CONTAINER *)n;
  CONTAINER     *lock = (CONTAINER *)l;
  NOTIFICATION  *notification;

  assert(notif != NULL);
  assert(notif->handle != NULL);
  assert(lock != NULL);
  assert(lock->handle != NULL);

  notification = notif->handle;

  if (pthread_cond_signal(&notification->cond)) {
    MSG0("release-notification: error signalling condition variable\n");
    return GENERAL_ERROR;
  }

  return OK;
}


/* 19 */
D primitive_release_all_notification(D n, D l)
{
  CONTAINER     *notif = (CONTAINER *)n;
  CONTAINER     *lock = (CONTAINER *)l;
  NOTIFICATION  *notification;

  assert(notif != NULL);
  assert(notif->handle != NULL);
  assert(lock != NULL);
  assert(lock->handle != NULL);

  notification = notif->handle;

  if (pthread_cond_broadcast(&notification->cond)) {
    MSG0("release-all-notification: error broadcasting condition variable");
    return GENERAL_ERROR;
  }

  return OK;
}


/* 20 */
D primitive_make_recursive_lock(D l, D n)
{
  CONTAINER      *lock = (CONTAINER *)l;
  RECURSIVELOCK  *rlock;

  int                 res;
  pthread_mutexattr_t attrs;

  ignore(n);

  assert(lock != NULL);

  rlock = (RECURSIVELOCK *)malloc(sizeof(RECURSIVELOCK));
  if (rlock == NULL) {
    MSG0("make-recursive-lock: malloc failed\n");
    return GENERAL_ERROR;
  }

  res = pthread_mutexattr_init(&attrs);
  if (res) {
    MSG0("make-recursive-lock: pthread_mutexattr_init returned error\n");
    goto err;
  }
  res = pthread_mutexattr_settype(&attrs, PTHREAD_MUTEX_RECURSIVE);
  if (res) {
    MSG0("make-recursive-lock: pthread_mutexattr_settype returned error\n");
    goto err;
  }
  res = pthread_mutex_init(&rlock->mutex, &attrs);
  if (res) {
    MSG0("make-recursive-lock: pthread_mutex_init returned error\n");
    goto err;
  }

  rlock->owner = 0;
  rlock->count = 0;

  lock->handle = rlock;

  return OK;

 err:
  MSG0("make-recursive-lock: error creating mutex\n");
  free(rlock);
  return GENERAL_ERROR;
}


/* 21 */
D primitive_destroy_recursive_lock(D l)
{
  CONTAINER     *lock = (CONTAINER *)l;
  RECURSIVELOCK *rlock;

  assert(lock != NULL);
  assert(lock->handle != NULL);

  rlock = lock->handle;
  int res = pthread_mutex_destroy(&rlock->mutex);
  if (res) {
    MSG0("destroy-recursive-lock: error destroying mutex\n");
    return GENERAL_ERROR;
  }
  free(rlock);
  return OK;
}


/* 22 */
D primitive_make_simple_lock(D l, D n)
{
  CONTAINER  *lock = (CONTAINER *)l;
  SIMPLELOCK *slock;

  int                 res;
  pthread_mutexattr_t attrs;

  ignore(n);

  assert(lock != NULL);

  slock = (SIMPLELOCK *)malloc(sizeof(SIMPLELOCK));
  if (slock == NULL) {
    MSG0("make-simple-lock: malloc failed\n");
    return GENERAL_ERROR;
  }

  res = pthread_mutexattr_init(&attrs);
  if (res) {
    MSG0("make-simple-lock: pthread_mutexattr_init returned error\n");
    goto err;
  }
  res = pthread_mutexattr_settype(&attrs, PTHREAD_MUTEX_ERRORCHECK);
  if (res) {
    MSG0("make-simple-lock: pthread_mutexattr_settype returned error\n");
    goto err;
  }
  res = pthread_mutex_init(&slock->mutex, &attrs);
  if (res) {
    MSG0("make-simple-lock: pthread_mutex_init returned error\n");
    goto err;
  }

  slock->owner = 0;

  lock->handle = slock;

  return OK;

 err:
  MSG0("make-simple-lock: error creating mutex\n");
  free(slock);
  return GENERAL_ERROR;
}


/* 23 */
D primitive_destroy_simple_lock(D l)
{
  CONTAINER  *lock = (CONTAINER *)l;
  SIMPLELOCK *slock;

  assert(lock != NULL);
  assert(lock->handle != NULL);

  slock = lock->handle;
  if (pthread_mutex_destroy(&slock->mutex)) {
    MSG0("destroy-simple-lock: pthread_mutex_destroy returned error\n");
    return GENERAL_ERROR;
  }
  lock->handle = NULL;
  free(slock);
  return OK;
}


/* 24 */
D primitive_owned_simple_lock(D l)
{
  CONTAINER   *lock = (CONTAINER *)l;
  SIMPLELOCK  *slock;
  TEB         *teb;

  assert(lock != NULL);
  assert(lock->handle != NULL);

  teb = get_teb();
  slock = lock->handle;

  if (slock->owner == teb)
    return(I(1));        // owned
  else
    return(I(0));        // not owned
}


/* 25 */
D primitive_owned_recursive_lock(D l)
{
  CONTAINER      *lock = (CONTAINER *)l;
  RECURSIVELOCK  *rlock;
  TEB            *teb;

  assert(lock != NULL);
  assert(lock->handle != NULL);

  teb = get_teb();
  rlock = lock->handle;

  if (rlock->owner == teb && rlock->count)
    return(I(1));     // owned
  else
    return(I(0));     // not owned
}


/* 26 */
D primitive_make_semaphore(D l, D n, D i, D m)
{
  CONTAINER  *lock = (CONTAINER *)l;
  ZINT        zinitial = (ZINT)i;
  ZINT        zmax = (ZINT)m;
  SEMAPHORE  *semaphore;
  ZINT        initial = zinitial >> 2;
  ZINT        max   = zmax >> 2;

  ignore(n);

  assert(lock != NULL);
  assert(IS_ZINT(zinitial));
  assert(IS_ZINT(zmax));

#ifdef HAVE_POSIX_SEMAPHORES
  if (max > SEM_VALUE_MAX) {
    MSG0("make-semaphore: max value exceeds system capabilities\n");
    return GENERAL_ERROR;
  }
#endif

  semaphore = (SEMAPHORE *)malloc(sizeof(SEMAPHORE));
  if (semaphore == NULL) {
    MSG0("make-semaphore: malloc failed\n");
    return GENERAL_ERROR;
  }

#ifdef HAVE_POSIX_SEMAPHORES
  int res = sem_init(&semaphore->semaphore, 0, initial);
  if (res) {
    MSG0("make-semaphore: sem_init returned error\n");
    free(semaphore);
    return GENERAL_ERROR;
  }
#else
  if (pthread_mutex_init(&semaphore->mutex, NULL)
      || pthread_cond_init(&semaphore->cond, NULL)) {
    MSG0("make-semaphore: error initializing OS objects\n");
    return GENERAL_ERROR;
  }

  semaphore->count = initial;
  semaphore->max_count = max;
#endif

  lock->handle = semaphore;

  return OK;
}


/* 27 */
D primitive_destroy_semaphore(D l)
{
  CONTAINER  *lock = (CONTAINER *)l;
  SEMAPHORE  *semaphore;

  assert(lock != NULL);
  assert(lock->handle != NULL);

  semaphore = lock->handle;

#ifdef HAVE_POSIX_SEMAPHORES
  if (sem_destroy(&semaphore->semaphore)) {
    MSG0("destroy-semaphore: sem_destroy returned errir\n");
    return GENERAL_ERROR;
  }
#else
  if (pthread_mutex_destroy(&semaphore->mutex)
      || pthread_cond_destroy(&semaphore->cond)) {
    MSG0("destroy-semaphore: error destroying OS objects\n");
    return GENERAL_ERROR;
  }
#endif

  free(semaphore);
  return OK;
}


/* 28 */
D primitive_make_notification(D n, D s)
{
  CONTAINER     *notif = (CONTAINER *)n;
  NOTIFICATION  *notification;

  ignore(s);
  assert(notif != NULL);

  notification = (NOTIFICATION *)malloc(sizeof(NOTIFICATION));
  if (notification == NULL) {
    MSG0("make-notification: malloc returned error\n");
    return GENERAL_ERROR;
  }
  if (pthread_cond_init(&notification->cond, NULL)) {
    MSG0("make-notification: error creating condition variable\n");
    free(notification);
    return GENERAL_ERROR;
  }
  notif->handle = notification;
  return OK;
}


/* 29 */
D primitive_destroy_notification(D n)
{
  CONTAINER     *notif = (CONTAINER *)n;
  NOTIFICATION  *notification;

  assert(notif != NULL);
  assert(notif->handle != NULL);

  notification = notif->handle;
  if (pthread_cond_destroy(&notification->cond)) {
    MSG0("destroy-notification: error destroying condition variable\n");
    return GENERAL_ERROR;
  }
  free(notification);
  return OK;
}


/* 30 */
void primitive_sleep(D m)
{
  ZINT  zmilsecs = (ZINT)m;
  DWORD milsecs = zmilsecs >> 2;

  assert(IS_ZINT(zmilsecs));
  sleep((milsecs + 999) / 1000);
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
D primitive_allocate_thread_variable(D v)
{
  size_t variable_offset, size, limit;

  pthread_mutex_lock(&tlv_vector_list_lock);

  // Get offset into to TLV vector for the new variable
  variable_offset = TLV_vector_offset;

  // increment offset for the next new variable
  TLV_vector_offset++;

  trace_tlv("Allocating variable at offset %"PRIxPTR, variable_offset);

  // First check if we need to grow the TLV vectors
  size = (size_t)(default_tlv_vector[1]) >> 2;
  limit = size + 2;
  if (variable_offset >= limit)
    grow_all_tlv_vectors(size+size);  // double the size each time we grow

  // Put the variable's default value in the default TLV vector
  default_tlv_vector[variable_offset] = v;

  // Update all the active thread TLV vectors with the default value
  update_tlv_vectors(variable_offset, v);

  // Finished
  pthread_mutex_unlock(&tlv_vector_list_lock);

  // return the offset into the TLV vector (an integer, not a pointer)
  return((void *)variable_offset);
}


/* 34 */
D primitive_read_thread_variable(D h)
{
  TLV_VECTOR   tlv_vector;
  D            value;
  uintptr_t    offset;

  // The variable handle is the byte offset where the variable's value is
  // stored in the TLV.
  offset = (uintptr_t)h;
  tlv_vector = get_tlv_vector();

  value = tlv_vector[offset];

  trace_tlv("Reading offset %"PRIxPTR" from vector %p: %p", offset, tlv_vector, value);

  return value;
}


/* 35 */

static void primitive_write_thread_variable_internal(void)
{
  do {
    if (atomic_decrement(&tlv_writer_counter) < 0) {
      pthread_mutex_lock(&tlv_vector_list_lock);
      pthread_mutex_unlock(&tlv_vector_list_lock);
    }
  } while (atomic_increment(&tlv_writer_counter) < 0);
}

D primitive_write_thread_variable(D h, D nv)
{
  TLV_VECTOR   tlv_vector;
  uintptr_t    offset;

  // Wait until there are no other writers
  if (atomic_increment(&tlv_writer_counter) < 0) {
    primitive_write_thread_variable_internal();
  }

  // The variable handle is the byte offset where the variable's value is
  // stored in the TLV.
  offset = (uintptr_t)h;
  tlv_vector = get_tlv_vector();

  trace_tlv("Writing offset %"PRIxPTR" in vector %p: %p", offset, tlv_vector, nv);

  // Store the actual value
  tlv_vector[offset] = nv;

  // Indicate that the write has finished
  atomic_decrement(&tlv_writer_counter);

  return(nv);
}


/* 36 */
D primitive_initialize_current_thread(D t, DBOOL synchronize)
{
  DTHREAD     *thread = (DTHREAD *)t;
  THREAD      *rthread;

  ignore(synchronize);
  assert(thread != NULL);

  rthread = (THREAD*)(thread->handle2);

  trace_threads("Initializing current thread %p", t);

  // Put the thread object and handle in the TEB for later use
  set_current_thread(thread);
  set_current_thread_handle((void *)rthread->tid);

  // Create and register the TLV vector
  setup_tlv_vector(thread);

  return t;
}


/* 36a */
D primitive_initialize_special_thread(D t)
{
  trace_threads("Initializing special thread %p", t);
  return primitive_initialize_current_thread(t, 0);
}


/* 37 */
D primitive_unlock_simple_lock(D l)
{
  CONTAINER  *lock = (CONTAINER *)l;
  SIMPLELOCK *slock;

  assert(lock != NULL);
  assert(lock->handle != NULL);

  slock = lock->handle;

  if (slock->owner == 0) {
    /* nothing to do - lock already released */
    return OK;
  }

  slock->owner = 0;

  if (pthread_mutex_unlock(&slock->mutex)) {
    MSG0("unlock-simple-lock: error releasing mutex\n");
    return GENERAL_ERROR;
  }

  return OK;
}


/* 38 */
D primitive_unlock_recursive_lock(D l)
{
  CONTAINER *lock = (CONTAINER *)l;
  RECURSIVELOCK *rlock;

  assert(lock != NULL);
  assert(lock->handle != NULL);

  rlock = lock->handle;

  if (rlock->count == 0) {
    /* nothing to do - lock already released */
    return OK;
  }

  rlock->owner = 0;
  rlock->count = 0;

  if (pthread_mutex_unlock(&rlock->mutex)) {
    MSG0("unlock-recursive-lock: error signalling cond\n");
    return GENERAL_ERROR;
  }
  return OK;
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
static int priority_map(int dylan_priority)
{
  return dylan_priority;
  /*
  int priority;

  if (dylan_priority < 0)
    if (dylan_priority < -1249)
      priority = THREAD_PRIORITY_IDLE;
    else
      priority = (dylan_priority - 250) / 500;
  else
    if (dylan_priority > 1249)
      priority = THREAD_PRIORITY_TIME_CRITICAL;
    else
      priority = (dylan_priority + 250) / 500;

  return (priority);
  */
}
