#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdbool.h>
#include <unistd.h>
#include <errno.h>
#include <pthread.h>
#include <semaphore.h>
#include <sys/time.h>

#include "llvm-runtime.h"
#include "mm.h"

// The BDW GC wants to wrap pthreads functions
#if defined(GC_USE_BOEHM)
#  include <gc/gc.h>
#endif

/* Error codes returned by the primitives. These correspond to the codes
 *  defined in dylan/return-codes.dylan
 */
#define OK              I(0)
#define GENERAL_ERROR   I(-1)
#define TIMEOUT         I(1)
#define NOT_LOCKED      I(2)
#define ALREADY_LOCKED  I(2)
#define COUNT_EXCEEDED  I(3)
#define CREATE_ERROR    I(1)
#define PRIORITY_ERROR  I(2)

#define RETURN_IF_ERROR(e, code)                \
  do {                                          \
    int rc = e;                                 \
    if (rc != 0) {                              \
      fprintf(stderr, "%s: %s returned %d\n",   \
              __func__, #e, rc);                \
      return code;                              \
    }                                           \
  } while (0)

#define UNLOCK_RETURN_IF_ERROR(e, mutexp, code) \
  do {                                          \
    int rc = e;                                 \
    if (rc != 0) {                              \
      fprintf(stderr, "%s: %s returned %d\n",   \
              __func__, #e, rc);                \
      pthread_mutex_unlock(mutexp);             \
      return code;                              \
    }                                           \
  } while (0)

/* Thread state flags
 */
#define COMPLETED       (1 << 0)
#define WANT_TO_JOIN    (1 << 1)
#define JOINED          (1 << 2)

typedef struct thread {
  pthread_t tid;
  uintptr_t state;
} THREAD;

// Mutex/condition variable pair
typedef struct mc {
  pthread_mutex_t mutex;
  pthread_cond_t  cond;
} mc;

typedef struct semaphore {
  struct mc mc;
  intptr_t count;
  intptr_t max_count;
} SEMAPHORE;

typedef struct simple_lock {
  struct mc mc;
  pthread_t owner;
  bool locked;
} SIMPLELOCK;

typedef struct recursive_lock {
  struct mc mc;
  pthread_t owner;
  intptr_t recursion_count;
} RECURSIVELOCK;

typedef struct notification {
  struct mc mc;
} NOTIFICATION;


/// Thread variables

static pthread_mutex_t tlv_initializations_lock = PTHREAD_MUTEX_INITIALIZER;

void primitive_register_thread_variable_initializer(dylan_value value, D function)
{
  pthread_mutex_lock(&tlv_initializations_lock);

  // Add the value and function to %tlv-initializations
  struct KLsimple_object_vectorGVKd *initializations
    = (struct KLsimple_object_vectorGVKd *) Ptlv_initializations;
  DSINT size = ((intptr_t) initializations->size) >> 2;

  DSINT new_size = Ptlv_initializations_cursor + 2;
  if (new_size > size) {
    Ptlv_initializations
      = primitive_grow_vector(Ptlv_initializations, 32);
    initializations
      = (struct KLsimple_object_vectorGVKd *) Ptlv_initializations;
  }

  initializations->vector_element[Ptlv_initializations_cursor++] = value;
  initializations->vector_element[Ptlv_initializations_cursor++] = function;

  pthread_mutex_unlock(&tlv_initializations_lock);

  // Perform the initialization and any others pending in this thread
  primitive_initialize_thread_variables();
}

void primitive_initialize_thread_variables(void)
{
  // Initialize thread-local variables to their init values
  // and register them as GC roots
  pthread_mutex_lock(&tlv_initializations_lock);

  struct KLsimple_object_vectorGVKd *initializations
    = (struct KLsimple_object_vectorGVKd *) Ptlv_initializations;
  DSINT size = Ptlv_initializations_cursor;

  char *range_start = NULL, *range_end = NULL;

  // Ensure that the TEB is registered
  if (Ptlv_initializations_local_cursor == 0) {
    range_start = (char *) &Pteb;
    range_end = (char *) (&Pteb + 1);
  }

  // Locate each TLV, initialize it, and register it as (part of a
  // range of) GC roots
  for (DSINT i = Ptlv_initializations_local_cursor; i < size; i += 2) {
    dylan_value *(*function)(void)
      = (dylan_value *(*)(void)) initializations->vector_element[i + 1];
    dylan_value *tlv = (*function)();
    *tlv = initializations->vector_element[i];

    // Does this address extend the current range?
    if ((char *) tlv == range_end) {
      range_end = (char *) (tlv + 1);
    }
    else {
#if defined(GC_USE_BOEHM)
      if (range_start != NULL) {
        GC_add_roots(range_start, range_end);
      }
#endif
      range_start = (char *) tlv;
      range_end = (char *) (tlv + 1);
    }
  }

#if defined(GC_USE_BOEHM)
  GC_add_roots(range_start, range_end);
#endif
  Ptlv_initializations_local_cursor = size;

  pthread_mutex_unlock(&tlv_initializations_lock);
}

void deinitialize_thread_variables(void)
{
  // Deregister TLV areas as GC roots
  pthread_mutex_lock(&tlv_initializations_lock);

  struct KLsimple_object_vectorGVKd *initializations
    = (struct KLsimple_object_vectorGVKd *) Ptlv_initializations;
  DSINT size = Ptlv_initializations_cursor;

  // Ensure that the TEB is deregistered
  char *range_start = (char *) &Pteb, *range_end = (char *) (&Pteb + 1);

  // Locate each TLV and deregister it as (part of a range of) GC
  // roots
  for (DSINT i = 0; i < size; i += 2) {
    dylan_value *(*function)(void)
      = (dylan_value *(*)(void)) initializations->vector_element[i + 1];
    dylan_value *tlv = (*function)();

    // Does this address extend the current range?
    if ((char *) tlv == range_end) {
      range_end = (char *) (tlv + 1);
    }
    else {
#if defined(GC_USE_BOEHM)
      GC_remove_roots(range_start, range_end);
#endif
      range_start = (char *) tlv;
      range_end = (char *) (tlv + 1);
    }
  }

#if defined(GC_USE_BOEHM)
  GC_remove_roots(range_start, range_end);
#endif

  pthread_mutex_unlock(&tlv_initializations_lock);
}


/// Thread operations

static pthread_mutex_t thread_join_lock = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t thread_exit_event = PTHREAD_COND_INITIALIZER;

static inline void thread_set_state(dylan_value t, uintptr_t state)
{
  struct KLthreadGYthreadsVdylan *thread
    = (struct KLthreadGYthreadsVdylan *) t;
  THREAD *rthread = (THREAD *) thread->handle1;
  rthread->state = state;
}

static inline uintptr_t thread_get_state(dylan_value t)
{
  struct KLthreadGYthreadsVdylan *thread
    = (struct KLthreadGYthreadsVdylan *) t;
  THREAD *rthread = (THREAD *) thread->handle1;
  return rthread->state;
}

void *get_current_thread_handle(void)
{
  return (void *) Pteb.teb_current_thread_handle;
}

static void *trampoline(void *arg)
{
  struct KLthreadGYthreadsVdylan *thread
    = (struct KLthreadGYthreadsVdylan *) arg;

  // FIXME set thread name
  // FIXME set thread priority

  EstablishDylanExceptionHandlers();

  primitive_initialize_thread_variables();

  Pteb.teb_current_thread_handle = (D) pthread_self();

  // Execute the thread function using the Dylan trampoline
  dylan_value result = call_dylan_function(thread->handle2, 0);

  // Drop references to the the <thread> object
  Pteb.teb_current_thread = NULL;

  deinitialize_thread_variables();

  RemoveDylanExceptionHandlers();

  // Mark the thread as completed
  pthread_mutex_lock(&thread_join_lock);
  thread_set_state(thread, thread_get_state(thread) | COMPLETED);
  pthread_cond_broadcast(&thread_exit_event);
  pthread_mutex_unlock(&thread_join_lock);

  return result;
}

// primitive-make-thread
dylan_value primitive_make_thread(dylan_value t, dylan_value n, dylan_value p, dylan_value f, DBOOL s)
{
  struct KLthreadGYthreadsVdylan *thread
    = (struct KLthreadGYthreadsVdylan *) t;

  THREAD *rthread = MMAllocMisc(sizeof(THREAD));
  rthread->state = 0;

  thread->handle1 = rthread;    // runtime thread object
  thread->handle2 = f;          // trampoline function

  pthread_attr_t attr;
  RETURN_IF_ERROR(pthread_attr_init(&attr), CREATE_ERROR);
  RETURN_IF_ERROR(pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED),
                  CREATE_ERROR);
  RETURN_IF_ERROR(pthread_create(&rthread->tid, &attr, trampoline, thread),
                  CREATE_ERROR);
  RETURN_IF_ERROR(pthread_attr_destroy(&attr), CREATE_ERROR);

  return OK;
}

// primitive-destroy-thread
dylan_value primitive_destroy_thread(dylan_value t)
{
  struct KLthreadGYthreadsVdylan *thread
    = (struct KLthreadGYthreadsVdylan *) t;
  THREAD *rthread = (THREAD *) thread->handle1;
  MMFreeMisc(rthread, sizeof(THREAD));
  return OK;
}

// primitive-thread-join-single
dylan_value primitive_thread_join_single(dylan_value t)
{
  RETURN_IF_ERROR(pthread_mutex_lock(&thread_join_lock), GENERAL_ERROR);

  // Check the thread state; if it is already marked as wanting to be
  // joined, or it has already been joined, then return an error
  uintptr_t state = thread_get_state(t);
  if ((state & (WANT_TO_JOIN | JOINED)) != 0) {
    pthread_mutex_unlock(&thread_join_lock);
    return GENERAL_ERROR;
  }

  // Mark the thread as wanting to be joined, and wait for it to be
  // marked completed
  thread_set_state(t, state | WANT_TO_JOIN);
  while (((state = thread_get_state(t)) & COMPLETED) == 0) {
    UNLOCK_RETURN_IF_ERROR(pthread_cond_wait(&thread_exit_event,
                                             &thread_join_lock),
                           &thread_join_lock, GENERAL_ERROR);
  }

  // Mark as joined now
  thread_set_state(t, (state & ~WANT_TO_JOIN) | JOINED);
  RETURN_IF_ERROR(pthread_mutex_unlock(&thread_join_lock), GENERAL_ERROR);

  return OK;
}

// primitive-thread-join-multiple
dylan_value primitive_thread_join_multiple(dylan_value v)
{
  struct KLsimple_object_vectorGVKd *threads
    = (struct KLsimple_object_vectorGVKd *) v;
  DSINT size = ((intptr_t) threads->size) >> 2;

  RETURN_IF_ERROR(pthread_mutex_lock(&thread_join_lock), GENERAL_ERROR);

  for (long i = 0; i < size; ++i) {
    // Check the thread state; if it is already marked as wanting to be
    // joined, or it has already been joined, then return an error
    uintptr_t state = thread_get_state(threads->vector_element[i]);
    if ((state & (WANT_TO_JOIN | JOINED)) != 0) {
      pthread_mutex_unlock(&thread_join_lock);
      return GENERAL_ERROR;
    }
  }

  // Mark each thread as wanting to be joined
  for (long i = 0; i < size; ++i) {
    dylan_value t = threads->vector_element[i];
    thread_set_state(t, thread_get_state(t) | WANT_TO_JOIN);
  }

  // Wait for a thread to exit
  dylan_value joined_thread = NULL;
  while (joined_thread == NULL) {
    UNLOCK_RETURN_IF_ERROR(pthread_cond_wait(&thread_exit_event,
                                             &thread_join_lock),
                           &thread_join_lock, GENERAL_ERROR);

    // Look for a thread marked as COMPLETED
    for (long i = 0; i < size; ++i) {
      dylan_value t = threads->vector_element[i];
      uintptr_t state = thread_get_state(t);
      if ((state & COMPLETED) != 0) {
        joined_thread = t;
        thread_set_state(t, state | JOINED);
        break;
      }
    }
  }

  // Remove the WANT_TO_JOIN state flag
  for (long i = 0; i < size; ++i) {
    dylan_value t = threads->vector_element[i];
    thread_set_state(t, thread_get_state(t) & ~WANT_TO_JOIN);
  }

  RETURN_IF_ERROR(pthread_mutex_unlock(&thread_join_lock), GENERAL_ERROR);

  return joined_thread;
}

// primitive-detach-thread
void primitive_detach_thread(dylan_value t)
{
  // Nothing to do, thread was created with PTHREAD_CREATE_DETACHED
}

// primitive-thread-yield
void primitive_thread_yield(void)
{
  sched_yield();
}

// primitive-current-thread
dylan_value primitive_current_thread(void)
{
  return Pteb.teb_current_thread;
}

// primitive-initialize-current-thread
void primitive_initialize_current_thread(dylan_value t, DBOOL synchronize)
{
  struct KLthreadGYthreadsVdylan *thread
    = (struct KLthreadGYthreadsVdylan *) t;
  thread->handle2 = NULL;      // Drop reference to trampoline closure

  Pteb.teb_current_thread = t;
}

// primitive-initialize-special-thread
void primitive_initialize_special_thread(dylan_value t)
{
  Pteb.teb_current_thread = t;
}

// primitive-sleep
void primitive_sleep(dylan_value m)
{
  long milsecs = ((intptr_t) m) >> 2;
  struct timespec req, rem;

  req.tv_sec = milsecs / 1000;
  req.tv_nsec = (milsecs % 1000) * 1000000;
  while (nanosleep(&req, &rem)) {
    if (errno == EINTR) {
      req = rem;
    } else {
      return;
    }
  }
}


/// Semaphores

static dylan_value mc_init(mc *mc)
{
  RETURN_IF_ERROR(pthread_mutex_init(&mc->mutex, NULL), GENERAL_ERROR);
  RETURN_IF_ERROR(pthread_cond_init(&mc->cond, NULL), GENERAL_ERROR);
  return OK;
}

static dylan_value mc_destroy(mc *mc)
{
  RETURN_IF_ERROR(pthread_cond_destroy(&mc->cond), GENERAL_ERROR);
  RETURN_IF_ERROR(pthread_mutex_destroy(&mc->mutex), GENERAL_ERROR);
  return OK;
}

// primitive-make-semaphore
dylan_value primitive_make_semaphore(dylan_value l, dylan_value n, dylan_value i, dylan_value m)
{
  struct KLsemaphoreGYthreadsVdylan *lock
    = (struct KLsemaphoreGYthreadsVdylan *) l;
  SEMAPHORE *semaphore = (SEMAPHORE *) MMAllocMisc(sizeof(SEMAPHORE));
  if (semaphore == NULL) {
    return GENERAL_ERROR;
  }

  dylan_value rc = mc_init(&semaphore->mc);
  if (rc != OK) {
    return rc;
  }

  semaphore->count = ((intptr_t) i) >> 2;
  semaphore->max_count = ((intptr_t) m) >> 2;

  lock->handle1 = (D) semaphore;

  return OK;
}

// primitive-release-semaphore
dylan_value primitive_release_semaphore(dylan_value l)
{
  struct KLsemaphoreGYthreadsVdylan *lock
    = (struct KLsemaphoreGYthreadsVdylan *) l;
  SEMAPHORE *semaphore = (SEMAPHORE *) lock->handle1;

  RETURN_IF_ERROR(pthread_mutex_lock(&semaphore->mc.mutex), GENERAL_ERROR);

  semaphore->count++;

  UNLOCK_RETURN_IF_ERROR(pthread_cond_signal(&semaphore->mc.cond),
                         &semaphore->mc.mutex, GENERAL_ERROR);
  RETURN_IF_ERROR(pthread_mutex_unlock(&semaphore->mc.mutex), GENERAL_ERROR);

  return OK;
}

static void compute_deadline(struct timespec *abstime, long ms)
{
  struct timeval tv;
  gettimeofday(&tv, NULL);
  abstime->tv_sec = tv.tv_sec + ms / 1000;
  abstime->tv_nsec = tv.tv_usec * 1000 + (ms % 1000) * 1000000;
  if (abstime->tv_nsec > 1000000000L) {
    abstime->tv_sec += abstime->tv_nsec / 1000000000L;
    abstime->tv_nsec = abstime->tv_nsec % 1000000000L;
  }
}

// primitive-wait-for-semaphore-timed
dylan_value primitive_wait_for_semaphore_timed(dylan_value l, dylan_value ms)
{
  struct KLsemaphoreGYthreadsVdylan *lock
    = (struct KLsemaphoreGYthreadsVdylan *) l;
  SEMAPHORE *semaphore = (SEMAPHORE *) lock->handle1;

  struct timespec deadline;
  compute_deadline(&deadline, ((intptr_t) ms) >> 2);

  RETURN_IF_ERROR(pthread_mutex_lock(&semaphore->mc.mutex), GENERAL_ERROR);

  while (semaphore->count <= 0) {
    int rc = pthread_cond_timedwait(&semaphore->mc.cond,
                                    &semaphore->mc.mutex,
                                    &deadline);
    if (rc != 0) {
      if (rc == ETIMEDOUT) {
        pthread_mutex_unlock(&semaphore->mc.mutex);
        return TIMEOUT;
      } else {
        return GENERAL_ERROR;
      }
    }
  }

  semaphore->count--;

  RETURN_IF_ERROR(pthread_mutex_unlock(&semaphore->mc.mutex), GENERAL_ERROR);

  return OK;
}

// primitive-wait-for-semaphore
dylan_value primitive_wait_for_semaphore(dylan_value l)
{
  struct KLsemaphoreGYthreadsVdylan *lock
    = (struct KLsemaphoreGYthreadsVdylan *) l;
  SEMAPHORE *semaphore = (SEMAPHORE *) lock->handle1;

  RETURN_IF_ERROR(pthread_mutex_lock(&semaphore->mc.mutex), GENERAL_ERROR);

  while (semaphore->count <= 0) {
    RETURN_IF_ERROR(pthread_cond_wait(&semaphore->mc.cond, 
				      &semaphore->mc.mutex),
                    GENERAL_ERROR);
  }

  semaphore->count--;

  RETURN_IF_ERROR(pthread_mutex_unlock(&semaphore->mc.mutex), GENERAL_ERROR);

  return OK;
}

// primitive-destroy-semaphore
dylan_value primitive_destroy_semaphore(dylan_value l)
{
  struct KLsemaphoreGYthreadsVdylan *lock
    = (struct KLsemaphoreGYthreadsVdylan *) l;
  SEMAPHORE *semaphore = (SEMAPHORE *) lock->handle1;

  dylan_value rc = mc_destroy(&semaphore->mc);

  MMFreeMisc(semaphore, sizeof(SEMAPHORE));

  return rc;
}


/// Recursive locks

// primitive-make-recursive-lock
dylan_value primitive_make_recursive_lock(dylan_value l, dylan_value n)
{
  struct KLrecursive_lockGYthreadsVdylan *lock
    = (struct KLrecursive_lockGYthreadsVdylan *) l;
  RECURSIVELOCK *rlock;

  rlock = MMAllocMisc(sizeof(RECURSIVELOCK));
  if (rlock == NULL) {
    return GENERAL_ERROR;
  }

  dylan_value rc = mc_init(&rlock->mc);
  if (rc != OK) {
    MMFreeMisc(rlock, sizeof(RECURSIVELOCK));
    return rc;
  }

  rlock->recursion_count = 0;

  lock->handle1 = (D) rlock;
  return OK;
}

// primitive-release-recursive-lock
dylan_value primitive_release_recursive_lock(dylan_value l)
{
  struct KLrecursive_lockGYthreadsVdylan *lock
    = (struct KLrecursive_lockGYthreadsVdylan *) l;
  RECURSIVELOCK *rlock = (RECURSIVELOCK *) lock->handle1;

  RETURN_IF_ERROR(pthread_mutex_lock(&rlock->mc.mutex), GENERAL_ERROR);

  if (rlock->recursion_count == 0) {
    pthread_mutex_unlock(&rlock->mc.mutex);
    return NOT_LOCKED;
  }

  pthread_t self = pthread_self();
  if (!pthread_equal(self, rlock->owner)) {
    pthread_mutex_unlock(&rlock->mc.mutex);
    return NOT_LOCKED;
  }

  if (--rlock->recursion_count == 0) {
    RETURN_IF_ERROR(pthread_cond_signal(&rlock->mc.cond), GENERAL_ERROR);
  }

  RETURN_IF_ERROR(pthread_mutex_unlock(&rlock->mc.mutex), GENERAL_ERROR);

  return OK;
}

// primitive-wait-for-recursive-lock
dylan_value primitive_wait_for_recursive_lock(dylan_value l)
{
  struct KLrecursive_lockGYthreadsVdylan *lock
    = (struct KLrecursive_lockGYthreadsVdylan *) l;
  RECURSIVELOCK *rlock = (RECURSIVELOCK *) lock->handle1;

  RETURN_IF_ERROR(pthread_mutex_lock(&rlock->mc.mutex), GENERAL_ERROR);

  pthread_t self = pthread_self();
  if (rlock->recursion_count != 0 && !pthread_equal(rlock->owner, self)) {
    // Wait until the lock is no longer owned
    while (rlock->recursion_count != 0) {
      UNLOCK_RETURN_IF_ERROR(pthread_cond_wait(&rlock->mc.cond,
					       &rlock->mc.mutex),
			     &rlock->mc.mutex, GENERAL_ERROR);
    }
  }

  // Claim the lock
  rlock->owner = self;
  ++rlock->recursion_count;

  RETURN_IF_ERROR(pthread_mutex_unlock(&rlock->mc.mutex), GENERAL_ERROR);

  return OK;
}

// primitive-wait-for-recursive-lock-timed
dylan_value primitive_wait_for_recursive_lock_timed(dylan_value l, dylan_value ms)
{
  struct KLrecursive_lockGYthreadsVdylan *lock
    = (struct KLrecursive_lockGYthreadsVdylan *) l;
  RECURSIVELOCK *rlock = (RECURSIVELOCK *) lock->handle1;

  struct timespec deadline;
  compute_deadline(&deadline, ((intptr_t) ms) >> 2);

  RETURN_IF_ERROR(pthread_mutex_lock(&rlock->mc.mutex), GENERAL_ERROR);

  pthread_t self = pthread_self();
  if (rlock->recursion_count != 0 && !pthread_equal(rlock->owner, self)) {
    // Wait until the lock is no longer owned
    while (rlock->recursion_count != 0) {
      int rc = pthread_cond_timedwait(&rlock->mc.cond, &rlock->mc.mutex,
				      &deadline);
      if (rc == ETIMEDOUT) {
	pthread_mutex_unlock(&rlock->mc.mutex);
	return TIMEOUT;
      } else if (rc != 0) {
	pthread_mutex_unlock(&rlock->mc.mutex);
        return GENERAL_ERROR;
      }
    }
  }

  // Claim the lock
  rlock->owner = self;
  ++rlock->recursion_count;

  RETURN_IF_ERROR(pthread_mutex_unlock(&rlock->mc.mutex), GENERAL_ERROR);

  return OK;
}

// primitive-owned-recursive-lock
dylan_value primitive_owned_recursive_lock(dylan_value l)
{
  struct KLrecursive_lockGYthreadsVdylan *lock
    = (struct KLrecursive_lockGYthreadsVdylan *) l;
  RECURSIVELOCK *rlock = (RECURSIVELOCK *) lock->handle1;

  RETURN_IF_ERROR(pthread_mutex_lock(&rlock->mc.mutex), GENERAL_ERROR);

  dylan_value res = I(0);                 // not owned
  if (rlock->recursion_count != 0 
      && pthread_equal(rlock->owner, pthread_self())) {
    res = I(1);                 // owned
  }

  RETURN_IF_ERROR(pthread_mutex_unlock(&rlock->mc.mutex), GENERAL_ERROR);

  return res;
}

// primitive-destroy-recursive-lock
dylan_value primitive_destroy_recursive_lock(dylan_value l)
{
  struct KLrecursive_lockGYthreadsVdylan *lock
    = (struct KLrecursive_lockGYthreadsVdylan *) l;
  RECURSIVELOCK *rlock = (RECURSIVELOCK *) lock->handle1;

  dylan_value rc = mc_destroy(&rlock->mc);

  MMFreeMisc(rlock, sizeof(RECURSIVELOCK));

  return rc;
}


/// Simple locks

// primitive-make-simple-lock
dylan_value primitive_make_simple_lock(dylan_value l, dylan_value n)
{
  struct KLsimple_lockGYthreadsVdylan *lock
    = (struct KLsimple_lockGYthreadsVdylan *) l;

  SIMPLELOCK *slock = MMAllocMisc(sizeof(SIMPLELOCK));
  if (slock == NULL) {
    return GENERAL_ERROR;
  }

  dylan_value rc = mc_init(&slock->mc);
  if (rc != OK) {
    MMFreeMisc(slock, sizeof(SIMPLELOCK));
    return rc;
  }
  slock->locked = false;

  lock->handle1 = (D) slock;
  return OK;
}

// primitive-release-simple-lock
dylan_value primitive_release_simple_lock(dylan_value l)
{
  struct KLsimple_lockGYthreadsVdylan *lock
    = (struct KLsimple_lockGYthreadsVdylan *) l;
  SIMPLELOCK *slock = (SIMPLELOCK *) lock->handle1;

  RETURN_IF_ERROR(pthread_mutex_lock(&slock->mc.mutex), GENERAL_ERROR);

  if (!slock->locked) {
    pthread_mutex_unlock(&slock->mc.mutex);
    return NOT_LOCKED;
  }

  pthread_t self = pthread_self();
  if (!pthread_equal(self, slock->owner)) {
    pthread_mutex_unlock(&slock->mc.mutex);
    return NOT_LOCKED;
  }

  slock->locked = false;
  UNLOCK_RETURN_IF_ERROR(pthread_cond_signal(&slock->mc.cond),
                         &slock->mc.mutex, GENERAL_ERROR);

  RETURN_IF_ERROR(pthread_mutex_unlock(&slock->mc.mutex), GENERAL_ERROR);

  return OK;
}

// primitive-wait-for-simple-lock
dylan_value primitive_wait_for_simple_lock(dylan_value l)
{
  struct KLsimple_lockGYthreadsVdylan *lock
    = (struct KLsimple_lockGYthreadsVdylan *) l;
  SIMPLELOCK *slock = (SIMPLELOCK *) lock->handle1;

  RETURN_IF_ERROR(pthread_mutex_lock(&slock->mc.mutex), GENERAL_ERROR);

  // Wait until the lock is no longer owned
  pthread_t self = pthread_self();
  while (slock->locked) {
    if (pthread_equal(slock->owner, self)) {
      pthread_mutex_unlock(&slock->mc.mutex);
      return ALREADY_LOCKED;
    }
    UNLOCK_RETURN_IF_ERROR(pthread_cond_wait(&slock->mc.cond, &slock->mc.mutex),
                           &slock->mc.mutex, GENERAL_ERROR);
  }

  // Claim it
  slock->owner = self;
  slock->locked = true;

  RETURN_IF_ERROR(pthread_mutex_unlock(&slock->mc.mutex), GENERAL_ERROR);

  return OK;
}

// primitive-wait-for-simple-lock-timed
dylan_value primitive_wait_for_simple_lock_timed(dylan_value l, dylan_value ms)
{
  struct KLsimple_lockGYthreadsVdylan *lock
    = (struct KLsimple_lockGYthreadsVdylan *) l;
  SIMPLELOCK *slock = (SIMPLELOCK *) lock->handle1;

  struct timespec deadline;
  compute_deadline(&deadline, ((intptr_t) ms) >> 2);

  RETURN_IF_ERROR(pthread_mutex_lock(&slock->mc.mutex), GENERAL_ERROR);

  while (slock->locked) {
    int rc = pthread_cond_timedwait(&slock->mc.cond, &slock->mc.mutex, &deadline);
    if (rc != 0) {
      if (rc == ETIMEDOUT) {
        pthread_mutex_unlock(&slock->mc.mutex);
        return TIMEOUT;
      } else {
        fprintf(stderr, "%s: pthread_cond_timedwait returned %s\n",
                __func__, sys_errlist[rc]);
        return GENERAL_ERROR;
      }
    }
  }

  // Claim it
  slock->owner = pthread_self();
  slock->locked = true;

  RETURN_IF_ERROR(pthread_mutex_unlock(&slock->mc.mutex), GENERAL_ERROR);

  return OK;
}

// primitive-owned-simple-lock
dylan_value primitive_owned_simple_lock(dylan_value l)
{
  struct KLsimple_lockGYthreadsVdylan *lock
    = (struct KLsimple_lockGYthreadsVdylan *) l;
  SIMPLELOCK *slock = (SIMPLELOCK *) lock->handle1;

  RETURN_IF_ERROR(pthread_mutex_lock(&slock->mc.mutex), GENERAL_ERROR);

  dylan_value res = I(0);                 // not owned
  if (slock->locked && pthread_equal(slock->owner, pthread_self())) {
    res = I(1);                 // owned
  }

  RETURN_IF_ERROR(pthread_mutex_unlock(&slock->mc.mutex), GENERAL_ERROR);

  return res;
}

// primitive-destroy-simple-lock
dylan_value primitive_destroy_simple_lock(dylan_value l)
{
  struct KLsimple_lockGYthreadsVdylan *lock
    = (struct KLsimple_lockGYthreadsVdylan *) l;
  SIMPLELOCK *slock = (SIMPLELOCK *) lock->handle1;

  dylan_value rc = mc_destroy(&slock->mc);

  MMFreeMisc(slock, sizeof(SIMPLELOCK));

  return rc;
}


/// Notification

// primitive-make-notification
dylan_value primitive_make_notification(dylan_value n, dylan_value s)
{
  struct KLnotificationGYthreadsVdylan *notif
    = (struct KLnotificationGYthreadsVdylan *) n;
  NOTIFICATION *notification;

  notification = MMAllocMisc(sizeof(NOTIFICATION));
  if (notification == NULL) {
    return GENERAL_ERROR;
  }

  dylan_value rc = mc_init(&notification->mc);
  if (rc != OK) {
    MMFreeMisc(notification, sizeof(NOTIFICATION));
    return rc;
  }

  notif->handle1 = notification;
  return OK;
}

// primitive-release-notification
dylan_value primitive_release_notification(dylan_value n, dylan_value l)
{
  struct KLnotificationGYthreadsVdylan *notif
    = (struct KLnotificationGYthreadsVdylan *) n;
  NOTIFICATION *notification = (NOTIFICATION *) notif->handle1;

  RETURN_IF_ERROR(pthread_mutex_lock(&notification->mc.mutex), GENERAL_ERROR);

#if 0
  if (primitive_owned_simple_lock(l) == I(0)) {
    return NOT_LOCKED;
  }
#endif

  UNLOCK_RETURN_IF_ERROR(pthread_cond_signal(&notification->mc.cond),
			 &notification->mc.mutex, GENERAL_ERROR);

  RETURN_IF_ERROR(pthread_mutex_unlock(&notification->mc.mutex), 
		  GENERAL_ERROR);
  return OK;
}

// primitive-release-all-notification
dylan_value primitive_release_all_notification(dylan_value n, dylan_value l)
{
  struct KLnotificationGYthreadsVdylan *notif
    = (struct KLnotificationGYthreadsVdylan *) n;
  NOTIFICATION *notification = (NOTIFICATION *) notif->handle1;

  RETURN_IF_ERROR(pthread_mutex_lock(&notification->mc.mutex), GENERAL_ERROR);

#if 0
  if (primitive_owned_simple_lock(l) == I(0)) {
    return NOT_LOCKED;
  }
#endif

  UNLOCK_RETURN_IF_ERROR(pthread_cond_broadcast(&notification->mc.cond),
			 &notification->mc.mutex, GENERAL_ERROR);

  RETURN_IF_ERROR(pthread_mutex_unlock(&notification->mc.mutex),
		  GENERAL_ERROR);

  return OK;
}

// primitive-wait-for-notification
dylan_value primitive_wait_for_notification(dylan_value n, dylan_value l)
{
  struct KLnotificationGYthreadsVdylan *notif
    = (struct KLnotificationGYthreadsVdylan *) n;
  NOTIFICATION *notification = (NOTIFICATION *) notif->handle1;

  RETURN_IF_ERROR(pthread_mutex_lock(&notification->mc.mutex), GENERAL_ERROR);

  dylan_value rc = primitive_release_simple_lock(l);
  if (rc != OK) {
    pthread_mutex_unlock(&notification->mc.mutex);
    return rc;
  }

  if (pthread_cond_wait(&notification->mc.cond,
			&notification->mc.mutex) != 0) {
    primitive_wait_for_simple_lock(l);
    pthread_mutex_unlock(&notification->mc.mutex);
    return GENERAL_ERROR;
  }

  rc = primitive_wait_for_simple_lock(l);

  RETURN_IF_ERROR(pthread_mutex_unlock(&notification->mc.mutex),
		  GENERAL_ERROR);

  return rc;
}

// primitive-wait-for-notification-timed
dylan_value primitive_wait_for_notification_timed(dylan_value n, dylan_value l, dylan_value ms)
{
  struct KLnotificationGYthreadsVdylan *notif
    = (struct KLnotificationGYthreadsVdylan *) n;
  NOTIFICATION *notification = (NOTIFICATION *) notif->handle1;

  struct timespec deadline;
  compute_deadline(&deadline, ((intptr_t) ms) >> 2);

  RETURN_IF_ERROR(pthread_mutex_lock(&notification->mc.mutex), GENERAL_ERROR);

  dylan_value rc = primitive_release_simple_lock(l);
  if (rc != OK) {
    pthread_mutex_unlock(&notification->mc.mutex);
    return rc;
  }

  int ret = pthread_cond_timedwait(&notification->mc.cond,
				   &notification->mc.mutex,
				   &deadline);
  if (ret == ETIMEDOUT) {
    primitive_wait_for_simple_lock(l);
    pthread_mutex_unlock(&notification->mc.mutex);
    return TIMEOUT;
  } else if (ret != 0) {
    primitive_wait_for_simple_lock(l);
    pthread_mutex_unlock(&notification->mc.mutex);
    return GENERAL_ERROR;
  }

  rc = primitive_wait_for_simple_lock(l);

  RETURN_IF_ERROR(pthread_mutex_unlock(&notification->mc.mutex),
		  GENERAL_ERROR);

  return rc;
}

// primitive-destroy-notification
dylan_value primitive_destroy_notification(dylan_value n)
{
  struct KLnotificationGYthreadsVdylan *notif
    = (struct KLnotificationGYthreadsVdylan *) n;
  NOTIFICATION *notification = (NOTIFICATION*) notif->handle1;

  dylan_value rc = mc_destroy(&notification->mc);

  MMFreeMisc(notification, sizeof(NOTIFICATION));

  return rc;
}
