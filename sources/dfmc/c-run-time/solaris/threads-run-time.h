/*
    ZINT	:   threads-run-time.h
    Created: 4th September 1995
    Author:  Rod Moyse

    Purpose:
      Add support for solaris threads to the C runtime system.
      
    Change History:
    
    To do:

*/

#ifndef THREADS_RUN_TIME_H
#define THREADS_RUN_TIME_H

#include <thread.h>

/*****************************************************************************/
/* Macro Definitions                                                         */
/*****************************************************************************/


/*****************************************************************************/
/* Type Definitions                                                          */
/*****************************************************************************/

typedef INT32 ZINT;

typedef struct _ctr1
{
  Z class;
  void * handle;
} CONTAINER;

typedef struct _ctr2
{
  Z class;
  void * handle1;
  int  handle2;
} DTHREAD;

typedef void * D_NAME;

typedef struct v_element
{
  thread_t thread;
  void ** thread_vec;
  void *  next;
} * LINKP;


/*****************************************************************************/
/* Function Prototypes                                                       */
/*****************************************************************************/


void	dylanXinternalXprimitive_make_thread(DTHREAD * newthread, D_NAME name, ZINT priority,
					     ZFN func);

void	dylanXinternalXprimitive_free_thread(DTHREAD * thread);

ZINT	dylanXinternalXprimitive_thread_join_single(DTHREAD * thread);
Z	dylanXinternalXprimitive_thread_join_multiple(SOV * thread_vector);

void	dylanXinternalXprimitive_thread_yield(void);
void*	dylanXinternalXprimitive_current_thread(void);

ZINT	dylanXinternalXprimitive_wait_for_simple_lock(CONTAINER * lock);
ZINT	dylanXinternalXprimitive_wait_for_recursive_lock(CONTAINER * lock);
ZINT	dylanXinternalXprimitive_wait_for_semaphore(CONTAINER * lock);
ZINT	dylanXinternalXprimitive_wait_for_notification(CONTAINER * condvar, CONTAINER * lock);

ZINT	dylanXinternalXprimitive_wait_for_simple_lock_timed(CONTAINER * lock, ZINT milsecs);
ZINT	dylanXinternalXprimitive_wait_for_recursive_lock_timed(CONTAINER * lock, ZINT milsecs);
ZINT	dylanXinternalXprimitive_wait_for_semaphore_timed(CONTAINER * lock, ZINT milsecs);
ZINT	dylanXinternalXprimitive_wait_for_notification_timed(CONTAINER * var, CONTAINER * lock,
							     ZINT milsecs);

ZINT	dylanXinternalXprimitive_release_simple_lock(CONTAINER * lock);
ZINT	dylanXinternalXprimitive_release_recursive_lock(CONTAINER * lock);
ZINT	dylanXinternalXprimitive_release_semaphore(CONTAINER * lock);
ZINT	dylanXinternalXprimitive_release_notification(CONTAINER * notif, CONTAINER * lock);
ZINT	dylanXinternalXprimitive_release_all_notification(CONTAINER * notif, CONTAINER * lock);

void	dylanXinternalXprimitive_make_recursive_lock(CONTAINER * lock, D_NAME name);
void	dylanXinternalXprimitive_destroy_recursive_lock(CONTAINER * lock);

void	dylanXinternalXprimitive_make_simple_lock(CONTAINER * lock, D_NAME name);
void	dylanXinternalXprimitive_destroy_simple_lock(CONTAINER * lock);

void	dylanXinternalXprimitive_make_semaphore(CONTAINER * sema, D_NAME name,
			      ZINT initial, ZINT max);
void	dylanXinternalXprimitive_destroy_semaphore(CONTAINER * sema);

void	dylanXinternalXprimitive_make_notification(CONTAINER * notif, D_NAME name);
void	dylanXinternalXprimitive_destroy_notification(CONTAINER * notif);

void	dylanXinternalXprimitive_sleep(ZINT milsecs);

ZINT	dylanXinternalXprimitive_owned_simple_lock(CONTAINER * lock);
ZINT	dylanXinternalXprimitive_owned_recursive_lock(CONTAINER * rlock);

Z	dylanXinternalXprimitive_assign_atomic_memory(void * * location, Z newval);
ZINT	dylanXinternalXprimitive_conditional_update_memory(void * * location, Z newval,
							   Z oldval);
void*	dylanXinternalXprimitive_allocate_thread_variable(Z value);
Z	dylanXinternalXprimitive_read_thread_variable(void * varhandle);
Z	dylanXinternalXprimitive_write_thread_variable(void * varhandle, Z newval);

void	dylanXinternalXprimitive_initialise_current_thread(DTHREAD * thread);


/* DO NOT ADD ANYTHING AFTER THIS LINE */
#endif



