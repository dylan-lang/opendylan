/*
 *  File:     linux-threads-primitives.h
 *  Created:  4th September 1995
 *  Author:   Rod Moyse, Keith Dennison, Tony Mann
 *  Copyright 1995, 1996 The Harlequin Group Ltd. All rights reserved.
 *
 *  Purpose:
 *    Threads portability layer for Linux.
 */

#ifndef THREADS_RUN_TIME_H
#define THREADS_RUN_TIME_H

#include <pthread.h>
#include <semaphore.h>

/*****************************************************************************/
/* Macro Definitions                                                         */
/*****************************************************************************/

#define THREADS_RUN_TIME_API



/* Size of vector for storing thread variables. */
#define TLV_VECTOR_INITIAL_SIZE 20

/* A large negative number used to indicate that a thread is in the process
   of growing the TLV vector */
#define TLV_GROW ((PVOID)(-2000000))


#ifndef _DEBUG      /* For Release builds */
#define  MSG0(msg)                          ((void)0)
#define  MSG1(msg, arg1)                    ((void)0)
#define  MSG2(msg, arg1, arg2)              ((void)0)
#define  MSG3(msg, arg1, arg2, arg3)        ((void)0)
#define  MSG4(msg, arg1, arg2, arg3, arg4)  ((void)0)
#else               /* For Debug builds */
#define  MSG0(msg)                          printf(msg)
#define  MSG1(msg, arg1)                    printf(msg, arg1)
#define  MSG2(msg, arg1, arg2)              printf(msg, arg1, arg2)
#define  MSG3(msg, arg1, arg2, arg3)        printf(msg, arg1, arg2, arg3)
#define  MSG4(msg, arg1, arg2, arg3, arg4)  printf(msg, arg1, arg2, arg3, arg4)
#endif

/* Error codes returned by the primitives. These correspond to the codes
   defined in D-lib-threads!return-codes.dylan */
#define OK              ((ZINT)I(0))
#define GENERAL_ERROR   ((ZINT)I(-1))
#define TIMEOUT         ((ZINT)I(1))
#define NOT_LOCKED      ((ZINT)I(2))
#define ALREADY_LOCKED  ((ZINT)I(2))
#define COUNT_EXCEEDED  ((ZINT)I(3))
#define CREATE_ERROR    ((ZINT)I(1))
#define PRIORITY_ERROR  ((ZINT)I(2))

/* Macros for tagged integers */
#define IS_ZINT(X)      (((int)(X) & 0x03) == 0x01)
#define I(a)            ((ZINT)(((a)<<2)+1))

/*****************************************************************************/
/* Type Definitions                                                          */
/*****************************************************************************/

typedef void *Z;
typedef Z (*ZFN)(Z,int,...);
typedef signed long ZINT;

typedef struct _sov{
  Z class;
  Z size;
  Z data[1];
} SOV;

typedef struct _ctr1
{
  Z class;
  void *handle;
} CONTAINER;

typedef struct _ctr2
{
  Z class;
  void *handle1;
  void *handle2;
} DTHREAD;

typedef void * D_NAME;

/*
typedef void *HANDLE;
typedef long LONG;
typedef char BYTE; 
typedef unsigned int BOOL; 
*/

typedef struct semaphore {
	sem_t sema;
} SEMAPHORE;

typedef struct simple_lock {
	pthread_mutex_t mutex;
        HANDLE owner;
} SIMPLELOCK;

typedef struct recursive_lock {
	pthread_mutex_t mutex;
        HANDLE owner;
	int    recursion_count;
} RECURSIVELOCK;

typedef struct notification {
        pthread_cond_t cond;
} NOTIFICATION;

typedef BYTE *TLV_VECTOR;

typedef struct tlv_vector_list_element
{
  HANDLE hThread;
  TLV_VECTOR tlv_vector;
  struct tlv_vector_list_element *next;
} *TLV_VECTOR_LIST;


/*****************************************************************************/
/* Function prototypes for the primitives                                    */
/*****************************************************************************/


THREADS_RUN_TIME_API  ZINT
primitive_make_thread(DTHREAD * newthread, D_NAME name, ZINT priority,
                      ZFN func, BOOL synchronize);

THREADS_RUN_TIME_API  ZINT
primitive_destroy_thread(DTHREAD * thread);


THREADS_RUN_TIME_API  ZINT
primitive_thread_join_single(DTHREAD * thread);

THREADS_RUN_TIME_API  Z
primitive_thread_join_multiple(SOV * thread_vector);


THREADS_RUN_TIME_API  void  primitive_thread_yield(void);
THREADS_RUN_TIME_API  Z     primitive_current_thread(void);

THREADS_RUN_TIME_API  ZINT
primitive_wait_for_simple_lock(CONTAINER * lock);

THREADS_RUN_TIME_API  ZINT
primitive_wait_for_recursive_lock(CONTAINER * lock);

THREADS_RUN_TIME_API  ZINT
primitive_wait_for_semaphore(CONTAINER * lock);

THREADS_RUN_TIME_API  ZINT
primitive_wait_for_notification(CONTAINER * condvar, CONTAINER * lock);


THREADS_RUN_TIME_API  ZINT
primitive_wait_for_simple_lock_timed(CONTAINER * lock, ZINT milsecs);

THREADS_RUN_TIME_API  ZINT
primitive_wait_for_recursive_lock_timed(CONTAINER * lock, ZINT milsecs);

THREADS_RUN_TIME_API  ZINT
primitive_wait_for_semaphore_timed(CONTAINER * lock, ZINT milsecs);

THREADS_RUN_TIME_API  ZINT
primitive_wait_for_notification_timed(CONTAINER * var, CONTAINER * lock,
                                      ZINT milsecs);


THREADS_RUN_TIME_API  ZINT
primitive_release_simple_lock(CONTAINER * lock);

THREADS_RUN_TIME_API  ZINT
primitive_release_recursive_lock(CONTAINER * lock);

THREADS_RUN_TIME_API  ZINT
primitive_release_semaphore(CONTAINER * lock);

THREADS_RUN_TIME_API  ZINT
primitive_release_notification(CONTAINER * notif, CONTAINER * lock);

THREADS_RUN_TIME_API  ZINT
primitive_release_all_notification(CONTAINER * notif, CONTAINER * lock);


THREADS_RUN_TIME_API  ZINT
primitive_make_recursive_lock(CONTAINER * lock, D_NAME name);

THREADS_RUN_TIME_API  ZINT
primitive_destroy_recursive_lock(CONTAINER * lock);


THREADS_RUN_TIME_API  ZINT
primitive_make_simple_lock(CONTAINER * lock, D_NAME name);

THREADS_RUN_TIME_API  ZINT
primitive_destroy_simple_lock(CONTAINER * lock);


THREADS_RUN_TIME_API  ZINT
primitive_make_semaphore(CONTAINER * sema, D_NAME name, ZINT initial,
                         ZINT max);

THREADS_RUN_TIME_API  ZINT
primitive_destroy_semaphore(CONTAINER * sema);


THREADS_RUN_TIME_API  ZINT
primitive_make_notification(CONTAINER * notif, D_NAME name);

THREADS_RUN_TIME_API  ZINT
primitive_destroy_notification(CONTAINER * notif);

THREADS_RUN_TIME_API  void
primitive_sleep(ZINT milsecs);

THREADS_RUN_TIME_API  ZINT
primitive_owned_simple_lock(CONTAINER * lock);

THREADS_RUN_TIME_API  ZINT
primitive_owned_recursive_lock(CONTAINER * rlock);

/*
THREADS_RUN_TIME_API  Z
primitive_assign_atomic_memory(void * * location, Z newval);

THREADS_RUN_TIME_API  ZINT
primitive_conditional_update_memory(void * * location, Z newval, Z oldval);
*/

THREADS_RUN_TIME_API  void*
primitive_allocate_thread_variable(Z value);

THREADS_RUN_TIME_API  Z
primitive_read_thread_variable(void * varhandle);

THREADS_RUN_TIME_API  Z
primitive_write_thread_variable(void * varhandle, Z newval);

THREADS_RUN_TIME_API  void
primitive_initialize_current_thread(DTHREAD * thread, BOOL synchronize);

THREADS_RUN_TIME_API  void
primitive_initialize_special_thread(DTHREAD * thread);


THREADS_RUN_TIME_API  ZINT
primitive_unlock_simple_lock(CONTAINER *);

THREADS_RUN_TIME_API  ZINT
primitive_unlock_recursive_lock(CONTAINER *);



/* DO NOT ADD ANYTHING AFTER THIS LINE */
#endif
