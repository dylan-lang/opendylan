/*
 *  File:     posix-threads.h
 *  Created:  28 July 1999
 *  Author:   Keith Dennison
 *  Copyright (c) 1999 Functional Objects, Inc. All rights reserved.
 *
 *  Purpose:
 *    Threads portability layer for POSIX
 */

#include "run-time.h"

#include <pthread.h>


#ifndef THREADS_RUN_TIME_H
#define THREADS_RUN_TIME_H


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
#define OK              (I(0))
#define GENERAL_ERROR   (I(-1))
#define TIMEOUT         (I(1))
#define NOT_LOCKED      (I(2))
#define ALREADY_LOCKED  (I(2))
#define COUNT_EXCEEDED  (I(3))
#define CREATE_ERROR    (I(1))
#define PRIORITY_ERROR  (I(2))

/* Thread state flags
 */
#define COMPLETED 1u
#define MARKED    2u
#define JOINED    4u

/* Macros for tagged integers */
#define IS_ZINT(X)      (((int)(X) & 0x03) == 0x01)

/*****************************************************************************/
/* Type Definitions                                                          */
/*****************************************************************************/

typedef signed long ZINT;

typedef struct _ctr1
{
  D class;
  void *handle;
} CONTAINER;

typedef struct _ctr2
{
  D class;
  void *handle1;
  void *handle2;
} DTHREAD;

typedef void * D_NAME;


typedef struct simple_lock {
  pthread_t        owner;
  pthread_mutex_t  mutex;
  pthread_cond_t   cond;
} SIMPLELOCK;

typedef struct recursive_lock {
  pthread_t        owner;
  int              count;
  pthread_mutex_t  mutex;
  pthread_cond_t   cond;
} RECURSIVELOCK;

typedef struct semaphore {
  pthread_mutex_t mutex;
  pthread_cond_t  cond;
  int             count;
  int             max_count;
} SEMAPHORE;

typedef struct {
  pthread_mutex_t  mutex;
  pthread_cond_t   cond;
} NOTIFICATION;

typedef D *TLV_VECTOR;

typedef struct tlv_vector_list_element
{
  DTHREAD                         *thread;
  TLV_VECTOR                       tlv_vector;
  struct tlv_vector_list_element  *next;
} *TLV_VECTOR_LIST;


#endif
