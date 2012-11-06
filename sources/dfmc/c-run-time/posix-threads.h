/*
 *  File:     posix-threads.h
 *  Created:  28 July 1999
 *  Author:   Keith Dennison
 *  Copyright (c) 1999 Functional Objects, Inc. All rights reserved.
 *
 *  Purpose:
 *    Threads portability layer for POSIX
 */

#ifdef OPEN_DYLAN_PLATFORM_LINUX
/* get pthread_setname_np */
#define _GNU_SOURCE
#endif

#include "run-time.h"

#include <stdint.h>

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
#define IS_ZINT(X)      (((intptr_t)(X) & 0x03) == 0x01)

/*****************************************************************************/
/* Type Definitions                                                          */
/*****************************************************************************/

typedef signed long ZINT;

/* This is a <portable-container>
 *
 * It is used to wrap the c-r-t representation of synchronization objects.
 *
 * It is associated with the following classes:
 *
 *   DYLAN class      | C-R-T struct in HANDLE
 *   -----------------------------------------
 *   <simple-lock>    | SIMPLELOCK
 *   <recursive-lock> | RECURSIVELOCK
 *   <semaphore>      | SEMAPHORE
 *   <notification>   | NOTIFICATION
 *
 */
typedef struct _ctr1
{
  D class;
  void *handle;
} CONTAINER;

/* This is <portable-double-container> used in <thread>
 *
 * HANDLE1 contains internal flags
 * HANDLE2 contains the runtime thread object
 *
 */
typedef struct _ctr2
{
  D class;
  void *handle1;
  void *handle2;
} DTHREAD;

typedef void * D_NAME;
typedef D *TLV_VECTOR;


/*
 * Thread structure
 *
 * Private to C-R-T.
 *
 * Allocated using the GC so it can reference dylan objects.
 *
 */
typedef struct thread {
  pthread_t tid;
  TEB* teb;
  D name;
  D function;
} THREAD;

/* Synchronization structures
 *
 * Private to C-R-T.
 *
 * CAUTION! These are manually allocated and no GC roots.
 */

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


/* This structure is used for growing TLV vectors.
 *
 * The THREAD field is rooted by the TEB.
 *
 * The TLV_VECTOR is allocated manually by code using this struct.
 */
typedef struct tlv_vector_list_element
{
  DTHREAD                         *thread;
  TEB                             *teb;
  TLV_VECTOR                       tlv_vector;
  struct tlv_vector_list_element  *next;
} *TLV_VECTOR_LIST;


#endif
