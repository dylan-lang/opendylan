/*
 * File:      threads_primitives_test.c
 * Author:    Keith Dennison
 * Created:   1 October, 1996
 * Copyright: 1996 Functional Objects, Inc. All rights reserved.
 */

/* Use the definitions below to select which category(ies) of primitives to test */
#define TEST_SIMPLE_LOCK
#define TEST_RECURSIVE_LOCK
#define TEST_SEMAPHORE
#define TEST_NOTIFICATION
#define TEST_THREAD

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

#include "win32-threads-primitives.h"


#define TEST_OK (Z)I(0)
#define TEST_FAILED (Z)I(1)

#define CHECK_OK(MSG)      printf("    PASSED: " MSG "\n");
#define CHECK_FAILED(MSG)  { printf("    FAILED: " MSG "\n"); \
                             return(TEST_FAILED); }
#define CHECK(MSG, EXP)    if (EXP) CHECK_OK(MSG) else CHECK_FAILED(MSG)

#define N_SIMPLE_LOCKS 3
#define N_THREADS 10


CONTAINER simple_lock, recursive_lock, notification, semaphore;
char name[] = "firstname", name2[] = "secondname";


/* *********************************************************************** */
/* Simple Lock tests                                                       */
/* *********************************************************************** */

/* Test 1
 *   Check the primitives for creating and destroying simple locks.
 */
Z test_sl1()
{
	printf("\n1 - Create/Destroy simple lock:\n");

	CHECK("make lock",
		primitive_make_simple_lock(&simple_lock, NULL) == OK);
	CHECK("lock not owned",
		primitive_owned_simple_lock(&simple_lock) == (ZINT)I(0));
	CHECK("destroy lock",
		primitive_destroy_simple_lock(&simple_lock) == OK);

    return(TEST_OK);
}

/* Test 2
 *   Check that claiming and releasing works correctly
 */
Z test_sl2()
{
	printf("\n2 - Claiming/Releasing simple lock:\n");

	CHECK("make lock",
		primitive_make_simple_lock(&simple_lock, NULL) == OK);
	CHECK("lock not owned",
		primitive_owned_simple_lock(&simple_lock) == (ZINT)I(0));
	CHECK("wait-for lock",
		primitive_wait_for_simple_lock(&simple_lock) == OK);
	CHECK("lock owned",
		primitive_owned_simple_lock(&simple_lock) == (ZINT)I(1));
	CHECK("release lock",
		primitive_release_simple_lock(&simple_lock) == OK);
	CHECK("lock not owned",
		primitive_owned_simple_lock(&simple_lock) == (ZINT)I(0));
	CHECK("destroy lock",
		primitive_destroy_simple_lock(&simple_lock) == OK);

	return(TEST_OK);
}

/* test 3
 *   Trying to claim a lock which the thread already owns. The second
 *   call to wait-for-simple-lock should return an error.
 */
Z test_sl3()
{
	printf("\n3 - Claiming when already owned by us:\n");

	CHECK("make lock",
		primitive_make_simple_lock(&simple_lock, NULL) == OK);
	CHECK("lock not owned",
		primitive_owned_simple_lock(&simple_lock) == (ZINT)I(0));
	CHECK("wait-for lock",
		primitive_wait_for_simple_lock(&simple_lock) == OK);
	CHECK("lock owned",
		primitive_owned_simple_lock(&simple_lock));
	CHECK("wait-for lock when already owned",
		primitive_wait_for_simple_lock(&simple_lock) == ALREADY_LOCKED);
	CHECK("lock owned",
		primitive_owned_simple_lock(&simple_lock));
	CHECK("release lock",
		primitive_release_simple_lock(&simple_lock) == OK);
	CHECK("lock not owned",
		primitive_owned_simple_lock(&simple_lock));
	CHECK("destroy lock",
		primitive_destroy_simple_lock(&simple_lock) == OK);

	return(TEST_OK);
}

/* Test 4
 *   releasing a lock which is not owned by the thread. The call to
 *   release-simple-lock should return an error.
 */
Z test_sl4()
{
	printf("\n4 - Releasing when not owned by us:\n");

	CHECK("make lock",
		primitive_make_simple_lock(&simple_lock, NULL) == OK);
	CHECK("lock not owned",
		primitive_owned_simple_lock(&simple_lock) == (ZINT)I(0));
	CHECK("release lock",
		primitive_release_simple_lock(&simple_lock) == NOT_LOCKED);
	CHECK("destroy lock",
		primitive_destroy_simple_lock(&simple_lock) == OK);

	return(TEST_OK);
}

/* Test 5
 *   Start a thread which claims the lock for a while. The parent thread tries
 *   to claim the lock while it is owned by the other thread, and only suceeds
 *   when it is released.
 */
Z test_sl5a(Z o, int n, ...)
{
	CHECK("thread 5a: lock not owned",
		primitive_owned_simple_lock(&simple_lock) == (ZINT)I(0));
	CHECK("thread 5a: wait-for lock",
		primitive_wait_for_simple_lock(&simple_lock) == OK);
	CHECK("thread 5a: lock owned",
		primitive_owned_simple_lock(&simple_lock) == (ZINT)I(1));
	Sleep(5000);
	CHECK("thread 5a: release lock",
		primitive_release_simple_lock(&simple_lock) == OK);
	CHECK("thread 5a: lock not owned",
		primitive_owned_simple_lock(&simple_lock) == (ZINT)I(0));

	return(TEST_OK);
}

Z test_sl5()
{
	DTHREAD   thread;

	printf("\n5 - Claiming when owned by someone else:\n");

	CHECK("make lock",
		primitive_make_simple_lock(&simple_lock, NULL) == OK);
	CHECK("lock not owned",
		primitive_owned_simple_lock(&simple_lock) == (ZINT)I(0));
	CHECK("make thread",
		primitive_make_thread(&thread, NULL, (ZINT)I(0), test_sl5a) == OK);
	Sleep(1000);
	CHECK("wait-for lock",
		primitive_wait_for_simple_lock(&simple_lock) == OK);
	CHECK("lock owned",
		primitive_owned_simple_lock(&simple_lock) == (ZINT)I(1));
	CHECK("release lock",
		primitive_release_simple_lock(&simple_lock) == OK);
	CHECK("lock not owned",
		primitive_owned_simple_lock(&simple_lock) == (ZINT)I(0));
	CHECK("join thread",
		primitive_thread_join_single(&thread) == OK);
	CHECK("destroy thread",
		primitive_destroy_thread(&thread) == OK);
	CHECK("destroy lock",
		primitive_destroy_simple_lock(&simple_lock) == OK);

	return(TEST_OK);
}


/* Test 6
 *   Start a thread which claims the lock but doesn't release it. The parent
 *   thread does a timed wait for the lock which returns with a timeout, then
 *   forces the lock to be released so it can claim it.
 */
Z test_sl6a(Z o, int n, ...)
{
	CHECK("thread 6a: wait-for lock",
		primitive_wait_for_simple_lock(&simple_lock) == OK);
	CHECK("thread 6a: lock owned",
		primitive_owned_simple_lock(&simple_lock) == (ZINT)I(1));
	Sleep(3000);
	CHECK("thread 6a: lock not owned",
		primitive_owned_simple_lock(&simple_lock) == (ZINT)I(0));
	return(TEST_OK);
}

Z test_sl6()
{
	DTHREAD thread;

	printf("\n6 - Force lock owned by someone else to be unlocked:\n");

	CHECK("make lock",
		primitive_make_simple_lock(&simple_lock, NULL) == OK);
	CHECK("make thread",
		primitive_make_thread(&thread, NULL, (ZINT)I(0), test_sl6a));
	Sleep(1000);
	CHECK("wait-for lock with timeout",
		primitive_wait_for_simple_lock_timed(&simple_lock, (ZINT)I(1000)) == TIMEOUT);
	CHECK("unlock lock",
		primitive_unlock_simple_lock(&simple_lock) == OK);
	CHECK("wait-for-lock",
		primitive_wait_for_simple_lock(&simple_lock) == OK);
	CHECK("lock owned",
		primitive_owned_simple_lock(&simple_lock) == (ZINT)I(1));
	CHECK("release lock",
		primitive_release_simple_lock(&simple_lock) == OK);
	CHECK("thread join",
		primitive_thread_join_single(&thread) == OK);
	CHECK("destroy thread",
		primitive_destroy_thread(&thread) == OK);
	CHECK("destroy lock",
		primitive_destroy_simple_lock(&simple_lock) == OK);

	return(TEST_OK);
}


/* *********************************************************************** */
/* Recursive Lock tests                                                    */
/* *********************************************************************** */

/* Test 1
 *   Check primitives for creating and destroying recursive locks
 */
Z test_rl1()
{
	printf("\n1 - Create/Destroy recursive lock:\n");

    CHECK("make lock",
		primitive_make_recursive_lock(&recursive_lock, NULL) == OK);
	CHECK("lock not owned",
		primitive_owned_recursive_lock(&recursive_lock) == (ZINT)I(0));
	CHECK("destroy lock",
		primitive_destroy_recursive_lock(&recursive_lock) == OK);

	return(TEST_OK);
}

/* Test 2
 *   Check that claiming and releasing the lock works correctly, and test
 *   owned primitive.
 */
Z test_rl2()
{
	printf("\n2 - Claiming/Releasing recursive lock\n");
	CHECK("make lock",
		primitive_make_recursive_lock(&recursive_lock, name) == OK);
	CHECK("lock not owned",
		primitive_owned_recursive_lock(&recursive_lock) == (ZINT)I(0));
	CHECK("wait-for lock",
		primitive_wait_for_recursive_lock(&recursive_lock) == OK);
	CHECK("lock owned",
		primitive_owned_recursive_lock(&recursive_lock) == (ZINT)I(1));
	CHECK("release lock",
		primitive_release_recursive_lock(&recursive_lock) == OK);
	CHECK("lock not owned",
		primitive_owned_recursive_lock(&recursive_lock) == (ZINT)I(0))
	CHECK("destroy lock",
		primitive_destroy_recursive_lock(&recursive_lock) == OK);

	return(TEST_OK);
}

/* Test 3
 *   Check that the recursive lock can be claimed multiple times, and is no
 *   longer owned only when it is released the same number of times.
 */
Z test_rl3()
{
	int i;

	printf("\n3 - Claiming when already owned by us\n");
	CHECK("make lock",
		primitive_make_recursive_lock(&recursive_lock, name) == OK);
	for (i=0; i<5; i++) {
		CHECK("wait-for lock",
			primitive_wait_for_recursive_lock(&recursive_lock) == OK);
		CHECK("lock owned",
			primitive_owned_recursive_lock(&recursive_lock) == (ZINT)I(1));
	}
	for (i=0; i<4; i++) {
		CHECK("release lock",
			primitive_release_recursive_lock(&recursive_lock) == OK);
		CHECK("lock owned",
			primitive_owned_recursive_lock(&recursive_lock) == (ZINT)I(1));
	}
	CHECK("release lock",
		primitive_release_recursive_lock(&recursive_lock) == OK);
	CHECK("lock not owned",
		primitive_owned_recursive_lock(&recursive_lock) == (ZINT)I(0));
	CHECK("destroy lock",
		primitive_destroy_recursive_lock(&recursive_lock) == OK);

	return(TEST_OK);
}

/* Test 4
 *   Try to release a lock which is not owned by the thread. The release should
 *   return an error.
 */
Z test_rl4()
{
	printf("\n4 - Releasing when not owned by us:\n");
	CHECK("make lock",
		primitive_make_recursive_lock(&recursive_lock, name) == OK);
	CHECK("release lock",
		primitive_release_recursive_lock(&recursive_lock) == NOT_LOCKED);
	CHECK("destroy lock",
		primitive_destroy_recursive_lock(&recursive_lock) == OK);

	return(TEST_OK);
}

/* Test 5
 *   Start a thread which claims the lock for a while. The parent thread tries
 *   to claim the lock while it is owned by the other thread. The claim succeeds
 *   only when the lock is released by the thread.
 */
Z test_rl5a(Z o, int n, ...)
{
	CHECK("thread 5a: wait-for lock",
		primitive_wait_for_recursive_lock(&recursive_lock) == OK);
	Sleep(5000);
	CHECK("thread 5a: release lock",
		primitive_release_recursive_lock(&recursive_lock) == OK);
	return(TEST_OK);
}

Z test_rl5()
{
	DTHREAD   thread;

	printf("\n5 - Claiming when owned by someone else:\n");

	CHECK("make lock",
		primitive_make_recursive_lock(&recursive_lock, name) == OK);
	CHECK("make thread",
		primitive_make_thread(&thread, NULL, (ZINT)I(0), test_rl5a) == OK);
	Sleep(1000);
	CHECK("wait-for lock",
		primitive_wait_for_recursive_lock(&recursive_lock) == OK);
	CHECK("release lock",
		primitive_release_recursive_lock(&recursive_lock) == OK);
	CHECK("thread join",
		primitive_thread_join_single(&thread) == OK);
	CHECK("destroy thread",
		primitive_destroy_thread(&thread) == OK);
	CHECK("destroy lock",
		primitive_destroy_recursive_lock(&recursive_lock) == OK);

	return(TEST_OK);
}

/* Test 6
 *   Start a thread which claims the lock but does not release it. The parent
 *   thread does a timed wait for the lock which returns with a timeout error,
 *   then forces the lock to be released so that it can claim it.
 */
Z test_rl6a(Z o, int n, ...)
{
	CHECK("thread 6a: wait-for lock",
		primitive_wait_for_recursive_lock(&recursive_lock) == OK);
	CHECK("thread 6a: lock owned",
		primitive_owned_recursive_lock(&recursive_lock) == (ZINT)I(1));
	Sleep(3000);
	CHECK("thread 6a: lock not owned",
		primitive_owned_recursive_lock(&recursive_lock) == (ZINT)I(0));
	return(TEST_OK);
}

Z test_rl6()
{
	DTHREAD thread;

	printf("\n6 - Force lock owned by someone else to be unlocked:\n");

	CHECK("make lock",
		primitive_make_recursive_lock(&recursive_lock, NULL) == OK);
	CHECK("make thread",
		primitive_make_thread(&thread, NULL, (ZINT)I(0), test_rl6a) == OK);
	Sleep(1000);
	CHECK("timed wait-for lock",
		primitive_wait_for_recursive_lock_timed(&recursive_lock, (ZINT)I(1000)) == TIMEOUT);
	CHECK("unlock lock",
		primitive_unlock_recursive_lock(&recursive_lock) == OK);
	CHECK("wait-for lock",
		primitive_wait_for_recursive_lock(&recursive_lock) == OK);
	CHECK("lock owned",
		primitive_owned_recursive_lock(&recursive_lock) == (ZINT)I(1));
	CHECK("release lock",
		primitive_release_recursive_lock(&recursive_lock) == OK);
	CHECK("thread join",
		primitive_thread_join_single(&thread) == OK);
	CHECK("destroy lock",
		primitive_destroy_recursive_lock(&recursive_lock) == OK);

	return(TEST_OK);
}


/* *********************************************************************** */
/* Semaphore tests                                                         */
/* *********************************************************************** */

/* Test 1
 *   Check the primitives for creating and destroying semaphores.
 */
Z test_s1()
{
	printf("\n1 - Create/Destroy:\n");

	CHECK("make semaphore",
		primitive_make_semaphore(&semaphore, NULL, (ZINT)I(0), (ZINT)I(6)) == OK);
	CHECK("destroy semaphore",
		primitive_destroy_semaphore(&semaphore) == OK);

	return(TEST_OK);
}

/* Test 2
 *   Make sure claiming and releasing work.
 */
Z test_s2()
{
	printf("\n2 - Claim/Release:\n");

    CHECK("make semaphore",
		primitive_make_semaphore(&semaphore, NULL, (ZINT)I(0), (ZINT)I(1)) == OK);
	CHECK("release semaphore",
		primitive_release_semaphore(&semaphore) == OK);
	CHECK("wait-for semaphore",
		primitive_wait_for_semaphore(&semaphore) == OK);
	CHECK("release semaphore",
		primitive_release_semaphore(&semaphore) == OK);
	CHECK("timed wait-for semaphore",
		primitive_wait_for_semaphore_timed(&semaphore, (ZINT)I(100)) == OK);
	CHECK("destroy semaphore",
		primitive_destroy_semaphore(&semaphore) == OK);

	return(TEST_OK);
}

/* Test 3
 *   Claim a semaphore too many times. The second wait is a timed wait which
 *   should succeed. The third wait should return with a timeout error.
 */
Z test_s3()
{
	printf("\n3 - Claim semaphore too many times with timed wait:\n");
    
	CHECK("make semaphore",
		primitive_make_semaphore(&semaphore, NULL, (ZINT)I(2), (ZINT)I(6)) == OK);
	CHECK("wait-for semaphore",
		primitive_wait_for_semaphore(&semaphore) == OK);
	CHECK("timed wait-for semaphore",
		primitive_wait_for_semaphore_timed(&semaphore, (ZINT)I(100)) == OK);
	CHECK("timed wait-for semaphore",
		primitive_wait_for_semaphore_timed(&semaphore, (ZINT)I(100)) == TIMEOUT);
	CHECK("destroy semaphore",
		primitive_destroy_semaphore(&semaphore) == OK);

	return(TEST_OK);
}



/* Test 4
 *   Release the semaphore too many times. The third release should generate
 *   a count exceeded error.
 */
Z test_s4()
{
	printf("\n4 - Release semaphore too many times:\n");
	CHECK("make semaphore",
		primitive_make_semaphore(&semaphore, NULL, (ZINT)I(2), (ZINT)I(4)) == OK);
	CHECK("release semaphore",
		primitive_release_semaphore(&semaphore) == OK);
	CHECK("release semaphore",
		primitive_release_semaphore(&semaphore) == OK);
	CHECK("release semaphore generates count exceeded",
		primitive_release_semaphore(&semaphore) == COUNT_EXCEEDED);
	return(TEST_OK);
}

/* Test 5
 *   Use another thread which claims the semaphore maximum count times, then
 *   try to claim the semaphore from the parent thread. The child thread
 *   releases the semaphore once to let the claim succeed.
 */
Z test_s5a()
{
	int i;

	for (i=0; i<5; i++)
		CHECK("thread 5a: wait-for semaphore",
			primitive_wait_for_semaphore(&semaphore) == OK);
	Sleep(1000);
	CHECK("thread 5a: release semaphore",
		primitive_release_semaphore(&semaphore) == OK);
	return(TEST_OK);
}

Z test_s5()
{
	DTHREAD thread;

	printf("\n5 - Claim semaphore from another thread\n");
	primitive_make_semaphore(&semaphore, NULL, (ZINT)I(5), (ZINT)I(5));
	primitive_make_thread(&thread, NULL, (ZINT)I(0), test_s5a);

	Sleep(500);
	CHECK("wait-for semaphore",
		primitive_wait_for_semaphore(&semaphore) == OK);
	CHECK("timed wait-for semaphore",
		primitive_wait_for_semaphore_timed(&semaphore, (ZINT)I(100)) == TIMEOUT);

	primitive_thread_join_single(&thread);
	primitive_destroy_thread(&thread);
	primitive_destroy_semaphore(&semaphore);
	return(TEST_OK);
}


/* *********************************************************************** */
/* Notification tests                                                      */
/* *********************************************************************** */

/* Test 1
 *   Check primitives for creating and destroying notifications
 */
Z test_n1()
{
	printf("\n1 - Create/Destroy:\n");

	CHECK("make notification",
		primitive_make_notification(&notification, NULL) == OK);
	CHECK("destroy notification",
		primitive_destroy_notification(&notification) == OK);

	return(TEST_OK);
}

/* Test 2
 *   Check that wait-for/release notification primitives work. The main
 *   thread waits for the notification while a created thread releases it.
 *   Note that a simple lock must be created and used as the associated lock
 *   for the notification.
 */
Z test_n2a(Z o, int n, ...)
{
	Sleep(5000);
	CHECK("thread 2a: wait-for lock",
		primitive_wait_for_simple_lock(&simple_lock) == OK);
	CHECK("thread 2a: release notification",
		primitive_release_notification(&notification, &simple_lock) == OK);
	CHECK("thread 2a: release lock",
		primitive_release_simple_lock(&simple_lock) == OK);
	return(TEST_OK);
}

Z test_n2()
{
	DTHREAD thread;

	printf("\n2 - Wait for notification:\n");
	CHECK("make notification",
		primitive_make_notification(&notification, NULL) == OK);
	CHECK("make lock",
		primitive_make_simple_lock(&simple_lock, NULL) == OK);
	CHECK("make thread",
		primitive_make_thread(&thread, name, (ZINT)I(0), test_n2a) == OK);
	CHECK("wait-for lock",
		primitive_wait_for_simple_lock(&simple_lock) == OK);
	CHECK("wait-for notification",
		primitive_wait_for_notification(&notification, &simple_lock) == OK);
	CHECK("join thread",
		primitive_thread_join_single(&thread) == OK);
	CHECK("destroy lock",
		primitive_destroy_simple_lock(&simple_lock) == OK);
	CHECK("destroy notification",
		primitive_destroy_notification(&notification) == OK);

	return(TEST_OK);
}

/* Test 3
 *   Check timed wait for a notification. The timed wait should return with a
 *   timeout error.
 */
Z test_n3a(Z o, int n, ...)
{
	Sleep(5000);
	CHECK("thread 3a: wait-for lock",
		primitive_wait_for_simple_lock(&simple_lock) == OK);
	CHECK("thread 3a: release-notification",
		primitive_release_notification(&notification, &simple_lock) == OK);
	CHECK("thread 3a: release lock",
		primitive_release_simple_lock(&simple_lock) == OK);
	return(TEST_OK);
}

Z test_n3()
{
	DTHREAD thread;

	printf("\n3 - Wait for notification timed:\n");
	CHECK("make notification",
		primitive_make_notification(&notification, name) == OK);
	CHECK("make lock",
		primitive_make_simple_lock(&simple_lock, name) == OK);
	CHECK("make thread",
		primitive_make_thread(&thread, name, (ZINT)I(0), test_n3a) == OK);

	CHECK("wait-for lock",
		primitive_wait_for_simple_lock(&simple_lock) == OK);
	CHECK("timed wait-for notification",
		primitive_wait_for_notification_timed(&notification, &simple_lock, (ZINT)I(500)) == TIMEOUT);
	CHECK("release lock",
		primitive_release_simple_lock(&simple_lock) == OK);
	
	CHECK("join thread",
		primitive_thread_join_single(&thread) == OK);
	CHECK("destroy lock",
		primitive_destroy_simple_lock(&simple_lock) == OK);
	CHECK("destroy notification",
		primitive_destroy_notification(&notification) == OK);

	return(TEST_OK);
}

/* Test 4
 *   Test release-all. Start up lots of threads which wait for a notification,
 *   and then release them all
 */
Z test_n4a(Z o, int n, ...)
{
	CHECK("thread: wait-for lock",
		primitive_wait_for_simple_lock(&simple_lock) == OK);
	Sleep(500 + (rand() % 500));
	CHECK("thread: wait-for notification",
		primitive_wait_for_notification(&notification, &simple_lock) == OK);
	Sleep(500 + (rand() % 500));
	CHECK("thread: release lock",
		primitive_release_simple_lock(&simple_lock) == OK);
	return(TEST_OK);
}

Z test_n4()
{
	DTHREAD thread[10];
	int i;

	printf("\n4 - Several threads waiting for a notification\n");
	primitive_make_notification(&notification, NULL);
	primitive_make_simple_lock(&simple_lock, NULL);
	for (i=0; i<10; i++)
		primitive_make_thread(&thread[i], NULL, (ZINT)I(0), test_n4a);

	Sleep(1000);
	CHECK("wait-for lock",
		primitive_wait_for_simple_lock(&simple_lock) == OK);
	CHECK("release-all",
		primitive_release_all_notification(&notification, &simple_lock) == OK);
	CHECK("release lock",
		primitive_release_simple_lock(&simple_lock) == OK);

	for (i=0; i<10; i++) {
		primitive_thread_join_single(&thread[i]);
		primitive_destroy_thread(&thread[i]);
	}
	primitive_destroy_simple_lock(&simple_lock);
	primitive_destroy_notification(&notification);
	return(TEST_OK);
}

/* Test 5
 *   Another test for timed waits.
 */
Z test_n5a()
{
	DWORD start, end;
	ZINT  zresult;
	int i, timeout;

	for (i=0; i<5; i++) {
		primitive_wait_for_simple_lock(&simple_lock);
		timeout = rand() % 500;
		start = GetTickCount();
		zresult = primitive_wait_for_notification_timed(&notification, &simple_lock, (ZINT)I(timeout));
		end = GetTickCount();
		switch (zresult) {
		case OK:
			printf("thread: got notification. Timeout was %dms\n", timeout);
			break;
		case TIMEOUT:
			printf("thread: notification wait timeout (%dms)\n", timeout);
			break;
		default:
			printf("thread: error waiting for notification: %d\n", zresult >> 2);
		}
		primitive_release_simple_lock(&simple_lock);
	}
	return(TEST_OK);
}

Z test_n5()
{
	DTHREAD thread;
	int i;

	primitive_make_simple_lock(&simple_lock, NULL);
	primitive_make_notification(&notification, NULL);
	primitive_make_thread(&thread, NULL, (ZINT)I(0), test_n5a);

	printf("\n5 - Timed waits for notification\n");
	for (i=0; i<15; i++) {
		Sleep(rand() % 1000);
		CHECK("wait-for lock",
			primitive_wait_for_simple_lock(&simple_lock) == OK);
		CHECK("release notification",
			primitive_release_notification(&notification, &simple_lock) == OK);
		CHECK("release lock",
			primitive_release_simple_lock(&simple_lock) == OK);
	}

	primitive_thread_join_single(&thread);
	primitive_destroy_thread(&thread);
	primitive_destroy_notification(&notification);
	primitive_destroy_simple_lock(&simple_lock);
	return(TEST_OK);
}


/* *********************************************************************** */
/* Thread tests                                                            */
/* *********************************************************************** */

/* Test 1
 *   Test creating and joining a single thread.
 */
Z test_t1a(Z o, int n, ...)
{
	Sleep(2000);
	return(TEST_OK);
}

Z test_t1()
{
	DTHREAD thread;

	printf("\n1 - Make a new thread, then join it.\n");
    CHECK("make thread",
		primitive_make_thread(&thread, name, (ZINT)I(0), test_t1a) == OK);
//	Sleep(1000);
	CHECK("join thread",
		primitive_thread_join_single(&thread) == OK);
	CHECK("destroy thread",
		primitive_destroy_thread(&thread) == OK);

	return(TEST_OK);
}

/* Test 2
 *   Create lots of threads which wait for random periods of time. Repeatedly
 *   use thread-join-multiple to wait for one to finish till they have all
 *   terminated.
 */
Z test_t2a(Z o, int n, ...)
{
	Sleep(500 + (rand() % 1000));
	return(TEST_OK);
}

Z test_t2()
{
	DTHREAD  thread[N_THREADS];
	DTHREAD *threadt;
	SOV     *sov;
	int      count =0, i, j, k;

	printf("\n2 - Make several new threads, then join them.\n");

	for (i=0; i<N_THREADS; i++) {
		CHECK("make thread",
			primitive_make_thread(&thread[i], name, (ZINT)I(0), test_t2a) == OK);
//		Sleep(500);
	}

	// Build a simple-object-vector of the thread objects
	sov = malloc(sizeof(SOV) + (N_THREADS-1)*sizeof(Z));
	assert(sov != NULL);
	sov->class = NULL;
	sov->size = I(N_THREADS);
	for (i=0; i<N_THREADS; i++) {
		sov->data[i] = &thread[i];
  }

	// Now wait for all the other threads to finish
	for (i=0; i<N_THREADS; i++) {
		threadt = primitive_thread_join_multiple(sov);
		CHECK("thread joined", !IS_ZINT(threadt));
		for (j=0; j < ((int)sov->size >> 2); j++) {
			if (sov->data[j] == threadt) {
				count++;
				for (k=j; k < ((int)sov->size >> 2)-1; k++) {
					sov->data[k] = sov->data[k+1];
        }
				sov->size = I(((int)sov->size >> 2) - 1);
				j = N_THREADS;
			}
		}
	}
	CHECK("joined all threads",
		count == N_THREADS);
}

/* Test 3
 *   Check primitive-sleep
 */
Z test_t3()
{
	DWORD start, end;
	int i, time;

	printf("\n3 - primitive-sleep\n");
	for (i=0; i<10; i++) {
		time = (i ^ 2) * 10 + (rand() % 500);
		start = GetTickCount();
		primitive_sleep((ZINT)I(time));
		end = GetTickCount();
		printf("Slept for %dms (wanted %dms)\n", end-start, time);
	}
    return(TEST_OK);
}

/* Test 4
 *   Check primitive-thread-yield
 */
Z test_t4()
{
	printf("\n4 - primitive-thread-yield\n");
	primitive_thread_yield();
	printf("OK\n");
	return(TEST_OK);
}

/* Test 5
 *   Check primitive-current-thread
 */
Z test_t5(DTHREAD *thread)
{
	Z result;

	printf("\n5 - Current thread\n");
	result = primitive_current_thread();
	CHECK("returned correct thread object",
		result == thread);
	return(TEST_OK);
}


/* Test 6
 *   There are three threads in this test. The main thread allocates two thread
 *   variables and all threads check they have the right values for these vars.
 *   Then each thread sets v2 to something different from the other threads.
 *   A notification is used to indicates that a new thread variable has been
 *   allocated by the parent thread - the other two threads check they get the
 *   correct value for the new variable. Before each thread terminates it checks
 *   that it has the correct values in each of the three variables.
 */
void *v1, *v2, *v3;

Z test_t6a(Z o, int x, ...)
{
    CHECK("thread 6a: v1 = 100",
		primitive_read_thread_variable(v1) == I(100));
    CHECK("thread 6a: v2 = 200",
		primitive_read_thread_variable(v2) == I(200));

	CHECK("thread 6a: assign v2 := 500",
		primitive_write_thread_variable(v2, I(500)) == I(500));

	primitive_wait_for_simple_lock(&simple_lock);
    primitive_wait_for_notification(&notification, &simple_lock);
	primitive_release_simple_lock(&simple_lock);

    CHECK("thread 6a: v1 = 100",
		primitive_read_thread_variable(v1) == I(100));
    CHECK("thread 6a: v2 = 500",
		primitive_read_thread_variable(v2) == I(500));
	CHECK("thread 6a: v3 = 100",
		primitive_read_thread_variable(v3) == I(100));

    return(TEST_OK);
}

Z test_t6b(Z o, int x, ...)
{
	CHECK("thread 6b: v1 = 100",
		primitive_read_thread_variable(v1) == I(100));
    CHECK("thread 6b: v2 = 200",
		primitive_read_thread_variable(v2) == I(200));

	CHECK("thread 6b: v2 = 600",
		primitive_write_thread_variable(v2, I(600)) == I(600));

	primitive_wait_for_simple_lock(&simple_lock);
    primitive_wait_for_notification(&notification, &simple_lock);
	primitive_release_simple_lock(&simple_lock);

    CHECK("thread 6b: v1 = 100",
		primitive_read_thread_variable(v1) == I(100));
    CHECK("thread 6b: v2 = 600",
		primitive_read_thread_variable(v2) == I(600));
	CHECK("thread 6b: v3 = 100",
		primitive_read_thread_variable(v3) == I(100));

    return(TEST_OK);
}

Z test_t6()
{
	DTHREAD thread1, thread2;

	printf("\n6 - Thread variables\n");
	primitive_make_simple_lock(&simple_lock, NULL);
	primitive_make_notification(&notification, NULL);

	v1 = primitive_allocate_thread_variable(I(100));
	v2 = primitive_allocate_thread_variable(I(200));

	primitive_make_thread(&thread1, name, (ZINT)I(0), test_t6a);
	primitive_make_thread(&thread2, name, (ZINT)I(0), test_t6b);

	CHECK("v1 = 100",
		primitive_read_thread_variable(v1) == I(100));
	CHECK("v2 = 200",
		primitive_read_thread_variable(v2) == I(200));

	Sleep(1000);

	CHECK("assign v2 := 100",
		primitive_write_thread_variable(v2, I(100)) == I(100));
	CHECK("v1 = 100",
		primitive_read_thread_variable(v1) == I(100));
	CHECK("v2 = 100",
		primitive_read_thread_variable(v2) == I(100));

	v3 = primitive_allocate_thread_variable(I(100));

	primitive_wait_for_simple_lock(&simple_lock);
	primitive_release_all_notification(&notification, &simple_lock);
	primitive_release_simple_lock(&simple_lock);

	CHECK("v3 = 100",
		primitive_read_thread_variable(v3) == I(100));
	CHECK("v2 = 100",
		primitive_read_thread_variable(v2) == I(100));
	CHECK("v1 = 100",
		primitive_read_thread_variable(v1) == I(100));

	primitive_thread_join_single(&thread1);
	primitive_thread_join_single(&thread2);
    primitive_destroy_thread(&thread1);
	primitive_destroy_thread(&thread2);
}




int main()
{
 	DTHREAD thread;

    primitive_initialize_current_thread(&thread);


#ifdef TEST_SIMPLE_LOCK
	printf("Testing Simple Locks\n");
	test_sl1();
	test_sl2();
	test_sl3();
	test_sl4();
	test_sl5();
	test_sl6();
#endif


#ifdef TEST_RECURSIVE_LOCK
	printf("\n\nTesting Recursive Locks\n");
	test_rl1();
	test_rl2();
	test_rl3();
	test_rl4();
	test_rl5();
	test_rl6();
#endif


#ifdef TEST_SEMAPHORE
	printf("\n\nTesting Semaphores\n");
	test_s1();
	test_s2();
	test_s3();
	test_s4();
	test_s5();
#endif


#ifdef TEST_NOTIFICATION
	printf("\n\nTesting Notifications\n");
	test_n1();
	test_n2();
	test_n3();
	test_n4();
	test_n5();
#endif


#ifdef TEST_THREAD
	printf("\n\nTesting Threads\n");
    test_t1();
	test_t2();
	test_t3();
	test_t4();
	test_t5(&thread);
	test_t6();
#endif

    return(0);
}
