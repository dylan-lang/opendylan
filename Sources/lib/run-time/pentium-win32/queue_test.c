/* File:      queue.c
 * Author:    Keith Dennison
 * Created:   8 October, 1996
 * Copyright: 1996 Functional Objects, Inc. All rights reserved.
 *
 * Tests the thread primitives by implementing a queue based on an
 * example in D-doc-lib!threads.doc
 *
 * This example implements a queue using a notifcation and lock, and
 * creates four threads - two which write to the queue, and two which
 * read from it.
 * The output when sorted according to the timestamps, should be a mixed list
 * of alphabetic and numeric characters. The letters and numbers should appear
 * in order.
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

#include "win32-threads-primitives.h"


/* Comment this definition out to see the difference */
#define SYNCHRONISE


typedef struct element {
	struct element *next;
	void *value;
} ELEMENT;

typedef struct queue {
	struct element *first;
	struct element *last;
	CONTAINER notification, lock;
} QUEUE;



/* User calls this to initialise the queue */
void q_initialise(QUEUE *queue)
{
	queue->first = queue->last = NULL;
}


/* Test for empty queue */
int q_empty(QUEUE *queue)
{
	return(queue->first == NULL);
}


/* Internal implementation of pushing something on the queue */
void q_push(QUEUE *queue, void *value)
{
	ELEMENT *element;

	element = malloc(sizeof(ELEMENT));
	assert(element != NULL);
	element->next = NULL;
	element->value = value;
	if (queue->first == NULL)
		queue->first = queue->last = element;
	else {
		queue->last->next = element;
		Sleep(0);
		queue->last = element;
	}
}


/* Internal implementation for popping something off the front of the queue */
void *q_pop(QUEUE *queue)
{
	ELEMENT *element;
	void *value;

	assert(queue->first != NULL);

	element = queue->first;
	value = element->value;
	if (element == queue->last)
		queue->first = queue->last = NULL;
	else
		queue->first = element->next;
	free(element);
	return(value);
}


/* User calls this to put something on the queue
 */
void put_on_queue(QUEUE *queue, void *value)
{
#ifdef SYNCHRONISE
	primitive_wait_for_simple_lock(&queue->lock);
	if (q_empty(queue)) {
		primitive_release_all_notification(&queue->notification,
			&queue->lock);
	}
#endif

	q_push(queue, value);

#ifdef SYNCHRONISE
	primitive_release_simple_lock(&queue->lock);
#endif
}


/* User calls this to get something from the front of the queue. Returns a
 * timestamp to indicate when the item was removed as well as the value.
 */
void *get_from_queue(QUEUE *queue, DWORD *time)
{
	void *value;

#ifdef SYNCHRONISE
	primitive_wait_for_simple_lock(&queue->lock);
#endif

	while(q_empty(queue)) {
#ifdef SYNCHRONISE
		primitive_wait_for_notification(&queue->notification,
			&queue->lock);
#endif
	}
	*time = GetTickCount();
	value = q_pop(queue);

#ifdef SYNCHRONISE
	primitive_release_simple_lock(&queue->lock);
#endif

	return(value);
}



/* The queue which is shared by all threads */
QUEUE q;
DWORD start;

/* There are two threads which write to the queue. The first adds alphabetic
 * characters in order. the second adds numerals in order.
 */
Z feeder1(Z o, int n, ...)
{
	char c;

	for(c = 'a'; c <= 'z'; c++) {
		put_on_queue(&q, (void *)c);
		Sleep(rand() % 50);
	}
	put_on_queue(&q, (void *)('\0'));
	return(NULL);
}

Z feeder2(Z o, int n, ...)
{
	char c;

	for(c='0'; c <= '9'; c++) {
		put_on_queue(&q, (void *)c);
		Sleep(rand() % 100);
	}
	put_on_queue(&q, (void *)('\0'));
	return(NULL);
}

/* And there are two threads which read from the queue. Each pops an element
 * from the queue and sends it to standard output with a timestamp.
 */

LONG counter = 0;

Z reader1(Z o, int n, ...)
{
	DWORD time;
	char ch;

	do {
		ch = (char)get_from_queue(&q, &time);
		if (ch == '\0')
			InterlockedIncrement(&counter);
		else
			printf("%010d:%c\n", time-start, ch);
		Sleep(rand() % 100);
	} while (counter < 1);
	return(NULL);
}

Z reader2(Z o, int n, ...)
{
	DWORD time;
	char ch;
	int counter = 0;

	do {
		ch = (char)get_from_queue(&q, &time);
		if (ch == '\0')
			InterlockedIncrement(&counter);
		else
			printf("%010d:%c\n", time-start, ch);
		Sleep(rand() % 50);
	} while (counter < 1);
	return(NULL);
}



main()
{
	DTHREAD thread;
	DTHREAD tfeeder1, tfeeder2, treader1, treader2;

    primitive_initialize_current_thread(&thread);

	q_initialise(&q);

	assert(primitive_make_simple_lock(&q.lock, NULL) == 1);
	assert(primitive_make_notification(&q.notification, NULL) == 1);

	start = GetTickCount();

	/* Start the threads */
	assert(primitive_make_thread(&tfeeder1, NULL, (ZINT)I(0), feeder1) == 1);
	assert(primitive_make_thread(&tfeeder2, NULL, (ZINT)I(0), feeder2) == 1);
	assert(primitive_make_thread(&treader1, NULL, (ZINT)I(0), reader1) == 1);
	assert(primitive_make_thread(&treader2, NULL, (ZINT)I(0), reader2) == 1);

	/* Wait for the threads to terminate */
	primitive_thread_join_single(&treader1);
	primitive_thread_join_single(&treader2);
	primitive_thread_join_single(&tfeeder1);
	primitive_thread_join_single(&tfeeder2);

	primitive_destroy_notification(&q.notification);
	primitive_destroy_simple_lock(&q.lock);
	return(0);
}
