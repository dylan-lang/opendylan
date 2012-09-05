/* INTERIM DYLAN RUN-TIME SYSTEM INTERFACE
 *
 * Copyright (C) 1996 Functional Objects, Inc. All rights reserved
 */

#define _GNU_SOURCE

#include <sys/signal.h>

#include "collector.c"


/* Heap stat stuff */

void clear_wrapper_stats ()
{
}


void display_wrapper_stats ()
{
}

void add_stat_for_object (void *object, void* wrapper, int size)
{
}

typedef struct wrapper_stats_s *wrapper_stats_t;

typedef struct wrapper_stats_s {
  void *wrapper_address;
  int  usage_count;
  int  usage_size;
} wrapper_stats_s;

void display_stats_for_memory_usage ()
{
}

void display_wrapper_breakpoints()
{
}

#include "break.c"

