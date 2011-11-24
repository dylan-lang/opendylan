/* INTERIM DYLAN RUN-TIME SYSTEM INTERFACE
 *
 * Copyright (C) 1996 Functional Objects, Inc. All rights reserved
 */


/* flag to select Boehm GC */
//#define BOEHM_GC

#include "wrapper-stats.h"

#define NO_LEAF_OBJECT
/* maybe? */
#define NO_WEAKNESS

#include "collector.c"


void clear_wrapper_stats ()
{
}


void display_wrapper_stats ()
{
}

void add_stat_for_object (void *object, void* wrapper, int size)
{
}

void display_wrapper_breakpoints()
{
}

#include "break.c"
