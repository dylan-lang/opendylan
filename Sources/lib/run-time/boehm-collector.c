/* INTERIM DYLAN RUN-TIME SYSTEM INTERFACE
 *
 * $HopeName: D-lib-pentium-run-time!boehm-collector.c(trunk.2) $
 * Copyright (C) 1996 Functional Objects, Inc. All rights reserved
 */


/* flag to select Boehm GC */
#define BOEHM_GC

#include "wrapper-stats.h"
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
