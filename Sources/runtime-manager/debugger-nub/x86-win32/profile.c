/* ********************************************************************** */
/* ** profile.c                                                        ** */
/* ** Functions to support profiling.                                  ** */
/* **                                                                  ** */
/* ** ---------------------------------------------------------------- ** */
/* ** Author: Keith Dennison                                           ** */
/* ** Copyright: Functional Objects, Inc. 1996, 1997                ** */
/* **            All Rights Reserved                                   ** */
/* ********************************************************************** */

#include "nub-core.h"

int get_os_wall_clock_time (LPDBGPROCESS process);

void nub_profile_wait_for_stop_reason_no_timeout 
  (NUB nub,
   NUBINT profile_interval,
   NUBINT *event_code)
{
  // Wait for a stop reason with a timeout equal to the profile interval.
  // This way we will only generate a profile event if no other stop reason
  // occurs during the profile interval.

  // This behaviour has now been changed so that the requested profile interval
  // overrides any other continuable stop-reasons (e.g. page faults) and breaks 
  // to the debugger to take profile snapshots, so we get much more reliable 
  // profiling results
  // Nosa  Jun 22, 1999

  nub_wait_for_stop_reason_with_timeout(nub, profile_interval, event_code);

  // If we get a timeout convert it to a profiler event making sure the
  // application is stopped. Increment the estimated cpu time counter.

  switch (*event_code) {

  case TIMED_OUT:
  case TIMED_OUT_HANDLED:
    *event_code = PROFILER_DBG_EVENT;
    break;

  case TIMED_OUT_UNHANDLED:
    *event_code = PROFILER_UNHANDLED_DBG_EVENT;
    break;

  default:
    return;
  }

  nub_application_stop(nub);
  os_time_estimate_counter += profile_interval;

}


void nub_profile_wait_for_stop_reason_with_timeout
  (NUB nub,
   NUBINT timeout,
   NUBINT profile_interval,
   NUBINT *stop_code)
{
  // Not implemented yet. Needed??
}


void nub_inform_profiling_started (NUB nub)
{
  // This function is called from the profiler manager (via access path)
  // when profiling is turned on. It raises the nub's thread's priority
  // so that the profiler manager gets profile interval closer to those
  // it requested.

  SetThreadPriority (GetCurrentThread(), THREAD_PRIORITY_TIME_CRITICAL);
}

void nub_inform_profiling_stopped (NUB nub)
{
  // This function is called from the profiler manager (via access path)
  // when profiling is turned off. It lowers the nub's thread's priority
  // back to normal.

  SetThreadPriority (GetCurrentThread(), THREAD_PRIORITY_NORMAL);
}

NUBINT nub_get_process_wall_clock_time (NUB nub)
{
  LPDBGPROCESS   process = (LPDBGPROCESS) nub;
  return (process->CumulativeWallClockTime);
}

NUBINT proxy_nub_get_process_wall_clock_time (NUB nub)
{
  return((NUBINT) 0);
}
