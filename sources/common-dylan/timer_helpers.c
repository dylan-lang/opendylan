#ifdef OPEN_DYLAN_PLATFORM_WINDOWS

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

typedef signed __int64 int64_t;
typedef unsigned int   uint32_t;
typedef int            int32_t;

void timer_get_point_in_time(uint32_t time[2])
{
  int64_t now, frequency;
  long seconds = 0,
       microseconds = 0;

  QueryPerformanceCounter(&now);
  if (QueryPerformanceFrequency(&frequency)) {
    seconds = (long)(now / frequency);
    microseconds = (long)((now % frequency) * 1000000 / frequency);
  }

  time[0] = seconds;
  time[1] = microseconds * 1000;
}

#elif OPEN_DYLAN_PLATFORM_DARWIN

/* From https://developer.apple.com/library/mac/qa/qa1398/_index.html */
#include <assert.h>
#include <mach/mach.h>
#include <mach/mach_time.h>
#include <stdint.h>
#include <unistd.h>

void timer_get_point_in_time(uint32_t time[2])
{
    uint64_t        now;
    uint64_t        nowNano;
    static mach_timebase_info_data_t    sTimebaseInfo;

    now = mach_absolute_time();

    // Convert to nanoseconds.

    // If this is the first time we've run, get the timebase.
    // We can use denom == 0 to indicate that sTimebaseInfo is
    // uninitialised because it makes no sense to have a zero
    // denominator is a fraction.

    if ( sTimebaseInfo.denom == 0 ) {
        (void) mach_timebase_info(&sTimebaseInfo);
    }

    // Do the maths. We hope that the multiplication doesn't
    // overflow; the price you pay for working in fixed point.

    nowNano = now * sTimebaseInfo.numer / sTimebaseInfo.denom;

    time[0] = now / 1000000000;
    time[1] = now % 1000000000;
}

#else

#include <stdint.h>
#include <time.h>
#include <unistd.h>

void timer_get_point_in_time(uint32_t time[2])
{
#if _POSIX_TIMERS > 0
  struct timespec now;

# if _POSIX_MONOTONIC_CLOCK > 0
  clock_gettime(CLOCK_MONOTONIC, &now);
# else
  clock_gettime(CLOCK_REALTIME, &now);
# endif

  time[0] = now.tv_sec;
  time[1] = now.tv_nsec;

#else
# error Need clock implementation.
#endif

}

#endif

void timer_accumulated_time(uint32_t starting_sec,
                            uint32_t starting_nsec,
                            uint32_t stopping_sec,
                            uint32_t stopping_nsec,
                            uint32_t accumulated_time[2])
{
  int32_t seconds = stopping_sec - starting_sec;
  int32_t nanoseconds = stopping_nsec - starting_nsec;

  if (nanoseconds < 0) {
    seconds -= 1;
    nanoseconds += 1000000000;
  }

  accumulated_time[0] = seconds;
  // microseconds!
  accumulated_time[1] = nanoseconds / 1000;
}
