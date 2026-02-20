/*
 * This pulls in the required sources from MPS.
 *
 * By building this way, we can let a lot of things get inlined
 * and optimized within MPS for good performance.
 *
 * See http://www.ravenbrook.com/project/mps/master/manual/build
 * for details.
 */

#ifdef OPEN_DYLAN_PLATFORM_WINDOWS
#define CONFIG_PLINTH_NONE
#endif

#include "mps.c"
#include "fmtdy.c"
#include "fmtno.c" // fmtdy "inherits" from fmtno.
