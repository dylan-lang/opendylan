/* ********************************************************************* */
/* ** nub-core.h                                                      ** */
/* ** Global type descriptions and macros used in the debugger nub.   ** */
/* ** --------------------------------------------------------------- ** */
/* ** Author: Paul Howard Copyright: (c) 1999 Functional Objects, Inc. ** */
/* ** Modified: yduJ                 All Rights Reserved              ** */
/* ****************************************************************** ** */

#include "utilities.h"
#include "nub-core-types.h"
#include "compat.h"
#include "bfd.h"

// Special function to perform validated writing to debugee's memory.

void *checked_malloc (int);
void checked_free(void*);

#define GLOBAL_STRING_SPACE_SIZE 1000

// The longest size allocated for strings.

#define MAX_PRINTABLE_REPRESENTATION_SIZE 200

// The longest size allocated for strings.

#define MAX_PRINTABLE_REPRESENTATION_SIZE 200

// Different types of register

#define GENERAL_PURPOSE 1
#define SPECIAL_PURPOSE 2
#define FLOATING 3

// Classifies a debug point.

#define DBG_POINT_BREAKPOINT 1
#define DBG_POINT_WATCHPOINT 2

// Classifies a breakpoint.

#define BREAKPOINT_PERMANENT 1
#define BREAKPOINT_SINGLE_STEP 2
#define APPLICATION_BREAKPOINT 3
#define INTERNAL_BREAKPOINT 4
#define UNKNOWN_BREAKPOINT 5
#define STACK_REPAIR_BREAKPOINT 6
#define CAPTURE_STEP_OUT_BREAKPOINT 7
#define CAPTURE_STEP_OVER_BREAKPOINT 8
#define CAPTURE_STEP_INTO_BREAKPOINT 9
#define BREAKPOINT_OUT_OF_CONTEXT 10

// Application state codes

#define STOPPED_BY_COMMAND 1
#define STOPPED_BY_EVENT 2
#define RUNNING 3
#define UNSTARTED 4
#define DEAD 5
#define POST_MORTEM 6
#define STOPPED_AT_BREAKPOINT 7

// Constants used in mapping Microsoft debug events to Access Path Interace
// <stop-reason> codes.

// These MUST agree with the codes specified in the Access Path.

#define MAX_EVENT_TYPES 35
#define TIMED_OUT 0
#define TIMED_OUT_HANDLED 32
#define TIMED_OUT_UNHANDLED 33
#define ACCESS_VIOLATION_EXCEPTION_DBG_EVENT 1
#define ARRAY_BOUNDS_EXCEPTION_DBG_EVENT 2
#define ILLEGAL_INSTRUCTION_EXCEPTION_DBG_EVENT 3
#define PRIVILEGED_INSTRUCTION_EXCEPTION_DBG_EVENT 4
#define DENORMAL_EXCEPTION_DBG_EVENT 5
#define FLOAT_DIVIDE_BY_ZERO_EXCEPTION_DBG_EVENT 6
#define INEXACT_RESULT_EXCEPTION_DBG_EVENT 7
#define INVALID_OPERATION_EXCEPTION_DBG_EVENT 8
#define FLOAT_OVERFLOW_EXCEPTION_DBG_EVENT 9
#define FLOAT_UNDERFLOW_EXCEPTION_DBG_EVENT 10
#define FLOAT_STACK_CHECK_EXCEPTION_DBG_EVENT 11
#define INTEGER_DIVIDE_BY_ZERO_EXCEPTION_DBG_EVENT 12
#define NONCONTINUABLE_EXCEPTION_DBG_EVENT 13
#define BREAKPOINT_EXCEPTION_DBG_EVENT 14
#define HARD_CODED_BREAKPOINT_DBG_EVENT 15
#define SINGLE_STEP_DBG_EVENT 16
#define CREATE_PROCESS_DBG_EVENT 17
#define EXIT_PROCESS_DBG_EVENT 18
#define CREATE_THREAD_DBG_EVENT 19
#define EXIT_THREAD_DBG_EVENT 20
#define LOAD_DLL_DBG_EVENT 21
#define UNLOAD_DLL_DBG_EVENT 22
#define RIP_DBG_EVENT 23
#define OUTPUT_DEBUG_STRING_DBG_EVENT 24
#define PROFILER_DBG_EVENT 25
#define PROFILER_UNHANDLED_DBG_EVENT 34
#define UNCLASSIFIED_DBG_EVENT 26
#define INTEGER_OVERFLOW_EXCEPTION_DBG_EVENT 27
#define STACK_OVERFLOW_EXCEPTION_DBG_EVENT 28
#define SOURCE_STEP_OVER_DBG_EVENT 29
#define SOURCE_STEP_OUT_DBG_EVENT 30
#define SOURCE_STEP_INTO_DBG_EVENT 31

// Special case
// This does not encode a stop-reason, and this code must _never_ be passed
// up to the access-path layer. This is just a special marker indicating
// that the debugger nub should simply filter out this event because it's
// uninteresting.

#define FILTER_HANDLED_DBG_EVENT 35
#define FILTER_UNHANDLED_DBG_EVENT 36
#define SPY_RETURN_DBG_EVENT 37

#define EXCEPTIONS_HANDLED 1
#define EXCEPTIONS_UNHANDLED 2

// Stop-reason loop modes.

#define STOP_REASON_WAIT_NORMAL 1
#define STOP_REASON_WAIT_SPY 2

// Register codes.

#define NUB_REGISTER_ILLEGAL 0

#define FIRST_REGISTER 1
#define LAST_REGISTER 24
#define FIRST_GENERAL_REGISTER 1
#define LAST_GENERAL_REGISTER 7
#define FIRST_SPECIAL_REGISTER 8
#define LAST_SPECIAL_REGISTER 16
#define FIRST_FLOATING_REGISTER 17
#define LAST_FLOATING_REGISTER 24

//+++ x86 specific.  move to another file?
#define NUB_REGISTER_EAX 1
#define NUB_REGISTER_EDX 2
#define NUB_REGISTER_ECX 3
#define NUB_REGISTER_EBX 4
#define NUB_REGISTER_EBP 5
#define NUB_REGISTER_ESI 6
#define NUB_REGISTER_EDI 7

#define NUB_REGISTER_ESP 8
#define NUB_REGISTER_EIP 9
#define NUB_REGISTER_EFL 10
#define NUB_REGISTER_CS 11
#define NUB_REGISTER_DS 12
#define NUB_REGISTER_SS 13
#define NUB_REGISTER_ES 14
#define NUB_REGISTER_FS 15
#define NUB_REGISTER_GS 16

#define NUB_REGISTER_FLT0 17
#define NUB_REGISTER_FLT1 18
#define NUB_REGISTER_FLT2 19
#define NUB_REGISTER_FLT3 20
#define NUB_REGISTER_FLT4 21
#define NUB_REGISTER_FLT5 22
#define NUB_REGISTER_FLT6 23
#define NUB_REGISTER_FLT7 24

// ************************* STRUCTURES AND TYPES ************************

#define MAX_PROCESS_NAME_LENGTH 256
#define MAX_MODULE_NAME_LENGTH 256
#define MAX_PROCESS_ID_LENGTH 256
#define MODULE_DESCRIPTOR_ALLOWANCE 512

typedef struct _PROCESSD {
  char                ProcessName[MAX_PROCESS_NAME_LENGTH];
  DWORD               ProcessNameLength;
  char                ProcessID[MAX_PROCESS_ID_LENGTH];
  DWORD               ProcessIDLength;
  DWORD               ActualProcessID;
  BOOL                Mark;
  struct _PROCESSD   *Next;
} PROCESSD, *LPPROCESSD;

#include "nub_interface.h"

typedef struct _DBGTHREAD *LPDBGTHREAD;
typedef struct _DBGPROCESS *LPDBGPROCESS;
typedef struct _DBGLIBRARY *LPDBGLIBRARY;


typedef struct _DBGPROCESS {
  NUBINT                     ProcessID;
  char                      *Command;
  char                      *Arguments;
  char                      **SymbolPaths;
  char                      *WorkingDirectory;
  char                      **LibrarySearchPaths;
  bfd                       *object_file;
  LPDBGTHREAD                ThreadList;
  LPDBGLIBRARY               LibraryList;
  BOOL                       ProcessAttached;
  int                        State;
} DBGPROCESS;

typedef struct _DBGTHREAD {

  int                    *ThreadID;
  char                   *ThreadName;
  BOOL                    ThreadActive;
  int                     ThreadState;
  BOOL                    WaitingForDebugger;
  BOOL                    SingleStepping;
  BOOL                    FrozenDuringSpyCall;
  DWORD                   AddressOfSpyBreakpoint;
  int                     StoppedState;
  LPDBGTHREAD             Next;

} DBGTHREAD;

typedef struct _DBGLIBRARY {
  LPDBGLIBRARY             Next;

} DBGLIBRARY;
