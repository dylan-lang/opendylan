/* ********************************************************************* */
/* ** nub-core.h                                                      ** */
/* ** Global type descriptions and macros used in the debugger nub.   ** */
/* ** --------------------------------------------------------------- ** */
/* ** Author: Paul Howard Copyright: (c) 1996 Functional Objects, Inc. ** */
/* **                                All Rights Reserved              ** */
/* ****************************************************************** ** */

#include <windows.h>
#include "IMAGEHLP.H"
#include "coff-extract.h"
#include "cv-extract.h"
#include "cv-types.h"

// Newer version of IMAGEHLP.H has deprecated some structure members we use.

#if defined(DBHLP_DEPRECIATED) || defined(DBHLP_DEPRECATED)
#define CodeViewSymbols ReservedCodeViewSymbols
#define SizeOfCodeViewSymbols ReservedSizeOfCodeViewSymbols
#define Sections ReservedSections
#define FunctionTableEntries ReservedFunctionTableEntries
#endif

// Special function to perform validated writing to debugee's memory.

BOOL ValidatedWriteProcessMemory
  (HANDLE hProcess, LPVOID lpBaseAddress, LPVOID lpBuffer,
   DWORD nSize, LPDWORD lpNumberOfBytesRead);

void *checked_malloc (int);
void checked_free(void*);

#define malloc(x) HeapAlloc(GetProcessHeap(), 0, (x))
#define free(x) HeapFree(GetProcessHeap(), 0, (x))

#define GLOBAL_STRING_SPACE_SIZE 1000

#define DBG_TRANSPORT_NOT_SUPPORTED 0

// Different types of debugging information. (These are not
// mutually exclusive, hence the use of powers of two so
// that they can be combined with OR).

#define NONE 0
#define CODEVIEW_PDB 1
#define CODEVIEW_IMAGE 2
#define COFF_PDB 4
#define COFF_IMAGE 8
#define NOT_YET_LOADED 16

// Languages we know about, including one we've sneaked in...

#define C_LANGUAGE 0
#define C_PLUS_PLUS_LANGUAGE 1
#define FORTRAN_LANGUAGE 2
#define MASM_LANGUAGE 3
#define PASCAL_LANGUAGE 4
#define BASIC_LANGUAGE 5
#define COBOL_LANGUAGE 6
#define DYLAN_LANGUAGE 9

// The longest size allocated for strings.

#define MAX_PRINTABLE_REPRESENTATION_SIZE 200

// This indicates the maximum size of an Intel
// instruction. I'm sure it's smaller than this, but
// the breathing space won't hurt.
 
#define MAX_SAVED_SEGMENT_SIZE 30

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

// Structure to define information about debug events.

typedef struct _DBG_EVENT_STRUCT {
  char          *DebugEventName;
  BOOL           ReceivingFirstChance;
  BOOL           ReceivableFirstChance;
} DBG_EVENT_STRUCT;

// Definitions for building a queue of debug events.

typedef struct _DEBUG_EVENT_QUEUE_NODE {
  DEBUG_EVENT                      EventSpace;
  struct _DEBUG_EVENT_QUEUE_NODE  *Next;
} DEBUG_EVENT_QUEUE_NODE;

// Debug events are popped from the head of the queue, and pushed
// onto the tail. The queue can be emptied by nub_flush_all_stop_reasons.

typedef struct _DEBUG_EVENT_QUEUE {
  DEBUG_EVENT_QUEUE_NODE       *Head;
  DEBUG_EVENT_QUEUE_NODE       *Tail;
  int                           Size;
} DEBUG_EVENT_QUEUE;

// Debugger nub error codes.

#define ACCESS_VIOLATION_ERROR 1
#define ACCESS_OK 0

#define BAD_PROCESS_HANDLE 1
#define BAD_THREAD_HANDLE 2
#define BAD_THREAD_DESCRIPTOR 3
#define BAD_THREAD_INDEX 4
#define BAD_LIBRARY_HANDLE 5
#define BAD_LIBRARY_INDEX 6
#define APPLICATION_CONTINUE 7

#define NOT_SUPPORTED 0
#define BREAKPOINT_ALREADY_EXISTS 1
#define BREAKPOINT_DOES_NOT_EXIST 2
#define WATCHPOINT_ALREADY_EXISTS 3
#define WATCHPOINT_DOES_NOT_EXIST 4
#define SET_BREAKPOINT_FAILED 5
#define CLEAR_BREAKPOINT_FAILED 6
#define OK 7
#define BREAKPOINT_WAS_DISABLED 8

#include "nub-core-types.h"

extern char global_string_space [GLOBAL_STRING_SPACE_SIZE];

// Conditionally display debugger messages for thread activities on this side of the tether

#if defined(DEBUGGING)
void debugger_message(char*, TARGET_ADDRESS, TARGET_ADDRESS);
#define dylan_debugger_message(message, arg1, arg2) \
  debugger_message(message, (TARGET_ADDRESS)arg1, (TARGET_ADDRESS)arg2)
#else
#define dylan_debugger_message(message, arg1, arg2)
#endif


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

typedef struct _MODULED {
  char                ModuleName[MAX_MODULE_NAME_LENGTH];
  DWORD               ModuleBase;
  struct _MODULED     *Next;
} MODULED, *LPROCESSD;

typedef int RegisterType;

// Used for describing the limits of source locations, or any pair
// of 32-bit offsets.

typedef struct _OFFSET_PAIR {

  DWORD       Start, End;

} OFFSET_PAIR;

// The INTERACTIVE_CODE_SEGMENTS data type is a dynamically extendable
// table of instruction pointer boundaries. Whenever a unit of interactive
// code is created by some means, the debugger nub must be told (via the
// API nub_register_interactive_code_segment), and an entry created in
// this table.

#define INTERACTIVE_CODE_TABLE_GRANULARITY 256

typedef struct _INTERACTIVE_SEGMENT {
  DWORD        SegmentLowerBoundary;
  DWORD        SegmentUpperBoundary;
} INTERACTIVE_SEGMENT;

typedef struct _INTERACTIVE_SEGMENT_TABLE {
  INTERACTIVE_SEGMENT        Segments[INTERACTIVE_CODE_TABLE_GRANULARITY];
  DWORD                               NextFreeIndex;
  struct _INTERACTIVE_SEGMENT_TABLE  *Next;
} INTERACTIVE_SEGMENT_TABLE;

typedef struct _INTERACTIVE_CODE_SEGMENTS {
  INTERACTIVE_SEGMENT_TABLE  *FirstSegmentTable;
  INTERACTIVE_SEGMENT_TABLE  *CurrentSegmentTable;
} INTERACTIVE_CODE_SEGMENTS;


// A debug-point descriptor. Describes any breakpoint or watchpoint that
// has been set in the process by the debugger nub.

typedef struct _DEBUG_POINT *LPDEBUG_POINT;
typedef struct _DBGTHREAD *LPDBGTHREAD;

typedef struct _BREAKPOINT *LPDBGBREAKPOINT;

typedef struct _BREAKPOINT {

  // Type is what kind of breakpoint this is.
  // One of: APPLICATION_BREAKPOINT, INTERNAL_BREAKPOINT.
  // (see debug_points.c for a description of breakpoints).

  int             Type;

  // Address is where in the process the breakpoint is written.

  DWORD           Address;
 
  BOOL            BreakpointEnabled;

  // SavedCodeSegment stores the instruction that was overwritten
  // by the breakpoint, or part thereof.

  BYTE            SavedCodeSegment;

  // Context of the breakpoint. These fields are not used for global
  // breakpoints.

  LPDBGTHREAD     ThreadContext;
  DWORD           FramePointerContext;
  DWORD           CallingFramePointerContext;

  // ID of the thread that wrote the breakpoint:

  DWORD           ThreadID;
 
  // If this is an INTERNAL_BREAKPOINT, it uses this field to point
  // back to the breakpoint that must be re-written.

  LPDBGBREAKPOINT  *AssociatedBreakpoint;

} DBGBREAKPOINT;


// TODO:
// Implement watchpoints!

typedef struct _WATCHPOINT *LPDBGWATCHPOINT;

typedef struct _WATCHPOINT {

  BOOL          ReadWatchpoint, WriteWatchpoint, ExecuteWatchpoint;
  NUBHANDLE     Address;
  int           Size;
  BYTE          SavedCodeSegment[MAX_SAVED_SEGMENT_SIZE];

} DBGWATCHPOINT;

typedef struct _DEBUG_POINT {

  // DebugPointType (breakpoint or watchpoint?)
 
  int           DebugPointType;

  // One of the above descriptors:

  union {

    DBGBREAKPOINT    Breakpoint;
    DBGWATCHPOINT    Watchpoint;

  } u;

  // Active breakpoints are stored in a list. This is the link field.

  LPDEBUG_POINT Next;

} DEBUG_POINT;


// States that threads can be in when the application is stopped:

#define THREAD_FROZEN 1
#define THREAD_DEAD 0
#define THREAD_STOPPED_BY_DEBUGGER 2
#define THREAD_STOPPED_AT_KNOWN_BREAKPOINT 3
#define THREAD_STOPPED_AT_FIRST_CHANCE_EXCEPTION 4
#define THREAD_STOPPED_AT_SECOND_CHANCE_EXCEPTION 5
#define THREAD_STOPPED_AT_DEBUG_EVENT 6
#define THREAD_INVOKED_DEBUGGER 7
#define THREAD_STOPPED_AT_SINGLE_STEP 8;
#define THREAD_STOPPED_FOR_BREAKPOINT_REPLACEMENT 9;


// A stack frame descriptor
// (Communicates between nub and access-path as NUBFRAME)

typedef struct _DBGFRAME *LPDBGFRAME;

typedef struct _DBGFRAME {

  // Include a pointer back to the thread.

  LPDBGTHREAD             StackFrameThread;

  // StackWalk communicates via STACKFRAME structures, so we
  // include one of these in the debugger nub's own object.

  BOOL                    IsStackWalkFrame;
  STACKFRAME              StackWalkFrame;

  // The following are resorted to when StackWalkFrame is NULL.

  DWORD                   FramePointer;
  DWORD                   InstructionAddress;
  DWORD                   ReturnAddress;
  BOOL                    IsFpoFrame;
  FPO_DATA                FpoRecord;

  // A flag for when this frame is known to be the last available.

  BOOL                    IsLastFrame;

  // Link Pointer

  LPDBGFRAME              PreviousFrame;

} DBGFRAME;
  
// A thread descriptor.
// (Communicates between nub and access-path as a NUBTHREAD)

#define STACK_TRACE_MAX_SIZE 512

typedef struct _DBGTHREAD {

  HANDLE                  ThreadHandle;
  char                   *ThreadName;
  DWORD                   ThreadID;
  LPCONTEXT               ThreadContext;
  LPCONTEXT               CachedThreadContext;
  LPTHREAD_START_ROUTINE  StartRoutineAddress;
  LPDBGFRAME              CurrentStackTrace;           // Obsolescent
  BOOL                    StackTraceValid;
  BOOL                    StackTraceUsingOverflow;
  int                     StackTraceSize;
  DWORD                   StackTraceStackPointers[STACK_TRACE_MAX_SIZE];
  DWORD                   StackTraceFramePointers[STACK_TRACE_MAX_SIZE];
  DWORD                   StackTraceInstrPointers[STACK_TRACE_MAX_SIZE];
  DWORD                   StackTraceRetrnPointers[STACK_TRACE_MAX_SIZE];
  LPVOID                  ThreadLocalStorageAddress;
  BOOL                    ThreadActive;
  int                     ThreadPriority;
  int                     ThreadState;
  BOOL                    Valid;
  BOOL                    WaitingForDebugger;
  BOOL                    SingleStepping;
  BOOL                    NeedsBreakpointReplacement;
  BOOL                    FrozenDuringSpyCall;
  DWORD                   AddressOfSpyBreakpoint;
  LPDEBUG_POINT           BreakpointToReplace;
  LPDEBUG_POINT           SteppingCaptureBreakpoints;
  int                     StoppedState;
  DEBUG_EVENT             LastReceivedEvent;
  NUBINT                  NubCodeOfLastEvent;
  LPDBGTHREAD             Next;

} DBGTHREAD;

typedef struct _THREAD_MEMORY {

  int                     ThreadState;
  BOOL                    WaitingForDebugger;
  BOOL                    SingleStepping;
  BOOL                    NeedsBreakpointReplacement;
  LPDEBUG_POINT           BreakpointToReplace;
  int                     StoppedState;
  DEBUG_EVENT             LastReceivedEvent;
  NUBINT                  NubCodeOfLastEvent;
  CONTEXT                 ThreadContext;

} THREAD_MEMORY;


// A cache for storing the delimiting addresses of function definitions.
// This is to improve the speed of nub_function_bounding_addresses, and
// consequently the speed of the profiler.

typedef struct _BOUNDARY_CACHE_ENTRY {

  DWORD                   HitCount;
  DWORD                   LowerBound;
  DWORD                   UpperBound;
  DWORD                   AddrAtLastHit;

} BOUNDARY_CACHE_ENTRY;

#define MAX_BOUNDARY_CACHE_ENTRIES 1024

typedef struct _BOUNDARY_CACHE {

  BOOL                    CacheFull;
  DWORD                   IndexNextEntry;
  DWORD                   IndexLastAddition;
  BOUNDARY_CACHE_ENTRY    EntryTable[MAX_BOUNDARY_CACHE_ENTRIES];

} BOUNDARY_CACHE;

// A library descriptor.
// (Communicates between nub and access-path as NUBLIBRARY)

typedef struct _DBGLIBRARY *LPDBGLIBRARY;

typedef struct _DBGLIBRARY {

  IMAGE_INFORMATION        ImageInformation;
  BOUNDARY_CACHE           BoundaryCache;
  PIMAGE_DEBUG_INFORMATION DebugMap;
  HANDLE                   SymbolFile;
  DWORD                    Subsection, Symbol;
  BOOL                     CachedTypeTableBase;
  void                    *TypeTableBase;
  CV_HEADER               *SymbolPointer;
  BYTE                     DebugType;
  BOOL                     SymbolHandlerWorking;
  IMAGEHLP_MODULE          ImagehlpModuleStruct;
  char                     DefaultImageName[256];
  BOOL                     Valid;
  LPDBGLIBRARY             Next;

} DBGLIBRARY;

// Function type for a register getter.

typedef DWORD (*PREGISTER_VALUE_GETTER) (LPDBGPROCESS, LPDBGTHREAD);

// Function type for a register setter.

typedef void (*PREGISTER_VALUE_SETTER) (LPDBGPROCESS, LPDBGTHREAD, DWORD);

// Describes a register.

typedef struct _REGISTER_DESCRIPTOR {

  char                    *Name;
  PREGISTER_VALUE_GETTER  Getter;
  PREGISTER_VALUE_SETTER  Setter;

} REGISTER_DESCRIPTOR;

typedef struct _DBGPROCESS *LPDBGPROCESS;

// EXT_IMAGEHLP_SYMBOL
// This is the same as the Win32-defined IMAGEHLP_SYMBOL, but contains
// more space for name information.

typedef struct _EXT_IMAGEHLP_SYMBOL {
  DWORD          SizeOfStruct;
  DWORD          Address;
  DWORD          Size;
  DWORD          Flags;
  DWORD          MaxNameLength;
  char           Name[256];
} EXT_IMAGEHLP_SYMBOL;


// DBGPROCESS
// Describes the running process.
// This is communicated between the nub and access-path as a NUB, but
// this is probably wrong. If we have a model of one nub per process,
// this descriptor could just be held somewhere in a static variable.
// (We'd still need this typedef, of course).

#define PLATFORM_WINDOWS_NT 1
#define PLATFORM_WINDOWS_95 2

typedef struct _DBGPROCESS {

  DWORD                      ProcessID;
  DWORD                      MainThreadID;
  char                      *Command;
  char                      *Arguments;
  char                      *SymbolPaths;
  char                      *WorkingDirectory;
  char                      *LibrarySearchPaths;
  char                      *NameCache;
  int                        State;
  HANDLE                     DebugHeap;
  HANDLE                     ProcessHandle;
  HANDLE                     JITEventHandle;
  LPDBGTHREAD                ThreadList;
  LPDBGTHREAD                ThreadRunningSpy;
  LPDBGTHREAD                ThreadCreatedDuringSpyCall;
  LPDBGTHREAD                PendingEventThread;
  LPDBGLIBRARY               LibraryList;
  LPDEBUG_POINT              DebugPointList;
  DEBUG_EVENT                PendingEvent;
  DEBUG_EVENT_QUEUE          EventQueue;
  DBG_EVENT_STRUCT           ExceptionInformation[MAX_EVENT_TYPES];
  PROCESS_INFORMATION        ProcessInfoContainer;
  BOOL                       SymbolHandlerWorking;
  INTERACTIVE_CODE_SEGMENTS  InteractiveCodeSegments;
  EXT_IMAGEHLP_SYMBOL        SymbolBuffer;
  EXT_IMAGEHLP_SYMBOL        SymbolAheadBuffer;
  EXT_IMAGEHLP_SYMBOL        SymbolBehindBuffer;
  BOOL                       SymbolBufferValid;
  BOOL                       SymbolAheadBufferValid;
  BOOL                       SymbolBehindBufferValid;
  BOOL                       ProcessAttached;
  MODULED                   *ModuleDescriptors;
  int                        HardCodedBreakpointCounter;
  STARTUPINFO                StartupInfoContainer;
  int                        Platform;
  NUBINT                     LastActivationWallClockTime;
  NUBINT                     CumulativeWallClockTime;
  TARGET_ADDRESS             ExitProcessFunction;
  BOOL                       ExitingProcess;
} DBGPROCESS;

// SYMBOL_LOOKUP_ENTRY
// Abstracts over symbol descriptions, which can vary depending on
// what kind of debug information is available. Various routines
// will be implemented which interpret Pointer based on LookupType.

typedef struct _SYMBOL_LOOKUP_ENTRY {

  BYTE                      LookupType;
  BYTE                      LanguageCode;
  void                     *Pointer;

} SYMBOL_LOOKUP_ENTRY;

#define MAPPED_CODEVIEW_SYMBOL 1
#define MAPPED_COFF_SYMBOL 2
#define PTR_IMAGEHLP_SYMBOL 3
#define LOOKUP_TABLE_SEGMENT_SIZE 10

typedef int SYMBOL_CLASSIFICATION;

#define STATIC_SYMBOL 1
#define GLOBAL_SYMBOL 2
#define EXPORTED_SYMBOL 3

typedef struct _LOOKUP_TABLE_SEGMENT {

  SYMBOL_LOOKUP_ENTRY          SegmentEntries[LOOKUP_TABLE_SEGMENT_SIZE];
  struct _LOOKUP_TABLE_SEGMENT *NextSegment;

} LOOKUP_TABLE_SEGMENT;

typedef struct _LOOKUP_TABLE {

  LPDBGPROCESS                 Process;
  LPDBGLIBRARY                 Module;
  LOOKUP_TABLE_SEGMENT        *FirstSegment;
  LOOKUP_TABLE_SEGMENT        *LastSegment;
  DWORD                        LastEntry;

} LOOKUP_TABLE;

typedef struct _LOWLEVEL_SOURCE_LOCATION {

  DWORD                        Offset;
  WORD                         LineNumber;

} LOWLEVEL_SOURCE_LOCATION;

///// 
///// ******** LOOKING UP SOURCE LOCATIONS ***********
/////

#define MAX_SOURCE_LOCATIONS 500

typedef struct _SL_LOOKUP_TABLE {

  LPDBGPROCESS                 Process;
  LPDBGLIBRARY                 Module;
  DWORD                        BaseAddress;
  WORD                         SegmentIndex;
  DWORD                        SegmentAddress;
  int                          NumberOfEntries;
  BYTE                         FilenameLength;
  char                        *Filename;
  LOWLEVEL_SOURCE_LOCATION     Locations[MAX_SOURCE_LOCATIONS];

} SL_LOOKUP_TABLE;


/////
///// **************** TYPE INFORMATION ****************
///// 

#define NUBTYPE_PRIMITIVE 1
#define NUBTYPE_FUNCTION 2
#define NUBTYPE_PRODUCT 3
#define NUBTYPE_BITFIELD 4
#define NUBTYPE_UNION 5
#define NUBTYPE_LINEAR_ARRAY 6
#define NUBTYPE_GENERIC_POINTER 7
#define NUBTYPE_ILLEGAL 0

// At the moment, a type is either a pointer or it isn't. I'm ignoring
// any near/far/huge/segment classification.

#define TYPE_MODE_DIRECT 0
#define TYPE_MODE_FLAT_32_POINTER 1

typedef struct _DBGTYPE *LPDBGTYPE;

typedef struct _TYPE_HEADER {

  BOOL                         Searched;
  WORD                         CodeViewIndex;
  DWORD                        Size;
  DWORD                        Alignment;
  DWORD                        Flag;
  DWORD                        Mode;
  BOOL                         NameAvailable;
  BYTE                         NameLength;
  char                        *Name;

} TYPE_HEADER;

typedef struct _STRUCTURED_TYPE_FIELD {

  BYTE                            NameLength;
  char                           *Name;
  DWORD                           ByteOffset;
  DWORD                           BitOffset;
  LPDBGTYPE                       Type;
  struct _STRUCTURED_TYPE_FIELD  *Next;

} STRUCTURED_TYPE_FIELD, *FIELD_LIST;

typedef struct _ARGUMENT_DESCRIPTION {

  LPDBGTYPE                       Type;
  struct _ARGUMENT_DESCRIPTION   *Next;

} ARGUMENT_DESCRIPTION, *ARGUMENT_LIST;

typedef struct _PRIMITIVE_TYPE {

  WORD Type;
  WORD Mode;
  WORD Size;

} PRIMITIVE_TYPE;

typedef struct _GENERIC_POINTER_TYPE {

  LPDBGTYPE                     ReferencedType;

} GENERIC_POINTER_TYPE;

#define ARGUMENTS_RIGHT_TO_LEFT 1
#define ARGUMENTS_LEFT_TO_RIGHT 2
#define CALLER_POPS_STACK 1
#define CALLEE_POPS_STACK 2

typedef struct _FUNCTION_TYPE {

  DWORD                         ArgumentCount;
  ARGUMENT_LIST                 ArgumentList;
  LPDBGTYPE                     ReturnType;
  NUB_INDEX                     ReturnRegister;
  int                           ArgumentOrder;
  int                           WhoPopsStack;

} FUNCTION_TYPE;

typedef struct _PRODUCT_TYPE {

  DWORD                         FieldCount;
  FIELD_LIST                    FieldList;

} PRODUCT_TYPE, UNION_TYPE, BITFIELD_TYPE;

typedef struct _LINEAR_ARRAY_TYPE {

  LPDBGTYPE                     ElementType;
  DWORD                         ElementCount;

} LINEAR_ARRAY_TYPE;

typedef union _TYPE_DESCRIPTION {

  PRIMITIVE_TYPE        Primitive;
  FUNCTION_TYPE         Function;
  PRODUCT_TYPE          Product;
  BITFIELD_TYPE         Bitfield;
  UNION_TYPE            Union;
  LINEAR_ARRAY_TYPE     LinearArray;
  GENERIC_POINTER_TYPE  Pointer;

} TYPE_DESCRIPTION;

typedef struct _DBGTYPE {

  TYPE_HEADER                     TypeHeader;
  TYPE_DESCRIPTION                u;

} DBGTYPE;


/* Encodings for debugger nub functions that are required to run on
   the same thread. */

#define NUB_CREATE_AND_DEBUG_PROCESS 1
#define NUB_SETUP_FUNCTION_CALL 2
#define NUB_REMOTE_CALL_SPY 3
#define NUB_APPLICATION_CONTINUE 4
#define NUB_APPLICATION_CONTINUE_UNHANDLED 5
#define NUB_APPLICATION_STOP 6
#define NUB_WAIT_FOR_STOP_REASON_WITH_TIMEOUT 7
#define NUB_WAIT_FOR_STOP_REASON_NO_TIMEOUT 8
#define NUB_PROFILE_WAIT_FOR_STOP_REASON_WITH_TIMEOUT 9
#define NUB_PROFILE_WAIT_FOR_STOP_REASON_NO_TIMEOUT 10
#define NUB_KILL_APPLICATION 11
#define NUB_CLOSE_APPLICATION 12
#define NUB_APPLICATION_RESTART 13
#define NUB_DEBUG_ACTIVE_PROCESS 14

/* Argument marshalling buffers for the above functions. */

typedef struct _ARG_NUB_CREATE_AND_DEBUG_PROCESS {
  SERVER            Server;
  char             *Module;
  char             *Arguments;
  NUBINT            PathCount;
  char            **Paths;
  NUBINT            LibCount;
  char            **Libs;
  char             *WorkingDirectory;
  NUBINT            CreateShell;
  NUB               Result;
} ARG_NUB_CREATE_AND_DEBUG_PROCESS;

typedef struct _ARG_NUB_DEBUG_ACTIVE_PROCESS {
  SERVER            Server;
  NUBPROCESS        Process;
  NUBINT            PathCount;
  char            **Paths;
  char             *JITInfo;
  NUB               Result;
} ARG_NUB_DEBUG_ACTIVE_PROCESS;

typedef struct _ARG_NUB_SETUP_FUNCTION_CALL {
  NUB               Nub;
  NUBTHREAD         Nubthread;
  TARGET_ADDRESS    Function;
  NUBINT            ArgCount;
  TARGET_ADDRESS   *Args;
  NUBHANDLE        *ContextHandle;
  TARGET_ADDRESS    Result;
} ARG_NUB_SETUP_FUNCTION_CALL;

typedef struct _ARG_NUB_REMOTE_CALL_SPY {
  NUB               Nub;
  NUBTHREAD         Nubthread;
  TARGET_ADDRESS    Function;
  NUBINT            ArgCount;
  TARGET_ADDRESS   *Args;
  NUB_ERROR        *Status;
  TARGET_ADDRESS    Result;
} ARG_NUB_REMOTE_CALL_SPY;

typedef struct _ARG_NUB_APPLICATION_CONTINUE {
  NUB               Nub;
} ARG_NUB_APPLICATION_CONTINUE;

typedef struct _ARG_NUB_APPLICATION_STOP {
  NUB               Nub;
} ARG_NUB_APPLICATION_STOP;

typedef struct _ARG_NUB_APPLICATION_CONTINUE_UNHANDLED {
  NUB               Nub;
} ARG_NUB_APPLICATION_CONTINUE_UNHANDLED;

typedef struct _ARG_NUB_APPLICATION_RESTART {
  NUB               Nub;
} ARG_NUB_APPLICATION_RESTART;

typedef struct _ARG_NUB_WAIT_FOR_STOP_REASON_WITH_TIMEOUT {
  NUB               Nub;
  NUBINT            Timeout;
  NUBINT           *Code;
} ARG_NUB_WAIT_FOR_STOP_REASON_WITH_TIMEOUT;

typedef struct _ARG_NUB_WAIT_FOR_STOP_REASON_NO_TIMEOUT {
  NUB               Nub;
  NUBINT           *Code;
} ARG_NUB_WAIT_FOR_STOP_REASON_NO_TIMEOUT;

typedef struct _ARG_NUB_PROFILE_WAIT_FOR_STOP_REASON_WITH_TIMEOUT {
  NUB               Nub;
  NUBINT            Timeout;
  NUBINT            ProfileInterval;
  NUBINT           *Code;
} ARG_NUB_PROFILE_WAIT_FOR_STOP_REASON_WITH_TIMEOUT;

typedef struct _ARG_NUB_PROFILE_WAIT_FOR_STOP_REASON_NO_TIMEOUT {
  NUB               Nub;
  NUBINT            ProfileInterval;
  NUBINT           *Code;
} ARG_NUB_PROFILE_WAIT_FOR_STOP_REASON_NO_TIMEOUT;

typedef struct _ARG_NUB_KILL_APPLICATION {
  NUB               Nub;
  NUB_ERROR         Result;
} ARG_NUB_KILL_APPLICATION;

typedef struct _ARG_NUB_CLOSE_APPLICATION {
  NUB               Nub;
} ARG_NUB_CLOSE_APPLICATION;

typedef union _ARG_DISPATCH {
  ARG_NUB_CREATE_AND_DEBUG_PROCESS   NubCreateAndDebugProcess;
  ARG_NUB_DEBUG_ACTIVE_PROCESS       NubDebugActiveProcess;
  ARG_NUB_SETUP_FUNCTION_CALL        NubSetupFunctionCall;
  ARG_NUB_REMOTE_CALL_SPY            NubRemoteCallSpy;
  ARG_NUB_APPLICATION_STOP           NubApplicationStop;
  ARG_NUB_APPLICATION_RESTART        NubApplicationRestart;
  ARG_NUB_APPLICATION_CONTINUE       NubApplicationContinue;
  ARG_NUB_APPLICATION_CONTINUE_UNHANDLED   
                                     NubApplicationContinueUnhandled;
  ARG_NUB_WAIT_FOR_STOP_REASON_WITH_TIMEOUT 
                                     NubWaitForStopReasonWithTimeout;
  ARG_NUB_WAIT_FOR_STOP_REASON_NO_TIMEOUT
                                     NubWaitForStopReasonNoTimeout;
  ARG_NUB_PROFILE_WAIT_FOR_STOP_REASON_WITH_TIMEOUT
                                     NubProfileWaitForStopReasonWithTimeout;
  ARG_NUB_PROFILE_WAIT_FOR_STOP_REASON_NO_TIMEOUT
                                     NubProfileWaitForStopReasonNoTimeout;
  ARG_NUB_KILL_APPLICATION           NubKillApplication;
  ARG_NUB_CLOSE_APPLICATION          NubCloseApplication;
} ARG_DISPATCH;

typedef struct _NUB_ARGUMENT_BUFFER {
  int             FunctionCode;
  ARG_DISPATCH    u;
} NUB_ARGUMENT_BUFFER;

#include "nub_interface.h"
#include "debug_map.h"
#include "coff_map.h"
#include "utils.h"

