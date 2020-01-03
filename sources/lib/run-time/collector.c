/* INTERIM DYLAN RUN-TIME SYSTEM INTERFACE
 *
 * Copyright (C) 1996 Functional Objects, Inc. All rights reserved
 *
 * This is the implementation of the interim interface between
 * the Dylan run-time system and the Memory Manager. It is
 * only here to make initial integration with Dylan easier
 * by removing some of the burden from the Dylan Group.  The
 * Dylan run-time system should migrate to the full MPS Interface
 * as soon as possible.
 *
 * This implementation now operates with multiple threads using
 * the full thread-safe MPS Interface.
 *
 * The interface is implemented using two pools: one AMC pool, which
 * holds the general objects, and an MV pool for miscellaneous
 * objects and wrappers.
 *
 * Problems:
 *  This module doesn't hold on to root handles, and violates the
 *  rule of symmetry when destroying the space on the way out from
 *  the trampoline.
 */

#ifndef GC_USE_MPS
#  define NO_ALLOCATION_COUNT_FOR_PROFILER 1
#endif

#define unused(param)   ((void)param)

/* HACK Added by phoward 17-JUN-98
 * The file SPY-INTERFACES.C contains definitions that are not
 * referenced from within the runtime itself, but are called
 * remotely by the debugger. The Microsoft linker will throw
 * away these definitions unless another file references at least
 * one of them. The following (uncalled) function is the forced
 * reference we need.
 */

extern int spy_load_extension_component(char *);

void force_reference_to_spy_interface()
{
  spy_load_extension_component("");
}


#include "mm.h"        /* Dylan Interface */
#include <memory.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#ifdef OPEN_DYLAN_BACKEND_LLVM
#include <inttypes.h>
typedef intptr_t DSINT;
#endif

#ifdef OPEN_DYLAN_PLATFORM_UNIX
#include "unix-types.h"
#else
#include "windows-types.h"
#endif

#if defined(__clang__)
#define EXTERN_INLINE
#define STATIC_INLINE static inline
#else
#define EXTERN_INLINE __inline
#define STATIC_INLINE static __inline
#endif


/* Configuration
 *
 * MISC* configure the MV pool.
 */

#define MISCEXTENDBY    ((size_t)16384)
#define MISCAVGSIZE     ((size_t)32)
#define MISCMAXSIZE     ((size_t)65536)

static void report_runtime_error (char* header, char* message);

static void simple_error (char* message)
{
  report_runtime_error("\nDylan runtime error: ", message);
}

typedef size_t                  word;
typedef unsigned char           byte_char;
typedef unsigned short          half_word;
typedef _int64                  double_word;
typedef float                   single_float;
typedef double                  double_float;
typedef void*                   dylan_object;

typedef int                     dylan_bool_t;

EXTERN_INLINE
void fill_dylan_object_mem(dylan_object *mem, dylan_object fill, int count)
{
  // This really should be controlled by a better define, but we don't have
  // or really need one at the moment.
#if defined(OPEN_DYLAN_PLATFORM_UNIX) && defined(OPEN_DYLAN_ARCH_X86)
  __asm__
    (
      "cld    \n\t"
      "movl   %0, %%eax\n\t"
      "movl   %1, %%ecx\n\t"
      "movl   %2, %%edi\n\t"
      "rep    \n\t"
      "stosl  %%eax,%%es:(%%edi)\n"

      // output operands
      :
      // input operands
      : "g" (fill), "g" (count), "g" (mem)
      // clobbered machine registers
      : "ax", "cx","di","si", "cc"
    );
#elif defined(OPEN_DYLAN_PLATFORM_WINDOWS) && defined(OPEN_DYLAN_ARCH_X86)
  __asm
    {
      cld
      mov eax, fill
      mov ecx, count
      mov edi, mem
      rep stosd
    };
#else
  int i;
  for (i = 0; i < count; i++) {
    mem[i] = fill;
  }
#endif
}


#define define_fill_mem(type) \
EXTERN_INLINE  \
void fill_ ## type ## _mem(type *mem, type fill, int count) \
{ \
  int index = 0; \
  while (index < count) \
    {  \
      mem[index] = fill; \
      ++index; \
    } \
}

define_fill_mem(word)
define_fill_mem(half_word)
define_fill_mem(double_word)
define_fill_mem(single_float)
define_fill_mem(double_float)


EXTERN_INLINE
void untraced_fill_byte_char_mem(void **object, byte_char fill, int count, int count_slot, dylan_bool_t ztq)
{
  byte_char *d = (byte_char*)(&(object[count_slot + 1]));
  memset(d, fill, count);
  if (ztq) {
    d[count] = 0;
  }
}

#define define_untraced_fill_mem(type) \
EXTERN_INLINE  \
void untraced_fill_ ## type ## _mem(void **object, type fill, size_t count, size_t count_slot, dylan_bool_t ztq) \
{ \
  size_t index = 0; \
  type *mem = (type*)(object + count_slot + 1); \
  unused(ztq); \
  object[count_slot] = (void*)((count << 2) + 1); \
 \
  while (index < count) \
    {  \
      mem[index] = fill; \
      ++index; \
    } \
}

define_untraced_fill_mem(dylan_object)
define_untraced_fill_mem(word)
define_untraced_fill_mem(half_word)
define_untraced_fill_mem(double_word)
define_untraced_fill_mem(single_float)
define_untraced_fill_mem(double_float)

/* Thread Local Variables, accessed via the GC-TEB*/

#ifdef OPEN_DYLAN_PLATFORM_UNIX
// On Unix platforms, use the thread-local storage provided by the system to
// hold the TEB pointer
__thread void* teb;
#endif

static BOOL heap_statsQ = FALSE;
#ifndef NO_ALLOCATION_COUNT_FOR_PROFILER
static BOOL heap_alloc_statsQ = FALSE;
#endif
extern void add_stat_for_object (void *object, void* wrapper, int size);
extern void clear_wrapper_stats ();
extern void display_wrapper_stats ();

char   *dylan_buffer = NULL;
int    dylan_buffer_pos = 0;
int    dylan_buffer_size = 8192;
BOOL   dylan_streamQ = FALSE;

RUN_TIME_API
void primitive_begin_heap_alloc_stats()
{
#ifndef NO_ALLOCATION_COUNT_FOR_PROFILER
  heap_statsQ = TRUE;
  heap_alloc_statsQ = TRUE;
  clear_wrapper_stats();
#endif
}

RUN_TIME_API
int primitive_end_heap_alloc_stats(char *buffer)
{
#ifndef NO_ALLOCATION_COUNT_FOR_PROFILER
  dylan_streamQ = TRUE;
  dylan_buffer = buffer;
  dylan_buffer_pos = 0;
  if (heap_alloc_statsQ) {
    display_wrapper_stats();
  }
  dylan_streamQ = FALSE;
  heap_alloc_statsQ = FALSE;
  return(dylan_buffer_pos);
#else
  unused(buffer);
  return 0;
#endif
}

extern_CRITICAL_SECTION(class_breakpoint_lock);

extern unsigned int class_breakpoints_pending;
extern HANDLE class_breakpoint_events[2];

extern void set_wrapper_breakpoint (void *wrapper, int count);
extern void clear_wrapper_breakpoint (void *wrapper);
extern BOOL check_wrapper_breakpoint_for_objectQ;

EXTERN_INLINE
void *class_wrapper(void *class)
{
  void *iclass = ((void**)class)[3];
  void *wrapper  = ((void**)iclass)[3];

  return wrapper;
}


// Handling of class breakpoints in multi-threaded applications requires
// that this function be called as a spy on an interactive thread immediately;
// then the set and clear breakpoint primitives will be run as regular interactions
// when the application continues; this is to enable synchronization with regular
// application threads that may already be in the allocation breakpointing code.
// The two class breakpoint events are used to bring this synchronization about.

RUN_TIME_API
void primitive_class_breakpoint_pending()
{
  heap_statsQ = TRUE;
  ++class_breakpoints_pending;
}

RUN_TIME_API
void primitive_set_class_breakpoint(void *class, int count)
{
  if (wait_for_EVENT(class_breakpoint_events[0], INFINITE) != EVENT_WAIT_SUCCESS) {
    // MSG0("primitive_set_class_breakpoint: error waiting for class breakpoint event\n");
  }

  if (class == (void *)1) {
    // set breakpoint on all dylan classes
    check_wrapper_breakpoint_for_objectQ = TRUE;
  } else {
    void *wrapper = class_wrapper(class);
    set_wrapper_breakpoint(wrapper, count >> 2);
  }

  --class_breakpoints_pending;
  set_EVENT(class_breakpoint_events[1]);
}

RUN_TIME_API
void primitive_clear_class_breakpoint(void *class)
{
  void *wrapper;

  if (wait_for_EVENT(class_breakpoint_events[0], INFINITE) != EVENT_WAIT_SUCCESS) {
    // MSG0("primitive_clear_class_breakpoint: error waiting for class breakpoint event\n");
  }

  switch ((long)class) {

  case 0:
    // clear all breakpoints
    check_wrapper_breakpoint_for_objectQ = FALSE;
    clear_wrapper_breakpoint(NULL);
    break;

  case 1:
    // clear breakpoint on all dylan classes
    check_wrapper_breakpoint_for_objectQ = FALSE;
    break;

  default:
    wrapper = class_wrapper(class);
    clear_wrapper_breakpoint(wrapper);
    break;

  }

  --class_breakpoints_pending;
  set_EVENT(class_breakpoint_events[1]);
}

extern void display_wrapper_breakpoints();

RUN_TIME_API
int primitive_display_class_breakpoints(char *buffer)
{
  if (wait_for_EVENT(class_breakpoint_events[0], INFINITE) != EVENT_WAIT_SUCCESS) {
    // MSG0("primitive_display_class_breakpoints: error waiting for class breakpoint event\n");
  }

  dylan_streamQ = TRUE; dylan_buffer = buffer; dylan_buffer_pos = 0;
#ifdef GC_USE_MPS
  display_wrapper_breakpoints();
#endif
  dylan_streamQ = FALSE;

  --class_breakpoints_pending;
  set_EVENT(class_breakpoint_events[1]);

  return(dylan_buffer_pos);
}

extern void *call_dylan_function(void *function, size_t arg_count, ...);


/* Support for keyboard-break handling */

extern void *dylan_keyboard_break_handler;
extern BOOL dylan_keyboard_interruptQ;
static BOOL DylanKeyboardInterruptPollingQ = TRUE;

RUN_TIME_API
BOOL primitive_keyboard_interrupt_signaled()
{
  return dylan_keyboard_interruptQ;
}

RUN_TIME_API
void primitive_keyboard_interrupt_signaled_setter(BOOL interruptQ)
{
  dylan_keyboard_interruptQ = interruptQ;
}

RUN_TIME_API
BOOL primitive_keyboard_interrupt_polling()
{
  return DylanKeyboardInterruptPollingQ;
}

RUN_TIME_API
void primitive_keyboard_interrupt_polling_setter(BOOL pollingQ)
{
  DylanKeyboardInterruptPollingQ = pollingQ;
}


#define MAX_POLLING_THREADS 50

static HANDLE polling_threads[MAX_POLLING_THREADS];

static int polling_threads_cursor = -1;

static define_CRITICAL_SECTION(polling_threads_lock);

static int polling_thread_index (HANDLE hThread)
{
  int i;

  enter_CRITICAL_SECTION(&polling_threads_lock);
  for (i = 0; i < polling_threads_cursor + 1; i++) {
    if (polling_threads[i] == hThread) {
      leave_CRITICAL_SECTION(&polling_threads_lock);
      return(i);
    }
  }
  leave_CRITICAL_SECTION(&polling_threads_lock);
  return(-1);
}

STATIC_INLINE
BOOL polling_threadQ (HANDLE hThread)
{
  int index = polling_thread_index(hThread);

  if (index < 0) return FALSE;
  else return TRUE;
}

STATIC_INLINE
BOOL polling_individual_threadsQ ()
{
  if (polling_threads_cursor > -1) return TRUE;
  return FALSE;
}


static void add_polling_thread (HANDLE hThread)
{
  if (polling_threadQ(hThread)) return;

  enter_CRITICAL_SECTION(&polling_threads_lock);
    if (polling_threads_cursor < MAX_POLLING_THREADS) {
      ++polling_threads_cursor;
      polling_threads[polling_threads_cursor] = hThread;
    }
  leave_CRITICAL_SECTION(&polling_threads_lock);
}


static void remove_polling_thread (HANDLE hThread)
{
  int index = polling_thread_index(hThread);
  int i;

  if (index > -1) {
    enter_CRITICAL_SECTION(&polling_threads_lock);
      for (i = index; i < polling_threads_cursor + 1; i++)
        polling_threads[i] = polling_threads[i+1];

      --polling_threads_cursor;
    leave_CRITICAL_SECTION(&polling_threads_lock);
  }
}


RUN_TIME_API
BOOL primitive_keyboard_interrupt_polling_thread(HANDLE hThread)
{
  if (DylanKeyboardInterruptPollingQ) return TRUE;
  return polling_threadQ(hThread);
}

RUN_TIME_API
void primitive_keyboard_interrupt_polling_thread_setter
  (BOOL pollingQ, HANDLE hThread)
{
  if (pollingQ) add_polling_thread(hThread);
  else remove_polling_thread(hThread);
}


extern HANDLE get_current_thread_handle();

static void HandleDylanKeyboardInterrupt()
{
  if (DylanKeyboardInterruptPollingQ
      || (polling_individual_threadsQ()
          && (polling_threadQ(get_current_thread_handle())))) {
    dylan_keyboard_interruptQ = FALSE;
    call_dylan_function(dylan_keyboard_break_handler, 0);
  }
}

extern int wrapper_breaks_cursor;
extern void check_wrapper_breakpoint (void *wrapper, int size);

// This is to enable Dylan spy functions to run unimpeded by class breakpoints

extern BOOL Prunning_dylan_spy_functionQ;

extern BOOL Prunning_under_dylan_debuggerQ;

/*
    The strategy at the moment for handling keyboard interrupts is merely
    to set a flag; the runtime will check this flag periodically (e.g. every
    time an attempt is made to heap-allocate an object) and signal a keyboard
    interrupt at that time. Provision is also made for applications to do their
    own polling of this flag, for example in a dedicated thread, if they so wish.
*/

/* Used by each memory manager in the init function */
BOOL WINAPI DylanBreakControlHandler(DWORD dwCtrlType)
{
  switch (dwCtrlType)
    {
    case CTRL_BREAK_EVENT:
    case CTRL_C_EVENT:
      {
        if (Prunning_under_dylan_debuggerQ == FALSE) {
          dylan_keyboard_interruptQ = TRUE;
        }
        return TRUE;
      }

    default:
      return FALSE;
    }
}

#if defined(GC_USE_BOEHM)
#include "boehm-collector.c"
#elif defined(GC_USE_MALLOC)
#include "malloc-collector.c"
#else
#include "mps-collector.c"
#endif

STATIC_INLINE
void update_allocation_counter(gc_teb_t gc_teb, size_t count, void* wrapper)
{
#ifdef GC_USE_MPS
  gc_teb->gc_teb_allocation_counter += count;
#elif defined(OPEN_DYLAN_BACKEND_LLVM)
  extern __thread DSINT Pallocation_count;
  Pallocation_count += count;
#else
  unused(gc_teb);
#endif

  // Periodic polling of keyboard-interrupt flag
  if (dylan_keyboard_interruptQ) HandleDylanKeyboardInterrupt();

#ifdef NO_ALLOCATION_COUNT_FOR_PROFILER
  unused(count);
  unused(wrapper);
#else
  if (heap_statsQ) {
    if (!Prunning_dylan_spy_functionQ) {
      if (heap_alloc_statsQ) {
        add_stat_for_object(NULL, wrapper, count);
      }
      check_wrapper_breakpoint(wrapper, count);
    }
  }
#endif
}

static void zero_allocation_counter(gc_teb_t gc_teb)
{
#ifdef GC_USE_MPS
  gc_teb->gc_teb_allocation_counter = 0;
#else
  unused(gc_teb);
#endif
}


EXTERN_INLINE
gc_teb_t current_gc_teb()
{
  gc_teb_t gc_teb;
#if defined(OPEN_DYLAN_PLATFORM_UNIX)

  gc_teb = teb;

#else
  __asm
    {
      mov eax, dword ptr fs:[0x14] /* the TEB */
      mov gc_teb, eax
    };
#endif
  gc_teb--; /* the GC-TEB is BEFORE the TEB */
  return(gc_teb);
}

void *dylan__malloc__misc(size_t size)
{
  return MMAllocMisc(size);
}

#define BLOCK_CODE_MASK  0xff000000
#define BLOCK_CODE_TOKEN 0xab000000
#define BLOCK_SIZE_MASK  0x00ffffff

static int encode_size_of_block(int size)
{
  if ((size & BLOCK_CODE_MASK) != 0) {
    simple_error("Unexpected block size for manual allocation");
  }
  return (size | BLOCK_CODE_TOKEN);
}

static int decode_size_of_block(int size)
{
  if ((size & BLOCK_CODE_MASK) != BLOCK_CODE_TOKEN) {
    simple_error("Attempt to free a corrupted manually managed object");
  }
  return (size & BLOCK_SIZE_MASK);
}

RUN_TIME_API
void *mps__malloc(size_t size)
{
  size_t tot_size = size + sizeof(size_t);
  size_t *block = (size_t *)MMAllocMisc(tot_size);
  *block = encode_size_of_block(tot_size);
  return (void*)(++block);
}


static void duplicated_deallocation_error(size_t *ptr)
{
  unused(ptr);
  simple_error("Duplicate attempt to free manually managed object");
}


RUN_TIME_API
void mps__free(size_t *old)
{
  if (old != NULL) {
    size_t freed = 0xdeadf00d;
    size_t *block = old - 1;
    if (*block == freed) {
      duplicated_deallocation_error(old);
    } else {
      size_t size = decode_size_of_block(*block);
      *block = freed;
      MMFreeMisc((void *)block, size);
    }
  }
}


void dylan__finish__malloc(void)
{
}


EXTERN_INLINE
void *wrapper_class(void *wrapper)
{
  void *iclass = ((void**)wrapper)[1];
  void *class  = ((void**)iclass)[2];

  return class;
}

#ifndef OPEN_DYLAN_BACKEND_LLVM
#include "exceptions.c"
#endif

