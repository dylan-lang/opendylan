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

#ifdef OPEN_DYLAN_PLATFORM_UNIX
#define RUN_TIME_API
#else
#define RUN_TIME_API __declspec( dllexport )
#endif


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


/* Controlling the use of the Leaf Object pool
 *
 * Fine control may be used to determine whether common allocation
 * profiles use the leaf pool or the main pool.
*/

#define USE_LEAF_FOR_SMALL_OBJECTS
#define USE_LEAF_FOR_STRINGS
#define USE_LEAF_FOR_REPEATED_OBJECTS


#ifdef USE_LEAF_FOR_SMALL_OBJECTS
#define MMReserveLeafObject MMReserveLeaf
#define MMCommitLeafObject MMCommitLeaf
#else
#define MMReserveLeafObject MMReserveObject
#define MMCommitLeafObject MMCommitObject
#endif

#ifdef USE_LEAF_FOR_REPEATED_OBJECTS
#define MMReserveLeafRepeated MMReserveLeaf
#define MMCommitLeafRepeated MMCommitLeaf
#else
#define MMReserveLeafRepeated MMReserveObject
#define MMCommitLeafRepeated MMCommitObject
#endif

#ifdef USE_LEAF_FOR_STRINGS
#define MMReserveLeafTerminated MMReserveLeaf
#define MMCommitLeafTerminated MMCommitLeaf
#else
#define MMReserveLeafTerminated MMReserveObject
#define MMCommitLeafTerminated MMCommitObject
#endif

#ifndef MAXIMUM_HEAP_SIZE
#define MAXIMUM_HEAP_SIZE (512 * 1024 * 1024)
#endif

#ifdef BOEHM_GC
#include <gc/gc.h>
#define MAX_BOEHM_HEAP_SIZE (1024 * 1024 * 1024)
/* #define INITIAL_BOEHM_HEAP_SIZE (50 * 1024 * 1024) */
#define NO_FINALIZATION
#endif

#include "mm.h"        /* Dylan Interface */
#include "mps.h"        /* MPS Interface */
#include "mpscmv.h"     /* MPS pool class MV */
#include "mpscamc.h"    /* MPS pool class AMC */
#include "mpsavm.h"     /* MPS arena class */
#ifndef BOEHM_GC
#ifndef OPEN_DYLAN_PLATFORM_UNIX
#include "mpsw3.h"
#endif
#else
#include "boehm.h"
#endif
#include "fmtdy.h"      /* Dylan object format */
#include "mpslib.h"     /* plinth interface */
#include <memory.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#ifdef OPEN_DYLAN_PLATFORM_UNIX
#include "unix-types.h"
#else
#include "win32-types.h"
#endif

#include "mpscawl.h"    /* MPS pool class AWL */
#include "mpsclo.h"    /* MPS pool class LO */

/* Configuration
 *
 * MISC* configure the MV pool.
 */

#define MISCEXTENDBY    ((size_t)16384)
#define MISCAVGSIZE     ((size_t)32)
#define MISCMAXSIZE     ((size_t)65536)


typedef mps_word_t              word;
typedef unsigned char           byte_char;
typedef unsigned short          half_word;
typedef _int64                  double_word;
typedef float                   single_float;
typedef double                  double_float;
typedef void*                         dylan_object;


#define TARG_CHECK (MPS_RES_OK == MMSUCCESS && \
                    MPS_RES_FAIL == MMFAILURE && \
                    MPS_RES_RESOURCE == MMRESOURCE && \
                    MPS_RES_MEMORY == MMRESMEM && \
                    MPS_RES_LIMIT == MMLIMIT && \
                    MPS_RES_UNIMPL == MMUNIMPLEMENTED && \
                    MPS_RES_IO == MMIO)



void report_runtime_error (char* header, char* message)
{
#ifndef BOEHM_GC
  mps_lib_FILE *stream = mps_lib_get_stderr();
  mps_lib_fputs(header, stream);
  mps_lib_fputs(message, stream);
  mps_lib_fputc('\n', stream);
  mps_lib_abort();
#endif
}

void simple_error (char* message)
{
  report_runtime_error("\nDylan runtime error: ", message);
}

#ifndef BOEHM_GC

#define unused(param)   ((void)param)

mps_bool_t dylan_check(mps_addr_t addr)
{
  assert(addr != 0);
  assert(((mps_word_t)addr & (ALIGN-1)) == 0);
  assert(dylan_wrapper_check((mps_word_t *)((mps_word_t *)addr)[0]));
  /* .assert.unused: Asserts throw away their conditions */
  /* in hot varieties, so UNUSED is needed. */
  unused(addr);
  return 1;
}

#endif


/* Default Error Handler
 *
 * This is the default error handler initially installed for all the
 * allocation interfaces in mm.h.  It prints a message on the standard
 * error stream then causes abnormal program termination.
 */

static void defaultHandler(MMError e, const char *opName, size_t size)
{
#ifndef BOEHM_GC
  mps_lib_FILE *stream = mps_lib_get_stderr();
  mps_lib_fputs("\nError: ", stream);
  mps_lib_fputs(opName, stream);
  mps_lib_fputs(" - Request to allocate failed -- aborting\n", stream);
  mps_lib_abort();
#endif
}

mps_arena_t arena;
mps_chain_t chain;
static mps_fmt_t format;
static mps_fmt_t dylan_fmt_weak_s;
static mps_fmt_A_t fmt_A;
static mps_fmt_A_t fmt_A_weak;
static mps_pool_t main_pool, weak_table_pool, wrapper_pool, misc_pool, leaf_pool;

#ifndef NO_FINALIZATION
static mps_message_type_t finalization_type;
#endif

#define genCOUNT 2

static mps_gen_param_s gc_default_gen_param[genCOUNT] = {
  { 8 * 1024, 0.45 },
  { MAXIMUM_HEAP_SIZE/1024 - 8 * 1024, 0.99 }
};

static MMAllocHandler main_handler = defaultHandler;
static MMAllocHandler weak_awl_handler = defaultHandler;
static MMAllocHandler exact_awl_handler = defaultHandler;
static MMAllocHandler wrapper_handler = defaultHandler;
static MMAllocHandler leaf_handler = defaultHandler;
static MMAllocHandler misc_handler = defaultHandler;


/* Thread Local Variables, accessed via the GC-TEB*/

#ifdef OPEN_DYLAN_PLATFORM_UNIX
// On Unix platforms, use the thread-local storage provided by the system to
// hold the TEB pointer
__thread void* teb;

#endif

typedef struct gc_teb_s {       /* GC Thread Environment block descriptor */
  mps_bool_t gc_teb_inside_tramp;  /* the HARP runtime assumes offset 0 for this */
  mps_ap_t   gc_teb_main_ap;       /* the HARP runtime assumes offset 1 for this */
  mps_ap_t   gc_teb_weak_awl_ap;
  mps_ap_t   gc_teb_exact_awl_ap;
  mps_ap_t   gc_teb_leaf_ap;
  mps_thr_t  gc_teb_thread;
  mps_root_t gc_teb_stack_root;
  size_t     gc_teb_allocation_counter;   /* the profiler assumes this is at offset -1 from main TEB */
} gc_teb_s;


/* The profiler can use this as an offset of the allocation counter from TEB */
/* This assumes that the gc_teb is contiguous with the main teb. the HARP    */
/* runtime ensure this is always true.                                       */

int teb_allocation_counter_offset = - ((int)sizeof(size_t));


BOOL heap_statsQ = FALSE;
BOOL heap_alloc_statsQ = FALSE;
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
  if (heap_alloc_statsQ)
    display_wrapper_stats();
  dylan_streamQ = FALSE;
  heap_alloc_statsQ = FALSE;
  return(dylan_buffer_pos);
#endif
}

extern_CRITICAL_SECTION(class_breakpoint_lock);

extern unsigned int class_breakpoints_pending;
extern HANDLE class_breakpoint_events[2];

extern void set_wrapper_breakpoint (void *wrapper, int count);
extern void clear_wrapper_breakpoint (void *wrapper);
extern BOOL check_wrapper_breakpoint_for_objectQ;

__inline
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
  };

  if (class == (void *)1)
    // set breakpoint on all dylan classes
    check_wrapper_breakpoint_for_objectQ = TRUE;
  else {
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
  };

  switch ((int)class) {

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
  };

  dylan_streamQ = TRUE; dylan_buffer = buffer; dylan_buffer_pos = 0;
  display_wrapper_breakpoints();
  dylan_streamQ = FALSE;

  --class_breakpoints_pending;
  set_EVENT(class_breakpoint_events[1]);

  return(dylan_buffer_pos);
}

extern void *call_dylan_function(void *function, size_t arg_count, ...);


/* Support for keyboard-break handling */

extern void *dylan_keyboard_break_handler;
extern BOOL dylan_keyboard_interruptQ;
BOOL DylanKeyboardInterruptPollingQ = TRUE;

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

HANDLE polling_threads[MAX_POLLING_THREADS];

int polling_threads_cursor = -1;

define_CRITICAL_SECTION(polling_threads_lock);


int polling_thread_index (HANDLE hThread)
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

__inline
BOOL polling_threadQ (HANDLE hThread)
{
  int index = polling_thread_index(hThread);

  if (index < 0) return FALSE;
  else return TRUE;
}

__inline
BOOL polling_individual_threadsQ ()
{
  if (polling_threads_cursor > -1) return TRUE;
  return FALSE;
}


void add_polling_thread (HANDLE hThread)
{
  if (polling_threadQ(hThread)) return;

  enter_CRITICAL_SECTION(&polling_threads_lock);
    if (polling_threads_cursor < MAX_POLLING_THREADS) {
      ++polling_threads_cursor;
      polling_threads[polling_threads_cursor] = hThread;
    };
  leave_CRITICAL_SECTION(&polling_threads_lock);
}


void remove_polling_thread (HANDLE hThread)
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

void HandleDylanKeyboardInterrupt()
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


__inline
void update_allocation_counter(gc_teb_t gc_teb, size_t count, void* wrapper)
{
#ifndef NO_ALLOCATION_COUNT_FOR_PROFILER
  gc_teb->gc_teb_allocation_counter += count;

  // Periodic polling of keyboard-interrupt flag
  if (dylan_keyboard_interruptQ) HandleDylanKeyboardInterrupt();

  if (heap_statsQ) {
    if (!Prunning_dylan_spy_functionQ) {
      if (heap_alloc_statsQ)
        add_stat_for_object(NULL, wrapper, count);
      check_wrapper_breakpoint(wrapper, count);
    }
  }
#endif
}

void zero_allocation_counter(gc_teb_t gc_teb)
{
#ifndef NO_ALLOCATION_COUNT_FOR_PROFILER
  gc_teb->gc_teb_allocation_counter = 0;
#endif
}


__inline
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
};


#define inside_tramp  (*current_gc_teb()).gc_teb_inside_tramp
#define main_ap       (*current_gc_teb()).gc_teb_main_ap
#define weak_awl_ap   (*current_gc_teb()).gc_teb_weak_awl_ap
#define exact_awl_ap  (*current_gc_teb()).gc_teb_exact_awl_ap
#define leaf_ap       (*current_gc_teb()).gc_teb_leaf_ap
#define thread        (*current_gc_teb()).gc_teb_thread
#define stack_root    (*current_gc_teb()).gc_teb_stack_root

#if defined(OPEN_DYLAN_PLATFORM_LINUX)
#include "x86-linux-exceptions.c"
#elif defined(OPEN_DYLAN_PLATFORM_FREEBSD)
#include "x86-freebsd-exceptions.c"
#else
#include "win32-exceptions.c"
#endif


/* Support for foreign call-ins */
extern void *dylan_callin_internal(void *arg_base, size_t s);


/* Thread creation & deletion code */

int num_threads = 0;

/* client estimate for handling requirements goes here */
int low_memory_allocation_per_thread = 128 * 1024;


define_CRITICAL_SECTION(reservoir_limit_set_lock);

__inline
void update_runtime_thread_count(int increment)
{

  enter_CRITICAL_SECTION(&reservoir_limit_set_lock);
    num_threads = num_threads + increment;
#ifndef BOEHM_GC
    mps_reservoir_limit_set(arena, num_threads * low_memory_allocation_per_thread);
#endif
  leave_CRITICAL_SECTION(&reservoir_limit_set_lock);
}


MMError dylan_mm_register_thread(void *stackBot)
{
  mps_res_t res;

  gc_teb_t gc_teb = current_gc_teb();

  update_runtime_thread_count(1);

  zero_allocation_counter(gc_teb);

#ifndef BOEHM_GC

  res = mps_ap_create(&gc_teb->gc_teb_main_ap, main_pool, MPS_RANK_EXACT);
  if(res) goto failApCreate;

  res = mps_ap_create(&gc_teb->gc_teb_leaf_ap, leaf_pool, MPS_RANK_EXACT);
  if(res) goto failLeafApCreate;

  res = mps_ap_create(&gc_teb->gc_teb_weak_awl_ap, weak_table_pool, MPS_RANK_WEAK);
  if(res) goto failWeakAWLApCreate;

  res = mps_ap_create(&gc_teb->gc_teb_exact_awl_ap, weak_table_pool, MPS_RANK_EXACT);
  if(res) goto failExactAWLApCreate;

  res = mps_thread_reg(&gc_teb->gc_teb_thread, arena);
  if(res) goto failThreadReg;

  /* Create a root object for ambiguously scanning the stack. */
  assert(stackBot != NULL);
  res = mps_root_create_reg(&gc_teb->gc_teb_stack_root, arena, MPS_RANK_AMBIG,
                           (mps_rm_t)0,
                            gc_teb->gc_teb_thread, mps_stack_scan_ambig, stackBot, 0);
  if(res) goto failStackRootCreate;
  return res;

  mps_root_destroy(gc_teb->gc_teb_stack_root);
failStackRootCreate:
  mps_thread_dereg(gc_teb->gc_teb_thread);
failThreadReg:
  mps_ap_destroy(gc_teb->gc_teb_exact_awl_ap);
failExactAWLApCreate:
  mps_ap_destroy(gc_teb->gc_teb_weak_awl_ap);
failWeakAWLApCreate:
  mps_ap_destroy(gc_teb->gc_teb_leaf_ap);
failLeafApCreate:
  mps_ap_destroy(gc_teb->gc_teb_main_ap);
failApCreate:
  return res;

#else

  return 0;

#endif


}


MMError dylan_mm_deregister_thread_from_teb(gc_teb_t gc_teb)
{

  update_runtime_thread_count(-1);
#ifndef BOEHM_GC
  mps_root_destroy(gc_teb->gc_teb_stack_root);
  mps_thread_dereg(gc_teb->gc_teb_thread);
  mps_ap_destroy(gc_teb->gc_teb_main_ap);
  mps_ap_destroy(gc_teb->gc_teb_leaf_ap);
  mps_ap_destroy(gc_teb->gc_teb_weak_awl_ap);
  mps_ap_destroy(gc_teb->gc_teb_exact_awl_ap);
  return MPS_RES_OK;
#else
  return 0;
#endif
}


/* for backwards compatibility with old runtimes */
MMError dylan_mm_deregister_thread()
{
  gc_teb_t gc_teb = current_gc_teb();

  return dylan_mm_deregister_thread_from_teb(gc_teb);
}


MMError dylan_init_thread(void **rReturn, void *(*f)(void *, size_t), void *p, size_t s)
{
  EXCEPTION_PREAMBLE()

  gc_teb_t gc_teb = current_gc_teb();

  gc_teb->gc_teb_inside_tramp = 1;

  /* Go for it! */
  mps_tramp(rReturn, f, p, s);

  gc_teb->gc_teb_inside_tramp = 0;

  EXCEPTION_POSTAMBLE()

  return MPS_RES_OK;
}


void *dylan_callin_handler(void *arg_base, size_t s)
{
  void *res;

  EXCEPTION_PREAMBLE()

  gc_teb_t gc_teb = current_gc_teb();

  mps_bool_t was_inside = gc_teb->gc_teb_inside_tramp;
  gc_teb->gc_teb_inside_tramp = 1;

  /* Go for it! */
  mps_tramp(&res, dylan_callin_internal, arg_base, s);

  gc_teb->gc_teb_inside_tramp = was_inside;

  EXCEPTION_POSTAMBLE()

  return res;
}



__inline
void fill_dylan_object_mem(dylan_object *mem, dylan_object fill, int count)
{
  // This really should be controlled by a better define, but we don't have
  // or really need one at the moment.
#if defined(OPEN_DYLAN_PLATFORM_UNIX)
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
#else
  __asm
    {
      cld
      mov eax, fill
      mov ecx, count
      mov edi, mem
      rep stosd
    };
#endif
};


#define define_fill_mem(type) \
__inline  \
void fill_ ## type ## _mem(type *mem, type fill, int count) \
{ \
  int index = 0; \
  while (index < count) \
    {  \
      mem[index] = fill; \
      ++index; \
    }; \
}

define_fill_mem(half_word)
define_fill_mem(double_word)
define_fill_mem(single_float)
define_fill_mem(double_float)


__inline
void untraced_fill_byte_char_mem(void **object, byte_char fill, int count, int count_slot, mps_bool_t ztq)
{
  byte_char *d = (byte_char*)(&(object[count_slot + 1]));
  memset(d, fill, count);
  if (ztq) {
    d[count] = 0;
  }
}

#define define_untraced_fill_mem(type) \
__inline  \
void untraced_fill_ ## type ## _mem(void **object, type fill, int count, int count_slot, mps_bool_t ztq) \
{ \
  int index = 0; \
  type *mem = (type*)(object + count_slot + 1); \
  object[count_slot] = (void*)((count << 2) + 1); \
 \
  while (index < count) \
    {  \
      mem[index] = fill; \
      ++index; \
    }; \
}

define_untraced_fill_mem(dylan_object)
define_untraced_fill_mem(half_word)
define_untraced_fill_mem(double_word)
define_untraced_fill_mem(single_float)
define_untraced_fill_mem(double_float)



void *dylan__malloc__misc(size_t size)
{
  return MMAllocMisc(size);
}



#define BLOCK_CODE_MASK  0xff000000
#define BLOCK_CODE_TOKEN 0xab000000
#define BLOCK_SIZE_MASK  0x00ffffff

int encode_size_of_block(int size)
{
  if ((size & BLOCK_CODE_MASK) != 0) {
    simple_error("Unexpected block size for manual allocation");
  }
  return (size | BLOCK_CODE_TOKEN);
}

int decode_size_of_block(int size)
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


void duplicated_deallocation_error(size_t *ptr)
{
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


__inline
void *wrapper_class(void *wrapper)
{
  void *iclass = ((void**)wrapper)[1];
  void *class  = ((void**)iclass)[2];

  return class;
}

extern void *dylan_signal_low_memory;
extern void *dylan_false;

#define reserve_memory_for_object(size,  \
                                  wrapper,  \
                                  gc_teb,  \
                                  gc_teb_ap,  \
                                  handler,  \
                                  MMReserve)  \
{  \
  mps_res_t res;  \
  mps_addr_t p;  \
  \
  assert(gc_teb->gc_teb_inside_tramp);  \
  \
  do {  \
    res = mps_reserve(&p, gc_teb->gc_teb_ap, size);  \
  \
    if (res == MPS_RES_OK) {  \
      /* Success */  \
      return (void *)p;  \
  \
    } else {  \
      /* Failure due to low-memory - ask for reservoir permit */  \
      void *class = wrapper_class(wrapper);  \
      void *permit = call_dylan_function(dylan_signal_low_memory, 2, class, ((size << 2) + 1));  \
      if (permit != dylan_false) {  \
        /* Have permission - so use reservoir */  \
        res = mps_reserve_with_reservoir_permit  \
                (&p, gc_teb->gc_teb_ap, size);  \
        if (res == MPS_RES_OK) {  \
          return (void *)p;  \
        }  \
        /* Failure even when using reservoir. Catastrophic */  \
        (*handler)((MMError)res, MMReserve, size);  \
      } else {  \
        /* No permission to use the reservoir.  */  \
        /* Check the reservoir is full before looping again */  \
        /* Do this inside a critical region with the limit setting function */  \
        enter_CRITICAL_SECTION(&reservoir_limit_set_lock);  \
          { \
          size_t limit = mps_reservoir_limit(arena);  \
          size_t avail = mps_reservoir_available(arena);  \
          if (avail < limit) {  \
            /* The reservoir is not full - so the handling policy failed */  \
            /* Could attempt to do something smart here - like work out */  \
            /* whether other threads are likely to free up memory, */  \
            /* and signal a different error if not */  \
            }  \
          }  \
        leave_CRITICAL_SECTION(&reservoir_limit_set_lock);  \
        /* Try allocation again */  \
      }  \
  \
    }  \
  \
  } while (TRUE);  \
}


__inline
void *MMReserveObject(size_t size, void *wrapper, gc_teb_t gc_teb)
{
#ifndef BOEHM_GC

  reserve_memory_for_object(size, wrapper, gc_teb, gc_teb_main_ap, main_handler, "MMReserveObject");

#else

  return GC_malloc(size);

#endif
}

__inline
int MMCommitObject(void *p, size_t size, gc_teb_t gc_teb)
{
#ifndef BOEHM_GC

  assert(gc_teb->gc_teb_inside_tramp);
  assert(dylan_check(p));
  return mps_commit(gc_teb->gc_teb_main_ap, p, size);

#else

  return 1;

#endif
}


__inline
void *MMReserveLeaf(size_t size, void *wrapper, gc_teb_t gc_teb)
{
#ifndef BOEHM_GC

  reserve_memory_for_object(size, wrapper, gc_teb, gc_teb_leaf_ap, leaf_handler, "MMReserveLeaf");

#else

  return GC_malloc_atomic(size);

#endif
}

__inline
int MMCommitLeaf(void *p, size_t size, gc_teb_t gc_teb)
{
#ifndef BOEHM_GC

  assert(gc_teb->gc_teb_inside_tramp);
  assert(dylan_check(p));
  return mps_commit(gc_teb->gc_teb_leaf_ap, p, size);

#else

  return 1;

#endif
}

MMAllocHandler MMReserveLeafHandler(MMAllocHandler handler)
{
  MMAllocHandler h = leaf_handler;
  leaf_handler = handler;
  return h;
}

__inline
void *MMReserveExactAWL(size_t size, void *wrapper, gc_teb_t gc_teb)
{
#ifndef BOEHM_GC

  reserve_memory_for_object(size, wrapper, gc_teb, gc_teb_exact_awl_ap, exact_awl_handler, "MMReserveExactAWL");

#else

  return GC_malloc(size);

#endif
}

__inline
int MMCommitExactAWL(void *p, size_t size, gc_teb_t gc_teb)
{
#ifndef BOEHM_GC

  assert(gc_teb->gc_teb_inside_tramp);
  assert(dylan_check(p));
  return mps_commit(gc_teb->gc_teb_exact_awl_ap, p, size);

#else

  return 1;

#endif
}

MMAllocHandler MMReserveExactAWLHandler(MMAllocHandler handler)
{
  MMAllocHandler h = exact_awl_handler;
  exact_awl_handler = handler;
  return h;
}

__inline
void *MMReserveWeakAWL(size_t size, void *wrapper, gc_teb_t gc_teb)
{
#ifndef BOEHM_GC

  reserve_memory_for_object(size, wrapper, gc_teb, gc_teb_weak_awl_ap, weak_awl_handler, "MMReserveWeakAWL");

#else

  return GC_malloc(size);

#endif
}

__inline
int MMCommitWeakAWL(void *p, size_t size, gc_teb_t gc_teb)
{
#ifndef BOEHM_GC

  assert(gc_teb->gc_teb_inside_tramp);
  assert(dylan_check(p));
  return mps_commit(gc_teb->gc_teb_weak_awl_ap, p, size);

#else

  return 1;

#endif
}

MMAllocHandler MMReserveWeakAWLHandler(MMAllocHandler handler)
{
  MMAllocHandler h = weak_awl_handler;
  weak_awl_handler = handler;
  return h;
}


MMAllocHandler MMReserveObjectHandler(MMAllocHandler handler)
{
  MMAllocHandler h = main_handler;
  main_handler = handler;
  return h;
}

__inline
void *MMReserveWrapper(size_t size, void *wrapper, gc_teb_t gc_teb)
{
#ifndef BOEHM_GC

  mps_res_t res;
  mps_addr_t p;

  assert(gc_teb->gc_teb_inside_tramp);

  res = mps_alloc(&p, wrapper_pool, size);
  if(res) {
    (*wrapper_handler)((MMError)res, "MMReserveWrapper", size);
    return (void *)NULL;
  }

  return (void *)p;

#else

  return GC_malloc_atomic(size);

#endif
}

/* We declare each wrapper as a root on commit.  As a flip may
 * happen between reserve and commit, the wrapper may be initialized
 * to contain any moveable references.
 */
__inline
int MMCommitWrapper(void *p, size_t size, gc_teb_t gc_teb)
{
#ifndef BOEHM_GC

  mps_res_t res;
  mps_root_t root;

  assert(gc_teb->gc_teb_inside_tramp);
  assert(dylan_check(p));
  // there used to be a call to dylan_wrapper_check(p) here, but
  // the wrapper isn't properly initialized until after allocation.
  // So the check will always fail.

  res = mps_root_create_fmt(&root, arena, MPS_RANK_EXACT,
                             (mps_rm_t)0, fmt_A->scan, p, (char *)p + size);
  if(res) return 0;
  return 1;

#else

  return 1;

#endif
}

MMAllocHandler MMReserveWrapperHandler(MMAllocHandler handler)
{
  MMAllocHandler h = wrapper_handler;
  wrapper_handler = handler;
  return h;
}

void *MMAllocMisc(size_t size)
{
#ifndef BOEHM_GC

  mps_res_t res;
  void *p;
  /* gc_teb_t gc_teb = current_gc_teb(); */

  /*  assert(gc_teb->gc_teb_inside_tramp);   not a necessary condition for misc mem  */

  res = mps_alloc((mps_addr_t *)&p, misc_pool, size);
  if(res) {
    (*misc_handler)((MMError)res, "MMAllocMisc", size);
    return NULL;
  }

  return p;

#else

  return GC_malloc_atomic(size);

#endif
}

MMAllocHandler MMAllocMiscHandler(MMAllocHandler handler)
{
  MMAllocHandler h = misc_handler;
  misc_handler = handler;
  return h;
}

void MMFreeMisc(void *old, size_t size)
{
#ifndef BOEHM_GC

  /* gc_teb_t gc_teb = current_gc_teb(); */

  /* assert(gc_teb->gc_teb_inside_tramp); */
  mps_free(misc_pool, (mps_addr_t)old, size);

#else

  GC_free(old);

#endif
}


/* Streamlined allocation primitives */

/*

  There are a variety of specialized allocators, which allocate
  in different pools, and perform different combinations
  of initialization.

  The names follow the following pattern:-
    primitive_alloc{pool_opt}{slot_opt}{repeat_opt}

  All take arguments (size_t size, void *wrapper, {parameters-for-options})

  Here are the options, with their parameters (although not all combinations
  are necessarily implemented):-

  pool_opt:

    <default>    ()                    Allocate in AMC
    _leaf        ()                    Allocate in LO
    _exact_awl   (assoc)               Allocate exact in AWL
    _weak_awl    (assoc)               Allocate weak in AWL
    _wrapper     ()                    Allocate in wrapper pool

  slot_opt:
    <default>    ()                    No initialization of fixed slots
    _s1          (data1)               Fill slot 1 with data1
    _s2          (data1, data2)        Fill slot 1 with data1, slot 2 with data2
    _s           (fill_num, fill)      Fill fill_num slots with fill


  repeat:opt
    <default>    ()                    No initializtion of repeated slots
    _r           (rep_size, offset)    Set repeated slot size at offset (raw param)
    _rf          (rep_size, off, fill) Set size slot and fill repeated data
    _rt          (rep_size, off, templ)Fill repeated data from template
    _ruf         (rep_size, off, fill) Set size slot and fill repeated untraced data
    _ruz         (rep_size, off)       Set rep slot size. Zero terminate untraced data
    _rufz        (rep_size, off, fill) Set size slot, fill & zero terminate  untraced data



*/


#define alloc_internal(size,  \
                       wrapper,  \
                         \
                       s1q,  /* init first 2 fixed slots */  \
                       s1,  \
                       s2q,  \
                       s2,  \
                         \
                       sq,   /* init any fixed slots */  \
                       no_to_fill,  \
                       fill,  \
                         \
                       rq,   /* init repeated slot size */  \
                       rep_size,  \
                       rep_size_slot,  \
                         \
                       rfq,  /* init repeated slot data for type */  \
                       type, \
                       word_fill,  \
                         \
                       ufq,  /* init untraced repeated slot data */  \
                       ztq,  \
                       type2, \
                       untraced_fill,  \
                         \
                       reserve,  \
                       commit)  \
{  \
  \
  size_t msize = (size);  \
  void *mwrapper = (wrapper);  \
                                 \
  mps_bool_t ms1q = (s1q);  /* init first 2 fixed slots */  \
  void *ms1 = (s1);  \
  mps_bool_t ms2q = (s2q);  \
  void *ms2 = (s2);  \
    \
  mps_bool_t msq = (sq);   /* init other fixed slots */  \
  int mno_to_fill = (no_to_fill);  \
  void *mfill = (fill);  \
    \
  mps_bool_t mrq = (rq);   /* init repeated slot size */  \
  int mrep_size = (rep_size);  \
  int mrep_size_slot = (rep_size_slot);  \
    \
  mps_bool_t mrfq = (rfq);  /* init word repeated slot data */  \
  type mword_fill = (type)(word_fill);  \
    \
  mps_bool_t mufq = (ufq);  /* init untraced repeated slot data */  \
  mps_bool_t mztq = (ztq);  \
  type2 muntraced_fill = (type2)(untraced_fill);  \
  \
  void **object;  \
  \
  gc_teb_t gc_teb = current_gc_teb();  \
  \
  update_allocation_counter(gc_teb, msize, wrapper);  \
  \
  do {  \
    object = reserve(msize, wrapper, gc_teb);  \
    object[0] = mwrapper;  \
    if (msq) fill_dylan_object_mem(object + 1, mfill, mno_to_fill);  \
    if (ms1q) object[1] = ms1;  \
    if (ms2q) object[2] = ms2;  \
    if (mrq)  \
      if (mrep_size_slot)  \
        object[mrep_size_slot] = (void*)((mrep_size << 2) + 1);  \
    if (mrfq) fill_ ## type ## _mem((type *)(object + mrep_size_slot + 1), mword_fill, mrep_size);  \
  }  \
  while(!commit(object, msize, gc_teb));  \
  \
  if (mufq && mrq) {  \
    untraced_fill_ ## type2 ## _mem(object, muntraced_fill, mrep_size, mrep_size_slot, mztq); \
  }  \
    \
  \
  return object;  \
}

RUN_TIME_API
void *primitive_alloc(size_t size,
                      void *wrapper)
{
  alloc_internal(size, wrapper,
                 0, 0, 0, 0,
                 0, 0, 0,
                 0, 0, 0,
                 0, dylan_object, 0,
                 0, 0, dylan_object, 0,
                 MMReserveObject, MMCommitObject);
}


RUN_TIME_API
void *primitive_alloc_s1(size_t size,
                          void *wrapper,
                          void *data1)
{
  alloc_internal(size, wrapper,
                 1, data1, 0, 0,
                 0, 0, 0,
                 0, 0, 0,
                 0, dylan_object, 0,
                 0, 0, dylan_object, 0,
                 MMReserveObject, MMCommitObject);
}

RUN_TIME_API
void *primitive_alloc_s2(size_t size,
                          void *wrapper,
                          void *data1,
                          void *data2)
{
  alloc_internal(size, wrapper,
                 1, data1, 1, data2,
                 0, 0, 0,
                 0, 0, 0,
                 0, dylan_object, 0,
                 0, 0, dylan_object, 0,
                 MMReserveObject, MMCommitObject);
}


RUN_TIME_API
void *primitive_alloc_s(size_t size,
                        void *wrapper,
                        int no_to_fill,
                        void *fill)
{
  alloc_internal(size, wrapper,
                 0, 0, 0, 0,
                 1, no_to_fill, fill,
                 0, 0, 0,
                 0, dylan_object, 0,
                 0, 0, dylan_object, 0,
                 MMReserveObject, MMCommitObject);
}


RUN_TIME_API
void *primitive_alloc_r(size_t size,
                        void *wrapper,
                        int rep_size,
                        int rep_size_slot)
{
  alloc_internal(size, wrapper,
                 0, 0, 0, 0,
                 0, 0, 0,
                 1, rep_size, rep_size_slot,
                 0, dylan_object, 0,
                 0, 0, dylan_object, 0,
                 MMReserveObject, MMCommitObject);
}

RUN_TIME_API
void *primitive_alloc_rf(size_t size,
                         void *wrapper,
                         int rep_size,
                         int rep_size_slot,
                         dylan_object fill)
{
  alloc_internal(size, wrapper,
                 0, 0, 0, 0,
                 0, 0, 0,
                 1, rep_size, rep_size_slot,
                 1, dylan_object, fill,
                 0, 0, dylan_object, 0,
                 MMReserveObject, MMCommitObject);
}


RUN_TIME_API
void *primitive_alloc_s_r(size_t size,
                          void *wrapper,
                          int no_to_fill,
                          void *fill,
                          int rep_size,
                          int rep_size_slot)
{
  alloc_internal(size, wrapper,
                 0, 0, 0, 0,
                 1, no_to_fill, fill,
                 1, rep_size, rep_size_slot,
                 0, dylan_object, 0,
                 0, 0, dylan_object, 0,
                 MMReserveObject, MMCommitObject);
}


#define define_primitive_alloc_s_rf(type, suffix) \
RUN_TIME_API \
void *primitive_alloc_s_ ## suffix(size_t size, \
                                   void *wrapper, \
                                   int no_to_fill, \
                                   void *fill, \
                                   int rep_size, \
                                   int rep_size_slot, \
                                   type rep_fill) \
{ \
  alloc_internal(size, wrapper,  \
                 0, 0, 0, 0, \
                 1, no_to_fill, fill,  \
                 1, rep_size, rep_size_slot, \
                 1, type, rep_fill, \
                 0, 0, dylan_object, 0, \
                 MMReserveObject, MMCommitObject); \
}

define_primitive_alloc_s_rf(dylan_object, rf)
define_primitive_alloc_s_rf(half_word, rhf)
define_primitive_alloc_s_rf(single_float, rsff)
define_primitive_alloc_s_rf(double_float, rdff)
define_primitive_alloc_s_rf(double_word, rdwf)


RUN_TIME_API
void *primitive_alloc_s_rbf(size_t size,
                            void *wrapper,
                            int no_to_fill,
                            void *fill,
                            int rep_size,
                            int rep_size_slot,
                            int byte_fill)
{
  alloc_internal(size, wrapper,
                 0, 0, 0, 0,
                 1, no_to_fill, fill,
                 1, rep_size, rep_size_slot,
                 0, dylan_object, 0,
                 1, 0, byte_char, byte_fill,
                 MMReserveObject, MMCommitObject);
}


RUN_TIME_API
void *primitive_alloc_s_rbfz(size_t size,
                             void *wrapper,
                             int no_to_fill,
                             void *fill,
                             int rep_size,
                             int rep_size_slot,
                             int byte_fill)
{
  alloc_internal(size, wrapper,
                 0, 0, 0, 0,
                 1, no_to_fill, fill,
                 1, rep_size, rep_size_slot,
                 0, dylan_object, 0,
                 1, 1, byte_char, byte_fill,
                 MMReserveObject, MMCommitObject);
}


RUN_TIME_API
void *primitive_alloc_rbfz(size_t size,
                           void *wrapper,
                           int rep_size,
                           int rep_size_slot,
                           int byte_fill)
{
  alloc_internal(size, wrapper,
                 0, 0, 0, 0,
                 0, 0, 0,
                 1, rep_size, rep_size_slot,
                 0, dylan_object, 0,
                 1, 1, byte_char, byte_fill,
                 MMReserveObject, MMCommitObject);
}


RUN_TIME_API
void *primitive_alloc_s_rb(size_t size,
                           void *wrapper,
                           int no_to_fill,
                           void *fill,
                           int rep_size,
                           int rep_size_slot)
{
  alloc_internal(size, wrapper,
                 0, 0, 0, 0,
                 1, no_to_fill, fill,
                 1, rep_size, rep_size_slot,
                 0, dylan_object, 0,
                 0, 0, dylan_object, 0,
                 MMReserveObject, MMCommitObject);
}


RUN_TIME_API
void *primitive_alloc_leaf(size_t size,
                           void *wrapper)
{
  alloc_internal(size, wrapper,
                 0, 0, 0, 0,
                 0, 0, 0,
                 0, 0, 0,
                 0, dylan_object, 0,
                 0, 0, dylan_object, 0,
                 MMReserveLeafObject, MMCommitLeafObject);
}


RUN_TIME_API
void *primitive_alloc_leaf_s_r(size_t size,
                               void *wrapper,
                               int no_to_fill,
                               void *fill,
                               int rep_size,
                               int rep_size_slot)
{
  alloc_internal(size, wrapper,
                 0, 0, 0, 0,
                 1, no_to_fill, fill,
                 1, rep_size, rep_size_slot,
                 0, dylan_object, 0,
                 0, 0, dylan_object, 0,
                 MMReserveLeafRepeated, MMCommitLeafRepeated);
}



RUN_TIME_API
void *primitive_alloc_leaf_s1(size_t size,
                              void *wrapper,
                              void *data1)
{
  alloc_internal(size, wrapper,
                 1, data1, 0, 0,
                 0, 0, 0,
                 0, 0, 0,
                 0, dylan_object, 0,
                 0, 0, dylan_object, 0,
                 MMReserveLeafObject, MMCommitLeafObject);
}

RUN_TIME_API
void *primitive_alloc_leaf_s2(size_t size,
                              void *wrapper,
                              void *data1,
                              void *data2)
{
  alloc_internal(size, wrapper,
                 1, data1, 1, data2,
                 0, 0, 0,
                 0, 0, 0,
                 0, dylan_object, 0,
                 0, 0, dylan_object, 0,
                 MMReserveLeafObject, MMCommitLeafObject);
}


RUN_TIME_API
void *primitive_alloc_leaf_s(size_t size,
                             void *wrapper,
                             int no_to_fill,
                             void *fill)
{
  alloc_internal(size, wrapper,
                 0, 0, 0, 0,
                 1, no_to_fill, fill,
                 0, 0, 0,
                 0, dylan_object, 0,
                 0, 0, dylan_object, 0,
                 MMReserveLeafObject, MMCommitLeafObject);
}



RUN_TIME_API
void *primitive_alloc_leaf_r(size_t size,
                             void *wrapper,
                             int rep_size,
                             int rep_size_slot)
{
  alloc_internal(size, wrapper,
                 0, 0, 0, 0,
                 0, 0, 0,
                 1, rep_size, rep_size_slot,
                 0, dylan_object, 0,
                 0, 0, dylan_object, 0,
                 MMReserveLeafRepeated, MMCommitLeafRepeated);
}


RUN_TIME_API
void *primitive_alloc_leaf_s_rbf(size_t size,
                                 void *wrapper,
                                 int no_to_fill,
                                 void *fill,
                                 int rep_size,
                                 int rep_size_slot,
                                 int byte_fill)
{
  alloc_internal(size, wrapper,
                 0, 0, 0, 0,
                 1, no_to_fill, fill,
                 1, rep_size, rep_size_slot,
                 0, dylan_object, 0,
                 1, 0, byte_char, byte_fill,
                 MMReserveLeafRepeated, MMCommitLeafRepeated);
}

#define define_primitive_alloc_leaf_rf(type, suffix) \
RUN_TIME_API \
void *primitive_alloc_leaf_ ## suffix(size_t size, \
                                      void *wrapper, \
                                      int rep_size, \
                                      int rep_size_slot, \
                                      type rep_fill) \
{ \
  alloc_internal(size, wrapper,  \
                 0, 0, 0, 0, \
                 1, 0, 0,  \
                 1, rep_size, rep_size_slot, \
                 0, dylan_object, 0, \
                 1, 0, type, rep_fill, \
                 MMReserveLeafRepeated, MMCommitLeafRepeated); \
}


define_primitive_alloc_leaf_rf(dylan_object, rf)
define_primitive_alloc_leaf_rf(byte_char, rbf)
define_primitive_alloc_leaf_rf(half_word, rhf)
define_primitive_alloc_leaf_rf(single_float, rsff)
define_primitive_alloc_leaf_rf(double_float, rdff)
define_primitive_alloc_leaf_rf(double_word, rdwf)

RUN_TIME_API
void *primitive_alloc_leaf_s_rbfz(size_t size,
                                  void *wrapper,
                                  int no_to_fill,
                                  void *fill,
                                  int rep_size,
                                  int rep_size_slot,
                                  int byte_fill)
{
  alloc_internal(size, wrapper,
                 0, 0, 0, 0,
                 1, no_to_fill, fill,
                 1, rep_size, rep_size_slot,
                 0, dylan_object, 0,
                 1, 1, byte_char, byte_fill,
                 MMReserveLeafTerminated, MMCommitLeafTerminated);
}


RUN_TIME_API
void *primitive_alloc_leaf_rbfz(size_t size,
                                void *wrapper,
                                int rep_size,
                                int rep_size_slot,
                                int byte_fill)
{
  alloc_internal(size, wrapper,
                 0, 0, 0, 0,
                 0, 0, 0,
                 1, rep_size, rep_size_slot,
                 0, dylan_object, 0,
                 1, 1, byte_char, byte_fill,
                 MMReserveLeafTerminated, MMCommitLeafTerminated);
}


RUN_TIME_API
void *primitive_alloc_leaf_s_rb(size_t size,
                                void *wrapper,
                                int no_to_fill,
                                void *fill,
                                int rep_size,
                                int rep_size_slot)
{
  alloc_internal(size, wrapper,
                 0, 0, 0, 0,
                 1, no_to_fill, fill,
                 1, rep_size, rep_size_slot,
                 0, dylan_object, 0,
                 0, 0, dylan_object, 0,
                 MMReserveLeafRepeated, MMCommitLeafRepeated);
}


RUN_TIME_API
void *primitive_alloc_exact_awl_s_r(size_t size,
                                    void *wrapper,
                                    void *assoc,
                                    int no_to_fill,
                                    void *fill,
                                    int rep_size,
                                    int rep_size_slot)
{
  alloc_internal(size, wrapper,
                 1, assoc, 0, 0,
                 1, no_to_fill, fill,
                 1, rep_size, rep_size_slot,
                 0, dylan_object, 0,
                 0, 0, dylan_object, 0,
                 MMReserveExactAWL, MMCommitExactAWL);
}


RUN_TIME_API
void *primitive_alloc_weak_awl_s_r(size_t size,
                                   void *wrapper,
                                   void *assoc,
                                   int no_to_fill,
                                   void *fill,
                                   int rep_size,
                                   int rep_size_slot)
{
  alloc_internal(size, wrapper,
                 1, assoc, 0, 0,
                 1, no_to_fill, fill,
                 1, rep_size, rep_size_slot,
                 0, dylan_object, 0,
                 0, 0, dylan_object, 0,
                 MMReserveWeakAWL, MMCommitWeakAWL);
}


RUN_TIME_API
void *primitive_alloc_exact_awl_rf(size_t size,
                                   void *wrapper,
                                   void *assoc,
                                   int rep_size,
                                   int rep_size_slot,
                                   void *fill)
{
  alloc_internal(size, wrapper,
                 1, assoc, 0, 0,
                 0, 0, 0,
                 1, rep_size, rep_size_slot,
                 1, dylan_object, fill,
                 0, 0, dylan_object, 0,
                 MMReserveExactAWL, MMCommitExactAWL);
}


RUN_TIME_API
void *primitive_alloc_weak_awl_rf(size_t size,
                                  void *wrapper,
                                  void *assoc,
                                  int rep_size,
                                  int rep_size_slot,
                                  void *fill)
{
  alloc_internal(size, wrapper,
                 1, assoc, 0, 0,
                 0, 0, 0,
                 1, rep_size, rep_size_slot,
                 1, dylan_object, fill,
                 0, 0, dylan_object, 0,
                 MMReserveWeakAWL, MMCommitWeakAWL);
}


RUN_TIME_API
void *primitive_alloc_wrapper_s_r(size_t size,
                                  void *wrapper,
                                  int no_to_fill,
                                  void *fill,
                                  int rep_size,
                                  int rep_size_slot)
{
  alloc_internal(size, wrapper,
                 0, 0, 0, 0,
                 1, no_to_fill, fill,
                 1, rep_size, rep_size_slot,
                 0, dylan_object, 0,
                 0, 0, dylan_object, 0,
                 MMReserveWrapper, MMCommitWrapper);
}


RUN_TIME_API
void *primitive_alloc_rt(size_t size,
                         void *wrapper,
                         int rep_size,
                         int rep_size_slot,
                         void *template)
{
  void **object;

  gc_teb_t gc_teb = current_gc_teb();

  update_allocation_counter(gc_teb, size, wrapper);

  do {
    int findex = 1;
    object = MMReserveObject(size, wrapper, gc_teb);
    object[0] = wrapper;
    object[rep_size_slot] = (void*)((rep_size << 2) + 1);
    memcpy(object + rep_size_slot + 1, template, rep_size << 2);
  }
  while(!MMCommitObject(object, size, gc_teb));


  return object;
}

RUN_TIME_API
void *primitive_copy(size_t size,
                     void *template)
{
  void **object;
  void *wrapper = ((void**)template)[0];

  gc_teb_t gc_teb = current_gc_teb();

  update_allocation_counter(gc_teb, size, wrapper);

  do {
    int findex = 1;
    object = MMReserveObject(size, wrapper, gc_teb);
    memcpy(object, template, size);
  }
  while(!MMCommitObject(object, size, gc_teb));

  return object;
}


/* Copy all but the repeated slots of a template */

RUN_TIME_API
void *primitive_copy_r(size_t size,
                       int rep_size,
                       int rep_size_slot,
                       void *template)
{
  void **object;
  void *wrapper = ((void**)template)[0];

  gc_teb_t gc_teb = current_gc_teb();

  update_allocation_counter(gc_teb, size, wrapper);

  do {
    int findex = 1;
    object = MMReserveObject(size, wrapper, gc_teb);
    memcpy(object, template, rep_size_slot << 2);
    object[rep_size_slot] = (void*)((rep_size << 2) + 1);
    /* ### kludge to prevent committing uninitialized memory */
    fill_dylan_object_mem((void **)(object + rep_size_slot + 1),
                          NULL, rep_size);
  }
  while(!MMCommitObject(object, size, gc_teb));


  return object;
}




unsigned MMCollectCount(void)
{
  gc_teb_t gc_teb = current_gc_teb();

  assert(gc_teb->gc_teb_inside_tramp);
#ifndef BOEHM_GC
  return (unsigned)mps_collections(arena);
#else
  return 0;
#endif
}

MMError MMRegisterRootStatic(mps_root_t *rootp, void *base, void *limit)
{
#ifndef BOEHM_GC
  /* assert(gc_teb->gc_teb_inside_tramp); tramp not needed for root registration */
  return mps_root_create_fmt(rootp, arena, MPS_RANK_EXACT,
                             MPS_RM_PROT, fmt_A->scan, base, limit);
#else
  return 0;
#endif
}

MMError MMRegisterRootImmut(mps_root_t *rootp, void *base, void *limit)
{
#ifndef BOEHM_GC
  /* assert(gc_teb->gc_teb_inside_tramp); tramp not needed for root registration */
  return mps_root_create_fmt(rootp, arena, MPS_RANK_EXACT,
                             MPS_RM_CONST, fmt_A->scan, base, limit);
#else
  return 0;
#endif
}


/* Don't protect ambiguous roots. That's because they're used */
/* for managing low-level runtime data including the TEBs.    */
/* In particular, they might be referenced by the Dylan trap  */
/* handler which must not be allowed to recursively trap      */

MMError MMRegisterRootAmbig(mps_root_t *rootp, void *base, void *limit)
{
#ifndef BOEHM_GC
  size_t s = ((char *)limit - (char *)base) / sizeof(mps_addr_t);
  /* assert(gc_teb->gc_teb_inside_tramp); tramp not needed for root registration */
  return mps_root_create_table(rootp, arena, MPS_RANK_AMBIG,
                               0, base, s);
#else
  return 0;
#endif
}

MMError MMRegisterRootExact(mps_root_t *rootp, void *base, void *limit)
{
#ifndef BOEHM_GC
  size_t s = ((char *)limit - (char *)base) / sizeof(mps_addr_t);
  /* assert(gc_teb->gc_teb_inside_tramp); tramp not needed for root registration */
  return mps_root_create_table_masked(rootp, arena, MPS_RANK_EXACT,
                                      MPS_RM_PROT, base, s, 3);
#else
  return 0;
#endif
}

void MMDeregisterRoot(mps_root_t root)
{
#ifndef BOEHM_GC
  if (root) {
    mps_root_destroy(root);
  }
#endif
}



void *dylan__malloc__ambig(size_t size)
{
  size_t new_size = size + 4;
  void *object = MMAllocMisc(new_size);

  MMRegisterRootAmbig(object, object, (char *)object + new_size);
  return (void *)((char *)object + 4);
}

/* This doesn't work yet -- results in GC anomaly; to be debugged

   Nosa  Mar 15, 1999  */

void *dylan__malloc__exact(size_t size)
{
  size_t new_size = size + 4;
  void *object = MMAllocMisc(new_size);

  MMRegisterRootExact(object, object, (char *)object + new_size);
  return (void *)((char *)object + 4);
}

void dylan__free__root(void *object, size_t size)
{
  size_t new_size = size + 4;
  void *new_object = (void *)((char *)object - 4);

  MMDeregisterRoot(((void**)new_object)[0]);
  MMFreeMisc(new_object, new_size);
}



/* Root regsitration support for the interactive downloader       */
/* This doesn't need to remember the root                         */
/* It must not use MPS_RM_PROT (see the cottonwood release notes) */


MMError MMRootStatic(void *base, void *limit)
{
#ifndef BOEHM_GC
  mps_root_t root;
  return mps_root_create_fmt(&root, arena, MPS_RANK_EXACT,
                             0, fmt_A->scan, base, limit);
#else
  return 0;
#endif
}

MMError MMRootImmut(void *base, void *limit)
{
  mps_root_t root;
  return  MMRegisterRootImmut(&root, base, limit);
}

MMError MMRootAmbig(void *base, void *limit)
{
#ifndef BOEHM_GC
  mps_root_t root;
  size_t s = ((char *)limit - (char *)base) / sizeof(mps_addr_t);
  return mps_root_create_table(&root, arena, MPS_RANK_AMBIG,
                               0, base, s);
#else
  return 0;
#endif
}

MMError MMRootExact(void *base, void *limit)
{
#ifndef BOEHM_GC
  mps_root_t root;
  size_t s = ((char *)limit - (char *)base) / sizeof(mps_addr_t);
  return mps_root_create_table_masked(&root, arena, MPS_RANK_EXACT,
                                      0, base, s, 3);
#else
  return 0;
#endif
}


/* Support for MM control */

RUN_TIME_API
void primitive_mps_clamp()
{
#ifndef BOEHM_GC
  mps_arena_clamp(arena);
#endif
}

RUN_TIME_API
void primitive_mps_park()
{
#ifndef BOEHM_GC
  mps_arena_park(arena);
#endif
}

RUN_TIME_API
void primitive_mps_release()
{
#ifndef BOEHM_GC
  mps_arena_release(arena);
#endif
}

extern void display_stats_for_memory_usage ();

RUN_TIME_API
void primitive_mps_collect(BOOL display_stats)
{
#ifndef BOEHM_GC
  mps_arena_collect(arena);
  if (display_stats)
    display_stats_for_memory_usage();
#endif
}

RUN_TIME_API
size_t primitive_mps_committed()
{
#ifndef BOEHM_GC
  return mps_arena_committed(arena);
#else
  return 0;
#endif
}

RUN_TIME_API
void primitive_mps_begin_ramp_alloc()
{
#ifndef BOEHM_GC
  gc_teb_t gc_teb = current_gc_teb();
  mps_alloc_pattern_t pattern = mps_alloc_pattern_ramp();

  mps_ap_alloc_pattern_begin(gc_teb->gc_teb_main_ap, pattern);
  mps_ap_alloc_pattern_begin(gc_teb->gc_teb_leaf_ap, pattern);
  mps_ap_alloc_pattern_begin(gc_teb->gc_teb_weak_awl_ap, pattern);
  mps_ap_alloc_pattern_begin(gc_teb->gc_teb_exact_awl_ap, pattern);
#endif
}

RUN_TIME_API
void primitive_mps_end_ramp_alloc()
{
#ifndef BOEHM_GC
  gc_teb_t gc_teb = current_gc_teb();
  mps_alloc_pattern_t pattern = mps_alloc_pattern_ramp();

  mps_ap_alloc_pattern_end(gc_teb->gc_teb_main_ap, pattern);
  mps_ap_alloc_pattern_end(gc_teb->gc_teb_leaf_ap, pattern);
  mps_ap_alloc_pattern_end(gc_teb->gc_teb_weak_awl_ap, pattern);
  mps_ap_alloc_pattern_end(gc_teb->gc_teb_exact_awl_ap, pattern);
#endif
}

RUN_TIME_API
void primitive_mps_begin_ramp_alloc_all()
{
#ifndef BOEHM_GC
  gc_teb_t gc_teb = current_gc_teb();
  mps_alloc_pattern_t pattern = mps_alloc_pattern_ramp_collect_all();

  mps_ap_alloc_pattern_begin(gc_teb->gc_teb_main_ap, pattern);
  mps_ap_alloc_pattern_begin(gc_teb->gc_teb_leaf_ap, pattern);
  mps_ap_alloc_pattern_begin(gc_teb->gc_teb_weak_awl_ap, pattern);
  mps_ap_alloc_pattern_begin(gc_teb->gc_teb_exact_awl_ap, pattern);
#endif
}

RUN_TIME_API
void primitive_mps_end_ramp_alloc_all()
{
#ifndef BOEHM_GC
  gc_teb_t gc_teb = current_gc_teb();
  mps_alloc_pattern_t pattern = mps_alloc_pattern_ramp_collect_all();

  mps_ap_alloc_pattern_end(gc_teb->gc_teb_main_ap, pattern);
  mps_ap_alloc_pattern_end(gc_teb->gc_teb_leaf_ap, pattern);
  mps_ap_alloc_pattern_end(gc_teb->gc_teb_weak_awl_ap, pattern);
  mps_ap_alloc_pattern_end(gc_teb->gc_teb_exact_awl_ap, pattern);
#endif
}


mps_message_t message;

RUN_TIME_API
void primitive_mps_enable_gc_messages()
{
#ifndef BOEHM_GC
  mps_message_type_enable(arena, mps_message_type_gc());
#endif
}


RUN_TIME_API
BOOL primitive_mps_collection_stats(void** results)
{
#ifndef BOEHM_GC
  size_t live, condemned, not_condemned;

  if (mps_message_get(&message, arena, mps_message_type_gc())) {

    live = mps_message_gc_live_size(arena, message);
    condemned = mps_message_gc_condemned_size(arena, message);
    not_condemned = mps_message_gc_not_condemned_size(arena, message);

    mps_message_discard(arena, message);

    results[0] =   (void*)((live << 2) + 1);
    results[1] = (void*)((condemned << 2) + 1);
    results[2] = (void*)((not_condemned << 2) + 1);
    return TRUE;
  }
  else
    return FALSE;
#else
  return FALSE;
#endif
}


/* Support for Finalization */


void primitive_mps_finalize(void *obj)
{
#ifndef NO_FINALIZATION
  mps_finalize(arena, &obj);
#endif
}

void* primitive_mps_finalization_queue_first()
{
#ifdef NO_FINALIZATION
  return 0;
#else
  mps_message_t finalization_message;
  if (mps_message_get(&finalization_message, arena, finalization_type))
    {
      mps_addr_t object_ref;
      mps_message_finalization_ref(&object_ref, arena, finalization_message);
      mps_message_discard(arena, finalization_message);
      return object_ref;
    }
  else
    return 0;
#endif
}

/* Support for Location Dependencies */

typedef struct d_hs_s      *d_hs_t;     /* Dylan Hash State */

typedef struct d_hs_s         /* Dylan Hash State object */
{
  void *dylan_wrapper;
  mps_ld_s internal_state;
} d_hs_s;



void primitive_mps_ld_reset(d_hs_t d_hs)
{
#ifndef BOEHM_GC
  mps_ld_t mps_ld = &(d_hs->internal_state);
  gc_teb_t gc_teb = current_gc_teb();
  assert(gc_teb->gc_teb_inside_tramp);
  mps_ld_reset(mps_ld, arena);
#endif
}

void primitive_mps_ld_add(d_hs_t d_hs, mps_addr_t addr)
{
#ifndef BOEHM_GC
  mps_ld_t mps_ld = &(d_hs->internal_state);
  gc_teb_t gc_teb = current_gc_teb();
  assert(gc_teb->gc_teb_inside_tramp);
  mps_ld_add(mps_ld, arena, addr);
#endif
}

mps_bool_t primitive_mps_ld_isstale(d_hs_t d_hs)
{
#ifndef BOEHM_GC

  mps_ld_t mps_ld = &(d_hs->internal_state);
  gc_teb_t gc_teb = current_gc_teb();
  assert(gc_teb->gc_teb_inside_tramp);
  return(mps_ld_isstale(mps_ld, arena, 0));

#else

  return 0; /* Never stale */

#endif
}


void primitive_mps_ld_merge(d_hs_t d_into, d_hs_t d_obj)
{
#ifndef BOEHM_GC
  mps_ld_t into = &(d_into->internal_state);
  mps_ld_t addr = &(d_obj->internal_state);
  gc_teb_t gc_teb = current_gc_teb();
  assert(gc_teb->gc_teb_inside_tramp);
  mps_ld_merge(into, arena, addr);
#endif
}



/* initialization and deinitialization */

void init_error (char* message)
{
  report_runtime_error("\nDylan runtime MPS initialization error: failed to ", message);
}



extern BOOL Prunning_under_dylan_debuggerQ;

/*
    The strategy at the moment for handling keyboard interrupts is merely
    to set a flag; the runtime will check this flag periodically (e.g. every
    time an attempt is made to heap-allocate an object) and signal a keyboard
    interrupt at that time. Provision is also made for applications to do their
    own polling of this flag, for example in a dedicated thread, if they so wish.
*/


BOOL WINAPI DylanBreakControlHandler(DWORD dwCtrlType)
{
  switch (dwCtrlType)
    {
    case CTRL_BREAK_EVENT:
    case CTRL_C_EVENT:
      {
        if (Prunning_under_dylan_debuggerQ == FALSE)
          dylan_keyboard_interruptQ = TRUE;
        return TRUE;
      }

    default:
      return FALSE;
    }
}

#ifndef BOEHM_GC
#include <stdlib.h>
#include <limits.h>

static mps_gen_param_s *
get_gen_params(const char *spec,
               size_t *gen_count_return,
               size_t *max_heap_size_return)
{
  size_t gen_count = 0;
  size_t max_heap_size = 0;
  mps_gen_param_s *params = NULL;

  while(*spec != '\0') {
    char *end;
    unsigned long capacity = strtoul(spec, &end, 0);
    double mortality;

    if(capacity == 0 || capacity > 2048 * 1024 || *end != ',') {
      free(params);
      return NULL;
    }
    max_heap_size += capacity * 1024;

    spec = end + 1;
    mortality = strtod(spec, &end);
    if(mortality < 0.0 || mortality > 1.0) {
      free(params);
      return NULL;
    }

    if(*end == ';') {
      spec = end + 1;
    } else if(*end == '\0') {
      spec = end;
    } else {
      free(params);
      return NULL;
    }

    ++gen_count;
    params = realloc(params, gen_count * sizeof(mps_gen_param_s));
    params[gen_count - 1].mps_capacity = capacity;
    params[gen_count - 1].mps_mortality = mortality;
  }

  *gen_count_return = gen_count;
  *max_heap_size_return = max_heap_size;

  return params;
}
#endif

MMError dylan_init_memory_manager()
{
  mps_res_t res;
  size_t max_heap_size = MAXIMUM_HEAP_SIZE;

  gc_teb_t gc_teb = current_gc_teb();

  if (Prunning_under_dylan_debuggerQ == FALSE)
    set_CONSOLE_CTRL_HANDLER(&DylanBreakControlHandler, TRUE);

  assert(!gc_teb->gc_teb_inside_tramp);
  assert(TARG_CHECK);

#ifndef BOEHM_GC
  {
    size_t gen_count = genCOUNT;
    mps_gen_param_s *params = NULL;

#ifdef _WIN32
    char specbuf[2048];
    const char *spec = NULL;
    if(GetEnvironmentVariableA("OPEN_DYLAN_MPS_HEAP", specbuf,
                               sizeof specbuf) != 0) {
      spec = specbuf;
    }
#else
    const char *spec = getenv("OPEN_DYLAN_MPS_HEAP");
#endif

    res = mps_arena_create(&arena, mps_arena_class_vm(), max_heap_size);
    if(res) { init_error("create arena"); return(res); }

    if(spec) {
      params = get_gen_params(spec, &gen_count, &max_heap_size);
      if(!params)
        init_error("parse OPEN_DYLAN_MPS_HEAP format");
    }

    if(params) {
      res = mps_chain_create(&chain, arena, gen_count, params);
      free(params);
    } else {
      res = mps_chain_create(&chain, arena, genCOUNT, gc_default_gen_param);
    }
    if(res) { init_error("create chain"); return(res); }
  }

  fmt_A = dylan_fmt_A();
  res = mps_fmt_create_A(&format, arena, fmt_A);
  if(res) { init_error("create format"); return(res); }

  fmt_A_weak = dylan_fmt_A_weak();
  res = mps_fmt_create_A(&dylan_fmt_weak_s, arena, fmt_A_weak);
  if(res) { init_error("create weak format"); return(res); }

  res = mps_pool_create(&main_pool, arena, mps_class_amc(), format, chain);
  if(res) { init_error("create main pool"); return(res); }

  /* Create the Leaf Object pool */
  res = mps_pool_create(&leaf_pool, arena, mps_class_amcz(), format, chain);
  if(res) { init_error("create leaf pool"); return(res); }

  /* Create the Automatic Weak Linked pool */
  res = mps_pool_create(&weak_table_pool, arena, mps_class_awl(),
                        dylan_fmt_weak_s, dylan_weak_dependent);
  if(res) { init_error("create weak pool"); return(res); }

  /* Create the MV pool for miscellaneous objects. */
  /* This is also used for wrappers. */
  res = mps_pool_create(&misc_pool, arena, mps_class_mv(),
                        MISCEXTENDBY, MISCAVGSIZE, MISCMAXSIZE);
  if(res) { init_error("create misc pool"); return(res); }

  wrapper_pool = misc_pool;

#ifndef NO_FINALIZATION
  finalization_type = mps_message_type_finalization();
  mps_message_type_enable(arena, finalization_type);
#endif

#endif

#ifdef BOEHM_GC
  /* Not required for the dll version of Boehm. */
  /* GC_init(); */

#ifdef MAX_BOEHM_HEAP_SIZE
  /* Only makes sense for a 128Mb machine. */
  GC_set_max_heap_size(MAX_BOEHM_HEAP_SIZE);
#endif

#ifdef INITIAL_BOEHM_HEAP_SIZE
  /* Call this to give an initial heap size hint. */
  GC_expand_hp(INITIAL_BOEHM_HEAP_SIZE);
#endif

  /* Call this to enable incrementality. This doesn't work with the MM GC. */
  /* GC_enable_incremental(); */
#endif

  initialize_CRITICAL_SECTION(&reservoir_limit_set_lock);
  initialize_CRITICAL_SECTION(&polling_threads_lock);

  if (Prunning_under_dylan_debuggerQ) {
    initialize_CRITICAL_SECTION(&class_breakpoint_lock);
    class_breakpoint_events[0] = create_EVENT(NULL, FALSE, FALSE, NULL);
    class_breakpoint_events[1] = create_EVENT(NULL, FALSE, FALSE, NULL);
  }

  return(0);

}



void dylan_shut_down_memory_manager()
{
#ifndef BOEHM_GC

#ifndef NO_FINALIZATION
  while(primitive_mps_finalization_queue_first());
#endif
  mps_pool_destroy(misc_pool);
  mps_pool_destroy(weak_table_pool);
  mps_pool_destroy(leaf_pool);
  mps_pool_destroy(main_pool);
  mps_fmt_destroy(dylan_fmt_weak_s);
  mps_fmt_destroy(format);
  mps_chain_destroy(chain);
  mps_arena_destroy(arena);

#endif
}


#ifndef OPEN_DYLAN_PLATFORM_UNIX

extern void dylan_main ();

int main ()
{
  dylan_main();
  return 0;
}

#endif
