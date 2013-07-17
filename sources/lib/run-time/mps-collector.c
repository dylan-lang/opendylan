#include "mps.h"        /* MPS Interface */
#include "mpscmv.h"     /* MPS pool class MV */
#include "mpscamc.h"    /* MPS pool class AMC */
#include "mpsavm.h"     /* MPS arena class */
#ifndef OPEN_DYLAN_PLATFORM_UNIX
#include "mpsw3.h"
#endif
#include "fmtdy.h"      /* Dylan object format */
#include "mpslib.h"     /* plinth interface */
#include "mpscawl.h"    /* MPS pool class AWL */
#include "mpsclo.h"    /* MPS pool class LO */

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

static void update_allocation_counter(gc_teb_t gc_teb, size_t count, void* wrapper);
static void zero_allocation_counter(gc_teb_t gc_teb);

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

#define TARG_CHECK ((int)MPS_RES_OK == (int)MMSUCCESS && \
                    (int)MPS_RES_FAIL == (int)MMFAILURE && \
                    (int)MPS_RES_RESOURCE == (int)MMRESOURCE && \
                    (int)MPS_RES_MEMORY == (int)MMRESMEM && \
                    (int)MPS_RES_LIMIT == (int)MMLIMIT && \
                    (int)MPS_RES_UNIMPL == (int)MMUNIMPLEMENTED && \
                    (int)MPS_RES_IO == (int)MMIO)


extern void mps_lib_abort(void);

void report_runtime_error (char* header, char* message)
{
  mps_lib_FILE *stream = mps_lib_get_stderr();
  mps_lib_fputs(header, stream);
  mps_lib_fputs(message, stream);
  mps_lib_fputc('\n', stream);
  mps_lib_abort();
}

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

/* Default Error Handler
 *
 * This is the default error handler initially installed for all the
 * allocation interfaces in mm.h.  It prints a message on the standard
 * error stream then causes abnormal program termination.
 */
static void defaultHandler(MMError e, const char *opName, size_t size)
{
  mps_lib_FILE *stream = mps_lib_get_stderr();
  unused(e);
  unused(size);
  mps_lib_fputs("\nError: ", stream);
  mps_lib_fputs(opName, stream);
  mps_lib_fputs(" - Request to allocate failed -- aborting\n", stream);
  mps_lib_abort();
}

static mps_message_type_t finalization_type;

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

mps_arena_t arena;
mps_chain_t chain;
static mps_fmt_t format;
static mps_fmt_t dylan_fmt_weak_s;
static mps_fmt_A_t fmt_A;
static mps_fmt_A_t fmt_A_weak;
static mps_pool_t main_pool, weak_table_pool, wrapper_pool, misc_pool, leaf_pool;

/* Thread creation & deletion code */

static int num_threads = 0;

/* client estimate for handling requirements goes here */
static int low_memory_allocation_per_thread = 128 * 1024;

static define_CRITICAL_SECTION(reservoir_limit_set_lock);

static __inline
void update_runtime_thread_count(int increment)
{

  enter_CRITICAL_SECTION(&reservoir_limit_set_lock);
    num_threads = num_threads + increment;
    mps_reservoir_limit_set(arena, num_threads * low_memory_allocation_per_thread);
  leave_CRITICAL_SECTION(&reservoir_limit_set_lock);
}


MMError dylan_mm_register_thread(void *stackBot)
{
  mps_res_t res;

  gc_teb_t gc_teb = current_gc_teb();

  update_runtime_thread_count(1);

  zero_allocation_counter(gc_teb);

  res = mps_ap_create(&gc_teb->gc_teb_main_ap, main_pool, mps_rank_exact());
  if (res) goto failApCreate;

  res = mps_ap_create(&gc_teb->gc_teb_leaf_ap, leaf_pool, mps_rank_exact());
  if (res) goto failLeafApCreate;

  res = mps_ap_create(&gc_teb->gc_teb_weak_awl_ap, weak_table_pool, mps_rank_weak());
  if (res) goto failWeakAWLApCreate;

  res = mps_ap_create(&gc_teb->gc_teb_exact_awl_ap, weak_table_pool, mps_rank_exact());
  if (res) goto failExactAWLApCreate;

  res = mps_thread_reg(&gc_teb->gc_teb_thread, arena);
  if (res) goto failThreadReg;

  /* Create a root object for ambiguously scanning the stack. */
  assert(stackBot != NULL);
  res = mps_root_create_reg(&gc_teb->gc_teb_stack_root, arena, mps_rank_ambig(),
                           (mps_rm_t)0,
                            gc_teb->gc_teb_thread, mps_stack_scan_ambig, stackBot, 0);
  if (res) goto failStackRootCreate;
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
}


MMError dylan_mm_deregister_thread_from_teb(gc_teb_t gc_teb)
{
  update_runtime_thread_count(-1);
  mps_root_destroy(gc_teb->gc_teb_stack_root);
  mps_thread_dereg(gc_teb->gc_teb_thread);
  mps_ap_destroy(gc_teb->gc_teb_main_ap);
  mps_ap_destroy(gc_teb->gc_teb_leaf_ap);
  mps_ap_destroy(gc_teb->gc_teb_weak_awl_ap);
  mps_ap_destroy(gc_teb->gc_teb_exact_awl_ap);
  return MPS_RES_OK;
}

extern void *dylan_signal_low_memory;
extern void *dylan_false;
void *wrapper_class(void *wrapper);

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
  reserve_memory_for_object(size, wrapper, gc_teb, gc_teb_main_ap, main_handler, "MMReserveObject");
}

__inline
int MMCommitObject(void *p, size_t size, gc_teb_t gc_teb)
{
  assert(gc_teb->gc_teb_inside_tramp);
  assert(dylan_check(p));
  return mps_commit(gc_teb->gc_teb_main_ap, p, size);
}


__inline
void *MMReserveLeaf(size_t size, void *wrapper, gc_teb_t gc_teb)
{
  reserve_memory_for_object(size, wrapper, gc_teb, gc_teb_leaf_ap, leaf_handler, "MMReserveLeaf");
}

__inline
int MMCommitLeaf(void *p, size_t size, gc_teb_t gc_teb)
{
  assert(gc_teb->gc_teb_inside_tramp);
  assert(dylan_check(p));
  return mps_commit(gc_teb->gc_teb_leaf_ap, p, size);
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
  reserve_memory_for_object(size, wrapper, gc_teb, gc_teb_exact_awl_ap, exact_awl_handler, "MMReserveExactAWL");
}

__inline
int MMCommitExactAWL(void *p, size_t size, gc_teb_t gc_teb)
{
  assert(gc_teb->gc_teb_inside_tramp);
  assert(dylan_check(p));
  return mps_commit(gc_teb->gc_teb_exact_awl_ap, p, size);
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
  reserve_memory_for_object(size, wrapper, gc_teb, gc_teb_weak_awl_ap, weak_awl_handler, "MMReserveWeakAWL");
}

__inline
int MMCommitWeakAWL(void *p, size_t size, gc_teb_t gc_teb)
{
  assert(gc_teb->gc_teb_inside_tramp);
  assert(dylan_check(p));
  return mps_commit(gc_teb->gc_teb_weak_awl_ap, p, size);
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
  mps_res_t res;
  mps_addr_t p;

  unused(wrapper);

  assert(gc_teb->gc_teb_inside_tramp);

  res = mps_alloc(&p, wrapper_pool, size);
  if (res) {
    (*wrapper_handler)((MMError)res, "MMReserveWrapper", size);
    return (void *)NULL;
  }

  return (void *)p;
}

/* We declare each wrapper as a root on commit.  As a flip may
 * happen between reserve and commit, the wrapper may be initialized
 * to contain any moveable references.
 */
__inline
int MMCommitWrapper(void *p, size_t size, gc_teb_t gc_teb)
{
  mps_res_t res;
  mps_root_t root;

  assert(gc_teb->gc_teb_inside_tramp);
  assert(dylan_check(p));
  // there used to be a call to dylan_wrapper_check(p) here, but
  // the wrapper isn't properly initialized until after allocation.
  // So the check will always fail.

  res = mps_root_create_fmt(&root, arena, mps_rank_exact(),
                             (mps_rm_t)0, fmt_A->scan, p, (char *)p + size);
  if (res) return 0;
  return 1;
}

MMAllocHandler MMReserveWrapperHandler(MMAllocHandler handler)
{
  MMAllocHandler h = wrapper_handler;
  wrapper_handler = handler;
  return h;
}

void *MMAllocMisc(size_t size)
{
  mps_res_t res;
  void *p;
  /* gc_teb_t gc_teb = current_gc_teb(); */

  /*  assert(gc_teb->gc_teb_inside_tramp);   not a necessary condition for misc mem  */

  res = mps_alloc((mps_addr_t *)&p, misc_pool, size);
  if (res) {
    (*misc_handler)((MMError)res, "MMAllocMisc", size);
    return NULL;
  }

  return p;
}

MMAllocHandler MMAllocMiscHandler(MMAllocHandler handler)
{
  MMAllocHandler h = misc_handler;
  misc_handler = handler;
  return h;
}

void MMFreeMisc(void *old, size_t size)
{
  mps_free(misc_pool, (mps_addr_t)old, size);
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
  size_t mrep_size = (rep_size);  \
  size_t mrep_size_slot = (rep_size_slot);  \
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
    if (mrq) {  \
      if (mrep_size_slot) {  \
        object[mrep_size_slot] = (void*)((mrep_size << 2) + 1);  \
      } \
    } \
    if (mrfq) fill_ ## type ## _mem((type *)(object + mrep_size_slot + 1), mword_fill, mrep_size);  \
  }  \
  while (!commit(object, msize, gc_teb));  \
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
                        size_t rep_size,
                        size_t rep_size_slot)
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
                         size_t rep_size,
                         size_t rep_size_slot,
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
                          size_t rep_size,
                          size_t rep_size_slot)
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
                                   size_t rep_size, \
                                   size_t rep_size_slot, \
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
                            size_t rep_size,
                            size_t rep_size_slot,
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
                             size_t rep_size,
                             size_t rep_size_slot,
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
                           size_t rep_size,
                           size_t rep_size_slot,
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
                           size_t rep_size,
                           size_t rep_size_slot)
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
                               size_t rep_size,
                               size_t rep_size_slot)
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
                             size_t rep_size,
                             size_t rep_size_slot)
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
                                 size_t rep_size,
                                 size_t rep_size_slot,
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
                                      size_t rep_size, \
                                      size_t rep_size_slot, \
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
                                  size_t rep_size,
                                  size_t rep_size_slot,
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
                                size_t rep_size,
                                size_t rep_size_slot,
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
                                size_t rep_size,
                                size_t rep_size_slot)
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
                                    size_t rep_size,
                                    size_t rep_size_slot)
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
                                   size_t rep_size,
                                   size_t rep_size_slot)
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
                                   size_t rep_size,
                                   size_t rep_size_slot,
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
                                  size_t rep_size,
                                  size_t rep_size_slot,
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
                                  size_t rep_size,
                                  size_t rep_size_slot)
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
                         size_t rep_size,
                         size_t rep_size_slot,
                         void *template)
{
  void **object;

  gc_teb_t gc_teb = current_gc_teb();

  update_allocation_counter(gc_teb, size, wrapper);

  do {
    object = MMReserveObject(size, wrapper, gc_teb);
    object[0] = wrapper;
    object[rep_size_slot] = (void*)((rep_size << 2) + 1);
    memcpy(object + rep_size_slot + 1, template, rep_size << 2);
  }
  while (!MMCommitObject(object, size, gc_teb));

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
    object = MMReserveObject(size, wrapper, gc_teb);
    memcpy(object, template, size);
  }
  while (!MMCommitObject(object, size, gc_teb));

  return object;
}


/* Copy all but the repeated slots of a template */

RUN_TIME_API
void *primitive_copy_r(size_t size,
                       size_t rep_size,
                       size_t rep_size_slot,
                       void *template)
{
  void **object;
  void *wrapper = ((void**)template)[0];

  gc_teb_t gc_teb = current_gc_teb();

  update_allocation_counter(gc_teb, size, wrapper);

  do {
    object = MMReserveObject(size, wrapper, gc_teb);
    memcpy(object, template, rep_size_slot << 2);
    object[rep_size_slot] = (void*)((rep_size << 2) + 1);
    /* ### kludge to prevent committing uninitialized memory */
    fill_dylan_object_mem((void **)(object + rep_size_slot + 1),
                          NULL, rep_size);
  }
  while (!MMCommitObject(object, size, gc_teb));


  return object;
}

/* This is unused for now, but the runtime.o still depends on it. */
unsigned MMCollectCount(void)
{
  gc_teb_t gc_teb = current_gc_teb();

  assert(gc_teb->gc_teb_inside_tramp);

  return (unsigned)mps_collections(arena);
}

MMError MMRegisterRootStatic(mps_root_t *rootp, void *base, void *limit)
{
  /* assert(gc_teb->gc_teb_inside_tramp); tramp not needed for root registration */
  return mps_root_create_fmt(rootp, arena, mps_rank_exact(),
                             MPS_RM_PROT, fmt_A->scan, base, limit);
}

MMError MMRegisterRootImmut(mps_root_t *rootp, void *base, void *limit)
{
  /* assert(gc_teb->gc_teb_inside_tramp); tramp not needed for root registration */
  return mps_root_create_fmt(rootp, arena, mps_rank_exact(),
                             MPS_RM_CONST, fmt_A->scan, base, limit);
}


/* Don't protect ambiguous roots. That's because they're used */
/* for managing low-level runtime data including the TEBs.    */
/* In particular, they might be referenced by the Dylan trap  */
/* handler which must not be allowed to recursively trap      */

MMError MMRegisterRootAmbig(mps_root_t *rootp, void *base, void *limit)
{
  size_t s = ((char *)limit - (char *)base) / sizeof(mps_addr_t);
  /* assert(gc_teb->gc_teb_inside_tramp); tramp not needed for root registration */
  return mps_root_create_table(rootp, arena, mps_rank_ambig(),
                               0, base, s);
}

MMError MMRegisterRootExact(mps_root_t *rootp, void *base, void *limit)
{
  size_t s = ((char *)limit - (char *)base) / sizeof(mps_addr_t);
  /* assert(gc_teb->gc_teb_inside_tramp); tramp not needed for root registration */
  return mps_root_create_table_masked(rootp, arena, mps_rank_exact(),
                                      MPS_RM_PROT, base, s, 3);
}

void MMDeregisterRoot(mps_root_t root)
{
  if (root) {
    mps_root_destroy(root);
  }
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


/* Root registration support for the interactive downloader       */
/* This doesn't need to remember the root                         */
/* It must not use MPS_RM_PROT (see the cottonwood release notes) */

MMError MMRootStatic(void *base, void *limit)
{
  mps_root_t root;
  return mps_root_create_fmt(&root, arena, mps_rank_exact(),
                             0, fmt_A->scan, base, limit);
}

MMError MMRootImmut(void *base, void *limit)
{
  mps_root_t root;
  return  MMRegisterRootImmut(&root, base, limit);
}

MMError MMRootAmbig(void *base, void *limit)
{
  mps_root_t root;
  size_t s = ((char *)limit - (char *)base) / sizeof(mps_addr_t);
  return mps_root_create_table(&root, arena, mps_rank_ambig(),
                               0, base, s);
}

MMError MMRootExact(void *base, void *limit)
{
  mps_root_t root;
  size_t s = ((char *)limit - (char *)base) / sizeof(mps_addr_t);
  return mps_root_create_table_masked(&root, arena, mps_rank_exact(),
                                      0, base, s, 3);
}


/* Support for MM control */

RUN_TIME_API
void primitive_mps_clamp(void)
{
  mps_arena_clamp(arena);
}

RUN_TIME_API
void primitive_mps_park(void)
{
  mps_arena_park(arena);
}

RUN_TIME_API
void primitive_mps_release(void)
{
  mps_arena_release(arena);
}

extern void display_stats_for_memory_usage (void);

RUN_TIME_API
void primitive_mps_collect(BOOL display_stats)
{
  mps_arena_collect(arena);
  if (display_stats)
    display_stats_for_memory_usage();
}

RUN_TIME_API
size_t primitive_mps_committed(void)
{
  return mps_arena_committed(arena);
}

RUN_TIME_API
void primitive_mps_begin_ramp_alloc(void)
{
  gc_teb_t gc_teb = current_gc_teb();
  mps_alloc_pattern_t pattern = mps_alloc_pattern_ramp();

  mps_ap_alloc_pattern_begin(gc_teb->gc_teb_main_ap, pattern);
  mps_ap_alloc_pattern_begin(gc_teb->gc_teb_leaf_ap, pattern);
  mps_ap_alloc_pattern_begin(gc_teb->gc_teb_weak_awl_ap, pattern);
  mps_ap_alloc_pattern_begin(gc_teb->gc_teb_exact_awl_ap, pattern);
}

RUN_TIME_API
void primitive_mps_end_ramp_alloc(void)
{
  gc_teb_t gc_teb = current_gc_teb();
  mps_alloc_pattern_t pattern = mps_alloc_pattern_ramp();

  mps_ap_alloc_pattern_end(gc_teb->gc_teb_main_ap, pattern);
  mps_ap_alloc_pattern_end(gc_teb->gc_teb_leaf_ap, pattern);
  mps_ap_alloc_pattern_end(gc_teb->gc_teb_weak_awl_ap, pattern);
  mps_ap_alloc_pattern_end(gc_teb->gc_teb_exact_awl_ap, pattern);
}

RUN_TIME_API
void primitive_mps_begin_ramp_alloc_all(void)
{
  gc_teb_t gc_teb = current_gc_teb();
  mps_alloc_pattern_t pattern = mps_alloc_pattern_ramp_collect_all();

  mps_ap_alloc_pattern_begin(gc_teb->gc_teb_main_ap, pattern);
  mps_ap_alloc_pattern_begin(gc_teb->gc_teb_leaf_ap, pattern);
  mps_ap_alloc_pattern_begin(gc_teb->gc_teb_weak_awl_ap, pattern);
  mps_ap_alloc_pattern_begin(gc_teb->gc_teb_exact_awl_ap, pattern);
}

RUN_TIME_API
void primitive_mps_end_ramp_alloc_all(void)
{
  gc_teb_t gc_teb = current_gc_teb();
  mps_alloc_pattern_t pattern = mps_alloc_pattern_ramp_collect_all();

  mps_ap_alloc_pattern_end(gc_teb->gc_teb_main_ap, pattern);
  mps_ap_alloc_pattern_end(gc_teb->gc_teb_leaf_ap, pattern);
  mps_ap_alloc_pattern_end(gc_teb->gc_teb_weak_awl_ap, pattern);
  mps_ap_alloc_pattern_end(gc_teb->gc_teb_exact_awl_ap, pattern);
}


mps_message_t message;

RUN_TIME_API
void primitive_mps_enable_gc_messages(void)
{
  mps_message_type_enable(arena, mps_message_type_gc());
}


RUN_TIME_API
BOOL primitive_mps_collection_stats(void** results)
{
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
  } else {
    return FALSE;
  }
}


/* Support for Finalization */

void primitive_mps_finalize(void *obj)
{
  mps_finalize(arena, &obj);
}

void* primitive_mps_finalization_queue_first(void)
{
  mps_message_t finalization_message;
  if (mps_message_get(&finalization_message, arena, finalization_type)) {
    mps_addr_t object_ref;
    mps_message_finalization_ref(&object_ref, arena, finalization_message);
    mps_message_discard(arena, finalization_message);
    return object_ref;
  } else {
    return 0;
  }
}

/* Support for Location Dependencies */

typedef struct d_hs_s      *d_hs_t;     /* Dylan Hash State */

typedef struct d_hs_s         /* Dylan Hash State object */
{
  void *dylan_wrapper;
  mps_ld_s internal_state;
} d_hs_s;

void primitive_mps_ld_reset(void *d_hs)
{
  mps_ld_t mps_ld = &(((d_hs_t)d_hs)->internal_state);
  gc_teb_t gc_teb = current_gc_teb();
  assert(gc_teb->gc_teb_inside_tramp);
  mps_ld_reset(mps_ld, arena);
}

void primitive_mps_ld_add(void *d_hs, void *addr)
{
  mps_ld_t mps_ld = &(((d_hs_t)d_hs)->internal_state);
  gc_teb_t gc_teb = current_gc_teb();
  assert(gc_teb->gc_teb_inside_tramp);
  mps_ld_add(mps_ld, arena, (mps_addr_t)addr);
}

int primitive_mps_ld_isstale(void *d_hs)
{
  mps_ld_t mps_ld = &(((d_hs_t)d_hs)->internal_state);
  gc_teb_t gc_teb = current_gc_teb();
  assert(gc_teb->gc_teb_inside_tramp);
  return(mps_ld_isstale(mps_ld, arena, 0));
}


void primitive_mps_ld_merge(void *d_into, void *d_obj)
{
  mps_ld_t into = &(((d_hs_t)d_into)->internal_state);
  mps_ld_t addr = &(((d_hs_t)d_obj)->internal_state);
  gc_teb_t gc_teb = current_gc_teb();
  assert(gc_teb->gc_teb_inside_tramp);
  mps_ld_merge(into, arena, addr);
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

  while (*spec != '\0') {
    char *end;
    unsigned long capacity = strtoul(spec, &end, 0);
    double mortality;

    if (capacity == 0 || capacity > 2048 * 1024 || *end != ',') {
      free(params);
      return NULL;
    }
    max_heap_size += capacity * 1024;

    spec = end + 1;
    mortality = strtod(spec, &end);
    if (mortality < 0.0 || mortality > 1.0) {
      free(params);
      return NULL;
    }

    if (*end == ';') {
      spec = end + 1;
    } else if (*end == '\0') {
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

MMError dylan_init_memory_manager(void)
{
  mps_res_t res;
  size_t max_heap_size = MAXIMUM_HEAP_SIZE;

  gc_teb_t gc_teb = current_gc_teb();

  if (Prunning_under_dylan_debuggerQ == FALSE)
    set_CONSOLE_CTRL_HANDLER(&DylanBreakControlHandler, TRUE);

  assert(!gc_teb->gc_teb_inside_tramp);
  assert(TARG_CHECK);

  {
    size_t gen_count = genCOUNT;
    mps_gen_param_s *params = NULL;

#ifdef _WIN32
    char specbuf[2048];
    const char *spec = NULL;
    if (GetEnvironmentVariableA("OPEN_DYLAN_MPS_HEAP", specbuf,
                               sizeof specbuf) != 0) {
      spec = specbuf;
    }
#else
    const char *spec = getenv("OPEN_DYLAN_MPS_HEAP");
#endif

    res = mps_arena_create(&arena, mps_arena_class_vm(), max_heap_size);
    if (res) { init_error("create arena"); return(res); }

    if (spec) {
      params = get_gen_params(spec, &gen_count, &max_heap_size);
      if (!params)
        init_error("parse OPEN_DYLAN_MPS_HEAP format");
    }

    if (params) {
      res = mps_chain_create(&chain, arena, gen_count, params);
      free(params);
    } else {
      res = mps_chain_create(&chain, arena, genCOUNT, gc_default_gen_param);
    }
    if (res) { init_error("create chain"); return(res); }
  }

  fmt_A = dylan_fmt_A();
  res = mps_fmt_create_A(&format, arena, fmt_A);
  if (res) { init_error("create format"); return(res); }

  fmt_A_weak = dylan_fmt_A_weak();
  res = mps_fmt_create_A(&dylan_fmt_weak_s, arena, fmt_A_weak);
  if (res) { init_error("create weak format"); return(res); }

  res = mps_pool_create(&main_pool, arena, mps_class_amc(), format, chain);
  if (res) { init_error("create main pool"); return(res); }

  /* Create the Leaf Object pool */
  res = mps_pool_create(&leaf_pool, arena, mps_class_amcz(), format, chain);
  if (res) { init_error("create leaf pool"); return(res); }

  /* Create the Automatic Weak Linked pool */
  res = mps_pool_create(&weak_table_pool, arena, mps_class_awl(),
                        dylan_fmt_weak_s, dylan_weak_dependent);
  if (res) { init_error("create weak pool"); return(res); }

  /* Create the MV pool for miscellaneous objects. */
  /* This is also used for wrappers. */
  res = mps_pool_create(&misc_pool, arena, mps_class_mv(),
                        MISCEXTENDBY, MISCAVGSIZE, MISCMAXSIZE);
  if (res) { init_error("create misc pool"); return(res); }

  wrapper_pool = misc_pool;

  finalization_type = mps_message_type_finalization();
  mps_message_type_enable(arena, finalization_type);

  initialize_CRITICAL_SECTION(&reservoir_limit_set_lock);
  initialize_CRITICAL_SECTION(&polling_threads_lock);

  if (Prunning_under_dylan_debuggerQ) {
    initialize_CRITICAL_SECTION(&class_breakpoint_lock);
    class_breakpoint_events[0] = create_EVENT(NULL, FALSE, FALSE, NULL);
    class_breakpoint_events[1] = create_EVENT(NULL, FALSE, FALSE, NULL);
  }

  return(0);
}



void dylan_shut_down_memory_manager(void)
{
  while (primitive_mps_finalization_queue_first());

  mps_pool_destroy(misc_pool);
  mps_pool_destroy(weak_table_pool);
  mps_pool_destroy(leaf_pool);
  mps_pool_destroy(main_pool);
  mps_fmt_destroy(dylan_fmt_weak_s);
  mps_fmt_destroy(format);
  mps_chain_destroy(chain);
  mps_arena_destroy(arena);
}
