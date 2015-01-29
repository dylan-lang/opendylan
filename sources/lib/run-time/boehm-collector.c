#include <gc/gc.h>

/* Temporary copy from run-time.h */
#ifdef __GNUC__
#  define OPEN_DYLAN_COMPILER_GCC_LIKE
#  if defined(__clang__)
#    define OPEN_DYLAN_COMPILER_CLANG
#  else
#    define OPEN_DYLAN_COMPILER_GCC
#  endif
#else
#  warning Unknown compiler
#endif

#ifdef OPEN_DYLAN_COMPILER_GCC_LIKE
#  define CONDITIONAL_UPDATE(var, new_val, old_val) \
     (__sync_bool_compare_and_swap(&var, old_val, new_val) ? TRUE : FALSE)
#else
#  warning missing primitive CONDITIONAL_UPDATE - thread safety compromised
#  define CONDITIONAL_UPDATE(var, new_val, old_val) \
     ((old_val) == (var) ? (var = (new_val), TRUE) : FALSE)
#endif

/* End temporary copy */

typedef size_t mps_word_t;
typedef int mps_bool_t;
typedef void* mps_root_t;
typedef void *mps_addr_t;       /* managed address (void *) */

typedef struct gc_teb_s {       /* GC Thread Environment block descriptor */
  mps_bool_t gc_teb_inside_tramp;  /* the HARP runtime assumes offset 0 for this */
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
#define MMAllocateLeafObject MMAllocateLeaf
#else
#define MMAllocateLeafObject MMAllocateObject
#endif

#ifdef USE_LEAF_FOR_REPEATED_OBJECTS
#define MMAllocateLeafRepeated MMAllocateLeaf
#else
#define MMAllocateLeafRepeated MMAllocateObject
#endif

#ifdef USE_LEAF_FOR_STRINGS
#define MMAllocateLeafTerminated MMAllocateLeaf
#else
#define MMAllocateLeafTerminated MMAllocateObject
#endif

void report_runtime_error (char* header, char* message)
{
  unused(header);
  unused(message);
}

/* Thread creation & deletion code */

static int num_threads = 0;

static define_CRITICAL_SECTION(reservoir_limit_set_lock);

static __inline
void update_runtime_thread_count(int increment)
{
  enter_CRITICAL_SECTION(&reservoir_limit_set_lock);
    num_threads = num_threads + increment;
  leave_CRITICAL_SECTION(&reservoir_limit_set_lock);
}


MMError dylan_mm_register_thread(void *stackBot)
{
  gc_teb_t gc_teb = current_gc_teb();

  update_runtime_thread_count(1);

  zero_allocation_counter(gc_teb);

  unused(stackBot);

  return 0;
}


MMError dylan_mm_deregister_thread_from_teb(gc_teb_t gc_teb)
{
  update_runtime_thread_count(-1);
  unused(gc_teb);
  return 0;
}

__inline
void *MMAllocateObject(size_t size, void *wrapper, gc_teb_t gc_teb)
{
  unused(wrapper);
  unused(gc_teb);

  return GC_MALLOC(size);
}

__inline
void *MMAllocateLeaf(size_t size, void *wrapper, gc_teb_t gc_teb)
{
  unused(wrapper);
  unused(gc_teb);

  return GC_MALLOC_ATOMIC(size);
}

__inline
void *MMAllocateExactAWL(size_t size, void *wrapper, gc_teb_t gc_teb)
{
  unused(wrapper);
  unused(gc_teb);

  return GC_MALLOC(size);
}

__inline
void *MMAllocateWeakAWL(size_t size, void *wrapper, gc_teb_t gc_teb)
{
  unused(wrapper);
  unused(gc_teb);

  return GC_MALLOC(size);
}

__inline
void *MMAllocateWrapper(size_t size, void *wrapper, gc_teb_t gc_teb)
{
  unused(wrapper);
  unused(gc_teb);

  return GC_MALLOC_ATOMIC(size);
}

RUN_TIME_API
void *MMAllocMisc(size_t size)
{
  return GC_MALLOC_UNCOLLECTABLE(size);
}

RUN_TIME_API
void MMFreeMisc(void *old, size_t size)
{
  unused(size);

  GC_FREE(old);
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
                       allocator)  \
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
  object = allocator(msize, wrapper, gc_teb);  \
  object[0] = mwrapper;  \
  if (msq) fill_dylan_object_mem(object + 1, mfill, mno_to_fill);  \
  if (ms1q) object[1] = ms1;  \
  if (ms2q) object[2] = ms2;  \
  if (mrq) { \
    if (mrep_size_slot) {  \
      object[mrep_size_slot] = (void*)((mrep_size << 2) + 1);  \
    } \
  } \
  if (mrfq) fill_ ## type ## _mem((type *)(object + mrep_size_slot + 1), mword_fill, mrep_size);  \
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
                 MMAllocateObject);
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
                 MMAllocateObject);
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
                 MMAllocateObject);
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
                 MMAllocateObject);
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
                 MMAllocateObject);
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
                 MMAllocateObject);
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
                 MMAllocateObject);
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
                 MMAllocateObject); \
}

define_primitive_alloc_s_rf(dylan_object, rf)
define_primitive_alloc_s_rf(word, rwf)
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
                 MMAllocateObject);
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
                 MMAllocateObject);
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
                 MMAllocateObject);
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
                 MMAllocateObject);
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
                 MMAllocateLeafObject);
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
                 MMAllocateLeafRepeated);
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
                 MMAllocateLeafObject);
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
                 MMAllocateLeafObject);
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
                 MMAllocateLeafObject);
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
                 MMAllocateLeafRepeated);
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
                 MMAllocateLeafRepeated);
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
                 MMAllocateLeafRepeated); \
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
                 MMAllocateLeafTerminated);
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
                 MMAllocateLeafTerminated);
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
                 MMAllocateLeafRepeated);
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
                 MMAllocateExactAWL);
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
                 MMAllocateWeakAWL);
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
                 MMAllocateExactAWL);
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
                 MMAllocateWeakAWL);
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
                 MMAllocateWrapper);
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

  object = MMAllocateObject(size, wrapper, gc_teb);
  object[0] = wrapper;
  object[rep_size_slot] = (void*)((rep_size << 2) + 1);
  memcpy(object + rep_size_slot + 1, template, rep_size << 2);

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

  object = MMAllocateObject(size, wrapper, gc_teb);
  memcpy(object, template, size);

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

  object = MMAllocateObject(size, wrapper, gc_teb);
  memcpy(object, template, rep_size_slot << 2);
  object[rep_size_slot] = (void*)((rep_size << 2) + 1);
  /* ### kludge to prevent committing uninitialized memory */
  fill_dylan_object_mem((void **)(object + rep_size_slot + 1),
                        NULL, rep_size);


  return object;
}


MMError MMRegisterRootStatic(mps_root_t *rootp, void *base, void *limit)
{
  unused(rootp);
  unused(base);
  unused(limit);
  return 0;
}

MMError MMRegisterRootImmut(mps_root_t *rootp, void *base, void *limit)
{
  unused(rootp);
  unused(base);
  unused(limit);
  return 0;
}

MMError MMRegisterRootAmbig(mps_root_t *rootp, void *base, void *limit)
{
  unused(rootp);
  unused(base);
  unused(limit);
  return 0;
}

MMError MMRegisterRootExact(mps_root_t *rootp, void *base, void *limit)
{
  unused(rootp);
  unused(base);
  unused(limit);
  return 0;
}

void MMDeregisterRoot(mps_root_t root)
{
  unused(root);
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

MMError MMRootStatic(void *base, void *limit)
{
  unused(base);
  unused(limit);
  return 0;
}

MMError MMRootImmut(void *base, void *limit)
{
  unused(base);
  unused(limit);
  return 0;
}

MMError MMRootAmbig(void *base, void *limit)
{
  unused(base);
  unused(limit);
  return 0;
}

MMError MMRootExact(void *base, void *limit)
{
  unused(base);
  unused(limit);
  return 0;
}


/* Support for MM control */

RUN_TIME_API
void primitive_mps_clamp(void)
{
}

RUN_TIME_API
void primitive_mps_park(void)
{
}

RUN_TIME_API
void primitive_mps_release(void)
{
}

extern void display_stats_for_memory_usage (void);

RUN_TIME_API
void primitive_mps_collect(BOOL display_stats)
{
  unused(display_stats);
  GC_gcollect();
}

RUN_TIME_API
size_t primitive_mps_committed(void)
{
  return GC_get_heap_size();
}

RUN_TIME_API
void primitive_mps_begin_ramp_alloc(void)
{
}

RUN_TIME_API
void primitive_mps_end_ramp_alloc(void)
{
}

RUN_TIME_API
void primitive_mps_begin_ramp_alloc_all(void)
{
}

RUN_TIME_API
void primitive_mps_end_ramp_alloc_all(void)
{
}

RUN_TIME_API
void primitive_mps_enable_gc_messages(void)
{
}


RUN_TIME_API
BOOL primitive_mps_collection_stats(void** results)
{
  unused(results);
  return FALSE;
}

/* Support for Finalization */

static struct _mps_finalization_queue {
  void *first;
  struct _mps_finalization_queue *rest;
} *mps_finalization_queue = NULL;

static void mps_finalization_proc(void *obj, struct _mps_finalization_queue *cons) {
  cons->first = obj;
  do {
    cons->rest = mps_finalization_queue;
  } while (FALSE == CONDITIONAL_UPDATE(mps_finalization_queue, cons, cons->rest));
}

void primitive_mps_finalize(void *obj) {
  GC_register_finalizer(obj,
                        (GC_finalization_proc)mps_finalization_proc,
                        GC_MALLOC(sizeof(struct _mps_finalization_queue)),
                        NULL, NULL);
}

void* primitive_mps_finalization_queue_first(void)
{
  struct _mps_finalization_queue *queue;

 RETRY:
  if ((queue = mps_finalization_queue)) {
    if (FALSE == CONDITIONAL_UPDATE(mps_finalization_queue, queue->rest, queue)) {
      goto RETRY;
    }

    return queue->first;
  }

  return NULL;
}

/* Support for Location Dependencies */

void primitive_mps_ld_reset(void *d_hs)
{
  unused(d_hs);
}

void primitive_mps_ld_add(void *d_hs, void *addr)
{
  unused(d_hs);
  unused(addr);
}

int primitive_mps_ld_isstale(void *d_hs)
{
  unused(d_hs);

  return 0; /* Never stale */
}

void primitive_mps_ld_merge(void *d_into, void *d_obj)
{
  unused(d_into);
  unused(d_obj);
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
        if (Prunning_under_dylan_debuggerQ == FALSE) {
          dylan_keyboard_interruptQ = TRUE;
        }
        return TRUE;
      }

    default:
      return FALSE;
    }
}

MMError dylan_init_memory_manager(void)
{
  gc_teb_t gc_teb = current_gc_teb();

  if (Prunning_under_dylan_debuggerQ == FALSE) {
    set_CONSOLE_CTRL_HANDLER(&DylanBreakControlHandler, TRUE);
  }

  assert(!gc_teb->gc_teb_inside_tramp);

  /* Not required for the dll version of Boehm. */
  /* GC_init(); */

  const char *heap_size = getenv("OPEN_DYLAN_MAX_BOEHM_HEAP_SIZE");
  if (NULL != heap_size) {
    size_t max_heap_size = strtoul(heap_size, (char **)NULL, 10);
    if (max_heap_size > 0) {
      GC_set_max_heap_size(max_heap_size);
    }
  }

  /* Call this to enable incrementality. This doesn't work with the MM GC. */
  /* GC_enable_incremental(); */

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
}
