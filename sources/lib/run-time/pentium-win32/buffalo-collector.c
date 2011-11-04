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

#include "mm.h"        /* Dylan Interface */
#include "mps.h"        /* MPS Interface */
#include "mpscmv.h"     /* MPS pool class MV */
#include "mpscamc.h"    /* MPS pool class AMC */
/*
#include "mpscawl.h"
*/
#include "fmtdy.h"      /* Dylan object format */
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <windows.h>


/* Revert the definitions of anything to do with weakness */

#define MPS_RANK_WEAK MPS_RANK_EXACT
#define dylan_fmt_A_weak dylan_fmt_A
#define mps_class_awl mps_class_amc

/* Plus an extra extern */

extern mps_res_t mps_root_create_table_masked(mps_root_t *, mps_space_t,
                                              mps_rank_t, mps_rm_t,
                                              mps_addr_t *, size_t,
                                              mps_word_t);




/* Configuration
 *
 * MISC* configure the MV pool.
 */

#define MISCEXTENDBY    ((size_t)16384)
#define MISCAVGSIZE     ((size_t)32)
#define MISCMAXSIZE     ((size_t)65536)


typedef mps_word_t word;


#define TARG_CHECK (MPS_RES_OK == MMSUCCESS && \
                    MPS_RES_FAIL == MMFAILURE && \
                    MPS_RES_RESOURCE == MMRESOURCE && \
                    MPS_RES_MEMORY == MMRESMEM && \
                    MPS_RES_LIMIT == MMLIMIT && \
                    MPS_RES_UNIMPL == MMUNIMPLEMENTED && \
                    MPS_RES_IO == MMIO)


/* Default Error Handler
 *
 * This is the default error handler initially installed for all the
 * allocation interfaces in mm.h.  It prints a message on the standard
 * error stream then causes abnormal program termination.
 */

#ifdef MPS_OS_SU
extern int fprintf(FILE *, const char *, ...);
#endif

static void defaultHandler(MMError e, const char *opName, size_t size)
{
  fprintf(stderr,
          "**** %s:%d: request for %lu bytes failed -- aborting\n",
          opName, (int)e, (unsigned long)size);
  abort();
}

static mps_space_t space;
static mps_fmt_t format;
static mps_fmt_t dylan_fmt_weak;
static mps_fmt_A_t fmt_A;
static mps_fmt_A_t fmt_A_weak;
static mps_pool_t main_pool, weak_table_pool, wrapper_pool, misc_pool;

static MMAllocHandler main_handler = defaultHandler;
static MMAllocHandler weak_awl_handler = defaultHandler;
static MMAllocHandler exact_awl_handler = defaultHandler;
static MMAllocHandler wrapper_handler = defaultHandler;
static MMAllocHandler misc_handler = defaultHandler;


/* Thread Local Variables, accessed via the GC-TEB*/

typedef struct gc_teb_s     *gc_teb_t;     /* GC Thread Environment block */

typedef struct gc_teb_s {       /* GC Thread Environment block descriptor */
  mps_bool_t gc_teb_inside_tramp;
  mps_ap_t   gc_teb_main_ap;
  mps_ap_t   gc_teb_weak_awl_ap;
  mps_ap_t   gc_teb_exact_awl_ap;
  mps_thr_t  gc_teb_thread;
  mps_root_t gc_teb_stack_root;
} gc_teb_s;

__inline 
 gc_teb_t current_gc_teb()
{ 
  gc_teb_t gc_teb;
  __asm
    {
      mov eax, dword ptr fs:[0x14] /* the TEB */
      mov gc_teb, eax
    };
  gc_teb--; /* the GC-TEB is BEFORE the TEB */
  return(gc_teb);
};


#define inside_tramp  (*current_gc_teb()).gc_teb_inside_tramp
#define main_ap       (*current_gc_teb()).gc_teb_main_ap
#define weak_awl_ap   (*current_gc_teb()).gc_teb_weak_awl_ap
#define exact_awl_ap  (*current_gc_teb()).gc_teb_exact_awl_ap
#define thread        (*current_gc_teb()).gc_teb_thread
#define stack_root    (*current_gc_teb()).gc_teb_stack_root



/* Support for handling exceptions in Dylan (other than MM traps) */
/* Currently, we just handle stack overflows & numeric overflows  */

extern int inside_dylan_ffi_barrier();
extern void dylan_stack_overflow_handler(PVOID base_address, int size, DWORD protection);
extern void dylan_integer_overflow_handler();
extern void dylan_integer_divide_0_handler();
extern void dylan_float_divide_0_handler();
extern void dylan_float_overflow_handler();
extern void dylan_float_underflow_handler();

/* Support for foreign call-ins */
extern void *dylan_callin_internal(void *arg_base, size_t s);




PVOID current_stack_pointer ()
{
  PVOID stack_ptr;
  __asm
  {  
    mov  stack_ptr, esp
  };
  return(stack_ptr);
};


#define VPAGESIZE 0x1000


void call_dylan_stack_overflow_handler ()
{
  MEMORY_BASIC_INFORMATION memBuf;
  PVOID stack_ptr = current_stack_pointer();
  int res = VirtualQuery(stack_ptr, &memBuf, sizeof(memBuf));

  PVOID baseAddress    = memBuf.BaseAddress;     // base address of region 
  PVOID allocationBase = memBuf.AllocationBase;  // allocation base address 
  DWORD protect        = memBuf.Protect;         // current access protection 
  
  dylan_stack_overflow_handler(baseAddress, VPAGESIZE, PAGE_GUARD + protect);

}


LONG DylanExceptionFilter (LPEXCEPTION_POINTERS info)
{

  LPEXCEPTION_RECORD er = info->ExceptionRecord;

  if (inside_dylan_ffi_barrier() == 0)
  {  return(EXCEPTION_CONTINUE_SEARCH);
  }

  switch (er->ExceptionCode) 
  {
  case EXCEPTION_STACK_OVERFLOW:
    {
      // On a stack overflow, the filter calls into Dylan to signal
      // an error, via dylan_signal_overflow_handler. The dylan
      // code will arrange to re-establish the guard protection on
      // the appropriate page of the stack (probably during the
      // rewind when recovering from the error). Before calling the 
      // handler, we do a check to ensure that there is sufficient 
      // spare stack space after the guard to allow the handler itself 
      // to run.

      MEMORY_BASIC_INFORMATION memBuf;
      PVOID stack_ptr = current_stack_pointer();
      int res = VirtualQuery(stack_ptr, &memBuf, sizeof(memBuf));

      PVOID baseAddress    = memBuf.BaseAddress;    // base address of region 
      PVOID allocationBase = memBuf.AllocationBase; // allocation base addr
  
      if ( ((int)baseAddress - (int)allocationBase) >= (2 * VPAGESIZE))
      {
        // There's enough space past the guard to invoke the Dylan handler.
        // Rather than attempt a long-jump within the filter (by simply
        // calling the Dylan handler) we destructively modify the execution
        // context, so that when Windows continues from the exception, it
        // actually continues in the Dylan handler calling code instead.
        // This handler will never return - instead it will ultimatly NLX

        info->ContextRecord->Eip = (unsigned long) &call_dylan_stack_overflow_handler;
        return(EXCEPTION_CONTINUE_EXECUTION);
      }
      else
        return(EXCEPTION_CONTINUE_SEARCH);
    }
  case EXCEPTION_INT_OVERFLOW:
    { info->ContextRecord->Eip = (unsigned long) &dylan_integer_overflow_handler;
      return(EXCEPTION_CONTINUE_EXECUTION);
    }
  case EXCEPTION_INT_DIVIDE_BY_ZERO:
    { info->ContextRecord->Eip = (unsigned long) &dylan_integer_divide_0_handler;
      return(EXCEPTION_CONTINUE_EXECUTION);
    }
  case EXCEPTION_FLT_DIVIDE_BY_ZERO:
    { info->ContextRecord->Eip = (unsigned long) &dylan_float_divide_0_handler;
      return(EXCEPTION_CONTINUE_EXECUTION);
    }
  case EXCEPTION_FLT_OVERFLOW:
    { info->ContextRecord->Eip = (unsigned long) &dylan_float_overflow_handler;
      return(EXCEPTION_CONTINUE_EXECUTION);
    }
  case EXCEPTION_FLT_UNDERFLOW:
    { info->ContextRecord->Eip = (unsigned long) &dylan_float_underflow_handler;
      return(EXCEPTION_CONTINUE_EXECUTION);
    }
  
  default:
    return(EXCEPTION_CONTINUE_SEARCH);
  }  
}



MMError dylan_mm_register_thread(void *stackBot)
{
  mps_res_t res;

  gc_teb_t gc_teb = current_gc_teb();

  res = mps_ap_create(&gc_teb->gc_teb_main_ap, main_pool, MPS_RANK_EXACT);
  if(res) goto failApCreate;

  res = mps_ap_create(&gc_teb->gc_teb_weak_awl_ap, weak_table_pool, MPS_RANK_WEAK);
  if(res) goto failWeakAWLApCreate;

  res = mps_ap_create(&gc_teb->gc_teb_exact_awl_ap, weak_table_pool, MPS_RANK_EXACT);
  if(res) goto failExactAWLApCreate;

  res = mps_thread_reg(&gc_teb->gc_teb_thread, space);
  if(res) goto failThreadReg;

  /* Create a root object for ambiguously scanning the stack. */
  assert(stackBot != NULL);
  res = mps_root_create_reg(&gc_teb->gc_teb_stack_root, space, MPS_RANK_AMBIG, 
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
  mps_ap_destroy(gc_teb->gc_teb_main_ap);
failApCreate:
  return res;

}


MMError dylan_mm_deregister_thread()
{
  gc_teb_t gc_teb = current_gc_teb();

  mps_root_destroy(gc_teb->gc_teb_stack_root);
  mps_thread_dereg(gc_teb->gc_teb_thread);
  mps_ap_destroy(gc_teb->gc_teb_main_ap);
  mps_ap_destroy(gc_teb->gc_teb_weak_awl_ap);
  mps_ap_destroy(gc_teb->gc_teb_exact_awl_ap);
  return MPS_RES_OK;
}


MMError dylan_init_thread(void **rReturn, void *(*f)(void *, size_t), void *p, size_t s)
{
  __try {   // establish the stack overflow filter outside the MPS handler
            // because it has less requirement for efficiency

    gc_teb_t gc_teb = current_gc_teb();

    gc_teb->gc_teb_inside_tramp = 1;
  
    /* Go for it! */
    mps_tramp(rReturn, f, p, s);

    gc_teb->gc_teb_inside_tramp = 0;

    } 

  __except (DylanExceptionFilter(GetExceptionInformation())) {
    }

  return MPS_RES_OK;

}

void *dylan_callin_handler(void *arg_base, size_t s)
{
  void *res;

  __try {   // establish the stack overflow filter outside the MPS handler
            // because it has less requirement for efficiency

    gc_teb_t gc_teb = current_gc_teb();

    mps_bool_t was_inside = gc_teb->gc_teb_inside_tramp;
    gc_teb->gc_teb_inside_tramp = 1;
  
    /* Go for it! */
    mps_tramp(&res, dylan_callin_internal, arg_base, s);

    gc_teb->gc_teb_inside_tramp = was_inside;

    } 

  __except (DylanExceptionFilter(GetExceptionInformation())) {
    }

  return res;

}


MMError dylan_init_memory_manager()
{
  mps_res_t res;

  gc_teb_t gc_teb = current_gc_teb();

  assert(!gc_teb->gc_teb_inside_tramp);
  assert(TARG_CHECK);

  res = mps_space_create(&space);
  if(res) return(res);

  fmt_A = dylan_fmt_A();    
  res = mps_fmt_create_A(&format, space, fmt_A);
  if(res) return(res); 

  /*
  fmt_A_weak = dylan_fmt_A_weak();    
  res = mps_fmt_create_A(&dylan_fmt_weak, space, fmt_A_weak);
  if(res) return(res);
  */

  res = mps_pool_create(&main_pool, space, mps_class_amc(), format);
  if(res) return(res);

  /* Create the Automatic Weak Linked pool */
  /*
  res = mps_pool_create(&weak_table_pool, space, mps_class_awl(), dylan_fmt_weak);
  if(res) return(res);
  */
  weak_table_pool = main_pool;

  /* Create the MV pool for miscellaneous objects. */
  /* This is also used for wrappers. */
  res = mps_pool_create(&misc_pool, space, mps_class_mv(),
                        MISCEXTENDBY, MISCAVGSIZE, MISCMAXSIZE);
  if(res) return(res);

  wrapper_pool = misc_pool;

  return(0);
  
}



void *dylan__malloc(size_t size,
                    void *wrapper,
                    int no_to_fill,
                    void *fill,
                    void *rep_size,
                    int rep_size_slot)
{
  void **object;

  do {
    object = MMReserveObject(size);
    object[0] = wrapper;
    {
      int index = 0;
      while (index < no_to_fill)
	{
	  ++index;
	  object[index] = fill;
	};
    }
    if (rep_size_slot)
      object[rep_size_slot] = rep_size;
  }
  while(!MMCommitObject(object, size));

  return object;
}

void *dylan__malloc__byte__fill(size_t size,
				void *wrapper,
				int no_to_fill,
				void *fill,
				void *rep_size,
				int rep_size_slot)
{
  void **object;

  do {
    object = MMReserveObject(size);
    object[0] = wrapper;
    {
      int index = 0;
      while (index < no_to_fill)
	{
	  ++index;
	  object[index] = fill;
	};
    }
    if (rep_size_slot)
      object[rep_size_slot] = rep_size;
  }
  while(!MMCommitObject(object, size));

  {
    unsigned char *d = (unsigned char*)(&(object[rep_size_slot + 1]));
    int index = 0;
    int byte_fill_size = ((unsigned int)rep_size >> 2);
    unsigned char byte_fill = (unsigned char)((unsigned int)fill >> 2);
    while (index < byte_fill_size)
      {
	d[index] = byte_fill;
	index++;
      };
  }
  return object;
}



void *dylan__malloc__exact__awl(size_t size,
				void *wrapper,
				int no_to_fill,
				void *fill,
				void *rep_size,
				int rep_size_slot,
				void *assoc)
{
  void **object;

  do {
    object = MMReserveExactAWL(size);
    object[0] = wrapper;
    {
      int index = 0;
      while (index < no_to_fill)
	{
	  ++index;
	  object[index] = fill;
	};
    }
    object[1] = assoc;
    if (rep_size_slot)
      object[rep_size_slot] = rep_size;
  }
  while(!MMCommitExactAWL(object, size));

  return object;
}



void *dylan__malloc__weak__awl(size_t size,
			       void *wrapper,
			       int no_to_fill,
			       void *fill,
			       void *rep_size,
			       int rep_size_slot,
			       void *assoc)
{
  void **object;

  do {
    object = MMReserveWeakAWL(size);
    object[0] = wrapper;
    {
      int index = 0;
      while (index < no_to_fill)
	{
	  ++index;
	  object[index] = fill;
	};
    }
    object[1] = assoc;
    if (rep_size_slot)
      object[rep_size_slot] = rep_size;
  }
  while(!MMCommitWeakAWL(object, size));

  return object;
}


void *dylan__malloc__wrapper(size_t size,
			     void *wrapper,
			     int no_to_fill,
			     void *fill,
			     void *rep_size,
			     int rep_size_slot)
{
  void **object;

  do {
    object = MMReserveWrapper(size);
    object[0] = wrapper;
    {
      int index = 0;
      while (index < no_to_fill)
	{
	  ++index;
	  object[index] = fill;
	};
    }
    if (rep_size_slot)
      object[rep_size_slot] = rep_size;
  }
  while(!MMCommitWrapper(object, size));

  return object;
}

void *dylan__malloc__misc(size_t size)
{
  return MMAllocMisc(size);
}


void dylan__finish__malloc(void)
{
}
  

void *MMReserveObject(size_t size)
{
  mps_res_t res;
  mps_addr_t p;

  gc_teb_t gc_teb = current_gc_teb();

  assert(gc_teb->gc_teb_inside_tramp);

  res = mps_reserve(&p, gc_teb->gc_teb_main_ap, size);
  if(res) {
    (*main_handler)((MMError)res, "MMReserveObject", size);
    return (void *)NULL;
  }

  return (void *)p;
}

int MMCommitObject(void *p, size_t size)
{
  gc_teb_t gc_teb = current_gc_teb();

  assert(gc_teb->gc_teb_inside_tramp);
  assert(dylan_check(p));
  return mps_commit(gc_teb->gc_teb_main_ap, p, size);
}


void *MMReserveExactAWL(size_t size)
{
  mps_res_t res;
  mps_addr_t p;
  gc_teb_t gc_teb = current_gc_teb();

  assert(gc_teb->gc_teb_inside_tramp);

  res = mps_reserve(&p, gc_teb->gc_teb_exact_awl_ap, size);
  if(res) {
    (*main_handler)((MMError)res, "MMReserveExactAWL", size);
    return (void *)NULL;
  }

  return (void *)p;
}

int MMCommitExactAWL(void *p, size_t size)
{
  gc_teb_t gc_teb = current_gc_teb();

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

void *MMReserveWeakAWL(size_t size)
{
  mps_res_t res;
  mps_addr_t p;
  gc_teb_t gc_teb = current_gc_teb();

  assert(gc_teb->gc_teb_inside_tramp);

  res = mps_reserve(&p, gc_teb->gc_teb_weak_awl_ap, size);
  if(res) {
    (*main_handler)((MMError)res, "MMReserveWeakAWL", size);
    return (void *)NULL;
  }

  return (void *)p;
}

int MMCommitWeakAWL(void *p, size_t size)
{
  gc_teb_t gc_teb = current_gc_teb();

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

void *MMReserveWrapper(size_t size)
{
  mps_res_t res;
  mps_addr_t p;
  gc_teb_t gc_teb = current_gc_teb();

  assert(gc_teb->gc_teb_inside_tramp);

  res = mps_alloc(&p, wrapper_pool, size);
  if(res) {
    (*wrapper_handler)((MMError)res, "MMReserveWrapper", size);
    return (void *)NULL;
  }

  return (void *)p;
}

/* We declare each wrapper as a root on commit.  As a flip may
 * happen between reserve and commit, the wrapper may be initialized
 * to contain any moveable references.
 */
int MMCommitWrapper(void *p, size_t size)
{
  mps_res_t res;
  mps_root_t root;
  gc_teb_t gc_teb = current_gc_teb();

  assert(gc_teb->gc_teb_inside_tramp);
  assert(dylan_check(p));

  res = mps_root_create_fmt(&root, space, MPS_RANK_EXACT,
                             (mps_rm_t)0, fmt_A->scan, p, (char *)p + size);
  if(res) return 0;
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
  gc_teb_t gc_teb = current_gc_teb();

  /*  assert(gc_teb->gc_teb_inside_tramp);   not a necessary condition for misc mem  */

  res = mps_alloc((mps_addr_t *)&p, misc_pool, size);
  if(res) {
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
  gc_teb_t gc_teb = current_gc_teb();

  /* assert(gc_teb->gc_teb_inside_tramp); */
  mps_free(misc_pool, (mps_addr_t)old, size);
}

unsigned MMCollectCount(void)
{
  gc_teb_t gc_teb = current_gc_teb();

  assert(gc_teb->gc_teb_inside_tramp);
  return (unsigned)mps_collections(space);
}

MMError MMRootStatic(void *base, void *limit)
{
  mps_root_t root;
  /* assert(gc_teb->gc_teb_inside_tramp); tramp not needed for root registration */
  return mps_root_create_fmt(&root, space, MPS_RANK_EXACT,
                             MPS_RM_PROT, fmt_A->scan, base, limit);
}

MMError MMRootImmut(void *base, void *limit)
{
  mps_root_t root;
  /* assert(gc_teb->gc_teb_inside_tramp); tramp not needed for root registration */
  return mps_root_create_fmt(&root, space, MPS_RANK_EXACT,
                             MPS_RM_CONST, fmt_A->scan, base, limit);
}

MMError MMRootAmbig(void *base, void *limit)
{
  mps_root_t root;
  size_t s = ((char *)limit - (char *)base) / sizeof(mps_addr_t);
  /* assert(gc_teb->gc_teb_inside_tramp); tramp not needed for root registration */
  return mps_root_create_table(&root, space, MPS_RANK_AMBIG,
                               MPS_RM_PROT, base, s);
}

MMError MMRootExact(void *base, void *limit)
{
  mps_root_t root;
  size_t s = ((char *)limit - (char *)base) / sizeof(mps_addr_t);
  /* assert(gc_teb->gc_teb_inside_tramp); tramp not needed for root registration */
  return mps_root_create_table_masked(&root, space, MPS_RANK_EXACT,
				      MPS_RM_PROT, base, s, 3);
}



// Support for Location Dependencies

typedef struct d_hs_s      *d_hs_t;     /* Dylan Hash State */

typedef struct d_hs_s         /* Dylan Hash State object */
{
  void *dylan_wrapper;
  mps_ld_s internal_state;
} d_hs_s;



void primitive_mps_ld_reset(d_hs_t d_hs)
{
  mps_ld_t mps_ld = &(d_hs->internal_state);
  gc_teb_t gc_teb = current_gc_teb();
  assert(gc_teb->gc_teb_inside_tramp);
  mps_ld_reset(mps_ld, space);
}

void primitive_mps_ld_add(d_hs_t d_hs, mps_addr_t addr)
{
  mps_ld_t mps_ld = &(d_hs->internal_state);
  gc_teb_t gc_teb = current_gc_teb();
  assert(gc_teb->gc_teb_inside_tramp);
  mps_ld_add(mps_ld, space, addr);
}

mps_bool_t primitive_mps_ld_isstale(d_hs_t d_hs)
{
  mps_ld_t mps_ld = &(d_hs->internal_state);
  gc_teb_t gc_teb = current_gc_teb();
  assert(gc_teb->gc_teb_inside_tramp);
  return(mps_ld_isstale(mps_ld, space, 0));
}


// This should be in the MPS code - but currently isn't
void mps_ld_merge(mps_ld_t into, mps_space_t space, mps_ld_t addr)
{
  into->w0 = min(into->w0, addr->w0);
  into->w1 = (into->w1 | addr->w1);
}

void primitive_mps_ld_merge(d_hs_t d_into, d_hs_t d_obj)
{
  mps_ld_t into = &(d_into->internal_state);
  mps_ld_t addr = &(d_obj->internal_state);
  gc_teb_t gc_teb = current_gc_teb();
  assert(gc_teb->gc_teb_inside_tramp);
  mps_ld_merge(into, space, addr);
}

