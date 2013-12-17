/* impl.h.mm: DYLAN INTERIM MEMORY MANAGER INTERFACE
 *
 * Copyright (C) 1994,1995,1996,1998 Functional Objects, Inc. All rights reserved
 *
 * This file defines a temporary interface between the Dylan run-time
 * system and the Memory Pool System (MPS).  It is
 * only here to make initial integration with Dylan easier
 * by removing some of the burden from the Dylan Group.  The
 * Dylan run-time system should migrate to the full MPS Interface
 * as soon as possible.
 *
 * The interface code internally defines a descriptor for the Dylan
 * object format.
 */

#ifndef mm_h
#define mm_h

#include <stddef.h>

#ifdef OPEN_DYLAN_PLATFORM_WINDOWS
#define RUN_TIME_API __declspec( dllexport )
#else
#define RUN_TIME_API
#endif

/* Error Codes */

typedef int MMError;
enum
{
  MMSUCCESS = 0,                /* general success */
  MMFAILURE,                    /* general failure */
  MMRESOURCE,                   /* unable to obtain a resource */
  MMRESMEM,                     /* unable to obtain memory resource */
  MMLIMIT,                      /* internal limit reached */
  MMUNIMPLEMENTED,              /* feature unimplemented */
  MMIO                          /* system I/O error */
};


/* Thread local data */
typedef struct gc_teb_s     *gc_teb_t;     /* GC Thread Environment block */

extern gc_teb_t current_gc_teb();


/* Allocation Error Handler Type
 *
 * Each allocation or reservation function defined below has a
 * corresponding error handler which is called if the allocation fails for
 * some reason.  The error handler is passed the error code (defined
 * above), a string containing the identifier of the operation which
 * failed (e.g. "MMReserveWrapper") and the size of the request which failed.
 *
 * If the error handler returns, NULL is returned from the
 * operation which failed.  Alternatively, the error handler may
 * perform a longjmp to abort the operation entirely.  The error handler
 * may _not_ allocate memory from the memory manager.
 */

typedef void (*MMAllocHandler)(MMError e,
                               const char *opName, size_t size);

/* Trampoline
 *
 * MMTramp must be used to dynamically enclose code which uses the rest
 * of this interface.  MMTramp is passed a function and some arguments
 * to apply it to.  That function and its descendents, may use the rest
 * of the MM interface.  The result of the function is stored in
 * *rReturn.
 */

extern MMError MMTramp(void **rReturn,
                       void *(*f)(void *, size_t), void *p, size_t s);


/* Allocate Space for a Dylan Object
 *
 * MMReserveObject allocates space on the main Dylan heap which is
 * managed by a garbage collection policy.  It returns a pointer
 * to an area of memory large enough to accommodate an object of
 * the size requested.  This memory must be initialized with an
 * object in the DylanWorks Container Format (design.dylan.container),
 * and then MMCommitObject must be called.  MMCommitObject is passed
 * the pointer previously returned by Reserve, and the size previously
 * passed to Reserve.  If Commit returns a non-zero (true) result then
 * the allocation succeeded and the new object may be used.  Otherwise
 * the allocation failed, the object is voided, and the reserved
 * memory must not be touched.  If you need the object, make another
 * attempt at allocation.
 *
 * Typically, the allocation sequence will be of this form:
 *
 *   do {
 *     object = MMReserveObject(size, teb);
 *     initialize(object, ...);
 *   } while (!MMCommitObject(object, size, teb));
 *
 * MMReserveObjectHandler installs an error handler for use by
 * MMReserveObject in case of error.  (See the description of error
 * handlers above.)  It returns the previously installed handler.
 * Initially, a default handler is installed which prints a message
 * on the standard error stream and calls abort().
 *
 * If you install an error handler which might return, then you must
 * test for a NULL return from MMReserveObject.  For example:
 *
 *   do {
 *     object = MMReserveObject(size, teb);
 *     if (object == NULL) break;
 *     initialize(object, ...);
 *   } while (!MMCommitObject(object, size, teb));
 *
 * Dylan object wrappers may _NOT_ be allocated using this method.
 * Use MMReserveWrapper and MMCommitWrapper instead.
 *
 * Similarly, weak tables may _NOT_ be allocated in this way.
 * MMReserveExactAWL, MMCommitExactAWL, MMReserveWeakAWL, MMCommitWeakAWL
 * should be used instead.
 *
 * Wrappers may not be initialised to contain references to allocated
 * objects.  It may however be updated to contain such a reference, but
 * only after the call to MMCommitWrapper.
 */

extern void *MMReserveObject(size_t size, void *wrapper, gc_teb_t teb);
extern int MMCommitObject(void *p, size_t size, gc_teb_t teb);
extern MMAllocHandler MMReserveObjectHandler(MMAllocHandler handler);



/* Allocate Space for an object in the Leaf Object
 *
 * MMReserveLeaf is like MMReserveObject except that it allocates space
 * in the untraced Leaf Object pool.  MMCommitLeaf is like MMCommitObject.
 *
 * MMReserveLeafHandler is similar to MMReserveObjectHandler except
 * that it installs a handler for use by MMReserveLeaf.
 */

extern void *MMReserveLeaf(size_t size, void *wrapper, gc_teb_t teb);
extern int MMCommitLeaf(void *p, size_t size, gc_teb_t teb);
extern MMAllocHandler MMReserveLeafHandler(MMAllocHandler handler);



/* Allocate Space for an exact object in the Auto Weak Linked pool
 *
 * MMReserveExactAWL is like MMReserveObject except that it allocates space
 * for a table vector in the AWL pool.  MMCommitExactAWL is like MMCommitObject.
 *
 * MMReserveExactAWLHandler is similar to MMReserveObjectHandler except
 * that it installs a handler for use by MMReserveEaxctAWL.
 */

extern void *MMReserveExactAWL(size_t size, void *wrapper, gc_teb_t teb);
extern int MMCommitExactAWL(void *p, size_t size, gc_teb_t teb);
extern MMAllocHandler MMReserveExactAWLHandler(MMAllocHandler handler);



/* Allocate Space for a weak object in the Auto Weak Linked pool
 *
 * MMReserveWeakAWL is like MMReserveExactAWL except that it allocates
 * an object with weak references. Etc.
 */

extern void *MMReserveWeakAWL(size_t size, void *wrapper, gc_teb_t teb);
extern int MMCommitWeakAWL(void *p, size_t size, gc_teb_t teb);
extern MMAllocHandler MMReserveWeakAWLHandler(MMAllocHandler handler);


/* Allocate Space for an Object Wrapper
 *
 * MMReserveWrapper is like MMReserveObject except that it allocates space
 * for a MM wrapper.  MMCommitWrapper is like MMCommitObject.  Wrappers may
 * be managed by a different policy to objects.
 *
 * MMReserveWrapperHandler is similar to MMReserveObjectHandler except
 * that it installs a handler for use by MMReserveWrapper.
 */

extern void *MMReserveWrapper(size_t size, void *wrapper, gc_teb_t teb);
extern int MMCommitWrapper(void *p, size_t size, gc_teb_t teb);
extern MMAllocHandler MMReserveWrapperHandler(MMAllocHandler handler);


/* Allocate General Purpose Space
 *
 * MMAllocMisc and MMFreeMisc provide general-purpose memory for
 * internal use by the Dylan run-time system in a manner similar
 * to ANSI malloc and free.  The memory will not be relocated, and must
 * be freed explicitly.
 */

RUN_TIME_API extern void *MMAllocMisc(size_t size);
RUN_TIME_API extern MMAllocHandler MMAllocMiscHandler(MMAllocHandler handler);
RUN_TIME_API extern void MMFreeMisc(void *p, size_t size);


/* Declare Roots for Garbage Collection
 *
 * MMRootStatic declares that the region [base, limit) contains objects
 * in the DylanWorks Container Format which contain references which
 * should be assumed to be alive.  The objects in the region must be
 * aligned as specified by the format.
 *
 * MMRootAmbig declares that [base, limit) is a word-aligned array of
 * ambiguous references which should be assumed to be alive.
 * MMRootExact similarly declares exact references.
 *
 * An exact reference is legal iff it points to one of:
 *  - an area declared by MMRootStatic,
 *  - the current stack,
 *  - the base address of a committed object (see MMCommitObject),
 *  - the base address of a committed wrapper (see MMCommitWrapper).
 * The garbage collector may check this condition.
 *
 * I'll buy you a pint if you're the first to read this -- richard
 */

extern MMError MMRootStatic(void *base, void *limit);
extern MMError MMRootAmbig(void *base, void *limit);
extern MMError MMRootExact(void *base, void *limit);


#endif /* mm_h */
