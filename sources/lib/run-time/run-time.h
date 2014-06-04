#ifndef OPENDYLAN_CRT_RUNTIME_H
#define OPENDYLAN_CRT_RUNTIME_H

#include <stddef.h>
#include <setjmp.h>

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

/* This check is needed to bootstrap from the 2011.1 release. */
#if !defined(OPEN_DYLAN_PLATFORM_DARWIN) && defined(__APPLE__)
#  define OPEN_DYLAN_PLATFORM_DARWIN 1
#endif

#ifdef OPEN_DYLAN_PLATFORM_DARWIN
#  include "AvailabilityMacros.h"
#  if !defined(OPEN_DYLAN_COMPILER_CLANG) || \
      (__clang_major__ < 3) || \
      (MAC_OS_X_VERSION_MIN_REQUIRED < MAC_OS_X_VERSION_10_7)
#    define USE_PTHREAD_TLS 1
#  endif
#endif

#define OPTIONAL_ARGUMENT_CHECK(fn, req, count)
#define REQUIRED_ARGUMENT_CHECK(fn, req, count)

/* CONCRETE RAW TYPES */

typedef signed char             INT8;
typedef unsigned char           UINT8;
typedef signed short            INT16;
typedef unsigned short          UINT16;
typedef signed long             INT32;
typedef unsigned long           UINT32;
#ifdef OPEN_DYLAN_PLATFORM_WINDOWS
typedef _int64                  INT64;
typedef unsigned _int64         UINT64;
#else
typedef signed long long        INT64;
typedef unsigned long long      UINT64;
#endif
typedef float                   FLT;
typedef double                  DFLT;
#ifdef OPEN_DYLAN_PLATFORM_WINDOWS
typedef double                  EFLT;
#else
typedef long double             EFLT;
#endif

/* C RAW TYPES */

typedef INT64                   long_long;
typedef UINT64                  unsigned_long_long;
typedef EFLT                    long_double;

/* DYLAN RAW TYPES */

typedef unsigned long           DBOOL;
typedef unsigned char           DBCHR;
typedef unsigned char           DBYTE;
typedef UINT16                  DDBYTE;
typedef UINT16                  DUCHR;
typedef long                    DSINT;
typedef long                    DMINT;
typedef DMINT                   DWORD;
typedef unsigned long           DUMINT;
typedef INT64                   DLMINT;
typedef DLMINT                  DDWORD;
typedef UINT64                  DULMINT;
typedef INT64                   DBINT;
typedef float                   DSFLT;
typedef double                  DDFLT;
typedef long double             DEFLT;
typedef unsigned long           DADDR;
typedef char*                   DBSTR;
typedef const char*             DCBSTR;
typedef void*                   D;

/* COMPILER-SPECIFIC INTRINSICS */

#ifdef OPEN_DYLAN_COMPILER_GCC_LIKE
#  define NORETURN_FUNCTION __attribute__((noreturn))
#else
#  define NORETURN_FUNCTION
#endif

#ifdef OPEN_DYLAN_COMPILER_GCC_LIKE
#  define PURE_FUNCTION __attribute__((pure))
#else
#  warning missing attribute PURE_FUNCTION - performance degraded
#  define PURE_FUNCTION
#endif

#ifdef OPEN_DYLAN_COMPILER_GCC_LIKE
#  define CONDITIONAL_UPDATE(var, new_val, old_val) \
     (__sync_bool_compare_and_swap(&var, old_val, new_val) ? DTRUE : DFALSE)
#else
#  warning missing primitive CONDITIONAL_UPDATE - thread safety compromised
#  define CONDITIONAL_UPDATE(var, new_val, old_val) \
     ((old_val) == (var) ? (var = (new_val), DTRUE) : DFALSE)
#endif

#ifdef OPEN_DYLAN_COMPILER_GCC_LIKE
#  define SYNCHRONIZE_SIDE_EFFECTS() __sync_synchronize()
#else
#  warning missing primitive SYNCHRONIZE_SIDE_EFFECTS - thread safety compromised
#  define SYNCHRONIZE_SIDE_EFFECTS()
#endif

#ifdef OPEN_DYLAN_COMPILER_GCC_LIKE
#  define SEQUENCE_POINT() __asm__ __volatile__ ("" ::: "memory")
#else
#  warning missing primitive SEQUENCE_POINT - thread safety compromised
#  define SEQUENCE_POINT()
#endif

#ifdef OPEN_DYLAN_COMPILER_GCC_LIKE
#  define TLS_VARIABLE __thread
#else
#  define TLS_VARIABLE
#endif

#ifdef OPEN_DYLAN_COMPILER_GCC
#  define TLS_INITIAL_EXEC __attribute__((tls_model("initial-exec")))
#endif

#if defined(OPEN_DYLAN_COMPILER_CLANG) && defined(__has_attribute)
#  if __has_attribute(tls_model)
#    define TLS_INITIAL_EXEC __attribute__((tls_model("initial-exec")))
#  endif
#endif

#ifndef TLS_INITIAL_EXEC
#  define TLS_INITIAL_EXEC
#endif

static inline long atomic_increment(long *var) {
#ifdef OPEN_DYLAN_COMPILER_GCC_LIKE
  return __sync_add_and_fetch(var, 1);
#else
#warning missing primitive atomic_increment - thread safety compromised
  *var = *var + 1;
  return *var;
#endif
}

static inline long atomic_decrement(long *var) {
#ifdef OPEN_DYLAN_COMPILER_GCC_LIKE
  return __sync_sub_and_fetch(var, 1);
#else
#warning missing primitive atomic_decrement - thread safety compromised
  *var = *var - 1;
  return *var;
#endif
}

static inline long atomic_cas(long *destination, long exchange, long compare) {
#ifdef OPEN_DYLAN_COMPILER_GCC_LIKE
  return __sync_val_compare_and_swap(destination, compare, exchange);
#else
#warning missing primitive atomic_cas - thread safety compromised
  int old = *destination;
  if (old == compare) {
     *destination = exchange;
  }
  return old;
#endif
}

/* DYLAN TAGGING */

#define TAG_BITS(x) (((unsigned long) x)&3)

#define ITAG 1
#define CTAG 2
#define UTAG 3

#define I(n) ((D)((((unsigned long)(n))<<2)|ITAG))
#define C(n) ((D)((((unsigned long)(n))<<2)|CTAG))
#define U(n) ((D)((((unsigned long)(n))<<2)|UTAG))
#define R(n) ((long)(n)>>2)

#define FI(n) ((D)(((unsigned long)(n))|ITAG))
#define FC(n) ((D)(((unsigned long)(n))|CTAG))
#define FU(n) ((D)(((unsigned long)(n))|UTAG))

#define TAGGEDQ(n) (TAG_BITS(n) != 0)

#define DTRUE  &KPtrueVKi
#define DFALSE &KPfalseVKi

/* BASIC DYLAN TYPES */

typedef D (*DFN)(D,int,...);
typedef D (*DLFN)();

struct _IClass;
struct _dylantype;
struct _DylanClass;

/* This corresponds to <mm-wrapper> defined in
 * dfmc/modeling/objects.dylan.
 */
typedef struct _Wrapper {
  struct _Wrapper * wrapper_wrapper;
  struct _IClass  * iclass;
  D                 subtype_mask;
  DMINT             fixed_part;
  DMINT             variable_part;
  D                 number_patterns;
  DMINT             patterns[1]; /* REPEATED */
} Wrapper;

typedef struct _obj {
  Wrapper * mm_wrapper;
  D         slots[1];
} OBJECT;

typedef struct _dsf_ {
  Wrapper * mm_wrapper;
  DSFLT     data;
} DSF_;

typedef struct _ddf_ {
  Wrapper * mm_wrapper;
  DDFLT     data;
} DDF_;

typedef struct _dmi_ {
  Wrapper * mm_wrapper;
  DMINT     data;
} DMI_;

typedef struct _dumi_ {
  Wrapper * mm_wrapper;
  DUMINT    data;
} DUMI_;

typedef struct _dbi_ {
  Wrapper * mm_wrapper;
  DUMINT    low;
  DMINT     high;
} DBI_;

/* This is the implementation class and corresponds
 * to the <implementation-class> defined in
 * dfmc/modeling/objects.dylan. Note that this
 * struct declaration is not the full struct.
 *
 * For a breakdown of the class properties, see
 * the packed-slots definition for ^class-properties
 * in dfmc/modeling/objects.dylan or the copy of it
 * in dylan/class.dylan.
 */
typedef struct _IClass {
  Wrapper *             my_wrapper;
  D                     the_class_properties;
  struct _DylanClass  * the_class;
  D                     the_wrapper;
} ICLASS;

/* This corresponds to <type> defined in
 * dfmc/modeling/objects.dylan.
 */
typedef struct _dylantype {
  Wrapper * mm_wrapper;
  DLFN      instancep_function;
} DYLANTYPE;

/* This corresponds to <class> defined in
 * dfmc/modeling/objects.dylan.
 */
typedef struct _DylanClass {
  Wrapper * my_wrapper;
  DLFN      instancep_function;
  D         debug_name;
  ICLASS  * the_iclass;
  D         subtype_bit;
} DYLANCLASS;

#define OBJECT_WRAPPER(x) \
    (((OBJECT*)(x))->mm_wrapper)

#define OBJECT_ICLASS(x) \
    (((Wrapper*)(OBJECT_WRAPPER(x)))->iclass)

#define OBJECT_CLASS(x) \
    (((ICLASS*)(OBJECT_ICLASS(x)))->the_class)

#define CLASS_WRAPPER(x) \
    (((DYLANCLASS*)(x))->the_wrapper)

#define CLASS_ICLASS(x) \
    (((DYLANCLASS*)(x))->the_iclass)

#define ICLASS_WRAPPER(x) \
    (((ICLASS*)(x))->the_wrapper)

#define ICLASS_CLASS(x) \
    (((ICLASS*)(x))->the_class)

#define WRAPPER_ICLASS(x) \
    (((Wrapper*)(x))->iclass)

#define WRAPPER_CLASS(x) \
    (ICLASS_CLASS(WRAPPER_ICLASS(x)))



typedef DSF_*  DSF;
typedef DDF_*  DDF;
typedef DMI_*  DMI;
typedef DUMI_* DUMI;
typedef DBI_*  DBI;

#define define_SOV(_name, _size) \
  typedef struct _sov##_name { \
    D class; \
    D size; \
    D data[_size]; \
  } _name

define_SOV(SOV, 1);

static inline int vector_size (SOV* vector) {
  return(R(vector->size));
}

static inline int vector_size_setter (int new_size, SOV* vector) {
  vector->size = I(new_size);
  return(new_size);
}

static inline D* vector_data(SOV* vector) {
  return(vector->data);
}

static inline D vector_ref(SOV* vector, int offset) {
  return(vector_data((SOV*)vector)[offset]);
}

#define define_byte_string(_name, _size) \
  typedef struct _bs##_name { \
    D class; \
    D size; \
    char data[_size + 1]; \
  } _name

define_byte_string(BS, 0);

typedef struct _symbol {
  D class;
  D name;
} SYMBOL;

typedef struct _fn {
  D    class;
  DFN  xep;
  D    signature;
  DLFN mep;
} FN;

typedef struct _cfn {
  D    class;
  DFN  xep;
  D    signature;
  DLFN mep;
  D    size;
  D    environment[0];
} CFN;

typedef struct _kfn {
  D    class;
  DFN  xep;
  D    signature;
  DLFN mep;
  DLFN iep;
  D    keyword_specifiers;
} KFN;

typedef struct _kcfn {
  D    class;
  DFN  xep;
  D    signature;
  DLFN mep;
  DLFN iep;
  D    keyword_specifiers;
  D    size;
  D    environment[0];
} KCFN;

typedef struct _accessor_method {
  D    header;
  DFN  xep;
  D    slotd;
} ACCESSOR;

typedef struct _sig {
  D    class;
  D    properties;
  SOV* required;
  SOV* values;
  D    rest_value;
} SIG;

typedef struct _engine {
  D    class;
  D    properties;
  DLFN callback;
  DLFN entry_point;
} ENGINE;

typedef struct _monomorphic_discriminator {
  D     class;
  D     properties;
  DLFN  callback;
  DLFN  entry_point;
  DWORD key;
  D     nextnode;
} MONOMORPHICDISCRIMINATOR;

typedef struct _if_type_discriminator {
  D    class;
  D    properties;
  DLFN callback;
  DLFN entry_point;
  D    type;
  D    thennode;
  D    elsenode;
} IFTYPEDISCRIMINATOR;

typedef struct _typecheck_discriminator {
  D    class;
  D    properties;
  DLFN callback;
  DLFN entry_point;
  D    type;
  D    nextnode;
} TYPECHECKDISCRIMINATOR;


typedef struct _single_method_engine_node {
  D    class;
  D    properties;
  DLFN callback;
  DLFN entry_point;
  D    meth;
  D    data;
  D    keywords;                /* Not in all. */
} SINGLEMETHODENGINE;

typedef struct _cache_header_engine_node {
  D    class;
  D    properties;
  DLFN callback;
  DLFN entry_point;
  D    nextnode;
  D    parent;
} CACHEHEADERENGINE;

typedef struct _profiling_cache_header_engine_node {
  D    class;
  D    properties;
  DLFN callback;
  DLFN entry_point;
  D    nextnode;
  D    parent;
  DSINT count1;
  DSINT count2;
} PROFILINGCACHEHEADERENGINE;


typedef struct _gfn {
  D       class;
  DFN     xep;
  D       signature;
  D       cache;
  D       debug_name;
  D       methods;
  ENGINE* engine;
} GFN;

#define DEFUN(name, xep, iep)   D name[] = {I(0),I(0),I(0),(D)xep,(D)iep,I(0),I(0),I(0),I(0)}

extern D primitive_set_generic_function_entrypoints(D fn); /* !@#$ FIX UP NAME */

extern D primitive_runtime_module_handle();

/* MULTIPLE VALUES */

#define VALUES_MAX 64           /* maximum number of multiple values */

typedef struct _mv {
  int count;
  D   value[VALUES_MAX];
} MV;

#define MV_GET_ELT(n) \
  (get_teb()->return_values.count > (n) ? get_teb()->return_values.value[n] : DFALSE)
#define MV_SET_ELT(n, t)        (get_teb()->return_values.value[n] = (t))
#define MV_SET_COUNT(n)         (get_teb()->return_values.count = (n))

extern D MV_SPILL (D first_value);
extern D MV_UNSPILL (D spill_t);
extern D MV_GET_REST_AT (D first_value, DSINT first);
extern D MV_SET_REST_AT (D v, DSINT first);
extern D MV_CHECK_TYPE_REST (D first_value, D rest_type, int n, ...);

#define MV_CHECK_TYPE_PROLOGUE(fv) \
  MV *spill;                       \
  spill = (MV*)MV_SPILL(fv)

#define MV_CHECK_TYPE_EPILOGUE()   \
  MV_UNSPILL((D)spill)

extern DMINT _unused_arg;
extern DMINT* P_unused_arg;

/* NON-LOCAL CONTROL FLOW FRAMES */

//#define VERIFY_NLX

typedef struct _bind_exit_frame {
  jmp_buf                       destination;
  MV                            return_values;
  struct _unwind_protect_frame* present_unwind_protect_frame;
#ifdef VERIFY_NLX
  struct _teb*                  verify_teb;
#endif
} Bind_exit_frame;

typedef struct _unwind_protect_frame {
  jmp_buf                       destination;
  MV                            return_values;
  struct _bind_exit_frame*      ultimate_destination;
  struct _unwind_protect_frame* previous_unwind_protect_frame;
#ifdef VERIFY_NLX
  struct _teb*                  verify_teb;
#endif
} Unwind_protect_frame;

extern D SETUP_EXIT_FRAME (D);
extern D SETUP_UNWIND_FRAME (D);
extern D FRAME_DEST (D);
extern D FRAME_RETVAL (D);
extern D FALL_THROUGH_UNWIND (D);
extern D CONTINUE_UNWIND ();
extern D NLX (Bind_exit_frame*, D);

#define ENTER_EXIT_FRAME(destvar) \
  Bind_exit_frame bef_ ## destvar; \
  destvar = SETUP_EXIT_FRAME(& bef_ ## destvar)

#define ENTER_UNWIND_FRAME(destvar) \
  Unwind_protect_frame uwp_ ## destvar; \
  destvar = SETUP_UNWIND_FRAME(& uwp_ ## destvar)

#define nlx_longjmp(env, val)_longjmp(env, val);
#define nlx_setjmp(env) _setjmp(env)

/* PER-THREAD CONTEXT */

#define MAX_ARGUMENTS 256

typedef struct _teb {
        /* dispatch context (used together, keep close) */
        FN *function;
        int argument_count;
        D   next_methods;

        /* return values (for multiple values) */
        MV  return_values;

        /* unwinding state */
        Unwind_protect_frame* uwp_frame;
        Unwind_protect_frame  top_uwp_frame;

        /* thread state */
        void *thread;
        void *thread_handle;
        void *tlv_vector;

        /* argument buffers (used in dispatch, primitives...) */
        D arguments[MAX_ARGUMENTS];
        D new_arguments[MAX_ARGUMENTS];
        D a[MAX_ARGUMENTS];
        D iep_a[MAX_ARGUMENTS];
        D apply_buffer[MAX_ARGUMENTS];
        D buffer[MAX_ARGUMENTS];
} TEB;

#ifdef USE_PTHREAD_TLS
extern PURE_FUNCTION TEB* get_teb(void);
#else
extern TLS_VARIABLE TLS_INITIAL_EXEC TEB* dylan_teb;
PURE_FUNCTION static inline TEB* get_teb()
{
  return dylan_teb;
}
#endif

/* CALLING CONVENTION ENTRY POINTS */

extern D XEP(FN*, int, ...);

extern D topI();

extern D xep_0 (FN*,int);
extern D xep_1 (FN*,int,D);
extern D xep_2 (FN*,int,D,D);
extern D xep_3 (FN*,int,D,D,D);
extern D xep_4 (FN*,int,D,D,D,D);
extern D xep_5 (FN*,int,D,D,D,D,D);
extern D xep_6 (FN*,int,D,D,D,D,D,D);
extern D xep_7 (FN*,int,D,D,D,D,D,D,D);
extern D xep_8 (FN*,int,D,D,D,D,D,D,D,D);
extern D xep_9 (FN*,int,D,D,D,D,D,D,D,D,D);
extern D xep   (FN*,int,...);

extern D rest_xep_0 (FN*,int,...);
extern D rest_xep_1 (FN*,int,D,...);
extern D rest_xep_2 (FN*,int,D,D,...);
extern D rest_xep_3 (FN*,int,D,D,D,...);
extern D rest_xep_4 (FN*,int,D,D,D,D,...);
extern D rest_xep_5 (FN*,int,D,D,D,D,D,...);
extern D rest_xep_6 (FN*,int,D,D,D,D,D,D,...);
extern D rest_xep_7 (FN*,int,D,D,D,D,D,D,D,...);
extern D rest_xep_8 (FN*,int,D,D,D,D,D,D,D,D,...);
extern D rest_xep_9 (FN*,int,D,D,D,D,D,D,D,D,D,...);
extern D rest_xep   (FN*,int,...);

extern D rest_key_xep_1 (FN*,int,...);
extern D rest_key_xep_2 (FN*,int,...);
extern D rest_key_xep_3 (FN*,int,...);
extern D rest_key_xep_4 (FN*,int,...);
extern D rest_key_xep_5 (FN*,int,...);
extern D rest_key_xep_6 (FN*,int,...);
extern D rest_key_xep_7 (FN*,int,...);
extern D rest_key_xep_8 (FN*,int,...);
extern D rest_key_xep_9 (FN*,int,...);
extern D rest_key_xep   (FN*,int,...);

extern D key_mep_1 (D a1, ...);
extern D key_mep_2 (D a1, ...);
extern D key_mep_3 (D a1, ...);
extern D key_mep_4 (D a1, ...);
extern D key_mep_5 (D a1, ...);
extern D key_mep_6 (D a1, ...);
extern D key_mep_7 (D a1, ...);
extern D key_mep_8 (D a1, ...);
extern D key_mep_9 (D a1, ...);
extern D key_mep (D a1, ...);

extern D gf_xep_0 (FN* fn, int n);
extern D gf_xep_1 (FN* fn, int n, D a1);
extern D gf_xep_2 (FN* fn, int n, D a1, D a2);
extern D gf_xep_3 (FN* fn, int n, D a1, D a2, D a3);
extern D gf_xep_4 (FN* fn, int n, D a1, D a2, D a3, D a4);
extern D gf_xep_5 (FN* fn, int n, D a1, D a2, D a3, D a4, D a5);
extern D gf_xep_6 (FN* fn, int n, D a1, D a2, D a3, D a4, D a5, D a6);
extern D gf_xep_7 (FN* fn, int n, D a1, D a2, D a3, D a4, D a5, D a6, D a7);
extern D gf_xep   (FN* fn, int n, ...);

extern D gf_optional_xep_0 (FN* fn, int n, ...);
extern D gf_optional_xep_1 (FN* fn, int n, ...);
extern D gf_optional_xep_2 (FN* fn, int n, ...);
extern D gf_optional_xep_3 (FN* fn, int n, ...);
extern D gf_optional_xep_4 (FN* fn, int n, ...);
extern D gf_optional_xep_5 (FN* fn, int n, ...);
extern D gf_optional_xep_6 (FN* fn, int n, ...);
extern D gf_optional_xep   (FN* fn, int n, ...);

extern D gf_iep_0 ();
extern D gf_iep_1 (D a1);
extern D gf_iep_2 (D a1, D a2);
extern D gf_iep_3 (D a1, D a2, D a3);
extern D gf_iep_4 (D a1, D a2, D a3, D a4);
extern D gf_iep_5 (D a1, D a2, D a3, D a4, D a5);
extern D gf_iep_6 (D a1, D a2, D a3, D a4, D a5, D a6);
extern D gf_iep_7 (D a1, D a2, D a3, D a4, D a5, D a6, D a7);
extern D gf_iep   (D argvec);

extern D slotacc_single_q_instance_getter_xep (ACCESSOR* am, int n, D a1);
extern D slotacc_single_q_instance_setter_xep (ACCESSOR* am, int n, D a1, D a2);
extern D slotacc_single_q_class_getter_xep (ACCESSOR* am, int n, D a1);
extern D slotacc_single_q_class_setter_xep (ACCESSOR* am, int n, D a1, D a2);
extern D slotacc_repeated_instance_getter_xep (ACCESSOR* am, int n, D a1, D a2);
extern D slotacc_repeated_instance_setter_xep (ACCESSOR* am, int n, D a1, D a2, D a3);
extern D primitive_set_accessor_method_xep (D am, D what);

#define CALLN(fn,n) (D)((((FN*)fn)->xep)(((FN*)(fn)),n
#define CALL0(fn) CALLN(fn,0)))
#define CALL1(fn,a1) CALLN(fn,1),(a1)))
#define CALL2(fn,a1,a2) CALLN(fn,2),(a1),(a2)))
#define CALL3(fn,a1,a2,a3) CALLN(fn,3),(a1),(a2),(a3)))
#define CALL4(fn,a1,a2,a3,a4) CALLN(fn,4),(a1),(a2),(a3),(a4)))
#define CALL5(fn,a1,a2,a3,a4,a5) CALLN(fn,5),(a1),(a2),(a3),(a4),(a5)))
#define CALL6(fn,a1,a2,a3,a4,a5,a6) CALLN(fn,6),(a1),(a2),(a3),(a4),(a5),(a6)))
#define CALL7(fn,a1,a2,a3,a4,a5,a6,a7) CALLN(fn,7),(a1),(a2),(a3),(a4),(a5),(a6),(a7)))
#define CALL8(fn,a1,a2,a3,a4,a5,a6,a7,a8) CALLN(fn,8),(a1),(a2),(a3),(a4),(a5),(a6),(a7),(a8)))
#define CALL9(fn,a1,a2,a3,a4,a5,a6,a7,a8,a9) CALLN(fn,9),(a1),(a2),(a3),(a4),(a5),(a6),(a7),(a8),(a9)))
#define CALL10(fn,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) CALLN(fn,10),(a1),(a2),(a3),(a4),(a5),(a6),(a7),(a8),(a9),(a10)))

#define MEP_CALL_PROLOG(fn,nm,ac) \
  {\
    TEB* mcp_teb = get_teb(); \
    mcp_teb->function = (FN*)(fn); \
    mcp_teb->next_methods = (nm); \
    mcp_teb->argument_count = (ac); \
  }
#define MEP_CALLN(fn) (D)((((FN*)fn)->mep)(
#define MEP_CALL0(fn) MEP_CALLN(fn)))
#define MEP_CALL1(fn,a1) MEP_CALLN(fn)(a1)))
#define MEP_CALL2(fn,a1,a2) MEP_CALLN(fn)(a1),(a2)))
#define MEP_CALL3(fn,a1,a2,a3) MEP_CALLN(fn)(a1),(a2),(a3)))
#define MEP_CALL4(fn,a1,a2,a3,a4) MEP_CALLN(fn)(a1),(a2),(a3),(a4)))
#define MEP_CALL5(fn,a1,a2,a3,a4,a5) MEP_CALLN(fn)(a1),(a2),(a3),(a4),(a5)))
#define MEP_CALL6(fn,a1,a2,a3,a4,a5,a6) MEP_CALLN(fn)(a1),(a2),(a3),(a4),(a5),(a6)))
#define MEP_CALL7(fn,a1,a2,a3,a4,a5,a6,a7) MEP_CALLN(fn)(a1),(a2),(a3),(a4),(a5),(a6),(a7)))
#define MEP_CALL8(fn,a1,a2,a3,a4,a5,a6,a7,a8) MEP_CALLN(fn)(a1),(a2),(a3),(a4),(a5),(a6),(a7),(a8)))
#define MEP_CALL9(fn,a1,a2,a3,a4,a5,a6,a7,a8,a9) MEP_CALLN(fn)(a1),(a2),(a3),(a4),(a5),(a6),(a7),(a8),(a9)))
#define MEP_CALL10(fn,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) MEP_CALLN(fn)(a1),(a2),(a3),(a4),(a5),(a6),(a7),(a8),(a9),(a10)))

#define MIEP_CALL_PROLOG(nm) { get_teb()->next_methods = (nm); }

#define ENGINE_NODE_CALL_PROLOG(fn,eng,ac) \
  { \
    TEB* encp_teb = get_teb(); \
    encp_teb->next_methods = (fn); \
    encp_teb->function = (FN*)(eng); \
    encp_teb->argument_count =(ac); \
  }
#define ENGINE_NODE_CALL(fn,eng,ac) \
  { \
    TEB* enc_teb = get_teb(); \
    enc_teb->next_methods = (fn); \
    enc_teb->function = (FN*)(eng); \
  }

#define ENGINE_NODE_CALL0(eng) \
    ((((ENGINE*)eng)->entry_point)())
#define ENGINE_NODE_CALL1(eng,a1) \
    ((((ENGINE*)eng)->entry_point)((a1)))
#define ENGINE_NODE_CALL2(eng,a1,a2) \
    ((((ENGINE*)eng)->entry_point)((a1),(a2)))
#define ENGINE_NODE_CALL3(eng,a1,a2,a3) \
    ((((ENGINE*)eng)->entry_point)((a1),(a2),(a3)))
#define ENGINE_NODE_CALL4(eng,a1,a2,a3,a4) \
    ((((ENGINE*)eng)->entry_point)((a1),(a2),(a3),(a4)))
#define ENGINE_NODE_CALL5(eng,a1,a2,a3,a4,a5) \
    ((((ENGINE*)eng)->entry_point)((a1),(a2),(a3),(a4),(a5)))
#define ENGINE_NODE_CALL6(eng,a1,a2,a3,a4,a5,a6) \
    ((((ENGINE*)eng)->entry_point)((a1),(a2),(a3),(a4),(a5),(a6)))
#define ENGINE_NODE_CALL7(eng,a1,a2,a3,a4,a5,a6,a7) \
    ((((ENGINE*)eng)->entry_point)((a1),(a2),(a3),(a4),(a5),(a6),(a7)))

extern D inline_invoke_engine_node(ENGINE*, int, ...);

#define ENGINE_NODE_CALLN(ac,eng) \
    (inline_invoke_engine_node((ENGINE*)(eng),(ac)

#define CONGRUENT_CALL_PROLOG(fn,ac) \
  { \
    TEB *ccp_teb = get_teb(); \
    ccp_teb->next_methods = (fn); \
    ccp_teb->function = (FN*)(((GFN*)fn)->engine); \
    ccp_teb->argument_count =(ac); \
  }
#define CONGRUENT_CALL(fn,ac) \
  { \
    TEB *cc_teb = get_teb(); \
    cc_teb->next_methods = (fn); \
    cc_teb->function = (FN*)(((GFN*)fn)->engine); \
  }

#define CONGRUENT_CALL0() \
    ((((ENGINE*)get_teb()->function)->entry_point)())
#define CONGRUENT_CALL1(a1) \
    ((((ENGINE*)get_teb()->function)->entry_point)((a1)))
#define CONGRUENT_CALL2(a1,a2) \
    ((((ENGINE*)get_teb()->function)->entry_point)((a1),(a2)))
#define CONGRUENT_CALL3(a1,a2,a3) \
    ((((ENGINE*)get_teb()->function)->entry_point)((a1),(a2),(a3)))
#define CONGRUENT_CALL4(a1,a2,a3,a4) \
    ((((ENGINE*)get_teb()->function)->entry_point)((a1),(a2),(a3),(a4)))
#define CONGRUENT_CALL5(a1,a2,a3,a4,a5) \
    ((((ENGINE*)get_teb()->function)->entry_point)((a1),(a2),(a3),(a4),(a5)))
#define CONGRUENT_CALL6(a1,a2,a3,a4,a5,a6) \
    ((((ENGINE*)get_teb()->function)->entry_point)((a1),(a2),(a3),(a4),(a5),(a6)))
#define CONGRUENT_CALL7(a1,a2,a3,a4,a5,a6,a7) \
    ((((ENGINE*)get_teb()->function)->entry_point)((a1),(a2),(a3),(a4),(a5),(a6),(a7)))

#define CONGRUENT_CALLN(ac) \
    (inline_invoke_engine_node((ENGINE*)(get_teb()->function),(ac)


#define IEP_CALLN(fn) (D)((((FN*)fn)->iep)(
#define IEP_CALL0(fn) IEP_CALLN(fn)))
#define IEP_CALL1(fn,a1) IEP_CALLN(fn)(a1)))
#define IEP_CALL2(fn,a1,a2) IEP_CALLN(fn)(a1),(a2)))
#define IEP_CALL3(fn,a1,a2,a3) IEP_CALLN(fn)(a1),(a2),(a3)))
#define IEP_CALL4(fn,a1,a2,a3,a4) IEP_CALLN(fn)(a1),(a2),(a3),(a4)))
#define IEP_CALL5(fn,a1,a2,a3,a4,a5) IEP_CALLN(fn)(a1),(a2),(a3),(a4),(a5)))
#define IEP_CALL6(fn,a1,a2,a3,a4,a5,a6) IEP_CALLN(fn)(a1),(a2),(a3),(a4),(a5),(a6)))
#define IEP_CALL7(fn,a1,a2,a3,a4,a5,a6,a7) IEP_CALLN(fn)(a1),(a2),(a3),(a4),(a5),(a6),(a7)))
#define IEP_CALL8(fn,a1,a2,a3,a4,a5,a6,a7,a8) IEP_CALLN(fn)(a1),(a2),(a3),(a4),(a5),(a6),(a7),(a8)))
#define IEP_CALL9(fn,a1,a2,a3,a4,a5,a6,a7,a8,a9) IEP_CALLN(fn)(a1),(a2),(a3),(a4),(a5),(a6),(a7),(a8),(a9)))
#define IEP_CALL10(fn,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) IEP_CALLN(fn)(a1),(a2),(a3),(a4),(a5),(a6),(a7),(a8),(a9),(a10)))

/* CLOSURE SUPPORT */

extern void INIT_CLOSURE(D, int, ...);
extern D MAKE_CLOSURE_INITD(D, int, ...);
extern D MAKE_CLOSURE_INITD_SIG(D, D, int, ...);
extern D MAKE_CLOSURE(D, int);
extern D MAKE_CLOSURE_SIG(D, D, int);
extern D MAKE_METHOD_SIG(D, D);
extern D SET_METHOD_SIG(D, D);
#define CAPTURE_ENVIRONMENT CFN* _fn = ((CFN*)get_teb()->function);
extern void INIT_KEYWORD_CLOSURE(D, int, ...);
extern D MAKE_KEYWORD_CLOSURE_INITD(D, int, ...);
extern D MAKE_KEYWORD_CLOSURE_INITD_SIG(D, D, int, ...);
extern D MAKE_KEYWORD_CLOSURE(D, int);
extern D MAKE_KEYWORD_CLOSURE_SIG(D, D, int);
extern D MAKE_KEYWORD_METHOD_SIG(D, D);
extern D SET_KEYWORD_METHOD_SIG(D, D);
#define CAPTURE_KEYWORD_ENVIRONMENT KCFN* _fn = ((KCFN*)get_teb()->function);
#define CREF(n) (_fn->environment[(n)])
#define MREF    (_fn)

/*
 * PRIMITIVES
 */

/* COMPARISON PRIMITIVES */

#define RAWASBOOL(x)                    ((D)((x) ? DTRUE : DFALSE))
#define primitive_raw_as_boolean(x)     RAWASBOOL(x)
#define primitive_as_boolean(x)         (((D)(x)) == DFALSE ? DFALSE : DTRUE)
#define BOOLASRAW(x)                    (((D)(x)) != DFALSE)
#define primitive_boolean_as_raw(x)     BOOLASRAW(x)
#define primitive_not(x)                (((D)(x)) == DFALSE ? DTRUE : DFALSE)
#define primitive_idQ(x, y)             (RAWASBOOL((x) == (y)))
#define primitive_not_idQ(x, y)         (RAWASBOOL((x) != (y)))

extern D primitive_compare_bytes(D base1, DSINT offset1,
                                 D base2, DSINT offset2, DSINT size);
extern D primitive_compare_words(D base1, DSINT offset1,
                                 D base2, DSINT offset2, DSINT size);


/* COMPARISON PRIMITIVES */

#define primitive_instanceQ(x, y)       ((((DYLANTYPE*)(y))->instancep_function)((x),(y)))
#define primitive_range_check(x, l, h)  (RAWASBOOL(((x) >= (l)) & ((x) < (h))))
extern D primitive_type_check(D x, D t);


/* OBJECT REPRESENTATION PRIMITIVES AND SUPPORT */

extern void primitive_break (void);
extern void primitive_invoke_debugger (D format_string, D arguments);
extern D primitive_inside_debuggerQ (void);
extern void primitive_debug_message (D format_string, D arguments);

#if defined(__alpha) || defined(__x86_64__)
#define primitive_word_size() 8
#else
#define primitive_word_size() 4
#endif

#define primitive_header_size() primitive_word_size ()



/* Well this wasn't right
#define primitive_object_class(object) (((OBJECT*)object)->class)
*/

#define primitive_initialized_slot_value(object, position) \
  ((((OBJECT*)object)->slots)[position])
#define primitive_slot_value_setter(new_value, object, position) \
  ((((OBJECT*)object)->slots)[position] = (new_value))

extern D primitive_slot_value(D object, DSINT position);

#define primitive_repeated_slot_value(object, base_position, position) \
  ((((OBJECT*)object)->slots)[base_position + position])
#define primitive_repeated_slot_value_setter(new_value, object, base_position, position) \
  ((((OBJECT*)object)->slots)[base_position + position] = (new_value))
#define primitive_repeated_instance_size(object, base_position) \
  (R((((OBJECT*)object)->slots)[base_position - 1]))

#define primitive_byte_element(object, position, index) \
        (((DBCHR*)&((((OBJECT*)object)->slots)[position]))[index])
#define primitive_byte_element_setter(new_value, object, position, index) \
        (((DBCHR*)&((((OBJECT*)object)->slots)[position]))[index] = \
           (new_value))

#define SLOT_VALUE_INITD(object, position) \
  ((((OBJECT*)object)->slots)[position])
#define SLOT_VALUE_SETTER(new_value, object, position) \
  ((((OBJECT*)object)->slots)[position] = (new_value))

extern D SLOT_VALUE(D object, DSINT position);

#define REPEATED_D_SLOT_VALUE(object, base_position, position) \
  ((((OBJECT*)object)->slots)[base_position + position])
#define REPEATED_D_SLOT_VALUE_SETTER(new_value, object, base_position, position) \
  ((((OBJECT*)object)->slots)[base_position + position] = (new_value))

#define REPEATED_DBCHR_SLOT_VALUE(object, position, index) \
        (((DBCHR*)&((((OBJECT*)object)->slots)[position]))[index])
#define REPEATED_DBCHR_SLOT_VALUE_SETTER(new_value, object, position, index) \
        (((DBCHR*)&((((OBJECT*)object)->slots)[position]))[index] = \
           (new_value))

#define REPEATED_DUCHR_SLOT_VALUE(object, position, index) \
        (((DUCHR*)&((((OBJECT*)object)->slots)[position]))[index])
#define REPEATED_DUCHR_SLOT_VALUE_SETTER(new_value, object, position, index) \
        (((DUCHR*)&((((OBJECT*)object)->slots)[position]))[index] = \
           (new_value))

#define REPEATED_DBYTE_SLOT_VALUE(object, position, index) \
        (((DBYTE*)&((((OBJECT*)object)->slots)[position]))[index])
#define REPEATED_DBYTE_SLOT_VALUE_SETTER(new_value, object, position, index) \
        (((DBYTE*)&((((OBJECT*)object)->slots)[position]))[index] = \
           (new_value))

#define REPEATED_DDBYTE_SLOT_VALUE(object, position, index) \
        (((DDBYTE*)&((((OBJECT*)object)->slots)[position]))[index])
#define REPEATED_DDBYTE_SLOT_VALUE_SETTER(new_value, object, position, index) \
        (((DDBYTE*)&((((OBJECT*)object)->slots)[position]))[index] = \
           (new_value))

#define REPEATED_DDFLT_SLOT_VALUE(object, base_position, position) \
  (((DDFLT*)(((OBJECT*)object)->slots))[base_position + R(position)])
#define REPEATED_DDFLT_SLOT_VALUE_SETTER(new_value, object, base_position, position) \
  (((DDFLT*)(((OBJECT*)object)->slots))[base_position + R(position)] = (new_value))

#define REPEATED_DDWORD_SLOT_VALUE(object, base_position, position) \
  (((DDWORD*)(((OBJECT*)object)->slots))[base_position + R(position)])
#define REPEATED_DDWORD_SLOT_VALUE_SETTER(new_value, object, base_position, position) \
  (((DDWORD*)(((OBJECT*)object)->slots))[base_position + R(position)] = (new_value))


#define REPEATED_D_SLOT_VALUE_TAGGED(object, base_position, position) \
  ((((OBJECT*)object)->slots)[base_position + R(position)])
#define REPEATED_D_SLOT_VALUE_TAGGED_SETTER(new_value, object, base_position, position) \
  ((((OBJECT*)object)->slots)[base_position + R(position)] = (new_value))

#define REPEATED_DWORD_SLOT_VALUE_TAGGED(object, base_position, position) \
  (((DWORD*)(((OBJECT*)object)->slots))[base_position + R(position)])
#define REPEATED_DWORD_SLOT_VALUE_TAGGED_SETTER(new_value, object, base_position, position) \
  (((DWORD*)(((OBJECT*)object)->slots))[base_position + R(position)] = (new_value))

#define REPEATED_DSFLT_SLOT_VALUE_TAGGED(object, base_position, position) \
  (((DSFLT*)(((OBJECT*)object)->slots))[base_position + R(position)])
#define REPEATED_DSFLT_SLOT_VALUE_TAGGED_SETTER(new_value, object, base_position, position) \
  (((DSFLT*)(((OBJECT*)object)->slots))[base_position + R(position)] = (new_value))

/* SHOULD REMOVE BELOW */

#define REPEATED_DBCHR_SLOT_VALUE_TAGGED(object, position, index) \
        (((DBCHR*)&((((OBJECT*)object)->slots)[position]))[R(index)])
#define REPEATED_DBCHR_SLOT_VALUE_TAGGED_SETTER(new_value, object, position, index) \
        (((DBCHR*)&((((OBJECT*)object)->slots)[position]))[R(index)] = \
           (new_value))

#define REPEATED_DUCHR_SLOT_VALUE_TAGGED(object, position, index) \
        (((DUCHR*)&((((OBJECT*)object)->slots)[position]))[R(index)])
#define REPEATED_DUCHR_SLOT_VALUE_TAGGED_SETTER(new_value, object, position, index) \
        (((DUCHR*)&((((OBJECT*)object)->slots)[position]))[R(index)] = \
           (new_value))

#define REPEATED_DBYTE_SLOT_VALUE_TAGGED(object, position, index) \
        (((DBYTE*)&((((OBJECT*)object)->slots)[position]))[R(index)])
#define REPEATED_DBYTE_SLOT_VALUE_TAGGED_SETTER(new_value, object, position, index) \
        (((DBYTE*)&((((OBJECT*)object)->slots)[position]))[R(index)] = \
           (new_value))

#define REPEATED_DDBYTE_SLOT_VALUE_TAGGED(object, position, index) \
        (((DDBYTE*)&((((OBJECT*)object)->slots)[position]))[R(index)])
#define REPEATED_DDBYTE_SLOT_VALUE_TAGGED_SETTER(new_value, object, position, index) \
        (((DDBYTE*)&((((OBJECT*)object)->slots)[position]))[R(index)] = \
           (new_value))

#define REPEATED_DDFLT_SLOT_VALUE_TAGGED(object, base_position, position) \
  (((DDFLT*)(((OBJECT*)object)->slots))[base_position + R(position)])
#define REPEATED_DDFLT_SLOT_VALUE_TAGGED_SETTER(new_value, object, base_position, position) \
  (((DDFLT*)(((OBJECT*)object)->slots))[base_position + R(position)] = (new_value))

extern DSINT primitive_repeated_slot_offset(D x);
extern D     primitive_repeated_slot_as_raw(D x, DSINT offset);

extern void primitive_fillX
  (D dst, int base_offset, int offset, int size, D value);

extern void primitive_fill_bytesX
  (D dst, int base_offset, int offset, int size, DSINT value);

extern void primitive_replace_bytesX
  (D dst, DSINT dst_base_offset, DSINT dst_offset,
   D src, DSINT src_base_offset, DSINT src_offset, DSINT size);

extern void primitive_replaceX
  (D dst, DSINT dst_base_offset, DSINT dst_offset,
   D src, DSINT src_base_offset, DSINT src_offset, DSINT size);

/* LOW-LEVEL ACCESSOR PRIMITIVES */

#define AT(t,x,o,b)             (((t*)(((DADDR)(x))+(int)(b)))[(int)(o)])
#define AT_SETTER(t,e,x,o,b)    (AT(t,x,o,b) = (e))

#define primitive_c_pointer_at(x, o, b) AT(void *,x,o,b)
#define primitive_c_pointer_at_setter(e, x, o, b)  AT_SETTER(void*,e,x,o,b)

#define primitive_c_signed_int_at(x, o, b) \
        AT(signed int,x,o,b)
#define primitive_c_signed_int_at_setter(e, x, o, b) \
        AT_SETTER(signed int,e,x,o,b)
#define primitive_c_unsigned_int_at(x, o, b) \
        AT(unsigned int,x,o,b)
#define primitive_c_unsigned_int_at_setter(e, x, o, b) \
        AT_SETTER(unsigned int,e,x,o,b)
#define primitive_c_signed_char_at(x, o, b) \
        AT(signed char,x,o,b)
#define primitive_c_signed_char_at_setter(e, x, o, b) \
        AT_SETTER(signed char,e,x,o,b)
#define primitive_c_unsigned_char_at(x, o, b) \
        AT(unsigned char,x,o,b)
#define primitive_c_unsigned_char_at_setter(e, x, o, b) \
        AT_SETTER(unsigned char,e,x,o,b)
#define primitive_c_signed_short_at(x, o, b) \
        AT(signed short,x,o,b)
#define primitive_c_signed_short_at_setter(e, x, o, b) \
        AT_SETTER(signed short,e,x,o,b)
#define primitive_c_unsigned_short_at(x, o, b) \
        AT(unsigned short,x,o,b)
#define primitive_c_unsigned_short_at_setter(e, x, o, b) \
        AT_SETTER(unsigned short,e,x,o,b)
#define primitive_c_signed_long_at(x, o, b) \
        AT(signed long,x,o,b)
#define primitive_c_signed_long_at_setter(e, x, o, b) \
        AT_SETTER(signed long,e,x,o,b)
#define primitive_c_unsigned_long_at(x, o, b) \
        AT(unsigned long,x,o,b)
#define primitive_c_unsigned_long_at_setter(e, x, o, b) \
        AT_SETTER(unsigned long,e,x,o,b)
#define primitive_c_signed_long_long_at(x, o, b) \
        AT(signed_long_long,x,o,b)
#define primitive_c_signed_long_long_at_setter(e, x, o, b) \
        AT_SETTER(signed_long_long,e,x,o,b)
#define primitive_c_unsigned_long_long_at(x, o, b) \
        AT(unsigned_long_long,x,o,b)
#define primitive_c_unsigned_long_long_at_setter(e, x, o, b) \
        AT_SETTER(unsigned_long_long,e,x,o,b)
#define primitive_c_float_at(x, o, b) \
        AT(float,x,o,b)
#define primitive_c_float_at_setter(e, x, o, b) \
        AT_SETTER(float,e,x,o,b)
#define primitive_c_double_at(x, o, b) \
        AT(double,x,o,b)
#define primitive_c_double_at_setter(e, x, o, b) \
        AT_SETTER(double,e,x,o,b)
#define primitive_c_long_double_at(x, o, b) \
        AT(long_double,x,o,b)
#define primitive_c_long_double_at_setter(e, x, o, b) \
        AT_SETTER(long_double,e,x,o,b)

/*
 * bit-size and bit-offset are Dylan constants so a good C compiler
 * should be able to optimize these.
 */
#define MAKE_MASK(t, bit_offset, bit_size) \
  ((~(((t)(-1l)) << (bit_size))) << (bit_offset))


#define AT_FIELD(t, x, b, bit_offset, bit_size) \
  ((t)(((*((unsigned long *)(((DADDR)(x))+(b)))) >> (bit_offset)) \
       & MAKE_MASK(t, 0, bit_size)))


#define AT_FIELD_SETTER(t, n, x, b, bit_offset, bit_size) \
  (*((t *)(((DADDR)(x))+(b))) = \
   ((((n) & MAKE_MASK(t, 0, bit_size)) << (bit_offset)) \
     | ( (*((t *)(((DADDR)(x))+(b)))) \
         & ((t)~MAKE_MASK(t, bit_offset, bit_size)))))

#define primitive_c_unsigned_field(pointer, byte_offset, bit_offset, \
                                    bit_size) \
  AT_FIELD(unsigned long, pointer, byte_offset, bit_offset, bit_size)

#define primitive_c_signed_field(pointer, byte_offset, bit_offset, \
                                  bit_size) \
  AT_FIELD(signed long, pointer, byte_offset, bit_offset, bit_size)

#define primitive_c_int_field(pointer, byte_offset, bit_offset, \
                               bit_size) \
  AT_FIELD(unsigned int, pointer, byte_offset, bit_offset, bit_size)

#define primitive_c_unsigned_field_setter(new, pointer, byte_offset, \
                                           bit_offset, bit_size) \
  AT_FIELD_SETTER(unsigned long, new, pointer, byte_offset, bit_offset, \
                  bit_size)

#define primitive_c_signed_field_setter(new, pointer, byte_offset, \
                                           bit_offset, bit_size) \
  AT_FIELD_SETTER(signed long, new, pointer, byte_offset, bit_offset, \
                  bit_size)

#define primitive_c_int_field_setter(new, pointer, byte_offset, \
                                          bit_offset, bit_size) \
  AT_FIELD_SETTER(unsigned int, new, pointer, byte_offset, bit_offset, \
                  bit_size)

#define primitive_element(x, o, b) \
        AT(D,x,o,b)
#define primitive_element_setter(e, x, o, b) \
        AT_SETTER(D,e,x,o,b)

/*
#define primitive_boolean_at(x, o, b) \
        AT(DBOOL,x,o,b)
#define primitive_boolean_at_setter(e, x, o, b) \
        AT_SETTER(DBOOL,e,x,o,b)
#define primitive_byte_character_at(x, o, b) \
        AT(DBCHR,x,o,b)
#define primitive_byte_character_at_setter(e, x, o, b) \
        AT_SETTER(DBCHR,e,x,o,b)
#define primitive_small_integer_at(x, o, b) \
        AT(DSINT,x,o,b)
#define primitive_small_integer_at_setter(e, x, o, b) \
        AT_SETTER(DSINT,e,x,o,b)
#define primitive_machine_integer_at(x, o, b) \
        AT(DMINT,x,o,b)
#define primitive_machine_integer_at_setter(e, x, o, b) \
        AT_SETTER(DMINT,e,x,o,b)
#define primitive_unsigned_machine_integer_at(x, o, b) \
        AT(DUMINT,x,o,b)
#define primitive_unsigned_machine_integer_at_setter(e, x, o, b) \
        AT_SETTER(DUMINT,e,x,o,b)
#define primitive_long_machine_integer_at(x, o, b) \
        AT(DMINT,x,o,b)
#define primitive_long_machine_integer_at_setter(e, x, o, b) \
        AT_SETTER(DMINT,e,x,o,b)
#define primitive_unsigned_long_machine_integer_at(x, o, b) \
        AT(DUMINT,x,o,b)
#define primitive_unsigned_long_machine_integer_at_setter(e, x, o, b) \
        AT_SETTER(DUMINT,e,x,o,b)
#define primitive_single_float_at(x, o, b) \
        AT(float,x,o,b)
#define primitive_single_float_at_setter(e, x, o, b) \
        AT_SETTER(float,e,x,o,b)
#define primitive_double_float_at(x, o, b) \
        AT(double,x,o,b)
#define primitive_double_float_at_setter(e, x, o, b) \
        AT_SETTER(double,e,x,o,b)
#define primitive_extended_float_at(x, o, b) \
        AT(long_double,x,o,b)
#define primitive_extended_float_at_setter(e, x, o, b) \
        AT_SETTER(long_double,e,x,o,b)
#define primitive_pointer_at(x, o, b) \
        AT(D,x,o,b)
#define primitive_pointer_at_setter(e, x, o, b) \
        AT_SETTER(D,e,x,o,b)
#define primitive_address_at(x, o, b) \
        AT(DADDR,x,o,b)
#define primitive_address_at_setter(e, x, o, b) \
        AT_SETTER(DADDR,e,x,o,b)
*/

/* Allocate General Purpose Space
 *
 * MMAllocMisc and MMFreeMisc provide general-purpose memory for
 * internal use by the Dylan run-time system in a manner similar
 * to ANSI malloc and free.  The memory will not be relocated, and must
 * be freed explicitly.
 */

extern void *MMAllocMisc(size_t size);
extern void MMFreeMisc(void *p, size_t size);

/* ALLOCATION PRIMITIVES */

extern D primitive_allocate(DSINT);
extern D primitive_byte_allocate(DSINT, DSINT);
extern D primitive_untraced_allocate(DSINT);
extern D primitive_manual_allocate(D);
extern void primitive_manual_free(D);
extern D primitive_allocate_wrapper(DSINT, D, DSINT, D, DSINT, DSINT);
extern D primitive_byte_allocate_filled_terminated(DSINT, DSINT, D, DSINT, D, DSINT, DSINT);
extern D primitive_byte_allocate_leaf_filled_terminated(DSINT, DSINT, D, DSINT, D, DSINT, DSINT);
extern D primitive_allocate_filled(DSINT, D, DSINT, D, DSINT, DSINT);
extern D primitive_object_allocate_filled(DSINT, D, DSINT, D, DSINT, DSINT, D);
extern D primitive_byte_allocate_filled(DSINT, D, DSINT, D, DSINT, DSINT, DBYTE);
extern D primitive_double_byte_allocate_filled(DSINT, D, DSINT, D, DSINT, DSINT, DDBYTE);
extern D primitive_word_allocate_filled(DSINT, D, DSINT, D, DSINT, DSINT, DWORD);
extern D primitive_double_word_allocate_filled(DSINT, D, DSINT, D, DSINT, DSINT, DDWORD);
extern D primitive_single_float_allocate_filled(DSINT, D, DSINT, D, DSINT, DSINT, DSFLT);
extern D primitive_double_float_allocate_filled(DSINT, D, DSINT, D, DSINT, DSINT, DDFLT);
extern D primitive_byte_allocate_leaf_filled(DSINT, D, DSINT, D, DSINT, DSINT, DBYTE);
extern D primitive_allocate_in_awl_pool(DSINT, D, DSINT, D, DSINT, DSINT, D);
extern D primitive_allocate_weak_in_awl_pool(DSINT, D, DSINT, D, DSINT, DSINT, D);

/* stack allocation */

#ifdef OPEN_DYLAN_PLATFORM_WINDOWS
#  include <malloc.h>
#else
#  ifdef OPEN_DYLAN_PLATFORM_FREEBSD
#     include <sys/types.h>
      extern void * alloca (size_t size);
#  else
#    include <alloca.h>
#  endif
#endif

#define primitive_stack_allocate(sz) ((D)(alloca((int)(sz) * sizeof(D))))

#define primitive_byte_stack_allocate(numwords, numbytes) \
  ((D)alloca(numwords * sizeof(D)) + (numbytes))

#define primitive_byte_stack_allocate_filled(size, class_wrapper, number_slots, fill_value, repeated_size, repeated_size_offset, repeated_fill_value) \
  initialize_byte_stack_allocate_filled((D*)primitive_byte_stack_allocate(size, repeated_size + 1), class_wrapper, number_slots, fill_value, repeated_size, repeated_size_offset, repeated_fill_value)

extern D initialize_byte_stack_allocate_filled
    (D ptr, D class_wrapper, DSINT number_slots,
     D fill_value, DSINT repeated_size, DSINT repeated_size_offset,
     DBYTE repeated_fill_value);


#define primitive_object_stack_allocate_filled(size, class_wrapper, number_slots, fill_value, repeated_size, repeated_size_offset, repeated_fill_value) \
  initialize_object_stack_allocate_filled(primitive_byte_stack_allocate(size, repeated_size * sizeof(D)), class_wrapper, number_slots, fill_value, repeated_size, repeated_size_offset, repeated_fill_value)

extern D initialize_object_stack_allocate_filled
      (D ptr, D class_wrapper, DSINT number_slots, D fill_value,
       DSINT repeated_size, DSINT repeated_size_offset,
       D repeated_fill_value);


/* allocation counting stubs     (gts,98sep10) */

#define primitive_initialize_allocation_count()
#define primitive_allocation_count() 0

#define primitive_begin_heap_alloc_stats()
#define primitive_end_heap_alloc_stats(x) 0

/* CLASS BREAKPOINT PRIMITIVES */

#define primitive_display_class_breakpoints(x) 0
#define primitive_clear_class_breakpoint(x)
#define primitive_set_class_breakpoint(x,y)

/* GC PRIMITIVES */

#define primitive_pin_object(x) (x)
extern void primitive_unpin_object(D);
extern void primitive_mps_finalize(D);
extern D primitive_mps_finalization_queue_first(void);
extern void primitive_mps_park(void);
extern void primitive_mps_clamp(void);
extern void primitive_mps_release(void);
extern void primitive_mps_collect(DBOOL);
extern DBOOL primitive_mps_collection_stats(D);
extern void primitive_mps_enable_gc_messages(void);
extern DSINT primitive_mps_committed(void);
extern void primitive_mps_begin_ramp_alloc(void);
extern void primitive_mps_end_ramp_alloc(void);
extern void primitive_mps_begin_ramp_alloc_all(void);
extern void primitive_mps_end_ramp_alloc_all(void);
extern void primitive_mps_ld_reset(void *hs);
extern void primitive_mps_ld_add(void *hs, void *o);
extern void primitive_mps_ld_merge(void *hs1, void *hs2);
extern int primitive_mps_ld_isstale(void *hs);

/* KEYBOARD BREAK HANDLING */

#define primitive_keyboard_interrupt_signaled() ((DBOOL)0)
#define primitive_keyboard_interrupt_signaled_setter(x)
#define primitive_keyboard_interrupt_polling() ((DBOOL)0)
#define primitive_keyboard_interrupt_polling_setter(x)
#define primitive_keyboard_interrupt_polling_thread(x) ((DBOOL)0)
#define primitive_keyboard_interrupt_polling_thread_setter(y, x)

/* BYTE CHARACTER PRIMITIVES */

#define primitive_byte_character_as_raw(x) \
   (R(x))
#define primitive_raw_as_byte_character(x) \
   (C(x))

/* UNICODE CHARACTER PRIMITIVES */

#define primitive_unicode_character_as_raw(x) \
   (R(x))
#define primitive_raw_as_unicode_character(x) \
   (U(x))

/* INTEGER PRIMITIVES */

/* SMALL-INTEGER PRIMITIVES */

#define primitive_raw_as_integer(x) \
   (I(x))

/* BIG-INTEGER PRIMITIVES */

/* MACHINE-INTEGER PRIMITIVES */

/* UNSIGNED-MACHINE-INTEGER PRIMITIVES */

/* ADDRESS PRIMITIVES */

/* POINTER PRIMITIVES */

#define primitive_cast_pointer_as_raw(x)  ((DADDR)x)
#define primitive_cast_raw_as_pointer(x)  ((D)x)

/* MACHINE-WORD PRIMITIVES */

#define primitive_integerQ(x)                             RAWASBOOL(TAG_BITS(x) == 1)

#define primitive_machine_word_equalQ(x, y)               RAWASBOOL((DMINT)(x) == (DMINT)(y))
#define primitive_machine_word_not_equalQ(x, y)           RAWASBOOL((DMINT)(x) != (DMINT)(y))
#define primitive_machine_word_less_thanQ(x, y)           RAWASBOOL((DMINT)(x) < (DMINT)(y))
#define primitive_machine_word_not_less_thanQ(x, y)       RAWASBOOL((DMINT)(x) >= (DMINT)(y))
#define primitive_machine_word_greater_thanQ(x, y)        RAWASBOOL((DMINT)(x) > (DMINT)(y))
#define primitive_machine_word_not_greater_thanQ(x, y)    RAWASBOOL((DMINT)(x) <= (DMINT)(y))

extern D primitive_wrap_machine_word(DMINT);

#define primitive_unwrap_machine_word(x) \
  (((DMI)(x))->data)

extern D primitive_wrap_c_pointer(D, DMINT);
/*   primitive_unwrap_c_pointer
 * assumes that instances of <c-pointer> are implemented as
 * struct {D class; D pointer_address; ...}
 * ..and that the address is stored as a raw pointer.
 */
#define primitive_unwrap_c_pointer(c_ptr) \
  ((void*)(*(((D*)(c_ptr))+1)))

#define primitive_cast_integer_as_raw(x)                  ((DMINT)(x))
#define primitive_cast_raw_as_integer(x)                  ((D)(x))

extern D primitive_wrap_abstract_integer(DMINT);
extern D primitive_wrap_unsigned_abstract_integer(DMINT);
extern DMINT primitive_unwrap_abstract_integer(D);
#define primitive_box_integer(x)                          (I(x))
#define primitive_unbox_integer(x)                        (R(x))

#define primitive_machine_word_logand(x, y)               ((x) & (y))
#define primitive_machine_word_logior(x, y)               ((x) | (y))
#define primitive_machine_word_logxor(x, y)               ((x) ^ (y))

#define primitive_machine_word_lognot(x)                  (~(x))

#define primitive_machine_word_logbitQ(i, x)              RAWASBOOL((1UL << (i)) & ((DUMINT)(x)))
#define primitive_machine_word_logbit_set(i, x)           ((1UL << (i)) | ((DUMINT)(x)))
#define primitive_machine_word_logbit_clear(i, x)         (~(1UL << (i)) & ((DUMINT)(x)))

#define primitive_machine_word_bit_field_deposit(f, o, s, x)      (((x) & ~(((1 << (s)) - 1) << (o))) | ((f) << (o)))
#define primitive_machine_word_bit_field_extract(o, s, x)         (((x) & (((1 << (s)) - 1) << (o))) >> (o))

#ifdef OPEN_DYLAN_COMPILER_GCC_LIKE
#define primitive_machine_word_count_low_zeros(x) __builtin_ctzl(x)
#define primitive_machine_word_count_high_zeros(x) __builtin_clzl(x)
#else
extern DMINT primitive_machine_word_count_low_zeros(DMINT);
extern DMINT primitive_machine_word_count_high_zeros(DMINT);
#endif

#define primitive_machine_word_add(x, y)                  ((x) + (y))

extern DMINT primitive_machine_word_add_with_overflow(DMINT, DMINT);

#define primitive_machine_word_subtract(x, y)             ((x) - (y))

extern DMINT primitive_machine_word_subtract_with_overflow(DMINT, DMINT);

#define primitive_machine_word_multiply_low(x, y)         ((x) * (y))

extern DMINT primitive_machine_word_multiply_high(DMINT, DMINT);
extern DMINT primitive_machine_word_multiply_lowShigh(DMINT, DMINT);
extern DMINT primitive_machine_word_multiply_low_with_overflow(DMINT, DMINT);
extern DMINT primitive_machine_word_multiply_with_overflow(DMINT, DMINT);

#define primitive_machine_word_negative(x)                (-(signed)(x))
#define primitive_machine_word_abs(x)                     ((x)<0?-(x):(x))

extern DMINT primitive_machine_word_negative_with_overflow(DMINT);
extern DMINT primitive_machine_word_abs_with_overflow(DMINT);

extern DMINT primitive_machine_word_floorS_quotient(DMINT, DMINT);
extern DMINT primitive_machine_word_floorS_remainder(DMINT, DMINT);
extern DMINT primitive_machine_word_floorS(DMINT, DMINT);
extern DMINT primitive_machine_word_ceilingS_quotient(DMINT, DMINT);
extern DMINT primitive_machine_word_ceilingS_remainder(DMINT, DMINT);
extern DMINT primitive_machine_word_ceilingS(DMINT, DMINT);
extern DMINT primitive_machine_word_roundS_quotient(DMINT, DMINT);
extern DMINT primitive_machine_word_roundS_remainder(DMINT, DMINT);
extern DMINT primitive_machine_word_roundS(DMINT, DMINT);
extern DMINT primitive_machine_word_truncateS_quotient(DMINT, DMINT);
extern DMINT primitive_machine_word_truncateS_remainder(DMINT, DMINT);
extern DMINT primitive_machine_word_truncateS(DMINT, DMINT);

#define primitive_machine_word_divide_quotient(x, y)      ((x) / (y))
#define primitive_machine_word_divide_remainder(x, y)     ((x) % (y))

extern DMINT primitive_machine_word_divide(DMINT, DMINT);

#define primitive_machine_word_shift_left_low(x, i)       ((DMINT)(x) << (i))

extern DMINT primitive_machine_word_shift_left_high(DMINT, DMINT);
extern DMINT primitive_machine_word_shift_left_lowShigh(DMINT, DMINT);
extern DMINT primitive_machine_word_shift_left_low_with_overflow(DMINT, DMINT);
extern DMINT primitive_machine_word_shift_left_with_overflow(DMINT, DMINT);

#define primitive_machine_word_shift_right(x, i)          ((DMINT)(x) >> (i))

/* The C run-time does no overflow checks ... */

#define primitive_machine_word_add_signal_overflow(x, y) \
  primitive_machine_word_add(x, y)
#define primitive_machine_word_subtract_signal_overflow(x, y) \
  primitive_machine_word_subtract(x, y)
#define primitive_machine_word_multiply_signal_overflow(x, y) \
  primitive_machine_word_multiply_low(x, y)
#define primitive_machine_word_negative_signal_overflow(x) \
  primitive_machine_word_negative(x)
#define primitive_machine_word_abs_signal_overflow(x) \
  primitive_machine_word_abs(x)
#define primitive_machine_word_shift_left_signal_overflow(x, y) \
  primitive_machine_word_shift_left_low(x, y)

extern DMINT primitive_machine_word_double_floorS_quotient(DMINT, DMINT, DMINT);
extern DMINT primitive_machine_word_double_floorS_remainder(DMINT, DMINT, DMINT);
extern DMINT primitive_machine_word_double_floorS(DMINT, DMINT, DMINT);
extern DMINT primitive_machine_word_double_ceilingS_quotient(DMINT, DMINT, DMINT);
extern DMINT primitive_machine_word_double_ceilingS_remainder(DMINT, DMINT, DMINT);
extern DMINT primitive_machine_word_double_ceilingS(DMINT, DMINT, DMINT);
extern DMINT primitive_machine_word_double_roundS_quotient(DMINT, DMINT, DMINT);
extern DMINT primitive_machine_word_double_roundS_remainder(DMINT, DMINT, DMINT);
extern DMINT primitive_machine_word_double_roundS(DMINT, DMINT, DMINT);
extern DMINT primitive_machine_word_double_truncateS_quotient(DMINT, DMINT, DMINT);
extern DMINT primitive_machine_word_double_truncateS_remainder(DMINT, DMINT, DMINT);
extern DMINT primitive_machine_word_double_truncateS(DMINT, DMINT, DMINT);
extern DMINT primitive_machine_word_double_divide_quotient(DMINT, DMINT, DMINT);
extern DMINT primitive_machine_word_double_divide_remainder(DMINT, DMINT, DMINT);
extern DMINT primitive_machine_word_double_divide(DMINT, DMINT, DMINT);

#define primitive_machine_word_unsigned_less_thanQ(x, y) \
  RAWASBOOL(((DUMINT)(x)) < ((DUMINT)(y)))

#define primitive_machine_word_unsigned_not_less_thanQ(x, y) \
  RAWASBOOL(((DUMINT)(x)) >= ((DUMINT)(y)))

#define primitive_machine_word_unsigned_greater_thanQ(x, y) \
  RAWASBOOL(((DUMINT)(x)) > ((DUMINT)(y)))

#define primitive_machine_word_unsigned_not_greater_thanQ(x, y) \
  RAWASBOOL(((DUMINT)(x)) <= ((DUMINT)(y)))

extern DMINT primitive_machine_word_unsigned_add_with_carry(DMINT, DMINT);
extern DMINT primitive_machine_word_unsigned_subtract_with_borrow(DMINT, DMINT);
extern DMINT primitive_machine_word_unsigned_multiply_high(DMINT, DMINT);
extern DMINT primitive_machine_word_unsigned_multiply(DMINT, DMINT);

#define primitive_machine_word_unsigned_divide_quotient(x, y) \
  ((DMINT)(((DUMINT)(x)) / ((DUMINT)(y))))

#define primitive_machine_word_unsigned_divide_remainder(x, y) \
  ((DMINT)(((DUMINT)(x)) % ((DUMINT)(y))))

extern DMINT primitive_machine_word_unsigned_divide(DMINT, DMINT);

extern DMINT primitive_machine_word_unsigned_rotate_left(DMINT, DMINT);
extern DMINT primitive_machine_word_unsigned_rotate_right(DMINT, DMINT);

#define primitive_machine_word_unsigned_shift_right(x, i) \
  ((DMINT)(((DUMINT)(x)) >> (i)))

extern DMINT primitive_machine_word_unsigned_double_divide_quotient(DMINT, DMINT, DMINT);
extern DMINT primitive_machine_word_unsigned_double_divide_remainder(DMINT, DMINT, DMINT);
extern DMINT primitive_machine_word_unsigned_double_divide(DMINT, DMINT, DMINT);

extern DMINT primitive_machine_word_unsigned_shift_left_high(DMINT, DMINT);
extern DMINT primitive_machine_word_unsigned_double_shift_left_high(DMINT, DMINT, DMINT);
extern DMINT primitive_machine_word_unsigned_double_shift_left(DMINT, DMINT, DMINT);

extern DMINT primitive_machine_word_unsigned_double_shift_right_low(DMINT, DMINT, DMINT);
extern DMINT primitive_machine_word_unsigned_double_shift_right_high(DMINT, DMINT, DMINT);
extern DMINT primitive_machine_word_unsigned_double_shift_right(DMINT, DMINT, DMINT);

/* FLOATS */
#include <math.h>

/* Win32 only defines the single precision functions for C++ (Huh?) */
#ifdef OPEN_DYLAN_PLATFORM_WINDOWS
#define sqrtf(x)  (DSFLT)sqrt((DDFLT)x)
#define logf(x)   (DSFLT)log((DDFLT)x)
#define expf(x)   (DSFLT)exp((DDFLT)x)
#define powf(b,p) (DSFLT)pow((DDFLT)b,(DDFLT)p)
#define sinf(x)   (DSFLT)sin((DDFLT)x)
#define cosf(x)   (DSFLT)cos((DDFLT)x)
#define tanf(x)   (DSFLT)tan((DDFLT)x)
#define asinf(x)  (DSFLT)asin((DDFLT)x)
#define acosf(x)  (DSFLT)acos((DDFLT)x)
#define atanf(x)  (DSFLT)atan((DDFLT)x)
#endif

#define primitive_double_float_as_single(x) ((DSFLT)x)
#define primitive_single_float_as_double(x) ((DDFLT)x)

/* SINGLE-FLOAT PRIMITIVES */

#define primitive_single_float_as_raw(x) \
    (((DSF)x)->data)
extern D primitive_raw_as_single_float(DSFLT x);
#define primitive_single_float_as_integer(x) \
    ((DSINT)(x))
#define primitive_integer_as_single_float(x) \
    ((DSFLT)(x))
extern DMINT primitive_single_float_as_double_integer(DSFLT);
extern DSFLT primitive_double_integer_as_single_float(DMINT, DMINT);
extern DUMINT primitive_cast_single_float_as_machine_word(DSFLT);
extern DSFLT primitive_cast_machine_word_as_single_float(DUMINT);
#define primitive_single_float_negate(x) \
    (-(x))
#define primitive_single_float_add(x, y) \
    ((x) + (y))
#define primitive_single_float_subtract(x, y) \
    ((x) - (y))
#define primitive_single_float_multiply(x, y) \
    ((x) * (y))
#define primitive_single_float_divide(x, y) \
    ((x) / (y))
#define primitive_single_float_equalsQ(x, y) \
    RAWASBOOL((x) == (y))
#define primitive_single_float_less_thanQ(x, y) \
    RAWASBOOL((x) < (y))
#define primitive_single_float_sqrt(x) \
    (sqrtf(x))
#define primitive_single_float_log(x) \
    (logf(x))
#define primitive_single_float_exp(x) \
    (expf(x))
#define primitive_single_float_expt(base, power) \
    (powf(base, power))
#define primitive_single_float_sin(x) \
    (sinf(x))
#define primitive_single_float_cos(x) \
    (cosf(x))
#define primitive_single_float_tan(x) \
    (tanf(x))
#define primitive_single_float_asin(x) \
    (asinf(x))
#define primitive_single_float_acos(x) \
    (acosf(x))
#define primitive_single_float_atan(x) \
    (atanf(x))

/* DOUBLE-FLOAT PRIMITIVES */

#define primitive_double_float_as_raw(x) \
    (((DDF)x)->data)
extern D primitive_raw_as_double_float(DDFLT x);
#define primitive_double_float_as_integer(x) \
    ((DSINT)(x))
#define primitive_integer_as_double_float(x) \
    ((DDFLT)(x))
extern DMINT primitive_double_float_as_double_integer(DDFLT);
extern DDFLT primitive_double_integer_as_double_float(DMINT, DMINT);
extern DUMINT primitive_cast_double_float_as_machine_words(DDFLT);
extern DDFLT primitive_cast_machine_words_as_double_float(DUMINT, DUMINT);
#define primitive_double_float_negate(x) \
    (-(x))
#define primitive_double_float_add(x, y) \
    ((x) + (y))
#define primitive_double_float_subtract(x, y) \
    ((x) - (y))
#define primitive_double_float_multiply(x, y) \
    ((x) * (y))
#define primitive_double_float_divide(x, y) \
    ((x) / (y))
#define primitive_double_float_equalsQ(x, y) \
    RAWASBOOL((x) == (y))
#define primitive_double_float_less_thanQ(x, y) \
    RAWASBOOL((x) < (y))
#define primitive_double_float_sqrt(x) \
    (sqrt(x))
#define primitive_double_float_log(x) \
    (log(x))
#define primitive_double_float_exp(x) \
    (exp(x))
#define primitive_double_float_expt(base, power) \
    (pow(base, power))
#define primitive_double_float_sin(x) \
    (sin(x))
#define primitive_double_float_cos(x) \
    (cos(x))
#define primitive_double_float_tan(x) \
    (tan(x))
#define primitive_double_float_asin(x) \
    (asin(x))
#define primitive_double_float_acos(x) \
    (acos(x))
#define primitive_double_float_atan(x) \
    (atan(x))

/* VECTOR PRIMITIVES */

#define primitive_vector_element(v, i)           ((((SOV*)v)->data)[R(i)])
#define primitive_vector_element_setter(e, v, i) ((((SOV*)v)->data)[R(i)] = (e))
#define primitive_vector_size(v)                 (((SOV*)v)->size)
#define primitive_vector_as_raw(v)               (((SOV*)v)->data)

extern D primitive_vector(D dn, ...);
extern D primitive_raw_as_vector(D a, D n);
extern D primitive_make_vector(int size);
extern D primitive_copy_vector(D vector);
extern D VECTOR_REF_OR_F(D v, int offset);

/* STRING PRIMITIVES */

#define primitive_strlen(s)                     (strlen((DBSTR)s))
#define primitive_string_as_raw(v)              (((BS*)v)->data)
extern D primitive_raw_as_string(DBSTR buffer);

/* CALLING CONVENTION PRIMITIVES */

#define primitive_function_parameter()                  ((D)(get_teb()->function))
#define primitive_next_methods_parameter()              ((D)(get_teb()->next_methods))

/* APPLY PRIMITIVES */

extern D primitive_apply (D fn, D sov);
extern D primitive_apply_spread (D fn, int n, ...);
extern D primitive_mep_apply_spread (D fn, D nm, int n, ...);
extern D primitive_xep_apply (FN* fn, int n, D a[]);
extern D primitive_mep_apply_with_optionals
  (FN* fn, D new_next_methods, D a);
extern D primitive_mep_apply (FN* fn, D next_methods, D a[]);
extern D primitive_iep_apply (FN* fn, int n, D a[]);
extern D primitive_engine_node_apply_with_optionals (D engD, D gfD, D args);

#define APPLYN(fn,n) (D)(primitive_apply_spread((fn),n
#define APPLY0(fn) APPLYN(fn,0)))
#define APPLY1(fn,a1) APPLYN(fn,1),(a1)))
#define APPLY2(fn,a1,a2) APPLYN(fn,2),(a1),(a2)))
#define APPLY3(fn,a1,a2,a3) APPLYN(fn,3),(a1),(a2),(a3)))
#define APPLY4(fn,a1,a2,a3,a4) APPLYN(fn,4),(a1),(a2),(a3),(a4)))
#define APPLY5(fn,a1,a2,a3,a4,a5) APPLYN(fn,5),(a1),(a2),(a3),(a4),(a5)))
#define APPLY6(fn,a1,a2,a3,a4,a5,a6) APPLYN(fn,6),(a1),(a2),(a3),(a4),(a5),(a6)))
#define APPLY7(fn,a1,a2,a3,a4,a5,a6,a7) APPLYN(fn,7),(a1),(a2),(a3),(a4),(a5),(a6),(a7)))
#define APPLY8(fn,a1,a2,a3,a4,a5,a6,a7,a8) APPLYN(fn,8),(a1),(a2),(a3),(a4),(a5),(a6),(a7),(a8)))
#define APPLY9(fn,a1,a2,a3,a4,a5,a6,a7,a8,a9) APPLYN(fn,9),(a1),(a2),(a3),(a4),(a5),(a6),(a7),(a8),(a9)))
#define APPLY10(fn,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) APPLYN(fn,10),(a1),(a2),(a3),(a4),(a5),(a6),(a7),(a8),(a9),(a10)))

#define MEP_APPLYN(fn,nm,n) (D)(primitive_mep_apply_spread((fn),(nm),n
#define MEP_APPLY0(fn,nm) MEP_APPLYN(fn,nm,0)))
#define MEP_APPLY1(fn,nm,a1) MEP_APPLYN(fn,nm,1),(a1)))
#define MEP_APPLY2(fn,nm,a1,a2) MEP_APPLYN(fn,nm,2),(a1),(a2)))
#define MEP_APPLY3(fn,nm,a1,a2,a3) MEP_APPLYN(fn,nm,3),(a1),(a2),(a3)))
#define MEP_APPLY4(fn,nm,a1,a2,a3,a4) MEP_APPLYN(fn,nm,4),(a1),(a2),(a3),(a4)))
#define MEP_APPLY5(fn,nm,a1,a2,a3,a4,a5) MEP_APPLYN(fn,nm,5),(a1),(a2),(a3),(a4),(a5)))
#define MEP_APPLY6(fn,nm,a1,a2,a3,a4,a5,a6) MEP_APPLYN(fn,nm,6),(a1),(a2),(a3),(a4),(a5),(a6)))
#define MEP_APPLY7(fn,nm,a1,a2,a3,a4,a5,a6,a7) MEP_APPLYN(fn,nm,7),(a1),(a2),(a3),(a4),(a5),(a6),(a7)))
#define MEP_APPLY8(fn,nm,a1,a2,a3,a4,a5,a6,a7,a8) MEP_APPLYN(fn,nm,8),(a1),(a2),(a3),(a4),(a5),(a6),(a7),(a8)))
#define MEP_APPLY9(fn,nm,a1,a2,a3,a4,a5,a6,a7,a8,a9) MEP_APPLYN(fn,nm,9),(a1),(a2),(a3),(a4),(a5),(a6),(a7),(a8),(a9)))
#define MEP_APPLY10(fn,nm,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) MEP_APPLYN(fn,nm,10),(a1),(a2),(a3),(a4),(a5),(a6),(a7),(a8),(a9),(a10)))


#define ENGINE_NODE_APPLYN(e,gf,n) (D)(primitive_engine_node_apply_spread((e),(gf),n


/* VALUES PRIMITIVES */

extern D primitive_values (D v);

/* OPERATING SYSTEM PRIMITIVES */

extern D Tcommand_nameT;
extern D pseudo_primitive_command_name (void);

extern D Tcommand_argumentsT;
extern D pseudo_primitive_command_arguments (void);

extern void  primitive_exit_application (DSINT code) NORETURN_FUNCTION;

/* TEMPORARY PRIMITIVES FOR ASSIGNMENT */

extern D MAKE_D_CELL(D);
#define GET_D_CELL_VAL(c)       (*(D*)c)
#define SET_D_CELL_VAL(c, v)    (*(D*)c = v)

extern D MAKE_DBCHR_CELL(DBCHR);
#define GET_DBCHR_CELL_VAL(c)           (*(DBCHR*)c)
#define SET_DBCHR_CELL_VAL(c, v)        (*(DBCHR*)c = v)

extern D MAKE_DDBYTE_CELL(DDBYTE);
#define GET_DDBYTE_CELL_VAL(c)          (*(DDBYTE*)c)
#define SET_DDBYTE_CELL_VAL(c, v)       (*(DDBYTE*)c = v)

extern D MAKE_DWORD_CELL(DWORD);
#define GET_DWORD_CELL_VAL(c)           (*(DWORD*)c)
#define SET_DWORD_CELL_VAL(c, v)        (*(DWORD*)c = v)

extern D MAKE_DDWORD_CELL(DDWORD);
#define GET_DDWORD_CELL_VAL(c)          (*(DDWORD*)c)
#define SET_DDWORD_CELL_VAL(c, v)       (*(DDWORD*)c = v)

extern D MAKE_DSFLT_CELL(DSFLT);
#define GET_DSFLT_CELL_VAL(c)           (*(DSFLT*)c)
#define SET_DSFLT_CELL_VAL(c, v)        (*(DSFLT*)c = v)

extern D MAKE_DDFLT_CELL(DDFLT);
#define GET_DDFLT_CELL_VAL(c)           (*(DDFLT*)c)
#define SET_DDFLT_CELL_VAL(c, v)        (*(DDFLT*)c = v)

/* THREAD SUPPORT */

extern void initialize_threads_primitives(void);

extern D primitive_release_simple_lock(D l);
extern D primitive_release_semaphore(D s);
extern D primitive_owned_recursive_lock(D l);
extern D primitive_destroy_simple_lock(D l);
extern D primitive_wait_for_semaphore_timed(D s, D ms);
extern D primitive_wait_for_semaphore(D s);
extern D primitive_wait_for_simple_lock_timed(D l, D ms);
extern D primitive_wait_for_simple_lock(D l);
extern D primitive_make_recursive_lock(D l, D n);
extern D primitive_release_recursive_lock(D l);
extern D primitive_make_semaphore(D l, D n, D i, D m);
extern D primitive_destroy_recursive_lock(D l);
extern D primitive_owned_simple_lock(D l);
extern D primitive_destroy_semaphore(D l);
extern D primitive_wait_for_recursive_lock_timed(D l, D ms);
extern D primitive_wait_for_recursive_lock(D l);
extern D primitive_thread_join_multiple(D v);
extern D primitive_thread_join_single(D t);
extern D primitive_initialize_current_thread(D t, DBOOL s);
extern D primitive_initialize_special_thread(D t);
extern D primitive_current_thread(void);
extern D primitive_make_thread(D t, D n, D p, D f, DBOOL s);
extern D primitive_destroy_thread(D t);
extern D primitive_destroy_notification(D n);
extern D primitive_release_all_notification(D n, D l);
extern D primitive_make_notification(D l, D n);
extern D primitive_wait_for_notification_timed(D n, D l, D ms);
extern D primitive_wait_for_notification(D n, D l);
extern D primitive_release_notification(D n, D l);
extern void primitive_detach_thread(D t);
extern void primitive_thread_yield(void);
extern void primitive_sleep(D ms);
extern D primitive_make_simple_lock(D l, D n);
extern D primitive_allocate_thread_variable(D i);
extern D primitive_read_thread_variable(D h);
extern D primitive_write_thread_variable(D h, D nv);
extern D primitive_unlock_simple_lock(D l);
extern D primitive_unlock_recursive_lock(D l);

/* ATOMIC PRIMITIVES */

#define primitive_sequence_point() SEQUENCE_POINT()
#define primitive_synchronize_side_effects() SYNCHRONIZE_SIDE_EFFECTS()

/* RUN-TIME CALLBACKS */

extern D primitive_resolve_symbol (D symbol);
extern D primitive_string_as_symbol (D string);
extern D primitive_preboot_symbols ();

/* ENGINE NODE HANDLER ASSIGNMENTS */

#define ENGINE_absent 0
#define ENGINE_inapplicable 1
#define ENGINE_unkeyed_single_method 2
#define ENGINE_implicit_keyed_single_method 3
#define ENGINE_explicit_keyed_single_method 4
#define ENGINE_unrestricted_keyed_single_method 5
#define ENGINE_reserved_terminal_n_a 6
#define ENGINE_reserved_terminal_n_b 7
#define ENGINE_reserved_terminal_n_c 8
#define ENGINE_reserved_terminal_n_d 9
#define ENGINE_reserved_terminal_n_e 10
#define ENGINE_reserved_terminal_n_f 11
#define ENGINE_reserved_terminal_n_g 12
#define ENGINE_profiling_cache_header 13
#define ENGINE_cache_header 14
#define ENGINE_ambiguous_methods 15
#define ENGINE_boxed_instance_slot_getter 16
#define ENGINE_boxed_instance_slot_setter 17
#define ENGINE_boxed_repeated_instance_slot_getter 18
#define ENGINE_boxed_repeated_instance_slot_setter 19
#define ENGINE_boxed_class_slot_getter 20
#define ENGINE_boxed_class_slot_setter 21
#define ENGINE_raw_byte_repeated_instance_slot_getter 22
#define ENGINE_raw_byte_repeated_instance_slot_setter 23
#define ENGINE_reserved_slot_a_getter 24
#define ENGINE_reserved_slot_a_setter 25
#define ENGINE_reserved_slot_b_getter 26
#define ENGINE_reserved_slot_b_setter 27
#define ENGINE_reserved_repeated_slot_a_getter 28
#define ENGINE_reserved_repeated_slot_a_setter 29
#define ENGINE_reserved_repeated_slot_b_getter 30
#define ENGINE_reserved_repeated_slot_b_setter 31
#define ENGINE_typecheck 32
#define ENGINE_if_type 33
#define ENGINE_linear_by_class 34
#define ENGINE_hashed_by_class 35
#define ENGINE_linear_by_singleton_class 36
#define ENGINE_hashed_by_singleton_class 37
#define ENGINE_immediate_linear_singleton 38
#define ENGINE_immediate_hashed_noreloc_singleton 39
#define ENGINE_immediate_hashed_singleton 40
#define ENGINE_value_object_linear_singleton 41
#define ENGINE_value_object_hashed_singleton 42
#define ENGINE_monomorphic 42
#define ENGINE_reserved_discriminator_a 43
#define ENGINE_reserved_discriminator_b 44
#define ENGINE_reserved_discriminator_c 45
#define ENGINE_reserved_discriminator_d 46
#define ENGINE_reserved_discriminator_e 47
#define ENGINE_reserved_discriminator_f 48
#define ENGINE_reserved_discriminator_g 49
#define ENGINE_reserved_discriminator_h 50
#define ENGINE_reserved_discriminator_i 51
#define ENGINE_reserved_discriminator_j 52
#define ENGINE_reserved_discriminator_k 53
#define ENGINE_reserved_discriminator_l 54
#define ENGINE_reserved_discriminator_m 55
#define ENGINE_reserved_discriminator_n 56
#define ENGINE_reserved_discriminator_o 57
#define ENGINE_reserved_discriminator_p 58
#define ENGINE_reserved_discriminator_q 59
#define ENGINE_reserved_discriminator_r 60
#define ENGINE_reserved_discriminator_s 61
#define ENGINE_reserved_discriminator_t 62
#define ENGINE_reserved_discriminator_u 63

#define ENGINE_first_discriminator 32
#define ENGINE_first_slot 16
#define ENGINE_number_slot_handlers 16

/* ENGINE NODE PROPERTIES BIT DEFINITIONS

There used to be two other bits having to do with permanency and precomputation or
something (I've forgotten!) which want to be in all engine-nodes at some point in
the future.  Adding them will be more convenient when the properties word is made
raw, and we can use the tag bits.


Engine Node
Entry-type is contained in low byte, shifted 2:  mask or shift out low 2 bits.

_31_________________________________________________________8_7____________2_1___________0_
|                             other                          |  entry type  |  fixnum tag  |
-------------------------------------------------------------------------------------------


Discriminator
Argument number to discriminate on is contained in second byte.
Third byte is the number of required arguments, and the following bit indicates whether
there are any optionals.  The sum of that byte and the bit give the number of MEP-style
arguments, which may be of use to primitive-initialize-discriminator.


_31_____25____24_____23___________16_15____________________8_7____________2_1___________0_
|  other  |  restp  |   nrequired   |  discriminator argnum  |  entry type  |  fixnum tag  |
-------------------------------------------------------------------------------------------


 */

#define EPROPS_V_ENTRY_TYPE 2
#define EPROPS_S_ENTRY_TYPE 6
#define EPROPS_M_ENTRY_TYPE 0xfc
#define ENODE_V_DATA_START 16
#define DPROPS_V_ARGNUM 8
#define DPROPS_S_ARGNUM 8
#define DPROPS_M_ARGNUM 0xFF00
#define DPROPS_V_NREQUIRED 16
#define DPROPS_S_NREQUIRED 8
#define DPROPS_M_NREQUIRED 0xFF0000
#define DPROPS_V_OPTIONALS 24
#define DPROPS_M_OPTIONALS 0x1000000

/* <simple-typechecked-cache-header-engine-node>.  See the Dylan definitions in
   dispatch-prologue.dylan for how these are defined.
   */
#define STCHEN_ARGUMENTS_LIMIT 8
#define STCHEN_V_CHECKEDMASK 16
#define STCHEN_S_CHECKEDMASK 8
#define STCHEN_M_CHECKEDMASK 0xff0000


#define SLOTENGINE_V_INDEX ENODE_V_DATA_START

extern D general_engine_node_1_engine (D a1);
extern D general_engine_node_2_engine (D a1, D a2);

extern D general_engine_node_3_engine (D a1, D a2, D a3);
extern D general_engine_node_n_engine (D a1, ...);
extern D general_engine_node_spread_engine (D a1, ...);

extern D cache_header_engine_0 ();
extern D cache_header_engine_1 (D a1);
extern D cache_header_engine_2 (D a1, D a2);
extern D cache_header_engine_3 (D a1, D a2, D a3);
extern D cache_header_engine_4 (D a1, D a2, D a3, D a4);
extern D cache_header_engine_5 (D a1, D a2, D a3, D a4, D a5);
extern D cache_header_engine_6 (D a1, D a2, D a3, D a4, D a5, D a6);
extern D cache_header_engine_7 (D a1, D a2, D a3, D a4, D a5, D a6, D a7);
extern D cache_header_engine_n (D argvec);


/* additions to run-time.c specific to handling pass-by-reference of non-first
   return values of primitives  (gts,9/97) */

extern DMINT primitive_single_float_as_double_integer_byref(DSFLT f, DMINT* v2);
extern DMINT primitive_double_float_as_double_integer_byref(DDFLT f, DMINT* v2);
extern DMINT primitive_cast_double_float_as_machine_words_byref(DDFLT x, DMINT* v2);
extern DMINT primitive_machine_word_divide_byref(DMINT x, DMINT y, DMINT* v2);
extern DMINT primitive_machine_word_floorS_byref(DMINT x, DMINT y, DMINT* v2);
extern DMINT primitive_machine_word_ceilingS_byref(DMINT x, DMINT y, DMINT* v2);
extern DMINT primitive_machine_word_roundS_byref(DMINT x, DMINT y, DMINT* v2);
extern DMINT primitive_machine_word_truncateS_byref(DMINT x, DMINT y, DMINT* v2);
extern DMINT primitive_machine_word_double_floorS_byref(DMINT xl, DMINT xh, DMINT y, DMINT* v2);
extern DMINT primitive_machine_word_double_ceilingS_byref(DMINT xl, DMINT xh, DMINT y, DMINT* v2);
extern DMINT primitive_machine_word_double_roundS_byref(DMINT xl, DMINT xh, DMINT y, DMINT* v2);
extern DMINT primitive_machine_word_double_truncateS_byref(DMINT xl, DMINT xh, DMINT y, DMINT* v2);
extern DMINT primitive_machine_word_double_divide_byref(DMINT xl, DMINT xh, DMINT y, DMINT* v2);
extern DMINT primitive_machine_word_add_with_overflow_byref(DMINT x, DMINT y, D* v2);
extern DMINT primitive_machine_word_subtract_with_overflow_byref(DMINT x, DMINT y, D* v2);
extern DMINT primitive_machine_word_multiply_with_overflow_byref(DMINT x, DMINT y, DMINT* v2, D* v3);
extern DMINT primitive_machine_word_negative_with_overflow_byref(DMINT x, D* v2);
extern DMINT primitive_machine_word_abs_with_overflow_byref(DMINT x, D* v2);
extern DMINT primitive_machine_word_shift_left_with_overflow_byref(DMINT x, DMINT y, DMINT* v2, D* v3);
extern DMINT primitive_machine_word_multiply_lowShigh_byref(DMINT x, DMINT y, DMINT* v2);
extern DMINT primitive_machine_word_multiply_low_with_overflow_byref(DMINT x, DMINT y, D* v2);
extern DMINT primitive_machine_word_unsigned_add_with_carry_byref(DMINT x, DMINT y, DMINT* v2);
extern DMINT primitive_machine_word_unsigned_subtract_with_borrow_byref(DMINT x, DMINT y, DMINT* v2);
extern DMINT primitive_machine_word_unsigned_multiply_byref(DMINT x, DMINT y, DMINT* v2);
extern DMINT primitive_machine_word_unsigned_divide_byref(DMINT x, DMINT y, DMINT* v2);
extern DMINT primitive_machine_word_unsigned_double_divide_byref(DMINT xl, DMINT xh, DMINT y, DMINT* v2);
extern DMINT primitive_machine_word_unsigned_double_shift_left_byref(DMINT xl, DMINT xh, DMINT y, DMINT* v2);
extern DMINT primitive_machine_word_unsigned_double_shift_right_byref(DMINT xl, DMINT xh, DMINT y, DMINT* v2);

#endif /* OPENDYLAN_CRT_RUNTIME_H */

/* eof */
