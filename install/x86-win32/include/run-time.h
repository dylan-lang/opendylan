#include <setjmp.h>

#define OPTIONAL_ARGUMENT_CHECK(fn, req, count)
#define REQUIRED_ARGUMENT_CHECK(fn, req, count)

/* CONCRETE RAW TYPES */

typedef signed char		INT8;
typedef unsigned char		UINT8;
typedef signed short		INT16;
typedef unsigned short		UINT16;
typedef signed long		INT32;
typedef unsigned long		UINT32;
#if defined(WIN32) || defined(macintosh)
typedef double	                INT64; /* Cleary wrong, but the right size. */
typedef double	                UINT64;
#else
typedef signed long long	INT64;
typedef unsigned long long	UINT64;
#endif
typedef float			FLT;
typedef double			DFLT;
#ifdef WIN32
typedef double			EFLT;
#else
typedef long double		EFLT;
#endif

/* C RAW TYPES */

typedef INT64			long_long;
typedef UINT64			unsigned_long_long;
typedef EFLT		        long_double;

/* DYLAN RAW TYPES */

typedef int			DBOOL;
typedef unsigned char		DBCHR;
typedef long			DSINT;
typedef long			DMINT;
typedef DMINT			DWORD;
typedef unsigned long		DUMINT;
typedef INT64			DLMINT;
typedef UINT64			DULMINT;
/* typedef INT64			DBINT; */
typedef DMINT			DBINT; /* TEMPORARY */
typedef float			DSFLT;
typedef double			DDFLT;
typedef long double		DEFLT;
typedef unsigned long		DADDR;
typedef char*			DBSTR;
typedef void* 			D;

/* BASIC DYLAN TYPES */

typedef D (*DFN)(D,int,...);
typedef D (*DLFN)();

typedef struct _obj {
  D class;
  D slots[1];
} OBJECT;

typedef struct _dsf_ {
  D     class;
  DSFLT data;
} DSF_;

typedef struct _dmi_ {
  D     class;
  DMINT data;
} DMI_;

typedef struct _dumi_ {
  D      class;
  DUMINT data;
} DUMI_;

typedef struct _dbi_ {
  D     class;
  DBINT data;
} DBI_;

typedef struct _Wrapper {
  D        wrapper_wrapper;
  D        class;
  DMINT    fixed_part;
  DMINT    variable_part;
  D        number_patterns;
  DMINT    patterns[1]; /* REPEATED */        
} Wrapper;

typedef DSF_*  DSF;
typedef DMI_*  DMI;
typedef DUMI_* DUMI;
typedef DBI_*  DBI;

#define define_SOV(_name, _size) \
  typedef struct _sov##_name { \
    D class; \
    D size; \
    D data[_size]; \
  } _name;

define_SOV(SOV, 1);

#define STACK_DATA_SIZE 16
#define STACK_SOV_SIZE STACK_DATA_SIZE

define_SOV(STACK_SOV, STACK_SOV_SIZE) 

#define define_byte_string(_name, _size) \
  typedef struct _bs##_name { \
    D class; \
    D size; \
    char data[_size + 1]; \
  } _name;

define_byte_string(BS, 0);

typedef struct _symbol {
  D class;
  D name;
} SYMBOL;

typedef struct _fn {
  D    class;
  D    debug_name;
  D    signature;
  DFN  xep;
  DLFN iep;
  /* LAMBDA */
  D    environment;
  /* METHOD */
  DLFN mep;
  /* COMPLEX METHOD */
  D    keyword_specifiers;
} FN;

typedef struct _sig {
  D    class;
  SOV* required;
  SOV* values;
  D    rest_value;
  D    properties;
} SIG;

#define DEFUN(name, xep, iep)   D name[] = {I(0),I(0),I(0),(D)xep,(D)iep,I(0),I(0),I(0),I(0)}

extern D primitive_set_generic_function_xep(D fn); /* !@#$ FIX UP NAME */

/* MULTIPLE VALUES */

#define VALUES_MAX 64		/* maximum number of multiple values */

typedef struct _mv {
  int count;
  D   value[VALUES_MAX];
} MV;

extern MV Preturn_values;	/* should be per-thread mv return area */

#define	MV_GET_ELT(n) \
  (Preturn_values.count > (n) ? Preturn_values.value[n] : &KPfalseYinternalVdylan)
#define	MV_SET_ELT(n, t)	(Preturn_values.value[n] = (t))
#define MV_SET_COUNT(n)		(Preturn_values.count = (n))
#define MV_GET_COUNT()		(Preturn_values.count)

extern D MV_SPILL (D first_value);
extern D MV_UNSPILL (D spill_t);
extern D MV_GET_REST_AT (D first_value, DSINT first);
extern D MV_SET_REST_AT (D v, DSINT first);

/* NON-LOCAL CONTROL FLOW FRAMES */

typedef struct _bind_exit_frame {
  jmp_buf		        destination;
  MV			        return_values;
  struct _unwind_protect_frame* present_unwind_protect_frame;
} Bind_exit_frame;

typedef struct _unwind_protect_frame {
  jmp_buf                       destination;
  MV	                        return_values;
  struct _bind_exit_frame*      ultimate_destination;
  struct _unwind_protect_frame* previous_unwind_protect_frame;
} Unwind_protect_frame;

extern D MAKE_EXIT_FRAME (); 
extern D MAKE_UNWIND_FRAME (); 
extern D FRAME_DEST (D);
extern D FRAME_RETVAL (D);
extern D FALL_THROUGH_UNWIND (D); 
extern D CONTINUE_UNWIND ();
extern D NLX (Bind_exit_frame*, D);
extern Unwind_protect_frame* Pcurrent_unwind_protect_frame;

/* CALLING CONVENTION ENTRY POINTS */

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
extern D gf_xep_8 (FN* fn, int n, D a1, D a2, D a3, D a4, D a5, D a6, D a7, D a8);
extern D gf_xep_9 (FN* fn, int n, D a1, D a2, D a3, D a4, D a5, D a6, D a7, D a8, D a9);
extern D gf_xep   (FN* fn, int n, ...);

extern D gf_optional_xep_0 (FN* fn, int n, ...);
extern D gf_optional_xep_1 (FN* fn, int n, ...);
extern D gf_optional_xep_2 (FN* fn, int n, ...);
extern D gf_optional_xep_3 (FN* fn, int n, ...);
extern D gf_optional_xep_4 (FN* fn, int n, ...);
extern D gf_optional_xep_5 (FN* fn, int n, ...);
extern D gf_optional_xep_6 (FN* fn, int n, ...);
extern D gf_optional_xep_7 (FN* fn, int n, ...);
extern D gf_optional_xep_8 (FN* fn, int n, ...);
extern D gf_optional_xep_9 (FN* fn, int n, ...);
extern D gf_optional_xep   (FN* fn, int n, ...);

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

#define MEP_CALL_PROLOG(fn,nm,ac) { Pfunction_ = (fn); Pnext_methods_ = (nm); Pargument_count_ = (ac); }
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

extern D MAKE_CLOSURE(D, int, ...);
#define BOX(x) ((D*)malloc(sizeof(D)))
#define CAPTURE_ENVIRONMENT D* _env = Pfunction_->environment;
#define CREF(n) (_env[(n)])
#define CIREF(n) (*((D*)(CREF(n))))

/* CALLING CONVENTION REGISTERS */

extern FN* Pfunction_;
extern int Pargument_count_;
extern D   Pnext_methods_;

extern D XEP(FN*, int, ...);

extern D topI();

/* 
 * PRIMITIVES
 */

/* OBJECT REPRESENTATION PRIMITIVES AND SUPPORT */
 
extern void primitive_break (DBSTR message);
extern D primitive_invoke_debugger (D format_string, D arguments); 

# if defined(__alpha)
#   define ALPHA
# endif

#if defined(ALPHA)
#define primitive_word_size()	8
#else
#define primitive_word_size()	4
#endif

#define primitive_header_size()	primitive_word_size ()

#define TAG_BITS(x) (((unsigned long) x)&3)

#define I(n) ((D)((((unsigned long)(n))<<2)|1))
#define C(n) ((D)((((unsigned long)(n))<<2)|2))
#define R(n) ((long)(n)>>2)

#define primitive_object_class(object) (((OBJECT*)object)->class)

#define primitive_initialized_slot_value(object, position) \
  ((((OBJECT*)object)->slots)[position])
#define primitive_slot_value_setter(new_value, object, position) \
  ((((OBJECT*)object)->slots)[position] = (new_value))

extern D primitive_slot_value(D object, DSINT position);

#define primitive_repeated_slot_value(object, base_position, position) \
  ((((OBJECT*)object)->slots)[base_position + position])
#define primitive_repeated_slot_value_setter(new_value, object, base_position, position) \
  ((((OBJECT*)object)->slots)[base_position + position] = (new_value))

#define primitive_byte_element(object, position, index) \
	(((DBCHR*)&((((OBJECT*)object)->slots)[position]))[index])
#define primitive_byte_element_setter(new_value, object, position, index) \
	(((DBCHR*)&((((OBJECT*)object)->slots)[position]))[index] = \
	   (new_value))

extern D primitive_replace_bytesX(D dst[], D src[], int size);
extern D primitive_replaceX(D dst[], D src[], int size);

/* LOW-LEVEL ACCESSOR PRIMITIVES */

#define AT(t,x,o,b)		(((t*)(((DADDR)(x))+(int)(b)))[(int)(o)])
#define AT_SETTER(t,e,x,o,b)	(AT(t,x,o,b) = (e))

/*
#define primitive_signed_8_bit_integer_at(x, o, b) \
 	AT(INT8,x,o,b)
#define primitive_signed_8_bit_integer_at_setter(e, x, o, b) \
 	AT_SETTER(INT8,e,x,o,b)
#define primitive_unsigned_8_bit_integer_at(x, o, b) \
 	AT(UINT8,x,o,b)
#define primitive_unsigned_8_bit_integer_at_setter(e, x, o, b) \
 	AT_SETTER(UINT8,e,x,o,b)
#define primitive_signed_16_bit_integer_at(x, o, b) \
 	AT(INT16,x,o,b)
#define primitive_signed_16_bit_integer_at_setter(e, x, o, b) \
 	AT_SETTER(INT16,e,x,o,b)
#define primitive_unsigned_16_bit_integer_at(x, o, b) \
 	AT(UINT16,x,o,b)
#define primitive_unsigned_16_bit_integer_at_setter(e, x, o, b) \
 	AT_SETTER(UINT16,e,x,o,b)
#define primitive_signed_32_bit_integer_at(x, o, b) \
 	AT(INT32,x,o,b)
#define primitive_signed_32_bit_integer_at_setter(e, x, o, b) \
 	AT_SETTER(INT32,e,x,o,b)
#define primitive_unsigned_32_bit_integer_at(x, o, b) \
 	AT(UINT32,x,o,b)
#define primitive_unsigned_32_bit_integer_at_setter(e, x, o, b) \
 	AT_SETTER(UINT32,e,x,o,b)
#define primitive_signed_64_bit_integer_at(x, o, b) \
 	AT(INT64,x,o,b)
#define primitive_signed_64_bit_integer_at_setter(e, x, o, b) \
 	AT_SETTER(INT64,e,x,o,b)
#define primitive_unsigned_64_bit_integer_at(x, o, b) \
 	AT(UINT64,x,o,b)
#define primitive_unsigned_64_bit_integer_at_setter(e, x, o, b) \
 	AT_SETTER(UINT64,e,x,o,b)
#define primitive_ieee_single_float_at(x, o, b) \
	AT(SFLT,x,o,b)
#define primitive_ieee_single_float_at_setter(e, x, o, b) \
	AT_SETTER(SFLT,e,x,o,b)
#define primitive_ieee_double_float_at(x, o, b) \
	AT(DFLT,x,o,b)
#define primitive_ieee_double_float_at_setter(e, x, o, b) \
	AT_SETTER(DFLT,e,x,o,b)
#define primitive_ieee_extended_float_at(x, o, b) \
 	AT(EFLT,x,o,b)
#define primitive_ieee_extended_float_at_setter(e, x, o, b) \
 	AT_SETTER(EFLT,e,x,o,b)
*/	


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
  (~(((unsigned long)(-1)) << (bit_size)) << (bit_offset))  

#define AT_FIELD(t, x, b, bit_offset, bit_size) \
  ((t)((*(((DADDR)(x))+(b)) << (bit_offset)) & MAKE_MASK(t, 0, bit_size)))

#define AT_FIELD_SETTER(t, n, x, b, bit_offset, bit_size) \
  (*((t *)(((DADDR)(x))+(b))) = \
   ((((n) & MAKE_MASK(t, 0, bit_size)) >> (bit_offset)) \
    & ((*(((t)(x))+(b))) \
       & ((t)~MAKE_MASK(t, bit_offset, bit_size)))))

#define primitive_c_unsigned_field(pointer, byte_offset, bit_offset, \
				    bit_size) \
  AT_FIELD((unsigned long), pointer, byte_offset, bit_offset, bit_size)

#define primitive_c_signed_field(pointer, byte_offset, bit_offset, \
				  bit_size) \
  AT_FIELD((signed long), pointer, byte_offset, bit_offset, bit_size)

#define primitive_c_int_field(pointer, byte_offset, bit_offset, \
			       bit_size) \
  AT_FIELD((unsigned int), pointer, byte_offset, bit_offset, bit_size)

#define primitive_c_unsigned_field_setter(new, pointer, byteoffset, \
					   bit_offset, bit_size) \
  AT_FIELD_SETTER((unsigned long), new, pointer, byte_offset, bit_offset, \
		  bit_size)

#define primitive_c_signed_field_setter(new, pointer, byte_offset, \
					   bit_offset, bit_size) \
  AT_FIELD_SETTER(signed long, new, pointer, byte_offset, bit_offset, \
		  bit_size)

#define primitive_c_int_field_setter(new, pointer, byte_offset, \
					  bit_offset, bit_size) \
  AT_FIELD_SETTER((unsigned int), new, pointer, byte_offset, bit_offset, \
		  bit_size)

#define primitive_element(x, o, b) \
	AT(D,x,o,b)
#define primitive_element_setter(e, x, o, b) \
	AT_SETTER(D,e,x,o,b)
	
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

/* ALLOCATION PRIMITIVES */

extern D primitive_allocate(DSINT);
extern D primitive_byte_allocate(DSINT, DSINT);
extern D primitive_allocate_filled(DSINT, D, DSINT, D, DSINT, DSINT);
extern D primitive_allocate_wrapper(DSINT, D, DSINT, D, DSINT, DSINT);
extern D primitive_byte_allocate_filled(DSINT, DSINT, D, DSINT, D, DSINT, DSINT);
extern D primitive_untraced_allocate(DSINT);

/* GC PRIMITIVES */

#define primitive_gc_state() (I(0)) /* !@#$ DUMMY DEFN */

/* BYTE CHARACTER PRIMITIVES */

#define primitive_byte_character_as_raw(x) \
   (R(x))
#define primitive_raw_as_byte_character(x) \
   (C(x))
   
/* INTEGER PRIMITIVES */

extern DSINT primitive_integer_as_raw(D x);
extern D     primitive_raw_as_integer(DSINT x);
   
/* SMALL-INTEGER PRIMITIVES */

#define primitive_small_integer_as_raw(x) \
   (R(x))
#define primitive_raw_as_small_integer(x) \
   (I(x))
#define primitive_small_integer_negate(x) \
   (-(x))
#define primitive_small_integer_add(x, y) \
   ((x) + (y))
#define primitive_small_integer_subtract(x, y) \
   ((x) - (y))
#define primitive_small_integer_multiply(x, y) \
   ((x) * (y))
#define primitive_small_integer_divide(x, y) \
   ((x) / (y))
#define primitive_small_integer_left_shift(x, y) \
   ((x) << (y))
#define primitive_small_integer_right_shift(x, y) \
   ((x) >> (y))
#define primitive_small_integer_and(x, y) \
   ((x) & (y))
#define primitive_small_integer_or(x, y) \
   ((x) | (y))
#define primitive_small_integer_xor(x, y) \
   ((x) ^ (y))
#define primitive_small_integer_not(x) \
   (~(x))
#define primitive_small_integer_equalsQ(x, y) \
   ((x) == (y))
#define primitive_small_integer_less_thanQ(x, y) \
   ((x) < (y))
#define primitive_small_integer_greater_thanQ(x, y) \
   ((x) > (y))
#define primitive_small_integer_less_than_or_equalQ(x, y) \
   ((x) <= (y))
#define primitive_small_integer_greater_than_or_equalQ(x, y) \
   ((x) >= (y))

/* BIG-INTEGER PRIMITIVES */

#define primitive_big_integer_as_raw(x) \
  (((DBI)(x))->data)
extern D primitive_raw_as_big_integer(DBINT x);
   
/* MACHINE-INTEGER PRIMITIVES */

#define primitive_machine_integer_as_raw(x) \
  (((DMI)(x))->data)
extern D primitive_raw_as_machine_integer(DMINT x);
   
/* UNSIGNED-MACHINE-INTEGER PRIMITIVES */

#define primitive_unsigned_machine_integer_as_raw(x) \
  (((DUMI)(x))->data)
extern D primitive_raw_as_unsigned_machine_integer(DUMINT x);
   
/* ADDRESS PRIMITIVES */

#define primitive_address_and(x, y) \
   ((DADDR)(x) & (DADDR)(y))
#define primitive_address_add(x, y) \
   ((DADDR)(x) + (DADDR)(y))
#define primitive_address_equalsQ(x, y) \
   ((DADDR)(x) == (DADDR)(y))
#define primitive_address_as_pointer(x) \
   ((D)x)	

/* POINTER PRIMITIVES */

#define primitive_pointer_as_address(x)		((DADDR)x)
#define primitive_pointer_as_small_integer(x)	((DSINT)x)

/* MACHINE-WORD PRIMITIVES */

#define primitive_integerQ(x)                         (TAG_BITS(x) == 1)

#define primitive_machine_word_equalQ(x, y)               ((x) == (y))
#define primitive_machine_word_not_equalQ(x, y)           ((x) != (y))
#define primitive_machine_word_less_thanQ(x, y)           ((x) < (y))
#define primitive_machine_word_not_less_thanQ(x, y)       ((x) >= (y))
#define primitive_machine_word_greater_thanQ(x, y)        ((x) > (y))
#define primitive_machine_word_not_greater_thanQ(x, y)    ((x) <= (y))

extern D primitive_wrap_machine_word(DMINT);

#define primitive_unwrap_machine_word(x) \
  (((DMI)(x))->data)

/*   primitive_unwrap_c_pointer
 * assumes that instances of <c-pointer> are implemented as
 * struct {D class; D pointer_address; ...}
 * ..and that the address is stored as a machine word.
 * When we can store a true raw slot in a C-pointer then the unwrap
 * machine-word will be unnecessary.
 */
#define primitive_unwrap_c_pointer(c_ptr) \
  ((void*)primitive_unwrap_machine_word(*(((D*)(c_ptr))+1)))

#define primitive_cast_integer_as_raw(x)                  ((DMINT)(x))
#define primitive_cast_raw_as_integer(x)                  ((D)(x))

extern D primitive_wrap_abstract_integer(DMINT);
extern D primitive_wrap_unsigned_abstract_integer(DMINT);
extern DMINT primitive_unwrap_abstract_integer(D);
#define primitive_box_integer(x) 			  (I(x))
#define primitive_unbox_integer(x) 			  (R(x))

extern DMINT primitive_machine_word_boole(D, DMINT, DMINT);

#define primitive_machine_word_lognot(x)                  (~(x))

#define primitive_machine_word_logbitQ(i, x) ((1UL << (i)) & ((DUMINT)(x)))

extern DMINT primitive_machine_word_count_low_zeros(DMINT);
extern DMINT primitive_machine_word_count_high_zeros(DMINT);

#define primitive_machine_word_add(x, y)                  ((x) + (y))

extern DMINT primitive_machine_word_add_with_overflow(DMINT, DMINT);

#define primitive_machine_word_subtract(x, y)             ((x) - (y))

extern DMINT primitive_machine_word_subtract_with_overflow(DMINT, DMINT);

#define primitive_machine_word_multiply_low(x, y)         ((x) * (y))

extern DMINT primitive_machine_word_multiply_high(DMINT, DMINT);
extern DMINT primitive_machine_word_multiply_lowShigh(DMINT, DMINT);
extern DMINT primitive_machine_word_multiply_low_with_overflow(DMINT, DMINT);
extern DMINT primitive_machine_word_multiply_with_overflow(DMINT, DMINT);

#define primitive_machine_word_negative(x)                (-(x))
#define primitive_machine_word_abs(x) 			  ((x)<0?-(x):(x))

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

#define primitive_machine_word_shift_left_low(x, i)       ((x) << (i))

extern DMINT primitive_machine_word_shift_left_high(DMINT, DMINT);
extern DMINT primitive_machine_word_shift_left_lowShigh(DMINT, DMINT);
extern DMINT primitive_machine_word_shift_left_low_with_overflow(DMINT, DMINT);
extern DMINT primitive_machine_word_shift_left_with_overflow(DMINT, DMINT);

#define primitive_machine_word_shift_right(x, i)          ((x) >> (i))

extern DMINT primitive_machine_word_add_signal_overflow(DMINT, DMINT);
extern DMINT primitive_machine_word_subtract_signal_overflow(DMINT, DMINT);
extern DMINT primitive_machine_word_multiply_signal_overflow(DMINT, DMINT);
extern DMINT primitive_machine_word_negative_signal_overflow(DMINT);
extern DMINT primitive_machine_word_abs_signal_overflow(DMINT);
extern DMINT primitive_machine_word_shift_left_signal_overflow(DMINT, DMINT);

extern DMINT
primitive_machine_word_double_floorS_quotient(DMINT, DMINT, DMINT);

extern DMINT
primitive_machine_word_double_floorS_remainder(DMINT, DMINT, DMINT);

extern DMINT
primitive_machine_word_double_floorS(DMINT, DMINT, DMINT);

extern DMINT
primitive_machine_word_double_ceilingS_quotient(DMINT, DMINT, DMINT);

extern DMINT
primitive_machine_word_double_ceilingS_remainder(DMINT, DMINT, DMINT);

extern DMINT
primitive_machine_word_double_ceilingS(DMINT, DMINT, DMINT);

extern DMINT
primitive_machine_word_double_roundS_quotient(DMINT, DMINT, DMINT);

extern DMINT
primitive_machine_word_double_roundS_remainder(DMINT, DMINT, DMINT);

extern DMINT
primitive_machine_word_double_roundS(DMINT, DMINT, DMINT);

extern DMINT
primitive_machine_word_double_truncateS_quotient(DMINT, DMINT, DMINT);

extern DMINT
primitive_machine_word_double_truncateS_remainder(DMINT, DMINT, DMINT);

extern DMINT
primitive_machine_word_double_truncateS(DMINT, DMINT, DMINT);

extern DMINT
primitive_machine_word_double_divide_quotient(DMINT, DMINT, DMINT);

extern DMINT
primitive_machine_word_double_divide_remainder(DMINT, DMINT, DMINT);

extern DMINT
primitive_machine_word_double_divide(DMINT, DMINT, DMINT);

#define primitive_machine_word_unsigned_less_thanQ(x, y) \
  (((DUMINT)(x)) < ((DUMINT)(y)))

#define primitive_machine_word_unsigned_not_less_thanQ(x, y) \
  (((DUMINT)(x)) >= ((DUMINT)(y)))

#define primitive_machine_word_unsigned_greater_thanQ(x, y) \
  (((DUMINT)(x)) > ((DUMINT)(y)))

#define primitive_machine_word_unsigned_not_greater_thanQ(x, y) \
  (((DUMINT)(x)) <= ((DUMINT)(y)))

extern DMINT primitive_machine_word_unsigned_add_with_carry(DMINT, DMINT);
extern DMINT
primitive_machine_word_unsigned_subtract_with_borrow(DMINT, DMINT);
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
  ((DMINT)(((DUMINT)(x)) << (i)))

extern DMINT
primitive_machine_word_unsigned_double_divide_quotient(DMINT, DMINT, DMINT);

extern DMINT
primitive_machine_word_unsigned_double_divide_remainder(DMINT, DMINT, DMINT);

extern DMINT
primitive_machine_word_unsigned_double_divide(DMINT, DMINT, DMINT);

extern DMINT
primitive_machine_word_unsigned_shift_left_high(DMINT, DMINT, DMINT);

extern DMINT
primitive_machine_word_unsigned_double_shift_left_high(DMINT, DMINT, DMINT);

extern DMINT
primitive_machine_word_unsigned_double_shift_left(DMINT, DMINT, DMINT);

extern DMINT
primitive_machine_word_unsigned_double_shift_right_low(DMINT, DMINT, DMINT);

extern DMINT
primitive_machine_word_unsigned_double_shift_right_high(DMINT, DMINT, DMINT);

extern DMINT
primitive_machine_word_unsigned_double_shift_right(DMINT, DMINT, DMINT);

/* end of MACHINE-WORD PRIMITIVES */

/* SINGLE-FLOAT PRIMITIVES */

extern FLT primitive_bits_as_single_float(DUMINT x);
extern DUMINT primitive_single_float_as_bits(FLT x);
#define primitive_single_float_as_raw(x) \
    (((DSF)x)->data)
extern D primitive_raw_as_single_float(FLT x);
#define primitive_single_float_as_small_integer(x) \
    ((DSINT)(x))
#define primitive_small_integer_as_single_float(x) \
    ((FLT)(x))
#define primitive_single_float_as_big_integer(x) \
    ((DBINT)(x))
#define primitive_big_integer_as_single_float(x) \
    ((FLT)(x))
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
    ((x) == (y))
#define primitive_single_float_less_thanQ(x, y) \
    ((x) < (y))
    
#include <math.h>

#define primitive_single_float_log(x) \
    (log(x))
#define primitive_single_float_expt(base, power) \
    (pow(base, power))
#define primitive_single_float_exp(x) \
    (exp(x))
#define primitive_single_float_sqrt(x) \
    (sqrt(x))
#define primitive_single_float_sin(x) \
    (sin(x))
#define primitive_single_float_cos(x) \
    (cos(x))
#define primitive_single_float_tan(x) \
    (tan(x))
#define primitive_single_float_asin(x) \
    (asin(x))
#define primitive_single_float_acos(x) \
    (acos(x))
#define primitive_single_float_atan(x) \
    (atan(x))
   
/* COMPARISON PRIMITIVES */

#define primitive_range_check(x, l, h)	(((x) >= (l)) & ((x) < (h)))
extern D primitive_type_check(D x, D t);

/* COMPARISON PRIMITIVES */

#define primitive_raw_as_boolean(x)	((D)((x) ? &KPtrueYinternalVdylan : &KPfalseYinternalVdylan))
#define primitive_boolean_as_raw(x)	((x) != &KPfalseYinternalVdylan)
#define primitive_trueQ(x)		((D)((x) ? &KPtrueYinternalVdylan : &KPfalseYinternalVdylan))
#define primitive_falseQ (x)		((D)((x) ? &KPfalseYinternalVdylan : &KPtrueYinternalVdylan))
#define primitive_not(x)		(~(x))
#define primitive_idQ(x, y)	        ((x) == (y))
#define primitive_not_idQ(x, y) 	((x) != (y))

/* VECTOR PRIMITIVES */

#define primitive_vector_element(v, i)      	 ((((SOV*)v)->data)[R(i)])
#define primitive_vector_element_setter(e, v, i) ((((SOV*)v)->data)[R(i)] = (e))
#define primitive_vector_size(v) 		 (((SOV*)v)->size)
#define primitive_vector_as_raw(v)		 (((SOV*)v)->data)

extern D primitive_vector(D dn, ...);
extern D primitive_raw_as_vector(D a, D n);
extern D primitive_make_vector(int size);
extern D primitive_copy_vector(D vector);

/*
extern D vectorYdylanVdylanI(SOV* b);
extern D vectorYdylanVdylan[];
*/

/* STRING PRIMITIVES */

#define primitive_strlen(s)      		(strlen((DBSTR)s))
#define primitive_string_as_raw(v)		(((BS*)v)->data)
extern D primitive_raw_as_string(DBSTR buffer);

/* CALLING CONVENTION PRIMITIVES */

#define primitive_current_function() 			((D)(Pfunction_))
#define primitive_function_parameter()			((D)(Pfunction_))
#define primitive_lambda_parameter()			((D)(Pfunction_))
#define primitive_next_methods_parameter()		((D)(Pnext_methods_))
#define primitive_next_methods_parameter_setter(x) 	((D)(Pnext_methods_ = (D)x))

/* APPLY PRIMITIVES */

extern D primitive_apply (D fn, D sov);
extern D primitive_apply_spread (D fn, int n, ...);
extern D primitive_mep_apply_spread (D fn, D nm, int n, ...);
extern D primitive_xep_apply (FN* fn, int n, D a[]);
extern D primitive_mep_apply_with_optionals 
  (FN* fn, D new_next_methods, D a);
extern D primitive_mep_apply (FN* fn, D next_methods, D a[]);  
extern D primitive_iep_apply (FN* fn, int n, D a[]);

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

/* BOOTING PRIMITIVES */

#define iA(x, y) (I(R(x) + R(y)))
#define i_(x, y) (I(R(x) - R(y)))
#define iT(x, y) (I(R(x) * R(y)))
#define iL(x, y) (primitive_trueQ(R(x) < R(y)))
#define iE(x, y) (primitive_trueQ(R(x) == R(y)))
#define iG(x, y) (primitive_trueQ(R(x) > R(y)))
#define pE(x, y) (primitive_trueQ(R(x) == R(y)))

/* VALUES PRIMITIVES */

extern D primitive_values (D v);

/* TERMINAL PRIMITIVES -- TODO: IGNORING FD FOR NOW */

#include <stdio.h>

#define primitive_open(name, direction)	\
  fopen((char*)name, (char*)direction)
#define primitive_input_terminal()		stdin
#define primitive_output_terminal() 		stdout
#define primitive_input(fd)			getchar()
#define primitive_output(c, fd)			putchar(c)
#define primitive_force_output(fd)		fflush((FILE*)(fd))
#define primitive_close(fd) 			fclose(fd)

/* OPERATING SYSTEM PRIMITIVES */

extern DSINT primitive_run_application (DBSTR command);
extern void  primitive_exit_application (DSINT code);
extern DBSTR primitive_environment_variable (DBSTR name);
extern DBSTR primitive_environment_variable_setter (DBSTR value, DBSTR name);

/* TEMPORARY PRIMITIVES FOR ASSIGNMENT */

extern D MAKE_BOX(D);
extern D GET_BOX_VAL(D);
extern D SET_BOX_VAL(D, D);

/* RUN-TIME CALLBACKS */

extern D primitive_resolve_symbol (D symbol);
extern D primitive_string_as_symbol (D string);

/* TIMER PRIMITIVES */

D primitive_start_timer();
D primitive_stop_timer();

/* eof */
