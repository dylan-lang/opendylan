/*

;;; Copyright (c) 1993, Jonathan Bachrach
;;; Copyright (c) 1993, The Harlequin Group Limited
;;; Copyright (c) 1993, IRCAM
;;; All rights reserved.
;;; 
;;; Redistribution and use in source and binary forms, with or without modifi-
;;; cation,  are not permitted without the express prior written permission of
;;; the copyright holders.
;;; 
;;; THIS SOFTWARE IS PROVIDED BY    JONATHAN BACHRACH,     THE HARLEQUIN GROUP
;;; LIMITED, AND IRCAM ``AS IS'' AND  ANY EXPRESS OR  IMPLIED WARRANTIES,  IN-
;;; CLUDING,  BUT  NOT LIMITED TO,  THE IMPLIED WARRANTIES OF  MERCHANTABILITY 
;;; AND  FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.   IN NO EVENT  SHALL 
;;; JONATHAN BACHRACH, THE HARLEQUIN GROUP LIMITED, OR IRCAM BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;;; (INCLUDING,  BUT NOT LIMITED TO,  PROCUREMENT OF SUBSTITUTE GOODS OR  SER-
;;; VICES;  LOSS OF USE, DATA, OR PROFITS;  OR  BUSINESS INTERRUPTION) HOWEVER
;;; CAUSED AND  ON ANY THEORY OF LIABILITY,  WHETHER IN CONTRACT,  STRICT LIA-
;;; BILITY,  OR TORT  (INCLUDING NEGLIGENCE OR  OTHERWISE)  ARISING IN ANY WAY 
;;; OUT OF  THE USE OF THIS SOFTWARE,  EVEN IF  ADVISED OF THE POSSIBILITY  OF 
;;; SUCH DAMAGE.

*/

typedef void* Z;

typedef void			VOID;
typedef signed char		INT8;
typedef unsigned char		UINT8;
typedef signed short		INT16;
typedef unsigned short		UINT16;
typedef signed long		INT32;
typedef unsigned long		UINT32;
#ifdef WIN32
typedef double	                INT64; /* Cleary wrong, but the right size. */
typedef double	                UINT64;
#else
typedef signed long long	INT64;
typedef unsigned long long	UINT64;
#endif
typedef float			FLT;
typedef double			DFLT;
/*
typedef long double		EFLT;
*/

#ifndef MAYBE_EXTERN
#define MAYBE_EXTERN extern
/* #undef MAYBE_EXTERN */
#endif
#include "machine.h"

#include <math.h>
#include <stdio.h>
#include <stdarg.h>
#include <setjmp.h>
#include <string.h>

#ifdef __GNUC__ 
#define INLINE inline
#define ALIGN(x) __attribute__ ((aligned (x)))
#else
#define INLINE
#define ALIGN(x) 
#endif

#define UNBOUND ((Z)0)
#define ARGUMENT_COUNT_MAX 64
#define VALUES_MAX 64

/*
typedef struct {
  Z k;
  Z c;
  Z rc;
} CW;
*/

typedef struct _sov{
  Z class;
  Z size;
  Z data[1];
} SOV;

typedef struct _class {
  Z meta_class;
  Z rest_slots[1];
} CLASS;

typedef Z (*ZFN)(Z,int,...);
typedef Z (*ZLFN)();

typedef struct _fn {
  Z class;
  Z debug_name;
  Z properties;
  ZFN xep;
  ZLFN iep;
  /* LAMBDA */
  Z environment;
  Z specializers;
  /* METHOD */
  ZLFN mep;
  /* COMPLEX METHOD */
  Z keyword_specifiers;
} FN;

typedef union {
  Z    z;
  float f;
} ZFLT;

typedef union {
  int   i;
  float f;
} INTFLT;

typedef union {
  INT64 i;
  double    f;
} INTDFLT;

typedef struct _bind_exit_frame {
  jmp_buf		        destination;
  Z			        return_values[VALUES_MAX+2];
  struct _unwind_protect_frame* present_unwind_protect_frame;
} Bind_exit_frame;

typedef struct _unwind_protect_frame {
  jmp_buf                       destination;
  int                           multiple_value_count;
  Z	                        multiple_values[VALUES_MAX];
  struct _bind_exit_frame*      ultimate_destination;
  struct _unwind_protect_frame* previous_unwind_protect_frame;
} Unwind_protect_frame;

extern Unwind_protect_frame* _P_current_unwind_protect_frame;

/*
typedef struct _state {
  Z      	 valid_state_p;
  jmp_buf 	 c_stack;
  Z      	 z_values;
  struct _state* previous_state;
  FN*            unwind_closure;      
  Z		 current_frame;
} State;

extern State* current_state;
*/

#define False 0
#define True  1

#define INITIAL_MAX_STACK_SIZE 200000
#define STACK_SIZE_INCREMENT    100000

extern int dylanXinternalX_P_bottom_of_stack;

#define SABOTAGE_STACK() \
   dylanXinternalX_P_max_stack_size = 0

#define IS_STACK_SABOTAGED() \
  (dylanXinternalX_P_max_stack_size == 0)

#define UNSABOTAGE_STACK() \
  dylanXinternalX_P_max_stack_size = dylanXinternalX_P_real_max_stack_size;

extern int dylanXinternalX_P_stack_check_enabled_Q_;
extern int dylanXinternalX_P_max_stack_size;
extern int dylanXinternalX_P_real_max_stack_size;
extern int dylanXinternalX_P_signal_number;
extern Z _P_function;
extern Z _P_next_methods;
extern int _P_argument_count;

extern int dylanXinternalXprimitive_file_exists_Q_(char* filename);
extern int dylanXinternalXerrno_value();
extern Z dylanXinternalXprimitive_nlx (Bind_exit_frame*, SOV*);
extern Z dylanXinternalXprimitive_inlined_nlx (Bind_exit_frame*, Z);
extern Z* dylanXinternalXprimitive_make_box(Z);
extern Z dylanXinternalXprimitive_copy_vector(Z);
extern Z dylanXinternalXprimitive_allocate(int);
extern Z dylanXinternalXprimitive_byte_allocate(int,int);
extern Z dylanXinternalXprimitive_element(Z*,int);
extern Z dylanXinternalXprimitive_element_setter(Z,Z*,int);
extern Z dylanXinternalXprimitive_fill_E_(Z*,int,Z);
extern Z dylanXinternalXprimitive_replace_E_(Z*,Z*,int);
extern Z dylanXinternalXprimitive_more_argument(Z*,int);
extern Z dylanXinternalXprimitive_native_string_allocate(int);
extern int dylanXinternalXprimitive_native_string_size(char*);
extern int dylanXinternalXprimitive_native_string_element(char*,int);
extern int dylanXinternalXprimitive_native_string_element_setter(int,char*,int);
extern int dylanXinternalXprimitive_random(int);
extern Z dylanXinternalXprimitive_run_shell_command(FILE*, char*);
extern int dylanXinternalXprimitive_environment_variable_setter(char*,char*);
extern char* dylanXinternalXprimitive_operating_system_name();
extern int dylanXinternalXprimitive_probe_file(char*);
extern int dylanXinternalXprimitive_file_date(char*);
extern int dylanXinternalXprimitive_file_date_setter(int,char*);
extern Z dylanXinternalX_P_stack_overflow_error_LOC(Z, Z);
extern int dylanXinternalXprimitive_break();
extern Z dylanXinternalXprimitive_continue_unwind ();
extern FILE* dylanXinternalXprimitive_open_input_terminal (char*);
extern FILE* dylanXinternalXprimitive_open_output_terminal (char*);
extern float dylanXinternalXprimitive_decoded_bits_as_single_float(int,int,int);
extern Z* dylanXinternalXprimitive_make_environment(int, ...);
extern Z dylanXinternalXprimitive_mep_apply (FN*, Z, Z*);
extern Z dylanXinternalXprimitive_mep_apply_with_optionals (FN*, Z, Z*);
extern Z dylanXinternalXprimitive_iep_apply (FN*, int, Z*);
extern Z dylanXinternalXprimitive_xep_apply (FN*, int, Z*);
extern Z dylanXinternalXprimitive_stack_vector_remaining_values(int);
extern float zed_to_single_float(Z z);
extern Z single_float_to_zed(float f);
extern double zed_to_double_float(Z z);
extern Z double_float_to_zed(double f);
extern float integer_to_single_float(int x);
extern int single_float_to_integer(float f);
extern double big_integer_to_double_float(INT64 x);
extern INT64 double_float_to_big_integer(double f);

extern UINT8 dylanXinternalXprimitive_unsigned_8_bit_integer_at (void *address);
extern UINT8 dylanXinternalXprimitive_unsigned_8_bit_integer_at_setter (UINT8 x, void *address);
extern INT8 dylanXinternalXprimitive_signed_8_bit_integer_at (void *address);
extern INT8 dylanXinternalXprimitive_signed_8_bit_integer_at_setter (INT8 x, void *address);
extern UINT16 dylanXinternalXprimitive_unsigned_16_bit_integer_at (void *address);
extern UINT16 dylanXinternalXprimitive_unsigned_16_bit_integer_at_setter (UINT16 x, void *address);
extern INT16 dylanXinternalXprimitive_signed_16_bit_integer_at (void *address);
extern INT16 dylanXinternalXprimitive_signed_16_bit_integer_at_setter (INT16 x, void *address);
extern UINT32 dylanXinternalXprimitive_unsigned_32_bit_integer_at (void *address);
extern UINT32 dylanXinternalXprimitive_unsigned_32_bit_integer_at_setter (UINT32 x, void *address);
extern INT32 dylanXinternalXprimitive_signed_32_bit_integer_at (void *address);
extern INT32 dylanXinternalXprimitive_signed_32_bit_integer_at_setter (INT32 x, void *address);
extern UINT64 dylanXinternalXprimitive_unsigned_64_bit_integer_at (void *address);
extern UINT64 dylanXinternalXprimitive_unsigned_64_bit_integer_at_setter (UINT64 x, void *address);
extern INT64 dylanXinternalXprimitive_signed_64_bit_integer_at (void *address);
extern INT64 dylanXinternalXprimitive_signed_64_bit_integer_at_setter (INT64 x, void *address);
extern FLT dylanXinternalXprimitive_ieee_single_float_at (void *address);
extern FLT dylanXinternalXprimitive_ieee_single_float_at_setter (FLT x, void *address);
extern DFLT dylanXinternalXprimitive_ieee_double_float_at (void *address);
extern DFLT dylanXinternalXprimitive_ieee_double_float_at_setter (DFLT x, void *address);
/*
extern EFLT dylanXinternalXprimitive_ieee_extended_float_at (void *address);
extern EFLT dylanXinternalXprimitive_ieee_extended_float_at_setter (EFLT x, void *address);
*/

/* TAGGED OBJECTS */

#define I(a) ((Z)(((a)<<2)+1))
#define C(a) ((Z)(((a)<<2)+2))

/*
#define M(w,b) ((Z)(((char *)(w)) + (b)))
*/
#define M(w,b) ((Z)(((char *)(w))))

/* FN PROPERTIES | NEXT? | REST? | ALL-KEYS? | KEY? | NUMBER-REQUIRED | */

#define GFP(r,ak,k,s) (I(((r)<<18)|((ak)<<17)|((k)<<16)|(s)))
#define FNP(n,r,ak,k,s) (I(((n)<<19)|((ak)<<18)|((r)<<17)|((k)<<16)|(s)))

/* PORTABLE STRING CONSTRUCTION FOR REPEATED CHARACTER DATA */

/*
#define S8(a,b,c,d,e,f,g,h) (Z)(((a)<<56)|((b)<<48)|((c)<<40)|((d)<<32)|((e)<<24)|((f)<<16)|((g)<<8)|(h))
#define S7(a,b,c,d,e,f,g)   (Z)(((a)<<56)|((b)<<48)|((c)<<40)|((d)<<32)|((e)<<24)|((f)<<16)|((g)<<8))
#define S6(a,b,c,d,e,f)     (Z)(((a)<<56)|((b)<<48)|((c)<<40)|((d)<<32)|((e)<<24)|((f)<<16))
#define S5(a,b,c,d,e)       (Z)(((a)<<56)|((b)<<48)|((c)<<40)|((d)<<32)|((e)<<24))
#define S4(a,b,c,d)         (Z)(((a)<<56)|((b)<<48)|((c)<<40)|((d)<<32)
#define S3(a,b,c)           (Z)(((a)<<56)|((b)<<48)|((c)<<40))
#define S2(a,b)             (Z)(((a)<<56)|((b)<<48))
#define S1(a)               (Z)(((a)<<56))
#define S0                  (Z)(0)
*/

#if defined(__alpha)

#define S8(a,b,c,d,e,f,g,h) (Z)(((long)(a))|(((long)(b))<<8)|(((long)(c))<<16)|(((long)(d))<<24)|(((long)(e))<<32)|(((long)(f))<<40)|(((long)(g))<<48)|(((long)(h))<<56))
#define S7(a,b,c,d,e,f,g)   (Z)(((long)(a))|(((long)(b))<<8)|(((long)(c))<<16)|(((long)(d))<<24)|(((long)(e))<<32)|(((long)(f))<<40)|(((long)(g))<<48))
#define S6(a,b,c,d,e,f)     (Z)(((long)(a))|(((long)(b))<<8)|(((long)(c))<<16)|(((long)(d))<<24)|(((long)(e))<<32)|(((long)(f))<<40))
#define S5(a,b,c,d,e)       (Z)(((long)(a))|(((long)(b))<<8)|(((long)(c))<<16)|(((long)(d))<<24)|(((long)(e))<<32))
#define S4(a,b,c,d)         (Z)(((long)(a))|(((long)(b))<<8)|(((long)(c))<<16)|(((long)(d))<<24))
#define S3(a,b,c)           (Z)(((long)(a))|(((long)(b))<<8)|(((long)(c))<<16))
#define S2(a,b)             (Z)(((long)(a))|(((long)(b))<<8))
#define S1(a)               (Z)(a)
#define S0                  (Z)(0)

#elif defined(_M_IX86)

#define S8(a,b,c,d,e,f,g,h) (Z)((a)|((b)<<8)|((c)<<16)|((d)<<24)),(Z)((e)|((f)<<8)|((g)<<16)|((h)<<24))
#define S7(a,b,c,d,e,f,g)   (Z)((a)|((b)<<8)|((c)<<16)|((d)<<24)),(Z)((e)|((f)<<8)|((g)<<16))
#define S6(a,b,c,d,e,f)     (Z)((a)|((b)<<8)|((c)<<16)|((d)<<24)),(Z)((e)|((f)<<8))
#define S5(a,b,c,d,e)       (Z)((a)|((b)<<8)|((c)<<16)|((d)<<24)),(Z)(e)
#define S4(a,b,c,d)         (Z)((a)|((b)<<8)|((c)<<16)|((d)<<24)),(Z)(0)
#define S3(a,b,c)           (Z)((a)|((b)<<8)|((c)<<16))
#define S2(a,b)             (Z)((a)|((b)<<8))
#define S1(a)               (Z)(a)
#define S0                  (Z)(0)

#else

#define S8(a,b,c,d,e,f,g,h) (Z)(((a)<<24)|((b)<<16)|((c)<<8)|(d)),(Z)(((e)<<24)|((f)<<16)|((g)<<8)|(h))
#define S7(a,b,c,d,e,f,g)   (Z)(((a)<<24)|((b)<<16)|((c)<<8)|(d)),(Z)(((e)<<24)|((f)<<16)|((g)<<8))
#define S6(a,b,c,d,e,f)     (Z)(((a)<<24)|((b)<<16)|((c)<<8)|(d)),(Z)(((e)<<24)|((f)<<16))
#define S5(a,b,c,d,e)       (Z)(((a)<<24)|((b)<<16)|((c)<<8)|(d)),(Z)(((e)<<24))
#define S4(a,b,c,d)         (Z)(((a)<<24)|((b)<<16)|((c)<<8)|(d)),(Z)(0)
#define S3(a,b,c)           (Z)(((a)<<24)|((b)<<16)|((c)<<8))
#define S2(a,b)             (Z)(((a)<<24)|((b)<<16))
#define S1(a)               (Z)(((a)<<24))
#define S0                  (Z)(0)

#endif

#define CALLN(fn,n) (Z)(((FN*)(fn))->xep)((fn),n
#define CALL0(fn) CALLN(fn,0))
#define CALL1(fn,a1) CALLN(fn,1),(a1))
#define CALL2(fn,a1,a2) CALLN(fn,2),(a1),(a2))
#define CALL3(fn,a1,a2,a3) CALLN(fn,3),(a1),(a2),(a3))
#define CALL4(fn,a1,a2,a3,a4) CALLN(fn,4),(a1),(a2),(a3),(a4))
#define CALL5(fn,a1,a2,a3,a4,a5) CALLN(fn,5),(a1),(a2),(a3),(a4),(a5))
#define CALL6(fn,a1,a2,a3,a4,a5,a6) CALLN(fn,6),(a1),(a2),(a3),(a4),(a5),(a6))
#define CALL7(fn,a1,a2,a3,a4,a5,a6,a7) CALLN(fn,7),(a1),(a2),(a3),(a4),(a5),(a6),(a7))
#define CALL8(fn,a1,a2,a3,a4,a5,a6,a7,a8) CALLN(fn,8),(a1),(a2),(a3),(a4),(a5),(a6),(a7),(a8))
#define CALL9(fn,a1,a2,a3,a4,a5,a6,a7,a8,a9) CALLN(fn,9),(a1),(a2),(a3),(a4),(a5),(a6),(a7),(a8),(a9))
#define CALL10(fn,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) CALLN(fn,10),(a1),(a2),(a3),(a4),(a5),(a6),(a7),(a8),(a9),(a10))

#define MEP_CALL_PROLOG(fn,nm,ac) { _P_function = (fn); _P_next_methods = (nm); _P_argument_count = (ac); }
#define MEP_CALL_N(fn) (Z)(((FN*)(fn))->mep)(
#define MEP_CALL_0(fn) MEP_CALL_N(fn))
#define MEP_CALL_1(fn,a1) MEP_CALL_N(fn)(a1))
#define MEP_CALL_2(fn,a1,a2) MEP_CALL_N(fn)(a1),(a2))
#define MEP_CALL_3(fn,a1,a2,a3) MEP_CALL_N(fn)(a1),(a2),(a3))
#define MEP_CALL_4(fn,a1,a2,a3,a4) MEP_CALL_N(fn)(a1),(a2),(a3),(a4))
#define MEP_CALL_5(fn,a1,a2,a3,a4,a5) MEP_CALL_N(fn)(a1),(a2),(a3),(a4),(a5))
#define MEP_CALL_6(fn,a1,a2,a3,a4,a5,a6) MEP_CALL_N(fn)(a1),(a2),(a3),(a4),(a5),(a6))
#define MEP_CALL_7(fn,a1,a2,a3,a4,a5,a6,a7) MEP_CALL_N(fn)(a1),(a2),(a3),(a4),(a5),(a6),(a7))
#define MEP_CALL_8(fn,a1,a2,a3,a4,a5,a6,a7,a8) MEP_CALL_N(fn)(a1),(a2),(a3),(a4),(a5),(a6),(a7),(a8))
#define MEP_CALL_9(fn,a1,a2,a3,a4,a5,a6,a7,a8,a9) MEP_CALL_N(fn)(a1),(a2),(a3),(a4),(a5),(a6),(a7),(a8),(a9))

#define IEP_CALL_PROLOG(fn) { _P_function = (fn); _P_next_methods = dylanXinternalX_P_false; }
#define IEP_CALL_N(fn) (Z)(((FN*)(fn))->iep)(
#define IEP_CALL_0(fn) IEP_CALL_N(fn))
#define IEP_CALL_1(fn,a1) IEP_CALL_N(fn)(a1))
#define IEP_CALL_2(fn,a1,a2) IEP_CALL_N(fn)(a1),(a2))
#define IEP_CALL_3(fn,a1,a2,a3) IEP_CALL_N(fn)(a1),(a2),(a3))
#define IEP_CALL_4(fn,a1,a2,a3,a4) IEP_CALL_N(fn)(a1),(a2),(a3),(a4))
#define IEP_CALL_5(fn,a1,a2,a3,a4,a5) IEP_CALL_N(fn)(a1),(a2),(a3),(a4),(a5))
#define IEP_CALL_6(fn,a1,a2,a3,a4,a5,a6) IEP_CALL_N(fn)(a1),(a2),(a3),(a4),(a5),(a6))
#define IEP_CALL_7(fn,a1,a2,a3,a4,a5,a6,a7) IEP_CALL_N(fn)(a1),(a2),(a3),(a4),(a5),(a6),(a7))
#define IEP_CALL_8(fn,a1,a2,a3,a4,a5,a6,a7,a8) IEP_CALL_N(fn)(a1),(a2),(a3),(a4),(a5),(a6),(a7),(a8))
#define IEP_CALL_9(fn,a1,a2,a3,a4,a5,a6,a7,a8,a9) IEP_CALL_N(fn)(a1),(a2),(a3),(a4),(a5),(a6),(a7),(a8),(a9))

extern Z xep_0(FN*,int);
extern Z xep_1(FN*,int,Z);
extern Z xep_2(FN*,int,Z,Z);
extern Z xep_3(FN*,int,Z,Z,Z);
extern Z xep_4(FN*,int,Z,Z,Z,Z);
extern Z xep_5(FN*,int,Z,Z,Z,Z,Z);
extern Z xep_6(FN*,int,Z,Z,Z,Z,Z,Z);
extern Z xep_7(FN*,int,Z,Z,Z,Z,Z,Z,Z);
extern Z xep_8(FN*,int,Z,Z,Z,Z,Z,Z,Z,Z);
extern Z xep_9(FN*,int,Z,Z,Z,Z,Z,Z,Z,Z,Z);
extern Z xep(FN*,int,...);
extern Z optional_xep(FN*,int,...);
/*
extern Z key_xep(FN*,int,...);
extern Z rest_xep(FN*,int,...);
 */
extern Z gf_xep_0(FN*,int);
extern Z gf_xep_1(FN*,int,Z);
extern Z gf_xep_2(FN*,int,Z,Z);
extern Z gf_xep_3(FN*,int,Z,Z,Z);
extern Z gf_xep_4(FN*,int,Z,Z,Z,Z);
extern Z gf_xep_5(FN*,int,Z,Z,Z,Z,Z);
extern Z gf_xep_6(FN*,int,Z,Z,Z,Z,Z,Z);
extern Z gf_xep_7(FN*,int,Z,Z,Z,Z,Z,Z,Z);
extern Z gf_xep_8(FN*,int,Z,Z,Z,Z,Z,Z,Z,Z);
extern Z gf_xep_9(FN*,int,Z,Z,Z,Z,Z,Z,Z,Z,Z);
extern Z gf_xep(FN*,int,...);
extern Z gf_optional_xep(FN*,int,...);
extern Z optional_mep(Z,...);
/*
extern Z key_mep(Z,...);
 */

extern Z dylanXinternalX_L_function_class_G_[];
extern Z _P_values[];
extern int _P_number_values;
extern Z dylanXinternalX_L_simple_object_vector_G_[];
extern Z dylanXinternalX_L_simple_object_vector_G__wrapper[];
extern Z dylanXinternalX_L_traceable_value_cell_G__wrapper[];
extern Z dylanXinternalX_L_byte_string_G_[];

/*
#ifdef _GNUC_
#include <alloca.h>

INLINE Z dylanXinternalXprimitive_stack_allocate(int size) {
  return((Z)(alloca(size*sizeof(Z)))); 
}	

INLINE Z dylanXinternalXprimitive_stack_allocate_vector(int size) {
  if (size == 0)
    return(dylanXinternalX_P_sv_empty);
  else {
    SOV* vector = (SOV*)dylanXinternalXprimitive_stack_allocate(size+2);
    vector_class_setter(dylanXinternalX_L_simple_object_vector_G_,vector);
    vector_size_setter(size, vector);
    return(vector);
  }
}	

INLINE Z dylanXinternalXprimitive_stack_allocate_vector_from_buffer(int size, Z* buffer) {
  SOV* vector = (SOV*)dylanXinternalXprimitive_stack_allocate_vector(size);
  dylanXinternalXprimitive_replace_E_(vector_data(vector), buffer, size);
  return((Z)vector);
}	

INLINE Z dylanXinternalXprimitive_stack_allocate_vector_from_buffer_with_offset
    (int size, Z* buffer, int offset) {
  return (dylanXinternalXprimitive_stack_allocate_vector_from_buffer
            (size, &buffer[offset]));
}	

#endif
*/

#include "threads-run-time.h"
