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
#define pc(a) putchar(a);putchar(10);

#define MAYBE_EXTERN
#include "c-run-time.h"
/* #undef MAYBE_EXTERN */

/* externs from boot.dylan */

extern Z dylanXinternalX_P_false[];
extern Z dylanXinternalX_P_sv_empty[];
extern Z dylanXinternalXargument_count_error[];
extern Z dylanXinternalXargument_count_overflow_error[];
extern Z dylanXinternalXstack_overflow_error[];
extern Z dylanXinternalXunknown_keyword_argument_error[];
extern Z dylanXinternalXodd_keyword_arguments_error[];
extern Z dylanXinternalX_P_argument_buffer;

/* GC SETUP */

#ifdef WITH_GC
#include "gc.h"

/*
extern Z JB_GC_malloc(int);
*/

#define malloc(n) GC_malloc(n)
#define calloc(m,n) GC_malloc((m)*(n))
#endif

/****************************************************************************
 ***** PRIMITIVES ***********************************************************
 ****************************************************************************/
 
/* 
 * MEMORY 
 */
 
/*
Z JB_GC_malloc(int size) {
  Z marker = &marker;

  memset((Z)((unsigned long)marker - 100 - 64000), 0xa9, 64000);
  return((Z)GC_malloc(size));
}
*/
 
Z dylanXinternalXprimitive_allocate(int size) {

  return((Z)(malloc(size*sizeof(Z))));
}	

Z dylanXinternalXprimitive_byte_allocate(int word_size, int byte_size) {
  int   total_size = word_size*sizeof(Z) + byte_size + 1;
  char* blob = (char*)(malloc(total_size));
  blob[total_size-1] = (char)0; /* null terminate */
  return((Z)blob);
}	

#ifdef NO_ALLOCA
#define alloca GC_malloc
#endif

#define dylanXinternalXprimitive_stack_allocate(size) ((Z)(alloca((size)*sizeof(Z))))

#define dylanXinternalXprimitive_stack_allocate_vector(size) \
  ((SOV*)dylanXinternalXprimitive_stack_allocate(size + VECTOR_HEADER_SIZE))
  
Z dylanXinternalXprimitive_element(Z storage[], int index) {
  return(storage[index]);
}

Z dylanXinternalXprimitive_element_setter(Z new_value, Z storage[], int index) {
  storage[index] = new_value;
  return(new_value);
}

Z dylanXinternalXprimitive_byte_element
    (char storage[], int base_offset, int byte_offset) {
  return((Z)(long)storage[base_offset * sizeof(Z) + byte_offset]);
}

int dylanXinternalXprimitive_byte_element_setter
    (int new_value, char storage[], int base_offset, int byte_offset) {
  storage[base_offset * sizeof(Z) + byte_offset] = (char)new_value;
  return(new_value);
}

Z dylanXinternalXprimitive_fill_E_(Z storage[], int size, Z value) {
  register int i;
  Z* ptr = storage;
  for (i=0;i<size;i++)
    *ptr++ = value;
  return(storage);
}

Z dylanXinternalXprimitive_replace_E_(Z dst[], Z src[], int size) {
  memcpy(dst, src, size * sizeof(Z));
  /*
  register int i;
  register Z* d = dst;
  register Z* s = src;
  for (i=0;i<size;i++)
    *d++ = *s++;
  */
  return(dst);
}

Z dylanXinternalXprimitive_more_argument(Z context[], int offset) {
  return context[offset];
}

int dylanXinternalXprimitive_integer_data(Z instance) {
  return(((long)instance)>>2);
}

Z dylanXinternalXprimitive_integer(long integer) {
  return((Z)((integer<<2)|1));
}

/* 
 * GENERIC DATATYPE ACCESSORS
 */
 
UINT8 dylanXinternalXprimitive_unsigned_8_bit_integer_at (void *address) {
  return(*((UINT8*)address));
}

UINT8 dylanXinternalXprimitive_unsigned_8_bit_integer_at_setter (UINT8 x, void *address) {
  *((UINT8*)address) = x;
  return(x);
}

INT8 dylanXinternalXprimitive_signed_8_bit_integer_at (void *address) {
  return(*((INT8*)address));
}

INT8 dylanXinternalXprimitive_signed_8_bit_integer_at_setter (INT8 x, void *address) {
  *((INT8*)address) = x;
  return(x);
}

UINT16 dylanXinternalXprimitive_unsigned_16_bit_integer_at (void *address) {
  return(*((UINT16*)address));
}

UINT16 dylanXinternalXprimitive_unsigned_16_bit_integer_at_setter (UINT16 x, void *address) {
  *((UINT16*)address) = x;
  return(x);
}

INT16 dylanXinternalXprimitive_signed_16_bit_integer_at (void *address) {
  return(*((INT16*)address));
}

INT16 dylanXinternalXprimitive_signed_16_bit_integer_at_setter (INT16 x, void *address) {
  *((INT16*)address) = x;
  return(x);
}

UINT32 dylanXinternalXprimitive_unsigned_32_bit_integer_at (void *address) {
  return(*((UINT32*)address));
}

UINT32 dylanXinternalXprimitive_unsigned_32_bit_integer_at_setter (UINT32 x, void *address) {
  *((UINT32*)address) = x;
  return(x);
}

INT32 dylanXinternalXprimitive_signed_32_bit_integer_at (void *address) {
  return(*((INT32*)address));
}

INT32 dylanXinternalXprimitive_signed_32_bit_integer_at_setter (INT32 x, void *address) {
  *((INT32*)address) = x;
  return(x);
}

UINT64 dylanXinternalXprimitive_unsigned_64_bit_integer_at (void *address) {
  return(*((UINT64*)address));
}

UINT64 dylanXinternalXprimitive_unsigned_64_bit_integer_at_setter (UINT64 x, void *address) {
  *((UINT64*)address) = x;
  return(x);
}

INT64 dylanXinternalXprimitive_signed_64_bit_integer_at (void *address) {
  return(*((INT64*)address));
}

INT64 dylanXinternalXprimitive_signed_64_bit_integer_at_setter (INT64 x, void *address) {
  *((INT64*)address) = x;
  return(x);
}

FLT dylanXinternalXprimitive_ieee_single_float_at (void *address) {
  return(*((FLT*)address));
}

FLT dylanXinternalXprimitive_ieee_single_float_at_setter (FLT x, void *address) {
  *((FLT*)address) = x;
  return(x);
}

DFLT dylanXinternalXprimitive_ieee_double_float_at (void *address) {
  return(*((DFLT*)address));
}

DFLT dylanXinternalXprimitive_ieee_double_float_at_setter (DFLT x, void *address) {
  *((DFLT*)address) = x;
  return(x);
}

/*
EFLT dylanXinternalXprimitive_ieee_double_float_at (void *address) {
  return(*((EFLT*)address));
}

EFLT dylanXinternalXprimitive_ieee_double_float_at_setter (EFLT x, void *address) {
  *((EFLT*)address) = x;
  return(x);
}
*/

/*
 * SINGLE-FLOAT
 */

/* 
 * PRIMITIVE FLOATING POINT CONVERSION 
 */

float zed_to_single_float(Z z) { ZFLT zflt; zflt.z = z; return(zflt.f); }
Z single_float_to_zed(float f) { ZFLT zflt; zflt.f = f; return(zflt.z); }
double zed_to_double_float(Z z) { ZFLT zflt; zflt.z = z; return((double)zflt.f); }
Z double_float_to_zed(double f) { ZFLT zflt; zflt.f = (float)f; return(zflt.z); }

float integer_to_single_float(int i) { INTFLT intflt; intflt.i = i; return(intflt.f); }
int single_float_to_integer(float f) { INTFLT intflt; intflt.f = f; return(intflt.i); }
double big_integer_to_double_float(INT64 i) 
  { INTDFLT intdflt; intdflt.i = i; return(intdflt.f); }
INT64 double_float_to_big_integer(double f)
  { INTDFLT intdflt; intdflt.f = f; return(intdflt.i); }

float dylanXinternalXprimitive_decoded_bits_as_single_float
        (int sign, int exponent, int significand) {
  long i,m;

  i  = (sign==0)?0:-1;
  m  = (1<<24)-1;
  i &= ~m;
  i |= significand;
  m  = (1<<8)-1;
  i  = (~(m<<23)&i)|(((exponent)&m)<<23);
  return(zed_to_single_float((Z)i));
}

/*
 * VARIABLES
 */

Z dylanXinternalXprimitive_variable_lookup(void* variable_pointer) {
  return(*((Z*)variable_pointer));
}

Z dylanXinternalXprimitive_variable_lookup_setter(void* new_value, void* variable_pointer) {
  return(*((Z*)variable_pointer) = (Z)new_value);
}

/*
 * OPERATING SYSTEM SUPPORT
 */

/* 
 * INPUT/OUTPUT 
 */

FILE* dylanXinternalXprimitive_open_input_terminal (char* name) {
  FILE *fd;
  
  if (strcmp(name, "input")==0)
    return stdin;
  else {
    fd = fopen(name, "r");
    if (fd == NULL)
      printf("UNABLE TO OPEN %s\n", name);
    return fd;
  }
}

FILE* dylanXinternalXprimitive_open_output_terminal (char* name) {
  FILE *fd;
  if (strcmp(name, "output")==0)
    return stdout;
  else {
    fd = fopen(name, "w");
    /* !@#$ THIS SHOULD BE A SIGNAL */
    if (fd == NULL)
      printf("UNABLE TO OPEN %s\n", name);
    return fd;
  }
}

/*
 * SIGNAL HANDLING
 */

#include <signal.h>

int dylanXinternalX_P_signal_number;

extern void c_softcore_signal_handler(int signal, ...);

void c_softcore_signal_handler(int the_signal, ...) {
  SABOTAGE_STACK();
  signal(the_signal, (void (*)())c_softcore_signal_handler);
}

extern void c_hardcore_signal_handler(int signal, ...);

extern Z dylanXinternalX_T_last_top_level_T_;

void c_hardcore_signal_handler(int the_signal, ...) {
  dylanXinternalX_P_signal_number = the_signal;
  if (dylanXinternalX_T_last_top_level_T_ != dylanXinternalX_P_false) {
    signal(the_signal, (void (*)())c_hardcore_signal_handler);
    CALL0(dylanXinternalX_T_last_top_level_T_);
  } else /* FLAME */
    dylanXinternalXprimitive_break();
}

Z dylanXinternalXprimitive_setup_c_handlers() {
  signal(SIGINT, (void (*)())c_softcore_signal_handler);  
  signal(SIGSEGV,(void (*)())c_hardcore_signal_handler);  
#ifndef WIN32
  signal(SIGBUS, (void (*)())c_hardcore_signal_handler);  
#endif
  return(dylanXinternalX_P_false);
}

int dylanXinternalXprimitive_break() {
  int *ptr = (int*)0;
  
  *ptr = 0; /* generate a memory fault */
}

int dylanXinternalXprimitive_error(char* string) {
  printf(string);
  dylanXinternalXprimitive_break();
}

#include <errno.h>

int dylanXinternalXprimitive_errno_value() {
  return(errno);
}

/*
 * SHELL
 */

Z dylanXinternalXprimitive_run_shell_command(FILE* stream, char* command) {
  /* !@#$ doesn't output result to stream yet */
  system(command);
}

/*
 * ENVIRONMENT-VARIABLE
 */

int dylanXinternalXprimitive_environment_variable_setter(char* new_value, char* name) {
#ifdef NeXT
  /* !@#$ doesn't work -- no putenv */
  return(new_value);
#else
  char name_value[100];
  
  dylanXinternalXprimitive_break(); 
  sprintf(name_value, "%s=%s", name, new_value);
  return(putenv(name_value));
#endif
}

#ifndef WIN32
/* 
 * OPERATING-SYSTEM-NAME
 */

#include <sys/utsname.h>

char* dylanXinternalXprimitive_operating_system_name() {
#ifdef NeXT
  return("NeXT");
#else
  char   *os_name;
  struct utsname name;

  uname(&name);
  os_name = (char*)malloc(strlen(1+name.sysname));
  strcpy(os_name,name.sysname);
  return((Z)os_name);
#endif
}
#endif

/*
 * FILE-EXISTS?
 */

#include <sys/types.h>
#include <sys/stat.h>

int dylanXinternalXprimitive_file_exists_Q_(char* filename) {
  struct stat stat_buffer;
  int result = stat(filename, &stat_buffer);

  return(result>=0);
}

/*
 * FILE-DATE
 */

/* !@#$ NEED TIME TYPE */

int dylanXinternalXprimitive_file_date(char* filename) {
  struct stat stat_buffer;

  int exists_p = stat(filename, &stat_buffer)>=0;
  return(exists_p ? (int)stat_buffer.st_mtime : 0);
}

int dylanXinternalXprimitive_file_date_setter(int time, char* filename) {
  struct stat stat_buffer;

  int exists_p = stat(filename, &stat_buffer)>=0;
  stat_buffer.st_mtime = (time_t)time;
  return(exists_p ? time : 0);
}

/****************************************************************************
 ***** ACCESSORS FOR BUILT-IN OBJECTS ***************************************
 ****************************************************************************/
 
INLINE Z class_meta_class(CLASS* class) {
  return (class->meta_class);
}

INLINE Z function_class(FN* function) {
  return(function->class);
}

INLINE ZFN function_xep(FN* function) {
  return(function->xep);
}

INLINE ZLFN function_mep(FN* function) {
  return(function->mep);
}

INLINE ZLFN function_iep(FN* function) {
  return(function->iep);
}

INLINE int function_number_required(FN* function) {
  return(dylanXinternalXprimitive_integer_data(function->properties) & 0xff);
}

INLINE int function_key_p(FN* function) {
  return((dylanXinternalXprimitive_integer_data(function->properties) & 0x10000) > 0);
}

INLINE int function_optionals_p(FN* function) {
  return((dylanXinternalXprimitive_integer_data(function->properties) & 0x50000) > 0);
}

INLINE Z method_keyword_specifiers(FN* method) {
  return(method->keyword_specifiers);
}

#define VECTOR_HEADER_SIZE 2

INLINE Z vector_class_setter(Z class, SOV* vector) {
  vector->class = class;
  return class;
}

INLINE int vector_size(SOV* vector) {
  return(dylanXinternalXprimitive_integer_data(vector->size));
}

INLINE int vector_size_setter(int size, SOV* vector) {
  vector->size = dylanXinternalXprimitive_integer(size);
  return size;
}

INLINE Z vector_data(SOV* vector) {
  return(vector->data);
}

#define SOV_instance_header dylanXinternalX_L_simple_object_vector_G__wrapper

Z dylanXinternalXprimitive_replace_vector_E_(SOV* dst, SOV* src) {
  int size = vector_size(src);
  vector_class_setter(SOV_instance_header, dst);
  vector_size_setter(size, dst);
  dylanXinternalXprimitive_replace_E_
    (vector_data((SOV*)dst),vector_data((SOV*)src),size);
  return((Z)dst);
}

Z dylanXinternalXprimitive_allocate_vector(int size) {
  if (size == 0) 
    return(dylanXinternalX_P_sv_empty);
  else {
    SOV* vector = dylanXinternalXprimitive_allocate(size+2);
    vector_class_setter(SOV_instance_header, vector);
    vector_size_setter(size, vector);
    return((Z)vector);
  }
}	

Z dylanXinternalXprimitive_copy_vector(Z vector) {
  int size = vector_size((SOV*)vector);
  Z new_vector = dylanXinternalXprimitive_allocate_vector (size);
  
  dylanXinternalXprimitive_replace_E_
    (vector_data((SOV*)new_vector),vector_data((SOV*)vector),size);
  return(new_vector);
}	

INLINE Z dylanXinternalXprimitive_initialize_vector_from_buffer
    (SOV* vector, int size, Z* buffer) {
  vector_class_setter(SOV_instance_header, vector);
  vector_size_setter(size, vector);
  dylanXinternalXprimitive_replace_E_(vector_data(vector), buffer, size);
  return((Z)vector);
}	

/***** STRING CREATION FROM C-STRINGS *****/

Z dylanXinternalXprimitive_make_string(char *string) {
  int new_size = strlen(string);
  Z new_string =
     dylanXinternalXprimitive_allocate_vector(new_size/sizeof(Z) + 1);
  vector_class_setter(dylanXinternalX_L_byte_string_G_,(SOV*)new_string);
  strcpy((char*)vector_data((SOV*)new_string), string);
  vector_size_setter(new_size, (SOV*)new_string);
  return(new_string);
}	

Z dylanXinternalXprimitive_make_argument_vector (int argc, char *argv[]) {
  Z args = dylanXinternalXprimitive_allocate_vector(argc);
  Z* data = (Z*)vector_data((SOV*)args);
  int i;

  for (i=0; i<argc; i++) 
    data[i] = dylanXinternalXprimitive_make_string(argv[i]);
  return(args);
}

/***** VARARGS *****/

INLINE void transfer_varargs(va_list ap, int argument_count, Z* arguments) {
  int i;
  
  for(i=0; i<argument_count; i++)
    arguments[i] = va_arg(ap, Z);
}

#define BUFFER_VARARGS(argument_count, buffer) \
{ va_list ap; va_start(ap,(argument_count)); \
  transfer_varargs(ap, (argument_count), (buffer)); va_end(ap); }

/****************************************************************************
 ***** BUILT-IN FUNCTIONS ***************************************************
 ****************************************************************************/
 
/* 
 * NON-LOCAL EXITS 
 */

Unwind_protect_frame* _P_current_unwind_protect_frame = (Unwind_protect_frame*)0;

void nlx_step (Bind_exit_frame* ultimate_destination) {
    /* handled all unwind protect frames presently in force? */
  if (_P_current_unwind_protect_frame == 
      ultimate_destination->present_unwind_protect_frame)
    longjmp(ultimate_destination->destination, 1);
  else if (_P_current_unwind_protect_frame == (Unwind_protect_frame*)0)
    dylanXinternalXprimitive_error("Uncaught non-local exit");
  else {
    Unwind_protect_frame* next_frame = _P_current_unwind_protect_frame;
      /* pop current unwind protect frame */
    _P_current_unwind_protect_frame = next_frame->previous_unwind_protect_frame;
    /*
    if (_P_current_unwind_protect_frame)
    */
      /* register ultimate destination of non-local exit in cupf */
      _P_current_unwind_protect_frame->ultimate_destination = ultimate_destination;
      /* do cleanup step in next unwind protect frame */
    longjmp(next_frame->destination, 1);
  }
}
    
Z dylanXinternalXprimitive_continue_unwind () {
  if (_P_current_unwind_protect_frame->ultimate_destination) /* nlx? */
    nlx_step(_P_current_unwind_protect_frame->ultimate_destination);
  else {
    /* restore saved values from current unwind protect frame */
    dylanXinternalXprimitive_replace_E_
      (_P_values,
       _P_current_unwind_protect_frame->multiple_values, 
       _P_current_unwind_protect_frame->multiple_value_count);
    _P_number_values = _P_current_unwind_protect_frame->multiple_value_count;
    return(_P_values[0]);
  }
}

Z dylanXinternalXprimitive_nlx (Bind_exit_frame* target, SOV* arguments) {
  int size = vector_size(arguments);
  
  /* save the arguments for returning as values */
  
  dylanXinternalXprimitive_replace_vector_E_((SOV*)(&target->return_values), arguments);
  
  nlx_step(target);
}

Z dylanXinternalXprimitive_inlined_nlx (Bind_exit_frame* target, Z first_argument) {
  int size = (int)_P_number_values;
  
  /* save the arguments (stored in multiple values area) */
  /* for returning as values */
  
  vector_class_setter(SOV_instance_header, (SOV*)(&target->return_values));
  vector_size_setter(size, (SOV*)(&target->return_values));
  dylanXinternalXprimitive_replace_E_
    (vector_data((SOV*)(target->return_values)), _P_values, size);
  
  nlx_step(target);
}

/* 
 * ENVIRONMENT
 */

Z* dylanXinternalXprimitive_make_box(Z object) {
  Z* box = (Z*)dylanXinternalXprimitive_allocate(2);
  box[0] = dylanXinternalX_L_traceable_value_cell_G__wrapper;
  box[1] = object;
  return(box);
}

/* PRIMITIVE-MAKE-ENVIRONMENT */

Z* dylanXinternalXprimitive_make_environment(int size, ...) {
  va_list ap;
  int i;
  SOV* environment = dylanXinternalXprimitive_allocate_vector(size);
  Z* data = vector_data(environment);

  va_start(ap,size);
  for (i=0; i<size; i++)
    data[i] = va_arg(ap, Z);
  va_end(ap);
  return((Z*)environment);
}	

/****************************************************************************
 ***** XEP CALLING **********************************************************
 ****************************************************************************/

/*
 * FOUNDATION DEFINITIONS 
 */
 
int dylanXinternalX_P_bottom_of_stack;
int dylanXinternalX_P_stack_check_enabled_Q_;
int dylanXinternalX_P_max_stack_size;
int dylanXinternalX_P_real_max_stack_size;
 
#define CONTEXT extern

/* Put the context in a single variable shared amongst all activations.
   This is done to reduce the rate of growth of the stack. */
 
Z arguments[ARGUMENT_COUNT_MAX];
Z new_arguments[ARGUMENT_COUNT_MAX];

INLINE int STACK_OK(int bottom, int current) {
  int delta, overflow_p;
#ifdef STACK_GROWS_UP
  delta = current - bottom;
#else
  delta = bottom - current;
#endif
  overflow_p = delta > (int)dylanXinternalX_P_max_stack_size;
  return (overflow_p);
}

INLINE int FUNCTION_OK(FN* function) {
  return(class_meta_class(function_class(function)) ==
         dylanXinternalX_L_function_class_G_);
}

INLINE void REQUIRED_ARGUMENT_CHECK
    (FN* function, int number_required, int argument_count) {
  if (argument_count != number_required)
    IEP_CALL_2(dylanXinternalXargument_count_error, function, argument_count);
}

INLINE void OPTIONAL_ARGUMENT_CHECK
    (FN* function, int number_required, int argument_count) {
  if (argument_count < number_required)
    IEP_CALL_2(dylanXinternalXargument_count_error, function, argument_count);
}

INLINE void KEYWORD_ARGUMENT_CHECK
    (FN* function, int number_required, int argument_count) {
  if ((argument_count - number_required) & 1)
    IEP_CALL_2(dylanXinternalXodd_keyword_arguments_error, function, (Z)(long)argument_count); 
}

INLINE void SIMPLE_CALL_CHECK(FN* function) {
  int stack_marker;
  
  if (STACK_OK((long)dylanXinternalX_P_bottom_of_stack,(long)(&stack_marker)))
    IEP_CALL_0(dylanXinternalXstack_overflow_error);
}    

INLINE void CALL_CHECK(FN* function, int argument_count) {
  SIMPLE_CALL_CHECK(function);
  if (argument_count > ARGUMENT_COUNT_MAX)
    IEP_CALL_2(dylanXinternalXargument_count_overflow_error,function,argument_count);
}    

/*
 * OPTIONAL ARGUMENT PROCESSING
 */

/* 
 * BUILDING BLOCKS
 */

extern Z _P_S_allow_other_keys;

INLINE void process_keyword_parameters_with_checking
    (FN* function, int number_required, 
     int number_keywords, Z keyword_specifiers[], 
     int argument_count, Z arguments[], Z new_arguments[]) { 
  int i,j,k;     

  int allow_other_keys_p = 0;
  int size_keyword_specifiers = number_keywords * 2;
  for (i=argument_count-1; i>=number_required;) {
    Z value   = arguments[i--];
    Z keyword = arguments[i--];
    for (j=0,k=number_required;;k++,j+=2) {
      if (j == size_keyword_specifiers) 
        if (!allow_other_keys_p)
          IEP_CALL_2(dylanXinternalXunknown_keyword_argument_error, function, keyword);
        else
          break;
      else {
        Z lambda_keyword = keyword_specifiers[j]; 
        /* !@#$ MIGHT NEED ANOTHER DEREFERENCE IF NO KEYWORD PATCHING */
        if (keyword == lambda_keyword) {
          new_arguments[k] = value;
          break;
        } else if (keyword == _P_S_allow_other_keys) {
          allow_other_keys_p = 1;
          break;
        } } } }
}	

INLINE void process_keyword_parameters
    (FN* function, int number_required,
     int number_keywords, Z keyword_specifiers[], 
     int number_optionals, Z optional_arguments[], Z new_arguments[]) { 
  int i,j,k;     

  int size_keyword_specifiers = number_keywords * 2;
  for (i = number_optionals - 1; i >= 0;) {
    Z value   = optional_arguments[i--];
    Z keyword = optional_arguments[i--];
    for (j = 0, k = number_required; j < size_keyword_specifiers; k++, j += 2) {
      Z lambda_keyword = keyword_specifiers[j]; 
      if (keyword == lambda_keyword) {
         new_arguments[k] = value;
         break;
      } } }
}	

INLINE void default_arguments
    (int number_required, Z* arguments, 
     int number_keywords, Z* keyword_specifiers,  
     int keyword_arguments_offset, Z* new_arguments) {	
  int i, j;

  /* copy arguments into staging ground */

  for (i=0; i<number_required; i++)
    new_arguments[i] = arguments[i];

  /* default keyword parameters */

  for (j=1, i=0; i < number_keywords; j += 2, i++) 
    new_arguments[i + keyword_arguments_offset] = keyword_specifiers[j];
}  

/* 
 * APPLY FOR INTERNAL ENTRY POINTS
 */

/* buffer used in apply code in boot.dylan to construct apply arguments */

INLINE Z dylanXinternalXprimitive_basic_iep_apply (FN* f, int argument_count, Z a[]) {

  ZLFN iep = function_iep(f);
  
  switch (argument_count) {
  case 0: return(iep());
  case 1: return(iep(a[0]));
  case 2: return(iep(a[0],a[1]));
  case 3: return(iep(a[0],a[1],a[2]));
  case 4: return(iep(a[0],a[1],a[2],a[3]));
  case 5: return(iep(a[0],a[1],a[2],a[3],a[4]));
  case 6: return(iep(a[0],a[1],a[2],a[3],a[4],a[5]));
  case 7: return(iep(a[0],a[1],a[2],a[3],a[4],a[5],a[6]));
  case 8: return(iep(a[0],a[1],a[2],a[3],a[4],a[5],a[6],a[7]));
  case 9: return(iep(a[0],a[1],a[2],a[3],a[4],a[5],a[6],a[7],a[8]));
  default:
  return(iep(a[ 0],a[ 1],a[ 2],a[ 3],a[ 4],a[ 5],a[ 6],a[ 7],a[ 8],a[ 9],
             a[10],a[11],a[12],a[13],a[14],a[15],a[16],a[17],a[18],a[19],
	     a[20],a[21],a[22],a[23],a[24],a[25],a[26],a[27],a[28],a[29],
	     a[30],a[31],a[32],a[33],a[34],a[35],a[36],a[37],a[38],a[39],
	     a[40],a[41],a[42],a[43],a[44],a[45],a[46],a[47],a[48],a[49],
	     a[50],a[51],a[52],a[53],a[54],a[55],a[56],a[57],a[58],a[59],
	     a[60],a[61],a[62],a[63]));
  }
}

INLINE Z dylanXinternalXprimitive_iep_apply (FN* f, int argument_count, Z a[]) {
  _P_function = f;  _P_next_methods = dylanXinternalX_P_false;
  return(dylanXinternalXprimitive_basic_iep_apply(f, argument_count, a));
}

/*
 * APPLY FOR METHOD ENTRY POINTS
 *
 * ONLY CALLED FROM %method-apply WHICH ASSUMES THAT f HAS OPTIONAL PARAMETERS
 *
 */

INLINE Z dylanXinternalXprimitive_mep_apply_with_optionals (FN* f, Z next_methods, Z* args) {

  ZLFN mep = function_mep(f);
  Z* a = vector_data((SOV*)args);
  
  _P_next_methods = next_methods; 
  _P_function = f;
  _P_argument_count = vector_size((SOV*)args);
  
  switch (_P_argument_count) {
  case 0: return(mep());
  case 1: return(mep(a[0]));
  case 2: return(mep(a[0],a[1]));
  case 3: return(mep(a[0],a[1],a[2]));
  case 4: return(mep(a[0],a[1],a[2],a[3]));
  case 5: return(mep(a[0],a[1],a[2],a[3],a[4]));
  case 6: return(mep(a[0],a[1],a[2],a[3],a[4],a[5]));
  case 7: return(mep(a[0],a[1],a[2],a[3],a[4],a[5],a[6]));
  case 8: return(mep(a[0],a[1],a[2],a[3],a[4],a[5],a[6],a[7]));
  case 9: return(mep(a[0],a[1],a[2],a[3],a[4],a[5],a[6],a[7],a[8]));
  default:
  return(mep(a[ 0],a[ 1],a[ 2],a[ 3],a[ 4],a[ 5],a[ 6],a[ 7],a[ 8],a[ 9],
             a[10],a[11],a[12],a[13],a[14],a[15],a[16],a[17],a[18],a[19],
	     a[20],a[21],a[22],a[23],a[24],a[25],a[26],a[27],a[28],a[29],
	     a[30],a[31],a[32],a[33],a[34],a[35],a[36],a[37],a[38],a[39],
	     a[40],a[41],a[42],a[43],a[44],a[45],a[46],a[47],a[48],a[49],
	     a[50],a[51],a[52],a[53],a[54],a[55],a[56],a[57],a[58],a[59],
	     a[60],a[61],a[62],a[63]));
  }}

INLINE Z dylanXinternalXprimitive_mep_apply (FN* function, Z next_methods, Z* args) {
  int  number_required = function_number_required(function);
  int  argument_count = vector_size((SOV*)args);
  if (function_optionals_p(function)) {
    OPTIONAL_ARGUMENT_CHECK(function,number_required,argument_count);
    { Z*  arguments = vector_data((SOV*)args);
      SOV* new_arguments_vector = (SOV*)new_arguments;
      Z*  new_arguments = vector_data(new_arguments_vector);
      int  optionals_count = argument_count - number_required;
      Z*  optional_arguments = &arguments[number_required];
      SOV* rest_arguments = (SOV*)dylanXinternalXprimitive_stack_allocate_vector(optionals_count);
      dylanXinternalXprimitive_initialize_vector_from_buffer
        (rest_arguments, optionals_count, optional_arguments);
      dylanXinternalXprimitive_replace_E_(new_arguments,arguments,number_required);
      new_arguments[number_required] = rest_arguments;
      vector_class_setter(SOV_instance_header, new_arguments_vector);
      vector_size_setter(number_required + 1, new_arguments_vector);
      return
        (dylanXinternalXprimitive_mep_apply_with_optionals
          (function, next_methods, (Z*)new_arguments_vector));
  }} else {
    REQUIRED_ARGUMENT_CHECK(function,number_required,argument_count);
    return
      (dylanXinternalXprimitive_mep_apply_with_optionals
         (function, next_methods, args));
  }}

/* 
 * APPLY FOR EXTERNAL ENTRY POINTS
 */

INLINE Z dylanXinternalXprimitive_xep_apply (FN* f, int argument_count, Z a[]) {

  ZLFN xep = (ZLFN)function_xep(f);
  _P_function = f;  _P_next_methods = dylanXinternalX_P_false;
  
  switch (argument_count) {
  case 0: return(xep(f,0));
  case 1: return(xep(f,1,a[0]));
  case 2: return(xep(f,2,a[0],a[1]));
  case 3: return(xep(f,3,a[0],a[1],a[2]));
  case 4: return(xep(f,4,a[0],a[1],a[2],a[3]));
  case 5: return(xep(f,5,a[0],a[1],a[2],a[3],a[4]));
  case 6: return(xep(f,6,a[0],a[1],a[2],a[3],a[4],a[5]));
  case 7: return(xep(f,7,a[0],a[1],a[2],a[3],a[4],a[5],a[6]));
  case 8: return(xep(f,8,a[0],a[1],a[2],a[3],a[4],a[5],a[6],a[7]));
  case 9: return(xep(f,9,a[0],a[1],a[2],a[3],a[4],a[5],a[6],a[7],a[8]));
  default:
  return(xep(f,argument_count,
             a[ 0],a[ 1],a[ 2],a[ 3],a[ 4],a[ 5],a[ 6],a[ 7],a[ 8],a[ 9],
             a[10],a[11],a[12],a[13],a[14],a[15],a[16],a[17],a[18],a[19],
	     a[20],a[21],a[22],a[23],a[24],a[25],a[26],a[27],a[28],a[29],
	     a[30],a[31],a[32],a[33],a[34],a[35],a[36],a[37],a[38],a[39],
	     a[40],a[41],a[42],a[43],a[44],a[45],a[46],a[47],a[48],a[49],
	     a[50],a[51],a[52],a[53],a[54],a[55],a[56],a[57],a[58],a[59],
	     a[60],a[61],a[62],a[63]));
  }
}

/* 
 * EXTERNAL ENTRY POINTS 
 */

/* 
 * REQUIRED EXTERNAL ENTRY POINT 
 */

Z xep_0(FN* function, int argument_count) {
  SIMPLE_CALL_CHECK(function);
  REQUIRED_ARGUMENT_CHECK(function,0,argument_count);
  _P_function = function;  _P_next_methods = dylanXinternalX_P_false;
  return(((ZLFN)function_iep(function))());
}

Z xep_1(FN* function, int argument_count, Z a1) {
  SIMPLE_CALL_CHECK(function);
  REQUIRED_ARGUMENT_CHECK(function,1,argument_count);
  _P_function = function;  _P_next_methods = dylanXinternalX_P_false;
  return(((ZLFN)function_iep(function))(a1));
}

Z xep_2(FN* function, int argument_count, Z a1, Z a2) {
  SIMPLE_CALL_CHECK(function);
  REQUIRED_ARGUMENT_CHECK(function,2,argument_count);
  _P_function = function;  _P_next_methods = dylanXinternalX_P_false;
  return(((ZLFN)function_iep(function))(a1,a2));
}

Z xep_3(FN* function, int argument_count, Z a1, Z a2, Z a3) {
  SIMPLE_CALL_CHECK(function);
  REQUIRED_ARGUMENT_CHECK(function,3,argument_count);
  _P_function = function;  _P_next_methods = dylanXinternalX_P_false;
  return(((ZLFN)function_iep(function))(a1,a2,a3));
}

Z xep_4(FN* function, int argument_count, Z a1, Z a2, Z a3, Z a4) {
  SIMPLE_CALL_CHECK(function);
  REQUIRED_ARGUMENT_CHECK(function,4,argument_count);
  _P_function = function;  _P_next_methods = dylanXinternalX_P_false;
  return(((ZLFN)function_iep(function))(a1,a2,a3,a4));
}

Z xep_5(FN* function, int argument_count, Z a1, Z a2, Z a3, Z a4, Z a5) {
  SIMPLE_CALL_CHECK(function);
  REQUIRED_ARGUMENT_CHECK(function,5,argument_count);
  _P_function = function;  _P_next_methods = dylanXinternalX_P_false;
  return(((ZLFN)function_iep(function))(a1,a2,a3,a4,a5));
}

Z xep_6(FN* function, int argument_count, Z a1, Z a2, Z a3, Z a4, Z a5, Z a6) {
  SIMPLE_CALL_CHECK(function);
  REQUIRED_ARGUMENT_CHECK(function,6,argument_count);
  _P_function = function;  _P_next_methods = dylanXinternalX_P_false;
  return(((ZLFN)function_iep(function))(a1,a2,a3,a4,a5,a6));
}

Z xep_7(FN* function, int argument_count, Z a1, Z a2, Z a3, Z a4, Z a5, Z a6, Z a7) {
  SIMPLE_CALL_CHECK(function);
  REQUIRED_ARGUMENT_CHECK(function,7,argument_count);
  _P_function = function;  _P_next_methods = dylanXinternalX_P_false;
  return(((ZLFN)function_iep(function))(a1,a2,a3,a4,a5,a6,a7));
}

Z xep_8(FN* function, int argument_count, Z a1, Z a2, Z a3, Z a4, Z a5, Z a6, Z a7, Z a8) {
  SIMPLE_CALL_CHECK(function);
  REQUIRED_ARGUMENT_CHECK(function,8,argument_count);
  _P_function = function;  _P_next_methods = dylanXinternalX_P_false;
  return(((ZLFN)function_iep(function))(a1,a2,a3,a4,a5,a6,a7,a8));
}

Z xep_9(FN* function, int argument_count, Z a1, Z a2, Z a3, Z a4, Z a5, Z a6, Z a7, Z a8, Z a9) {
  SIMPLE_CALL_CHECK(function);
  REQUIRED_ARGUMENT_CHECK(function,9,argument_count);
  _P_function = function;  _P_next_methods = dylanXinternalX_P_false;
  return(((ZLFN)function_iep(function))(a1,a2,a3,a4,a5,a6,a7,a8,a9));
}

Z xep(FN* function, int argument_count, ...) {
  CALL_CHECK(function,argument_count);
  REQUIRED_ARGUMENT_CHECK
    (function,function_number_required(function),argument_count);
  BUFFER_VARARGS(argument_count, arguments);
  return(dylanXinternalXprimitive_iep_apply(function, argument_count, arguments));
}

/* 
 * REST AND KEYWORD EXTERNAL ENTRY POINT 
 */

Z optional_xep (FN* function, int argument_count, ...) {
  CALL_CHECK(function,argument_count);
  { int number_required = function_number_required(function);
  OPTIONAL_ARGUMENT_CHECK(function,number_required,argument_count);
  
  BUFFER_VARARGS(argument_count, arguments);
  
  { SOV* keyword_specifier_vector = method_keyword_specifiers(function);
    int  number_keywords = vector_size(keyword_specifier_vector) / 2;
    Z*  keyword_specifiers = vector_data(keyword_specifier_vector);
    int  new_argument_count = number_required + number_keywords + 1;
 
  if (function_key_p(function)) {
    KEYWORD_ARGUMENT_CHECK(function,number_required,argument_count);
    default_arguments(number_required, arguments, number_keywords, 
                      keyword_specifiers, number_required+1, new_arguments);
  } else
    dylanXinternalXprimitive_replace_E_(new_arguments,arguments,number_required);
		    
  { int optionals_count = argument_count-number_required;
    Z* optional_arguments = &arguments[number_required];

  if (function_key_p(function)) 
    process_keyword_parameters
      (function, number_required, number_keywords, keyword_specifiers, 
       optionals_count, optional_arguments, new_arguments); 

  {SOV* rest_arguments = dylanXinternalXprimitive_stack_allocate_vector(optionals_count);
   dylanXinternalXprimitive_initialize_vector_from_buffer
     (rest_arguments, optionals_count, optional_arguments);
  new_arguments[number_required + number_keywords] = rest_arguments;
  
  return(dylanXinternalXprimitive_iep_apply(function, new_argument_count, new_arguments));
}}}}}

/*
 * THE FOLLOWING TWO ARE NO LONGER USED
 */

/* 
 * KEYWORD ONLY EXTERNAL ENTRY POINT 
 *

Z key_xep (FN* function, int argument_count, ...) {
  CALL_CHECK(function,argument_count);
  { int number_required = function_number_required(function);
  KEYWORD_ARGUMENT_CHECK(function,number_required,argument_count);
  
  BUFFER_VARARGS(argument_count, arguments);

  { SOV* keyword_specifier_vector = method_keyword_specifiers(function);
    int  number_keywords = vector_size(keyword_specifier_vector) / 2;
    Z*  keyword_specifiers = vector_data(keyword_specifier_vector);
    int  new_argument_count = number_required + number_keywords;

  default_arguments(number_required, arguments, number_keywords, 
                    keyword_specifiers, number_required, new_arguments);
		    
  process_keyword_parameters_with_checking
    (function, number_required, number_keywords, keyword_specifiers, 
     argument_count, arguments, new_arguments); 
     
  return(dylanXinternalXprimitive_iep_apply (function, new_argument_count, new_arguments));
}}}
  
 *
 * REST ONLY EXTERNAL ENTRY POINT 
 *

Z rest_xep (FN* function, int argument_count, ...) {
  int number_required = function_number_required(function);
  CALL_CHECK(function, argument_count);
  OPTIONAL_ARGUMENT_CHECK(function, number_required, argument_count);
  BUFFER_VARARGS(argument_count, arguments);

  dylanXinternalXprimitive_replace_E_(new_arguments,arguments,number_required);
    
  {SOV* rest_arguments = dylanXinternalXprimitive_stack_allocate_vector(optionals_count);
   dylanXinternalXprimitive_initialize_vector_from_buffer
     (rest_arguments, argument_count-number_required, &arguments[number_required]);
  new_arguments[number_required] = rest_arguments;

  return(dylanXinternalXprimitive_iep_apply(function, number_required + 1, new_arguments));
}}

 */

/*
 * REQUIRED EXTERNAL ENTRY POINT FOR GENERIC-FUNCTION
 */

#define NUMBER_GENERIC_FUNCTION_TEMPLATES 7

Z gf_xep_0(FN* function, int argument_count) {
  SIMPLE_CALL_CHECK(function);
  REQUIRED_ARGUMENT_CHECK(function,0,argument_count);
  _P_function = function; 
  return(((ZLFN)function_iep(function))());
}

Z gf_xep_1(FN* function, int argument_count, Z a1) {
  SIMPLE_CALL_CHECK(function);
  REQUIRED_ARGUMENT_CHECK(function,1,argument_count);
  _P_function = function; 
  return(((ZLFN)function_iep(function))(a1));
}

Z gf_xep_2(FN* function, int argument_count, Z a1, Z a2) {
  SIMPLE_CALL_CHECK(function);
  REQUIRED_ARGUMENT_CHECK(function,2,argument_count);
  _P_function = function; 
  return(((ZLFN)function_iep(function))(a1,a2));
}

Z gf_xep_3(FN* function, int argument_count, Z a1, Z a2, Z a3) {
  SIMPLE_CALL_CHECK(function);
  REQUIRED_ARGUMENT_CHECK(function,3,argument_count);
  _P_function = function; 
  return(((ZLFN)function_iep(function))(a1,a2,a3));
}

Z gf_xep_4(FN* function, int argument_count, Z a1, Z a2, Z a3, Z a4) {
  SIMPLE_CALL_CHECK(function);
  REQUIRED_ARGUMENT_CHECK(function,4,argument_count);
  _P_function = function; 
  return(((ZLFN)function_iep(function))(a1,a2,a3,a4));
}

Z gf_xep_5(FN* function, int argument_count, Z a1, Z a2, Z a3, Z a4, Z a5) {
  SIMPLE_CALL_CHECK(function);
  REQUIRED_ARGUMENT_CHECK(function,5,argument_count);
  _P_function = function; 
  return(((ZLFN)function_iep(function))(a1,a2,a3,a4,a5));
}

Z gf_xep_6(FN* function, int argument_count, Z a1, Z a2, Z a3, Z a4, Z a5, Z a6) {
  SIMPLE_CALL_CHECK(function);
  REQUIRED_ARGUMENT_CHECK(function,6,argument_count);
  _P_function = function; 
  return(((ZLFN)function_iep(function))(a1,a2,a3,a4,a5,a6));
}

/*
Z gf_xep_7(FN* function, int argument_count, Z a1, Z a2, Z a3, Z a4, Z a5, Z a6, Z a7) {
  SIMPLE_CALL_CHECK(function);
  REQUIRED_ARGUMENT_CHECK(function,7,argument_count);
  _P_function = function; 
  return(((ZLFN)function_iep(function))(a1,a2,a3,a4,a5,a6,a7));
}

Z gf_xep_8(FN* function, int argument_count, Z a1, Z a2, Z a3, Z a4, Z a5, Z a6, Z a7, Z a8) {
  SIMPLE_CALL_CHECK(function);
  REQUIRED_ARGUMENT_CHECK(function,8,argument_count);
  _P_function = function; 
  return(((ZLFN)function_iep(function))(a1,a2,a3,a4,a5,a6,a7,a8));
}

Z gf_xep_9(FN* function, int argument_count, Z a1, Z a2, Z a3, Z a4, Z a5, Z a6, Z a7, Z a8, Z a9) {
  SIMPLE_CALL_CHECK(function);
  REQUIRED_ARGUMENT_CHECK(function,9,argument_count);
  _P_function = function; 
  return(((ZLFN)function_iep(function))(a1,a2,a3,a4,a5,a6,a7,a8,a9));
}
*/

/* DEFAULT-DISCRIMINATOR -- MUST AGREE WITH DISCRIMINATOR TEMPLATES */

Z gf_xep(FN* function, int argument_count, ...) {
  CALL_CHECK(function,argument_count);
  { int number_required = function_number_required(function);
  REQUIRED_ARGUMENT_CHECK (function,number_required,argument_count);
  BUFFER_VARARGS(argument_count, arguments);
  
  {SOV* new_arguments = dylanXinternalXprimitive_stack_allocate_vector(number_required);
   dylanXinternalXprimitive_initialize_vector_from_buffer
     (new_arguments, number_required, arguments);
  _P_function = function; 
  return((function_iep(function))(new_arguments));
}}}

/*
 * OPTIONAL EXTERNAL ENTRY POINT FOR GENERIC-FUNCTION
 */

Z gf_optional_xep (FN* function, int argument_count, ...) {
  CALL_CHECK(function, argument_count);
  { int number_required = function_number_required(function);
    int optionals_count = argument_count - number_required;
    
  OPTIONAL_ARGUMENT_CHECK(function, number_required, argument_count);

  BUFFER_VARARGS(argument_count, arguments);

  if (number_required < NUMBER_GENERIC_FUNCTION_TEMPLATES) {
    dylanXinternalXprimitive_replace_E_(new_arguments,arguments,number_required);
    
    { SOV* rest_arguments = dylanXinternalXprimitive_stack_allocate_vector(optionals_count);
      dylanXinternalXprimitive_initialize_vector_from_buffer
        (rest_arguments, optionals_count, &arguments[number_required]);
      new_arguments[number_required] = rest_arguments;

      return(dylanXinternalXprimitive_iep_apply(function, number_required + 1, new_arguments));
    }
    
  } else { /* CALLING DEFAULT-DISCRIMINATOR WANTS #MORE'D ARGUMENTS */

    SOV* rest_arguments = 
       dylanXinternalXprimitive_stack_allocate_vector(optionals_count);
    Z* new_arguments = (Z*)dylanXinternalXprimitive_stack_allocate_vector(number_required + 1);
    dylanXinternalXprimitive_initialize_vector_from_buffer
      ((SOV*)new_arguments, number_required, arguments);
    vector_size_setter(number_required + 1, (SOV*)new_arguments);
    new_arguments[number_required + VECTOR_HEADER_SIZE] = rest_arguments;
    dylanXinternalXprimitive_initialize_vector_from_buffer
      (rest_arguments, optionals_count, &arguments[number_required]);
    _P_function = function; 
    return((function_iep(function))(new_arguments));
}}}

/* 
 * KEYWORD REST METHOD ENTRY POINT 
 */
 
Z optional_mep (Z first_argument, ...) {
  int  number_required = function_number_required(_P_function);
  SOV* keyword_specifier_vector = method_keyword_specifiers(_P_function);
  int  number_keywords = vector_size(keyword_specifier_vector) / 2;
  Z*  keyword_specifiers = vector_data(keyword_specifier_vector);
  int  new_argument_count = number_required + number_keywords + 1;

  arguments[0] = first_argument;
  { va_list ap; va_start(ap,first_argument); 
    transfer_varargs(ap, _P_argument_count - 1, &arguments[1]); va_end(ap); }
  
  { SOV* rest_arguments = (SOV*)arguments[number_required];
    int  number_optionals = vector_size(rest_arguments);
    Z*  optional_arguments = vector_data(rest_arguments);

  if (function_key_p(_P_function)) {
    default_arguments(number_required, arguments, number_keywords, 
                      keyword_specifiers, number_required, new_arguments);
    
    process_keyword_parameters
      (_P_function, number_required, number_keywords, keyword_specifiers, 
       number_optionals, optional_arguments, new_arguments); 
  } else
    dylanXinternalXprimitive_replace_E_(new_arguments,arguments,number_required);


  new_arguments[number_required + number_keywords] = rest_arguments;
  
  return(dylanXinternalXprimitive_basic_iep_apply(_P_function, new_argument_count, new_arguments));
}}

/* 
 * KEYWORD METHOD ENTRY POINT 
 
Z key_mep (Z first_argument, ...) {
  int  number_required = function_number_required(_P_function);
  SOV* keyword_specifier_vector = method_keyword_specifiers(_P_function);
  int  number_keywords = vector_size(keyword_specifier_vector) / 2;
  Z   keyword_specifiers = vector_data(keyword_specifier_vector);
  int  new_argument_count = number_required + number_keywords;

  arguments[0] = first_argument;
  { va_list ap; va_start(ap,first_argument); 
    transfer_varargs(ap, _P_argument_count - 1, &arguments[1]); va_end(ap); }
  
  { SOV* rest_arguments = (SOV*)arguments[number_required];
    int  number_optionals = vector_size(rest_arguments);
    Z*  optional_arguments = vector_data(rest_arguments);

  default_arguments(number_required, arguments, number_keywords, 
                    keyword_specifiers, number_required, new_arguments);
    
  process_keyword_parameters
    (_P_function, number_required, number_keywords, keyword_specifiers, 
     number_optionals, optional_arguments, new_arguments); 

  return(dylanXinternalXprimitive_basic_iep_apply(_P_function, new_argument_count, new_arguments));
}}
 */

/****************************************************************************
 ***** TOP-LEVEL INITIALIZATION *********************************************
 ****************************************************************************/

Z _P_values[VALUES_MAX];
Z _P_unbound;
static Unwind_protect_frame base_unwind_protect_frame;
    
Z
dylanXinternalXtop_level_form_c_run_time()
{
  int stack_marker;


  dylanXinternalX_P_signal_number = 0;
  dylanXinternalX_P_max_stack_size = INITIAL_MAX_STACK_SIZE;
  dylanXinternalX_P_real_max_stack_size = INITIAL_MAX_STACK_SIZE;
  dylanXinternalX_P_bottom_of_stack = (unsigned long)&stack_marker;
  dylanXinternalXprimitive_setup_c_handlers();
  dylanXinternalX_P_argument_buffer = (Z*)malloc(ARGUMENT_COUNT_MAX*sizeof(Z)); 
  base_unwind_protect_frame.previous_unwind_protect_frame = (Unwind_protect_frame*)0;
  base_unwind_protect_frame.ultimate_destination = (Bind_exit_frame*)0;
  _P_current_unwind_protect_frame = & base_unwind_protect_frame;
}


Z dylanXinternalXprimitive_stack_vector_remaining_values(int offset) {
  int number_values = _P_number_values - offset;
  Z* values = (Z*)dylanXinternalXprimitive_allocate_vector(number_values);
  dylanXinternalXprimitive_initialize_vector_from_buffer
      ((SOV*)values, number_values, &_P_values[offset]);
  return(values);      
}

/*
;;; BACKTRACE MECHANISM
;;;
;;; Ian Piumarta
*/

/* #define or #undef the following appropriately... */

#define	BACKTRACE_CALLS		/* enable backtracing of CALL*()s */
#define	BACKTRACE_METHOD_CALLS	/* enable backtracing of METHOD_CALL*()s */

/*
;;;
;;; COMMON DECLARATIONS FOR BACKTRACE
;;;
*/

Z dylanXinternalX_P_ddb_frame= (Z *)0;

#if defined(BACKTRACE_CALLS) || defined(BACKTRACE_METHOD_CALLS)

typedef struct small_context {
  struct small_context *next;
  int nargs;
  Z *args[10];
} CTX;

typedef struct large_context {
  struct small_context *next;
  int nargs;
  Z *args[ARGUMENT_COUNT_MAX];
} CTX_;

static Z *a1, *a2, *a3, *a4, *a5, *a6, *a7, *a8, *a9;

INLINE void ddb_enter(CTX *ctxp, int nargs,
		      Z *fn, Z *a1, Z *a2, Z *a3, Z *a4,
		      Z *a5, Z *a6, Z *a7, Z *a8, Z *a9)
{
  ctxp->next= (CTX *)dylanXinternalX_P_ddb_frame;
  ctxp->nargs= nargs;
  ctxp->args[0]= fn;
  ctxp->args[1]= a1;
  ctxp->args[2]= a2;
  ctxp->args[3]= a3;
  ctxp->args[4]= a4;
  ctxp->args[5]= a5;
  ctxp->args[6]= a6;
  ctxp->args[7]= a7;
  ctxp->args[8]= a8;
  ctxp->args[9]= a9;
  dylanXinternalX_P_ddb_frame= (Z *)ctxp;
}

INLINE Z ddb_exit(CTX* ctxp, Z value)
{
  if(ctxp != dylanXinternalX_P_ddb_frame) {
    fprintf(stderr,"%ddb-frame synchronization problem (check state saves/restores)!\n");
    dylanXinternalXprimitive_break();
  }
  dylanXinternalX_P_ddb_frame= (Z *)(((CTX *)dylanXinternalX_P_ddb_frame)->next);
  return(value);
}

#define NA ((Z *)0)

#endif /* BACKTRACE_CALLS || BACKTRACE_METHOD_CALLS */

/*
;;;
;;; CALL*() DECLARATIONS FOR BACTRACE
;;;
*/

#ifdef BACKTRACE_CALLS

#define CTX_DECL		CTX ctx
#define CTX_DECL_		CTX_ ctx
#define	ENTER(CTXP,NARGS,FN)	ddb_enter(CTXP,NARGS,FN,a1,a2,a3,a4,a5,a6,a7,a8,a9)
#define	ENTER_(CTXP,NARGS,FN)	ddb_enter(CTXP,NARGS,FN,NA,NA,NA,NA,NA,NA,NA,NA,NA)
#define SAVE_ARG(CTXP,NUM,ARG)	(((CTXP)->args[NUM])= (ARG))
#define EXIT(CTXP,VALUE)	ddb_exit(CTXP,VALUE)
#else
#define CTX_DECL
#define CTX_DECL_
#define	ENTER(CTXP,NARGS,FN)
#define	ENTER_(CTXP,NARGS,FN)
#define SAVE_ARG(CTXP,NUM,ARG)
#define EXIT(CTXP,VALUE)	(VALUE)
#endif /* BACKTRACE_CALLS */

/*
;;;
;;; METHOD_CALL*() DECLARATIONS FOR BACKTRACE
;;;
*/

#ifdef BACKTRACE_METHOD_CALLS

#define M_CTX_DECL		CTX ctx
#define M_CTX_DECL_		CTX_ ctx
#define	M_ENTER(CTXP,NARGS,FN)	ddb_enter(CTXP,NARGS-1,FN,a1,a2,a3,a4,a5,a6,a7,a8,a9)
#define	M_ENTER_(CTXP,NARGS,FN)	ddb_enter(CTXP,NARGS-1,FN,NA,NA,NA,NA,NA,NA,NA,NA,NA)
#define M_SAVE_ARG(CTXP,NUM,ARG)	(((CTXP)->args[NUM])= (ARG))
#define M_EXIT(CTXP,VALUE)	ddb_exit(CTXP,VALUE)
#else
#define M_CTX_DECL
#define M_CTX_DECL_
#define	M_ENTER(CTXP,NARGS,FN)
#define	M_ENTER_(CTXP,NARGS,FN)
#define M_SAVE_ARG(CTXP,NUM,ARG)
#define M_EXIT(CTXP,VALUE)	(VALUE)

#endif /* BACKTRACE_METHOD_CALLS */

/*
 * TIMING PRIMITIVES -- IAN PIUMARTA
 */


#if defined(poopoo)

#if !defined(AUX) && !defined(WIN32)

#include <sys/time.h>
#include <sys/resource.h>

static struct rusage start, stop;

Z dylanXinternalXprimitive_start_timer()
{
  getrusage(RUSAGE_SELF, &start);
  return dylanXinternalX_P_false;
}

Z dylanXinternalXprimitive_stop_timer()
{
  getrusage(RUSAGE_SELF, &stop);

  stop.ru_utime.tv_usec -= start.ru_utime.tv_usec;
  stop.ru_utime.tv_sec -= start.ru_utime.tv_sec;

  if (stop.ru_utime.tv_usec < 0) {
    stop.ru_utime.tv_usec += 1000000;
    stop.ru_utime.tv_sec -= 1;
  }

  { SOV* value  = dylanXinternalXprimitive_allocate_vector(2);
    Z* data;
    data = (Z*)vector_data(value);
    data[0] = dylanXinternalXprimitive_integer(stop.ru_utime.tv_sec);
    data[1] = dylanXinternalXprimitive_integer(stop.ru_utime.tv_usec);
    return(value); 
    
  /*
  printf("%d.%03d", stop.ru_utime.tv_sec, stop.ru_utime.tv_usec / 1000);
  */
}}

#endif
#endif
