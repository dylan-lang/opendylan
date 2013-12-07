#include "run-time.h"
#include "trace.h"

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <gc/gc.h>

#ifdef OPEN_DYLAN_PLATFORM_UNIX
#include <signal.h>
#endif

#define ignore(x) (void)x

#ifdef OPEN_DYLAN_COMPILER_GCC_LIKE
#  define likely(x)       __builtin_expect((x),1)
#  define unlikely(x)     __builtin_expect((x),0)
#else
#  define likely(x) x
#  define unlikely(x) x
#endif

#define MAX_HEAP_SIZE          (2047 * 1024 * 1024)

/*
 stack allocation
   relies on inlining
 calls to error handlers
 pointer_element_at (base, byte_offset)
 at and at_setter's for base and byte_offset
   int8,  int16,  int32,  int64,
   uint8, uint16, uint32, uint64,
   single_float, double_float
 signal handling
 break, error and errno
 file date and file date setter
 */

/* PLATFORM SPECIFIC HAX */

#ifdef OPEN_DYLAN_PLATFORM_WINDOWS
#define rint(x) floor(x)
#define strncasecmp _strnicmp
#endif

/* DYLAN CONSTANTS */

extern OBJECT KPfalseVKi;
extern OBJECT KPtrueVKi;
extern OBJECT KPunboundVKi;

#define UNBOUND_P(x) ((x) == &KPunboundVKi)


#if defined(OPEN_DYLAN_PLATFORM_WINDOWS)
#define INLINE __inline
#define FORCE_INLINE __forceinline
#elif defined(OPEN_DYLAN_COMPILER_CLANG)
#define INLINE static inline
#define FORCE_INLINE static inline __attribute__((always_inline))
#else
#define INLINE inline
#define FORCE_INLINE inline __attribute__((always_inline))
#endif

/* stubbed primitives */
D primitive_runtime_module_handle()
{
  return(I(0));
}

/* SUPPORT */

void primitive_break() {
#if defined(OPEN_DYLAN_PLATFORM_WINDOWS)
  extern void __stdcall DebugBreak(void);
  DebugBreak();
#else
#ifdef SIGTRAP
  fprintf(stderr, "Breaking into debugger.\n");
  fflush(stderr);
  raise(SIGTRAP);
#else
  int *ptr = (int*)0;
  puts("Breaking into debugger.");
  fflush(stdout);
  *ptr = 0; /* generate a memory fault */
#endif
#endif
}

/* MEMORY */

INLINE D instance_header_setter (D header, D* instance) {
  instance[0] = header;
  return(header);
}

D allocate (unsigned long size) {
#if defined(GC_USE_BOEHM)
  return((D)GC_MALLOC((size_t)size));
#elif defined(GC_USE_MALLOC)
  return((D)malloc((size_t)size));
#endif
}

D primitive_allocate (DSINT size) {
  return((D)allocate(size * sizeof(D)));
}

D primitive_byte_allocate (DSINT number_words, DSINT number_bytes) {
  return((D)allocate(number_words * sizeof(D) + number_bytes));
}

D primitive_untraced_allocate (DSINT size) {
  return(allocate(size * sizeof(D)));
}

D primitive_manual_allocate (D sizeObject) {
  size_t size = (size_t)R(sizeObject);
#if defined(GC_USE_BOEHM)
  void* p = GC_MALLOC_UNCOLLECTABLE(size);
#elif defined(GC_USE_MALLOC)
  void* p = malloc(size);
#endif
  return(primitive_wrap_machine_word((DMINT)p));
}

void primitive_manual_free (D object) {
#if defined(GC_USE_BOEHM)
  GC_FREE((void*)primitive_unwrap_c_pointer(object));
#elif defined(GC_USE_MALLOC)
  free((void*)primitive_unwrap_c_pointer(object));
#endif
}

void primitive_fillX(D dst, int base_offset, int offset, int size, D value) {
  register int i;
  D* target = ((D*)dst) + base_offset + offset;
  for (i = 0; i < size; i++) {
    target[i] = value;
  }
}

void primitive_fill_bytesX
    (D dst, int base_offset, int offset, int size, DSINT value) {
  if (size > 0) {
    memset(((unsigned char*)((D*)dst + base_offset)) + offset, value, (size_t)size);
  }
}

DSINT primitive_repeated_slot_offset(D x) {
  D*       instance   = (D*)x;
  Wrapper* wrapper    = (Wrapper*)instance[0];
  DSINT    fixed_part = wrapper->fixed_part;
  DSINT    n_slots    = fixed_part >> 2;
  DSINT    offset     = 1 + n_slots + 1;
  return(offset);
}

D primitive_repeated_slot_as_raw(D x, DSINT offset) {
  return((D)((D*)x + offset));
}

void primitive_replace_bytesX
    (D dst, DSINT dst_base_offset, DSINT dst_offset,
     D src, DSINT src_base_offset, DSINT src_offset, DSINT size) {
  if (size > 0) {
    memcpy(&(((char*)(&(((D*)dst)[dst_base_offset])))[dst_offset]),
           &(((char*)(&(((D*)src)[src_base_offset])))[src_offset]),
           (size_t)size);
  }
}

#define COPY_WORDS(dst, src, size) memcpy((dst), (src), (size) * sizeof(D))

void primitive_replaceX
    (D dst, DSINT dst_base_offset, DSINT dst_offset,
     D src, DSINT src_base_offset, DSINT src_offset, DSINT size) {
  ignore(src_base_offset);
  if (size > 0) {
    COPY_WORDS(&(((D*)dst)[dst_base_offset + dst_offset]),
               &(((D*)src)[dst_base_offset + src_offset]),
               size);
  }
}


D primitive_compare_bytes(D base1, DSINT offset1,
                          D base2, DSINT offset2, DSINT size) {
  return (RAWASBOOL(memcmp(&((((BS*)base1)->data)[offset1]),
                           &((((BS*)base2)->data)[offset2]),
                           (size_t)size)));
}

D primitive_compare_words(D base1, DSINT offset1,
                          D base2, DSINT offset2, DSINT size) {
  return (RAWASBOOL(memcmp(&((((BS*)base1)->data)[offset1]),
                           &((((BS*)base2)->data)[offset2]),
                           size * sizeof(D))));
}


D primitive_byte_allocate_filled_terminated
    (DSINT size, DSINT number_bytes, D class_wrapper, DSINT number_slots,
     D fill_value, DSINT repeated_size, DSINT repeated_size_offset)
{
  D* object = primitive_byte_allocate(size, number_bytes);
  instance_header_setter(class_wrapper, object);
  primitive_fillX(object, 1, 0, number_slots, fill_value);
  primitive_fill_bytesX
    (object, repeated_size_offset + 1, 0, repeated_size,
     (unsigned char)R(fill_value));
  ((char*)(&object[repeated_size_offset + 1]))[repeated_size] = (char)0;
  if (repeated_size_offset > 0) {
    object[repeated_size_offset] = I(repeated_size);
  }
  return((D)object);
}

/* This one still zero-terminates. TODO: turn that off */
D primitive_byte_allocate_filled
    (DSINT size, D class_wrapper, DSINT number_slots,
     D fill_value, DSINT repeated_size, DSINT repeated_size_offset,
     DBYTE repeated_fill_value)
{
  D* object = primitive_byte_allocate(size, repeated_size + 1);
  instance_header_setter(class_wrapper, object);
  primitive_fillX(object, 1, 0, number_slots, fill_value);
  primitive_fill_bytesX(object, repeated_size_offset + 1, 0, repeated_size,
                        repeated_fill_value);
  ((char*)(&object[repeated_size_offset + 1]))[repeated_size] = (char)0;
  if (repeated_size_offset > 0) {
    object[repeated_size_offset] = I(repeated_size);
  }
  return((D)object);
}

D primitive_byte_allocate_leaf_filled_terminated
    (DSINT size, DSINT number_bytes, D class_wrapper, DSINT number_slots,
     D fill_value, DSINT repeated_size, DSINT repeated_size_offset)
{
  return primitive_byte_allocate_filled_terminated(size, number_bytes,
                                                   class_wrapper, number_slots,
                                                   fill_value, repeated_size,
                                                   repeated_size_offset);
}

D primitive_byte_allocate_leaf_filled
    (DSINT size, D class_wrapper, DSINT number_slots,
     D fill_value, DSINT repeated_size, DSINT repeated_size_offset,
     DBYTE repeated_fill_value)
{
  return primitive_byte_allocate_filled(size, class_wrapper,
                                        number_slots, fill_value,
                                        repeated_size,
                                        repeated_size_offset,
                                        repeated_fill_value);
}

#define define_repeated_allocator(name, type) \
  D primitive_ ## name ## _allocate_filled \
      (DSINT size, D class_wrapper, DSINT number_slots, D fill_value, \
       DSINT repeated_size, DSINT repeated_size_offset, \
       type repeated_fill_value) \
  { \
    int i; \
    D* object = primitive_byte_allocate(size, (DSINT)(repeated_size * sizeof(type))); \
    instance_header_setter(class_wrapper, object); \
    primitive_fillX(object, 1, 0, number_slots, fill_value); \
    for (i = 0; i < repeated_size; i++) {\
      ((type*)(&object[repeated_size_offset + 1]))[i] = (type)repeated_fill_value; \
    } \
    if (repeated_size_offset > 0) { \
      object[repeated_size_offset] = I(repeated_size); \
    } \
    return((D)object); \
  }

define_repeated_allocator(object, D)
define_repeated_allocator(double_byte, DDBYTE)
define_repeated_allocator(word, DWORD)
define_repeated_allocator(double_word, DDWORD)
define_repeated_allocator(single_float, DSFLT)
define_repeated_allocator(double_float, DDFLT)

D primitive_allocate_filled
    (DSINT size, D class_wrapper, DSINT number_slots, D fill_value,
     DSINT repeated_size, DSINT repeated_size_offset)
{
  D* object = primitive_allocate(size);
  instance_header_setter(class_wrapper, object);
  primitive_fillX(object, 1, 0, number_slots, fill_value);
  primitive_fillX(object, repeated_size_offset + 1, 0, repeated_size, fill_value);
  if (repeated_size_offset > 0) {
    object[repeated_size_offset] = I(repeated_size);
  }
  return((D)object);
}

D primitive_allocate_in_awl_pool
    (DSINT size, D class_wrapper, DSINT number_slots, D fill_value,
     DSINT repeated_size, DSINT repeated_size_offset, D assoc)
{
  D* object = primitive_allocate(size);
  instance_header_setter(class_wrapper, object);
  primitive_fillX(object, 1, 0, number_slots, fill_value);
  primitive_fillX(object, repeated_size_offset + 1, 0, repeated_size, fill_value);
  if (repeated_size_offset > 0) {
    object[repeated_size_offset] = I(repeated_size);
  }
  object[1] = assoc;
  return((D)object);
}

D primitive_allocate_weak_in_awl_pool
    (DSINT size, D class_wrapper, DSINT number_slots, D fill_value,
     DSINT repeated_size, DSINT repeated_size_offset, D assoc)
{
  D* object = primitive_allocate(size);
  instance_header_setter(class_wrapper, object);
  primitive_fillX(object, 1, 0, number_slots, fill_value);
  primitive_fillX(object, repeated_size_offset + 1, 0, repeated_size, fill_value);
  if (repeated_size_offset > 0) {
    object[repeated_size_offset] = I(repeated_size);
  }
  object[1] = assoc;
  return((D)object);
}

D primitive_allocate_wrapper
    (DSINT size, D class_wrapper, DSINT number_slots, D fill_value,
     DSINT repeated_size, DSINT repeated_size_offset)
{
  D* object = primitive_allocate(size);
  instance_header_setter(class_wrapper, object);
  primitive_fillX(object, 1, 0, number_slots, fill_value);
  primitive_fillX(object, repeated_size_offset + 1, 0, repeated_size, fill_value);
  if (repeated_size_offset > 0) {
    object[repeated_size_offset] = I(repeated_size);
  }
  return((D)object);
}

/* STACK ALLOCATION */


/* This one still zero-terminates. TODO: turn that off */
D initialize_byte_stack_allocate_filled
    (D ptr, D class_wrapper, DSINT number_slots,
     D fill_value, DSINT repeated_size, DSINT repeated_size_offset,
     DBYTE repeated_fill_value)
{
  D* object = ptr;
  instance_header_setter(class_wrapper, object);
  primitive_fillX(object, 1, 0, number_slots, fill_value);
  primitive_fill_bytesX
    (object, repeated_size_offset + 1, 0, repeated_size,
     (unsigned char)R(repeated_fill_value));
  ((char*)(&object[repeated_size_offset + 1]))[repeated_size] = (char)0;
  if (repeated_size_offset > 0) {
    object[repeated_size_offset] = I(repeated_size);
  }
  return((D)object);
}

D initialize_object_stack_allocate_filled
      (D ptr, D class_wrapper, DSINT number_slots, D fill_value,
       DSINT repeated_size, DSINT repeated_size_offset,
       D repeated_fill_value)
{
  int i;
  D* object = ptr;
  instance_header_setter(class_wrapper, object);
  primitive_fillX(object, 1, 0, number_slots, fill_value);
  for (i = 0; i < repeated_size; i++) {
    ((D*)(&object[repeated_size_offset + 1]))[i] = (D)repeated_fill_value;
  }
  if (repeated_size_offset > 0) {
    object[repeated_size_offset] = I(repeated_size);
  }
  return((D)object);
}


/* PINNING PRIMITIVES */

void primitive_unpin_object(D object)
{
  ignore(object);
}

/* C-FFI PRIMITIVES */

D primitive_wrap_c_pointer(D wrapper, DMINT x) {
  return(primitive_allocate_filled
           (2, wrapper, 1, (D)x, 0, 0));
}

/* VECTOR */

extern Wrapper KLsimple_object_vectorGVKdW;

#define VECTOR_HEADER_SIZE (2)

/* gts,98apr08 */
D  VECTOR_REF_OR_F(D vector, int offset) {
  if (offset >= vector_size(vector)) {
    return(DFALSE);
  } else {
    return(vector_ref((SOV*)vector, offset));
  }
}

INLINE D  vector_ref_setter (D new_value, SOV* vector, int offset) {
  return(vector_data(vector)[offset] = new_value);
}

extern SOV* Pempty_vectorVKi;

SOV* allocate_vector (int size) {
  if (size == 0) {
    return(Pempty_vectorVKi);
  } else {
    SOV* vector = (SOV*)primitive_allocate(size + VECTOR_HEADER_SIZE);
    return(vector);
  }
}

SOV* make_vector (int size) {
  if (size == 0) {
    return(Pempty_vectorVKi);
  } else {
    SOV* vector = allocate_vector(size);
    instance_header_setter(&KLsimple_object_vectorGVKdW, (D*)vector);
    vector_size_setter(size, vector);
    return(vector);
  }
}

D primitive_make_vector (int size) { return((D)make_vector(size)); }

SOV* initialize_vector_from_buffer_with_size
    (SOV* vector, int vector_size, D* buffer, int buffer_size)
{
  instance_header_setter(&KLsimple_object_vectorGVKdW, (D*)vector);
  vector_size_setter(vector_size, vector);
  COPY_WORDS(vector_data(vector), buffer, buffer_size);
  return(vector);
}

SOV* initialize_vector_from_buffer (SOV* vector, int size, D* buffer) {
  return(initialize_vector_from_buffer_with_size(vector, size, buffer, size));
}

SOV* make_vector_from_buffer (int size, D* buffer) {
  SOV* copy = allocate_vector(size);
  initialize_vector_from_buffer(copy, size, buffer);
  return(copy);
}

D primitive_copy_vector (D vector) {
  return((D)make_vector_from_buffer(vector_size((SOV*)vector), vector_data((SOV*)vector)));
}

D primitive_raw_as_vector (D size, D buffer) {
  return(make_vector_from_buffer((long)size, (D*)buffer));
}

#define DEF_STACK_VECTOR(_name, _size) \
  D _stk_##_name[_size + VECTOR_HEADER_SIZE]; \
  SOV* _name = (SOV*)(&_stk_##_name)

#define DEF_STACK_VECTOR_INITTED(_name, _size) \
  D _stk_##_name[_size + VECTOR_HEADER_SIZE]; \
  SOV* _name = (init_stack_vector((SOV*)(&_stk_##_name), (_size)))

INLINE SOV* init_stack_vector(SOV* vector, int size) {
  instance_header_setter(&KLsimple_object_vectorGVKdW, (D*)vector);
  vector_size_setter(size, vector);
  return(vector);
  }

#define DEF_STACK_VECTOR_FROM_BUFFER_WITH_SIZE(_name, _vector_size, _buffer, _buffer_size) \
  DEF_STACK_VECTOR(_name, _vector_size); \
  initialize_vector_from_buffer_with_size (_name, _vector_size, _buffer, _buffer_size)

#define DEF_STACK_VECTOR_FROM_BUFFER(_name, _size, _buffer) \
  DEF_STACK_VECTOR_FROM_BUFFER_WITH_SIZE(_name, _size, _buffer, _size)

#define primitive_stack_allocate_vector(size) \
  ((SOV*)primitive_stack_allocate(size + VECTOR_HEADER_SIZE))

/* STRING */

extern Wrapper KLbyte_stringGVKdW;

D primitive_raw_as_string (DBSTR buffer) {
  size_t size = strlen(buffer);
  BS* string = (BS*)allocate(sizeof(BS) + size + 1);
  instance_header_setter(&KLbyte_stringGVKdW, (D*)string);
  string->size = I(size);
  memcpy(string->data, buffer, size);
  return((D)string);
}

/* SIGNATURES */

INLINE SOV* signature_required(SIG* sig) {
  return(sig->required);
}

INLINE SOV* signature_values(SIG* sig) {
  return(sig->values);
}

INLINE D signature_rest_value(SIG* sig) {
  return(sig->rest_value);
}

#define NUMBER_REQUIRED_MASK 0x0000ff
#define NUMBER_VALUES_MASK   0x00ff00
#define NUMBER_VALUES_OFFSET 8
#define KEY_P_MASK           0x010000
#define ALL_KEYS_P_MASK      0x020000
#define REST_P_MASK          0x040000
#define OPTIONALS_P_MASK     (KEY_P_MASK | REST_P_MASK)
#define REST_VALUE_P_MASK    0x080000
#define NEXT_P_MASK          0x100000

INLINE int signature_number_required(SIG* sig) {
  return(R(sig->properties) & NUMBER_REQUIRED_MASK);
}

INLINE int signature_number_values(SIG* sig) {
  return((R(sig->properties) & NUMBER_VALUES_MASK)
           >> NUMBER_VALUES_OFFSET);
}

INLINE int signature_key_p(SIG* sig) {
  return((R(sig->properties) & KEY_P_MASK) > 0);
}

INLINE int signature_all_keys_p(SIG* sig) {
  return((R(sig->properties) & ALL_KEYS_P_MASK) > 0);
}

INLINE int signature_rest_p(SIG* sig) {
  return((R(sig->properties) & REST_P_MASK) > 0);
}

INLINE int signature_optionals_p(SIG* sig) {
  return((R(sig->properties) & OPTIONALS_P_MASK) > 0);
}


INLINE int signature_rest_value_p(SIG* sig) {
  return((R(sig->properties) & REST_VALUE_P_MASK) > 0);
}

INLINE int signature_next_p(SIG* sig) {
  return((R(sig->properties) & NEXT_P_MASK) > 0);
}

INLINE D signature_make_properties
    (int number_required, int number_values,
     int key_p, int all_keys_p, int rest_p, int rest_value_p) {
  return(I(number_required
           | (number_values << NUMBER_VALUES_OFFSET)
           | (key_p ? KEY_P_MASK : 0)
           | (all_keys_p ? ALL_KEYS_P_MASK : 0)
           | (rest_p ? REST_P_MASK : 0)
           | (rest_value_p ? REST_VALUE_P_MASK : 0)));
}

/* FUNCTION */

INLINE DFN function_xep(FN* function) {
  return(function->xep);
}

DFN primitive_function_xep(D function) {
  return(function_xep((FN*)function));
}

INLINE DLFN function_mep(FN* function) {
  return(function->mep);
}

INLINE DLFN function_iep(FN* function) {
  return(function->mep);
}

INLINE DLFN keyword_function_iep(FN* function) {
  return(((KFN*)function)->iep);
}

INLINE D method_keyword_specifiers(FN* method) {
  return(((KFN*)method)->keyword_specifiers);
}

INLINE SOV* function_specializers(FN* function) {
  return(signature_required(function->signature));
}

INLINE int function_number_required(FN* function) {
  return(signature_number_required(function->signature));
}

INLINE int function_number_values(FN* function) {
  return(signature_number_values(function->signature));
}

INLINE int function_key_p(FN* function) {
  return(signature_key_p(function->signature));
}

INLINE int function_all_keys_p(FN* function) {
  return(signature_all_keys_p(function->signature));
}

INLINE int function_rest_p(FN* function) {
  return(signature_rest_p(function->signature));
}

INLINE int function_optionals_p(FN* function) {
  return(signature_optionals_p(function->signature));
}


INLINE int function_rest_value_p(FN* function) {
  return(signature_rest_value_p(function->signature));
}

INLINE int function_next_p(FN* function) {
  return(signature_next_p(function->signature));
}

/* VARARGS SUPPORT */

INLINE void transfer_varargs(va_list ap, int n, D* arguments) {
  int i;

  for (i=0; i<n; i++) {
    arguments[i] = va_arg(ap, D);
  }
}

#define BUFFER_VARARGS(n, last_parameter, buffer) \
  { \
    va_list ap; \
    va_start(ap, (last_parameter)); \
    transfer_varargs(ap, (n), (buffer)); \
    va_end(ap); \
  }

/* CALLING CONVENTION */




/* CALL CHECKS */

extern D LobjectGVKd;
extern D Ktype_check_errorVKiI(D argument, D specializer);

#define INSTANCEP(x, y) (primitive_instanceQ((x), (y)) != DFALSE)

#define FUNCTIONP(x) \
    (R((((Wrapper*)OBJECT_WRAPPER(x)))->subtype_mask) & 64)

FORCE_INLINE D PERFORM_INLINE_TYPE_CHECK(D value, D type) {
  if (unlikely(type != LobjectGVKd && !INSTANCEP(value, type))) {
    Ktype_check_errorVKiI(value, type);
  }
  return(value);
}

D primitive_type_check (D value, D type) {
  return PERFORM_INLINE_TYPE_CHECK(value, type);
}

extern D Kstack_overflow_errorVKiI();

INLINE void SIMPLE_CALL_CHECK(FN* function) {
  /* int stack_marker; */
  ignore(function);
  /*
  if (STACK_OK(bottom_of_stack,(unsigned long)(&stack_marker))) {
    stack_overflowedQ = 1;
    Kstack_overflow_errorVKiI();
  }*/
}

extern D Kargument_count_overflow_errorVKiI(D function, D argc);

INLINE void CALL_CHECK(FN* function, int argument_count) {
  SIMPLE_CALL_CHECK(function);
  if (unlikely(argument_count > MAX_ARGUMENTS)) {
    Kargument_count_overflow_errorVKiI(function, I(argument_count));
  }
}

FORCE_INLINE void TYPE_CHECK_ARG (D specializer, D argument) {
  PERFORM_INLINE_TYPE_CHECK(argument, specializer);
}

INLINE void TYPE_CHECK_ARGS(D function, int argument_count, D* arguments) {
  SOV* specs = function_specializers((FN*)function);
  if (specs) {
    D* specializers = vector_data(specs);
    int i;
    for (i = 0; i < argument_count; i++) {
      TYPE_CHECK_ARG(specializers[i], arguments[i]);
    }
  }
}

INLINE void TYPE_CHECK_ARGS_1(D fn, D a1) {
  SOV* specs = function_specializers((FN*)fn);
  if (specs) {
    D* specializers = vector_data(specs);
    TYPE_CHECK_ARG(specializers[0], a1);
  }
}

INLINE void TYPE_CHECK_ARGS_2(D fn, D a1, D a2) {
  SOV* specs = function_specializers((FN*)fn);
  if (specs) {
    D* specializers = vector_data(specs);
    TYPE_CHECK_ARG(specializers[0], a1);
    TYPE_CHECK_ARG(specializers[1], a2);
  }
}

INLINE void TYPE_CHECK_ARGS_3(D fn, D a1, D a2, D a3) {
  SOV* specs = function_specializers((FN*)fn);
  if (specs) {
    D* specializers = vector_data(specs);
    TYPE_CHECK_ARG(specializers[0], a1);
    TYPE_CHECK_ARG(specializers[1], a2);
    TYPE_CHECK_ARG(specializers[2], a3);
  }
}

INLINE void TYPE_CHECK_ARGS_4(D fn, D a1, D a2, D a3, D a4) {
  SOV* specs = function_specializers((FN*)fn);
  if (specs) {
    D* specializers = vector_data(specs);
    TYPE_CHECK_ARG(specializers[0], a1);
    TYPE_CHECK_ARG(specializers[1], a2);
    TYPE_CHECK_ARG(specializers[2], a3);
    TYPE_CHECK_ARG(specializers[3], a4);
  }
}

INLINE void TYPE_CHECK_ARGS_5(D fn, D a1, D a2, D a3, D a4, D a5) {
  SOV* specs = function_specializers((FN*)fn);
  if (specs) {
    D* specializers = vector_data(specs);
    TYPE_CHECK_ARG(specializers[0], a1);
    TYPE_CHECK_ARG(specializers[1], a2);
    TYPE_CHECK_ARG(specializers[2], a3);
    TYPE_CHECK_ARG(specializers[3], a4);
    TYPE_CHECK_ARG(specializers[4], a5);
  }
}

INLINE void TYPE_CHECK_ARGS_6 (D fn, D a1, D a2, D a3, D a4, D a5, D a6) {
  SOV* specs = function_specializers((FN*)fn);
  if (specs) {
    D* specializers = vector_data(specs);
    TYPE_CHECK_ARG(specializers[0], a1);
    TYPE_CHECK_ARG(specializers[1], a2);
    TYPE_CHECK_ARG(specializers[2], a3);
    TYPE_CHECK_ARG(specializers[3], a4);
    TYPE_CHECK_ARG(specializers[4], a5);
    TYPE_CHECK_ARG(specializers[5], a6);
  }
}

INLINE void TYPE_CHECK_ARGS_7
    (D fn, D a1, D a2, D a3, D a4, D a5, D a6, D a7) {
  SOV* specs = function_specializers((FN*)fn);
  if (specs) {
    D* specializers = vector_data(specs);
    TYPE_CHECK_ARG(specializers[0], a1);
    TYPE_CHECK_ARG(specializers[1], a2);
    TYPE_CHECK_ARG(specializers[2], a3);
    TYPE_CHECK_ARG(specializers[3], a4);
    TYPE_CHECK_ARG(specializers[4], a5);
    TYPE_CHECK_ARG(specializers[5], a6);
    TYPE_CHECK_ARG(specializers[6], a7);
  }
}

INLINE void TYPE_CHECK_ARGS_8
    (D fn, D a1, D a2, D a3, D a4, D a5, D a6, D a7, D a8) {
  SOV* specs = function_specializers((FN*)fn);
  if (specs) {
    D* specializers = vector_data(specs);
    TYPE_CHECK_ARG(specializers[0], a1);
    TYPE_CHECK_ARG(specializers[1], a2);
    TYPE_CHECK_ARG(specializers[2], a3);
    TYPE_CHECK_ARG(specializers[3], a4);
    TYPE_CHECK_ARG(specializers[4], a5);
    TYPE_CHECK_ARG(specializers[5], a6);
    TYPE_CHECK_ARG(specializers[6], a7);
    TYPE_CHECK_ARG(specializers[7], a8);
  }
}

INLINE void TYPE_CHECK_ARGS_9
    (D fn, D a1, D a2, D a3, D a4, D a5, D a6, D a7, D a8, D a9) {
  SOV* specs = function_specializers((FN*)fn);
  if (specs) {
    D* specializers = vector_data(specs);
    TYPE_CHECK_ARG(specializers[0], a1);
    TYPE_CHECK_ARG(specializers[1], a2);
    TYPE_CHECK_ARG(specializers[2], a3);
    TYPE_CHECK_ARG(specializers[3], a4);
    TYPE_CHECK_ARG(specializers[4], a5);
    TYPE_CHECK_ARG(specializers[5], a6);
    TYPE_CHECK_ARG(specializers[6], a7);
    TYPE_CHECK_ARG(specializers[7], a8);
    TYPE_CHECK_ARG(specializers[8], a9);
  }
}

extern D Kargument_count_errorVKiI(D function, D argc);

INLINE void BASIC_REQUIRED_CALL_CHECK
    (FN* function, int number_required, int argument_count) {
  CALL_CHECK(function, argument_count);
  if (unlikely(argument_count != number_required)) {
    Kargument_count_errorVKiI(function, I(argument_count));
  }
}

INLINE void REQUIRED_CALL_CHECK
    (FN* function, int number_required, int argument_count, D* arguments) {
  BASIC_REQUIRED_CALL_CHECK(function, number_required, argument_count);
  TYPE_CHECK_ARGS(function, argument_count, arguments);
}

INLINE void BASIC_OPTIONAL_CALL_CHECK
    (FN* function, int number_required, int argument_count) {
  CALL_CHECK(function, argument_count);
  if (unlikely(argument_count < number_required)) {
    Kargument_count_errorVKiI(function, I(argument_count));
  }
}

INLINE void OPTIONAL_CALL_CHECK
    (FN* function, int number_required, int argument_count, D* arguments) {
  BASIC_OPTIONAL_CALL_CHECK(function, number_required, argument_count);
  TYPE_CHECK_ARGS(function, number_required, arguments);
}

extern D Kodd_keyword_arguments_errorVKiI(D function, D argc);

INLINE void KEYWORD_CALL_CHECK
    (FN* function, int number_required, int argument_count, D* arguments) {
  OPTIONAL_CALL_CHECK (function, number_required, argument_count, arguments);
  if (unlikely((argument_count - number_required) & 1)) {
    Kodd_keyword_arguments_errorVKiI(function, I(argument_count));
  }
}

/* CALLING CONVENTION */

D primitive_xep_apply (FN* fn, int n, D a[]) {
  TEB* teb = get_teb();
  DFN xep = fn->xep;

  teb->function = fn;
  teb->next_methods = DFALSE;

  switch (n) {
  case 0: return(xep(fn,n));
  case 1: return(xep(fn,n,a[0]));
  case 2: return(xep(fn,n,a[0],a[1]));
  case 3: return(xep(fn,n,a[0],a[1],a[2]));
  case 4: return(xep(fn,n,a[0],a[1],a[2],a[3]));
  case 5: return(xep(fn,n,a[0],a[1],a[2],a[3],a[4]));
  case 6: return(xep(fn,n,a[0],a[1],a[2],a[3],a[4],a[5]));
  case 7: return(xep(fn,n,a[0],a[1],a[2],a[3],a[4],a[5],a[6]));
  case 8: return(xep(fn,n,a[0],a[1],a[2],a[3],a[4],a[5],a[6],a[7]));
  case 9: return(xep(fn,n,a[0],a[1],a[2],a[3],a[4],a[5],a[6],a[7],a[8]));
  default:
  if (n > 64) {
    primitive_break();
  }
  return(xep(fn,n,
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
INLINE int FUNCTION_OK(FN* function) {
  return(class_meta_class(function_class(function)) ==
         dylanXinternalX_L_function_class_G_);
}
*/

D primitive_xep_call (FN* fn, int n, ...) {
  TEB* teb = get_teb();
  int i;
  va_list ap; va_start(ap,n);
  for (i=0; i<n; i++) {
    D argument = va_arg(ap, D);
    teb->arguments[i] = argument;
  }
  REQUIRED_CALL_CHECK(fn, function_number_required(fn), n, teb->arguments);
  return(primitive_xep_apply(fn, n, teb->arguments));
}

/*
  CALL_DYLAN_FUNCTION_RETURNING_ALL_VALUES Added by phoward, 13th FEB 97.
  This is not called from anywhere in the runtime. It can be called
  remotely from the debugger to execute a dylan function via its XEP,
  and vector all return values.
*/

D call_dylan_function_returning_all_values (FN* fn, int n, ...) {
  TEB* teb = get_teb();
  int i;
  D first_value;
  va_list ap; va_start(ap,n);
  for (i=0; i<n; i++) {
    D argument = va_arg(ap, D);
    teb->arguments[i] = argument;
  }
  first_value = primitive_xep_apply(fn, n, teb->arguments);
  return(MV_GET_REST_AT(first_value, 0));
}

D primitive_mep_apply_with_optionals (FN* fn, D new_next_methods, D args) {
  TEB* teb = get_teb();
  DLFN mep = fn->mep;
  D*   v   = vector_data((SOV*)args);

  teb->next_methods = new_next_methods;
  teb->function = fn;
  teb->argument_count = vector_size((SOV*)args);

  switch (teb->argument_count) {
  case  0: return(mep());
  case  1: return(mep(v[0]));
  case  2: return(mep(v[0],v[1]));
  case  3: return(mep(v[0],v[1],v[2]));
  case  4: return(mep(v[0],v[1],v[2],v[3]));
  case  5: return(mep(v[0],v[1],v[2],v[3],v[4]));
  case  6: return(mep(v[0],v[1],v[2],v[3],v[4],v[5]));
  case  7: return(mep(v[0],v[1],v[2],v[3],v[4],v[5],v[6]));
  case  8: return(mep(v[0],v[1],v[2],v[3],v[4],v[5],v[6],v[7]));
  case  9: return(mep(v[0],v[1],v[2],v[3],v[4],v[5],v[6],v[7],v[8]));
  default:
    if (teb->argument_count > 64) {
      primitive_break();
    }
    return(mep(v[ 0], v[ 1], v[ 2], v[ 3], v[ 4], v[ 5], v[ 6], v[ 7],
               v[ 8], v[ 9], v[10], v[11], v[12], v[13], v[14], v[15],
               v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23],
               v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31],
               v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39],
               v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47],
               v[48], v[49], v[50], v[51], v[52], v[53], v[54], v[55],
               v[56], v[57], v[58], v[59], v[60], v[61], v[62], v[63]));
  }
}

INLINE GFN* parent_gf (D cache_header_or_gf) {
  while (!FUNCTIONP(cache_header_or_gf)) {
    cache_header_or_gf = ((CACHEHEADERENGINE*)cache_header_or_gf)->parent;
  }
  return((GFN*)cache_header_or_gf);
}


extern D primitive_engine_node_apply_with_optionals (D engD, D parent, D args);

D primitive_engine_node_apply_with_optionals (D engD, D parent, D args) {
  TEB* teb = get_teb();
  ENGINE* eng = (ENGINE*)engD;
  DLFN ep = eng->entry_point;
  D*   a   = vector_data((SOV*)args);

  teb->next_methods = parent;
  teb->function = (D)eng;
  teb->argument_count = vector_size((SOV*)args);

  switch (teb->argument_count) {
  case 0: return(ep());
  case 1: return(ep(a[0]));
  case 2: return(ep(a[0],a[1]));
  case 3: return(ep(a[0],a[1],a[2]));
  case 4: return(ep(a[0],a[1],a[2],a[3]));
  case 5: return(ep(a[0],a[1],a[2],a[3],a[4]));
  case 6: return(ep(a[0],a[1],a[2],a[3],a[4],a[5]));
  case 7: return(ep(a[0],a[1],a[2],a[3],a[4],a[5],a[6]));
  };
  /* Over 7 implementation args, the engine-node calling sequence passes the
     implementation args as a vector, but the engine-node *might* be a method,
     in which case they have to be spread out again!
     */
  if (FUNCTIONP(eng)) {
    return(primitive_mep_apply_with_optionals((FN*)eng, parent, args));
  } else {
    return(ep(args));
  }
}

D inline_invoke_engine_node (ENGINE* eng, int argcount, ...) {
  TEB* teb = get_teb();
  int i;
  DEF_STACK_VECTOR_INITTED(argvec, teb->argument_count);
  va_list ap; va_start(ap,argcount);
  for (i=0; i<argcount; i++) {
    D argument = va_arg(ap, D);
    vector_ref_setter(argument, argvec, i);
  }
  if (FUNCTIONP(eng)) {
    return(primitive_mep_apply_with_optionals((FN*)eng, teb->next_methods, argvec));
  } else {
    return((eng->entry_point)((D)argvec));
  }
}


D primitive_engine_node_apply(ENGINE* eng, D parent, D a[]) {
  GFN* gf = parent_gf(parent);
  SIG* sig = gf->signature;
  int  number_required = signature_number_required(sig);
  int  argument_count = vector_size((SOV*)a);
  if (signature_optionals_p(sig)) {
    /* OPTIONAL_CALL_CHECK(gfn,number_required,argument_count); */
    D*   arguments = vector_data((SOV*)a);
    DEF_STACK_VECTOR_FROM_BUFFER_WITH_SIZE
      (new_arguments, number_required + 1, arguments, number_required);
    int  optionals_count = argument_count - number_required;
    DEF_STACK_VECTOR_FROM_BUFFER
      (rest_arguments, optionals_count, &arguments[number_required]);
    vector_ref_setter(rest_arguments, new_arguments, number_required);
    return(primitive_engine_node_apply_with_optionals((D)eng, parent, (D*)new_arguments));
  } else {
    /* REQUIRED_CALL_CHECK(gfn,number_required,argument_count); */
    return(primitive_engine_node_apply_with_optionals((D)eng, parent, a));
  }
}


D primitive_mep_apply (FN* fn, D next_methods, D a[]) {
  int  number_required = function_number_required(fn);
  int  argument_count = vector_size((SOV*)a);
  if (function_optionals_p(fn)) {
    /* OPTIONAL_CALL_CHECK(fn,number_required,argument_count); */
    D*   arguments = vector_data((SOV*)a);
    DEF_STACK_VECTOR_FROM_BUFFER_WITH_SIZE
      (new_arguments, number_required + 1, arguments, number_required);
    int  optionals_count = argument_count - number_required;
    DEF_STACK_VECTOR_FROM_BUFFER
      (rest_arguments, optionals_count, &arguments[number_required]);
    vector_ref_setter(rest_arguments, new_arguments, number_required);
    return(primitive_mep_apply_with_optionals
             (fn, next_methods, (D*)new_arguments));
  } else {
    /* REQUIRED_CALL_CHECK(fn,number_required,argument_count); */
    return(primitive_mep_apply_with_optionals(fn, next_methods, a));
  }
}

D iep_apply (DLFN iep, int n, D a[]) {
  switch (n) {
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
  if (n > 64) {
    primitive_break();
  }
  return(iep(a[ 0],a[ 1],a[ 2],a[ 3],a[ 4],a[ 5],a[ 6],a[ 7],a[ 8],a[ 9],
             a[10],a[11],a[12],a[13],a[14],a[15],a[16],a[17],a[18],a[19],
             a[20],a[21],a[22],a[23],a[24],a[25],a[26],a[27],a[28],a[29],
             a[30],a[31],a[32],a[33],a[34],a[35],a[36],a[37],a[38],a[39],
             a[40],a[41],a[42],a[43],a[44],a[45],a[46],a[47],a[48],a[49],
             a[50],a[51],a[52],a[53],a[54],a[55],a[56],a[57],a[58],a[59],
             a[60],a[61],a[62],a[63]));
  }
}

D primitive_iep_apply (FN* fn, int n, D a[]) {
  TEB* teb = get_teb();
  teb->function = fn; teb->next_methods = DFALSE;
  return(iep_apply(function_iep(fn), n, a));
}

/* required xep's */

D xep_0 (FN* fn, int n) {
  TEB* teb = get_teb();
  BASIC_REQUIRED_CALL_CHECK(fn, 0, n);
  teb->function = fn; teb->next_methods = DFALSE;
  return((function_iep(fn))());
}
D xep_1 (FN* fn, int n, D a1) {
  TEB* teb = get_teb();
  BASIC_REQUIRED_CALL_CHECK(fn, 1, n);
  TYPE_CHECK_ARGS_1(fn, a1);
  teb->function = fn; teb->next_methods = DFALSE;
  return((function_iep(fn))(a1));
}
D xep_2 (FN* fn, int n, D a1, D a2) {
  TEB* teb = get_teb();
  BASIC_REQUIRED_CALL_CHECK(fn, 2, n);
  TYPE_CHECK_ARGS_2(fn, a1, a2);
  teb->function = fn; teb->next_methods = DFALSE;
  return(function_iep(fn)(a1,a2));
}
D xep_3 (FN* fn, int n, D a1, D a2, D a3) {
  TEB* teb = get_teb();
  BASIC_REQUIRED_CALL_CHECK(fn, 3, n);
  TYPE_CHECK_ARGS_3(fn, a1, a2, a3);
  teb->function = fn; teb->next_methods = DFALSE;
  return(function_iep(fn)(a1,a2,a3));
}
D xep_4 (FN* fn, int n, D a1, D a2, D a3, D a4) {
  TEB* teb = get_teb();
  BASIC_REQUIRED_CALL_CHECK(fn, 4, n);
  TYPE_CHECK_ARGS_4(fn, a1, a2, a3, a4);
  teb->function = fn; teb->next_methods = DFALSE;
  return(function_iep(fn)(a1,a2,a3,a4));
}
D xep_5 (FN* fn, int n, D a1, D a2, D a3, D a4, D a5) {
  TEB* teb = get_teb();
  BASIC_REQUIRED_CALL_CHECK(fn, 5, n);
  TYPE_CHECK_ARGS_5(fn, a1, a2, a3, a4, a5);
  teb->function = fn; teb->next_methods = DFALSE;
  return(function_iep(fn)(a1,a2,a3,a4,a5));
}
D xep_6 (FN* fn, int n, D a1, D a2, D a3, D a4, D a5, D a6) {
  TEB* teb = get_teb();
  BASIC_REQUIRED_CALL_CHECK(fn, 6, n);
  TYPE_CHECK_ARGS_6(fn, a1, a2, a3, a4, a5, a6);
  teb->function = fn; teb->next_methods = DFALSE;
  return(function_iep(fn)(a1,a2,a3,a4,a5,a6));
}
D xep_7 (FN* fn, int n, D a1, D a2, D a3, D a4, D a5, D a6, D a7) {
  TEB* teb = get_teb();
  BASIC_REQUIRED_CALL_CHECK(fn, 7, n);
  TYPE_CHECK_ARGS_7(fn, a1, a2, a3, a4, a5, a6, a7);
  teb->function = fn; teb->next_methods = DFALSE;
  return(function_iep(fn)(a1,a2,a3,a4,a5,a6,a7));
}
D xep_8 (FN* fn, int n, D a1, D a2, D a3, D a4, D a5, D a6, D a7, D a8) {
  TEB* teb = get_teb();
  BASIC_REQUIRED_CALL_CHECK(fn, 8, n);
  TYPE_CHECK_ARGS_8(fn, a1, a2, a3, a4, a5, a6, a7, a8);
  teb->function = fn; teb->next_methods = DFALSE;
  return(function_iep(fn)(a1,a2,a3,a4,a5,a6,a7,a8));
}
D xep_9 (FN* fn, int n, D a1, D a2, D a3, D a4, D a5, D a6, D a7, D a8, D a9) {
  TEB* teb = get_teb();
  BASIC_REQUIRED_CALL_CHECK(fn, 9, n);
  TYPE_CHECK_ARGS_9(fn, a1, a2, a3, a4, a5, a6, a7, a8, a9);
  teb->function = fn; teb->next_methods = DFALSE;
  return(function_iep(fn)(a1,a2,a3,a4,a5,a6,a7,a8,a9));
}
D xep (FN* fn, int n, ...) {
  TEB* teb = get_teb();
  BUFFER_VARARGS(n, n, teb->arguments);
  REQUIRED_CALL_CHECK(fn, function_number_required(fn), n, teb->arguments);
  return(iep_apply(function_iep(fn), n, teb->arguments));
}

/* REST XEP'S */
/*   numbered by the number of required arguments == # parameters in IEP - 1 */

D rest_xep_0 (FN* fn, int n, ...) {
  TEB* teb = get_teb();
  BUFFER_VARARGS(n, n, teb->arguments);
  BASIC_OPTIONAL_CALL_CHECK(fn, 0, n);
  teb->function = fn; teb->next_methods = DFALSE;
  return((function_iep(fn))(make_vector_from_buffer(n, teb->arguments)));
}
D rest_xep_1 (FN* fn, int n, D a1, ...) {
  TEB* teb = get_teb();
  BUFFER_VARARGS(n - 1, a1, teb->arguments);
  BASIC_OPTIONAL_CALL_CHECK(fn, 1, n);
  TYPE_CHECK_ARGS_1(fn, a1);
  teb->function = fn; teb->next_methods = DFALSE;
  return((function_iep(fn))(a1, make_vector_from_buffer(n - 1, teb->arguments)));
}
D rest_xep_2 (FN* fn, int n, D a1, D a2, ...) {
  TEB* teb = get_teb();
  BUFFER_VARARGS(n - 2, a2, teb->arguments);
  BASIC_OPTIONAL_CALL_CHECK(fn, 2, n);
  TYPE_CHECK_ARGS_2(fn, a1, a2);
  teb->function = fn; teb->next_methods = DFALSE;
  return(function_iep(fn)(a1,a2,make_vector_from_buffer(n - 2, teb->arguments)));
}
D rest_xep_3 (FN* fn, int n, D a1, D a2, D a3, ...) {
  TEB* teb = get_teb();
  BUFFER_VARARGS(n - 3, a3, teb->arguments);
  BASIC_OPTIONAL_CALL_CHECK(fn, 3, n);
  TYPE_CHECK_ARGS_3(fn, a1, a2, a3);
  teb->function = fn; teb->next_methods = DFALSE;
  return(function_iep(fn)(a1,a2,a3,make_vector_from_buffer(n - 3, teb->arguments)));
}
D rest_xep_4 (FN* fn, int n, D a1, D a2, D a3, D a4, ...) {
  TEB* teb = get_teb();
  BUFFER_VARARGS(n - 4, a4, teb->arguments);
  BASIC_OPTIONAL_CALL_CHECK(fn, 4, n);
  TYPE_CHECK_ARGS_4(fn, a1, a2, a3, a4);
  teb->function = fn; teb->next_methods = DFALSE;
  return(function_iep(fn)(a1,a2,a3,a4,make_vector_from_buffer(n - 4, teb->arguments)));
}
D rest_xep_5 (FN* fn, int n, D a1, D a2, D a3, D a4, D a5, ...) {
  TEB* teb = get_teb();
  BUFFER_VARARGS(n - 5, a5, teb->arguments);
  BASIC_OPTIONAL_CALL_CHECK(fn, 5, n);
  TYPE_CHECK_ARGS_5(fn, a1, a2, a3, a4, a5);
  teb->function = fn; teb->next_methods = DFALSE;
  return(function_iep(fn)(a1,a2,a3,a4,a5,make_vector_from_buffer(n - 5, teb->arguments)));
}
D rest_xep_6 (FN* fn, int n, D a1, D a2, D a3, D a4, D a5, D a6, ...) {
  TEB* teb = get_teb();
  BUFFER_VARARGS(n - 6, a6, teb->arguments);
  BASIC_OPTIONAL_CALL_CHECK(fn, 6, n);
  TYPE_CHECK_ARGS_6(fn, a1, a2, a3, a4, a5, a6);
  teb->function = fn; teb->next_methods = DFALSE;
  return(function_iep(fn)(a1,a2,a3,a4,a5,a6,make_vector_from_buffer(n - 6, teb->arguments)));
}
D rest_xep_7 (FN* fn, int n, D a1, D a2, D a3, D a4, D a5, D a6, D a7, ...) {
  TEB* teb = get_teb();
  BUFFER_VARARGS(n - 7, a7, teb->arguments);
  BASIC_OPTIONAL_CALL_CHECK(fn, 7, n);
  TYPE_CHECK_ARGS_7(fn, a1, a2, a3, a4, a5, a6, a7);
  teb->function = fn; teb->next_methods = DFALSE;
  return(function_iep(fn)(a1,a2,a3,a4,a5,a6,a7,make_vector_from_buffer(n - 7, teb->arguments)));
}
D rest_xep_8 (FN* fn, int n, D a1, D a2, D a3, D a4, D a5, D a6, D a7, D a8, ...) {
  TEB* teb = get_teb();
  BUFFER_VARARGS(n - 8, a8, teb->arguments);
  BASIC_OPTIONAL_CALL_CHECK(fn, 8, n);
  TYPE_CHECK_ARGS_8(fn, a1, a2, a3, a4, a5, a6, a7, a8);
  teb->function = fn; teb->next_methods = DFALSE;
  return(function_iep(fn)(a1,a2,a3,a4,a5,a6,a7,a8,make_vector_from_buffer(n - 8, teb->arguments)));
}
D rest_xep_9 (FN* fn, int n, D a1, D a2, D a3, D a4, D a5, D a6, D a7, D a8, D a9, ...) {
  TEB* teb = get_teb();
  BUFFER_VARARGS(n - 9, a9, teb->arguments);
  BASIC_OPTIONAL_CALL_CHECK(fn, 9, n);
  TYPE_CHECK_ARGS_9(fn, a1, a2, a3, a4, a5, a6, a7, a8, a9);
  teb->function = fn; teb->next_methods = DFALSE;
  return(function_iep(fn)(a1,a2,a3,a4,a5,a6,a7,a8,a9,make_vector_from_buffer(n - 9, teb->arguments)));
}

D rest_xep (FN* fn, int n, ...) {
  TEB* teb = get_teb();
  int number_required = function_number_required(fn);
  int optionals_count = n - number_required;
  D*  optional_arguments = &teb->arguments[number_required];

  BUFFER_VARARGS(n, n, teb->arguments);
  OPTIONAL_CALL_CHECK(fn, number_required, n, teb->arguments);
  COPY_WORDS(teb->new_arguments,teb->arguments,number_required);
  DEF_STACK_VECTOR_FROM_BUFFER(rest_arguments, optionals_count, optional_arguments);
  teb->new_arguments[number_required] = rest_arguments;
  teb->function = fn; teb->next_methods = DFALSE;
  return(iep_apply(function_iep(fn), number_required + 1, teb->new_arguments));
}


/* ACCESSOR-METHOD XEP'S */


extern D KPslotacc_single_q_instance_getterVKiI(D accmeth, D inst);
extern D KPslotacc_single_q_instance_setterVKiI(D value, D accmeth, D inst);
extern D KPslotacc_single_q_class_getterVKiI(D accmeth, D inst);
extern D KPslotacc_single_q_class_setterVKiI(D value, D accmeth, D inst);
extern D KPslotacc_repeated_instance_getterVKiI(D accmeth, D inst, D idx);
extern D KPslotacc_repeated_instance_setterVKiI(D value, D accmeth, D inst, D idx);


D slotacc_single_q_instance_getter_xep (ACCESSOR* am, int n, D a1) {
  BASIC_REQUIRED_CALL_CHECK(((FN*)am), 1, n);
  return(KPslotacc_single_q_instance_getterVKiI(am, a1));
}

D slotacc_single_q_instance_setter_xep (ACCESSOR* am, int n, D a1, D a2) {
  BASIC_REQUIRED_CALL_CHECK(((FN*)am), 2, n);
  return(KPslotacc_single_q_instance_setterVKiI(am, a1, a2));
}

D slotacc_single_q_class_getter_xep (ACCESSOR* am, int n, D a1) {
  BASIC_REQUIRED_CALL_CHECK(((FN*)am), 1, n);
  return(KPslotacc_single_q_class_getterVKiI(am, a1));
}

D slotacc_single_q_class_setter_xep (ACCESSOR* am, int n, D a1, D a2) {
  BASIC_REQUIRED_CALL_CHECK(((FN*)am), 2, n);
  return(KPslotacc_single_q_class_setterVKiI(am, a1, a2));
}

D slotacc_repeated_instance_getter_xep (ACCESSOR* am, int n, D a1, D a2) {
  BASIC_REQUIRED_CALL_CHECK(((FN*)am), 2, n);
  return(KPslotacc_repeated_instance_getterVKiI(am, a1, a2));
}

D slotacc_repeated_instance_setter_xep (ACCESSOR* am, int n, D a1, D a2, D a3) {
  BASIC_REQUIRED_CALL_CHECK(((FN*)am), 3, n);
  return(KPslotacc_repeated_instance_setterVKiI(am, a1, a2, a3));
}

D primitive_set_accessor_method_xep (D accmeth, D what) {
  ACCESSOR* am = (ACCESSOR*)accmeth;
  switch (R(what)) {
  case 0: am->xep = (DFN)&slotacc_single_q_instance_getter_xep; break;
  case 1: am->xep = (DFN)&slotacc_single_q_instance_setter_xep; break;
  case 2: am->xep = (DFN)&slotacc_single_q_class_getter_xep; break;
  case 3: am->xep = (DFN)&slotacc_single_q_class_setter_xep; break;
  case 4: am->xep = (DFN)&slotacc_repeated_instance_getter_xep; break;
  case 5: am->xep = (DFN)&slotacc_repeated_instance_setter_xep; break;
  };
  return((D)am);
}


/* KEYWORD PROCESSING SUPPORT */

INLINE void default_arguments
    (int number_required, D* arguments,
     int number_keywords, D* keyword_specifiers,
     int keyword_arguments_offset, D* new_arguments) {
  int i, j;

  /* copy arguments into staging ground */

  for (i=0; i<number_required; i++) {
    new_arguments[i] = arguments[i];
  }

  /* default keyword parameters */

  for (j=1, i=0; i < number_keywords; j += 2, i++) {
    new_arguments[i + keyword_arguments_offset] = keyword_specifiers[j];
  }
}

INLINE void process_keyword_parameters
    (FN* function, int number_required,
     int number_keywords, D keyword_specifiers[],
     int number_optionals, D optional_arguments[], D new_arguments[]) {
  int i,j,k;
  int size_keyword_specifiers = number_keywords * 2;
  ignore(function);
  for (i = number_optionals - 1; i >= 0;) {
    D value   = optional_arguments[i--];
    D keyword = optional_arguments[i--];
    for (j = 0, k = number_required + 1; j < size_keyword_specifiers; k++, j += 2) {
      D lambda_keyword = keyword_specifiers[j];
      if (keyword == lambda_keyword) {
         new_arguments[k] = value;
         break;
      }
    }
  }
}

extern D unknown_keyword_argument_errorVKi(D function, D keyword);

INLINE void process_keyword_parameters_into_with_checking
    (FN* function, int number_required,
     int number_keywords, D keyword_specifiers[],
     int argument_count, D arguments[], D new_arguments[]) {
  int i,j,k;

  int allow_other_keys_p = function_all_keys_p(function);
  int size_keyword_specifiers = number_keywords * 2;
  for (i=argument_count-1; i>=number_required;) {
    D value   = arguments[i--];
    D keyword = arguments[i--];
    for (j=0,k=number_required;;k++,j+=2) {
      if (j == size_keyword_specifiers) {
        if (!allow_other_keys_p) {
          unknown_keyword_argument_errorVKi(function, keyword);
        } else {
          break;
        }
      } else {
        D lambda_keyword = keyword_specifiers[j];
        if (keyword == lambda_keyword) {
          new_arguments[k] = value;
          break;
        }
      }
    }
  }
}

INLINE int process_keyword_call_into
    (D* new_arguments, FN* function, int argument_count,
     int number_required, D* required_arguments,
     int optionals_count, D* optional_arguments, SOV* rest_arguments) {
  SOV* keyword_specifier_vector = method_keyword_specifiers(function);
  int  number_keywords = vector_size(keyword_specifier_vector) / 2;
  D*   keyword_specifiers = vector_data(keyword_specifier_vector);
  int  new_argument_count = number_required + number_keywords + 1;

  ignore(argument_count);
  default_arguments(number_required, required_arguments, number_keywords,
                    keyword_specifiers, number_required + 1, new_arguments);
  process_keyword_parameters
    (function, number_required, number_keywords, keyword_specifiers,
     optionals_count, optional_arguments, new_arguments);

  new_arguments[number_required] = rest_arguments;
  return(new_argument_count);
}

/* TODO: Turn this back into stack allocation. This was a function
         returning stack allocated data! */

INLINE int process_keyword_call_and_restify_into
    (D* new_arguments, FN* function,
     int argument_count, D* arguments, SOV* rest_arguments) {
  int number_required = function_number_required(function);
  int optionals_count = argument_count - number_required;
  KEYWORD_CALL_CHECK(function,number_required,argument_count,arguments);
  initialize_vector_from_buffer_with_size
    (rest_arguments, optionals_count,
     &arguments[number_required], optionals_count);
  return(process_keyword_call_into
          (new_arguments, function, argument_count,
           number_required, arguments, optionals_count,
           &arguments[number_required], rest_arguments));
}

INLINE D* process_keyword_call
    (FN* function, int argument_count, D* arguments, D rest_arguments) {
  TEB* teb = get_teb();
  process_keyword_call_and_restify_into
    (teb->new_arguments, function, argument_count, arguments, (SOV*)rest_arguments);
  return(teb->new_arguments);
}

INLINE D* process_keyword_call_and_n
    (FN* function, int argument_count,
     D* arguments, D rest_arguments, int *new_argument_count) {
  TEB* teb = get_teb();
  *new_argument_count =
    process_keyword_call_and_restify_into
      (teb->new_arguments, function, argument_count, arguments, (SOV*)rest_arguments);
  return(teb->new_arguments);
}


/* REST and KEY XEP's */
/*   numbered by the total number of parameters in the IEP */

D rest_key_xep_1 (FN* fn, int n, ...) {
  TEB* teb = get_teb();
  int number_required = function_number_required(fn);
  int optionals_count = n - number_required;
  BUFFER_VARARGS(n, n, teb->arguments);
  KEYWORD_CALL_CHECK(fn, number_required, n, teb->arguments);
  DEF_STACK_VECTOR_FROM_BUFFER
    (rest_arguments, optionals_count, &teb->arguments[number_required]);
  D* a = process_keyword_call(fn, n, teb->arguments, rest_arguments);
  teb->function = fn; teb->next_methods = DFALSE;
  return(keyword_function_iep(fn)(a[0]));
}

D rest_key_xep_2 (FN* fn, int n, ...) {
  TEB* teb = get_teb();
  int number_required = function_number_required(fn);
  int optionals_count = n - number_required;
  BUFFER_VARARGS(n, n, teb->arguments);
  KEYWORD_CALL_CHECK(fn, number_required, n, teb->arguments);
  DEF_STACK_VECTOR_FROM_BUFFER
    (rest_arguments, optionals_count, &teb->arguments[number_required]);
  D* a = process_keyword_call(fn, n, teb->arguments, rest_arguments);
  teb->function = fn; teb->next_methods = DFALSE;
  return(keyword_function_iep(fn)(a[0],a[1]));
}

D rest_key_xep_3 (FN* fn, int n, ...) {
  TEB* teb = get_teb();
  int number_required = function_number_required(fn);
  int optionals_count = n - number_required;
  BUFFER_VARARGS(n, n, teb->arguments);
  KEYWORD_CALL_CHECK(fn, number_required, n, teb->arguments);
  DEF_STACK_VECTOR_FROM_BUFFER
    (rest_arguments, optionals_count, &teb->arguments[number_required]);
  D* a = process_keyword_call(fn, n, teb->arguments, rest_arguments);
  teb->function = fn; teb->next_methods = DFALSE;
  return(keyword_function_iep(fn)(a[0],a[1],a[2]));
}

D rest_key_xep_4 (FN* fn, int n, ...) {
  TEB* teb = get_teb();
  int number_required = function_number_required(fn);
  int optionals_count = n - number_required;
  BUFFER_VARARGS(n, n, teb->arguments);
  KEYWORD_CALL_CHECK(fn, number_required, n, teb->arguments);
  DEF_STACK_VECTOR_FROM_BUFFER
    (rest_arguments, optionals_count, &teb->arguments[number_required]);
  D* a = process_keyword_call(fn, n, teb->arguments, rest_arguments);
  teb->function = fn; teb->next_methods = DFALSE;
  return(keyword_function_iep(fn)(a[0],a[1],a[2],a[3]));
}

D rest_key_xep_5 (FN* fn, int n, ...) {
  TEB* teb = get_teb();
  int number_required = function_number_required(fn);
  int optionals_count = n - number_required;
  BUFFER_VARARGS(n, n, teb->arguments);
  KEYWORD_CALL_CHECK(fn, number_required, n, teb->arguments);
  DEF_STACK_VECTOR_FROM_BUFFER
    (rest_arguments, optionals_count, &teb->arguments[number_required]);
  D* a = process_keyword_call(fn, n, teb->arguments, rest_arguments);
  teb->function = fn; teb->next_methods = DFALSE;
  return(keyword_function_iep(fn)(a[0],a[1],a[2],a[3],a[4]));
}

D rest_key_xep_6 (FN* fn, int n, ...) {
  TEB* teb = get_teb();
  int number_required = function_number_required(fn);
  int optionals_count = n - number_required;
  BUFFER_VARARGS(n, n, teb->arguments);
  KEYWORD_CALL_CHECK(fn, number_required, n, teb->arguments);
  DEF_STACK_VECTOR_FROM_BUFFER
    (rest_arguments, optionals_count, &teb->arguments[number_required]);
  D* a = process_keyword_call(fn, n, teb->arguments, rest_arguments);
  teb->function = fn; teb->next_methods = DFALSE;
  return(keyword_function_iep(fn)(a[0],a[1],a[2],a[3],a[4],a[5]));
}

D rest_key_xep_7 (FN* fn, int n, ...) {
  TEB* teb = get_teb();
  int number_required = function_number_required(fn);
  int optionals_count = n - number_required;
  BUFFER_VARARGS(n, n, teb->arguments);
  KEYWORD_CALL_CHECK(fn, number_required, n, teb->arguments);
  DEF_STACK_VECTOR_FROM_BUFFER
    (rest_arguments, optionals_count, &teb->arguments[number_required]);
  D* a = process_keyword_call(fn, n, teb->arguments, rest_arguments);
  teb->function = fn; teb->next_methods = DFALSE;
  return(keyword_function_iep(fn)(a[0],a[1],a[2],a[3],a[4],a[5],a[6]));
}

D rest_key_xep_8 (FN* fn, int n, ...) {
  TEB* teb = get_teb();
  int number_required = function_number_required(fn);
  int optionals_count = n - number_required;
  BUFFER_VARARGS(n, n, teb->arguments);
  KEYWORD_CALL_CHECK(fn, number_required, n, teb->arguments);
  DEF_STACK_VECTOR_FROM_BUFFER
    (rest_arguments, optionals_count, &teb->arguments[number_required]);
  D* a = process_keyword_call(fn, n, teb->arguments, rest_arguments);
  teb->function = fn; teb->next_methods = DFALSE;
  return(keyword_function_iep(fn)(a[0],a[1],a[2],a[3],a[4],a[5],a[6],a[7]));
}

D rest_key_xep_9 (FN* fn, int n, ...) {
  TEB* teb = get_teb();
  int number_required = function_number_required(fn);
  int optionals_count = n - number_required;
  BUFFER_VARARGS(n, n, teb->arguments);
  KEYWORD_CALL_CHECK(fn, number_required, n, teb->arguments);
  DEF_STACK_VECTOR_FROM_BUFFER
    (rest_arguments, optionals_count, &teb->arguments[number_required]);
  D* a = process_keyword_call(fn, n, teb->arguments, rest_arguments);
  teb->function = fn; teb->next_methods = DFALSE;
  return(keyword_function_iep(fn)(a[0],a[1],a[2],a[3],a[4],a[5],a[6],a[7],a[8]));
}

D rest_key_xep (FN* fn, int n, ...) {
  TEB* teb = get_teb();
  int number_required = function_number_required(fn);
  int optionals_count = n - number_required;
  int new_n;
  BUFFER_VARARGS(n, n, teb->arguments);
  KEYWORD_CALL_CHECK(fn, number_required, n, teb->arguments);
  DEF_STACK_VECTOR_FROM_BUFFER
    (rest_arguments, optionals_count, &teb->arguments[number_required]);
  D* a = process_keyword_call_and_n(fn, n, teb->arguments, rest_arguments, &new_n);
  teb->function = fn; teb->next_methods = DFALSE;
  return(iep_apply(keyword_function_iep(fn), new_n, a));
}


/* METHOD ENTRY POINTS -- MEPs */
/*   numbered by the total number of parameters in the IEP */

D key_mep_1 (D a1, ...) {
  TEB* teb = get_teb();
  int  number_required = function_number_required(teb->function);
  teb->a[0] = a1; BUFFER_VARARGS(teb->argument_count - 1, a1, &teb->a[1]);
  SOV* rest = teb->a[number_required];
  process_keyword_call_into
    (teb->iep_a, teb->function, teb->argument_count, number_required, teb->a,
     vector_size(rest), vector_data(rest), rest);
  return(keyword_function_iep(teb->function)(teb->iep_a[0]));
}

D key_mep_2 (D a1, ...) {
  TEB* teb = get_teb();
  int  number_required = function_number_required(teb->function);
  teb->a[0] = a1; BUFFER_VARARGS(teb->argument_count - 1, a1, &teb->a[1]);
  SOV* rest = teb->a[number_required];
  process_keyword_call_into
    (teb->iep_a, teb->function, teb->argument_count, number_required, teb->a,
     vector_size(rest), vector_data(rest), rest);
  return(keyword_function_iep(teb->function)(teb->iep_a[0],teb->iep_a[1]));
}

D key_mep_3 (D a1, ...) {
  TEB* teb = get_teb();
  int  number_required = function_number_required(teb->function);
  teb->a[0] = a1; BUFFER_VARARGS(teb->argument_count - 1, a1, &teb->a[1]);
  SOV* rest = teb->a[number_required];
  process_keyword_call_into
    (teb->iep_a, teb->function, teb->argument_count, number_required, teb->a,
     vector_size(rest), vector_data(rest), rest);
  return(keyword_function_iep(teb->function)(teb->iep_a[0],teb->iep_a[1],teb->iep_a[2]));
}

D key_mep_4 (D a1, ...) {
  TEB* teb = get_teb();
  int  number_required = function_number_required(teb->function);
  teb->a[0] = a1; BUFFER_VARARGS(teb->argument_count - 1, a1, &teb->a[1]);
  SOV* rest = teb->a[number_required];
  process_keyword_call_into
    (teb->iep_a, teb->function, teb->argument_count, number_required, teb->a,
     vector_size(rest), vector_data(rest), rest);
  return(keyword_function_iep(teb->function)(teb->iep_a[0],teb->iep_a[1],teb->iep_a[2],teb->iep_a[3]));
}

D key_mep_5 (D a1, ...) {
  TEB* teb = get_teb();
  int  number_required = function_number_required(teb->function);
  teb->a[0] = a1; BUFFER_VARARGS(teb->argument_count - 1, a1, &teb->a[1]);
  SOV* rest = teb->a[number_required];
  process_keyword_call_into
    (teb->iep_a, teb->function, teb->argument_count, number_required, teb->a,
     vector_size(rest), vector_data(rest), rest);
  return(keyword_function_iep(teb->function)(teb->iep_a[0],teb->iep_a[1],teb->iep_a[2],teb->iep_a[3],teb->iep_a[4]));
}

D key_mep_6 (D a1, ...) {
  TEB* teb = get_teb();
  int  number_required = function_number_required(teb->function);
  teb->a[0] = a1; BUFFER_VARARGS(teb->argument_count - 1, a1, &teb->a[1]);
  SOV* rest = teb->a[number_required];
  process_keyword_call_into
    (teb->iep_a, teb->function, teb->argument_count, number_required, teb->a,
     vector_size(rest), vector_data(rest), rest);
  return(keyword_function_iep(teb->function)(teb->iep_a[0],teb->iep_a[1],teb->iep_a[2],teb->iep_a[3],teb->iep_a[4],teb->iep_a[5]));
}

D key_mep_7 (D a1, ...) {
  TEB* teb = get_teb();
  int  number_required = function_number_required(teb->function);
  teb->a[0] = a1; BUFFER_VARARGS(teb->argument_count - 1, a1, &teb->a[1]);
  SOV* rest = teb->a[number_required];
  process_keyword_call_into
    (teb->iep_a, teb->function, teb->argument_count, number_required, teb->a,
     vector_size(rest), vector_data(rest), rest);
  return(keyword_function_iep(teb->function)(teb->iep_a[0],teb->iep_a[1],teb->iep_a[2],teb->iep_a[3],teb->iep_a[4],teb->iep_a[5],teb->iep_a[6]));
}

D key_mep_8 (D a1, ...) {
  TEB* teb = get_teb();
  int  number_required = function_number_required(teb->function);
  teb->a[0] = a1; BUFFER_VARARGS(teb->argument_count - 1, a1, &teb->a[1]);
  SOV* rest = teb->a[number_required];
  process_keyword_call_into
    (teb->iep_a, teb->function, teb->argument_count, number_required, teb->a,
     vector_size(rest), vector_data(rest), rest);
  return(keyword_function_iep(teb->function)(teb->iep_a[0],teb->iep_a[1],teb->iep_a[2],teb->iep_a[3],teb->iep_a[4],teb->iep_a[5],teb->iep_a[6],teb->iep_a[7]));
}

D key_mep_9 (D a1, ...) {
  TEB* teb = get_teb();
  int  number_required = function_number_required(teb->function);
  teb->a[0] = a1; BUFFER_VARARGS(teb->argument_count - 1, a1, &teb->a[1]);
  SOV* rest = teb->a[number_required];
  process_keyword_call_into
    (teb->iep_a, teb->function, teb->argument_count, number_required, teb->a,
     vector_size(rest), vector_data(rest), rest);
  return(keyword_function_iep(teb->function)(teb->iep_a[0],teb->iep_a[1],teb->iep_a[2],teb->iep_a[3],teb->iep_a[4],teb->iep_a[5],teb->iep_a[6],teb->iep_a[7],teb->iep_a[8]));
}

D key_mep (D a1, ...) {
  TEB* teb = get_teb();
  int  number_required = function_number_required(teb->function);
  teb->a[0] = a1; BUFFER_VARARGS(teb->argument_count - 1, a1, &teb->a[1]);
  SOV* rest = teb->a[number_required];
  int new_argument_count
    = process_keyword_call_into
       (teb->new_arguments, teb->function, teb->argument_count, number_required, teb->a,
        vector_size(rest), vector_data(rest), rest);
  return(iep_apply(keyword_function_iep(teb->function), new_argument_count, teb->new_arguments));
}

/* NEW GF SUPPORT */

D gf_iep_0 () {
  TEB* teb = get_teb();
  GFN* gf = (GFN*)teb->function;
  ENGINE* e = gf->engine;
  teb->next_methods = (D)gf;
  teb->function = (D)e;
  return((e->entry_point)());
}

D gf_iep_1 (D a1) {
  TEB* teb = get_teb();
  GFN* gf = (GFN*)teb->function;
  ENGINE* e = gf->engine;
  teb->next_methods = (D)gf;
  teb->function = (D)e;
  return((e->entry_point)(a1));
}

D gf_iep_2 (D a1, D a2) {
  TEB* teb = get_teb();
  GFN* gf = (GFN*)teb->function;
  ENGINE* e = gf->engine;
  teb->next_methods = (D)gf;
  teb->function = (D)e;
  return((e->entry_point)(a1, a2));
}

D gf_iep_3 (D a1, D a2, D a3) {
  TEB* teb = get_teb();
  GFN* gf = (GFN*)teb->function;
  ENGINE* e = gf->engine;
  teb->next_methods = (D)gf;
  teb->function = (D)e;
  return((e->entry_point)(a1, a2, a3));
}

D gf_iep_4 (D a1, D a2, D a3, D a4) {
  TEB* teb = get_teb();
  GFN* gf = (GFN*)teb->function;
  ENGINE* e = gf->engine;
  teb->next_methods = (D)gf;
  teb->function = (D)e;
  return((e->entry_point)(a1, a2, a3, a4));
}

D gf_iep_5 (D a1, D a2, D a3, D a4, D a5) {
  TEB* teb = get_teb();
  GFN* gf = (GFN*)teb->function;
  ENGINE* e = gf->engine;
  teb->next_methods = (D)gf;
  teb->function = (D)e;
  return((e->entry_point)(a1, a2, a3, a4, a5));
}

D gf_iep_6 (D a1, D a2, D a3, D a4, D a5, D a6) {
  TEB* teb = get_teb();
  GFN* gf = (GFN*)teb->function;
  ENGINE* e = gf->engine;
  teb->next_methods = (D)gf;
  teb->function = (D)e;
  return((e->entry_point)(a1, a2, a3, a4, a5, a6));
}

D gf_iep_7 (D a1, D a2, D a3, D a4, D a5, D a6, D a7) {
  TEB* teb = get_teb();
  GFN* gf = (GFN*)teb->function;
  ENGINE* e = gf->engine;
  teb->next_methods = (D)gf;
  teb->function = (D)e;
  return((e->entry_point)(a1, a2, a3, a4, a5, a6, a7));
}

D gf_iep (D new_arguments) {
  TEB* teb = get_teb();
  GFN* gf = (GFN*)teb->function;
  ENGINE* e = gf->engine;
  /* Unfortunately, due to the vectorization of arguments in this case, we have to check
     to see if the engine is actually a method in which case we have to invoke it with the
     args spread.  I'm passing the gf as the extra-arg to simulate the "normal" case where
     the method is blindly invoked. */
  if (FUNCTIONP(e)) {
    return(primitive_mep_apply_with_optionals((FN*)e, (D)gf, new_arguments));
  } else {
    teb->next_methods = (D)gf;
    teb->function = (D)e;
    return((e->entry_point)(new_arguments));
  }
}

/* GENERIC FUNCTION EXTERNAL ENTRY POINTS -- GF_XEP's */

/* REQ ONLY GF XEP's */
/*   numbered by the number of required arguments */

D gf_xep_0 (FN* fn, int n) {
  TEB* teb = get_teb();
  teb->function = fn; BASIC_REQUIRED_CALL_CHECK(fn, 0, n);
  return(gf_iep_0());
}
D gf_xep_1 (FN* fn, int n, D a1) {
  TEB* teb = get_teb();
  teb->function = fn; BASIC_REQUIRED_CALL_CHECK(fn, 1, n);
  return(gf_iep_1(a1));
}
D gf_xep_2 (FN* fn, int n, D a1, D a2) {
  TEB* teb = get_teb();
  teb->function = fn; BASIC_REQUIRED_CALL_CHECK(fn, 2, n);
  return(gf_iep_2(a1,a2));
}
D gf_xep_3 (FN* fn, int n, D a1, D a2, D a3) {
  TEB* teb = get_teb();
  teb->function = fn; BASIC_REQUIRED_CALL_CHECK(fn, 3, n);
  return(gf_iep_3(a1,a2,a3));
}
D gf_xep_4 (FN* fn, int n, D a1, D a2, D a3, D a4) {
  TEB* teb = get_teb();
  teb->function = fn; BASIC_REQUIRED_CALL_CHECK(fn, 4, n);
  return(gf_iep_4(a1,a2,a3,a4));
}
D gf_xep_5 (FN* fn, int n, D a1, D a2, D a3, D a4, D a5) {
  TEB* teb = get_teb();
  teb->function = fn; BASIC_REQUIRED_CALL_CHECK(fn, 5, n);
  return(gf_iep_5(a1,a2,a3,a4,a5));
}
D gf_xep_6 (FN* fn, int n, D a1, D a2, D a3, D a4, D a5, D a6) {
  TEB* teb = get_teb();
  teb->function = fn; BASIC_REQUIRED_CALL_CHECK(fn, 6, n);
  return(gf_iep_6(a1,a2,a3,a4,a5,a6));
}
D gf_xep_7 (FN* fn, int n, D a1, D a2, D a3, D a4, D a5, D a6, D a7) {
  TEB* teb = get_teb();
  teb->function = fn; BASIC_REQUIRED_CALL_CHECK(fn, 7, n);
  return(gf_iep_7(a1,a2,a3,a4,a5,a6,a7));
}
D gf_xep (FN* fn, int n, ...) {
  TEB* teb = get_teb();
  int number_required = function_number_required(fn);
  BUFFER_VARARGS(n, n, teb->arguments);
  teb->function = fn; BASIC_REQUIRED_CALL_CHECK(fn, number_required, n);
  DEF_STACK_VECTOR_FROM_BUFFER(new_arguments, number_required, teb->arguments);
  return(gf_iep(new_arguments));
}

/* OPTIONAL GF XEP's */
/*   numbered by the number of required arguments */

D gf_optional_xep_0 (FN* fn, int n, ...) {
  TEB* teb = get_teb();
  int optionals_count = n - 0; BASIC_OPTIONAL_CALL_CHECK(fn, 0, n);
  teb->function = fn; BUFFER_VARARGS(n, n, teb->a);
  DEF_STACK_VECTOR_FROM_BUFFER(rest_arguments, optionals_count, &teb->a[0]);
  teb->a[0] = rest_arguments;
  return(gf_iep_1(teb->a[0]));
}

D gf_optional_xep_1 (FN* fn, int n, ...) {
  TEB* teb = get_teb();
  int optionals_count = n - 1; BASIC_OPTIONAL_CALL_CHECK(fn, 1, n);
  teb->function = fn; BUFFER_VARARGS(n, n, teb->a);
  DEF_STACK_VECTOR_FROM_BUFFER(rest_arguments, optionals_count, &teb->a[1]);
  teb->a[1] = rest_arguments;
  return(gf_iep_2(teb->a[0], teb->a[1]));
}

D gf_optional_xep_2 (FN* fn, int n, ...) {
  TEB* teb = get_teb();
  int optionals_count = n - 2; BASIC_OPTIONAL_CALL_CHECK(fn, 2, n);
  teb->function = fn; BUFFER_VARARGS(n, n, teb->a);
  DEF_STACK_VECTOR_FROM_BUFFER(rest_arguments, optionals_count, &teb->a[2]);
  teb->a[2] = rest_arguments;
  return(gf_iep_3(teb->a[0],teb->a[1],teb->a[2]));
}

D gf_optional_xep_3 (FN* fn, int n, ...) {
  TEB* teb = get_teb();
  int optionals_count = n - 3; BASIC_OPTIONAL_CALL_CHECK(fn, 3, n);
  teb->function = fn; BUFFER_VARARGS(n, n, teb->a);
  DEF_STACK_VECTOR_FROM_BUFFER(rest_arguments, optionals_count, &teb->a[3]);
  teb->a[3] = rest_arguments;
  return(gf_iep_4(teb->a[0],teb->a[1],teb->a[2],teb->a[3]));
}

D gf_optional_xep_4 (FN* fn, int n, ...) {
  TEB* teb = get_teb();
  int optionals_count = n - 4; BASIC_OPTIONAL_CALL_CHECK(fn, 4, n);
  teb->function = fn; BUFFER_VARARGS(n, n, teb->a);
  DEF_STACK_VECTOR_FROM_BUFFER(rest_arguments, optionals_count, &teb->a[4]);
  teb->a[4] = rest_arguments;
  return(gf_iep_5(teb->a[0],teb->a[1],teb->a[2],teb->a[3],teb->a[4]));
}

D gf_optional_xep_5 (FN* fn, int n, ...) {
  TEB* teb = get_teb();
  int optionals_count = n - 5; BASIC_OPTIONAL_CALL_CHECK(fn, 5, n);
  teb->function = fn; BUFFER_VARARGS(n, n, teb->a);
  DEF_STACK_VECTOR_FROM_BUFFER(rest_arguments, optionals_count, &teb->a[5]);
  teb->a[5] = rest_arguments;
  return(gf_iep_6(teb->a[0],teb->a[1],teb->a[2],teb->a[3],teb->a[4],teb->a[5]));
}

D gf_optional_xep_6 (FN* fn, int n, ...) {
  TEB* teb = get_teb();
  int optionals_count = n - 6; BASIC_OPTIONAL_CALL_CHECK(fn, 6, n);
  teb->function = fn; BUFFER_VARARGS(n, n, teb->a);
  DEF_STACK_VECTOR_FROM_BUFFER(rest_arguments, optionals_count, &teb->a[6]);
  teb->a[6] = rest_arguments;
  return(gf_iep_7(teb->a[0],teb->a[1],teb->a[2],teb->a[3],teb->a[4],teb->a[5],teb->a[6]));
}

D gf_optional_xep (FN* fn, int n, ...) {
  TEB* teb = get_teb();
    int number_required = function_number_required(fn);
    int optionals_count = n - number_required;
    BUFFER_VARARGS(n, n, teb->arguments);
    BASIC_OPTIONAL_CALL_CHECK(fn, number_required, n);
    DEF_STACK_VECTOR_FROM_BUFFER_WITH_SIZE
      (new_arguments, number_required + 1, teb->arguments, number_required);
    DEF_STACK_VECTOR_FROM_BUFFER
      (rest_arguments, optionals_count, &teb->arguments[number_required]);
    vector_ref_setter(rest_arguments, new_arguments, number_required);
    teb->function = fn;
    return(gf_iep(new_arguments));
}

/* dynamic setting of gf's entrypoints */

D primitive_set_generic_function_entrypoints(D fn) {
  D the_xep;
  FN* function = (FN*)fn;
  if (function_optionals_p(function)) {
    switch (function_number_required(function)) {
    case 0:  the_xep = gf_optional_xep_0; break;
    case 1:  the_xep = gf_optional_xep_1; break;
    case 2:  the_xep = gf_optional_xep_2; break;
    case 3:  the_xep = gf_optional_xep_3; break;
    case 4:  the_xep = gf_optional_xep_4; break;
    case 5:  the_xep = gf_optional_xep_5; break;
    case 6:  the_xep = gf_optional_xep_6; break;
    default: the_xep = gf_optional_xep;   break;
    }
  } else {
    switch (function_number_required(function)) {
    case 0:  the_xep = gf_xep_0; break;
    case 1:  the_xep = gf_xep_1; break;
    case 2:  the_xep = gf_xep_2; break;
    case 3:  the_xep = gf_xep_3; break;
    case 4:  the_xep = gf_xep_4; break;
    case 5:  the_xep = gf_xep_5; break;
    case 6:  the_xep = gf_xep_6; break;
    case 7:  the_xep = gf_xep_7; break;
    default: the_xep = gf_xep;   break;
    }
  }
  function->xep    = (DFN)the_xep;
  return(function);
}


D general_engine_node_1_engine (D a1) {
  TEB* teb = get_teb();
  ENGINE* e = (ENGINE*)teb->function;
  D parent = teb->next_methods;
  return((e->callback)(a1, e, parent));
}

D general_engine_node_2_engine (D a1, D a2) {
  TEB* teb = get_teb();
  ENGINE* e = (ENGINE*)teb->function;
  D parent = teb->next_methods;
  return((e->callback)(a1, a2, e, parent));
}


D general_engine_node_3_engine (D a1, D a2, D a3) {
  TEB* teb = get_teb();
  ENGINE* e = (ENGINE*)teb->function;
  D parent = teb->next_methods;
  DLFN cb = e->callback;
  return(cb(a1, a2, a3, e, parent));
}

D general_engine_node_n_engine (D a1, ...) {
  TEB* teb = get_teb();
  ENGINE* e = (ENGINE*)teb->function;
  D parent = teb->next_methods;
  GFN* gf = parent_gf(parent);
  DLFN cb = e->callback;
  SIG* sig = (SIG*)gf->signature;
  int nreq = signature_number_required(sig);
  int impargs = nreq + signature_optionals_p(sig);
  if (impargs > 7) {
    /* The calling sequence passes just a vector of MEP args. */
    return(cb(a1, e, parent));
  } else {
    /* The args are spread, last one may be a rest vector. */
    va_list ap;
    DEF_STACK_VECTOR_INITTED(svec, impargs);
    D* svdata = vector_data(svec);
    if (impargs > 0) {
      int i;
      svdata[0] = a1;
      va_start(ap, a1);
      for (i=1; i<impargs; i++) {
        D argument = va_arg(ap, D);
        svdata[i] = argument;
      }
    }
    return(cb(svec, e, parent));
  }
}
D general_engine_node_spread_engine (D a1, ...) {
  TEB* teb = get_teb();
  ENGINE* e = (ENGINE*)teb->function;
  D parent = teb->next_methods;
  GFN* gf = parent_gf(parent);
  DLFN cb = e->callback;
  SIG* sig = (SIG*)gf->signature;
  int nreq = signature_number_required(sig);
  int impargs = nreq + signature_optionals_p(sig);
  if (nreq != impargs) {
    /* If there's optionals, we will need to make a new vector and spread them out.  */
    if (impargs > 7) {
      /* All the args are in a stack vector, the last of which is the optionals... */
      SOV* mepargvec = (SOV*)a1;
      D* mepargdata = vector_data(mepargvec);
      SOV* optargvec = (SOV*)mepargdata[nreq];
      D* optargdata = vector_data(optargvec);
      int nopts = vector_size(optargvec);
      DEF_STACK_VECTOR_INITTED(svec, nreq + nopts);
      D* svdata = vector_data(svec);
      int i;
      for (i=0; i<nreq; i++) svdata[i] = mepargdata[i];
      for (i=0; i<nopts; i++) svdata[i+nreq] = optargdata[i];
      return(cb(svec, e, parent));
    } else {
      /* The arguments are spread, the last one is the optionals vector. */
      teb->arguments[0] = a1;
      BUFFER_VARARGS(nreq, a1, &teb->arguments[1]);
      SOV* optargvec = (SOV*)teb->arguments[nreq];
      D* optargdata = vector_data(optargvec);
      int nopts = vector_size(optargvec);
      DEF_STACK_VECTOR_INITTED(svec, nreq + nopts);
      D* svdata = vector_data(svec);
      int i;
      for (i=0; i<nreq; i++) svdata[i] = teb->arguments[i];
      for (i=0; i<nopts; i++) svdata[i+nreq] = optargdata[i];
      return(cb(svec, e, parent));
      }
  } else if (impargs > 7) {
    /* We have a vector of MEP args, and no optionals, so just use that vector. */
    return(cb(a1, e, parent));
  } else {
    /* No optionals, args are spread, copy them into a vector.  */
    va_list ap;
    DEF_STACK_VECTOR_INITTED(svec, nreq);
    D* svdata = vector_data(svec);
    if (nreq > 0) {
      int i;
      svdata[0] = a1;
      va_start(ap, a1);
      for (i=1; i<nreq; i++) {
        D argument = va_arg(ap, D);
        svdata[i] = argument;
      }
    }
    return(cb(svec, e, parent));
  }
}

extern D Krepeated_slot_getter_index_out_of_range_trapVKeI(D obj, D idx);
extern D Krepeated_slot_setter_index_out_of_range_trapVKeI(D val, D obj, D idx);
#define REPEATED_GETTER_OOR Krepeated_slot_getter_index_out_of_range_trapVKeI
#define REPEATED_SETTER_OOR Krepeated_slot_setter_index_out_of_range_trapVKeI

extern D Kunbound_instance_slotVKeI(D obj, D offset);
#define UNBOUND_INSTANCE_SLOT Kunbound_instance_slotVKeI
extern D Kunbound_repeated_slotVKeI(D obj, D offset);
#define UNBOUND_REPEATED_SLOT Kunbound_repeated_slotVKeI


D boxed_instance_slot_getter_engine (D object) {
  TEB* teb = get_teb();
  ENGINE* e = (ENGINE*)teb->function;
  int idx =  (int)(((DADDR)(e->properties)) >> SLOTENGINE_V_INDEX);
  D slot_value = primitive_initialized_slot_value(object, idx);
  if (UNBOUND_P(slot_value)) {
    return(UNBOUND_INSTANCE_SLOT(object, I(idx)));
  } else {
    return(slot_value);
  }
}

D boxed_instance_slot_setter_engine (D newval, D object) {
  TEB* teb = get_teb();
  ENGINE* e = (ENGINE*)teb->function;
  int baseidx = (int)(((DADDR)(e->properties)) >> SLOTENGINE_V_INDEX);
  primitive_slot_value_setter(newval, object, baseidx);
  return(newval);
}


D boxed_repeated_instance_slot_getter_engine (D object, D idx) {
  TEB* teb = get_teb();
  ENGINE* e = (ENGINE*)teb->function;
  int baseidx = (int)(((DADDR)(e->properties)) >> SLOTENGINE_V_INDEX);
  int size = primitive_repeated_instance_size(object, baseidx);
  int ridx = R(idx);
  if (ridx >= 0 && ridx < size) {
    D slot_value = primitive_repeated_slot_value(object, baseidx, ridx);
    if (UNBOUND_P(slot_value)) {
      return(UNBOUND_REPEATED_SLOT(object, idx));
    } else {
      return(slot_value);
    }
  } else {
    return(REPEATED_GETTER_OOR(object, idx));
  }
}

D boxed_repeated_instance_slot_setter_engine (D newval, D object, D idx) {
  TEB* teb = get_teb();
  ENGINE* e = (ENGINE*)teb->function;
  int baseidx = (int)(((DADDR)(e->properties)) >> SLOTENGINE_V_INDEX);
  int size = primitive_repeated_instance_size(object, baseidx);
  int ridx = R(idx);
  if (ridx >= 0 && ridx < size) {
    primitive_repeated_slot_value_setter(newval, object, baseidx, ridx);
    return(newval);
  } else {
    return(REPEATED_SETTER_OOR(newval, object, idx));
  }
}

D raw_byte_repeated_instance_slot_getter_engine (D object, D idx) {
  TEB* teb = get_teb();
  ENGINE* e = (ENGINE*)teb->function;
  int baseidx = (int)(((DADDR)(e->properties)) >> SLOTENGINE_V_INDEX);
  int size = primitive_repeated_instance_size(object, baseidx);
  int ridx = R(idx);
  if (ridx >= 0 && ridx < size) {
    return(C(primitive_byte_element(object, baseidx, ridx)));
  } else {
    return(REPEATED_GETTER_OOR(object, idx));
  }
}
D raw_byte_repeated_instance_slot_setter_engine (D newval, D object, D idx) {
  TEB* teb = get_teb();
  ENGINE* e = (ENGINE*)teb->function;
  int baseidx = (int)(((DADDR)(e->properties)) >> SLOTENGINE_V_INDEX);
  int size = primitive_repeated_instance_size(object, baseidx);
  int ridx = R(idx);
  if (ridx >= 0 && ridx < size) {
    primitive_byte_element_setter((DBCHR)R(newval), object, baseidx, ridx);
    return(newval);
  } else {
    return(REPEATED_SETTER_OOR(newval, object, idx));
  }
}




/* **************************************************************** */

#define PARAMTEMPLATE0
#define PARAMTEMPLATE1 D a1
#define PARAMTEMPLATE2 D a1, D a2
#define PARAMTEMPLATE3 D a1, D a2, D a3
#define PARAMTEMPLATE4 D a1, D a2, D a3, D a4
#define PARAMTEMPLATE5 D a1, D a2, D a3, D a4, D a5
#define PARAMTEMPLATE6 D a1, D a2, D a3, D a4, D a5, D a6
#define PARAMTEMPLATE7 D a1, D a2, D a3, D a4, D a5, D a6, D a7
#define ARGTEMPLATE0
#define ARGTEMPLATE1 a1
#define ARGTEMPLATE2 a1, a2
#define ARGTEMPLATE3 a1, a2, a3
#define ARGTEMPLATE4 a1, a2, a3, a4
#define ARGTEMPLATE5 a1, a2, a3, a4, a5
#define ARGTEMPLATE6 a1, a2, a3, a4, a5, a6
#define ARGTEMPLATE7 a1, a2, a3, a4, a5, a6, a7
#define TYPETEMPLATE0
#define TYPETEMPLATE1 D
#define TYPETEMPLATE2 D, D
#define TYPETEMPLATE3 D, D, D
#define TYPETEMPLATE4 D, D, D, D
#define TYPETEMPLATE5 D, D, D, D, D
#define TYPETEMPLATE6 D, D, D, D, D, D
#define TYPETEMPLATE7 D, D, D, D, D, D, D
#define PARAMTEMPLATEPREFIX0
#define PARAMTEMPLATEPREFIX1 D a1,
#define PARAMTEMPLATEPREFIX2 D a1, D a2,
#define PARAMTEMPLATEPREFIX3 D a1, D a2, D a3,
#define PARAMTEMPLATEPREFIX4 D a1, D a2, D a3, D a4,
#define PARAMTEMPLATEPREFIX5 D a1, D a2, D a3, D a4, D a5,
#define PARAMTEMPLATEPREFIX6 D a1, D a2, D a3, D a4, D a5, D a6,
#define PARAMTEMPLATEPREFIX7 D a1, D a2, D a3, D a4, D a5, D a6, D a7,
#define ARGTEMPLATEPREFIX0
#define ARGTEMPLATEPREFIX1 a1,
#define ARGTEMPLATEPREFIX2 a1, a2,
#define ARGTEMPLATEPREFIX3 a1, a2, a3,
#define ARGTEMPLATEPREFIX4 a1, a2, a3, a4,
#define ARGTEMPLATEPREFIX5 a1, a2, a3, a4, a5,
#define ARGTEMPLATEPREFIX6 a1, a2, a3, a4, a5, a6,
#define ARGTEMPLATEPREFIX7 a1, a2, a3, a4, a5, a6, a7,
#define PARAMTEMPLATESUFFIX0
#define PARAMTEMPLATESUFFIX1 D a1
#define PARAMTEMPLATESUFFIX2 D a1, D a2
#define PARAMTEMPLATESUFFIX3 D a1, D a2, D a3
#define PARAMTEMPLATESUFFIX4 D a1, D a2, D a3, D a4
#define PARAMTEMPLATESUFFIX5 D a1, D a2, D a3, D a4, D a5
#define PARAMTEMPLATESUFFIX6 D a1, D a2, D a3, D a4, D a5, D a6
#define PARAMTEMPLATESUFFIX7 D a1, D a2, D a3, D a4, D a5, D a6, D a7
#define ARGTEMPLATESUFFIX0
#define ARGTEMPLATESUFFIX1 , a1
#define ARGTEMPLATESUFFIX2 , a1, a2
#define ARGTEMPLATESUFFIX3 , a1, a2, a3
#define ARGTEMPLATESUFFIX4 , a1, a2, a3, a4
#define ARGTEMPLATESUFFIX5 , a1, a2, a3, a4, a5
#define ARGTEMPLATESUFFIX6 , a1, a2, a3, a4, a5, a6
#define ARGTEMPLATESUFFIX7 , a1, a2, a3, a4, a5, a6, a7
#define ARGUMENTNAME1 a1
#define ARGUMENTNAME2 a2
#define ARGUMENTNAME3 a3
#define ARGUMENTNAME4 a4
#define ARGUMENTNAME5 a5
#define ARGUMENTNAME6 a6
#define ARGUMENTNAME7 a7
#define KLUDGEARGS0(_gubble)
#define KLUDGEARGS1(_gubble) \
        _gubble(0, a1)
#define KLUDGEARGS2(_gubble) \
     _gubble(0, a1); \
        _gubble(1, a2)
#define KLUDGEARGS3(_gubble) \
     _gubble(0, a1); \
     _gubble(1, a2); \
     _gubble(2, a3)
#define KLUDGEARGS4(_gubble) \
     _gubble(0, a1); \
     _gubble(1, a2); \
     _gubble(2, a3); \
     _gubble(3, a4)
#define KLUDGEARGS5(_gubble) \
     _gubble(0, a1); \
     _gubble(1, a2); \
     _gubble(2, a3); \
     _gubble(3, a4); \
     _gubble(4, a5)
#define KLUDGEARGS6(_gubble) \
     _gubble(0, a1); \
     _gubble(1, a2); \
     _gubble(2, a3); \
     _gubble(3, a4); \
     _gubble(4, a5); \
     _gubble(5, a6)
#define KLUDGEARGS7(_gubble) \
     _gubble(0, a1); \
     _gubble(1, a2); \
     _gubble(2, a3); \
     _gubble(3, a4); \
     _gubble(4, a5); \
     _gubble(5, a6); \
     _gubble(6, a7)

#define DEFERDECLARE extern


/* SINGLE METHOD ENGINE NODES

These are used to invoke a method with a specific next-method list.
The method is in data_1, the next-method data in data_2.
We use a different routine for different number of implementation
args;  i.e.
   single_method_engine_3
is used by a function of 3 required args, or of 2 required + optionals.

*/


#define DEFINE_SINGLE_METHOD_ENGINE(_nparams) \
D single_method_engine_##_nparams (PARAMTEMPLATE##_nparams) { \
    TEB* teb = get_teb(); \
    SINGLEMETHODENGINE* e = (SINGLEMETHODENGINE*)teb->function; \
    FN* meth = (FN*)e->meth; \
    DLFN mep = ((FN*)meth)->mep; \
    teb->function = meth; \
    teb->next_methods = e->data; \
    return(mep(ARGTEMPLATE##_nparams)); \
                                   }


DEFINE_SINGLE_METHOD_ENGINE(0)
DEFINE_SINGLE_METHOD_ENGINE(1)
DEFINE_SINGLE_METHOD_ENGINE(2)
DEFINE_SINGLE_METHOD_ENGINE(3)
DEFINE_SINGLE_METHOD_ENGINE(4)
DEFINE_SINGLE_METHOD_ENGINE(5)
DEFINE_SINGLE_METHOD_ENGINE(6)
DEFINE_SINGLE_METHOD_ENGINE(7)


D single_method_engine_n (D impargvec) {
  TEB* teb = get_teb();
  SINGLEMETHODENGINE* e = (SINGLEMETHODENGINE*)teb->function;
  return(primitive_mep_apply_with_optionals(e->meth, e->data, impargvec));
}



D check_explicit_kwds (SOV* optionals, SOV* kwds, int kwdskip) {
  D* optdata = vector_data(optionals);
  int optsize = vector_size(optionals);
  D* kwddata = vector_data(kwds);
  int kwdsize = vector_size(kwds);
  if ((optsize & 1) != 0) { // Check if odd?
    return(DFALSE);
  } else {
    int i;
    int j;
    for (i=0; i<optsize; i+=2) {
      D kwdarg = optdata[i];
      for (j=0; j<kwdsize; j+=kwdskip) {
        D kwd = kwddata[j];
        if (kwd == kwdarg) goto check_next;
      }
      return(kwdarg);
    check_next: ;
    }
    return(NULL);
  }
}

D check_unrestricted_kwds (SOV* optionals) {
  int optsize = vector_size(optionals);
  if ((optsize & 1) != 0) { // Check if odd?
    return(DFALSE);
  } else {
    return(NULL);
  }
}


#define KEYED_SKIP_COUNT_explicit 1
#define KEYED_SKIP_COUNT_implicit 2
#define KEYED_SKIP_COUNT_unrestricted 0

#define CHECK_KEYWORDS_explicit(_optionals, _meth, _e) \
    check_explicit_kwds((_optionals), (_e)->keywords, 1)

#define CHECK_KEYWORDS_implicit(_optionals, _meth, _e) \
    check_explicit_kwds((_optionals), ((KFN*)(_meth))->keyword_specifiers, 2)

#define CHECK_KEYWORDS_unrestricted(_optionals, _meth, _e) \
    check_unrestricted_kwds(_optionals)

#define INITMYARGVEC(argnum_, arg_) _argvec_data[argnum_] = arg_

extern D Kodd_number_of_keyword_args_trapVKeI(D gfargs, D gf, D engine);

extern D Kinvalid_keyword_trapVKeI(D gfargs, D gf, D engine, D badkwd, D keys, D implicitp);

#define INVALID_KEYWORD_explicit(invgf_, invargvec_, invbadkwd_, invmeth_, invengine_) \
  ( ( (invbadkwd_) == DFALSE) ? \
     Kodd_number_of_keyword_args_trapVKeI((invargvec_), (invgf_), (invengine_)) \
   : \
     Kinvalid_keyword_trapVKeI((invargvec_), (invgf_), (invengine_), (invbadkwd_), \
                                                   (invengine_)->keywords, DFALSE))


#define INVALID_KEYWORD_implicit(invgf_, invargvec_, invbadkwd_, invmeth_, invengine_) \
  ( ((invbadkwd_) == DFALSE) ? \
     Kodd_number_of_keyword_args_trapVKeI((invargvec_), (invgf_), (invengine_)) \
   : \
     Kinvalid_keyword_trapVKeI((invargvec_), (invgf_), (invengine_), (invbadkwd_), \
                                                   ((KFN*)(invengine_)->meth)->keyword_specifiers, \
                                                   DTRUE))

#define INVALID_KEYWORD_unrestricted(invgf_, invargvec_, invbadkwd_, invmeth_, invengine_) \
  ( ((invbadkwd_) == DFALSE) ? \
     Kodd_number_of_keyword_args_trapVKeI((invargvec_), (invgf_), (invengine_)) \
    : \
     Kinvalid_keyword_trapVKeI((invargvec_), (invgf_), (invengine_), (invbadkwd_), \
                                                   DFALSE, DFALSE))


#define DEFINE_KEYED_SINGLE_METHOD_ENGINE(_how, _nparams) \
  D  _how##_keyed_single_method_engine_##_nparams (PARAMTEMPLATEPREFIX##_nparams SOV* optionals) \
  {  TEB* teb = get_teb(); \
    SINGLEMETHODENGINE* e = (SINGLEMETHODENGINE*)teb->function; \
    D parent = teb->next_methods; \
    FN* meth = (FN*)e->meth; \
    D badkwd; \
    badkwd = CHECK_KEYWORDS_##_how(optionals, meth, e); \
    if (badkwd == NULL) { \
      teb->function = meth; \
      teb->next_methods = e->data; \
      return((((FN*)meth)->mep)(ARGTEMPLATEPREFIX##_nparams optionals)); \
    } else { \
      int _argvecsize = _nparams + 1; \
      DEF_STACK_VECTOR_INITTED(_argvec, _argvecsize); \
      D* _argvec_data = vector_data(_argvec); \
      KLUDGEARGS##_nparams(INITMYARGVEC ); \
      _argvec_data[_nparams] = optionals; \
      return(INVALID_KEYWORD_##_how(parent_gf(parent), _argvec, badkwd, meth, e)); \
    } \
  }


#define DEFINE_KEYED_SINGLE_METHOD_ENGINE_UNSPREAD(_how) \
  D  _how##_keyed_single_method_engine_n (SOV* mepargvec) \
  { TEB* teb = get_teb(); \
    SINGLEMETHODENGINE* e = (SINGLEMETHODENGINE*)teb->function; \
    D parent = teb->next_methods; \
    FN* meth = (FN*)e->meth; \
    D badkwd; \
    int nimpargs = vector_size(mepargvec); \
    badkwd = CHECK_KEYWORDS_##_how(vector_ref(mepargvec, nimpargs-1), meth, e); \
    if (badkwd == NULL) { \
      return(primitive_mep_apply_with_optionals(meth, e->data, (D)mepargvec)); \
    } else { \
      return(INVALID_KEYWORD_##_how(parent_gf(parent), mepargvec, badkwd, meth, e)); \
    } \
  }


DEFINE_KEYED_SINGLE_METHOD_ENGINE_UNSPREAD(explicit)
DEFINE_KEYED_SINGLE_METHOD_ENGINE_UNSPREAD(implicit)
DEFINE_KEYED_SINGLE_METHOD_ENGINE_UNSPREAD(unrestricted)



DEFINE_KEYED_SINGLE_METHOD_ENGINE(explicit, 0)
DEFINE_KEYED_SINGLE_METHOD_ENGINE(explicit, 1)
DEFINE_KEYED_SINGLE_METHOD_ENGINE(explicit, 2)
DEFINE_KEYED_SINGLE_METHOD_ENGINE(explicit, 3)
DEFINE_KEYED_SINGLE_METHOD_ENGINE(explicit, 4)
DEFINE_KEYED_SINGLE_METHOD_ENGINE(explicit, 5)
DEFINE_KEYED_SINGLE_METHOD_ENGINE(explicit, 6)
DEFINE_KEYED_SINGLE_METHOD_ENGINE(implicit, 0)
DEFINE_KEYED_SINGLE_METHOD_ENGINE(implicit, 1)
DEFINE_KEYED_SINGLE_METHOD_ENGINE(implicit, 2)
DEFINE_KEYED_SINGLE_METHOD_ENGINE(implicit, 3)
DEFINE_KEYED_SINGLE_METHOD_ENGINE(implicit, 4)
DEFINE_KEYED_SINGLE_METHOD_ENGINE(implicit, 5)
DEFINE_KEYED_SINGLE_METHOD_ENGINE(implicit, 6)
DEFINE_KEYED_SINGLE_METHOD_ENGINE(unrestricted, 0)
DEFINE_KEYED_SINGLE_METHOD_ENGINE(unrestricted, 1)
DEFINE_KEYED_SINGLE_METHOD_ENGINE(unrestricted, 2)
DEFINE_KEYED_SINGLE_METHOD_ENGINE(unrestricted, 3)
DEFINE_KEYED_SINGLE_METHOD_ENGINE(unrestricted, 4)
DEFINE_KEYED_SINGLE_METHOD_ENGINE(unrestricted, 5)
DEFINE_KEYED_SINGLE_METHOD_ENGINE(unrestricted, 6)

/* **************************************************************** */


/* A "cache-header" entry is an entrypoint for use by a cache header of some
   type, which has had its "next" slot (in data_1) filled in.  There may be
   an issue of it being possible for this to be a method in addition to
   another engine-node, but for the simple caching I'm imagining now that
   would be nonsensical.  (This is only an issue for the C backend anyway,
   just as it's an issue for various discriminators.)
   */

#define DEFINE_CACHE_HEADER_ENGINE(_nparams) \
D cache_header_engine_##_nparams (PARAMTEMPLATE##_nparams) { \
    TEB* teb = get_teb(); \
    CACHEHEADERENGINE* e = (CACHEHEADERENGINE*)teb->function; \
    ENGINE* nxt = (ENGINE*)e->nextnode; \
    DLFN entrypt = nxt->entry_point; \
    teb->function = (FN*)nxt; \
    teb->next_methods = (D)e; \
    return(entrypt(ARGTEMPLATE##_nparams)); \
   }

extern D cache_header_engine_0 ();
extern D cache_header_engine_1 (D a1);
extern D cache_header_engine_2 (D a1, D a2);
extern D cache_header_engine_3 (D a1, D a2, D a3);
extern D cache_header_engine_4 (D a1, D a2, D a3, D a4);
extern D cache_header_engine_5 (D a1, D a2, D a3, D a4, D a5);
extern D cache_header_engine_6 (D a1, D a2, D a3, D a4, D a5, D a6);
extern D cache_header_engine_7 (D a1, D a2, D a3, D a4, D a5, D a6, D a7);
extern D cache_header_engine_n (D argvec);


DEFINE_CACHE_HEADER_ENGINE(0)
DEFINE_CACHE_HEADER_ENGINE(1)
DEFINE_CACHE_HEADER_ENGINE(2)
DEFINE_CACHE_HEADER_ENGINE(3)
DEFINE_CACHE_HEADER_ENGINE(4)
DEFINE_CACHE_HEADER_ENGINE(5)
DEFINE_CACHE_HEADER_ENGINE(6)
DEFINE_CACHE_HEADER_ENGINE(7)

D cache_header_engine_n (D theargvec) {
  TEB* teb = get_teb();
  SOV* argvec = (SOV*)theargvec;
  CACHEHEADERENGINE* e = (CACHEHEADERENGINE*)teb->function;
  ENGINE* newengine = (ENGINE*)(e->nextnode);
  if (FUNCTIONP(newengine)) {
    return(primitive_mep_apply_with_optionals((FN*)newengine, (D)e, argvec));
  } else {
    teb->function = (FN*)newengine;
    teb->next_methods = (D)e;
    return((newengine->entry_point)(argvec));
  }
}

#define DEFINE_PROFILING_CACHE_HEADER_ENGINE(_nparams) \
D profiling_cache_header_engine_##_nparams (PARAMTEMPLATE##_nparams) { \
    TEB* teb = get_teb(); \
    PROFILINGCACHEHEADERENGINE* e = (PROFILINGCACHEHEADERENGINE*)teb->function; \
    ENGINE* nxt = (ENGINE*)e->nextnode; \
    DLFN entrypt = nxt->entry_point; \
    teb->function = (FN*)nxt; \
    teb->next_methods = (D)e; \
    e->count1 += 4; \
    if (unlikely((D)(e->count1) == I(0))) e->count2 += 4; \
    return(entrypt(ARGTEMPLATE##_nparams)); \
   }

extern D profiling_cache_header_engine_0 ();
extern D profiling_cache_header_engine_1 (D a1);
extern D profiling_cache_header_engine_2 (D a1, D a2);
extern D profiling_cache_header_engine_3 (D a1, D a2, D a3);
extern D profiling_cache_header_engine_4 (D a1, D a2, D a3, D a4);
extern D profiling_cache_header_engine_5 (D a1, D a2, D a3, D a4, D a5);
extern D profiling_cache_header_engine_6 (D a1, D a2, D a3, D a4, D a5, D a6);
extern D profiling_cache_header_engine_7 (D a1, D a2, D a3, D a4, D a5, D a6, D a7);
extern D profiling_cache_header_engine_n (D argvec);


DEFINE_PROFILING_CACHE_HEADER_ENGINE(0)
DEFINE_PROFILING_CACHE_HEADER_ENGINE(1)
DEFINE_PROFILING_CACHE_HEADER_ENGINE(2)
DEFINE_PROFILING_CACHE_HEADER_ENGINE(3)
DEFINE_PROFILING_CACHE_HEADER_ENGINE(4)
DEFINE_PROFILING_CACHE_HEADER_ENGINE(5)
DEFINE_PROFILING_CACHE_HEADER_ENGINE(6)
DEFINE_PROFILING_CACHE_HEADER_ENGINE(7)


D profiling_cache_header_engine_n (D theargvec) {
  TEB* teb = get_teb();
  SOV* argvec = (SOV*)theargvec;
  CACHEHEADERENGINE* e = (CACHEHEADERENGINE*)teb->function;
  ENGINE* newengine = (ENGINE*)(e->nextnode);
  if (FUNCTIONP(newengine)) {
    return(primitive_mep_apply_with_optionals((FN*)newengine, (D)e, argvec));
  } else {
    teb->function = (FN*)newengine;
    teb->next_methods = (D)e;
    return((newengine->entry_point)(argvec));
  }
}

D primitive_enable_cache_header_engine_node (D engine, D genfun) {
  ENGINE* e = (ENGINE*)engine;
  GFN* gf = (GFN*)genfun;
  SIG* sig = (SIG*)gf->signature;
  DUMINT props = (DUMINT)e->properties;
  DUMINT etype = (props & EPROPS_M_ENTRY_TYPE) >> EPROPS_V_ENTRY_TYPE;
  switch (etype) {
  case ENGINE_cache_header: {
    switch (signature_number_required(sig) + signature_optionals_p(sig)) {
    case 0: e->entry_point = (DLFN)&cache_header_engine_0; break;
    case 1: e->entry_point = (DLFN)&cache_header_engine_1; break;
    case 2: e->entry_point = (DLFN)&cache_header_engine_2; break;
    case 3: e->entry_point = (DLFN)&cache_header_engine_3; break;
    case 4: e->entry_point = (DLFN)&cache_header_engine_4; break;
    case 5: e->entry_point = (DLFN)&cache_header_engine_5; break;
    case 6: e->entry_point = (DLFN)&cache_header_engine_6; break;
    case 7: e->entry_point = (DLFN)&cache_header_engine_7; break;
    };
    break;
  }
  case ENGINE_profiling_cache_header: {
    switch (signature_number_required(sig) + signature_optionals_p(sig)) {
    case 0: e->entry_point = (DLFN)&profiling_cache_header_engine_0; break;
    case 1: e->entry_point = (DLFN)&profiling_cache_header_engine_1; break;
    case 2: e->entry_point = (DLFN)&profiling_cache_header_engine_2; break;
    case 3: e->entry_point = (DLFN)&profiling_cache_header_engine_3; break;
    case 4: e->entry_point = (DLFN)&profiling_cache_header_engine_4; break;
    case 5: e->entry_point = (DLFN)&profiling_cache_header_engine_5; break;
    case 6: e->entry_point = (DLFN)&profiling_cache_header_engine_6; break;
    case 7: e->entry_point = (DLFN)&profiling_cache_header_engine_7; break;
    };
    break;
  }
  }
  return(engine);
}


D primitive_invalidate_cache_engine_node (D engine, D genfun) {
  ignore(genfun);
  ((ENGINE*)engine)->entry_point = (DLFN)&general_engine_node_n_engine;
  return(engine);
}


/* **************************************************************** */

D primitive_initialize_engine_node (D engine) {
  ENGINE* eng = (ENGINE*)engine;
  DUMINT props = (DUMINT)eng->properties;
  DUMINT etype = (props & EPROPS_M_ENTRY_TYPE) >> EPROPS_V_ENTRY_TYPE;
  switch (etype) {
  case ENGINE_absent:
    eng->entry_point = (DLFN)general_engine_node_n_engine;
    break;
  case ENGINE_ambiguous_methods:
  case ENGINE_inapplicable:
    eng->entry_point = (DLFN)general_engine_node_spread_engine;
    break;
  case ENGINE_unkeyed_single_method:
  case ENGINE_implicit_keyed_single_method:
  case ENGINE_explicit_keyed_single_method:
  case ENGINE_unrestricted_keyed_single_method: {
    SINGLEMETHODENGINE* e = (SINGLEMETHODENGINE*)eng;
    FN* meth = (FN*)e->meth;
    SIG* sig = (SIG*)meth->signature;
    int nreq = signature_number_required(sig);
    int impargs = nreq + signature_optionals_p(sig);
    switch (etype) {
    case ENGINE_unkeyed_single_method: {
      switch (impargs) {
      case 0: e->entry_point = (DLFN)single_method_engine_0; break;
      case 1: e->entry_point = (DLFN)single_method_engine_1; break;
      case 2: e->entry_point = (DLFN)single_method_engine_2; break;
      case 3: e->entry_point = (DLFN)single_method_engine_3; break;
      case 4: e->entry_point = (DLFN)single_method_engine_4; break;
      case 5: e->entry_point = (DLFN)single_method_engine_5; break;
      case 6: e->entry_point = (DLFN)single_method_engine_6; break;
      case 7: e->entry_point = (DLFN)single_method_engine_7; break;
      default: e->entry_point = (DLFN)single_method_engine_n; break;
      }
      break;
    }
    case ENGINE_explicit_keyed_single_method: {
      switch (nreq) {
      case 0: e->entry_point = (DLFN)explicit_keyed_single_method_engine_0; break;
      case 1: e->entry_point = (DLFN)explicit_keyed_single_method_engine_1; break;
      case 2: e->entry_point = (DLFN)explicit_keyed_single_method_engine_2; break;
      case 3: e->entry_point = (DLFN)explicit_keyed_single_method_engine_3; break;
      case 4: e->entry_point = (DLFN)explicit_keyed_single_method_engine_4; break;
      case 5: e->entry_point = (DLFN)explicit_keyed_single_method_engine_5; break;
      case 6: e->entry_point = (DLFN)explicit_keyed_single_method_engine_6; break;
      default: e->entry_point = (DLFN)explicit_keyed_single_method_engine_n; break;
      }
      break;
    }
    case ENGINE_implicit_keyed_single_method: {
      switch (nreq) {
      case 0: e->entry_point = (DLFN)implicit_keyed_single_method_engine_0; break;
      case 1: e->entry_point = (DLFN)implicit_keyed_single_method_engine_1; break;
      case 2: e->entry_point = (DLFN)implicit_keyed_single_method_engine_2; break;
      case 3: e->entry_point = (DLFN)implicit_keyed_single_method_engine_3; break;
      case 4: e->entry_point = (DLFN)implicit_keyed_single_method_engine_4; break;
      case 5: e->entry_point = (DLFN)implicit_keyed_single_method_engine_5; break;
      case 6: e->entry_point = (DLFN)implicit_keyed_single_method_engine_6; break;
      default: e->entry_point = (DLFN)implicit_keyed_single_method_engine_n; break;
      }
      break;
    }
    case ENGINE_unrestricted_keyed_single_method: {
      switch (nreq) {
      case 0: e->entry_point = (DLFN)unrestricted_keyed_single_method_engine_0; break;
      case 1: e->entry_point = (DLFN)unrestricted_keyed_single_method_engine_1; break;
      case 2: e->entry_point = (DLFN)unrestricted_keyed_single_method_engine_2; break;
      case 3: e->entry_point = (DLFN)unrestricted_keyed_single_method_engine_3; break;
      case 4: e->entry_point = (DLFN)unrestricted_keyed_single_method_engine_4; break;
      case 5: e->entry_point = (DLFN)unrestricted_keyed_single_method_engine_5; break;
      case 6: e->entry_point = (DLFN)unrestricted_keyed_single_method_engine_6; break;
      default: e->entry_point = (DLFN)unrestricted_keyed_single_method_engine_n; break;
      }
    break;
    }
    }
    break;
  }
  case ENGINE_reserved_terminal_n_a:
  case ENGINE_reserved_terminal_n_b:
  case ENGINE_reserved_terminal_n_c:
  case ENGINE_reserved_terminal_n_d:
  case ENGINE_reserved_terminal_n_e:
  case ENGINE_reserved_terminal_n_f:
  case ENGINE_reserved_terminal_n_g:
  case ENGINE_profiling_cache_header:
  case ENGINE_cache_header:
    primitive_enable_cache_header_engine_node(eng, parent_gf(eng));
    /* eng->entry_point = (DLFN)general_engine_node_n_engine; */
    break;
  case ENGINE_boxed_instance_slot_getter:
    eng->entry_point = (DLFN)boxed_instance_slot_getter_engine;
    break;
  case ENGINE_boxed_instance_slot_setter:
    eng->entry_point = (DLFN)boxed_instance_slot_setter_engine;
    break;
  case ENGINE_boxed_repeated_instance_slot_getter:
    eng->entry_point = (DLFN)boxed_repeated_instance_slot_getter_engine;
    break;
  case ENGINE_boxed_repeated_instance_slot_setter:
    eng->entry_point = (DLFN)boxed_repeated_instance_slot_setter_engine;
    break;
  case ENGINE_raw_byte_repeated_instance_slot_getter:
    eng->entry_point = (DLFN)raw_byte_repeated_instance_slot_getter_engine;
    break;
  case ENGINE_raw_byte_repeated_instance_slot_setter:
    eng->entry_point = (DLFN)raw_byte_repeated_instance_slot_setter_engine;
    break;
  case ENGINE_boxed_class_slot_getter:
  case ENGINE_reserved_slot_a_getter:
  case ENGINE_reserved_slot_b_getter:
    eng->entry_point = (DLFN)general_engine_node_1_engine;
    break;
  case ENGINE_reserved_slot_a_setter:
  case ENGINE_reserved_slot_b_setter:
  case ENGINE_boxed_class_slot_setter:
  case ENGINE_reserved_repeated_slot_a_getter:
  case ENGINE_reserved_repeated_slot_b_getter:
    eng->entry_point = (DLFN)general_engine_node_2_engine;
    break;
  case ENGINE_reserved_repeated_slot_a_setter:
  case ENGINE_reserved_repeated_slot_b_setter:
    eng->entry_point = (DLFN)general_engine_node_3_engine;
    break;
  default:
    /* FMH */
    ;
  }
  return(engine);
}



/* **************************************************************** */


#define DEFINE_DISCRIMINATOR_ENGINE(_argnum, _nargs) \
  D discriminate_engine_##_argnum##_##_nargs (PARAMTEMPLATE##_nargs) { \
    TEB* teb = get_teb(); \
    ENGINE* d_ = (ENGINE*)teb->function; \
    D parent_ = teb->next_methods; \
    DLFN cb_ = d_->callback; \
    ENGINE* newengine_ = (ENGINE*)(cb_((ARGUMENTNAME##_argnum), parent_, d_)); \
    DLFN ncb_ = newengine_->entry_point; \
    teb->function = (FN*)newengine_; \
    teb->next_methods = parent_; \
    return(ncb_(ARGTEMPLATE##_nargs)); \
  }

DEFINE_DISCRIMINATOR_ENGINE(1, 1)
DEFINE_DISCRIMINATOR_ENGINE(1, 2)
DEFINE_DISCRIMINATOR_ENGINE(1, 3)
DEFINE_DISCRIMINATOR_ENGINE(1, 4)
DEFINE_DISCRIMINATOR_ENGINE(1, 5)
DEFINE_DISCRIMINATOR_ENGINE(1, 6)
DEFINE_DISCRIMINATOR_ENGINE(1, 7)
DEFINE_DISCRIMINATOR_ENGINE(2, 2)
DEFINE_DISCRIMINATOR_ENGINE(2, 3)
DEFINE_DISCRIMINATOR_ENGINE(2, 4)
DEFINE_DISCRIMINATOR_ENGINE(2, 5)
DEFINE_DISCRIMINATOR_ENGINE(2, 6)
DEFINE_DISCRIMINATOR_ENGINE(2, 7)
DEFINE_DISCRIMINATOR_ENGINE(3, 3)
DEFINE_DISCRIMINATOR_ENGINE(3, 4)
DEFINE_DISCRIMINATOR_ENGINE(3, 5)
DEFINE_DISCRIMINATOR_ENGINE(3, 6)
DEFINE_DISCRIMINATOR_ENGINE(3, 7)
DEFINE_DISCRIMINATOR_ENGINE(4, 4)
DEFINE_DISCRIMINATOR_ENGINE(4, 5)
DEFINE_DISCRIMINATOR_ENGINE(4, 6)
DEFINE_DISCRIMINATOR_ENGINE(4, 7)
DEFINE_DISCRIMINATOR_ENGINE(5, 5)
DEFINE_DISCRIMINATOR_ENGINE(5, 6)
DEFINE_DISCRIMINATOR_ENGINE(5, 7)
DEFINE_DISCRIMINATOR_ENGINE(6, 6)
DEFINE_DISCRIMINATOR_ENGINE(6, 7)
DEFINE_DISCRIMINATOR_ENGINE(7, 7)


D discriminate_engine_n_n (SOV* args) {
  TEB* teb = get_teb();
  ENGINE* e = (ENGINE*)teb->function;
  D parent = teb->next_methods;
  DLFN cb = e->callback;
  long props = (long)e->properties;
  long argnum = (props >> 8) & 0xFF;
  D* a = vector_data(args);
  D arg = a[argnum];
  ENGINE* newengine = (ENGINE*)(cb(arg, parent, e));
  if (FUNCTIONP(newengine)) {
    return(primitive_mep_apply_with_optionals((FN*)newengine, parent, args));
  } else {
    teb->function = (FN*)newengine;
    teb->next_methods = parent;
    return((newengine->entry_point)(args));
  }
}

/* ---------------------------------------------- */

extern D Dabsent_engine_nodeVKg;
extern D Ddirect_object_mm_wrappersVKi;

#define MONO_WRAPPER_KEY(x) \
  (TAGGEDQ(x) ? ((D*)Ddirect_object_mm_wrappersVKi)[TAG_BITS(x)] : ((OBJECT*)x)->mm_wrapper)

#define DEFINE_MONOMORPHIC_DISCRIMINATOR(_argnum, _nargs) \
  D monomorphic_discriminator_engine_##_argnum##_##_nargs (PARAMTEMPLATE##_nargs) { \
    TEB* teb = get_teb(); \
    MONOMORPHICDISCRIMINATOR* d_ = (MONOMORPHICDISCRIMINATOR*)teb->function; \
    D parent_ = teb->next_methods; \
    DWORD key = (DWORD)(FI(MONO_WRAPPER_KEY(ARGUMENTNAME##_argnum))); \
    ENGINE* newengine_ = (ENGINE*)((key == d_->key) \
                                    ? d_->nextnode \
                                    : Dabsent_engine_nodeVKg);  \
    DLFN ncb_ = newengine_->entry_point; \
    teb->function = (FN*)newengine_; \
    teb->next_methods = parent_; \
    return(ncb_(ARGTEMPLATE##_nargs)); \
  }

DEFINE_MONOMORPHIC_DISCRIMINATOR(1, 1)
DEFINE_MONOMORPHIC_DISCRIMINATOR(1, 2)
DEFINE_MONOMORPHIC_DISCRIMINATOR(1, 3)
DEFINE_MONOMORPHIC_DISCRIMINATOR(1, 4)
DEFINE_MONOMORPHIC_DISCRIMINATOR(1, 5)
DEFINE_MONOMORPHIC_DISCRIMINATOR(1, 6)
DEFINE_MONOMORPHIC_DISCRIMINATOR(1, 7)
DEFINE_MONOMORPHIC_DISCRIMINATOR(2, 2)
DEFINE_MONOMORPHIC_DISCRIMINATOR(2, 3)
DEFINE_MONOMORPHIC_DISCRIMINATOR(2, 4)
DEFINE_MONOMORPHIC_DISCRIMINATOR(2, 5)
DEFINE_MONOMORPHIC_DISCRIMINATOR(2, 6)
DEFINE_MONOMORPHIC_DISCRIMINATOR(2, 7)
DEFINE_MONOMORPHIC_DISCRIMINATOR(3, 3)
DEFINE_MONOMORPHIC_DISCRIMINATOR(3, 4)
DEFINE_MONOMORPHIC_DISCRIMINATOR(3, 5)
DEFINE_MONOMORPHIC_DISCRIMINATOR(3, 6)
DEFINE_MONOMORPHIC_DISCRIMINATOR(3, 7)
DEFINE_MONOMORPHIC_DISCRIMINATOR(4, 4)
DEFINE_MONOMORPHIC_DISCRIMINATOR(4, 5)
DEFINE_MONOMORPHIC_DISCRIMINATOR(4, 6)
DEFINE_MONOMORPHIC_DISCRIMINATOR(4, 7)
DEFINE_MONOMORPHIC_DISCRIMINATOR(5, 5)
DEFINE_MONOMORPHIC_DISCRIMINATOR(5, 6)
DEFINE_MONOMORPHIC_DISCRIMINATOR(5, 7)
DEFINE_MONOMORPHIC_DISCRIMINATOR(6, 6)
DEFINE_MONOMORPHIC_DISCRIMINATOR(6, 7)
DEFINE_MONOMORPHIC_DISCRIMINATOR(7, 7)


D monomorphic_discriminator_engine_n_n (SOV* args) {
  TEB* teb = get_teb();
  MONOMORPHICDISCRIMINATOR* e = (MONOMORPHICDISCRIMINATOR*)teb->function;
  D parent = teb->next_methods;
  long props = (long)e->properties;
  long argnum = (props >> 8) & 0xFF;
  D* a = vector_data(args);
  OBJECT* arg = (OBJECT*)a[argnum];
  DWORD key = (DWORD)(FI(MONO_WRAPPER_KEY(arg)));
  ENGINE* newengine = (ENGINE*)((key == e->key)
                                ? e->nextnode
                                : Dabsent_engine_nodeVKg);
  if (FUNCTIONP(newengine)) {
    return(primitive_mep_apply_with_optionals((FN*)newengine, parent, args));
  } else {
    teb->function = (FN*)newengine;
    teb->next_methods = parent;
    return((newengine->entry_point)(args));
  }
}

/* ---------------------------------------------- */

extern D Dinapplicable_engine_nodeVKg;

#define DEFINE_IF_TYPE_DISCRIMINATOR(_argnum, _nargs) \
  D if_type_discriminator_engine_##_argnum##_##_nargs (PARAMTEMPLATE##_nargs) { \
    TEB* teb = get_teb(); \
    IFTYPEDISCRIMINATOR* d_ = (IFTYPEDISCRIMINATOR*)teb->function; \
    D parent_ = teb->next_methods; \
    ENGINE* newengine_ = (ENGINE*)((INSTANCEP((ARGUMENTNAME##_argnum),d_->type)) \
                                   ? d_->thennode \
                                   : d_->elsenode); \
    DLFN ncb_ = newengine_->entry_point; \
    teb->function = (FN*)newengine_; \
    teb->next_methods = parent_; \
    return(ncb_(ARGTEMPLATE##_nargs)); \
  }

DEFINE_IF_TYPE_DISCRIMINATOR(1, 1)
DEFINE_IF_TYPE_DISCRIMINATOR(1, 2)
DEFINE_IF_TYPE_DISCRIMINATOR(1, 3)
DEFINE_IF_TYPE_DISCRIMINATOR(1, 4)
DEFINE_IF_TYPE_DISCRIMINATOR(1, 5)
DEFINE_IF_TYPE_DISCRIMINATOR(1, 6)
DEFINE_IF_TYPE_DISCRIMINATOR(1, 7)
DEFINE_IF_TYPE_DISCRIMINATOR(2, 2)
DEFINE_IF_TYPE_DISCRIMINATOR(2, 3)
DEFINE_IF_TYPE_DISCRIMINATOR(2, 4)
DEFINE_IF_TYPE_DISCRIMINATOR(2, 5)
DEFINE_IF_TYPE_DISCRIMINATOR(2, 6)
DEFINE_IF_TYPE_DISCRIMINATOR(2, 7)
DEFINE_IF_TYPE_DISCRIMINATOR(3, 3)
DEFINE_IF_TYPE_DISCRIMINATOR(3, 4)
DEFINE_IF_TYPE_DISCRIMINATOR(3, 5)
DEFINE_IF_TYPE_DISCRIMINATOR(3, 6)
DEFINE_IF_TYPE_DISCRIMINATOR(3, 7)
DEFINE_IF_TYPE_DISCRIMINATOR(4, 4)
DEFINE_IF_TYPE_DISCRIMINATOR(4, 5)
DEFINE_IF_TYPE_DISCRIMINATOR(4, 6)
DEFINE_IF_TYPE_DISCRIMINATOR(4, 7)
DEFINE_IF_TYPE_DISCRIMINATOR(5, 5)
DEFINE_IF_TYPE_DISCRIMINATOR(5, 6)
DEFINE_IF_TYPE_DISCRIMINATOR(5, 7)
DEFINE_IF_TYPE_DISCRIMINATOR(6, 6)
DEFINE_IF_TYPE_DISCRIMINATOR(6, 7)
DEFINE_IF_TYPE_DISCRIMINATOR(7, 7)


D if_type_discriminator_engine_n_n (SOV* args) {
  TEB* teb = get_teb();
  IFTYPEDISCRIMINATOR* e = (IFTYPEDISCRIMINATOR*)teb->function;
  D parent = teb->next_methods;
  long props = (long)e->properties;
  long argnum = (props >> 8) & 0xFF;
  D* a = vector_data(args);
  D arg = a[argnum];
  ENGINE* newengine = (ENGINE*)(INSTANCEP(arg, e->type)
                                ? e->thennode
                                : e->elsenode);
  if (FUNCTIONP(newengine)) {
    return(primitive_mep_apply_with_optionals((FN*)newengine, parent, args));
  } else {
    teb->function = (FN*)newengine;
    teb->next_methods = parent;
    return((newengine->entry_point)(args));
  }
}

/* ---------------------------------------------- */

extern D Dinapplicable_engine_nodeVKg;

#define DEFINE_TYPECHECK_DISCRIMINATOR(_argnum, _nargs) \
  D typecheck_discriminator_engine_##_argnum##_##_nargs (PARAMTEMPLATE##_nargs) { \
    TEB* teb = get_teb(); \
    TYPECHECKDISCRIMINATOR* d_ = (TYPECHECKDISCRIMINATOR*)teb->function; \
    D parent_ = teb->next_methods; \
    ENGINE* newengine_ = (ENGINE*)((INSTANCEP((ARGUMENTNAME##_argnum),d_->type)) \
                                   ? d_->nextnode \
                                   : Dinapplicable_engine_nodeVKg); \
    DLFN ncb_ = newengine_->entry_point; \
    teb->function = (FN*)newengine_; \
    teb->next_methods = parent_; \
    return(ncb_(ARGTEMPLATE##_nargs)); \
  }

DEFINE_TYPECHECK_DISCRIMINATOR(1, 1)
DEFINE_TYPECHECK_DISCRIMINATOR(1, 2)
DEFINE_TYPECHECK_DISCRIMINATOR(1, 3)
DEFINE_TYPECHECK_DISCRIMINATOR(1, 4)
DEFINE_TYPECHECK_DISCRIMINATOR(1, 5)
DEFINE_TYPECHECK_DISCRIMINATOR(1, 6)
DEFINE_TYPECHECK_DISCRIMINATOR(1, 7)
DEFINE_TYPECHECK_DISCRIMINATOR(2, 2)
DEFINE_TYPECHECK_DISCRIMINATOR(2, 3)
DEFINE_TYPECHECK_DISCRIMINATOR(2, 4)
DEFINE_TYPECHECK_DISCRIMINATOR(2, 5)
DEFINE_TYPECHECK_DISCRIMINATOR(2, 6)
DEFINE_TYPECHECK_DISCRIMINATOR(2, 7)
DEFINE_TYPECHECK_DISCRIMINATOR(3, 3)
DEFINE_TYPECHECK_DISCRIMINATOR(3, 4)
DEFINE_TYPECHECK_DISCRIMINATOR(3, 5)
DEFINE_TYPECHECK_DISCRIMINATOR(3, 6)
DEFINE_TYPECHECK_DISCRIMINATOR(3, 7)
DEFINE_TYPECHECK_DISCRIMINATOR(4, 4)
DEFINE_TYPECHECK_DISCRIMINATOR(4, 5)
DEFINE_TYPECHECK_DISCRIMINATOR(4, 6)
DEFINE_TYPECHECK_DISCRIMINATOR(4, 7)
DEFINE_TYPECHECK_DISCRIMINATOR(5, 5)
DEFINE_TYPECHECK_DISCRIMINATOR(5, 6)
DEFINE_TYPECHECK_DISCRIMINATOR(5, 7)
DEFINE_TYPECHECK_DISCRIMINATOR(6, 6)
DEFINE_TYPECHECK_DISCRIMINATOR(6, 7)
DEFINE_TYPECHECK_DISCRIMINATOR(7, 7)


D typecheck_discriminator_engine_n_n (SOV* args) {
  TEB* teb = get_teb();
  TYPECHECKDISCRIMINATOR* e = (TYPECHECKDISCRIMINATOR*)teb->function;
  D parent = teb->next_methods;
  long props = (long)e->properties;
  long argnum = (props >> 8) & 0xFF;
  D* a = vector_data(args);
  D arg = a[argnum];
  ENGINE* newengine = (ENGINE*)(INSTANCEP(arg, e->type)
                                ? e->nextnode
                                : Dinapplicable_engine_nodeVKg);
  if (FUNCTIONP(newengine)) {
    return(primitive_mep_apply_with_optionals((FN*)newengine, parent, args));
  } else {
    teb->function = (FN*)newengine;
    teb->next_methods = parent;
    return((newengine->entry_point)(args));
  }
}

/* ---------------------------------------------- */

D primitive_initialize_discriminator(D discriminator) {
  ENGINE* d = (ENGINE*)discriminator;
  long props = (long)d->properties;
  long argnum = ((props & DPROPS_M_ARGNUM) >> DPROPS_V_ARGNUM);
  long nreq = ((props & DPROPS_M_NREQUIRED) >> DPROPS_V_NREQUIRED);
  long optionals = ((props & DPROPS_M_OPTIONALS) >> DPROPS_V_OPTIONALS);
  long impargs = nreq + optionals;
  long etype = ((props & EPROPS_M_ENTRY_TYPE) >> EPROPS_V_ENTRY_TYPE);
  DLFN handler;
  if (etype == ENGINE_if_type) {
    switch (impargs) {
    case 1: handler = if_type_discriminator_engine_1_1; break;
    case 2:
      switch (argnum) {
      case 0: handler = if_type_discriminator_engine_1_2; break;
      case 1: handler = if_type_discriminator_engine_2_2; break;
      }
      break;
    case 3:
      switch (argnum) {
      case 0: handler = if_type_discriminator_engine_1_3; break;
      case 1: handler = if_type_discriminator_engine_2_3; break;
      case 2: handler = if_type_discriminator_engine_3_3; break;
      }
      break;
    case 4:
      switch (argnum) {
      case 0: handler = if_type_discriminator_engine_1_4; break;
      case 1: handler = if_type_discriminator_engine_2_4; break;
      case 2: handler = if_type_discriminator_engine_3_4; break;
      case 3: handler = if_type_discriminator_engine_4_4; break;
      }
      break;
    case 5:
      switch (argnum) {
      case 0: handler = if_type_discriminator_engine_1_5; break;
      case 1: handler = if_type_discriminator_engine_2_5; break;
      case 2: handler = if_type_discriminator_engine_3_5; break;
      case 3: handler = if_type_discriminator_engine_4_5; break;
      case 4: handler = if_type_discriminator_engine_5_5; break;
      }
      break;
    case 6:
      switch (argnum) {
      case 0: handler = if_type_discriminator_engine_1_6; break;
      case 1: handler = if_type_discriminator_engine_2_6; break;
      case 2: handler = if_type_discriminator_engine_3_6; break;
      case 3: handler = if_type_discriminator_engine_4_6; break;
      case 4: handler = if_type_discriminator_engine_5_6; break;
      case 5: handler = if_type_discriminator_engine_6_6; break;
      }
      break;
    case 7:
      switch (argnum) {
      case 0: handler = if_type_discriminator_engine_1_7; break;
      case 1: handler = if_type_discriminator_engine_2_7; break;
      case 2: handler = if_type_discriminator_engine_3_7; break;
      case 3: handler = if_type_discriminator_engine_4_7; break;
      case 4: handler = if_type_discriminator_engine_5_7; break;
      case 5: handler = if_type_discriminator_engine_6_7; break;
      case 6: handler = if_type_discriminator_engine_7_7; break;
      }
      break;
    default:
      handler = if_type_discriminator_engine_n_n;
      break;
    }
  } else if (etype == ENGINE_typecheck) {
    switch (impargs) {
    case 1: handler = typecheck_discriminator_engine_1_1; break;
    case 2:
      switch (argnum) {
      case 0: handler = typecheck_discriminator_engine_1_2; break;
      case 1: handler = typecheck_discriminator_engine_2_2; break;
      }
      break;
    case 3:
      switch (argnum) {
      case 0: handler = typecheck_discriminator_engine_1_3; break;
      case 1: handler = typecheck_discriminator_engine_2_3; break;
      case 2: handler = typecheck_discriminator_engine_3_3; break;
      }
      break;
    case 4:
      switch (argnum) {
      case 0: handler = typecheck_discriminator_engine_1_4; break;
      case 1: handler = typecheck_discriminator_engine_2_4; break;
      case 2: handler = typecheck_discriminator_engine_3_4; break;
      case 3: handler = typecheck_discriminator_engine_4_4; break;
      }
      break;
    case 5:
      switch (argnum) {
      case 0: handler = typecheck_discriminator_engine_1_5; break;
      case 1: handler = typecheck_discriminator_engine_2_5; break;
      case 2: handler = typecheck_discriminator_engine_3_5; break;
      case 3: handler = typecheck_discriminator_engine_4_5; break;
      case 4: handler = typecheck_discriminator_engine_5_5; break;
      }
      break;
    case 6:
      switch (argnum) {
      case 0: handler = typecheck_discriminator_engine_1_6; break;
      case 1: handler = typecheck_discriminator_engine_2_6; break;
      case 2: handler = typecheck_discriminator_engine_3_6; break;
      case 3: handler = typecheck_discriminator_engine_4_6; break;
      case 4: handler = typecheck_discriminator_engine_5_6; break;
      case 5: handler = typecheck_discriminator_engine_6_6; break;
      }
      break;
    case 7:
      switch (argnum) {
      case 0: handler = typecheck_discriminator_engine_1_7; break;
      case 1: handler = typecheck_discriminator_engine_2_7; break;
      case 2: handler = typecheck_discriminator_engine_3_7; break;
      case 3: handler = typecheck_discriminator_engine_4_7; break;
      case 4: handler = typecheck_discriminator_engine_5_7; break;
      case 5: handler = typecheck_discriminator_engine_6_7; break;
      case 6: handler = typecheck_discriminator_engine_7_7; break;
      }
      break;
    default:
      handler = typecheck_discriminator_engine_n_n;
      break;
    }
  } else if (etype == ENGINE_monomorphic) {
    switch (impargs) {
    case 1: handler = monomorphic_discriminator_engine_1_1; break;
    case 2:
      switch (argnum) {
      case 0: handler = monomorphic_discriminator_engine_1_2; break;
      case 1: handler = monomorphic_discriminator_engine_2_2; break;
      }
      break;
    case 3:
      switch (argnum) {
      case 0: handler = monomorphic_discriminator_engine_1_3; break;
      case 1: handler = monomorphic_discriminator_engine_2_3; break;
      case 2: handler = monomorphic_discriminator_engine_3_3; break;
      }
      break;
    case 4:
      switch (argnum) {
      case 0: handler = monomorphic_discriminator_engine_1_4; break;
      case 1: handler = monomorphic_discriminator_engine_2_4; break;
      case 2: handler = monomorphic_discriminator_engine_3_4; break;
      case 3: handler = monomorphic_discriminator_engine_4_4; break;
      }
      break;
    case 5:
      switch (argnum) {
      case 0: handler = monomorphic_discriminator_engine_1_5; break;
      case 1: handler = monomorphic_discriminator_engine_2_5; break;
      case 2: handler = monomorphic_discriminator_engine_3_5; break;
      case 3: handler = monomorphic_discriminator_engine_4_5; break;
      case 4: handler = monomorphic_discriminator_engine_5_5; break;
      }
      break;
    case 6:
      switch (argnum) {
      case 0: handler = monomorphic_discriminator_engine_1_6; break;
      case 1: handler = monomorphic_discriminator_engine_2_6; break;
      case 2: handler = monomorphic_discriminator_engine_3_6; break;
      case 3: handler = monomorphic_discriminator_engine_4_6; break;
      case 4: handler = monomorphic_discriminator_engine_5_6; break;
      case 5: handler = monomorphic_discriminator_engine_6_6; break;
      }
      break;
    case 7:
      switch (argnum) {
      case 0: handler = monomorphic_discriminator_engine_1_7; break;
      case 1: handler = monomorphic_discriminator_engine_2_7; break;
      case 2: handler = monomorphic_discriminator_engine_3_7; break;
      case 3: handler = monomorphic_discriminator_engine_4_7; break;
      case 4: handler = monomorphic_discriminator_engine_5_7; break;
      case 5: handler = monomorphic_discriminator_engine_6_7; break;
      case 6: handler = monomorphic_discriminator_engine_7_7; break;
      }
      break;
    default:
      handler = monomorphic_discriminator_engine_n_n;
      break;
    }
  } else {
    switch (impargs) {
    case 1: handler = discriminate_engine_1_1; break;
    case 2:
      switch (argnum) {
      case 0: handler = discriminate_engine_1_2; break;
      case 1: handler = discriminate_engine_2_2; break;
      }
      break;
    case 3:
      switch (argnum) {
      case 0: handler = discriminate_engine_1_3; break;
      case 1: handler = discriminate_engine_2_3; break;
      case 2: handler = discriminate_engine_3_3; break;
      }
      break;
    case 4:
      switch (argnum) {
      case 0: handler = discriminate_engine_1_4; break;
      case 1: handler = discriminate_engine_2_4; break;
      case 2: handler = discriminate_engine_3_4; break;
      case 3: handler = discriminate_engine_4_4; break;
      }
      break;
    case 5:
      switch (argnum) {
      case 0: handler = discriminate_engine_1_5; break;
      case 1: handler = discriminate_engine_2_5; break;
      case 2: handler = discriminate_engine_3_5; break;
      case 3: handler = discriminate_engine_4_5; break;
      case 4: handler = discriminate_engine_5_5; break;
      }
      break;
    case 6:
      switch (argnum) {
      case 0: handler = discriminate_engine_1_6; break;
      case 1: handler = discriminate_engine_2_6; break;
      case 2: handler = discriminate_engine_3_6; break;
      case 3: handler = discriminate_engine_4_6; break;
      case 4: handler = discriminate_engine_5_6; break;
      case 5: handler = discriminate_engine_6_6; break;
      }
      break;
    case 7:
      switch (argnum) {
      case 0: handler = discriminate_engine_1_7; break;
      case 1: handler = discriminate_engine_2_7; break;
      case 2: handler = discriminate_engine_3_7; break;
      case 3: handler = discriminate_engine_4_7; break;
      case 4: handler = discriminate_engine_5_7; break;
      case 5: handler = discriminate_engine_6_7; break;
      case 6: handler = discriminate_engine_7_7; break;
      }
      break;
    default:
      handler = discriminate_engine_n_n;
      break;
    }
  }
  d->entry_point = (DLFN) handler;
  return(discriminator);
}


/* MULTIPLE VALUES */

DMINT _unused_arg = 0;
DMINT* P_unused_arg = &_unused_arg;

INLINE D MV_SPILL_into (D first_value, MV *dest) {
  TEB* teb = get_teb();
  int i, n = teb->return_values.count;
  teb->return_values.value[0] = first_value;
  for (i = 0; i < n; i++) {
    dest->value[i] = teb->return_values.value[i];
  }
  dest->count = n;
  return (D) dest;
}

D MV_SPILL (D first_value) {
  TEB* teb = get_teb();
  int n = teb->return_values.count;
  MV *dest = (MV *) primitive_allocate(1 + n);
  MV_SPILL_into(first_value, dest);
  return (D) dest;
}

D MV_UNSPILL (D spill_t) {
  TEB* teb = get_teb();
  MV *src = (MV *) spill_t;
  int i;
  int n = src->count;
  for (i = 0; i < n; i++) {
    teb->return_values.value[i] = src->value[i];
  }
  teb->return_values.count = n;
  return teb->return_values.count == 0 ? DFALSE : teb->return_values.value[0];
}

D MV_CHECK_TYPE_REST (D first_value, D rest_type, int n, ...) {
  TEB* teb = get_teb();
  int i, mv_n = teb->return_values.count;
  MV spill;
  va_list ap; va_start(ap, n);
  MV_SPILL_into(first_value, &spill);
  for (i = 0; i < n; i++) {
    D type = va_arg(ap, D);
    PERFORM_INLINE_TYPE_CHECK(spill.value[i], type);
  }
  for (; i < mv_n; i++) {
    PERFORM_INLINE_TYPE_CHECK(spill.value[i], rest_type);
  }
  MV_UNSPILL((D)&spill);
  return first_value;
}


D MV_GET_REST_AT (D first_value, DSINT first) {
  TEB* teb = get_teb();
  int  offset = first;
  int  n = teb->return_values.count - offset;
  teb->return_values.value[0] = first_value;
  return(make_vector_from_buffer(n < 0 ? 0 : n, &teb->return_values.value[offset]));
}

D MV_SET_REST_AT (D v, DSINT first) {
  TEB* teb = get_teb();
  int i, size = vector_size(v), offset = first;
  for (i=0; i<size; ++i) {
    teb->return_values.value[offset + i] = vector_ref(v, i);
  }
  teb->return_values.count = offset + size;
  return teb->return_values.count == 0 ? DFALSE : teb->return_values.value[0];
}

D MV2_ (D x, D y) {
  TEB* teb = get_teb();
  teb->return_values.value[0] = x;
  teb->return_values.value[1] = y;
  teb->return_values.count = 2;
  return x;
}

D MV3_ (D x, D y, D z) {
  TEB* teb = get_teb();
  teb->return_values.value[0] = x;
  teb->return_values.value[1] = y;
  teb->return_values.value[2] = z;
  teb->return_values.count = 3;
  return x;
}


/* NON-LOCAL EXITS */

#ifdef VERIFY_NLX
/**
 * Check the NLX stack of the given TEB for invariants
 *
 * This will crash dylan if:
 *  - the UWP chain can reach NULL
 *  - an UWP from another thread is present
 *
 */
void verify_nlx_stack (TEB* teb) {
  int bug_found = 0;
  Unwind_protect_frame* ptr;
  /* iterate over uwp stack and verify it */
  for (ptr = teb->uwp_frame; 1; ptr = ptr->previous_unwind_protect_frame) {
    /* top of uwp stack */
    if (ptr == &teb->top_uwp_frame) {
      break;
    }
    /* NULL current uwp */
    if (ptr == NULL) {
      bug_found = 1;
      break;
    }
    /* frame belongs to wrong TEB */
    if (ptr->verify_teb != teb) {
      bug_found = 1;
      break;
    }
    /* NULL previous uwp */
    if (ptr->previous_unwind_protect_frame == NULL) {
      bug_found = 1;
      break;
    }
  }
  /* act on results from verification run */
  if (bug_found) {
    fprintf(stderr, "BUG: invalid uwp stack:\n");
    /* run over stack again */
    for (ptr = teb->uwp_frame; 1; ptr = ptr->previous_unwind_protect_frame) {
      /* top of uwp stack */
      if (ptr == &teb->top_uwp_frame) {
        fprintf(stderr, "  reached top\n");
        break;
      }
      /* NULL current uwp */
      if (ptr == NULL) {
        fprintf(stderr, "  current uwp is NULL\n");
        break;
      }
      fprintf(stderr, "  uwp<%p> previous uwp<%p>\n",
             ptr, ptr->previous_unwind_protect_frame);
      /* frame belongs to wrong TEB */
      if (ptr->verify_teb != teb) {
        fprintf(stderr, "  frame belongs to other thread teb<%p>\n",
                ptr->verify_teb);
      }
      /* NULL previous uwp */
      if (ptr->previous_unwind_protect_frame == NULL) {
        fprintf(stderr, "  previous uwp is NULL\n");
        break;
      }
    }
    fflush(stderr);
    abort();
  }
}
/**
 * Check a single BEF for invariants
 *
 * This will crash dylan if:
 *  - the BEF is owned by another thread
 *  - the UWP associated with the BEF is unreachable
 */
void verify_nlx_bef(TEB* teb, Bind_exit_frame* bef) {
  Unwind_protect_frame* dest_uwp = bef->present_unwind_protect_frame;
  int dest_reachable = 0;
  Unwind_protect_frame* ptr;
  /* check if BEF is of this TEB */
  if (bef->verify_teb != teb) {
    fprintf(stderr, "BUG: trying to nlx to bef<%p> of other thread teb<%p>\n",
                    bef, bef->verify_teb);
    fflush(stderr);
    abort();
  }
  /* verify that the associated UWP is reachable */
  for (ptr = teb->uwp_frame; 1; ptr = ptr->previous_unwind_protect_frame) {
    /*  uwp found */
    if (ptr == dest_uwp) {
      dest_reachable = 1;
      break;
    }
    /* top of uwp stack */
    if (ptr == &teb->top_uwp_frame) {
      break;
    }
  }
  /* act on errors from verification run */
  if (!dest_reachable) {
    fprintf(stderr, "BUG: destination uwp<%p> of bef<%p> is unreachable\n",
           dest_uwp, bef);
    fflush(stderr);
    abort();
  }
}
#endif

static void nlx_step (Bind_exit_frame*) NORETURN_FUNCTION;

static void nlx_step (Bind_exit_frame* ultimate_destination) {
  TEB* teb = get_teb();

#ifdef VERIFY_NLX
  verify_nlx_stack(teb);
  verify_nlx_bef(teb, ultimate_destination);
#endif

  /* handled all unwind protect frames presently in force? */
  if (teb->uwp_frame == ultimate_destination->present_unwind_protect_frame) {
    trace_nlx("step to bef<%p> - reached ultimate uwp<%p>",
              ultimate_destination, teb->uwp_frame);

    /* invalidate current frame */
    teb->uwp_frame->ultimate_destination = NULL;

    /* jump to ultimate destination */
    nlx_longjmp(ultimate_destination->destination, 1);
  } else {
    Unwind_protect_frame* next_frame = teb->uwp_frame;

    trace_nlx("step to bef<%p> - to uwp<%p> previous uwp<%p> destination uwp<%p>",
              ultimate_destination,
              next_frame, next_frame->previous_unwind_protect_frame,
              ultimate_destination->present_unwind_protect_frame);

    /* pop current unwind protect frame */
    teb->uwp_frame = next_frame->previous_unwind_protect_frame;

    /* register ultimate destination of non-local exit in cupf */
    teb->uwp_frame->ultimate_destination = ultimate_destination;

    /* do cleanup step in next unwind protect frame */
    nlx_longjmp(next_frame->destination, 1);
  }
}

D FALL_THROUGH_UNWIND (D argument) {
  TEB* teb = get_teb();

#ifdef VERIFY_NLX
  verify_nlx_stack(teb);
#endif

  trace_nlx("fallthrough uwp<%p>", teb->uwp_frame);

  /* return values */
  teb->uwp_frame->return_values.count = teb->return_values.count;
  teb->uwp_frame->return_values.value[0] = argument;
  if (teb->return_values.count > 1) {
    COPY_WORDS
      (&teb->uwp_frame->return_values.value[1],
       &teb->return_values.value[1], teb->return_values.count - 1);
  }

  /* invalidate current frame */
  teb->uwp_frame->ultimate_destination = NULL;

  return((D)0);
}

D CONTINUE_UNWIND () {
  TEB* teb = get_teb();

#ifdef VERIFY_NLX
  verify_nlx_stack(teb);
#endif

  if (teb->uwp_frame->ultimate_destination) { /* nlx? */
    trace_nlx("continue unwind to bef<%p> from uwp<%p>",
              teb->uwp_frame->ultimate_destination,
              teb->uwp_frame);

    nlx_step(teb->uwp_frame->ultimate_destination);

    return(DFALSE);     /* Keeps some compilers happy */
  } else {
    trace_nlx("continue execution in uwp<%p> from uwp<%p>",
              teb->uwp_frame->previous_unwind_protect_frame,
              teb->uwp_frame);

    /* return values */
    int i;
    int n = teb->uwp_frame->return_values.count;
    teb->return_values.count = n;
    for (i = 0; i < n; i++) {
      teb->return_values.value[i]
        = teb->uwp_frame->return_values.value[i];
    }

    /* pop current unwind protect frame */
    teb->uwp_frame = teb->uwp_frame->previous_unwind_protect_frame;

    return n == 0 ? DFALSE : teb->return_values.value[0];
  }
}

D NLX (Bind_exit_frame* target, D argument) {
  TEB* teb = get_teb();
  trace_nlx("nlx to bef<%p> from uwp<%p>", target, teb->uwp_frame);
#ifdef VERIFY_NLX
  verify_nlx_stack(teb);
  verify_nlx_bef(teb, target);
#endif
  target->return_values.count = teb->return_values.count;
  target->return_values.value[0] = argument;
  if (teb->return_values.count > 1) {
    COPY_WORDS
      (&target->return_values.value[1],
       &teb->return_values.value[1], teb->return_values.count - 1);
  }
  nlx_step(target);
  return((D)0);                 /* Keeps some compilers happy -- Won't actually get here */
}

D SETUP_EXIT_FRAME (D frame) {
  TEB* teb = get_teb();
  Bind_exit_frame* be_frame = (Bind_exit_frame*)frame;
  trace_nlx("setup bef<%p> in uwp<%p>", be_frame, teb->uwp_frame);
#ifdef VERIFY_NLX
  verify_nlx_stack(teb);
  be_frame->verify_teb = teb;
#endif
  be_frame->present_unwind_protect_frame = teb->uwp_frame;
  return frame;
}

D SETUP_UNWIND_FRAME (D frame) {
  TEB* teb = get_teb();
  Unwind_protect_frame* uwp_frame = (Unwind_protect_frame*)frame;
  trace_nlx("setup uwp<%p> in uwp<%p>", uwp_frame, teb->uwp_frame);
#ifdef VERIFY_NLX
  verify_nlx_stack(teb);
  uwp_frame->verify_teb = teb;
#endif
  uwp_frame->previous_unwind_protect_frame = teb->uwp_frame;
  teb->uwp_frame = uwp_frame;
  uwp_frame->ultimate_destination = (Bind_exit_frame*)0;
  return frame;
}

D FRAME_DEST (D frame) {
  return((D)(((Bind_exit_frame*)frame)->destination));
}

D FRAME_RETVAL (D frame) {
  /* TODO: real multiple values */
  TEB* teb = get_teb();
  Bind_exit_frame *bef = ((Bind_exit_frame*) frame);
  /* Copy the multiple values into the result values MV */
  COPY_WORDS
      (&(teb->return_values.value[0]),
       &(bef->return_values.value[0]),
       bef->return_values.count);
  teb->return_values.count = bef->return_values.count;
  return((D)(bef->return_values.value[0]));
}

/* CLOSURES */

extern Wrapper KLmethodGVKdW;

D MAKE_CLOSURE (D schema, int closure_size) {
  CFN* fn = (CFN*)allocate(sizeof(CFN) + closure_size * sizeof(D));
  memcpy(fn, schema, sizeof(CFN));
  return((D)fn);
}

D MAKE_CLOSURE_SIG (D schema, D sig, int closure_size) {
  CFN* fn = (CFN*)allocate(sizeof(CFN) + closure_size * sizeof(D));
  memcpy(fn, schema, sizeof(CFN));
  fn->signature = sig;
  return((D)fn);
}

INLINE void init_environment (CFN* fn, int size, D* buf) {
  if (size > 0) {
    COPY_WORDS(&(fn->environment), buf, size);
  }
  fn->size = I(size);
}

void INIT_CLOSURE (D function, int closure_size, ...) {
  TEB* teb = get_teb();
  CFN* fn = function;
  BUFFER_VARARGS(closure_size, closure_size, teb->buffer);
  init_environment(fn, closure_size, teb->buffer);
}

D MAKE_CLOSURE_INITD (D schema, int closure_size, ...) {
  TEB* teb = get_teb();
  CFN* fn = (CFN*)allocate(sizeof(CFN) + closure_size * sizeof(D));
  memcpy(fn, schema, sizeof(CFN));
  BUFFER_VARARGS(closure_size, closure_size, teb->buffer);
  init_environment(fn, closure_size, teb->buffer);
  return((D)fn);
}

D MAKE_CLOSURE_INITD_SIG (D schema, D sig, int closure_size, ...) {
  TEB* teb = get_teb();
  CFN* fn = (CFN*)allocate(sizeof(CFN) + closure_size * sizeof(D));
  memcpy(fn, schema, sizeof(CFN));
  fn->signature = sig;
  BUFFER_VARARGS(closure_size, closure_size, teb->buffer);
  init_environment(fn, closure_size, teb->buffer);
  return((D)fn);
}

D MAKE_METHOD_SIG (D schema, D sig) {
  CFN* fn = (CFN*)allocate(sizeof(CFN));
  memcpy(fn, schema, sizeof(CFN));
  fn->signature = sig;
  return(fn);
}

D SET_METHOD_SIG (D method, D sig) {
  CFN* fn = (CFN*)method;
  fn->signature = sig;
  return((D)fn);
}

D MAKE_KEYWORD_CLOSURE (D schema, int closure_size) {
  KCFN* fn = (KCFN*)allocate(sizeof(KCFN) + closure_size * sizeof(D));
  memcpy(fn, schema, sizeof(KCFN));
  return((D)fn);
}

D MAKE_KEYWORD_CLOSURE_SIG (D schema, D sig, int closure_size) {
  KCFN* fn = (KCFN*)allocate(sizeof(KCFN) + closure_size * sizeof(D));
  memcpy(fn, schema, sizeof(KCFN));
  fn->signature = sig;
  return((D)fn);
}

INLINE void init_keyword_environment (KCFN* fn, int size, D* buf) {
  if (size > 0) {
    COPY_WORDS(&(fn->environment), buf, size);
  }
  fn->size = I(size);
}

void INIT_KEYWORD_CLOSURE (D function, int closure_size, ...) {
  TEB* teb = get_teb();
  KCFN* fn = function;
  BUFFER_VARARGS(closure_size, closure_size, teb->buffer);
  init_keyword_environment(fn, closure_size, teb->buffer);
}

D MAKE_KEYWORD_CLOSURE_INITD (D schema, int closure_size, ...) {
  TEB* teb = get_teb();
  KCFN* fn = (KCFN*)allocate(sizeof(KCFN) + closure_size * sizeof(D));
  memcpy(fn, schema, sizeof(KCFN));
  BUFFER_VARARGS(closure_size, closure_size, teb->buffer);
  init_keyword_environment(fn, closure_size, teb->buffer);
  return((D)fn);
}

D MAKE_KEYWORD_CLOSURE_INITD_SIG (D schema, D sig, int closure_size, ...) {
  TEB* teb = get_teb();
  KCFN* fn = (KCFN*)allocate(sizeof(KCFN) + closure_size * sizeof(D));
  memcpy(fn, schema, sizeof(KCFN));
  fn->signature = sig;
  BUFFER_VARARGS(closure_size, closure_size, teb->buffer);
  init_keyword_environment(fn, closure_size, teb->buffer);
  return((D)fn);
}

D MAKE_KEYWORD_METHOD_SIG (D schema, D sig) {
  KFN* fn = (KFN*)allocate(sizeof(KFN));
  memcpy(fn, schema, sizeof(KFN));
  fn->signature = sig;
  return(fn);
}

D SET_KEYWORD_METHOD_SIG (D method, D sig) {
  KCFN* fn = (KCFN*)method;
  fn->signature = sig;
  return((D)fn);
}

/* PRIMITIVES */

INLINE D primitive_apply_using_buffer (FN* fn, int n, D a[]) {
  TEB* teb = get_teb();
  int i, j;
  SOV* optionals = (SOV*)a[n - 1];
  int optionals_size = vector_size(optionals);
  int new_size = n + optionals_size - 1;
  for (i = 0; i < n - 1; i++) {
    teb->apply_buffer[i] = a[i];
  }
  for (i = n - 1, j = 0; j < optionals_size; i++, j++) {
    teb->apply_buffer[i] = vector_ref(optionals, j);
  }
  return(primitive_xep_apply(fn, new_size, teb->apply_buffer));
}

D primitive_apply (D fn, D args) {
  return(primitive_apply_using_buffer
           ((FN*)fn, vector_size((SOV*)args), vector_data((SOV*)args)));
}

D primitive_apply_spread (D fn, int n, ...) {
  TEB* teb = get_teb();
  BUFFER_VARARGS(n, n, teb->buffer);
  return(primitive_apply_using_buffer((FN*)fn, n, teb->buffer));
}

D primitive_mep_apply_spread (D fn, D nm, int n, ...) {
  TEB* teb = get_teb();
  BUFFER_VARARGS(n, n, teb->buffer);
  SOV* v = (SOV*)teb->buffer[n - 1];
  int v_size = vector_size(v);
  int new_size = n + v_size - 1;
  DEF_STACK_VECTOR_FROM_BUFFER_WITH_SIZE
    (new_arguments, new_size, teb->buffer, n - 1);
  COPY_WORDS
    (&(vector_data(new_arguments)[n - 1]), vector_data(v), v_size);
  return(primitive_mep_apply((FN*)fn, nm, (D *)new_arguments));
}

D primitive_engine_node_apply_spread (ENGINE* e, D parent, int n, ...) {
  TEB* teb = get_teb();
  BUFFER_VARARGS(n, n, teb->buffer);
  SOV* v = (SOV*)teb->buffer[n - 1];
  int v_size = vector_size(v);
  int new_size = n + v_size - 1;
  DEF_STACK_VECTOR_FROM_BUFFER_WITH_SIZE
    (new_arguments, new_size, teb->buffer, n - 1);
  COPY_WORDS
    (&(vector_data(new_arguments)[n - 1]), vector_data(v), v_size);
  return(primitive_engine_node_apply(e, parent, (D *)new_arguments));
}

/* temporary primitives for assignment */

D MAKE_D_CELL(D value) {
  D cell = primitive_allocate(1);
  *(D*)cell = value;
  return cell;
}

#define define_make_cell(type) \
  D MAKE_ ## type ## _CELL(type value) { \
    type* cell = (type*)allocate(sizeof(type)); \
    *cell = value; \
    return cell; \
  }

define_make_cell(DBCHR)
define_make_cell(DDBYTE)
define_make_cell(DSFLT)
define_make_cell(DDFLT)
define_make_cell(DWORD)
define_make_cell(DDWORD)

D primitive_vector (D n, ...) {
  TEB* teb = get_teb();
  BUFFER_VARARGS(R(n), n, teb->arguments);
  return(make_vector_from_buffer(R(n), teb->arguments));
}

D primitive_values (D v) {
  return(MV_SET_REST_AT(v, 0));
}

/* Hack oblist */

#define INITIAL_OBLIST_SIZE (64)

extern D LsymbolGVKd;
extern D Ksystem_allocate_simple_instanceVKeI (D class_, D Urest_, D fill_);

D primitive_make_symbol (D string)
{
  D symbol
    = Ksystem_allocate_simple_instanceVKeI
        (LsymbolGVKd, Pempty_vectorVKi, &KPunboundVKi);
  ((SYMBOL*)symbol)->name = string;
  return(symbol);
}

// XXX locking?
static int oblist_size = 0;
static int oblist_cursor = 0;
static D *oblist = NULL;

D primitive_preboot_symbols () {
  return(primitive_raw_as_vector((D)(long)oblist_cursor, oblist));
}

D primitive_string_as_symbol_using_symbol (D string, D symbol)
{
  int input_string_size = R(((BS*)string)->size);
  char *input_string_data = ((BS*)string)->data;

  int i;

  for (i = 0; i < oblist_cursor; ++i) {
    SYMBOL *oblist_symbol = (SYMBOL*)oblist[i];
    int oblist_string_size = R(((BS*)(oblist_symbol->name))->size);
    char *oblist_string_data = ((BS*)(oblist_symbol->name))->data;
    if (oblist_string_size == input_string_size
          && strncasecmp
               (oblist_string_data, input_string_data, (size_t)input_string_size)
                  == 0) {
      return((D)oblist_symbol);
    }
  }
  if (oblist_cursor >= oblist_size) {
    oblist_size += INITIAL_OBLIST_SIZE;
#if defined(GC_USE_BOEHM)
    oblist = (D*)GC_REALLOC(oblist, oblist_size * sizeof(D));
#elif defined(GC_USE_MALLOC)
    oblist = (D*)realloc(oblist, oblist_size * sizeof(D));
#endif
  }
  if (symbol == NULL) {
    symbol = primitive_make_symbol(string);
  }
  oblist[oblist_cursor++] = symbol;
  return symbol;
}

D primitive_string_as_symbol (D string)
{
  return(primitive_string_as_symbol_using_symbol(string, NULL));
}

D primitive_resolve_symbol (D symbol)
{
  return(primitive_string_as_symbol_using_symbol
           (((SYMBOL*)symbol)->name, symbol));
}

D primitive_slot_value(D object, DSINT position)
{
  D slot_value = primitive_initialized_slot_value(object, position);
  if (UNBOUND_P(slot_value)) {
    return(UNBOUND_INSTANCE_SLOT(object, I(position)));
  } else {
    return(slot_value);
  }
  return(slot_value);
}

D SLOT_VALUE(D object, DSINT position)
{
  D slot_value = primitive_initialized_slot_value(object, position);
  if (UNBOUND_P(slot_value)) {
    return(UNBOUND_INSTANCE_SLOT(object, I(position)));
  } else {
    return(slot_value);
  }
}

/* TERMINAL */

/* OPERATING SYSTEM */

D Tcommand_nameT;

D pseudo_primitive_command_name () {
  return(Tcommand_nameT);
}

D Tcommand_argumentsT;

D pseudo_primitive_command_arguments () {
  return(Tcommand_argumentsT);
}

void primitive_exit_application (DSINT code) {
  exit(code);
}


/* TOP LEVEL INITIALIZATION */

void GC_set_max_heap_size(unsigned long);

static void call_application_exit_functions(void) {
  extern D Kcall_application_exit_functionsVKeI();
  (void) Kcall_application_exit_functionsVKeI();
}

extern D IKJboole_ior_, IKJboole_xor_;

/**
 * Initialize the dylan run-time
 *
 * This must be called by every library using the C back-end
 * as part of its initialization before using anything else.
 *
 */
void _Init_Run_Time ()
{
  static int initp = 0;
  if (!initp) {
    initp = 1;

    // initialize the tracing system
    trace_init();
    trace_runtime("Initializing runtime");

    // register our dylan-level atexit mechanism
    atexit(call_application_exit_functions);

    // set up signal handlers
#ifdef OPEN_DYLAN_PLATFORM_UNIX
#ifdef SIGPIPE
    signal(SIGPIPE, SIG_IGN);
#endif
#endif

#ifdef GC_USE_BOEHM
    GC_INIT();
    GC_set_max_heap_size(MAX_HEAP_SIZE);
#endif

    initialize_threads_primitives();
  }
}
