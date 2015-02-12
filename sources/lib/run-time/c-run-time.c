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

extern dylan_object KPfalseVKi;
extern dylan_object KPtrueVKi;
extern dylan_object KPunboundVKi;

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
dylan_value primitive_runtime_module_handle()
{
  return(I(0));
}

/* SUPPORT */

void primitive_break() {
#if defined(OPEN_DYLAN_PLATFORM_WINDOWS)
  extern void __stdcall DebugBreak(void);
  DebugBreak();
#else
  fprintf(stderr, "Breaking into debugger.\n");
  fflush(stderr);
  raise(SIGTRAP);
#endif
}

/* MEMORY */

dylan_value allocate (unsigned long size) {
#if defined(GC_USE_BOEHM)
  return((dylan_value)GC_MALLOC((size_t)size));
#elif defined(GC_USE_MALLOC)
  return((dylan_value)malloc((size_t)size));
#endif
}

dylan_value primitive_allocate (DSINT size) {
  return((dylan_value)allocate(size * sizeof(dylan_value)));
}

dylan_value primitive_untraced_allocate (DSINT size) {
  return dylan__malloc__misc(size * sizeof(dylan_value));
}

dylan_value primitive_manual_allocate (dylan_value sizeObject) {
  size_t size = (size_t)R(sizeObject);
  void* p = mps__malloc(size);
  return primitive_wrap_machine_word((DMINT)p);
}

void primitive_manual_free (dylan_value object) {
  mps__free((void*)primitive_unwrap_c_pointer(object));
}

void primitive_fillX(dylan_value dst, int base_offset, int offset, int size, dylan_value value) {
  register int i;
  dylan_value* target = ((dylan_value*)dst) + base_offset + offset;
  for (i = 0; i < size; i++) {
    target[i] = value;
  }
}

void primitive_fill_bytesX
    (dylan_value dst, int base_offset, int offset, int size, DSINT value) {
  if (size > 0) {
    memset(((unsigned char*)((dylan_value*)dst + base_offset)) + offset, value, (size_t)size);
  }
}

DSINT primitive_repeated_slot_offset(dylan_value x) {
  dylan_value*       instance   = (dylan_value*)x;
  Wrapper* wrapper    = (Wrapper*)instance[0];
  DSINT    fixed_part = wrapper->fixed_part;
  DSINT    n_slots    = fixed_part >> 2;
  DSINT    offset     = 1 + n_slots + 1;
  return(offset);
}

dylan_value primitive_repeated_slot_as_raw(dylan_value x, DSINT offset) {
  return((dylan_value)((dylan_value*)x + offset));
}

void primitive_replace_bytesX
    (dylan_value dst, DSINT dst_base_offset, DSINT dst_offset,
     dylan_value src, DSINT src_base_offset, DSINT src_offset, DSINT size) {
  if (size > 0) {
    memcpy(&(((char*)(&(((dylan_value*)dst)[dst_base_offset])))[dst_offset]),
           &(((char*)(&(((dylan_value*)src)[src_base_offset])))[src_offset]),
           (size_t)size);
  }
}

#define COPY_WORDS(dst, src, size) memcpy((dst), (src), (size) * sizeof(dylan_value))

void primitive_replaceX
    (dylan_value dst, DSINT dst_base_offset, DSINT dst_offset,
     dylan_value src, DSINT src_base_offset, DSINT src_offset, DSINT size) {
  ignore(src_base_offset);
  if (size > 0) {
    COPY_WORDS(&(((dylan_value*)dst)[dst_base_offset + dst_offset]),
               &(((dylan_value*)src)[dst_base_offset + src_offset]),
               size);
  }
}


dylan_value primitive_compare_bytes(dylan_value base1, DSINT offset1,
                          dylan_value base2, DSINT offset2, DSINT size) {
  return (RAWASBOOL(memcmp(&((((dylan_byte_string*)base1)->data)[offset1]),
                           &((((dylan_byte_string*)base2)->data)[offset2]),
                           (size_t)size)));
}

dylan_value primitive_compare_words(dylan_value base1, DSINT offset1,
                          dylan_value base2, DSINT offset2, DSINT size) {
  return (RAWASBOOL(memcmp(&((((dylan_byte_string*)base1)->data)[offset1]),
                           &((((dylan_byte_string*)base2)->data)[offset2]),
                           size * sizeof(dylan_value))));
}

DSINT round_up_to_word(DSINT val) {
  size_t a = sizeof(void *);
  return (val + a - 1) & ~((unsigned long)a - 1);
}

dylan_value primitive_byte_allocate_filled_terminated
    (DSINT size, DSINT number_bytes, dylan_value class_wrapper, DSINT number_slots,
     dylan_value fill_value, DSINT repeated_size, DSINT repeated_size_offset)
{
  size = round_up_to_word(size * sizeof(dylan_value) + number_bytes);
  int byte_fill = R(fill_value);
  if (number_slots == 0) {
    return primitive_alloc_leaf_rbfz(size,
                                     class_wrapper,
                                     repeated_size,
                                     repeated_size_offset,
                                     byte_fill);
  } else {
    return primitive_alloc_leaf_s_rbfz(size,
                                       class_wrapper,
                                       number_slots,
                                       fill_value,
                                       repeated_size,
                                       repeated_size_offset,
                                       byte_fill);
  }
}

dylan_value primitive_byte_allocate_filled
    (DSINT size, dylan_value class_wrapper, DSINT number_slots,
     dylan_value fill_value, DSINT repeated_size, DSINT repeated_size_offset,
     DBYTE repeated_fill_value)
{
  size = round_up_to_word(size * sizeof(dylan_value) + repeated_size);
  if (repeated_size_offset == 0) {
    return primitive_alloc_s(size, class_wrapper, number_slots, fill_value);
  } else {
    return primitive_alloc_s_rbf(size, class_wrapper, number_slots, fill_value,
                                 repeated_size, repeated_size_offset,
                                 repeated_fill_value);
  }
}

dylan_value primitive_byte_allocate_leaf_filled_terminated
    (DSINT size, DSINT number_bytes, dylan_value class_wrapper, DSINT number_slots,
     dylan_value fill_value, DSINT repeated_size, DSINT repeated_size_offset)
{
  return primitive_byte_allocate_filled_terminated(size, number_bytes,
                                                   class_wrapper, number_slots,
                                                   fill_value, repeated_size,
                                                   repeated_size_offset);
}

dylan_value primitive_byte_allocate_leaf_filled
    (DSINT size, dylan_value class_wrapper, DSINT number_slots,
     dylan_value fill_value, DSINT repeated_size, DSINT repeated_size_offset,
     DBYTE repeated_fill_value)
{
  size = round_up_to_word(size * sizeof(dylan_value) + repeated_size);
  if (repeated_size_offset == 0) {
    return primitive_alloc_leaf_s(size, class_wrapper, number_slots,
                                  fill_value);
  } else {
    return primitive_alloc_leaf_s_rbf(size, class_wrapper, number_slots,
                                      fill_value, repeated_size,
                                      repeated_size_offset,
                                      repeated_fill_value);
  }
}

#define define_repeated_allocator(name, type, alloc_rf, alloc_s_rf) \
  dylan_value primitive_ ## name ## _allocate_filled \
      (DSINT size, dylan_value class_wrapper, DSINT number_slots, dylan_value fill_value, \
       DSINT repeated_size, DSINT repeated_size_offset, \
       type repeated_fill_value) \
  { \
    size = (size * sizeof(dylan_value)) + (repeated_size * sizeof(type)); \
    if (number_slots == 0) { \
      return alloc_rf(size, class_wrapper, repeated_size, \
                      repeated_size_offset, repeated_fill_value); \
    } else { \
      return alloc_s_rf(size, class_wrapper, number_slots, fill_value, \
                        repeated_size, repeated_size_offset, \
                        repeated_fill_value); \
    } \
  }

define_repeated_allocator(object, dylan_value, primitive_alloc_rf, primitive_alloc_s_rf)
define_repeated_allocator(double_byte, DDBYTE, primitive_alloc_leaf_rhf, primitive_alloc_s_rhf)
define_repeated_allocator(single_float, DSFLT, primitive_alloc_leaf_rsff, primitive_alloc_s_rsff)
define_repeated_allocator(double_float, DDFLT, primitive_alloc_leaf_rdff, primitive_alloc_s_rdff)

dylan_value primitive_word_allocate_filled
    (DSINT size, dylan_value class_wrapper, DSINT number_slots, dylan_value fill_value,
     DSINT repeated_size, DSINT repeated_size_offset, DWORD repeated_fill_value) {
  size = (size * sizeof(dylan_value)) + (repeated_size * sizeof(DWORD));
  if (number_slots == 0) {
    return primitive_alloc_leaf_rf(size, class_wrapper, repeated_size,
                                   repeated_size_offset, &repeated_fill_value);
  } else {
    return primitive_alloc_s_rf(size, class_wrapper, number_slots, fill_value,
                        repeated_size, repeated_size_offset,
                        &repeated_fill_value);
  }
}

dylan_value primitive_allocate_filled
    (DSINT size, dylan_value class_wrapper, DSINT number_slots, dylan_value fill_value,
     DSINT repeated_size, DSINT repeated_size_offset)
{
    size = size * sizeof(dylan_value);
    if (repeated_size_offset == 0) {
      if (number_slots == 0) {
        return primitive_alloc(size, class_wrapper);
      } else if (number_slots == 1) {
        return primitive_alloc_s1(size, class_wrapper, fill_value);
      } else if (number_slots == 2) {
        return primitive_alloc_s2(size, class_wrapper, fill_value, fill_value);
      } else {
        return primitive_alloc_s(size, class_wrapper, repeated_size, fill_value);
      }
      abort();
    } else {
      abort();
    }
}

dylan_value primitive_allocate_in_awl_pool
    (DSINT size, dylan_value class_wrapper, DSINT number_slots, dylan_value fill_value,
     DSINT repeated_size, DSINT repeated_size_offset, dylan_value assoc)
{
  size = size * sizeof(dylan_value);
  if (number_slots == 1) {
    return primitive_alloc_exact_awl_rf(size,
                                        class_wrapper,
                                        assoc,
                                        repeated_size,
                                        repeated_size_offset,
                                        fill_value);
  } else {
    return primitive_alloc_exact_awl_s_r(size,
                                         class_wrapper,
                                         assoc,
                                         number_slots,
                                         fill_value,
                                         repeated_size,
                                         repeated_size_offset);
  }
}

dylan_value primitive_allocate_weak_in_awl_pool
    (DSINT size, dylan_value class_wrapper, DSINT number_slots, dylan_value fill_value,
     DSINT repeated_size, DSINT repeated_size_offset, dylan_value assoc)
{
  size = size * sizeof(dylan_value);
  if (number_slots == 0) {
    return primitive_alloc_weak_awl_rf(size,
                                       class_wrapper,
                                       assoc,
                                       repeated_size,
                                       repeated_size_offset,
                                       fill_value);
  } else {
    return primitive_alloc_weak_awl_s_r(size,
                                        class_wrapper,
                                        assoc,
                                        number_slots,
                                        fill_value,
                                        repeated_size,
                                        repeated_size_offset);
  }
}

dylan_value primitive_allocate_wrapper
    (DSINT size, dylan_value class_wrapper, DSINT number_slots, dylan_value fill_value,
     DSINT repeated_size, DSINT repeated_size_offset)
{
  size = size * sizeof(dylan_value);
  return primitive_alloc_wrapper_s_r(size,
                                     class_wrapper,
                                     number_slots,
                                     fill_value,
                                     repeated_size,
                                     repeated_size_offset);
}

/* STACK ALLOCATION */


/* This one still zero-terminates. TODO: turn that off */
dylan_value initialize_byte_stack_allocate_filled
    (dylan_value ptr, dylan_value class_wrapper, DSINT number_slots,
     dylan_value fill_value, DSINT repeated_size, DSINT repeated_size_offset,
     DBYTE repeated_fill_value)
{
  dylan_value* object = ptr;
  object[0] = class_wrapper;
  primitive_fillX(object, 1, 0, number_slots, fill_value);
  primitive_fill_bytesX
    (object, repeated_size_offset + 1, 0, repeated_size,
     (unsigned char)R(repeated_fill_value));
  ((char*)(&object[repeated_size_offset + 1]))[repeated_size] = (char)0;
  if (repeated_size_offset > 0) {
    object[repeated_size_offset] = I(repeated_size);
  }
  return((dylan_value)object);
}

dylan_value initialize_object_stack_allocate_filled
      (dylan_value ptr, dylan_value class_wrapper, DSINT number_slots, dylan_value fill_value,
       DSINT repeated_size, DSINT repeated_size_offset,
       dylan_value repeated_fill_value)
{
  int i;
  dylan_value* object = ptr;
  object[0] = class_wrapper;
  primitive_fillX(object, 1, 0, number_slots, fill_value);
  for (i = 0; i < repeated_size; i++) {
    ((dylan_value*)(&object[repeated_size_offset + 1]))[i] = (dylan_value)repeated_fill_value;
  }
  if (repeated_size_offset > 0) {
    object[repeated_size_offset] = I(repeated_size);
  }
  return((dylan_value)object);
}


/* PINNING PRIMITIVES */

void primitive_unpin_object(dylan_value object)
{
  ignore(object);
}

/* C-FFI PRIMITIVES */

dylan_value primitive_wrap_c_pointer(dylan_value wrapper, DMINT x) {
  return(primitive_allocate_filled
           (2, wrapper, 1, (dylan_value)x, 0, 0));
}

/* VECTOR */

extern Wrapper KLsimple_object_vectorGVKdW;

#define VECTOR_HEADER_SIZE (2)

/* gts,98apr08 */
dylan_value  VECTOR_REF_OR_F(dylan_value vector, int offset) {
  if (offset >= vector_size(vector)) {
    return(DFALSE);
  } else {
    return(vector_ref((dylan_simple_object_vector*)vector, offset));
  }
}

INLINE dylan_value  vector_ref_setter (dylan_value new_value, dylan_simple_object_vector* vector, int offset) {
  return(vector_data(vector)[offset] = new_value);
}

extern dylan_simple_object_vector* Pempty_vectorVKi;

dylan_simple_object_vector* allocate_vector (int size) {
  if (size == 0) {
    return(Pempty_vectorVKi);
  } else {
    dylan_simple_object_vector* vector = (dylan_simple_object_vector*)primitive_allocate(size + VECTOR_HEADER_SIZE);
    return(vector);
  }
}

void *make_dylan_vector(size_t n) {
  if (n == 0) {
    return Pempty_vectorVKi;
  } else {
    size_t size;
    void* vector;

    size = (n + VECTOR_HEADER_SIZE) * sizeof(dylan_value);
    vector = primitive_alloc_rf(size,
                                &KLsimple_object_vectorGVKdW,
                                n, 1, &KPunboundVKi);
    return vector;
  }
}

dylan_value primitive_make_vector (int size) {
  return (dylan_value)make_dylan_vector((size_t)size);
}

dylan_simple_object_vector* initialize_vector_from_buffer_with_size
    (dylan_simple_object_vector* vector, int vector_size, dylan_value* buffer, int buffer_size)
{
  vector->class = &KLsimple_object_vectorGVKdW;
  vector_size_setter(vector_size, vector);
  COPY_WORDS(vector_data(vector), buffer, buffer_size);
  return(vector);
}

dylan_simple_object_vector* initialize_vector_from_buffer (dylan_simple_object_vector* vector, int size, dylan_value* buffer) {
  return(initialize_vector_from_buffer_with_size(vector, size, buffer, size));
}

dylan_simple_object_vector* make_vector_from_buffer (int size, dylan_value* buffer) {
  dylan_simple_object_vector* copy = allocate_vector(size);
  initialize_vector_from_buffer(copy, size, buffer);
  return(copy);
}

dylan_value primitive_copy_vector (dylan_value vector) {
  return((dylan_value)make_vector_from_buffer(vector_size((dylan_simple_object_vector*)vector), vector_data((dylan_simple_object_vector*)vector)));
}

dylan_value primitive_raw_as_vector (dylan_value size, dylan_value buffer) {
  return(make_vector_from_buffer((long)size, (dylan_value*)buffer));
}

#define DEF_STACK_VECTOR(_name, _size) \
  dylan_value _stk_##_name[_size + VECTOR_HEADER_SIZE]; \
  dylan_simple_object_vector* _name = (dylan_simple_object_vector*)(&_stk_##_name)

#define DEF_STACK_VECTOR_INITTED(_name, _size) \
  dylan_value _stk_##_name[_size + VECTOR_HEADER_SIZE]; \
  dylan_simple_object_vector* _name = (init_stack_vector((dylan_simple_object_vector*)(&_stk_##_name), (_size)))

INLINE dylan_simple_object_vector* init_stack_vector(dylan_simple_object_vector* vector, int size) {
  vector->class = (dylan_value)&KLsimple_object_vectorGVKdW;
  vector_size_setter(size, vector);
  return(vector);
  }

#define DEF_STACK_VECTOR_FROM_BUFFER_WITH_SIZE(_name, _vector_size, _buffer, _buffer_size) \
  DEF_STACK_VECTOR(_name, _vector_size); \
  initialize_vector_from_buffer_with_size (_name, _vector_size, _buffer, _buffer_size)

#define DEF_STACK_VECTOR_FROM_BUFFER(_name, _size, _buffer) \
  DEF_STACK_VECTOR_FROM_BUFFER_WITH_SIZE(_name, _size, _buffer, _size)

#define primitive_stack_allocate_vector(size) \
  ((dylan_simple_object_vector*)primitive_stack_allocate(size + VECTOR_HEADER_SIZE))

/* STRING */

extern Wrapper KLbyte_stringGVKdW;

dylan_value primitive_raw_as_string (DBSTR buffer) {
  size_t base_size = 2 * sizeof(dylan_value); // 1 for wrapper, 1 for size slot
  size_t len = strlen(buffer);
  size_t size = round_up_to_word(base_size + len + 1);
  dylan_value string = primitive_alloc_leaf_r(size, &KLbyte_stringGVKdW, len, 1);
  memcpy(((dylan_byte_string*)string)->data, buffer, len);
  return string;
}

/* SIGNATURES */

INLINE dylan_simple_object_vector* signature_required(dylan_signature* sig) {
  return(sig->required);
}

INLINE dylan_simple_object_vector* signature_values(dylan_signature* sig) {
  return(sig->values);
}

INLINE dylan_value signature_rest_value(dylan_signature* sig) {
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

INLINE int signature_number_required(dylan_signature* sig) {
  return(R(sig->properties) & NUMBER_REQUIRED_MASK);
}

INLINE int signature_number_values(dylan_signature* sig) {
  return((R(sig->properties) & NUMBER_VALUES_MASK)
           >> NUMBER_VALUES_OFFSET);
}

INLINE int signature_key_p(dylan_signature* sig) {
  return((R(sig->properties) & KEY_P_MASK) > 0);
}

INLINE int signature_all_keys_p(dylan_signature* sig) {
  return((R(sig->properties) & ALL_KEYS_P_MASK) > 0);
}

INLINE int signature_rest_p(dylan_signature* sig) {
  return((R(sig->properties) & REST_P_MASK) > 0);
}

INLINE int signature_optionals_p(dylan_signature* sig) {
  return((R(sig->properties) & OPTIONALS_P_MASK) > 0);
}


INLINE int signature_rest_value_p(dylan_signature* sig) {
  return((R(sig->properties) & REST_VALUE_P_MASK) > 0);
}

INLINE int signature_next_p(dylan_signature* sig) {
  return((R(sig->properties) & NEXT_P_MASK) > 0);
}

INLINE dylan_value signature_make_properties
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

INLINE DFN function_xep(dylan_simple_method* function) {
  return(function->xep);
}

DFN primitive_function_xep(dylan_value function) {
  return(function_xep((dylan_simple_method*)function));
}

INLINE DLFN function_mep(dylan_simple_method* function) {
  return(function->mep);
}

INLINE DLFN function_iep(dylan_simple_method* function) {
  return(function->mep);
}

INLINE DLFN keyword_function_iep(dylan_simple_method* function) {
  return(((dylan_keyword_method*)function)->iep);
}

INLINE dylan_value method_keyword_specifiers(dylan_simple_method* method) {
  return(((dylan_keyword_method*)method)->keyword_specifiers);
}

INLINE dylan_simple_object_vector* function_specializers(dylan_simple_method* function) {
  return(signature_required(function->signature));
}

INLINE int function_number_required(dylan_simple_method* function) {
  return(signature_number_required(function->signature));
}

INLINE int function_number_values(dylan_simple_method* function) {
  return(signature_number_values(function->signature));
}

INLINE int function_key_p(dylan_simple_method* function) {
  return(signature_key_p(function->signature));
}

INLINE int function_all_keys_p(dylan_simple_method* function) {
  return(signature_all_keys_p(function->signature));
}

INLINE int function_rest_p(dylan_simple_method* function) {
  return(signature_rest_p(function->signature));
}

INLINE int function_optionals_p(dylan_simple_method* function) {
  return(signature_optionals_p(function->signature));
}


INLINE int function_rest_value_p(dylan_simple_method* function) {
  return(signature_rest_value_p(function->signature));
}

INLINE int function_next_p(dylan_simple_method* function) {
  return(signature_next_p(function->signature));
}

/* VARARGS SUPPORT */

INLINE void transfer_varargs(va_list ap, int n, dylan_value* arguments) {
  int i;

  for (i=0; i<n; i++) {
    arguments[i] = va_arg(ap, dylan_value);
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

extern dylan_value LobjectGVKd;
extern dylan_value Ktype_check_errorVKiI(dylan_value argument, dylan_value specializer);

#define INSTANCEP(x, y) (primitive_instanceQ((x), (y)) != DFALSE)

#define FUNCTIONP(x) \
    (R((((Wrapper*)OBJECT_WRAPPER(x)))->subtype_mask) & 64)

FORCE_INLINE dylan_value PERFORM_INLINE_TYPE_CHECK(dylan_value value, dylan_value type) {
  if (unlikely(type != LobjectGVKd && !INSTANCEP(value, type))) {
    Ktype_check_errorVKiI(value, type);
  }
  return(value);
}

dylan_value primitive_type_check (dylan_value value, dylan_value type) {
  return PERFORM_INLINE_TYPE_CHECK(value, type);
}

extern dylan_value Kstack_overflow_errorVKiI();

INLINE void SIMPLE_CALL_CHECK(dylan_simple_method* function) {
  /* int stack_marker; */
  ignore(function);
  /*
  if (STACK_OK(bottom_of_stack,(unsigned long)(&stack_marker))) {
    stack_overflowedQ = 1;
    Kstack_overflow_errorVKiI();
  }*/
}

extern dylan_value Kargument_count_overflow_errorVKiI(dylan_value function, dylan_value argc);

INLINE void CALL_CHECK(dylan_simple_method* function, int argument_count) {
  SIMPLE_CALL_CHECK(function);
  if (unlikely(argument_count > MAX_ARGUMENTS)) {
    Kargument_count_overflow_errorVKiI(function, I(argument_count));
  }
}

FORCE_INLINE void TYPE_CHECK_ARG (dylan_value specializer, dylan_value argument) {
  PERFORM_INLINE_TYPE_CHECK(argument, specializer);
}

INLINE void TYPE_CHECK_ARGS(dylan_value function, int argument_count, dylan_value* arguments) {
  dylan_simple_object_vector* specs = function_specializers((dylan_simple_method*)function);
  if (specs) {
    dylan_value* specializers = vector_data(specs);
    int i;
    for (i = 0; i < argument_count; i++) {
      TYPE_CHECK_ARG(specializers[i], arguments[i]);
    }
  }
}

INLINE void TYPE_CHECK_ARGS_1(dylan_value fn, dylan_value a1) {
  dylan_simple_object_vector* specs = function_specializers((dylan_simple_method*)fn);
  if (specs) {
    dylan_value* specializers = vector_data(specs);
    TYPE_CHECK_ARG(specializers[0], a1);
  }
}

INLINE void TYPE_CHECK_ARGS_2(dylan_value fn, dylan_value a1, dylan_value a2) {
  dylan_simple_object_vector* specs = function_specializers((dylan_simple_method*)fn);
  if (specs) {
    dylan_value* specializers = vector_data(specs);
    TYPE_CHECK_ARG(specializers[0], a1);
    TYPE_CHECK_ARG(specializers[1], a2);
  }
}

INLINE void TYPE_CHECK_ARGS_3(dylan_value fn, dylan_value a1, dylan_value a2, dylan_value a3) {
  dylan_simple_object_vector* specs = function_specializers((dylan_simple_method*)fn);
  if (specs) {
    dylan_value* specializers = vector_data(specs);
    TYPE_CHECK_ARG(specializers[0], a1);
    TYPE_CHECK_ARG(specializers[1], a2);
    TYPE_CHECK_ARG(specializers[2], a3);
  }
}

INLINE void TYPE_CHECK_ARGS_4(dylan_value fn, dylan_value a1, dylan_value a2, dylan_value a3, dylan_value a4) {
  dylan_simple_object_vector* specs = function_specializers((dylan_simple_method*)fn);
  if (specs) {
    dylan_value* specializers = vector_data(specs);
    TYPE_CHECK_ARG(specializers[0], a1);
    TYPE_CHECK_ARG(specializers[1], a2);
    TYPE_CHECK_ARG(specializers[2], a3);
    TYPE_CHECK_ARG(specializers[3], a4);
  }
}

INLINE void TYPE_CHECK_ARGS_5(dylan_value fn, dylan_value a1, dylan_value a2, dylan_value a3, dylan_value a4, dylan_value a5) {
  dylan_simple_object_vector* specs = function_specializers((dylan_simple_method*)fn);
  if (specs) {
    dylan_value* specializers = vector_data(specs);
    TYPE_CHECK_ARG(specializers[0], a1);
    TYPE_CHECK_ARG(specializers[1], a2);
    TYPE_CHECK_ARG(specializers[2], a3);
    TYPE_CHECK_ARG(specializers[3], a4);
    TYPE_CHECK_ARG(specializers[4], a5);
  }
}

INLINE void TYPE_CHECK_ARGS_6 (dylan_value fn, dylan_value a1, dylan_value a2, dylan_value a3, dylan_value a4, dylan_value a5, dylan_value a6) {
  dylan_simple_object_vector* specs = function_specializers((dylan_simple_method*)fn);
  if (specs) {
    dylan_value* specializers = vector_data(specs);
    TYPE_CHECK_ARG(specializers[0], a1);
    TYPE_CHECK_ARG(specializers[1], a2);
    TYPE_CHECK_ARG(specializers[2], a3);
    TYPE_CHECK_ARG(specializers[3], a4);
    TYPE_CHECK_ARG(specializers[4], a5);
    TYPE_CHECK_ARG(specializers[5], a6);
  }
}

INLINE void TYPE_CHECK_ARGS_7
    (dylan_value fn, dylan_value a1, dylan_value a2, dylan_value a3, dylan_value a4, dylan_value a5, dylan_value a6, dylan_value a7) {
  dylan_simple_object_vector* specs = function_specializers((dylan_simple_method*)fn);
  if (specs) {
    dylan_value* specializers = vector_data(specs);
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
    (dylan_value fn, dylan_value a1, dylan_value a2, dylan_value a3, dylan_value a4, dylan_value a5, dylan_value a6, dylan_value a7, dylan_value a8) {
  dylan_simple_object_vector* specs = function_specializers((dylan_simple_method*)fn);
  if (specs) {
    dylan_value* specializers = vector_data(specs);
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
    (dylan_value fn, dylan_value a1, dylan_value a2, dylan_value a3, dylan_value a4, dylan_value a5, dylan_value a6, dylan_value a7, dylan_value a8, dylan_value a9) {
  dylan_simple_object_vector* specs = function_specializers((dylan_simple_method*)fn);
  if (specs) {
    dylan_value* specializers = vector_data(specs);
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

extern dylan_value Kargument_count_errorVKiI(dylan_value function, dylan_value argc);

INLINE void BASIC_REQUIRED_CALL_CHECK
    (dylan_simple_method* function, int number_required, int argument_count) {
  CALL_CHECK(function, argument_count);
  if (unlikely(argument_count != number_required)) {
    Kargument_count_errorVKiI(function, I(argument_count));
  }
}

INLINE void REQUIRED_CALL_CHECK
    (dylan_simple_method* function, int number_required, int argument_count, dylan_value* arguments) {
  BASIC_REQUIRED_CALL_CHECK(function, number_required, argument_count);
  TYPE_CHECK_ARGS(function, argument_count, arguments);
}

INLINE void BASIC_OPTIONAL_CALL_CHECK
    (dylan_simple_method* function, int number_required, int argument_count) {
  CALL_CHECK(function, argument_count);
  if (unlikely(argument_count < number_required)) {
    Kargument_count_errorVKiI(function, I(argument_count));
  }
}

INLINE void OPTIONAL_CALL_CHECK
    (dylan_simple_method* function, int number_required, int argument_count, dylan_value* arguments) {
  BASIC_OPTIONAL_CALL_CHECK(function, number_required, argument_count);
  TYPE_CHECK_ARGS(function, number_required, arguments);
}

extern dylan_value Kodd_keyword_arguments_errorVKiI(dylan_value function, dylan_value argc);

INLINE void KEYWORD_CALL_CHECK
    (dylan_simple_method* function, int number_required, int argument_count, dylan_value* arguments) {
  OPTIONAL_CALL_CHECK (function, number_required, argument_count, arguments);
  if (unlikely((argument_count - number_required) & 1)) {
    Kodd_keyword_arguments_errorVKiI(function, I(argument_count));
  }
}

/* CALLING CONVENTION */

dylan_value primitive_xep_apply (dylan_simple_method* fn, int n, dylan_value a[]) {
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

dylan_value primitive_xep_call (dylan_simple_method* fn, int n, ...) {
  TEB* teb = get_teb();
  int i;
  va_list ap; va_start(ap,n);
  for (i=0; i<n; i++) {
    dylan_value argument = va_arg(ap, dylan_value);
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

dylan_value call_dylan_function_returning_all_values (dylan_simple_method* fn, int n, ...) {
  TEB* teb = get_teb();
  int i;
  dylan_value first_value;
  va_list ap; va_start(ap,n);
  for (i=0; i<n; i++) {
    dylan_value argument = va_arg(ap, dylan_value);
    teb->arguments[i] = argument;
  }
  first_value = primitive_xep_apply(fn, n, teb->arguments);
  return(MV_GET_REST_AT(first_value, 0));
}

dylan_value primitive_mep_apply_with_optionals (dylan_simple_method* fn, dylan_value new_next_methods, dylan_value args) {
  TEB* teb = get_teb();
  DLFN mep = fn->mep;
  dylan_value*   v   = vector_data((dylan_simple_object_vector*)args);

  teb->next_methods = new_next_methods;
  teb->function = fn;
  teb->argument_count = vector_size((dylan_simple_object_vector*)args);

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

INLINE dylan_generic_function* parent_gf (dylan_value cache_header_or_gf) {
  while (!FUNCTIONP(cache_header_or_gf)) {
    cache_header_or_gf = ((CACHEHEADERENGINE*)cache_header_or_gf)->parent;
  }
  return((dylan_generic_function*)cache_header_or_gf);
}


extern dylan_value primitive_engine_node_apply_with_optionals (dylan_value engD, dylan_value parent, dylan_value args);

dylan_value primitive_engine_node_apply_with_optionals (dylan_value engD, dylan_value parent, dylan_value args) {
  TEB* teb = get_teb();
  ENGINE* eng = (ENGINE*)engD;
  DLFN ep = eng->entry_point;
  dylan_value*   a   = vector_data((dylan_simple_object_vector*)args);

  teb->next_methods = parent;
  teb->function = (dylan_value)eng;
  teb->argument_count = vector_size((dylan_simple_object_vector*)args);

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
    return(primitive_mep_apply_with_optionals((dylan_simple_method*)eng, parent, args));
  } else {
    return(ep(args));
  }
}

dylan_value inline_invoke_engine_node (ENGINE* eng, int argcount, ...) {
  TEB* teb = get_teb();
  int i;
  DEF_STACK_VECTOR_INITTED(argvec, teb->argument_count);
  va_list ap; va_start(ap,argcount);
  for (i=0; i<argcount; i++) {
    dylan_value argument = va_arg(ap, dylan_value);
    vector_ref_setter(argument, argvec, i);
  }
  if (FUNCTIONP(eng)) {
    return(primitive_mep_apply_with_optionals((dylan_simple_method*)eng, teb->next_methods, argvec));
  } else {
    return((eng->entry_point)((dylan_value)argvec));
  }
}


dylan_value primitive_engine_node_apply(ENGINE* eng, dylan_value parent, dylan_value a[]) {
  dylan_generic_function* gf = parent_gf(parent);
  dylan_signature* sig = gf->signature;
  int  number_required = signature_number_required(sig);
  int  argument_count = vector_size((dylan_simple_object_vector*)a);
  if (signature_optionals_p(sig)) {
    /* OPTIONAL_CALL_CHECK(gfn,number_required,argument_count); */
    dylan_value*   arguments = vector_data((dylan_simple_object_vector*)a);
    DEF_STACK_VECTOR_FROM_BUFFER_WITH_SIZE
      (new_arguments, number_required + 1, arguments, number_required);
    int  optionals_count = argument_count - number_required;
    DEF_STACK_VECTOR_FROM_BUFFER
      (rest_arguments, optionals_count, &arguments[number_required]);
    vector_ref_setter(rest_arguments, new_arguments, number_required);
    return(primitive_engine_node_apply_with_optionals((dylan_value)eng, parent, (dylan_value*)new_arguments));
  } else {
    /* REQUIRED_CALL_CHECK(gfn,number_required,argument_count); */
    return(primitive_engine_node_apply_with_optionals((dylan_value)eng, parent, a));
  }
}


dylan_value primitive_mep_apply (dylan_simple_method* fn, dylan_value next_methods, dylan_value a[]) {
  int  number_required = function_number_required(fn);
  int  argument_count = vector_size((dylan_simple_object_vector*)a);
  if (function_optionals_p(fn)) {
    /* OPTIONAL_CALL_CHECK(fn,number_required,argument_count); */
    dylan_value*   arguments = vector_data((dylan_simple_object_vector*)a);
    DEF_STACK_VECTOR_FROM_BUFFER_WITH_SIZE
      (new_arguments, number_required + 1, arguments, number_required);
    int  optionals_count = argument_count - number_required;
    DEF_STACK_VECTOR_FROM_BUFFER
      (rest_arguments, optionals_count, &arguments[number_required]);
    vector_ref_setter(rest_arguments, new_arguments, number_required);
    return(primitive_mep_apply_with_optionals
             (fn, next_methods, (dylan_value*)new_arguments));
  } else {
    /* REQUIRED_CALL_CHECK(fn,number_required,argument_count); */
    return(primitive_mep_apply_with_optionals(fn, next_methods, a));
  }
}

dylan_value iep_apply (DLFN iep, int n, dylan_value a[]) {
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

dylan_value primitive_iep_apply (dylan_simple_method* fn, int n, dylan_value a[]) {
  TEB* teb = get_teb();
  teb->function = fn; teb->next_methods = DFALSE;
  return(iep_apply(function_iep(fn), n, a));
}

/* required xep's */

dylan_value xep_0 (dylan_simple_method* fn, int n) {
  TEB* teb = get_teb();
  BASIC_REQUIRED_CALL_CHECK(fn, 0, n);
  teb->function = fn; teb->next_methods = DFALSE;
  return((function_iep(fn))());
}
dylan_value xep_1 (dylan_simple_method* fn, int n, dylan_value a1) {
  TEB* teb = get_teb();
  BASIC_REQUIRED_CALL_CHECK(fn, 1, n);
  TYPE_CHECK_ARGS_1(fn, a1);
  teb->function = fn; teb->next_methods = DFALSE;
  return((function_iep(fn))(a1));
}
dylan_value xep_2 (dylan_simple_method* fn, int n, dylan_value a1, dylan_value a2) {
  TEB* teb = get_teb();
  BASIC_REQUIRED_CALL_CHECK(fn, 2, n);
  TYPE_CHECK_ARGS_2(fn, a1, a2);
  teb->function = fn; teb->next_methods = DFALSE;
  return(function_iep(fn)(a1,a2));
}
dylan_value xep_3 (dylan_simple_method* fn, int n, dylan_value a1, dylan_value a2, dylan_value a3) {
  TEB* teb = get_teb();
  BASIC_REQUIRED_CALL_CHECK(fn, 3, n);
  TYPE_CHECK_ARGS_3(fn, a1, a2, a3);
  teb->function = fn; teb->next_methods = DFALSE;
  return(function_iep(fn)(a1,a2,a3));
}
dylan_value xep_4 (dylan_simple_method* fn, int n, dylan_value a1, dylan_value a2, dylan_value a3, dylan_value a4) {
  TEB* teb = get_teb();
  BASIC_REQUIRED_CALL_CHECK(fn, 4, n);
  TYPE_CHECK_ARGS_4(fn, a1, a2, a3, a4);
  teb->function = fn; teb->next_methods = DFALSE;
  return(function_iep(fn)(a1,a2,a3,a4));
}
dylan_value xep_5 (dylan_simple_method* fn, int n, dylan_value a1, dylan_value a2, dylan_value a3, dylan_value a4, dylan_value a5) {
  TEB* teb = get_teb();
  BASIC_REQUIRED_CALL_CHECK(fn, 5, n);
  TYPE_CHECK_ARGS_5(fn, a1, a2, a3, a4, a5);
  teb->function = fn; teb->next_methods = DFALSE;
  return(function_iep(fn)(a1,a2,a3,a4,a5));
}
dylan_value xep_6 (dylan_simple_method* fn, int n, dylan_value a1, dylan_value a2, dylan_value a3, dylan_value a4, dylan_value a5, dylan_value a6) {
  TEB* teb = get_teb();
  BASIC_REQUIRED_CALL_CHECK(fn, 6, n);
  TYPE_CHECK_ARGS_6(fn, a1, a2, a3, a4, a5, a6);
  teb->function = fn; teb->next_methods = DFALSE;
  return(function_iep(fn)(a1,a2,a3,a4,a5,a6));
}
dylan_value xep_7 (dylan_simple_method* fn, int n, dylan_value a1, dylan_value a2, dylan_value a3, dylan_value a4, dylan_value a5, dylan_value a6, dylan_value a7) {
  TEB* teb = get_teb();
  BASIC_REQUIRED_CALL_CHECK(fn, 7, n);
  TYPE_CHECK_ARGS_7(fn, a1, a2, a3, a4, a5, a6, a7);
  teb->function = fn; teb->next_methods = DFALSE;
  return(function_iep(fn)(a1,a2,a3,a4,a5,a6,a7));
}
dylan_value xep_8 (dylan_simple_method* fn, int n, dylan_value a1, dylan_value a2, dylan_value a3, dylan_value a4, dylan_value a5, dylan_value a6, dylan_value a7, dylan_value a8) {
  TEB* teb = get_teb();
  BASIC_REQUIRED_CALL_CHECK(fn, 8, n);
  TYPE_CHECK_ARGS_8(fn, a1, a2, a3, a4, a5, a6, a7, a8);
  teb->function = fn; teb->next_methods = DFALSE;
  return(function_iep(fn)(a1,a2,a3,a4,a5,a6,a7,a8));
}
dylan_value xep_9 (dylan_simple_method* fn, int n, dylan_value a1, dylan_value a2, dylan_value a3, dylan_value a4, dylan_value a5, dylan_value a6, dylan_value a7, dylan_value a8, dylan_value a9) {
  TEB* teb = get_teb();
  BASIC_REQUIRED_CALL_CHECK(fn, 9, n);
  TYPE_CHECK_ARGS_9(fn, a1, a2, a3, a4, a5, a6, a7, a8, a9);
  teb->function = fn; teb->next_methods = DFALSE;
  return(function_iep(fn)(a1,a2,a3,a4,a5,a6,a7,a8,a9));
}
dylan_value xep (dylan_simple_method* fn, int n, ...) {
  TEB* teb = get_teb();
  BUFFER_VARARGS(n, n, teb->arguments);
  REQUIRED_CALL_CHECK(fn, function_number_required(fn), n, teb->arguments);
  return(iep_apply(function_iep(fn), n, teb->arguments));
}

/* REST XEP'S */
/*   numbered by the number of required arguments == # parameters in IEP - 1 */

dylan_value rest_xep_0 (dylan_simple_method* fn, int n, ...) {
  TEB* teb = get_teb();
  BUFFER_VARARGS(n, n, teb->arguments);
  BASIC_OPTIONAL_CALL_CHECK(fn, 0, n);
  teb->function = fn; teb->next_methods = DFALSE;
  return((function_iep(fn))(make_vector_from_buffer(n, teb->arguments)));
}
dylan_value rest_xep_1 (dylan_simple_method* fn, int n, dylan_value a1, ...) {
  TEB* teb = get_teb();
  BUFFER_VARARGS(n - 1, a1, teb->arguments);
  BASIC_OPTIONAL_CALL_CHECK(fn, 1, n);
  TYPE_CHECK_ARGS_1(fn, a1);
  teb->function = fn; teb->next_methods = DFALSE;
  return((function_iep(fn))(a1, make_vector_from_buffer(n - 1, teb->arguments)));
}
dylan_value rest_xep_2 (dylan_simple_method* fn, int n, dylan_value a1, dylan_value a2, ...) {
  TEB* teb = get_teb();
  BUFFER_VARARGS(n - 2, a2, teb->arguments);
  BASIC_OPTIONAL_CALL_CHECK(fn, 2, n);
  TYPE_CHECK_ARGS_2(fn, a1, a2);
  teb->function = fn; teb->next_methods = DFALSE;
  return(function_iep(fn)(a1,a2,make_vector_from_buffer(n - 2, teb->arguments)));
}
dylan_value rest_xep_3 (dylan_simple_method* fn, int n, dylan_value a1, dylan_value a2, dylan_value a3, ...) {
  TEB* teb = get_teb();
  BUFFER_VARARGS(n - 3, a3, teb->arguments);
  BASIC_OPTIONAL_CALL_CHECK(fn, 3, n);
  TYPE_CHECK_ARGS_3(fn, a1, a2, a3);
  teb->function = fn; teb->next_methods = DFALSE;
  return(function_iep(fn)(a1,a2,a3,make_vector_from_buffer(n - 3, teb->arguments)));
}
dylan_value rest_xep_4 (dylan_simple_method* fn, int n, dylan_value a1, dylan_value a2, dylan_value a3, dylan_value a4, ...) {
  TEB* teb = get_teb();
  BUFFER_VARARGS(n - 4, a4, teb->arguments);
  BASIC_OPTIONAL_CALL_CHECK(fn, 4, n);
  TYPE_CHECK_ARGS_4(fn, a1, a2, a3, a4);
  teb->function = fn; teb->next_methods = DFALSE;
  return(function_iep(fn)(a1,a2,a3,a4,make_vector_from_buffer(n - 4, teb->arguments)));
}
dylan_value rest_xep_5 (dylan_simple_method* fn, int n, dylan_value a1, dylan_value a2, dylan_value a3, dylan_value a4, dylan_value a5, ...) {
  TEB* teb = get_teb();
  BUFFER_VARARGS(n - 5, a5, teb->arguments);
  BASIC_OPTIONAL_CALL_CHECK(fn, 5, n);
  TYPE_CHECK_ARGS_5(fn, a1, a2, a3, a4, a5);
  teb->function = fn; teb->next_methods = DFALSE;
  return(function_iep(fn)(a1,a2,a3,a4,a5,make_vector_from_buffer(n - 5, teb->arguments)));
}
dylan_value rest_xep_6 (dylan_simple_method* fn, int n, dylan_value a1, dylan_value a2, dylan_value a3, dylan_value a4, dylan_value a5, dylan_value a6, ...) {
  TEB* teb = get_teb();
  BUFFER_VARARGS(n - 6, a6, teb->arguments);
  BASIC_OPTIONAL_CALL_CHECK(fn, 6, n);
  TYPE_CHECK_ARGS_6(fn, a1, a2, a3, a4, a5, a6);
  teb->function = fn; teb->next_methods = DFALSE;
  return(function_iep(fn)(a1,a2,a3,a4,a5,a6,make_vector_from_buffer(n - 6, teb->arguments)));
}
dylan_value rest_xep_7 (dylan_simple_method* fn, int n, dylan_value a1, dylan_value a2, dylan_value a3, dylan_value a4, dylan_value a5, dylan_value a6, dylan_value a7, ...) {
  TEB* teb = get_teb();
  BUFFER_VARARGS(n - 7, a7, teb->arguments);
  BASIC_OPTIONAL_CALL_CHECK(fn, 7, n);
  TYPE_CHECK_ARGS_7(fn, a1, a2, a3, a4, a5, a6, a7);
  teb->function = fn; teb->next_methods = DFALSE;
  return(function_iep(fn)(a1,a2,a3,a4,a5,a6,a7,make_vector_from_buffer(n - 7, teb->arguments)));
}
dylan_value rest_xep_8 (dylan_simple_method* fn, int n, dylan_value a1, dylan_value a2, dylan_value a3, dylan_value a4, dylan_value a5, dylan_value a6, dylan_value a7, dylan_value a8, ...) {
  TEB* teb = get_teb();
  BUFFER_VARARGS(n - 8, a8, teb->arguments);
  BASIC_OPTIONAL_CALL_CHECK(fn, 8, n);
  TYPE_CHECK_ARGS_8(fn, a1, a2, a3, a4, a5, a6, a7, a8);
  teb->function = fn; teb->next_methods = DFALSE;
  return(function_iep(fn)(a1,a2,a3,a4,a5,a6,a7,a8,make_vector_from_buffer(n - 8, teb->arguments)));
}
dylan_value rest_xep_9 (dylan_simple_method* fn, int n, dylan_value a1, dylan_value a2, dylan_value a3, dylan_value a4, dylan_value a5, dylan_value a6, dylan_value a7, dylan_value a8, dylan_value a9, ...) {
  TEB* teb = get_teb();
  BUFFER_VARARGS(n - 9, a9, teb->arguments);
  BASIC_OPTIONAL_CALL_CHECK(fn, 9, n);
  TYPE_CHECK_ARGS_9(fn, a1, a2, a3, a4, a5, a6, a7, a8, a9);
  teb->function = fn; teb->next_methods = DFALSE;
  return(function_iep(fn)(a1,a2,a3,a4,a5,a6,a7,a8,a9,make_vector_from_buffer(n - 9, teb->arguments)));
}

dylan_value rest_xep (dylan_simple_method* fn, int n, ...) {
  TEB* teb = get_teb();
  int number_required = function_number_required(fn);
  int optionals_count = n - number_required;
  dylan_value*  optional_arguments = &teb->arguments[number_required];

  BUFFER_VARARGS(n, n, teb->arguments);
  OPTIONAL_CALL_CHECK(fn, number_required, n, teb->arguments);
  COPY_WORDS(teb->new_arguments,teb->arguments,number_required);
  DEF_STACK_VECTOR_FROM_BUFFER(rest_arguments, optionals_count, optional_arguments);
  teb->new_arguments[number_required] = rest_arguments;
  teb->function = fn; teb->next_methods = DFALSE;
  return(iep_apply(function_iep(fn), number_required + 1, teb->new_arguments));
}


/* ACCESSOR-METHOD XEP'S */


extern dylan_value KPslotacc_single_q_instance_getterVKiI(dylan_value accmeth, dylan_value inst);
extern dylan_value KPslotacc_single_q_instance_setterVKiI(dylan_value value, dylan_value accmeth, dylan_value inst);
extern dylan_value KPslotacc_single_q_class_getterVKiI(dylan_value accmeth, dylan_value inst);
extern dylan_value KPslotacc_single_q_class_setterVKiI(dylan_value value, dylan_value accmeth, dylan_value inst);
extern dylan_value KPslotacc_repeated_instance_getterVKiI(dylan_value accmeth, dylan_value inst, dylan_value idx);
extern dylan_value KPslotacc_repeated_instance_setterVKiI(dylan_value value, dylan_value accmeth, dylan_value inst, dylan_value idx);


dylan_value slotacc_single_q_instance_getter_xep (dylan_accessor_method* am, int n, dylan_value a1) {
  BASIC_REQUIRED_CALL_CHECK(((dylan_simple_method*)am), 1, n);
  return(KPslotacc_single_q_instance_getterVKiI(am, a1));
}

dylan_value slotacc_single_q_instance_setter_xep (dylan_accessor_method* am, int n, dylan_value a1, dylan_value a2) {
  BASIC_REQUIRED_CALL_CHECK(((dylan_simple_method*)am), 2, n);
  return(KPslotacc_single_q_instance_setterVKiI(am, a1, a2));
}

dylan_value slotacc_single_q_class_getter_xep (dylan_accessor_method* am, int n, dylan_value a1) {
  BASIC_REQUIRED_CALL_CHECK(((dylan_simple_method*)am), 1, n);
  return(KPslotacc_single_q_class_getterVKiI(am, a1));
}

dylan_value slotacc_single_q_class_setter_xep (dylan_accessor_method* am, int n, dylan_value a1, dylan_value a2) {
  BASIC_REQUIRED_CALL_CHECK(((dylan_simple_method*)am), 2, n);
  return(KPslotacc_single_q_class_setterVKiI(am, a1, a2));
}

dylan_value slotacc_repeated_instance_getter_xep (dylan_accessor_method* am, int n, dylan_value a1, dylan_value a2) {
  BASIC_REQUIRED_CALL_CHECK(((dylan_simple_method*)am), 2, n);
  return(KPslotacc_repeated_instance_getterVKiI(am, a1, a2));
}

dylan_value slotacc_repeated_instance_setter_xep (dylan_accessor_method* am, int n, dylan_value a1, dylan_value a2, dylan_value a3) {
  BASIC_REQUIRED_CALL_CHECK(((dylan_simple_method*)am), 3, n);
  return(KPslotacc_repeated_instance_setterVKiI(am, a1, a2, a3));
}

dylan_value primitive_set_accessor_method_xep (dylan_value accmeth, dylan_value what) {
  dylan_accessor_method* am = (dylan_accessor_method*)accmeth;
  switch (R(what)) {
  case 0: am->xep = (DFN)&slotacc_single_q_instance_getter_xep; break;
  case 1: am->xep = (DFN)&slotacc_single_q_instance_setter_xep; break;
  case 2: am->xep = (DFN)&slotacc_single_q_class_getter_xep; break;
  case 3: am->xep = (DFN)&slotacc_single_q_class_setter_xep; break;
  case 4: am->xep = (DFN)&slotacc_repeated_instance_getter_xep; break;
  case 5: am->xep = (DFN)&slotacc_repeated_instance_setter_xep; break;
  };
  return((dylan_value)am);
}


/* KEYWORD PROCESSING SUPPORT */

INLINE void default_arguments
    (int number_required, dylan_value* arguments,
     int number_keywords, dylan_value* keyword_specifiers,
     int keyword_arguments_offset, dylan_value* new_arguments) {
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
    (dylan_simple_method* function, int number_required,
     int number_keywords, dylan_value keyword_specifiers[],
     int number_optionals, dylan_value optional_arguments[], dylan_value new_arguments[]) {
  int i,j,k;
  int size_keyword_specifiers = number_keywords * 2;
  ignore(function);
  for (i = number_optionals - 1; i >= 0;) {
    dylan_value value   = optional_arguments[i--];
    dylan_value keyword = optional_arguments[i--];
    for (j = 0, k = number_required + 1; j < size_keyword_specifiers; k++, j += 2) {
      dylan_value lambda_keyword = keyword_specifiers[j];
      if (keyword == lambda_keyword) {
         new_arguments[k] = value;
         break;
      }
    }
  }
}

extern dylan_value unknown_keyword_argument_errorVKi(dylan_value function, dylan_value keyword);

INLINE void process_keyword_parameters_into_with_checking
    (dylan_simple_method* function, int number_required,
     int number_keywords, dylan_value keyword_specifiers[],
     int argument_count, dylan_value arguments[], dylan_value new_arguments[]) {
  int i,j,k;

  int allow_other_keys_p = function_all_keys_p(function);
  int size_keyword_specifiers = number_keywords * 2;
  for (i=argument_count-1; i>=number_required;) {
    dylan_value value   = arguments[i--];
    dylan_value keyword = arguments[i--];
    for (j=0,k=number_required;;k++,j+=2) {
      if (j == size_keyword_specifiers) {
        if (!allow_other_keys_p) {
          unknown_keyword_argument_errorVKi(function, keyword);
        } else {
          break;
        }
      } else {
        dylan_value lambda_keyword = keyword_specifiers[j];
        if (keyword == lambda_keyword) {
          new_arguments[k] = value;
          break;
        }
      }
    }
  }
}

INLINE int process_keyword_call_into
    (dylan_value* new_arguments, dylan_simple_method* function, int argument_count,
     int number_required, dylan_value* required_arguments,
     int optionals_count, dylan_value* optional_arguments, dylan_simple_object_vector* rest_arguments) {
  dylan_simple_object_vector* keyword_specifier_vector = method_keyword_specifiers(function);
  int  number_keywords = vector_size(keyword_specifier_vector) / 2;
  dylan_value*   keyword_specifiers = vector_data(keyword_specifier_vector);
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
    (dylan_value* new_arguments, dylan_simple_method* function,
     int argument_count, dylan_value* arguments, dylan_simple_object_vector* rest_arguments) {
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

INLINE dylan_value* process_keyword_call
    (dylan_simple_method* function, int argument_count, dylan_value* arguments, dylan_value rest_arguments) {
  TEB* teb = get_teb();
  process_keyword_call_and_restify_into
    (teb->new_arguments, function, argument_count, arguments, (dylan_simple_object_vector*)rest_arguments);
  return(teb->new_arguments);
}

INLINE dylan_value* process_keyword_call_and_n
    (dylan_simple_method* function, int argument_count,
     dylan_value* arguments, dylan_value rest_arguments, int *new_argument_count) {
  TEB* teb = get_teb();
  *new_argument_count =
    process_keyword_call_and_restify_into
      (teb->new_arguments, function, argument_count, arguments, (dylan_simple_object_vector*)rest_arguments);
  return(teb->new_arguments);
}


/* REST and KEY XEP's */
/*   numbered by the total number of parameters in the IEP */

dylan_value rest_key_xep_1 (dylan_simple_method* fn, int n, ...) {
  TEB* teb = get_teb();
  int number_required = function_number_required(fn);
  int optionals_count = n - number_required;
  BUFFER_VARARGS(n, n, teb->arguments);
  KEYWORD_CALL_CHECK(fn, number_required, n, teb->arguments);
  DEF_STACK_VECTOR_FROM_BUFFER
    (rest_arguments, optionals_count, &teb->arguments[number_required]);
  dylan_value* a = process_keyword_call(fn, n, teb->arguments, rest_arguments);
  teb->function = fn; teb->next_methods = DFALSE;
  return(keyword_function_iep(fn)(a[0]));
}

dylan_value rest_key_xep_2 (dylan_simple_method* fn, int n, ...) {
  TEB* teb = get_teb();
  int number_required = function_number_required(fn);
  int optionals_count = n - number_required;
  BUFFER_VARARGS(n, n, teb->arguments);
  KEYWORD_CALL_CHECK(fn, number_required, n, teb->arguments);
  DEF_STACK_VECTOR_FROM_BUFFER
    (rest_arguments, optionals_count, &teb->arguments[number_required]);
  dylan_value* a = process_keyword_call(fn, n, teb->arguments, rest_arguments);
  teb->function = fn; teb->next_methods = DFALSE;
  return(keyword_function_iep(fn)(a[0],a[1]));
}

dylan_value rest_key_xep_3 (dylan_simple_method* fn, int n, ...) {
  TEB* teb = get_teb();
  int number_required = function_number_required(fn);
  int optionals_count = n - number_required;
  BUFFER_VARARGS(n, n, teb->arguments);
  KEYWORD_CALL_CHECK(fn, number_required, n, teb->arguments);
  DEF_STACK_VECTOR_FROM_BUFFER
    (rest_arguments, optionals_count, &teb->arguments[number_required]);
  dylan_value* a = process_keyword_call(fn, n, teb->arguments, rest_arguments);
  teb->function = fn; teb->next_methods = DFALSE;
  return(keyword_function_iep(fn)(a[0],a[1],a[2]));
}

dylan_value rest_key_xep_4 (dylan_simple_method* fn, int n, ...) {
  TEB* teb = get_teb();
  int number_required = function_number_required(fn);
  int optionals_count = n - number_required;
  BUFFER_VARARGS(n, n, teb->arguments);
  KEYWORD_CALL_CHECK(fn, number_required, n, teb->arguments);
  DEF_STACK_VECTOR_FROM_BUFFER
    (rest_arguments, optionals_count, &teb->arguments[number_required]);
  dylan_value* a = process_keyword_call(fn, n, teb->arguments, rest_arguments);
  teb->function = fn; teb->next_methods = DFALSE;
  return(keyword_function_iep(fn)(a[0],a[1],a[2],a[3]));
}

dylan_value rest_key_xep_5 (dylan_simple_method* fn, int n, ...) {
  TEB* teb = get_teb();
  int number_required = function_number_required(fn);
  int optionals_count = n - number_required;
  BUFFER_VARARGS(n, n, teb->arguments);
  KEYWORD_CALL_CHECK(fn, number_required, n, teb->arguments);
  DEF_STACK_VECTOR_FROM_BUFFER
    (rest_arguments, optionals_count, &teb->arguments[number_required]);
  dylan_value* a = process_keyword_call(fn, n, teb->arguments, rest_arguments);
  teb->function = fn; teb->next_methods = DFALSE;
  return(keyword_function_iep(fn)(a[0],a[1],a[2],a[3],a[4]));
}

dylan_value rest_key_xep_6 (dylan_simple_method* fn, int n, ...) {
  TEB* teb = get_teb();
  int number_required = function_number_required(fn);
  int optionals_count = n - number_required;
  BUFFER_VARARGS(n, n, teb->arguments);
  KEYWORD_CALL_CHECK(fn, number_required, n, teb->arguments);
  DEF_STACK_VECTOR_FROM_BUFFER
    (rest_arguments, optionals_count, &teb->arguments[number_required]);
  dylan_value* a = process_keyword_call(fn, n, teb->arguments, rest_arguments);
  teb->function = fn; teb->next_methods = DFALSE;
  return(keyword_function_iep(fn)(a[0],a[1],a[2],a[3],a[4],a[5]));
}

dylan_value rest_key_xep_7 (dylan_simple_method* fn, int n, ...) {
  TEB* teb = get_teb();
  int number_required = function_number_required(fn);
  int optionals_count = n - number_required;
  BUFFER_VARARGS(n, n, teb->arguments);
  KEYWORD_CALL_CHECK(fn, number_required, n, teb->arguments);
  DEF_STACK_VECTOR_FROM_BUFFER
    (rest_arguments, optionals_count, &teb->arguments[number_required]);
  dylan_value* a = process_keyword_call(fn, n, teb->arguments, rest_arguments);
  teb->function = fn; teb->next_methods = DFALSE;
  return(keyword_function_iep(fn)(a[0],a[1],a[2],a[3],a[4],a[5],a[6]));
}

dylan_value rest_key_xep_8 (dylan_simple_method* fn, int n, ...) {
  TEB* teb = get_teb();
  int number_required = function_number_required(fn);
  int optionals_count = n - number_required;
  BUFFER_VARARGS(n, n, teb->arguments);
  KEYWORD_CALL_CHECK(fn, number_required, n, teb->arguments);
  DEF_STACK_VECTOR_FROM_BUFFER
    (rest_arguments, optionals_count, &teb->arguments[number_required]);
  dylan_value* a = process_keyword_call(fn, n, teb->arguments, rest_arguments);
  teb->function = fn; teb->next_methods = DFALSE;
  return(keyword_function_iep(fn)(a[0],a[1],a[2],a[3],a[4],a[5],a[6],a[7]));
}

dylan_value rest_key_xep_9 (dylan_simple_method* fn, int n, ...) {
  TEB* teb = get_teb();
  int number_required = function_number_required(fn);
  int optionals_count = n - number_required;
  BUFFER_VARARGS(n, n, teb->arguments);
  KEYWORD_CALL_CHECK(fn, number_required, n, teb->arguments);
  DEF_STACK_VECTOR_FROM_BUFFER
    (rest_arguments, optionals_count, &teb->arguments[number_required]);
  dylan_value* a = process_keyword_call(fn, n, teb->arguments, rest_arguments);
  teb->function = fn; teb->next_methods = DFALSE;
  return(keyword_function_iep(fn)(a[0],a[1],a[2],a[3],a[4],a[5],a[6],a[7],a[8]));
}

dylan_value rest_key_xep (dylan_simple_method* fn, int n, ...) {
  TEB* teb = get_teb();
  int number_required = function_number_required(fn);
  int optionals_count = n - number_required;
  int new_n;
  BUFFER_VARARGS(n, n, teb->arguments);
  KEYWORD_CALL_CHECK(fn, number_required, n, teb->arguments);
  DEF_STACK_VECTOR_FROM_BUFFER
    (rest_arguments, optionals_count, &teb->arguments[number_required]);
  dylan_value* a = process_keyword_call_and_n(fn, n, teb->arguments, rest_arguments, &new_n);
  teb->function = fn; teb->next_methods = DFALSE;
  return(iep_apply(keyword_function_iep(fn), new_n, a));
}


/* METHOD ENTRY POINTS -- MEPs */
/*   numbered by the total number of parameters in the IEP */

dylan_value key_mep_1 (dylan_value a1, ...) {
  TEB* teb = get_teb();
  int  number_required = function_number_required(teb->function);
  teb->a[0] = a1; BUFFER_VARARGS(teb->argument_count - 1, a1, &teb->a[1]);
  dylan_simple_object_vector* rest = teb->a[number_required];
  process_keyword_call_into
    (teb->iep_a, teb->function, teb->argument_count, number_required, teb->a,
     vector_size(rest), vector_data(rest), rest);
  return(keyword_function_iep(teb->function)(teb->iep_a[0]));
}

dylan_value key_mep_2 (dylan_value a1, ...) {
  TEB* teb = get_teb();
  int  number_required = function_number_required(teb->function);
  teb->a[0] = a1; BUFFER_VARARGS(teb->argument_count - 1, a1, &teb->a[1]);
  dylan_simple_object_vector* rest = teb->a[number_required];
  process_keyword_call_into
    (teb->iep_a, teb->function, teb->argument_count, number_required, teb->a,
     vector_size(rest), vector_data(rest), rest);
  return(keyword_function_iep(teb->function)(teb->iep_a[0],teb->iep_a[1]));
}

dylan_value key_mep_3 (dylan_value a1, ...) {
  TEB* teb = get_teb();
  int  number_required = function_number_required(teb->function);
  teb->a[0] = a1; BUFFER_VARARGS(teb->argument_count - 1, a1, &teb->a[1]);
  dylan_simple_object_vector* rest = teb->a[number_required];
  process_keyword_call_into
    (teb->iep_a, teb->function, teb->argument_count, number_required, teb->a,
     vector_size(rest), vector_data(rest), rest);
  return(keyword_function_iep(teb->function)(teb->iep_a[0],teb->iep_a[1],teb->iep_a[2]));
}

dylan_value key_mep_4 (dylan_value a1, ...) {
  TEB* teb = get_teb();
  int  number_required = function_number_required(teb->function);
  teb->a[0] = a1; BUFFER_VARARGS(teb->argument_count - 1, a1, &teb->a[1]);
  dylan_simple_object_vector* rest = teb->a[number_required];
  process_keyword_call_into
    (teb->iep_a, teb->function, teb->argument_count, number_required, teb->a,
     vector_size(rest), vector_data(rest), rest);
  return(keyword_function_iep(teb->function)(teb->iep_a[0],teb->iep_a[1],teb->iep_a[2],teb->iep_a[3]));
}

dylan_value key_mep_5 (dylan_value a1, ...) {
  TEB* teb = get_teb();
  int  number_required = function_number_required(teb->function);
  teb->a[0] = a1; BUFFER_VARARGS(teb->argument_count - 1, a1, &teb->a[1]);
  dylan_simple_object_vector* rest = teb->a[number_required];
  process_keyword_call_into
    (teb->iep_a, teb->function, teb->argument_count, number_required, teb->a,
     vector_size(rest), vector_data(rest), rest);
  return(keyword_function_iep(teb->function)(teb->iep_a[0],teb->iep_a[1],teb->iep_a[2],teb->iep_a[3],teb->iep_a[4]));
}

dylan_value key_mep_6 (dylan_value a1, ...) {
  TEB* teb = get_teb();
  int  number_required = function_number_required(teb->function);
  teb->a[0] = a1; BUFFER_VARARGS(teb->argument_count - 1, a1, &teb->a[1]);
  dylan_simple_object_vector* rest = teb->a[number_required];
  process_keyword_call_into
    (teb->iep_a, teb->function, teb->argument_count, number_required, teb->a,
     vector_size(rest), vector_data(rest), rest);
  return(keyword_function_iep(teb->function)(teb->iep_a[0],teb->iep_a[1],teb->iep_a[2],teb->iep_a[3],teb->iep_a[4],teb->iep_a[5]));
}

dylan_value key_mep_7 (dylan_value a1, ...) {
  TEB* teb = get_teb();
  int  number_required = function_number_required(teb->function);
  teb->a[0] = a1; BUFFER_VARARGS(teb->argument_count - 1, a1, &teb->a[1]);
  dylan_simple_object_vector* rest = teb->a[number_required];
  process_keyword_call_into
    (teb->iep_a, teb->function, teb->argument_count, number_required, teb->a,
     vector_size(rest), vector_data(rest), rest);
  return(keyword_function_iep(teb->function)(teb->iep_a[0],teb->iep_a[1],teb->iep_a[2],teb->iep_a[3],teb->iep_a[4],teb->iep_a[5],teb->iep_a[6]));
}

dylan_value key_mep_8 (dylan_value a1, ...) {
  TEB* teb = get_teb();
  int  number_required = function_number_required(teb->function);
  teb->a[0] = a1; BUFFER_VARARGS(teb->argument_count - 1, a1, &teb->a[1]);
  dylan_simple_object_vector* rest = teb->a[number_required];
  process_keyword_call_into
    (teb->iep_a, teb->function, teb->argument_count, number_required, teb->a,
     vector_size(rest), vector_data(rest), rest);
  return(keyword_function_iep(teb->function)(teb->iep_a[0],teb->iep_a[1],teb->iep_a[2],teb->iep_a[3],teb->iep_a[4],teb->iep_a[5],teb->iep_a[6],teb->iep_a[7]));
}

dylan_value key_mep_9 (dylan_value a1, ...) {
  TEB* teb = get_teb();
  int  number_required = function_number_required(teb->function);
  teb->a[0] = a1; BUFFER_VARARGS(teb->argument_count - 1, a1, &teb->a[1]);
  dylan_simple_object_vector* rest = teb->a[number_required];
  process_keyword_call_into
    (teb->iep_a, teb->function, teb->argument_count, number_required, teb->a,
     vector_size(rest), vector_data(rest), rest);
  return(keyword_function_iep(teb->function)(teb->iep_a[0],teb->iep_a[1],teb->iep_a[2],teb->iep_a[3],teb->iep_a[4],teb->iep_a[5],teb->iep_a[6],teb->iep_a[7],teb->iep_a[8]));
}

dylan_value key_mep (dylan_value a1, ...) {
  TEB* teb = get_teb();
  int  number_required = function_number_required(teb->function);
  teb->a[0] = a1; BUFFER_VARARGS(teb->argument_count - 1, a1, &teb->a[1]);
  dylan_simple_object_vector* rest = teb->a[number_required];
  int new_argument_count
    = process_keyword_call_into
       (teb->new_arguments, teb->function, teb->argument_count, number_required, teb->a,
        vector_size(rest), vector_data(rest), rest);
  return(iep_apply(keyword_function_iep(teb->function), new_argument_count, teb->new_arguments));
}

/* NEW GF SUPPORT */

dylan_value gf_iep_0 () {
  TEB* teb = get_teb();
  dylan_generic_function* gf = (dylan_generic_function*)teb->function;
  ENGINE* e = gf->engine;
  teb->next_methods = (dylan_value)gf;
  teb->function = (dylan_value)e;
  return((e->entry_point)());
}

dylan_value gf_iep_1 (dylan_value a1) {
  TEB* teb = get_teb();
  dylan_generic_function* gf = (dylan_generic_function*)teb->function;
  ENGINE* e = gf->engine;
  teb->next_methods = (dylan_value)gf;
  teb->function = (dylan_value)e;
  return((e->entry_point)(a1));
}

dylan_value gf_iep_2 (dylan_value a1, dylan_value a2) {
  TEB* teb = get_teb();
  dylan_generic_function* gf = (dylan_generic_function*)teb->function;
  ENGINE* e = gf->engine;
  teb->next_methods = (dylan_value)gf;
  teb->function = (dylan_value)e;
  return((e->entry_point)(a1, a2));
}

dylan_value gf_iep_3 (dylan_value a1, dylan_value a2, dylan_value a3) {
  TEB* teb = get_teb();
  dylan_generic_function* gf = (dylan_generic_function*)teb->function;
  ENGINE* e = gf->engine;
  teb->next_methods = (dylan_value)gf;
  teb->function = (dylan_value)e;
  return((e->entry_point)(a1, a2, a3));
}

dylan_value gf_iep_4 (dylan_value a1, dylan_value a2, dylan_value a3, dylan_value a4) {
  TEB* teb = get_teb();
  dylan_generic_function* gf = (dylan_generic_function*)teb->function;
  ENGINE* e = gf->engine;
  teb->next_methods = (dylan_value)gf;
  teb->function = (dylan_value)e;
  return((e->entry_point)(a1, a2, a3, a4));
}

dylan_value gf_iep_5 (dylan_value a1, dylan_value a2, dylan_value a3, dylan_value a4, dylan_value a5) {
  TEB* teb = get_teb();
  dylan_generic_function* gf = (dylan_generic_function*)teb->function;
  ENGINE* e = gf->engine;
  teb->next_methods = (dylan_value)gf;
  teb->function = (dylan_value)e;
  return((e->entry_point)(a1, a2, a3, a4, a5));
}

dylan_value gf_iep_6 (dylan_value a1, dylan_value a2, dylan_value a3, dylan_value a4, dylan_value a5, dylan_value a6) {
  TEB* teb = get_teb();
  dylan_generic_function* gf = (dylan_generic_function*)teb->function;
  ENGINE* e = gf->engine;
  teb->next_methods = (dylan_value)gf;
  teb->function = (dylan_value)e;
  return((e->entry_point)(a1, a2, a3, a4, a5, a6));
}

dylan_value gf_iep_7 (dylan_value a1, dylan_value a2, dylan_value a3, dylan_value a4, dylan_value a5, dylan_value a6, dylan_value a7) {
  TEB* teb = get_teb();
  dylan_generic_function* gf = (dylan_generic_function*)teb->function;
  ENGINE* e = gf->engine;
  teb->next_methods = (dylan_value)gf;
  teb->function = (dylan_value)e;
  return((e->entry_point)(a1, a2, a3, a4, a5, a6, a7));
}

dylan_value gf_iep (dylan_value new_arguments) {
  TEB* teb = get_teb();
  dylan_generic_function* gf = (dylan_generic_function*)teb->function;
  ENGINE* e = gf->engine;
  /* Unfortunately, due to the vectorization of arguments in this case, we have to check
     to see if the engine is actually a method in which case we have to invoke it with the
     args spread.  I'm passing the gf as the extra-arg to simulate the "normal" case where
     the method is blindly invoked. */
  if (FUNCTIONP(e)) {
    return(primitive_mep_apply_with_optionals((dylan_simple_method*)e, (dylan_value)gf, new_arguments));
  } else {
    teb->next_methods = (dylan_value)gf;
    teb->function = (dylan_value)e;
    return((e->entry_point)(new_arguments));
  }
}

/* GENERIC FUNCTION EXTERNAL ENTRY POINTS -- GF_XEP's */

/* REQ ONLY GF XEP's */
/*   numbered by the number of required arguments */

dylan_value gf_xep_0 (dylan_simple_method* fn, int n) {
  TEB* teb = get_teb();
  teb->function = fn; BASIC_REQUIRED_CALL_CHECK(fn, 0, n);
  return(gf_iep_0());
}
dylan_value gf_xep_1 (dylan_simple_method* fn, int n, dylan_value a1) {
  TEB* teb = get_teb();
  teb->function = fn; BASIC_REQUIRED_CALL_CHECK(fn, 1, n);
  return(gf_iep_1(a1));
}
dylan_value gf_xep_2 (dylan_simple_method* fn, int n, dylan_value a1, dylan_value a2) {
  TEB* teb = get_teb();
  teb->function = fn; BASIC_REQUIRED_CALL_CHECK(fn, 2, n);
  return(gf_iep_2(a1,a2));
}
dylan_value gf_xep_3 (dylan_simple_method* fn, int n, dylan_value a1, dylan_value a2, dylan_value a3) {
  TEB* teb = get_teb();
  teb->function = fn; BASIC_REQUIRED_CALL_CHECK(fn, 3, n);
  return(gf_iep_3(a1,a2,a3));
}
dylan_value gf_xep_4 (dylan_simple_method* fn, int n, dylan_value a1, dylan_value a2, dylan_value a3, dylan_value a4) {
  TEB* teb = get_teb();
  teb->function = fn; BASIC_REQUIRED_CALL_CHECK(fn, 4, n);
  return(gf_iep_4(a1,a2,a3,a4));
}
dylan_value gf_xep_5 (dylan_simple_method* fn, int n, dylan_value a1, dylan_value a2, dylan_value a3, dylan_value a4, dylan_value a5) {
  TEB* teb = get_teb();
  teb->function = fn; BASIC_REQUIRED_CALL_CHECK(fn, 5, n);
  return(gf_iep_5(a1,a2,a3,a4,a5));
}
dylan_value gf_xep_6 (dylan_simple_method* fn, int n, dylan_value a1, dylan_value a2, dylan_value a3, dylan_value a4, dylan_value a5, dylan_value a6) {
  TEB* teb = get_teb();
  teb->function = fn; BASIC_REQUIRED_CALL_CHECK(fn, 6, n);
  return(gf_iep_6(a1,a2,a3,a4,a5,a6));
}
dylan_value gf_xep_7 (dylan_simple_method* fn, int n, dylan_value a1, dylan_value a2, dylan_value a3, dylan_value a4, dylan_value a5, dylan_value a6, dylan_value a7) {
  TEB* teb = get_teb();
  teb->function = fn; BASIC_REQUIRED_CALL_CHECK(fn, 7, n);
  return(gf_iep_7(a1,a2,a3,a4,a5,a6,a7));
}
dylan_value gf_xep (dylan_simple_method* fn, int n, ...) {
  TEB* teb = get_teb();
  int number_required = function_number_required(fn);
  BUFFER_VARARGS(n, n, teb->arguments);
  teb->function = fn; BASIC_REQUIRED_CALL_CHECK(fn, number_required, n);
  DEF_STACK_VECTOR_FROM_BUFFER(new_arguments, number_required, teb->arguments);
  return(gf_iep(new_arguments));
}

/* OPTIONAL GF XEP's */
/*   numbered by the number of required arguments */

dylan_value gf_optional_xep_0 (dylan_simple_method* fn, int n, ...) {
  TEB* teb = get_teb();
  int optionals_count = n - 0; BASIC_OPTIONAL_CALL_CHECK(fn, 0, n);
  teb->function = fn; BUFFER_VARARGS(n, n, teb->a);
  DEF_STACK_VECTOR_FROM_BUFFER(rest_arguments, optionals_count, &teb->a[0]);
  teb->a[0] = rest_arguments;
  return(gf_iep_1(teb->a[0]));
}

dylan_value gf_optional_xep_1 (dylan_simple_method* fn, int n, ...) {
  TEB* teb = get_teb();
  int optionals_count = n - 1; BASIC_OPTIONAL_CALL_CHECK(fn, 1, n);
  teb->function = fn; BUFFER_VARARGS(n, n, teb->a);
  DEF_STACK_VECTOR_FROM_BUFFER(rest_arguments, optionals_count, &teb->a[1]);
  teb->a[1] = rest_arguments;
  return(gf_iep_2(teb->a[0], teb->a[1]));
}

dylan_value gf_optional_xep_2 (dylan_simple_method* fn, int n, ...) {
  TEB* teb = get_teb();
  int optionals_count = n - 2; BASIC_OPTIONAL_CALL_CHECK(fn, 2, n);
  teb->function = fn; BUFFER_VARARGS(n, n, teb->a);
  DEF_STACK_VECTOR_FROM_BUFFER(rest_arguments, optionals_count, &teb->a[2]);
  teb->a[2] = rest_arguments;
  return(gf_iep_3(teb->a[0],teb->a[1],teb->a[2]));
}

dylan_value gf_optional_xep_3 (dylan_simple_method* fn, int n, ...) {
  TEB* teb = get_teb();
  int optionals_count = n - 3; BASIC_OPTIONAL_CALL_CHECK(fn, 3, n);
  teb->function = fn; BUFFER_VARARGS(n, n, teb->a);
  DEF_STACK_VECTOR_FROM_BUFFER(rest_arguments, optionals_count, &teb->a[3]);
  teb->a[3] = rest_arguments;
  return(gf_iep_4(teb->a[0],teb->a[1],teb->a[2],teb->a[3]));
}

dylan_value gf_optional_xep_4 (dylan_simple_method* fn, int n, ...) {
  TEB* teb = get_teb();
  int optionals_count = n - 4; BASIC_OPTIONAL_CALL_CHECK(fn, 4, n);
  teb->function = fn; BUFFER_VARARGS(n, n, teb->a);
  DEF_STACK_VECTOR_FROM_BUFFER(rest_arguments, optionals_count, &teb->a[4]);
  teb->a[4] = rest_arguments;
  return(gf_iep_5(teb->a[0],teb->a[1],teb->a[2],teb->a[3],teb->a[4]));
}

dylan_value gf_optional_xep_5 (dylan_simple_method* fn, int n, ...) {
  TEB* teb = get_teb();
  int optionals_count = n - 5; BASIC_OPTIONAL_CALL_CHECK(fn, 5, n);
  teb->function = fn; BUFFER_VARARGS(n, n, teb->a);
  DEF_STACK_VECTOR_FROM_BUFFER(rest_arguments, optionals_count, &teb->a[5]);
  teb->a[5] = rest_arguments;
  return(gf_iep_6(teb->a[0],teb->a[1],teb->a[2],teb->a[3],teb->a[4],teb->a[5]));
}

dylan_value gf_optional_xep_6 (dylan_simple_method* fn, int n, ...) {
  TEB* teb = get_teb();
  int optionals_count = n - 6; BASIC_OPTIONAL_CALL_CHECK(fn, 6, n);
  teb->function = fn; BUFFER_VARARGS(n, n, teb->a);
  DEF_STACK_VECTOR_FROM_BUFFER(rest_arguments, optionals_count, &teb->a[6]);
  teb->a[6] = rest_arguments;
  return(gf_iep_7(teb->a[0],teb->a[1],teb->a[2],teb->a[3],teb->a[4],teb->a[5],teb->a[6]));
}

dylan_value gf_optional_xep (dylan_simple_method* fn, int n, ...) {
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

dylan_value primitive_set_generic_function_entrypoints(dylan_value fn) {
  dylan_value the_xep;
  dylan_simple_method* function = (dylan_simple_method*)fn;
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


dylan_value general_engine_node_1_engine (dylan_value a1) {
  TEB* teb = get_teb();
  ENGINE* e = (ENGINE*)teb->function;
  dylan_value parent = teb->next_methods;
  return((e->callback)(a1, e, parent));
}

dylan_value general_engine_node_2_engine (dylan_value a1, dylan_value a2) {
  TEB* teb = get_teb();
  ENGINE* e = (ENGINE*)teb->function;
  dylan_value parent = teb->next_methods;
  return((e->callback)(a1, a2, e, parent));
}


dylan_value general_engine_node_3_engine (dylan_value a1, dylan_value a2, dylan_value a3) {
  TEB* teb = get_teb();
  ENGINE* e = (ENGINE*)teb->function;
  dylan_value parent = teb->next_methods;
  DLFN cb = e->callback;
  return(cb(a1, a2, a3, e, parent));
}

dylan_value general_engine_node_n_engine (dylan_value a1, ...) {
  TEB* teb = get_teb();
  ENGINE* e = (ENGINE*)teb->function;
  dylan_value parent = teb->next_methods;
  dylan_generic_function* gf = parent_gf(parent);
  DLFN cb = e->callback;
  dylan_signature* sig = (dylan_signature*)gf->signature;
  int nreq = signature_number_required(sig);
  int impargs = nreq + signature_optionals_p(sig);
  if (impargs > 7) {
    /* The calling sequence passes just a vector of MEP args. */
    return(cb(a1, e, parent));
  } else {
    /* The args are spread, last one may be a rest vector. */
    va_list ap;
    DEF_STACK_VECTOR_INITTED(svec, impargs);
    dylan_value* svdata = vector_data(svec);
    if (impargs > 0) {
      int i;
      svdata[0] = a1;
      va_start(ap, a1);
      for (i=1; i<impargs; i++) {
        dylan_value argument = va_arg(ap, dylan_value);
        svdata[i] = argument;
      }
    }
    return(cb(svec, e, parent));
  }
}
dylan_value general_engine_node_spread_engine (dylan_value a1, ...) {
  TEB* teb = get_teb();
  ENGINE* e = (ENGINE*)teb->function;
  dylan_value parent = teb->next_methods;
  dylan_generic_function* gf = parent_gf(parent);
  DLFN cb = e->callback;
  dylan_signature* sig = (dylan_signature*)gf->signature;
  int nreq = signature_number_required(sig);
  int impargs = nreq + signature_optionals_p(sig);
  if (nreq != impargs) {
    /* If there's optionals, we will need to make a new vector and spread them out.  */
    if (impargs > 7) {
      /* All the args are in a stack vector, the last of which is the optionals... */
      dylan_simple_object_vector* mepargvec = (dylan_simple_object_vector*)a1;
      dylan_value* mepargdata = vector_data(mepargvec);
      dylan_simple_object_vector* optargvec = (dylan_simple_object_vector*)mepargdata[nreq];
      dylan_value* optargdata = vector_data(optargvec);
      int nopts = vector_size(optargvec);
      DEF_STACK_VECTOR_INITTED(svec, nreq + nopts);
      dylan_value* svdata = vector_data(svec);
      int i;
      for (i=0; i<nreq; i++) svdata[i] = mepargdata[i];
      for (i=0; i<nopts; i++) svdata[i+nreq] = optargdata[i];
      return(cb(svec, e, parent));
    } else {
      /* The arguments are spread, the last one is the optionals vector. */
      teb->arguments[0] = a1;
      BUFFER_VARARGS(nreq, a1, &teb->arguments[1]);
      dylan_simple_object_vector* optargvec = (dylan_simple_object_vector*)teb->arguments[nreq];
      dylan_value* optargdata = vector_data(optargvec);
      int nopts = vector_size(optargvec);
      DEF_STACK_VECTOR_INITTED(svec, nreq + nopts);
      dylan_value* svdata = vector_data(svec);
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
    dylan_value* svdata = vector_data(svec);
    if (nreq > 0) {
      int i;
      svdata[0] = a1;
      va_start(ap, a1);
      for (i=1; i<nreq; i++) {
        dylan_value argument = va_arg(ap, dylan_value);
        svdata[i] = argument;
      }
    }
    return(cb(svec, e, parent));
  }
}

extern dylan_value Krepeated_slot_getter_index_out_of_range_trapVKeI(dylan_value obj, dylan_value idx);
extern dylan_value Krepeated_slot_setter_index_out_of_range_trapVKeI(dylan_value val, dylan_value obj, dylan_value idx);
#define REPEATED_GETTER_OOR Krepeated_slot_getter_index_out_of_range_trapVKeI
#define REPEATED_SETTER_OOR Krepeated_slot_setter_index_out_of_range_trapVKeI

extern dylan_value Kunbound_instance_slotVKeI(dylan_value obj, dylan_value offset);
#define UNBOUND_INSTANCE_SLOT Kunbound_instance_slotVKeI
extern dylan_value Kunbound_repeated_slotVKeI(dylan_value obj, dylan_value offset);
#define UNBOUND_REPEATED_SLOT Kunbound_repeated_slotVKeI


dylan_value boxed_instance_slot_getter_engine (dylan_value object) {
  TEB* teb = get_teb();
  ENGINE* e = (ENGINE*)teb->function;
  int idx =  (int)(((DADDR)(e->properties)) >> SLOTENGINE_V_INDEX);
  dylan_value slot_value = primitive_initialized_slot_value(object, idx);
  if (UNBOUND_P(slot_value)) {
    return(UNBOUND_INSTANCE_SLOT(object, I(idx)));
  } else {
    return(slot_value);
  }
}

dylan_value boxed_instance_slot_setter_engine (dylan_value newval, dylan_value object) {
  TEB* teb = get_teb();
  ENGINE* e = (ENGINE*)teb->function;
  int baseidx = (int)(((DADDR)(e->properties)) >> SLOTENGINE_V_INDEX);
  primitive_slot_value_setter(newval, object, baseidx);
  return(newval);
}


dylan_value boxed_repeated_instance_slot_getter_engine (dylan_value object, dylan_value idx) {
  TEB* teb = get_teb();
  ENGINE* e = (ENGINE*)teb->function;
  int baseidx = (int)(((DADDR)(e->properties)) >> SLOTENGINE_V_INDEX);
  int size = primitive_repeated_instance_size(object, baseidx);
  int ridx = R(idx);
  if (ridx >= 0 && ridx < size) {
    dylan_value slot_value = primitive_repeated_slot_value(object, baseidx, ridx);
    if (UNBOUND_P(slot_value)) {
      return(UNBOUND_REPEATED_SLOT(object, idx));
    } else {
      return(slot_value);
    }
  } else {
    return(REPEATED_GETTER_OOR(object, idx));
  }
}

dylan_value boxed_repeated_instance_slot_setter_engine (dylan_value newval, dylan_value object, dylan_value idx) {
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

dylan_value raw_byte_repeated_instance_slot_getter_engine (dylan_value object, dylan_value idx) {
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
dylan_value raw_byte_repeated_instance_slot_setter_engine (dylan_value newval, dylan_value object, dylan_value idx) {
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
#define PARAMTEMPLATE1 dylan_value a1
#define PARAMTEMPLATE2 dylan_value a1, dylan_value a2
#define PARAMTEMPLATE3 dylan_value a1, dylan_value a2, dylan_value a3
#define PARAMTEMPLATE4 dylan_value a1, dylan_value a2, dylan_value a3, dylan_value a4
#define PARAMTEMPLATE5 dylan_value a1, dylan_value a2, dylan_value a3, dylan_value a4, dylan_value a5
#define PARAMTEMPLATE6 dylan_value a1, dylan_value a2, dylan_value a3, dylan_value a4, dylan_value a5, dylan_value a6
#define PARAMTEMPLATE7 dylan_value a1, dylan_value a2, dylan_value a3, dylan_value a4, dylan_value a5, dylan_value a6, dylan_value a7
#define ARGTEMPLATE0
#define ARGTEMPLATE1 a1
#define ARGTEMPLATE2 a1, a2
#define ARGTEMPLATE3 a1, a2, a3
#define ARGTEMPLATE4 a1, a2, a3, a4
#define ARGTEMPLATE5 a1, a2, a3, a4, a5
#define ARGTEMPLATE6 a1, a2, a3, a4, a5, a6
#define ARGTEMPLATE7 a1, a2, a3, a4, a5, a6, a7
#define TYPETEMPLATE0
#define TYPETEMPLATE1 dylan_value
#define TYPETEMPLATE2 dylan_value, dylan_value
#define TYPETEMPLATE3 dylan_value, dylan_value, dylan_value
#define TYPETEMPLATE4 dylan_value, dylan_value, dylan_value, dylan_value
#define TYPETEMPLATE5 dylan_value, dylan_value, dylan_value, dylan_value, dylan_value
#define TYPETEMPLATE6 dylan_value, dylan_value, dylan_value, dylan_value, dylan_value, dylan_value
#define TYPETEMPLATE7 dylan_value, dylan_value, dylan_value, dylan_value, dylan_value, dylan_value, dylan_value
#define PARAMTEMPLATEPREFIX0
#define PARAMTEMPLATEPREFIX1 dylan_value a1,
#define PARAMTEMPLATEPREFIX2 dylan_value a1, dylan_value a2,
#define PARAMTEMPLATEPREFIX3 dylan_value a1, dylan_value a2, dylan_value a3,
#define PARAMTEMPLATEPREFIX4 dylan_value a1, dylan_value a2, dylan_value a3, dylan_value a4,
#define PARAMTEMPLATEPREFIX5 dylan_value a1, dylan_value a2, dylan_value a3, dylan_value a4, dylan_value a5,
#define PARAMTEMPLATEPREFIX6 dylan_value a1, dylan_value a2, dylan_value a3, dylan_value a4, dylan_value a5, dylan_value a6,
#define PARAMTEMPLATEPREFIX7 dylan_value a1, dylan_value a2, dylan_value a3, dylan_value a4, dylan_value a5, dylan_value a6, dylan_value a7,
#define ARGTEMPLATEPREFIX0
#define ARGTEMPLATEPREFIX1 a1,
#define ARGTEMPLATEPREFIX2 a1, a2,
#define ARGTEMPLATEPREFIX3 a1, a2, a3,
#define ARGTEMPLATEPREFIX4 a1, a2, a3, a4,
#define ARGTEMPLATEPREFIX5 a1, a2, a3, a4, a5,
#define ARGTEMPLATEPREFIX6 a1, a2, a3, a4, a5, a6,
#define ARGTEMPLATEPREFIX7 a1, a2, a3, a4, a5, a6, a7,
#define PARAMTEMPLATESUFFIX0
#define PARAMTEMPLATESUFFIX1 dylan_value a1
#define PARAMTEMPLATESUFFIX2 dylan_value a1, dylan_value a2
#define PARAMTEMPLATESUFFIX3 dylan_value a1, dylan_value a2, dylan_value a3
#define PARAMTEMPLATESUFFIX4 dylan_value a1, dylan_value a2, dylan_value a3, dylan_value a4
#define PARAMTEMPLATESUFFIX5 dylan_value a1, dylan_value a2, dylan_value a3, dylan_value a4, dylan_value a5
#define PARAMTEMPLATESUFFIX6 dylan_value a1, dylan_value a2, dylan_value a3, dylan_value a4, dylan_value a5, dylan_value a6
#define PARAMTEMPLATESUFFIX7 dylan_value a1, dylan_value a2, dylan_value a3, dylan_value a4, dylan_value a5, dylan_value a6, dylan_value a7
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
dylan_value single_method_engine_##_nparams (PARAMTEMPLATE##_nparams) { \
    TEB* teb = get_teb(); \
    SINGLEMETHODENGINE* e = (SINGLEMETHODENGINE*)teb->function; \
    dylan_simple_method* meth = (dylan_simple_method*)e->meth; \
    DLFN mep = ((dylan_simple_method*)meth)->mep; \
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


dylan_value single_method_engine_n (dylan_value impargvec) {
  TEB* teb = get_teb();
  SINGLEMETHODENGINE* e = (SINGLEMETHODENGINE*)teb->function;
  return(primitive_mep_apply_with_optionals(e->meth, e->data, impargvec));
}



dylan_value check_explicit_kwds (dylan_simple_object_vector* optionals, dylan_simple_object_vector* kwds, int kwdskip) {
  dylan_value* optdata = vector_data(optionals);
  int optsize = vector_size(optionals);
  dylan_value* kwddata = vector_data(kwds);
  int kwdsize = vector_size(kwds);
  if ((optsize & 1) != 0) { // Check if odd?
    return(DFALSE);
  } else {
    int i;
    int j;
    for (i=0; i<optsize; i+=2) {
      dylan_value kwdarg = optdata[i];
      for (j=0; j<kwdsize; j+=kwdskip) {
        dylan_value kwd = kwddata[j];
        if (kwd == kwdarg) goto check_next;
      }
      return(kwdarg);
    check_next: ;
    }
    return(NULL);
  }
}

dylan_value check_unrestricted_kwds (dylan_simple_object_vector* optionals) {
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
    check_explicit_kwds((_optionals), ((dylan_keyword_method*)(_meth))->keyword_specifiers, 2)

#define CHECK_KEYWORDS_unrestricted(_optionals, _meth, _e) \
    check_unrestricted_kwds(_optionals)

#define INITMYARGVEC(argnum_, arg_) _argvec_data[argnum_] = arg_

extern dylan_value Kodd_number_of_keyword_args_trapVKeI(dylan_value gfargs, dylan_value gf, dylan_value engine);

extern dylan_value Kinvalid_keyword_trapVKeI(dylan_value gfargs, dylan_value gf, dylan_value engine, dylan_value badkwd, dylan_value keys, dylan_value implicitp);

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
                                                   ((dylan_keyword_method*)(invengine_)->meth)->keyword_specifiers, \
                                                   DTRUE))

#define INVALID_KEYWORD_unrestricted(invgf_, invargvec_, invbadkwd_, invmeth_, invengine_) \
  ( ((invbadkwd_) == DFALSE) ? \
     Kodd_number_of_keyword_args_trapVKeI((invargvec_), (invgf_), (invengine_)) \
    : \
     Kinvalid_keyword_trapVKeI((invargvec_), (invgf_), (invengine_), (invbadkwd_), \
                                                   DFALSE, DFALSE))


#define DEFINE_KEYED_SINGLE_METHOD_ENGINE(_how, _nparams) \
  dylan_value  _how##_keyed_single_method_engine_##_nparams (PARAMTEMPLATEPREFIX##_nparams dylan_simple_object_vector* optionals) \
  {  TEB* teb = get_teb(); \
    SINGLEMETHODENGINE* e = (SINGLEMETHODENGINE*)teb->function; \
    dylan_value parent = teb->next_methods; \
    dylan_simple_method* meth = (dylan_simple_method*)e->meth; \
    dylan_value badkwd; \
    badkwd = CHECK_KEYWORDS_##_how(optionals, meth, e); \
    if (badkwd == NULL) { \
      teb->function = meth; \
      teb->next_methods = e->data; \
      return((((dylan_simple_method*)meth)->mep)(ARGTEMPLATEPREFIX##_nparams optionals)); \
    } else { \
      int _argvecsize = _nparams + 1; \
      DEF_STACK_VECTOR_INITTED(_argvec, _argvecsize); \
      dylan_value* _argvec_data = vector_data(_argvec); \
      KLUDGEARGS##_nparams(INITMYARGVEC ); \
      _argvec_data[_nparams] = optionals; \
      return(INVALID_KEYWORD_##_how(parent_gf(parent), _argvec, badkwd, meth, e)); \
    } \
  }


#define DEFINE_KEYED_SINGLE_METHOD_ENGINE_UNSPREAD(_how) \
  dylan_value  _how##_keyed_single_method_engine_n (dylan_simple_object_vector* mepargvec) \
  { TEB* teb = get_teb(); \
    SINGLEMETHODENGINE* e = (SINGLEMETHODENGINE*)teb->function; \
    dylan_value parent = teb->next_methods; \
    dylan_simple_method* meth = (dylan_simple_method*)e->meth; \
    dylan_value badkwd; \
    int nimpargs = vector_size(mepargvec); \
    badkwd = CHECK_KEYWORDS_##_how(vector_ref(mepargvec, nimpargs-1), meth, e); \
    if (badkwd == NULL) { \
      return(primitive_mep_apply_with_optionals(meth, e->data, (dylan_value)mepargvec)); \
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
dylan_value cache_header_engine_##_nparams (PARAMTEMPLATE##_nparams) { \
    TEB* teb = get_teb(); \
    CACHEHEADERENGINE* e = (CACHEHEADERENGINE*)teb->function; \
    ENGINE* nxt = (ENGINE*)e->nextnode; \
    DLFN entrypt = nxt->entry_point; \
    teb->function = (dylan_simple_method*)nxt; \
    teb->next_methods = (dylan_value)e; \
    return(entrypt(ARGTEMPLATE##_nparams)); \
   }

extern dylan_value cache_header_engine_0 ();
extern dylan_value cache_header_engine_1 (dylan_value a1);
extern dylan_value cache_header_engine_2 (dylan_value a1, dylan_value a2);
extern dylan_value cache_header_engine_3 (dylan_value a1, dylan_value a2, dylan_value a3);
extern dylan_value cache_header_engine_4 (dylan_value a1, dylan_value a2, dylan_value a3, dylan_value a4);
extern dylan_value cache_header_engine_5 (dylan_value a1, dylan_value a2, dylan_value a3, dylan_value a4, dylan_value a5);
extern dylan_value cache_header_engine_6 (dylan_value a1, dylan_value a2, dylan_value a3, dylan_value a4, dylan_value a5, dylan_value a6);
extern dylan_value cache_header_engine_7 (dylan_value a1, dylan_value a2, dylan_value a3, dylan_value a4, dylan_value a5, dylan_value a6, dylan_value a7);
extern dylan_value cache_header_engine_n (dylan_value argvec);


DEFINE_CACHE_HEADER_ENGINE(0)
DEFINE_CACHE_HEADER_ENGINE(1)
DEFINE_CACHE_HEADER_ENGINE(2)
DEFINE_CACHE_HEADER_ENGINE(3)
DEFINE_CACHE_HEADER_ENGINE(4)
DEFINE_CACHE_HEADER_ENGINE(5)
DEFINE_CACHE_HEADER_ENGINE(6)
DEFINE_CACHE_HEADER_ENGINE(7)

dylan_value cache_header_engine_n (dylan_value theargvec) {
  TEB* teb = get_teb();
  dylan_simple_object_vector* argvec = (dylan_simple_object_vector*)theargvec;
  CACHEHEADERENGINE* e = (CACHEHEADERENGINE*)teb->function;
  ENGINE* newengine = (ENGINE*)(e->nextnode);
  if (FUNCTIONP(newengine)) {
    return(primitive_mep_apply_with_optionals((dylan_simple_method*)newengine, (dylan_value)e, argvec));
  } else {
    teb->function = (dylan_simple_method*)newengine;
    teb->next_methods = (dylan_value)e;
    return((newengine->entry_point)(argvec));
  }
}

#define DEFINE_PROFILING_CACHE_HEADER_ENGINE(_nparams) \
dylan_value profiling_cache_header_engine_##_nparams (PARAMTEMPLATE##_nparams) { \
    TEB* teb = get_teb(); \
    PROFILINGCACHEHEADERENGINE* e = (PROFILINGCACHEHEADERENGINE*)teb->function; \
    ENGINE* nxt = (ENGINE*)e->nextnode; \
    DLFN entrypt = nxt->entry_point; \
    teb->function = (dylan_simple_method*)nxt; \
    teb->next_methods = (dylan_value)e; \
    e->count1 += 4; \
    if (unlikely((dylan_value)(e->count1) == I(0))) e->count2 += 4; \
    return(entrypt(ARGTEMPLATE##_nparams)); \
   }

extern dylan_value profiling_cache_header_engine_0 ();
extern dylan_value profiling_cache_header_engine_1 (dylan_value a1);
extern dylan_value profiling_cache_header_engine_2 (dylan_value a1, dylan_value a2);
extern dylan_value profiling_cache_header_engine_3 (dylan_value a1, dylan_value a2, dylan_value a3);
extern dylan_value profiling_cache_header_engine_4 (dylan_value a1, dylan_value a2, dylan_value a3, dylan_value a4);
extern dylan_value profiling_cache_header_engine_5 (dylan_value a1, dylan_value a2, dylan_value a3, dylan_value a4, dylan_value a5);
extern dylan_value profiling_cache_header_engine_6 (dylan_value a1, dylan_value a2, dylan_value a3, dylan_value a4, dylan_value a5, dylan_value a6);
extern dylan_value profiling_cache_header_engine_7 (dylan_value a1, dylan_value a2, dylan_value a3, dylan_value a4, dylan_value a5, dylan_value a6, dylan_value a7);
extern dylan_value profiling_cache_header_engine_n (dylan_value argvec);


DEFINE_PROFILING_CACHE_HEADER_ENGINE(0)
DEFINE_PROFILING_CACHE_HEADER_ENGINE(1)
DEFINE_PROFILING_CACHE_HEADER_ENGINE(2)
DEFINE_PROFILING_CACHE_HEADER_ENGINE(3)
DEFINE_PROFILING_CACHE_HEADER_ENGINE(4)
DEFINE_PROFILING_CACHE_HEADER_ENGINE(5)
DEFINE_PROFILING_CACHE_HEADER_ENGINE(6)
DEFINE_PROFILING_CACHE_HEADER_ENGINE(7)


dylan_value profiling_cache_header_engine_n (dylan_value theargvec) {
  TEB* teb = get_teb();
  dylan_simple_object_vector* argvec = (dylan_simple_object_vector*)theargvec;
  CACHEHEADERENGINE* e = (CACHEHEADERENGINE*)teb->function;
  ENGINE* newengine = (ENGINE*)(e->nextnode);
  if (FUNCTIONP(newengine)) {
    return(primitive_mep_apply_with_optionals((dylan_simple_method*)newengine, (dylan_value)e, argvec));
  } else {
    teb->function = (dylan_simple_method*)newengine;
    teb->next_methods = (dylan_value)e;
    return((newengine->entry_point)(argvec));
  }
}

dylan_value primitive_enable_cache_header_engine_node (dylan_value engine, dylan_value genfun) {
  ENGINE* e = (ENGINE*)engine;
  dylan_generic_function* gf = (dylan_generic_function*)genfun;
  dylan_signature* sig = (dylan_signature*)gf->signature;
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


dylan_value primitive_invalidate_cache_engine_node (dylan_value engine, dylan_value genfun) {
  ignore(genfun);
  ((ENGINE*)engine)->entry_point = (DLFN)&general_engine_node_n_engine;
  return(engine);
}


/* **************************************************************** */

dylan_value primitive_initialize_engine_node (dylan_value engine) {
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
    dylan_simple_method* meth = (dylan_simple_method*)e->meth;
    dylan_signature* sig = (dylan_signature*)meth->signature;
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
  dylan_value discriminate_engine_##_argnum##_##_nargs (PARAMTEMPLATE##_nargs) { \
    TEB* teb = get_teb(); \
    ENGINE* d_ = (ENGINE*)teb->function; \
    dylan_value parent_ = teb->next_methods; \
    DLFN cb_ = d_->callback; \
    ENGINE* newengine_ = (ENGINE*)(cb_((ARGUMENTNAME##_argnum), parent_, d_)); \
    DLFN ncb_ = newengine_->entry_point; \
    teb->function = (dylan_simple_method*)newengine_; \
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


dylan_value discriminate_engine_n_n (dylan_simple_object_vector* args) {
  TEB* teb = get_teb();
  ENGINE* e = (ENGINE*)teb->function;
  dylan_value parent = teb->next_methods;
  DLFN cb = e->callback;
  long props = (long)e->properties;
  long argnum = (props >> 8) & 0xFF;
  dylan_value* a = vector_data(args);
  dylan_value arg = a[argnum];
  ENGINE* newengine = (ENGINE*)(cb(arg, parent, e));
  if (FUNCTIONP(newengine)) {
    return(primitive_mep_apply_with_optionals((dylan_simple_method*)newengine, parent, args));
  } else {
    teb->function = (dylan_simple_method*)newengine;
    teb->next_methods = parent;
    return((newengine->entry_point)(args));
  }
}

/* ---------------------------------------------- */

extern dylan_value Dabsent_engine_nodeVKg;
extern dylan_value Ddirect_object_mm_wrappersVKi;

#define MONO_WRAPPER_KEY(x) \
  (TAGGEDQ(x) ? ((dylan_value*)Ddirect_object_mm_wrappersVKi)[TAG_BITS(x)] : ((dylan_object*)x)->mm_wrapper)

#define DEFINE_MONOMORPHIC_DISCRIMINATOR(_argnum, _nargs) \
  dylan_value monomorphic_discriminator_engine_##_argnum##_##_nargs (PARAMTEMPLATE##_nargs) { \
    TEB* teb = get_teb(); \
    MONOMORPHICDISCRIMINATOR* d_ = (MONOMORPHICDISCRIMINATOR*)teb->function; \
    dylan_value parent_ = teb->next_methods; \
    DWORD key = (DWORD)(FI(MONO_WRAPPER_KEY(ARGUMENTNAME##_argnum))); \
    ENGINE* newengine_ = (ENGINE*)((key == d_->key) \
                                    ? d_->nextnode \
                                    : Dabsent_engine_nodeVKg);  \
    DLFN ncb_ = newengine_->entry_point; \
    teb->function = (dylan_simple_method*)newengine_; \
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


dylan_value monomorphic_discriminator_engine_n_n (dylan_simple_object_vector* args) {
  TEB* teb = get_teb();
  MONOMORPHICDISCRIMINATOR* e = (MONOMORPHICDISCRIMINATOR*)teb->function;
  dylan_value parent = teb->next_methods;
  long props = (long)e->properties;
  long argnum = (props >> 8) & 0xFF;
  dylan_value* a = vector_data(args);
  dylan_object* arg = (dylan_object*)a[argnum];
  DWORD key = (DWORD)(FI(MONO_WRAPPER_KEY(arg)));
  ENGINE* newengine = (ENGINE*)((key == e->key)
                                ? e->nextnode
                                : Dabsent_engine_nodeVKg);
  if (FUNCTIONP(newengine)) {
    return(primitive_mep_apply_with_optionals((dylan_simple_method*)newengine, parent, args));
  } else {
    teb->function = (dylan_simple_method*)newengine;
    teb->next_methods = parent;
    return((newengine->entry_point)(args));
  }
}

/* ---------------------------------------------- */

extern dylan_value Dinapplicable_engine_nodeVKg;

#define DEFINE_IF_TYPE_DISCRIMINATOR(_argnum, _nargs) \
  dylan_value if_type_discriminator_engine_##_argnum##_##_nargs (PARAMTEMPLATE##_nargs) { \
    TEB* teb = get_teb(); \
    IFTYPEDISCRIMINATOR* d_ = (IFTYPEDISCRIMINATOR*)teb->function; \
    dylan_value parent_ = teb->next_methods; \
    ENGINE* newengine_ = (ENGINE*)((INSTANCEP((ARGUMENTNAME##_argnum),d_->type)) \
                                   ? d_->thennode \
                                   : d_->elsenode); \
    DLFN ncb_ = newengine_->entry_point; \
    teb->function = (dylan_simple_method*)newengine_; \
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


dylan_value if_type_discriminator_engine_n_n (dylan_simple_object_vector* args) {
  TEB* teb = get_teb();
  IFTYPEDISCRIMINATOR* e = (IFTYPEDISCRIMINATOR*)teb->function;
  dylan_value parent = teb->next_methods;
  long props = (long)e->properties;
  long argnum = (props >> 8) & 0xFF;
  dylan_value* a = vector_data(args);
  dylan_value arg = a[argnum];
  ENGINE* newengine = (ENGINE*)(INSTANCEP(arg, e->type)
                                ? e->thennode
                                : e->elsenode);
  if (FUNCTIONP(newengine)) {
    return(primitive_mep_apply_with_optionals((dylan_simple_method*)newengine, parent, args));
  } else {
    teb->function = (dylan_simple_method*)newengine;
    teb->next_methods = parent;
    return((newengine->entry_point)(args));
  }
}

/* ---------------------------------------------- */

extern dylan_value Dinapplicable_engine_nodeVKg;

#define DEFINE_TYPECHECK_DISCRIMINATOR(_argnum, _nargs) \
  dylan_value typecheck_discriminator_engine_##_argnum##_##_nargs (PARAMTEMPLATE##_nargs) { \
    TEB* teb = get_teb(); \
    TYPECHECKDISCRIMINATOR* d_ = (TYPECHECKDISCRIMINATOR*)teb->function; \
    dylan_value parent_ = teb->next_methods; \
    ENGINE* newengine_ = (ENGINE*)((INSTANCEP((ARGUMENTNAME##_argnum),d_->type)) \
                                   ? d_->nextnode \
                                   : Dinapplicable_engine_nodeVKg); \
    DLFN ncb_ = newengine_->entry_point; \
    teb->function = (dylan_simple_method*)newengine_; \
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


dylan_value typecheck_discriminator_engine_n_n (dylan_simple_object_vector* args) {
  TEB* teb = get_teb();
  TYPECHECKDISCRIMINATOR* e = (TYPECHECKDISCRIMINATOR*)teb->function;
  dylan_value parent = teb->next_methods;
  long props = (long)e->properties;
  long argnum = (props >> 8) & 0xFF;
  dylan_value* a = vector_data(args);
  dylan_value arg = a[argnum];
  ENGINE* newengine = (ENGINE*)(INSTANCEP(arg, e->type)
                                ? e->nextnode
                                : Dinapplicable_engine_nodeVKg);
  if (FUNCTIONP(newengine)) {
    return(primitive_mep_apply_with_optionals((dylan_simple_method*)newengine, parent, args));
  } else {
    teb->function = (dylan_simple_method*)newengine;
    teb->next_methods = parent;
    return((newengine->entry_point)(args));
  }
}

/* ---------------------------------------------- */

dylan_value primitive_initialize_discriminator(dylan_value discriminator) {
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

INLINE dylan_value MV_SPILL_into (dylan_value first_value, MV *dest) {
  TEB* teb = get_teb();
  int i, n = teb->return_values.count;
  teb->return_values.value[0] = first_value;
  for (i = 0; i < n; i++) {
    dest->value[i] = teb->return_values.value[i];
  }
  dest->count = n;
  return (dylan_value) dest;
}

dylan_value MV_SPILL (dylan_value first_value) {
  TEB* teb = get_teb();
  int n = teb->return_values.count;
  MV *dest = (MV *) primitive_allocate(1 + n);
  MV_SPILL_into(first_value, dest);
  return (dylan_value) dest;
}

dylan_value MV_UNSPILL (dylan_value spill_t) {
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

dylan_value MV_CHECK_TYPE_REST (dylan_value first_value, dylan_value rest_type, int n, ...) {
  TEB* teb = get_teb();
  int i, mv_n = teb->return_values.count;
  MV spill;
  va_list ap; va_start(ap, n);
  MV_SPILL_into(first_value, &spill);
  for (i = 0; i < n; i++) {
    dylan_value type = va_arg(ap, dylan_value);
    PERFORM_INLINE_TYPE_CHECK(spill.value[i], type);
  }
  for (; i < mv_n; i++) {
    PERFORM_INLINE_TYPE_CHECK(spill.value[i], rest_type);
  }
  MV_UNSPILL((dylan_value)&spill);
  return first_value;
}


dylan_value MV_GET_REST_AT (dylan_value first_value, DSINT first) {
  TEB* teb = get_teb();
  int  offset = first;
  int  n = teb->return_values.count - offset;
  teb->return_values.value[0] = first_value;
  return(make_vector_from_buffer(n < 0 ? 0 : n, &teb->return_values.value[offset]));
}

dylan_value MV_SET_REST_AT (dylan_value v, DSINT first) {
  TEB* teb = get_teb();
  int i, size = vector_size(v), offset = first;
  for (i=0; i<size; ++i) {
    teb->return_values.value[offset + i] = vector_ref(v, i);
  }
  teb->return_values.count = offset + size;
  return teb->return_values.count == 0 ? DFALSE : teb->return_values.value[0];
}

dylan_value MV2_ (dylan_value x, dylan_value y) {
  TEB* teb = get_teb();
  teb->return_values.value[0] = x;
  teb->return_values.value[1] = y;
  teb->return_values.count = 2;
  return x;
}

dylan_value MV3_ (dylan_value x, dylan_value y, dylan_value z) {
  TEB* teb = get_teb();
  teb->return_values.value[0] = x;
  teb->return_values.value[1] = y;
  teb->return_values.value[2] = z;
  teb->return_values.count = 3;
  return x;
}


/* CLOSURES */

extern Wrapper KLmethodGVKdW;

dylan_value MAKE_CLOSURE (dylan_value schema, int closure_size) {
  dylan_simple_closure_method* fn = (dylan_simple_closure_method*)allocate(sizeof(dylan_simple_closure_method) + closure_size * sizeof(dylan_value));
  memcpy(fn, schema, sizeof(dylan_simple_closure_method));
  return((dylan_value)fn);
}

dylan_value MAKE_CLOSURE_SIG (dylan_value schema, dylan_value sig, int closure_size) {
  dylan_simple_closure_method* fn = (dylan_simple_closure_method*)allocate(sizeof(dylan_simple_closure_method) + closure_size * sizeof(dylan_value));
  memcpy(fn, schema, sizeof(dylan_simple_closure_method));
  fn->signature = sig;
  return((dylan_value)fn);
}

INLINE void init_environment (dylan_simple_closure_method* fn, int size, dylan_value* buf) {
  if (size > 0) {
    COPY_WORDS(&(fn->environment), buf, size);
  }
  fn->size = I(size);
}

void INIT_CLOSURE (dylan_value function, int closure_size, ...) {
  TEB* teb = get_teb();
  dylan_simple_closure_method* fn = function;
  BUFFER_VARARGS(closure_size, closure_size, teb->buffer);
  init_environment(fn, closure_size, teb->buffer);
}

dylan_value MAKE_CLOSURE_INITD (dylan_value schema, int closure_size, ...) {
  TEB* teb = get_teb();
  dylan_simple_closure_method* fn = (dylan_simple_closure_method*)allocate(sizeof(dylan_simple_closure_method) + closure_size * sizeof(dylan_value));
  memcpy(fn, schema, sizeof(dylan_simple_closure_method));
  BUFFER_VARARGS(closure_size, closure_size, teb->buffer);
  init_environment(fn, closure_size, teb->buffer);
  return((dylan_value)fn);
}

dylan_value MAKE_CLOSURE_INITD_SIG (dylan_value schema, dylan_value sig, int closure_size, ...) {
  TEB* teb = get_teb();
  dylan_simple_closure_method* fn = (dylan_simple_closure_method*)allocate(sizeof(dylan_simple_closure_method) + closure_size * sizeof(dylan_value));
  memcpy(fn, schema, sizeof(dylan_simple_closure_method));
  fn->signature = sig;
  BUFFER_VARARGS(closure_size, closure_size, teb->buffer);
  init_environment(fn, closure_size, teb->buffer);
  return((dylan_value)fn);
}

dylan_value MAKE_METHOD_SIG (dylan_value schema, dylan_value sig) {
  dylan_simple_closure_method* fn = (dylan_simple_closure_method*)allocate(sizeof(dylan_simple_closure_method));
  memcpy(fn, schema, sizeof(dylan_simple_closure_method));
  fn->signature = sig;
  return(fn);
}

dylan_value SET_METHOD_SIG (dylan_value method, dylan_value sig) {
  dylan_simple_closure_method* fn = (dylan_simple_closure_method*)method;
  fn->signature = sig;
  return((dylan_value)fn);
}

dylan_value MAKE_KEYWORD_CLOSURE (dylan_value schema, int closure_size) {
  dylan_keyword_closure_method* fn = (dylan_keyword_closure_method*)allocate(sizeof(dylan_keyword_closure_method) + closure_size * sizeof(dylan_value));
  memcpy(fn, schema, sizeof(dylan_keyword_closure_method));
  return((dylan_value)fn);
}

dylan_value MAKE_KEYWORD_CLOSURE_SIG (dylan_value schema, dylan_value sig, int closure_size) {
  dylan_keyword_closure_method* fn = (dylan_keyword_closure_method*)allocate(sizeof(dylan_keyword_closure_method) + closure_size * sizeof(dylan_value));
  memcpy(fn, schema, sizeof(dylan_keyword_closure_method));
  fn->signature = sig;
  return((dylan_value)fn);
}

INLINE void init_keyword_environment (dylan_keyword_closure_method* fn, int size, dylan_value* buf) {
  if (size > 0) {
    COPY_WORDS(&(fn->environment), buf, size);
  }
  fn->size = I(size);
}

void INIT_KEYWORD_CLOSURE (dylan_value function, int closure_size, ...) {
  TEB* teb = get_teb();
  dylan_keyword_closure_method* fn = function;
  BUFFER_VARARGS(closure_size, closure_size, teb->buffer);
  init_keyword_environment(fn, closure_size, teb->buffer);
}

dylan_value MAKE_KEYWORD_CLOSURE_INITD (dylan_value schema, int closure_size, ...) {
  TEB* teb = get_teb();
  dylan_keyword_closure_method* fn = (dylan_keyword_closure_method*)allocate(sizeof(dylan_keyword_closure_method) + closure_size * sizeof(dylan_value));
  memcpy(fn, schema, sizeof(dylan_keyword_closure_method));
  BUFFER_VARARGS(closure_size, closure_size, teb->buffer);
  init_keyword_environment(fn, closure_size, teb->buffer);
  return((dylan_value)fn);
}

dylan_value MAKE_KEYWORD_CLOSURE_INITD_SIG (dylan_value schema, dylan_value sig, int closure_size, ...) {
  TEB* teb = get_teb();
  dylan_keyword_closure_method* fn = (dylan_keyword_closure_method*)allocate(sizeof(dylan_keyword_closure_method) + closure_size * sizeof(dylan_value));
  memcpy(fn, schema, sizeof(dylan_keyword_closure_method));
  fn->signature = sig;
  BUFFER_VARARGS(closure_size, closure_size, teb->buffer);
  init_keyword_environment(fn, closure_size, teb->buffer);
  return((dylan_value)fn);
}

dylan_value MAKE_KEYWORD_METHOD_SIG (dylan_value schema, dylan_value sig) {
  dylan_keyword_method* fn = (dylan_keyword_method*)allocate(sizeof(dylan_keyword_method));
  memcpy(fn, schema, sizeof(dylan_keyword_method));
  fn->signature = sig;
  return(fn);
}

dylan_value SET_KEYWORD_METHOD_SIG (dylan_value method, dylan_value sig) {
  dylan_keyword_closure_method* fn = (dylan_keyword_closure_method*)method;
  fn->signature = sig;
  return((dylan_value)fn);
}

/* PRIMITIVES */

INLINE dylan_value primitive_apply_using_buffer (dylan_simple_method* fn, int n, dylan_value a[]) {
  TEB* teb = get_teb();
  int i, j;
  dylan_simple_object_vector* optionals = (dylan_simple_object_vector*)a[n - 1];
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

dylan_value primitive_apply (dylan_value fn, dylan_value args) {
  return(primitive_apply_using_buffer
           ((dylan_simple_method*)fn, vector_size((dylan_simple_object_vector*)args), vector_data((dylan_simple_object_vector*)args)));
}

dylan_value primitive_apply_spread (dylan_value fn, int n, ...) {
  TEB* teb = get_teb();
  BUFFER_VARARGS(n, n, teb->buffer);
  return(primitive_apply_using_buffer((dylan_simple_method*)fn, n, teb->buffer));
}

dylan_value primitive_mep_apply_spread (dylan_value fn, dylan_value nm, int n, ...) {
  TEB* teb = get_teb();
  BUFFER_VARARGS(n, n, teb->buffer);
  dylan_simple_object_vector* v = (dylan_simple_object_vector*)teb->buffer[n - 1];
  int v_size = vector_size(v);
  int new_size = n + v_size - 1;
  DEF_STACK_VECTOR_FROM_BUFFER_WITH_SIZE
    (new_arguments, new_size, teb->buffer, n - 1);
  COPY_WORDS
    (&(vector_data(new_arguments)[n - 1]), vector_data(v), v_size);
  return(primitive_mep_apply((dylan_simple_method*)fn, nm, (dylan_value *)new_arguments));
}

dylan_value primitive_engine_node_apply_spread (ENGINE* e, dylan_value parent, int n, ...) {
  TEB* teb = get_teb();
  BUFFER_VARARGS(n, n, teb->buffer);
  dylan_simple_object_vector* v = (dylan_simple_object_vector*)teb->buffer[n - 1];
  int v_size = vector_size(v);
  int new_size = n + v_size - 1;
  DEF_STACK_VECTOR_FROM_BUFFER_WITH_SIZE
    (new_arguments, new_size, teb->buffer, n - 1);
  COPY_WORDS
    (&(vector_data(new_arguments)[n - 1]), vector_data(v), v_size);
  return(primitive_engine_node_apply(e, parent, (dylan_value *)new_arguments));
}

/* temporary primitives for assignment */

dylan_value MAKE_DYLAN_VALUE_CELL(dylan_value value) {
  dylan_value cell = primitive_allocate(1);
  *(dylan_value*)cell = value;
  return cell;
}

#define define_make_cell(type) \
  dylan_value MAKE_ ## type ## _CELL(type value) { \
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

dylan_value primitive_vector (dylan_value n, ...) {
  TEB* teb = get_teb();
  BUFFER_VARARGS(R(n), n, teb->arguments);
  return(make_vector_from_buffer(R(n), teb->arguments));
}

dylan_value primitive_values (dylan_value v) {
  return(MV_SET_REST_AT(v, 0));
}

/* Hack oblist */

#define INITIAL_OBLIST_SIZE (64)

extern dylan_value LsymbolGVKd;
extern dylan_value Ksystem_allocate_simple_instanceVKeI (dylan_value class_, dylan_value Urest_, dylan_value fill_);

dylan_value primitive_make_symbol (dylan_value string)
{
  dylan_value symbol
    = Ksystem_allocate_simple_instanceVKeI
        (LsymbolGVKd, Pempty_vectorVKi, &KPunboundVKi);
  ((dylan_symbol*)symbol)->name = string;
  return(symbol);
}

// XXX locking?
static int oblist_size = 0;
static int oblist_cursor = 0;
static dylan_value *oblist = NULL;

dylan_value primitive_preboot_symbols () {
  return(primitive_raw_as_vector((dylan_value)(long)oblist_cursor, oblist));
}

dylan_value primitive_string_as_symbol_using_symbol (dylan_value string, dylan_value symbol)
{
  int input_string_size = R(((dylan_byte_string*)string)->size);
  char *input_string_data = ((dylan_byte_string*)string)->data;

  int i;

  for (i = 0; i < oblist_cursor; ++i) {
    dylan_symbol *oblist_symbol = (dylan_symbol*)oblist[i];
    int oblist_string_size = R(((dylan_byte_string*)(oblist_symbol->name))->size);
    char *oblist_string_data = ((dylan_byte_string*)(oblist_symbol->name))->data;
    if (oblist_string_size == input_string_size
          && strncasecmp
               (oblist_string_data, input_string_data, (size_t)input_string_size)
                  == 0) {
      return((dylan_value)oblist_symbol);
    }
  }
  if (oblist_cursor >= oblist_size) {
    oblist_size += INITIAL_OBLIST_SIZE;
#if defined(GC_USE_BOEHM)
    oblist = (dylan_value*)GC_REALLOC(oblist, oblist_size * sizeof(dylan_value));
#elif defined(GC_USE_MALLOC)
    oblist = (dylan_value*)realloc(oblist, oblist_size * sizeof(dylan_value));
#endif
  }
  if (symbol == NULL) {
    symbol = primitive_make_symbol(string);
  }
  oblist[oblist_cursor++] = symbol;
  return symbol;
}

dylan_value primitive_string_as_symbol (dylan_value string)
{
  return(primitive_string_as_symbol_using_symbol(string, NULL));
}

dylan_value primitive_resolve_symbol (dylan_value symbol)
{
  return(primitive_string_as_symbol_using_symbol
           (((dylan_symbol*)symbol)->name, symbol));
}

dylan_value primitive_slot_value(dylan_value object, DSINT position)
{
  dylan_value slot_value = primitive_initialized_slot_value(object, position);
  if (UNBOUND_P(slot_value)) {
    return(UNBOUND_INSTANCE_SLOT(object, I(position)));
  } else {
    return(slot_value);
  }
  return(slot_value);
}

dylan_value SLOT_VALUE(dylan_value object, DSINT position)
{
  dylan_value slot_value = primitive_initialized_slot_value(object, position);
  if (UNBOUND_P(slot_value)) {
    return(UNBOUND_INSTANCE_SLOT(object, I(position)));
  } else {
    return(slot_value);
  }
}

/* TERMINAL */

/* OPERATING SYSTEM */

dylan_value Tcommand_nameT;

dylan_value pseudo_primitive_command_name () {
  return(Tcommand_nameT);
}

dylan_value Tcommand_argumentsT;

dylan_value pseudo_primitive_command_arguments () {
  return(Tcommand_argumentsT);
}

void primitive_exit_application (DSINT code) {
  exit(code);
}


/* TOP LEVEL INITIALIZATION */

static void call_application_exit_functions(void) {
  extern dylan_value Kcall_application_exit_functionsVKeI();
  (void) Kcall_application_exit_functionsVKeI();
}

extern dylan_value IKJboole_ior_, IKJboole_xor_;

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
#endif

    initialize_threads_primitives();
  }
}
