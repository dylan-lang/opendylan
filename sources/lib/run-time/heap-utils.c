#include "heap-utils.h"

#include <stdlib.h>
#ifdef OPEN_DYLAN_PLATFORM_UNIX
#include <signal.h>
#endif

#ifdef OPEN_DYLAN_PLATFORM_UNIX
void *alloc_obj(size_t size)
{
  return malloc(size);
}

void free_obj(void *obj, size_t size)
{
  (void)size;

  free(obj);
}
#else
static HANDLE process_heap = 0;

void *alloc_obj(size_t size)
{
  void *new;
  if (process_heap == 0) {
    process_heap = GetProcessHeap();
  }
  new = HeapAlloc(process_heap, 0, size);
  return new;
}

void free_obj(void *obj, size_t size)
{
  HeapFree(process_heap, 0, obj);
}
#endif

void report_message (char* message)
{
  mps_lib_FILE *stream = mps_lib_get_stdout();
  mps_lib_fputs(message, stream);
}

void report_error (char* message)
{
  mps_lib_FILE *stream = mps_lib_get_stderr();
  mps_lib_fputs("\nError:\n", stream);
  mps_lib_fputs(message, stream);
  mps_lib_fputc('\n', stream);
  mps_lib_abort();
}


void report_break (char* message)
{
  mps_lib_FILE *stream = mps_lib_get_stdout();
  mps_lib_fputs("Break to debugger:\n    ", stream);
  mps_lib_fputs(message, stream);
  mps_lib_fputc('\n', stream);
#ifdef OPEN_DYLAN_PLATFORM_UNIX
  raise(SIGTRAP);
#else
  DebugBreak();
#endif
}


static int wrapper_fixed_format(void* wrapper)
{
  int wf = ((int*)wrapper)[3];
  return wf & 3;
}

static int wrapper_fixed_length_in_words(void* wrapper)
{
  int wf = ((int*)wrapper)[3];
  int fl = wf >> 2;
  return fl + 1; /* 1 for the header itself */
}

static int wrapper_vector_format(void* wrapper)
{
  int wf = ((int*)wrapper)[4];
  return wf & 7;
}

static int wrapper_vector_scaling_in_bytes(void* wrapper)
{
  int wv = ((int*)wrapper)[4];
  int vf = wv & 7;
  switch (vf) {
  case 7:
    return(0);
  case 4:
  case 5:
    return(1);  /* assume all non-word vectors are byte sized */
  default:
    return(4);
  }
}

static int wrapper_pattern(void* wrapper, int pat_index)
{
  return ((int*)wrapper)[6 + pat_index];
}


int object_repeated_size (void *object, int fixed_offset)
{
  int tagged_size = ((int*)object)[fixed_offset];
  return tagged_size >> 2;
}

int size_of_object (void *object, void* wrapper)
{
  int fixed = wrapper_fixed_length_in_words(wrapper);
  int vec_scale = wrapper_vector_scaling_in_bytes(wrapper);
  if (vec_scale) {
    return (4 * fixed)
           + 4 /* for the size slot */
           + (vec_scale * object_repeated_size(object, fixed));
  } else {
    return 4 * fixed;
  }
}

static int traceable_word_size_of_object (void *object, void* wrapper)
{
  int fixed = wrapper_fixed_length_in_words(wrapper);
  int vec_scale = wrapper_vector_scaling_in_bytes(wrapper);
  if (vec_scale == 4) {
    return fixed
           + 1 /* for the size slot */
           + object_repeated_size(object, fixed);
  } else {
    return fixed;
  }
}

int trace_object (mps_addr_t parent, object_tracer_t fn, void* env)
{
  mps_addr_t *refs = (mps_addr_t*)parent;
  mps_addr_t wrapper = *refs;
  int parent_size = 0;

  if ((wrapper != NULL) && ((int)wrapper & 3) == 0) {
    /* check wrapper to ensure it's a proper object */

    /* decode the wrapper */
    int fixlen = wrapper_fixed_length_in_words(wrapper);
    int fixform = wrapper_fixed_format(wrapper);
    int vecform = wrapper_vector_format(wrapper);
    int i;

    parent_size = size_of_object(parent, wrapper);

    /* trace the fixed fields */
    if (fixform == 1) {   /* traceable */
      for (i = 1; i < fixlen; i++) {
        if (! fn(refs[i], parent, parent_size, env)) {
          goto done;
        }
      }
    } else if (fixform == 2) {  /* patterned */
      int patindex = 0;
      int seen = 0;
      int pat = 0;
      for (i = 1; i < fixlen; i++) {
        if (seen == 0) {
          pat = wrapper_pattern(wrapper, patindex);
          patindex++;
        }
        if (pat & 1) { /* trace this reference */
          if (! fn(refs[i], parent, parent_size, env)) {
            goto done;
          }
        }
        pat >>= 1; /* shift down for next bit */
        seen++;    /* increment count of bits used */
        if (seen == 32)
          seen = 0; /* pattern word exhausted. Use next one */
      }
    }

    /* trace the vector fields */
    if (vecform == 2) { /* traceable */
      int replen = object_repeated_size(parent, fixlen);
      int totlen = fixlen + replen + 1; /* 1 for the size slot */
      for (i = fixlen+1; i < totlen; i++) {
        if (! fn(refs[i], parent, parent_size, env)) {
          goto done;
        }
      }
    }
  }
  done:
  return parent_size;
}



static
__inline
void *wrapper_class(void *wrapper)
{
  void *iclass = ((void**)wrapper)[1];
  void *class  = ((void**)iclass)[2];

  return class;
}

char* class_name_from_wrapper (void* wrapper)
{
  void *class = wrapper_class(wrapper);
  char *name = (char*)((void**)class)[2];
  return (name + 8);
}

void display_integer (int integer, mps_lib_FILE *stream)
{  /* This is naieve. Assume no more than 7 digits */
  int remainder = integer;
  int leading = 1;
  int power;
  if (integer == 0) {
    /* special case needs the leading zero */
      mps_lib_fputs("       0", stream);
      return;
  }
  for (power = 10000000; power > 0; power = power / 10) {
    int digit = remainder / power;
    remainder = remainder % power;
    if (digit == 0) {
      mps_lib_fputc(leading ? ' ' : '0', stream);
    } else {
      leading = 0;
      mps_lib_fputc('0' + digit, stream);
    };
    if ((power == 1000000) || (power == 1000)) {
      if (digit == 0) {
        mps_lib_fputc(leading ? ' ' : ',', stream);
      } else {
        mps_lib_fputc(',', stream);
      }
    }
  }
}

void display_hex_address (void *address, mps_lib_FILE *stream)
{
  unsigned int integer = (unsigned int)address;
  unsigned int remainder = integer;
  int leading = 1;
  unsigned int power;
  for (power = 0x10000000; power > 0; power = power / 0x10) {
    unsigned int digit = remainder / power;
    remainder = remainder % power;
    if (digit == 0) {
      mps_lib_fputc(leading ? ' ' : '0', stream);
    } else if (digit > 9) {
      leading = 0;
      mps_lib_fputc('A' + digit - 10, stream);
    } else {
      leading = 0;
      mps_lib_fputc('0' + digit, stream);
    }
  }
}



static int padding_for_string(char *string, int length)
{
  int padding = length;
  char *remaining = string;
  while (*remaining) {
    remaining++;
    padding--;
  }
  return padding;
}

void display_padding_for_string(char *string, char pad, int field, mps_lib_FILE *stream)
{
  int i;
  int padding = padding_for_string(string, field);
  for (i = 0; i < padding; i++)
    mps_lib_fputc(' ', stream);
}


