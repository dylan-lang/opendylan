#include "run-time.h"
#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>

// XXX: This BOOL definition is kept around for escape_p since there's
//      a usage of it as an int to pass a flag other than true/false
//      with print_integer. This should be fixed one day.
#define BOOL            int
#ifndef TRUE
#define TRUE            1
#define FALSE           0
#endif

#define STREAM     char*
#define format(string,cs,arg)     { char CS[128]; sprintf(CS, "%%s%s", cs); sprintf(stream, CS, stream, arg); }
#define put_string(string,stream) sprintf(stream, "%s%s", stream, string)
#define put_char(char,stream)     sprintf(stream, "%s%c", stream, char)

static int dylan_print_length = 10;
static int dylan_print_depth  = 3;

#define ignore(x) (void)x

/* INSTANCE */

D dylan_slot_element (D* instance, int offset) {
  return instance[offset + 1];
}

D dylan_object_class (D* instance) {
  return OBJECT_CLASS(instance);
}

/* BOOLEAN */

extern D LbooleanGVKd;
extern OBJECT KPfalseVKi;
extern OBJECT KPtrueVKi;

bool dylan_boolean_p (D instance) {
  return dylan_object_class(instance) == LbooleanGVKd;
}

bool dylan_true_p (D instance) {
  return instance == DTRUE;
}

/* FLOAT */

extern D Lsingle_floatGVKd;
extern D Ldouble_floatGVKd;

bool dylan_float_p (D instance) {
  return dylan_object_class(instance) == Lsingle_floatGVKd ||
         dylan_object_class(instance) == Ldouble_floatGVKd;
}

bool dylan_single_float_p (D instance) {
  return dylan_object_class(instance) == Lsingle_floatGVKd;
}

float
dylan_single_float_data (D instance) {
  return ((DSF)instance)->data;
}

bool dylan_double_float_p (D instance) {
  return dylan_object_class(instance) == Ldouble_floatGVKd;
}

double
dylan_double_float_data (D instance) {
  return ((DDF)instance)->data;
}

/* SYMBOL */

extern D LsymbolGVKd;

bool dylan_symbol_p (D instance) {
  return dylan_object_class(instance) == LsymbolGVKd;
}

D dylan_symbol_name (D instance) {
  return dylan_slot_element(instance, 0);
}

/* PAIR */

extern D LpairGVKd;
extern D Lempty_listGVKd;

bool dylan_pair_p (D instance) {
  return dylan_object_class(instance) == LpairGVKd;
}

bool dylan_empty_list_p (D instance) {
  return dylan_object_class(instance) == Lempty_listGVKd;
}

D dylan_head (D instance) {
  return dylan_slot_element(instance, 0);
}

D dylan_tail (D instance) {
  return dylan_slot_element(instance, 1);
}

/* VECTOR */

extern D  Lsimple_object_vectorGVKd;
extern D  vector_ref (SOV* vector, int offset);
extern D* vector_data (SOV* vector);
extern int vector_size (SOV* vector);

bool dylan_vector_p (D instance) {
  return dylan_object_class(instance) == Lsimple_object_vectorGVKd;
}

/* STRING */

#include <string.h>

extern D Lbyte_stringGVKd;

bool dylan_string_p (D instance) {
  return dylan_object_class(instance) == Lbyte_stringGVKd;
}

char* dylan_string_data (D instance) {
  return ((BS*)instance)->data;
}

/* SIMPLE-CONDITION */

extern D Lsimple_conditionGVKe;
extern FN KinstanceQVKd;
extern FN Kcondition_format_stringVKd;
extern FN Kcondition_format_argumentsVKd;

bool dylan_simple_condition_p (D instance) {
  return DTRUE == CALL2(&KinstanceQVKd, instance, Lsimple_conditionGVKe);
}

D dylan_simple_condition_format_string (D instance) {
  return CALL1(&Kcondition_format_stringVKd, instance);
}

D dylan_simple_condition_format_args (D instance) {
  return CALL1(&Kcondition_format_argumentsVKd, instance);
}

/* CLASS */

extern D LclassGVKd;

bool dylan_class_p (D instance) {
  D class = dylan_object_class(instance);
  return class == LclassGVKd;
}

D dylan_class_debug_name (D instance) {
  return dylan_slot_element(instance, 1);
}

/* FUNCTION */

extern D Lfunction_classGVKi;

bool dylan_function_p (D instance) {
  D class = dylan_object_class(instance);
  D class_class = dylan_object_class(class);
  return class_class == Lfunction_classGVKi;
}

D dylan_function_debug_name (D instance) {
  /*
  return dylan_slot_element(instance, 0));
  */
  ignore(instance);
  return DFALSE;
}

/*
    ENUM DYLAN_TYPE_ENUM
    REPRESENT CRUCIAL BUILT-IN TYPES WITH AN ENUM
 */

enum dylan_type_enum {
  user_defined_type,
  dylan_boolean_type,
  integer_type,
  character_type,
  float_type,
  string_type,
  vector_type,
  pair_type,
  empty_list_type,
  symbol_type,
  simple_condition_type,
  class_type,
  function_type,
  unknown_type
};

static void print_object (STREAM, D, BOOL, int);
static void dylan_format (STREAM, D, D);

static enum dylan_type_enum
dylan_type (D instance) {
  if ((DUMINT)instance & 3) {
    if ((DUMINT)instance & 1) {
      return integer_type;
    } else if ((DUMINT)instance & 2) {
      return character_type;
    } else {
      return unknown_type;
    }
  } else { /* dylan pointer */
    if (dylan_float_p(instance)) {
      return float_type;
    } else if (dylan_boolean_p(instance)) {
      return dylan_boolean_type;
    } else if (dylan_string_p(instance)) {
      return string_type;
    } else if (dylan_vector_p(instance)) {
      return vector_type;
    } else if (dylan_pair_p(instance)) {
      return pair_type;
    } else if (dylan_empty_list_p(instance)) {
      return empty_list_type;
    } else if (dylan_symbol_p(instance)) {
      return symbol_type;
    } else if (dylan_simple_condition_p(instance)) {
      return simple_condition_type;
    } else if (dylan_class_p(instance)) {
      return class_type;
    } else if (dylan_function_p(instance)) {
      return function_type;
    } else {
      return user_defined_type;
    }
  }
}

static void print_integer (STREAM stream, D instance, BOOL escape_p, int print_depth) {
  ignore(print_depth);
  switch (escape_p) {
    case 'D':
      format(stream, "%ld", R(instance)); break;
    case 'X':
      format(stream, "%lx", R(instance)); break;
    case 'O':
      format(stream, "%lo", R(instance)); break;
    default:
      format(stream, "%ld", R(instance)); break;
  }
}

static void print_character (STREAM stream, D instance, BOOL escape_p, int print_depth) {
  ignore(print_depth);
  if (escape_p) {
    format(stream, "'%c'", R(instance))
  } else {
    format(stream, "%c", R(instance));
  }
}

static void print_float (STREAM stream, D instance, BOOL escape_p, int print_depth) {
  ignore(escape_p); ignore(print_depth);
  if (dylan_single_float_p(instance)) {
    format(stream, "%f", dylan_single_float_data(instance))
  } else if (dylan_double_float_p(instance)) {
    format(stream, "%.15f", dylan_double_float_data(instance));
  } else {
    put_string("{unknown float type}", stream);
  }
}

static void print_string (STREAM stream, D instance, BOOL escape_p, int print_depth) {
  ignore(print_depth);
  if (escape_p) {
    format(stream, "\"%s\"", dylan_string_data(instance))
  } else {
    format(stream, "%s", dylan_string_data(instance));
  }
}

static void print_string_data (STREAM stream, D instance, BOOL escape_p, int print_depth) {
  ignore(escape_p); ignore(print_depth);
  format(stream, "%s", dylan_string_data(instance));
}

#define MAX(x, y) ((x) > (y) ? (x) : (y))
#define MIN(x, y) ((x) < (y) ? (x) : (y))

static void print_vector (STREAM stream, D instance, BOOL escape_p, int print_depth) {
  int size = vector_size(instance), i = 0;
  bool first = true;
  D element;
  int max_size = MIN(size, dylan_print_length);

  put_string("#[", stream);
  if (print_depth < dylan_print_depth) {
    for (; i < max_size; i++) {
      if (first) {
        first = false;
      } else {
        put_string(", ", stream);
      }
      element = vector_ref(instance, i);
      print_object(stream, element, escape_p, print_depth + 1);
    }
  }
  if (size > max_size || print_depth >= dylan_print_depth) {
    if (i > 0) {
      put_string(", ", stream);
    }
    format(stream, "... 0x%lx", instance);
  }
  put_string("]", stream);
}

static void print_pair (STREAM stream, D instance, BOOL escape_p, int print_depth) {
  D head = dylan_head(instance);
  D tail = dylan_tail(instance);
  enum dylan_type_enum type;
  int i = 0;
  bool first = true;

  put_string("#(", stream);
  if (print_depth < dylan_print_depth) {
    for (; i<dylan_print_length; i++) {
      if (first) {
        first = false;
      } else {
        put_string(", ", stream);
      }
      print_object(stream, head, escape_p, print_depth + 1);
      type = dylan_type(tail);
      switch (type) {
        case pair_type:
          head = dylan_head(tail);
          tail = dylan_tail(tail);
          continue;
        case empty_list_type:
          goto done;
        default:
          put_string(" . ", stream);
          print_object(stream, tail, escape_p, print_depth + 1);
          goto done;
      }
    }
  }
  if (i > 0) {
    put_string(", ", stream);
  }
  format(stream, "... 0x%lx", instance);
done:
  put_string(")", stream);
}

static void print_empty_list (STREAM stream, D instance, BOOL escape_p, int print_depth) {
  ignore(instance); ignore(escape_p); ignore(print_depth);
  put_string("#()", stream);
}

static void print_symbol_name (STREAM stream, D instance, BOOL escape_p, int print_depth) {
  ignore(escape_p);
  print_object(stream, dylan_symbol_name(instance), TRUE, print_depth);
}

static void print_symbol (STREAM stream, D instance, BOOL escape_p, int print_depth) {
  ignore(escape_p);
  put_string("#", stream);
  print_symbol_name(stream, instance, TRUE, print_depth);
}

static void print_boolean (STREAM stream, D instance, BOOL escape_p, int print_depth) {
  ignore(escape_p); ignore(print_depth);
  if (dylan_true_p(instance)) {
    put_string("#t", stream);
  } else {
    put_string("#f", stream);
  }
}

static void print_simple_condition (STREAM stream, D instance, BOOL escape_p, int print_depth) {
  D format_string = dylan_simple_condition_format_string(instance);
  D format_args = dylan_simple_condition_format_args(instance);
  ignore(print_depth);
  if (escape_p) put_char('"', stream);
  dylan_format(stream, format_string, format_args);
  if (escape_p) put_char('"', stream);
}

static void print_class_debug_name (STREAM stream, D instance, BOOL escape_p, int print_depth) {
  D name = dylan_class_debug_name(instance);
  ignore(escape_p);
  print_string_data(stream, name, TRUE, print_depth);
}

static void print_class (STREAM stream, D instance, BOOL escape_p, int print_depth) {
  ignore(escape_p);
  put_string("{class ", stream);
  print_class_debug_name(stream, instance, TRUE, print_depth);
  format(stream, " 0x%lx}", instance);
}

static void print_function (STREAM stream, D instance, BOOL escape_p, int print_depth) {
  ignore(escape_p); ignore(print_depth);
  /*
  D name = dylan_function_debug_name(instance);
  */
  put_string("{function ", stream);
  /*
  print_string_data(stream, name, TRUE, print_depth);
  */
  format(stream, " 0x%lx}", instance);
}

static void print_user_defined (STREAM stream, D instance, BOOL escape_p, int print_depth) {
  D class = dylan_object_class(instance);
  ignore(escape_p);
  put_string("{", stream);
  print_class_debug_name(stream, class, TRUE, print_depth);
  format(stream, " 0x%lx}", instance);
}

static void print_object (STREAM stream, D instance, BOOL escape_p, int print_depth) {
  enum dylan_type_enum type = dylan_type(instance);
  switch (type) {
      print_integer(stream, instance, escape_p, print_depth); break;
    case character_type:
      print_character(stream, instance, escape_p, print_depth); break;
    case float_type:
      print_float (stream, instance, escape_p, print_depth); break;
    case dylan_boolean_type:
      print_boolean(stream, instance, escape_p, print_depth); break;
    case string_type:
      print_string (stream, instance, escape_p, print_depth); break;
    case vector_type:
      print_vector(stream, instance, escape_p, print_depth); break;
    case pair_type:
      print_pair(stream, instance, escape_p, print_depth); break;
    case empty_list_type:
      print_empty_list(stream, instance, escape_p, print_depth); break;
    case symbol_type:
      print_symbol(stream, instance, escape_p, print_depth); break;
    case simple_condition_type:
      print_simple_condition(stream, instance, escape_p, print_depth); break;
    case class_type:
      print_class(stream, instance, escape_p, print_depth); break;
    case function_type:
      print_function(stream, instance, escape_p, print_depth); break;
    case unknown_type:
      format(stream, "?%lx", instance); break;
    default:
      print_user_defined(stream, instance, escape_p, print_depth); break;
  }
}

static void dylan_format (STREAM stream, D dylan_string, D dylan_arguments) {
  BOOL  percent_p = false;
  char* string = dylan_string_data(dylan_string);
  D*    arguments = vector_data(dylan_arguments);
  int   argument_count = vector_size(dylan_arguments),
        argument_index = 0,
        size = (int)strlen(string),
        i;
  for (i = 0; i < size; i++) {
    char c = string[i];
    if (percent_p) {
      char cc = (char)toupper(c);
      switch (cc) {
        case 'S': case 'C':
          if (argument_index < argument_count) {
            print_object(stream, arguments[argument_index++], FALSE, 0);
          } else {
            put_string("**MISSING**", stream);
          }
          break;
        case '=':
          if (argument_index < argument_count) {
            print_object(stream, arguments[argument_index++], TRUE, 0);
          } else {
            put_string("**MISSING**", stream);
          }
          break;
        case 'D': case 'X': case 'O': case 'B':
          if (argument_index < argument_count) {
            print_object(stream, arguments[argument_index++], (BOOL)cc, 0);
          } else {
            put_string("**MISSING**", stream);
          }
          break;
        case '%':
          put_char('%', stream); break;
        default: ;
      }
      percent_p = false;
    } else if (c == '%') {
      percent_p = true;
    } else {
      put_char(c, stream);
    }
  }
}

static void do_debug_message (D string, D arguments) {
  char error_output[8192];
  error_output[0] = 0;

  dylan_format(error_output, string, arguments);
#ifdef OPEN_DYLAN_PLATFORM_WINDOWS
  {
    #define $STD_OUTPUT_HANDLE (unsigned long)-11
    #define $INVALID_HANDLE_VALUE (void*)-1
    extern void* __stdcall GetStdHandle(unsigned long);
    extern BOOL __stdcall WriteFile(void*, char*, unsigned long, unsigned long*, void*);
    extern void __stdcall OutputDebugStringA(char*);
    void* stdoutHandle = GetStdHandle($STD_OUTPUT_HANDLE);
    put_char('\n', error_output);
    if ((stdoutHandle != $INVALID_HANDLE_VALUE) && (stdoutHandle != (void*)0)) {
      unsigned long nBytes = strlen(error_output);
      WriteFile(stdoutHandle, error_output, nBytes, &nBytes, (void*)0);
    }
    OutputDebugStringA(error_output);
  }
#else
  fputs(error_output, stderr);
  fputs("\n", stderr); /* Adds a terminating newline */
  fflush(stderr);
#endif
}

void dylan_print_object (D object) {
  char output[8192];
  output[0] = 0;

  print_object(output, object, TRUE, 0);

  fputs(output, stdout);
  fputs("\n", stdout);
  fflush(stdout);
}

void primitive_invoke_debugger (D string, D arguments) {
  do_debug_message(string, arguments);
  primitive_break();
}

D primitive_inside_debuggerQ (void) {
  return DFALSE;
}

void primitive_debug_message (D string, D arguments) {
  do_debug_message(string, arguments);
}
