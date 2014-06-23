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

dylan_value dylan_slot_element (dylan_value instance, int offset) {
  return ((dylan_object*)instance)->slots[offset];
}

dylan_value dylan_object_class (dylan_value instance) {
  return (dylan_value)OBJECT_CLASS(instance);
}

/* BOOLEAN */

extern dylan_value LbooleanGVKd;
extern dylan_object KPfalseVKi;
extern dylan_object KPtrueVKi;

bool dylan_boolean_p (dylan_value instance) {
  return dylan_object_class(instance) == LbooleanGVKd;
}

bool dylan_true_p (dylan_value instance) {
  return instance == DTRUE;
}

/* FLOAT */

extern dylan_value Lsingle_floatGVKd;
extern dylan_value Ldouble_floatGVKd;

bool dylan_float_p (dylan_value instance) {
  return dylan_object_class(instance) == Lsingle_floatGVKd ||
         dylan_object_class(instance) == Ldouble_floatGVKd;
}

bool dylan_single_float_p (dylan_value instance) {
  return dylan_object_class(instance) == Lsingle_floatGVKd;
}

float
dylan_single_float_data (dylan_value instance) {
  return ((dylan_single_float*)instance)->data;
}

bool dylan_double_float_p (dylan_value instance) {
  return dylan_object_class(instance) == Ldouble_floatGVKd;
}

double
dylan_double_float_data (dylan_value instance) {
  return ((dylan_double_float*)instance)->data;
}

/* SYMBOL */

extern dylan_value LsymbolGVKd;

bool dylan_symbol_p (dylan_value instance) {
  return dylan_object_class(instance) == LsymbolGVKd;
}

dylan_value dylan_symbol_name (dylan_value instance) {
  return dylan_slot_element(instance, 0);
}

/* PAIR */

extern dylan_value LpairGVKd;
extern dylan_value Lempty_listGVKd;

bool dylan_pair_p (dylan_value instance) {
  return dylan_object_class(instance) == LpairGVKd;
}

bool dylan_empty_list_p (dylan_value instance) {
  return dylan_object_class(instance) == Lempty_listGVKd;
}

dylan_value dylan_head (dylan_value instance) {
  return dylan_slot_element(instance, 0);
}

dylan_value dylan_tail (dylan_value instance) {
  return dylan_slot_element(instance, 1);
}

/* VECTOR */

extern dylan_value  Lsimple_object_vectorGVKd;
extern dylan_value  vector_ref (dylan_simple_object_vector* vector, int offset);
extern dylan_value* vector_data (dylan_simple_object_vector* vector);
extern int vector_size (dylan_simple_object_vector* vector);

bool dylan_vector_p (dylan_value instance) {
  return dylan_object_class(instance) == Lsimple_object_vectorGVKd;
}

/* STRING */

#include <string.h>

extern dylan_value Lbyte_stringGVKd;

bool dylan_string_p (dylan_value instance) {
  return dylan_object_class(instance) == Lbyte_stringGVKd;
}

char* dylan_string_data (dylan_value instance) {
  return ((dylan_byte_string*)instance)->data;
}

/* SIMPLE-CONDITION */

extern dylan_value Lsimple_conditionGVKe;
extern dylan_simple_method KinstanceQVKd;
extern dylan_simple_method Kcondition_format_stringVKd;
extern dylan_simple_method Kcondition_format_arguments_vectorVKi;

bool dylan_simple_condition_p (dylan_value instance) {
  return DTRUE == CALL2(&KinstanceQVKd, instance, Lsimple_conditionGVKe);
}

dylan_value dylan_simple_condition_format_string (dylan_value instance) {
  return CALL1(&Kcondition_format_stringVKd, instance);
}

dylan_simple_object_vector* dylan_simple_condition_format_args (dylan_value instance) {
  return (dylan_simple_object_vector*)CALL1(&Kcondition_format_arguments_vectorVKi, instance);
}

/* CLASS */

extern dylan_value LclassGVKd;

bool dylan_class_p (dylan_value instance) {
  dylan_value class = dylan_object_class(instance);
  return class == LclassGVKd;
}

dylan_value dylan_class_debug_name (dylan_value instance) {
  return dylan_slot_element(instance, 1);
}

/* FUNCTION */

extern dylan_value Lfunction_classGVKi;

bool dylan_function_p (dylan_value instance) {
  dylan_value class = dylan_object_class(instance);
  dylan_value class_class = dylan_object_class(class);
  return class_class == Lfunction_classGVKi;
}

dylan_value dylan_function_debug_name (dylan_value instance) {
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

static void print_object (STREAM, dylan_value, BOOL, int);
void dylan_format (STREAM, dylan_value, dylan_simple_object_vector*);

static enum dylan_type_enum
dylan_get_type (dylan_value instance) {
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

static void print_integer (STREAM stream, dylan_value instance, BOOL escape_p, int print_depth) {
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

static void print_character (STREAM stream, dylan_value instance, BOOL escape_p, int print_depth) {
  ignore(print_depth);
  if (escape_p) {
    format(stream, "'%c'", R(instance))
  } else {
    format(stream, "%c", R(instance));
  }
}

static void print_float (STREAM stream, dylan_value instance, BOOL escape_p, int print_depth) {
  ignore(escape_p); ignore(print_depth);
  if (dylan_single_float_p(instance)) {
    format(stream, "%f", dylan_single_float_data(instance))
  } else if (dylan_double_float_p(instance)) {
    format(stream, "%.15f", dylan_double_float_data(instance));
  } else {
    put_string("{unknown float type}", stream);
  }
}

static void print_string (STREAM stream, dylan_value instance, BOOL escape_p, int print_depth) {
  ignore(print_depth);
  if (escape_p) {
    format(stream, "\"%s\"", dylan_string_data(instance))
  } else {
    format(stream, "%s", dylan_string_data(instance));
  }
}

static void print_string_data (STREAM stream, dylan_value instance, BOOL escape_p, int print_depth) {
  ignore(escape_p); ignore(print_depth);
  format(stream, "%s", dylan_string_data(instance));
}

#define MAX(x, y) ((x) > (y) ? (x) : (y))
#define MIN(x, y) ((x) < (y) ? (x) : (y))

static void print_vector (STREAM stream, dylan_simple_object_vector* instance, BOOL escape_p, int print_depth) {
  int size = vector_size(instance), i = 0;
  bool first = true;
  dylan_value element;
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

static void print_pair (STREAM stream, dylan_value instance, BOOL escape_p, int print_depth) {
  dylan_value head = dylan_head(instance);
  dylan_value tail = dylan_tail(instance);
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
      type = dylan_get_type(tail);
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

static void print_empty_list (STREAM stream, dylan_value instance, BOOL escape_p, int print_depth) {
  ignore(instance); ignore(escape_p); ignore(print_depth);
  put_string("#()", stream);
}

static void print_symbol_name (STREAM stream, dylan_value instance, BOOL escape_p, int print_depth) {
  ignore(escape_p);
  print_object(stream, dylan_symbol_name(instance), TRUE, print_depth);
}

static void print_symbol (STREAM stream, dylan_value instance, BOOL escape_p, int print_depth) {
  ignore(escape_p);
  put_string("#", stream);
  print_symbol_name(stream, instance, TRUE, print_depth);
}

static void print_boolean (STREAM stream, dylan_value instance, BOOL escape_p, int print_depth) {
  ignore(escape_p); ignore(print_depth);
  if (dylan_true_p(instance)) {
    put_string("#t", stream);
  } else {
    put_string("#f", stream);
  }
}

static void print_simple_condition (STREAM stream, dylan_value instance, BOOL escape_p, int print_depth) {
  dylan_value format_string = dylan_simple_condition_format_string(instance);
  dylan_simple_object_vector* format_args = dylan_simple_condition_format_args(instance);
  ignore(print_depth);
  if (escape_p) put_char('"', stream);
  dylan_format(stream, format_string, format_args);
  if (escape_p) put_char('"', stream);
}

static void print_class_debug_name (STREAM stream, dylan_value instance, BOOL escape_p, int print_depth) {
  dylan_value name = dylan_class_debug_name(instance);
  ignore(escape_p);
  print_string_data(stream, name, TRUE, print_depth);
}

static void print_class (STREAM stream, dylan_value instance, BOOL escape_p, int print_depth) {
  ignore(escape_p);
  put_string("{class ", stream);
  print_class_debug_name(stream, instance, TRUE, print_depth);
  format(stream, " 0x%lx}", instance);
}

static void print_function (STREAM stream, dylan_value instance, BOOL escape_p, int print_depth) {
  ignore(escape_p); ignore(print_depth);
  /*
  dylan_value name = dylan_function_debug_name(instance);
  */
  put_string("{function ", stream);
  /*
  print_string_data(stream, name, TRUE, print_depth);
  */
  format(stream, " 0x%lx}", instance);
}

static void print_user_defined (STREAM stream, dylan_value instance, BOOL escape_p, int print_depth) {
  dylan_value class = dylan_object_class(instance);
  ignore(escape_p);
  put_string("{", stream);
  print_class_debug_name(stream, class, TRUE, print_depth);
  format(stream, " 0x%lx}", instance);
}

typedef void (*DEBUG_PRINT_FUNCPTR)(STREAM, dylan_value, BOOL, int);
static void print_object (STREAM stream, dylan_value instance, BOOL escape_p, int print_depth) {
  enum dylan_type_enum type = dylan_get_type(instance);
  static DEBUG_PRINT_FUNCPTR printers[] = {
    [integer_type]          = (DEBUG_PRINT_FUNCPTR)print_integer,
    [character_type]        = (DEBUG_PRINT_FUNCPTR)print_character,
    [float_type]            = (DEBUG_PRINT_FUNCPTR)print_float,
    [dylan_boolean_type]    = (DEBUG_PRINT_FUNCPTR)print_boolean,
    [string_type]           = (DEBUG_PRINT_FUNCPTR)print_string,
    [vector_type]           = (DEBUG_PRINT_FUNCPTR)print_vector,
    [pair_type]             = (DEBUG_PRINT_FUNCPTR)print_pair,
    [empty_list_type]       = (DEBUG_PRINT_FUNCPTR)print_empty_list,
    [symbol_type]           = (DEBUG_PRINT_FUNCPTR)print_symbol,
    [simple_condition_type] = (DEBUG_PRINT_FUNCPTR)print_simple_condition,
    [class_type]            = (DEBUG_PRINT_FUNCPTR)print_class,
    [function_type]         = (DEBUG_PRINT_FUNCPTR)print_function,
    [user_defined_type]     = (DEBUG_PRINT_FUNCPTR)print_user_defined
  };
  if (type == unknown_type) {
    format(stream, "?%lx", instance);
  } else {
    (printers[type])(stream, instance, escape_p, print_depth);
  }
}

void dylan_format (STREAM stream, dylan_value dylan_string, dylan_simple_object_vector* dylan_arguments) {
  BOOL  percent_p = false;
  char* string = dylan_string_data(dylan_string);
  dylan_value*    arguments = vector_data(dylan_arguments);
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

void dylan_print_object (dylan_value object) {
  char output[8192];
  output[0] = 0;

  print_object(output, object, TRUE, 0);

  fputs(output, stdout);
  fputs("\n", stdout);
  fflush(stdout);
}

