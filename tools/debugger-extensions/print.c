#include "dylan.h" 

/*
CORE_ADDR values[MAX_VALUES];
int values_index = 0;
int max_values_index = 0;

void add_value (CORE_ADDR value) {
  values[values_index++] = value;
  values_index = (values_index > MAX_VALUES) ? 0 : values_index;
  max_values_index
    = (max_values_index > values_index) ? max_values_index : values_index;
  return(values_index - 1);
}
*/

#include "value.h"
#include "dylan-lang.h"

int add_value (CORE_ADDR value) {
  value_ptr val = value_from_longest(lookup_pointer_type(builtin_type_void), value);
  int histindex = record_latest_value (val);

  if (histindex >= 0)
    annotate_value_history_begin (histindex, VALUE_TYPE (val));
  else
    annotate_value_begin (VALUE_TYPE (val));

  if (histindex >= 0)
    annotate_value_history_value ();

  if (histindex >= 0)
    annotate_value_history_end ();
  else
    annotate_value_end ();

  return(histindex);
}

/* 
   MAYBE_FIND_SYMBOLIC_NAME
   RETURNS VARIABLE NAME CORRESPONDING TO ADDRESS IF GLOBAL-P AND EXISTS
*/

char *
maybe_find_any_symbolic_name (CORE_ADDR instance) {
  CORE_ADDR location;
  char *name = find_closest_symbolic_name (instance, &location);
  if (name && location == instance) 
    return(name);
  else
    return(NULL);
}

char *
maybe_find_symbolic_name (CORE_ADDR instance) {
  CORE_ADDR location;
  char *name = maybe_find_any_symbolic_name (instance);
  if (name && global_dylan_mangled_name_p(name)) 
    return(name);
  else
    return(NULL);
}

/*
 * DYLAN PRINTING
 */

/*
    ENUM DYLAN_TYPE_ENUM
    REPRESENT CRUCIAL BUILT-IN TYPES WITH AN ENUM
 */

enum dylan_type_enum {
  user_defined_type,
  dylan_boolean_type,
  big_integer_type,
  machine_integer_type,
  unsigned_machine_integer_type,
  single_float_type,
  string_type,
  vector_type,
  pair_type,
  empty_list_type,
  symbol_type,
  class_type,
  implementation_class_type,
  open_class_type,
  sealed_class_type,
  instance_slot_descriptor_type,
  repeated_slot_descriptor_type,
  method_type,
  complex_method_type,
  slot_method_type,
  generic_function_type
};

/*
    LOOKUP_DYLAN_TYPE_ENUM
    RETURN DYLAN_TYPE_ENUM FOR GIVEN ADDRESS OF INSTANCE-HEADER
 */

boolean dylan_address_p (CORE_ADDR address, char *variable_name) {
  char *name = maybe_find_symbolic_name (address);
  if (name) {
    char demangled_name[1000], library_name[1000], module_name[1000];
    
    demangle_dylan_name_exploded
      (name, library_name, module_name, 
       variable_name, demangled_name, false);
       
    return (!strcmp(library_name, "dylan"));
  } else
    return false;
}

enum dylan_type_enum
lookup_dylan_type_enum (CORE_ADDR instance) {
  char name[1000];
  if (dylan_address_p(dylan_object_class(instance), name)) {
	 if (!strcmp(name, "<big-integer>"))
      return(big_integer_type);
    else if (!strcmp(name, "<machine-integer>"))
      return(machine_integer_type);
    else if (!strcmp(name, "<unsigned-machine-integer>"))
      return(unsigned_machine_integer_type);
    else if (!strcmp(name, "<single-float>"))
      return(single_float_type);
    else if (!strcmp(name, "<byte-string>"))
      return(string_type);
    else if (!strcmp(name, "<simple-object-vector>"))
      return(vector_type);
    else if (!strcmp(name, "<pair>"))
      return(pair_type);
    else if (!strcmp(name, "<empty-list>"))
      return(empty_list_type);
    else if (!strcmp(name, "<symbol>"))
      return(symbol_type);
    else if (!strcmp(name, "<class>"))
      return(class_type);
    else if (!strcmp(name, "<implementation-class>"))
      return(implementation_class_type);
    else if (!strcmp(name, "<open-class>"))
      return(class_type);
    else if (!strcmp(name, "<sealed-class>"))
      return(class_type);
    else if (!strcmp(name, "<instance-slot-descriptor>"))
      return(instance_slot_descriptor_type);
    else if (!strcmp(name, "<repeated-slot-descriptor>"))
      return(repeated_slot_descriptor_type);
    else if (!strcmp(name, "<method>"))
      return(method_type);
    else if (!strcmp(name, "<slot-method>"))
      return(method_type);
    else if (!strcmp(name, "<complex-method>"))
      return(method_type);
    else if (!strcmp(name, "<generic-function>"))
      return(generic_function_type);
    else if (!strcmp(name, "<boolean>"))
      return(dylan_boolean_type);
    else
      return(user_defined_type);
  } else
   return(user_defined_type);
}

void 
print_dylan_integer
  (FILE *stream, CORE_ADDR instance, boolean verbose_p, int print_depth) 
{
  fprintf_filtered(stream, "%ld", instance >> 2);
}

void 
print_dylan_character
  (FILE *stream, CORE_ADDR instance, boolean verbose_p, int print_depth) 
{
  if (verbose_p)
    fprintf_filtered(stream, "'%c'", instance >> 2);
  else
    fprintf_filtered(stream, "%c", instance >> 2);
}

void
print_dylan_machine_integer
  (FILE* stream, CORE_ADDR instance, boolean verbose_p, int print_depth) 
{
  fprintf_filtered(stream, "%ld", dylan_machine_integer_data(instance));
}

void
print_dylan_unsigned_machine_integer
  (FILE* stream, CORE_ADDR instance, boolean verbose_p, int print_depth) 
{
  fprintf_filtered(stream, "%0x%lx", dylan_unsigned_machine_integer_data(instance));
}

void
print_dylan_big_integer
  (FILE* stream, CORE_ADDR instance, boolean verbose_p, int print_depth) 
{
  print_dylan_machine_integer(stream, instance, verbose_p, print_depth);
}

void
print_dylan_single_float
  (FILE* stream, CORE_ADDR instance, boolean verbose_p, int print_depth) 
{
  fprintf_filtered(stream, "%f", dylan_single_float_data(instance));
}

void
print_dylan_string
  (FILE* stream, CORE_ADDR instance, boolean verbose_p, int print_depth) 
{
  int size = dylan_string_size(instance);
  char string[1000], *s = string;

  if (size > 1000)
    s = malloc(size + 1);
  read_memory(dylan_string_data_address(instance), s, size);
  s[size] = (char)0;
  if (verbose_p)
    fprintf_filtered(stream, "\"%s\"", s);
  else
    fprintf_filtered(stream, "%s", s);
  if (size > 1000)
    free(s);
}

#define MAX(x, y) ((x) > (y) ? (x) : (y))
#define MIN(x, y) ((x) < (y) ? (x) : (y))

void
print_dylan_vector
  (FILE* stream, CORE_ADDR instance, boolean verbose_p, int print_depth) 
{
  int size = dylan_vector_size(instance);
  int first = true, i = 0;
  CORE_ADDR element;
  int max_size = MIN(size, dylan_print_length);

  fputs_filtered("#[", stream);
  if (print_depth < dylan_print_depth) {
    for (; i<max_size; i++) {
      if (first) 
	first = false;
      else
	fputs_filtered(", ", stream);
      element = dylan_vector_element(instance, i);
      print_dylan_object(stream, element, verbose_p, print_depth + 1);
    }
  }
  if (size > max_size || print_depth >= dylan_print_depth) {
    if (i > 0)
      fputs_filtered(", ", stream);
    fprintf_filtered(stream, "... 0x%lx", instance);
  }
  fputs_filtered("]", stream);
}

void
print_dylan_pair
  (FILE* stream, CORE_ADDR instance, boolean verbose_p, int print_depth) 
{
  CORE_ADDR head = dylan_head(instance);
  CORE_ADDR tail = dylan_tail(instance);
  CORE_ADDR instance_header;
  enum dylan_type_enum type;
  int first = true, i = 0;

  fputs_filtered("#(", stream);
  if (print_depth < dylan_print_depth) {
    for (; i<dylan_print_length; i++) {
      if (first) 
	first = false;
      else
	fputs_filtered(", ", stream);
      print_dylan_object(stream, head, verbose_p, print_depth + 1);
      type = lookup_dylan_type_enum(tail);
      switch (type) {
	case pair_type:       
	  head = dylan_head(tail);
	  tail = dylan_tail(tail);
	  continue;
	case empty_list_type: 
	  goto done;
	default:            
	  fputs_filtered(" . ", stream);
	  print_dylan_object(stream, tail, verbose_p, print_depth + 1);
	  goto done;
      }
    }
  }
  if (i > 0)
    fputs_filtered(", ", stream);
  fprintf_filtered(stream, "... 0x%lx", instance);
done:
  fputs_filtered(")", stream);
}

void
print_dylan_empty_list
  (FILE* stream, CORE_ADDR instance, boolean verbose_p, int print_depth) 
{
  fputs_filtered("#()", stream);
}

void
print_dylan_symbol
  (FILE* stream, CORE_ADDR instance, boolean verbose_p, int print_depth) 
{
  CORE_ADDR name = dylan_symbol_name(instance);
  if (verbose_p)
    fputs_filtered("#", stream);
  print_dylan_object(stream, name, verbose_p, print_depth);
}

void
print_dylan_boolean
  (FILE* stream, CORE_ADDR instance, boolean verbose_p, int print_depth) 
{
  char *name = maybe_find_any_symbolic_name(instance);
  if (name) {
    char demangled_name[1000], library_name[1000], module_name[1000];
    char variable_name[1000];
    
    demangle_dylan_name_exploded
      (name, library_name, module_name, 
       variable_name, demangled_name, false);
       
    if (!strcmp(variable_name, "%true"))
      fputs_filtered("#t", stream);
    else if (!strcmp(variable_name, "%false"))
      fputs_filtered("#f", stream);
    else
      error("Unknown Boolean %s\n", demangled_name);
  } else
    error("Unknown Boolean %lx\n", instance);
}

void
print_dylan_class_debug_name
  (FILE* stream, CORE_ADDR instance, boolean verbose_p, int print_depth) 
{
  char *name = maybe_find_symbolic_name (instance);
  if (name) {
    char demangled[100];
    fputs_filtered(demangle_dylan_name (name, demangled, 0), stream);
  } else {
    CORE_ADDR name = dylan_class_debug_name(instance);
    print_dylan_object(stream, name, false, print_depth);
  }
}

void
print_dylan_class
  (FILE* stream, CORE_ADDR instance, boolean verbose_p, int print_depth) 
{
  fputs_filtered("{class ", stream);
  print_dylan_class_debug_name(stream, instance, false, print_depth);
  fprintf_filtered(stream, " 0x%lx}", instance);
}

void
print_dylan_implementation_class
  (FILE* stream, CORE_ADDR instance, boolean verbose_p, int print_depth)
{
  CORE_ADDR theclass = dylan_implementation_class_class(instance);
  fputs_filtered("{implementation-class ", stream);
  if (!dylan_class_p(theclass)) {
    fputs_filtered("BOGUS! ", stream);
  } else {
    if (instance != dylan_class_implementation_class(theclass))
      fputs_filtered("(obsolete) ", stream);
    print_dylan_class_debug_name(stream, theclass, false, print_depth);
  };
  fprintf_filtered(stream, " 0x%lx}", instance);
}

void
print_dylan_function_debug_name
  (FILE* stream, CORE_ADDR instance, boolean verbose_p, int print_depth) 
{
  char *name = maybe_find_symbolic_name (instance);
  if (name) {
    char demangled[MAX_NAME_SIZE];
    fputs_filtered(demangle_dylan_name (name, demangled, 0), stream);
  } else {
    CORE_ADDR name = dylan_function_debug_name(instance);
    print_dylan_object(stream, name, false, print_depth);
  }
}

void
print_dylan_method
  (FILE* stream, CORE_ADDR instance, boolean verbose_p, int print_depth) 
{
  fputs_filtered("{method ", stream);
  print_dylan_function_debug_name(stream, instance, false, print_depth);
  fprintf_filtered(stream, " 0x%lx}", instance);
}

void
print_dylan_generic_function
  (FILE* stream, CORE_ADDR instance, boolean verbose_p, int print_depth) 
{
  fputs_filtered("{generic-function ", stream);
  print_dylan_function_debug_name(stream, instance, false, print_depth);
  fprintf_filtered(stream, " 0x%lx}", instance);
}

/*
 * DESCRIBE
 */

static void
describe_slot_value 
    (FILE* stream, 
     CORE_ADDR instance, int offset, CORE_ADDR slot_descriptor,
     boolean verbose_p, int print_depth) {
  CORE_ADDR slot_getter = dylan_slot_getter(slot_descriptor);
  CORE_ADDR slot_value = dylan_slot_element(instance, offset);
  fprintf_filtered(stream, "$%d = \t", add_value(slot_value));
  print_dylan_function_debug_name(stream, slot_getter, false, 0);
  fprintf_filtered(stream, ": ");
  print_dylan_object(stream, slot_value, verbose_p, print_depth);
  fprintf_filtered(stream, "\n");
}
 
void
describe_dylan_object
  (FILE* stream, CORE_ADDR instance, boolean verbose_p, int print_depth) 
{
  if (instance & 3) { /* tagged? */
    if (instance & 1)
      print_dylan_integer(stream, instance, verbose_p, print_depth);
    else if (instance & 2)
      print_dylan_character(stream, instance, verbose_p, print_depth);
    else
      fprintf_filtered(stream, "?%lx", instance);
    fprintf_filtered(stream, "\n");
  } else { /* dylan pointer */
    CORE_ADDR iclass = dylan_object_implementation_class(instance);
    CORE_ADDR slot_descriptors = dylan_implementation_class_instance_slot_descriptors(iclass);
    CORE_ADDR repeated_slot_descriptor = dylan_implementation_class_repeated_slot_descriptor(iclass);
    int size = dylan_vector_size(slot_descriptors);
    int i;

    size = MIN(size, dylan_print_length);
    fprintf_filtered(stream, "$%d = \t", add_value(iclass));
    print_dylan_object(stream, iclass, verbose_p, 0);
    fprintf_filtered(stream, "\n");
    for (i=0; i<size; i++) {
      CORE_ADDR slot_descriptor = dylan_vector_element(slot_descriptors, i);
      describe_slot_value
	(stream, instance, i, slot_descriptor, verbose_p, print_depth);
    }
    if (lookup_dylan_type_enum(repeated_slot_descriptor) != dylan_boolean_type 
        && lookup_dylan_type_enum(instance) != string_type) {
      int size_offset = size - 1;
      int repeated_size = dylan_slot_element(instance, size_offset) >> 2;
      int k = 0;
      describe_slot_value
	(stream, instance, size_offset, repeated_slot_descriptor, verbose_p, print_depth);
      if ((size + repeated_size) > dylan_print_length)
        repeated_size = MAX(dylan_print_length - size, 0);
      for (i = size_offset + 1; k < repeated_size; k++, i++) {
        CORE_ADDR slot_value = dylan_slot_element(instance, i);
        fprintf_filtered(stream, "$%d = \t%2d: ", add_value(slot_value), k);
        print_dylan_object(stream, slot_value, verbose_p, print_depth);
        fprintf_filtered(stream, "\n");
      }
    }
  }
}

void
print_dylan_user_defined
  (FILE* stream, CORE_ADDR instance, boolean verbose_p, int print_depth) 
{
  CORE_ADDR class = dylan_object_class(instance);
  fputs_filtered("{", stream);
  print_dylan_class_debug_name(stream, class, false, print_depth);
  fprintf_filtered(stream, " 0x%lx}", instance);
}

void
print_dylan_object 
  (FILE* stream, CORE_ADDR instance, boolean verbose_p, int print_depth) 
{
  if (!dylan_object_p(instance))
    fprintf_filtered(stream, "?0x%lx", instance);
  else if (instance & 3) { /* tagged? */
    if (instance & 1)
      print_dylan_integer(stream, instance, verbose_p, print_depth);
    else if (instance & 2)
      print_dylan_character(stream, instance, verbose_p, print_depth);
    else
      fprintf_filtered(stream, "?%lx", instance);
  } else { /* dylan pointer */
    char *name;
    name = maybe_find_symbolic_name (instance);
    if (0 /* name */) {
      char demangled[MAX_NAME_SIZE];
      fputs_filtered(demangle_dylan_name (name, demangled, 0), stream);
    } else {
      enum dylan_type_enum type;
      type = lookup_dylan_type_enum(instance);
      switch (type) {
	case big_integer_type: 
	  print_dylan_big_integer
            (stream, instance, verbose_p, print_depth); break; 
        case machine_integer_type:
	  print_dylan_machine_integer
            (stream, instance, verbose_p, print_depth); break;
	case unsigned_machine_integer_type:
	  print_dylan_unsigned_machine_integer
            (stream, instance, verbose_p, print_depth); break;
	case single_float_type:
	  print_dylan_single_float
            (stream, instance, verbose_p, print_depth); break;
	case string_type:
	  print_dylan_string
            (stream, instance, verbose_p, print_depth); break; 
        case vector_type:
	  print_dylan_vector(stream, instance, verbose_p, print_depth); break;
	case pair_type:
	  print_dylan_pair(stream, instance, verbose_p, print_depth); break;
	case empty_list_type:
	  print_dylan_empty_list(stream, instance, verbose_p, print_depth); break;
	case symbol_type:
	  print_dylan_symbol(stream, instance, verbose_p, print_depth); break;
	case class_type:
	  print_dylan_class(stream, instance, verbose_p, print_depth); break;
        case implementation_class_type:
	  print_dylan_implementation_class(stream, instance, verbose_p, print_depth); break;
	case method_type:
	  print_dylan_method(stream, instance, verbose_p, print_depth); break;
	case generic_function_type:
	  print_dylan_generic_function(stream, instance, verbose_p, print_depth); break;
	case dylan_boolean_type:
	  print_dylan_boolean(stream, instance, verbose_p, print_depth); break;
	case user_defined_type: 
	case instance_slot_descriptor_type: 
	case repeated_slot_descriptor_type: 
	default:
	  print_dylan_user_defined
            (stream, instance, verbose_p, print_depth); break;
      }
    }
  }
}

void
fprintf_dylan_symbol_filtered (FILE *stream, char *name, boolean localp)
{
  if (!localp && global_dylan_mangled_name_p(name)) {
    char demangled[1000];
    demangle_dylan_name_in_library_module(name, demangled, "", "");
    fputs_filtered (demangled, stream);
    free (demangled);
  } else
    fputs_filtered (name, stream);
}


