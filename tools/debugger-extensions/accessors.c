#include "dylan.h"

/*
 *  LOW-LEVEL ACCESSORS
 */

CORE_ADDR
dylan_slot_element (CORE_ADDR instance, int slot_offset) {
  CORE_ADDR slot;
  int status = 
    target_read_memory
      (instance + DYLAN_POINTER_SIZE * (slot_offset + 1), &slot, DYLAN_POINTER_SIZE);
  return(slot);
}

CORE_ADDR
dylan_slot_element_probe (CORE_ADDR instance, int slot_offset, CORE_ADDR *slot) {
  int status = 
    target_read_memory
      (instance + DYLAN_POINTER_SIZE * (slot_offset + 1), slot, DYLAN_POINTER_SIZE);
  return(status == 0);
}

long
dylan_machine_integer_data(CORE_ADDR instance) {
  return((long)dylan_slot_element(instance, 0));
}

unsigned long
dylan_unsigned_machine_integer_data(CORE_ADDR instance) {
  return((unsigned long)dylan_slot_element(instance, 0));
}

typedef union {
  unsigned long  i;
  float          f;
} INTFLT;

float
dylan_single_float_data(CORE_ADDR instance) {
  unsigned long x = dylan_slot_element(instance, 0);
  INTFLT intflt; intflt.i = x; return(intflt.f); 
}

int
dylan_string_size(CORE_ADDR instance) {
  return(dylan_slot_element(instance, 0) >> 2);
}

CORE_ADDR
dylan_string_data_address(CORE_ADDR instance) {
  return(instance + DYLAN_POINTER_SIZE * 2);
}

int
dylan_vector_size(CORE_ADDR instance) {
  return(dylan_slot_element(instance, 0) >> 2);
}

CORE_ADDR
dylan_vector_element(CORE_ADDR instance, int offset) {
  return(dylan_slot_element(instance, offset + 1));
}

CORE_ADDR
dylan_head(CORE_ADDR instance) {
  return(dylan_slot_element(instance, 0));
}

CORE_ADDR
dylan_tail(CORE_ADDR instance) {
  return(dylan_slot_element(instance, 1));
}

CORE_ADDR
dylan_symbol_name(CORE_ADDR instance) {
  return(dylan_slot_element(instance, 0));
}

CORE_ADDR
dylan_class_debug_name(CORE_ADDR instance) {
  return(dylan_slot_element(instance, DYLAN_CLASS_I_DEBUG_NAME));
}

CORE_ADDR
dylan_function_debug_name(CORE_ADDR instance) {
  return(dylan_slot_element(instance, 0));
}

CORE_ADDR
dylan_instance_header (CORE_ADDR instance) {
  CORE_ADDR header;
  target_read_memory (instance, &header, DYLAN_POINTER_SIZE);
  return(header);
}

CORE_ADDR
dylan_instance_header_probe (CORE_ADDR instance, CORE_ADDR *header) {
  int status = target_read_memory (instance, header, DYLAN_POINTER_SIZE);
  return(status == 0);
}

CORE_ADDR
dylan_class_p (CORE_ADDR instance) {
  CORE_ADDR header, header_header, header_header_header;
  return(dylan_instance_header_probe(instance, &header) &&
	 dylan_instance_header_probe(header, &header_header) &&
         dylan_instance_header_probe(header_header, &header_header_header) &&
         header_header == header_header_header);
}	   

CORE_ADDR
dylan_wrapper_p (CORE_ADDR instance) {
  CORE_ADDR header, header_header, header_header_header;
  return(dylan_instance_header_probe(instance, &header) &&
         dylan_instance_header_probe(header, &header_header) &&
         dylan_instance_header_probe(header_header, &header_header_header) &&
         header_header == header_header_header);
}	   

CORE_ADDR
dylan_object_p (CORE_ADDR instance) {
  CORE_ADDR header, header_header, header_header_header;
  char *name;
  if (instance & 3) 
    return(((instance & 1) != 0) || ((instance & 2) != 0));
  else 
    return(dylan_instance_header_probe (instance, &header) &&
           dylan_wrapper_p (header));
}

/*
CORE_ADDR
dylan_object_p (CORE_ADDR instance) {
  CORE_ADDR header, header_header, header_header_header;
  char *name;
  if (instance & 3) 
    return(((instance & 1) != 0) || ((instance & 2) != 0));
  else
    return(dylan_instance_header_probe (instance, &header) &&
           dylan_instance_header_probe(header, &header_header) &&
           dylan_instance_header_probe(header_header, &header_header_header) &&
           header_header == header_header_header);
}

CORE_ADDR
dylan_object_p (CORE_ADDR instance) {
  CORE_ADDR header, header_header, header_header_header;
  char *name;
  if (instance & 3) 
    return(((instance & 1) != 0) || ((instance & 2) != 0));
  else if (dylan_instance_header_probe (instance, &header) &&
           dylan_instance_header_probe(header, &header_header) &&
           dylan_instance_header_probe(header, &header_header) &&
           (name = maybe_find_symbolic_name (header_header))) {
    char demangled_name[1000];
    demangle_dylan_dylan_name(name, demangled_name);
    if (!strcmp(demangled_name, "<mm-wrapper>-wrapper"))
      return(true);
  }
  return(false);
}
*/

CORE_ADDR
dylan_mm_wrapper_class(CORE_ADDR instance) {
  /* return(dylan_slot_element(instance, 0)); */
  return(dylan_slot_element(dylan_slot_element(instance, DYLAN_MM_WRAPPER_I_IMPLEMENTATION_CLASS),
			    DYLAN_IMPLEMENTATION_CLASS_I_CLASS));
}

CORE_ADDR	    
dylan_object_class (CORE_ADDR instance) {
  CORE_ADDR instance_header = dylan_instance_header(instance);
  return(dylan_mm_wrapper_class(instance_header)); 
/*  return(instance_header); */
}

CORE_ADDR
dylan_object_implementation_class (CORE_ADDR instance) {
  CORE_ADDR wrapper = dylan_instance_header(instance);
  return(dylan_slot_element(wrapper, DYLAN_MM_WRAPPER_I_IMPLEMENTATION_CLASS));
}


/* CORE_ADDR */
/* dylan_class_non_class_descriptors(CORE_ADDR instance) { */
/*   return(dylan_slot_element(instance, 16)); */
/* } */

CORE_ADDR
dylan_implementation_class_instance_slot_descriptors(CORE_ADDR instance) {
  return(dylan_slot_element(instance, DYLAN_IMPLEMENTATION_CLASS_I_INSTANCE_SLOT_DESCRIPTORS));
}

/* CORE_ADDR */
/* dylan_class_repeated_slot_descriptor(CORE_ADDR instance) { */
/*   return(dylan_slot_element(instance, 15)); */
/* } */

CORE_ADDR
dylan_implementation_class_repeated_slot_descriptor(CORE_ADDR instance) {
  return(dylan_slot_element(instance, DYLAN_IMPLEMENTATION_CLASS_I_REPEATED_SLOT_DESCRIPTOR));
}

CORE_ADDR
dylan_class_implementation_class(CORE_ADDR instance) {
  return(dylan_slot_element(instance, DYLAN_CLASS_I_IMPLEMENTATION_CLASS));
}

CORE_ADDR
dylan_implementation_class_class(CORE_ADDR instance) {
  return(dylan_slot_element(instance, DYLAN_IMPLEMENTATION_CLASS_I_CLASS));
}

CORE_ADDR
dylan_slot_getter(CORE_ADDR instance) {
  return(dylan_slot_element(instance, 7));
}

CORE_ADDR
dylan_slot_keyword(CORE_ADDR instance) {
  return(dylan_slot_element(instance, 5));
}
