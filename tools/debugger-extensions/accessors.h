#ifndef ACCESSORSDEFD
#define ACCESSORSDEFD

extern CORE_ADDR dylan_slot_element (CORE_ADDR instance, int slot_offset);
extern CORE_ADDR dylan_slot_element_probe (CORE_ADDR instance, int slot_offset, CORE_ADDR *slot);
extern CORE_ADDR dylan_instance_header (CORE_ADDR instance);
extern long dylan_machine_integer_data (CORE_ADDR instance);
extern unsigned long dylan_unsigned_machine_integer_data(CORE_ADDR instance); 
extern float dylan_single_float_data (CORE_ADDR instance);
extern int dylan_string_size(CORE_ADDR instance);
extern CORE_ADDR dylan_string_data_address(CORE_ADDR instance);
extern int dylan_vector_size(CORE_ADDR instance);
extern CORE_ADDR dylan_vector_element(CORE_ADDR instance, int offset);
extern CORE_ADDR dylan_head(CORE_ADDR instance);
extern CORE_ADDR dylan_tail(CORE_ADDR instance);
extern CORE_ADDR dylan_symbol_name(CORE_ADDR instance);
extern CORE_ADDR dylan_class_debug_name(CORE_ADDR instance);
extern CORE_ADDR dylan_function_debug_name(CORE_ADDR instance);
extern CORE_ADDR dylan_instance_header (CORE_ADDR instance);
extern CORE_ADDR dylan_instance_header_probe (CORE_ADDR instance, CORE_ADDR *header);
extern CORE_ADDR dylan_object_p (CORE_ADDR instance);
extern CORE_ADDR dylan_mm_wrapper_class(CORE_ADDR instance);
extern CORE_ADDR dylan_object_class (CORE_ADDR instance);
extern CORE_ADDR dylan_implementation_class_instance_slot_descriptors(CORE_ADDR instance);
extern CORE_ADDR dylan_implementation_class_repeated_slot_descriptor(CORE_ADDR instance);
extern CORE_ADDR dylan_class_implementation_class(CORE_ADDR instance);
extern CORE_ADDR dylan_implementation_class_class(CORE_ADDR instance);
extern CORE_ADDR dylan_slot_getter(CORE_ADDR instance); 

#endif
