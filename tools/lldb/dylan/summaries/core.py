import lldb
from dylan.accessors import *
from dylan import summaries

@summaries.register('<boolean>', 'dylan', 'dylan')
def dylan_boolean_summary(value, internal_dict):
  b = '#f'
  if dylan_boolean_value(value):
    b = '#t'
  return b

@summaries.register('<byte-string>', 'dylan', 'dylan')
def dylan_byte_string_summary(value, internal_dict):
  string_data = dylan_byte_string_data(value)
  string_data = string_data.replace('\r\n', '\\n')
  string_data = string_data.replace('\n', '\\n')
  if string_data == '':
    return 'size: 0'
  else:
    return 'size: %d, data: "%s"' % (len(string_data), string_data)

@summaries.register('<class>', 'dylan', 'dylan')
def dylan_class_summary(value, internal_dict):
  return dylan_class_name(value)

@summaries.register('<double-float>', 'dylan', 'dylan')
def dylan_double_float_summary(value, internal_dict):
  return dylan_double_float_data(value)

@summaries.register('<double-integer>', 'dylan-extensions', 'dylan')
def dylan_double_integer_summary(value, internal_dict):
  return dylan_double_integer_value(value)

@summaries.register('<empty-list>', 'dylan', 'dylan')
def dylan_empty_list_summary(value, internal_dict):
  return '#()'

@summaries.register('<generic-function>', 'dylan', 'dylan')
@summaries.register('<incremental-generic-function>', 'dylan-extensions', 'dylan')
@summaries.register('<sealed-generic-function>', 'dylan-extensions', 'dylan')
def dylan_generic_function_summary(value, internal_dict):
  return dylan_generic_function_name(value)

@summaries.register('<hash-state>', 'dylan-extensions', 'dylan')
def dylan_hash_state_summary(value, internal_dict):
  w0 = dylan_slot_element_by_name(value, 'mps-w0').GetValueAsUnsigned()
  w1 = dylan_slot_element_by_name(value, 'mps-w1').GetValueAsUnsigned()
  return 'w0: %s w1: %s' % (format_machine_word_value(w0), format_machine_word_value(w1))

@summaries.register('<instance-slot-descriptor>', 'dylan-extensions', 'dylan')
@summaries.register('<repeated-slot-descriptor>', 'dylan-extensions', 'dylan')
def dylan_slot_descriptor_summary(value, internal_dict):
  return dylan_slot_descriptor_name(value)

@summaries.register('<library>', 'dylan-extensions', 'dylan')
def dylan_library_summary(value, internal_dict):
  name = dylan_slot_element_by_name(value, 'namespace-name')
  return dylan_byte_string_data(name)

def format_machine_word_value(value):
  target = lldb.debugger.GetSelectedTarget()
  word_size = target.GetAddressByteSize()
  if word_size == 4:
    fmt = "%#010x"
  elif word_size == 8:
    fmt = "%#018x"
  else:
    fmt = "%#x"
  return fmt % value

@summaries.register('<machine-word>', 'dylan-extensions', 'dylan')
def dylan_machine_word_summary(value, internal_dict):
  machine_word_value = dylan_machine_word_value(value)
  return format_machine_word_value(machine_word_value)

@summaries.register('<mm-wrapper>', 'internal', 'dylan')
def dylan_mm_wrapper_summary(value, internal_dict):
  return dylan_object_wrapper_class_name(value)

@summaries.register('<module>', 'dylan-extensions', 'dylan')
def dylan_module_summary(value, internal_dict):
  module_name = dylan_slot_element_by_name(value, 'namespace-name')
  home_library = dylan_slot_element_by_name(value, 'home-library')
  home_library_name = dylan_slot_element_by_name(home_library, 'namespace-name')
  return '%s in %s' % (dylan_byte_string_data(module_name),
                       dylan_byte_string_data(home_library_name))

@summaries.register('<simple-object-vector>', 'dylan', 'dylan')
def dylan_simple_object_vector_summary(value, internal_dict):
  return dylan_simple_vector_size(value)

@summaries.register('<single-float>', 'dylan', 'dylan')
def dylan_single_float_summary(value, internal_dict):
  return dylan_single_float_data(value)

@summaries.register('<stretchy-object-vector>', 'dylan-extensions', 'dylan')
def dylan_stretchy_object_vector_summary(value, internal_dict):
  return dylan_stretchy_vector_size(value)

@summaries.register('<symbol>', 'dylan', 'dylan')
def dylan_symbol_summary(value, internal_dict):
  return '#"%s"' % dylan_symbol_name(value)

@summaries.register('<thread>', 'threads', 'dylan')
@summaries.register('<synchronous-thread>', 'threads', 'dylan')
def dylan_thread_summary(value, internal_dict):
  thread_name = dylan_slot_element_by_name(value, '')
  if not dylan_is_boolean(thread_name):
    return dylan_string_data(thread_name)
  return None

@summaries.register('<unicode-string>', 'dylan', 'dylan')
def dylan_unicode_string_summary(value, internal_dict):
  return dylan_unicode_string_data(value)

@summaries.register('<used-library>', 'dylan-extensions', 'dylan')
def dylan_used_library_summary(value, internal_dict):
  used_library = dylan_slot_element_by_name(value, 'used-library')
  name = dylan_slot_element_by_name(used_library, 'namespace-name')
  return dylan_byte_string_data(name)
