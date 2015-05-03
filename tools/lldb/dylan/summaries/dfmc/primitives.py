from dylan.accessors import *
from dylan import mangling, summaries

@summaries.register('<&primitive>', 'dfmc-modeling', 'dfmc-modeling')
def primitive_summary(value, internal_dict):
  name_symbol = dylan_slot_element_by_name(value, 'primitive-descriptor-getter-name')
  return dylan_symbol_name(name_symbol)

@summaries.register('<&c-function>', 'dfmc-modeling', 'dfmc-modeling')
@summaries.register('<&objc-msgsend>', 'dfmc-modeling', 'dfmc-modeling')
def c_function_summary(value, internal_dict):
  name = dylan_slot_element_by_name(value, 'c-function-name')
  return dylan_byte_string_data(name)
