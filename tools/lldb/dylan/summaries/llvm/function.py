from dylan.accessors import *
from dylan import mangling, summaries
from dylan.summaries.llvm.types import llvm_type_name

@summaries.register('<llvm-argument>', 'llvm', 'llvm')
def llvm_argument_summary(value, internal_dict):
  argument_type = dylan_slot_element_by_name(value, 'llvm-value-type')
  argument_name_value = dylan_slot_element_by_name(value, 'llvm-argument-name')
  argument_name = ''
  # argument_name_value is false-or(<string>)
  if not dylan_is_boolean(argument_name_value):
    argument_name = ' ' + dylan_byte_string_data(argument_name_value)
  return '%s%s' % (llvm_type_name(value), argument_name)
