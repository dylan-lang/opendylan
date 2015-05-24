from dylan.accessors import *
from dylan import summaries

@summaries.register('<llvm-basic-block>', 'llvm', 'llvm')
def llvm_basic_block_summary(value, internal_dict):
  bblock_name_value = dylan_slot_element_by_name(value, 'llvm-basic-block-name')
  # bblock_name_value is false-or(<string>)
  if dylan_is_false(bblock_name_value):
    return ''
  return dylan_string_data(bblock_name_value)

@summaries.register('<llvm-metadata-string>', 'llvm', 'llvm')
def llvm_metadata_string_summary(value, internal_dict):
  string_value = dylan_slot_element_by_name(value, 'llvm-metadata-string')
  return dylan_string_data(string_value)

@summaries.register('<llvm-named-metadata>', 'llvm', 'llvm')
def llvm_named_metadata_summary(value, internal_dict):
  name_value = dylan_slot_element_by_name(value, 'llvm-named-metadata-name')
  return dylan_string_data(name_value)

@summaries.register('<llvm-module>', 'llvm', 'llvm')
def llvm_module_summary(value, internal_dict):
  module_name_value = dylan_slot_element_by_name(value, 'llvm-module-name')
  return dylan_string_data(module_name_value)
