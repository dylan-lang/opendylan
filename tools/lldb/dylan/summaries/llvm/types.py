from dylan.accessors import *
from dylan import summaries

@summaries.register('<llvm-function-type>', 'llvm', 'llvm')
def llvm_function_type_summary(value, internal_dict):
  return_type = dylan_slot_element_by_name(value, 'llvm-function-type-return-type')
  parameter_types = dylan_slot_element_by_name(value, 'llvm-function-type-parameter-types')
  varargs = dylan_slot_element_by_name(value, 'llvm-function-type-varargs?')
  has_varargs = dylan_boolean_value(varargs)
  name = summaries.summary(return_type)
  name += ' ('
  first = True
  for p in dylan_vector_elements(parameter_types):
    if not first:
      name += ', '
    name += summaries.summary(p)
    first = False
  if has_varargs:
    if not first:
      name += ', '
    name += '...'
  name += ')'
  return name

@summaries.register('<llvm-integer-type>', 'llvm', 'llvm')
def llvm_integer_type_summary(value, internal_dict):
  width = dylan_slot_element_by_name(value, 'llvm-integer-type-width')
  return 'i%s' % dylan_integer_value(width)

@summaries.register('<llvm-pointer-type>', 'llvm', 'llvm')
def llvm_pointer_type_summary(value, internal_dict):
  address_space_name = ''
  address_space = dylan_slot_element_by_name(value, 'llvm-pointer-type-address-space')
  address_space_id = dylan_integer_value(address_space)
  if address_space_id > 0:
    address_space_name = ' addrspace(%d)' % address_space_id
  pointee = dylan_slot_element_by_name(value, 'llvm-pointer-type-pointee')
  return '%s%s*' % (summaries.summary(pointee), address_space_name)

@summaries.register('<llvm-primitive-type>', 'llvm', 'llvm')
def llvm_primitive_type_summary(value, internal_dict):
  kind = dylan_slot_element_by_name(value, 'llvm-primitive-type-kind')
  kind_name = dylan_symbol_name(kind)
  return kind_name.lower()
