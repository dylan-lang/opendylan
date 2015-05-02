from dylan.accessors import *
from dylan import mangling, summaries

LLVM_TYPE_NAMERS = {}

def llvm_type_name(value):
  wrapper = dylan_object_wrapper_symbol_name(value)
  namer = LLVM_TYPE_NAMERS.get(wrapper, llvm_unknown_type_name)
  return namer(value)

def llvm_unknown_type_name(value):
  class_name = dylan_object_class_name(value)
  return 'Unknown LLVM type: %s' % class_name

def llvm_type_namer(binding, module, library):
  def _register(function):
    wrapper = mangling.dylan_mangle_wrapper(binding, module, library)
    LLVM_TYPE_NAMERS[wrapper] = function
    return function
  return _register

@llvm_type_namer('<llvm-function-type>', 'llvm', 'llvm')
def llvm_function_type_name(value):
  return_type = dylan_slot_element_by_name(value, 'llvm-function-type-return-type')
  parameter_types = dylan_slot_element_by_name(value, 'llvm-function-type-parameter-types')
  varargs = dylan_slot_element_by_name(value, 'llvm-function-type-varargs?')
  has_varargs = dylan_boolean_value(varargs)
  name = llvm_type_name(return_type)
  name += ' ('
  first = True
  for p in dylan_vector_elements(parameter_types):
    if not first:
      name += ', '
    name += llvm_type_name(p)
    first = False
  if has_varargs:
    if not first:
      name += ', '
    name += '...'
  name += ')'
  return name

@summaries.register('<llvm-function-type>', 'llvm', 'llvm')
def llvm_function_type_summary(value, internal_dict):
  return '{<llvm-function-type>: %s}' % llvm_function_type_name(value)

@llvm_type_namer('<llvm-integer-type>', 'llvm', 'llvm')
def llvm_integer_type_name(value):
  width = dylan_slot_element_by_name(value, 'llvm-integer-type-width')
  return 'i%s' % dylan_integer_value(width)

@summaries.register('<llvm-integer-type>', 'llvm', 'llvm')
def llvm_integer_type_summary(value, internal_dict):
  return '{<llvm-integer-type>: %s}' % llvm_integer_type_name(value)

@llvm_type_namer('<llvm-pointer-type>', 'llvm', 'llvm')
def llvm_pointer_type_name(value):
  address_space_name = ''
  address_space = dylan_slot_element_by_name(value, 'llvm-pointer-type-address-space')
  address_space_id = dylan_integer_value(address_space)
  if address_space_id > 0:
    address_space_name = ' addrspace(%d)' % address_space_id
  pointee = dylan_slot_element_by_name(value, 'llvm-pointer-type-pointee')
  return '%s%s*' % (llvm_type_name(pointee), address_space_name)

@summaries.register('<llvm-pointer-type>', 'llvm', 'llvm')
def llvm_pointer_type_summary(value, internal_dict):
  return '{<llvm-pointer-type>: %s}' % llvm_pointer_type_name(value)

@llvm_type_namer('<llvm-primitive-type>', 'llvm', 'llvm')
def llvm_primitive_type_name(value):
  kind = dylan_slot_element_by_name(value, 'llvm-primitive-type-kind')
  kind_name = dylan_symbol_name(kind)
  return kind_name.lower()

@summaries.register('<llvm-primitive-type>', 'llvm', 'llvm')
def llvm_primitive_type_summary(value, internal_dict):
  return '{<llvm-primitive-type>: %s}' % llvm_primitive_type_name(value)
