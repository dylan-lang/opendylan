import lldb

OBJECT_TAG = 0
INTEGER_TAG = 1
BYTE_CHARACTER_TAG = 2
UNICODE_CHARACTER_TAG = 3

BYTE_STRING_SIZE = 0
BYTE_STRING_DATA = 1
CLASS_DEBUG_NAME = 1
GENERIC_FUNCTION_DEBUG_NAME = 3
GENERIC_FUNCTION_METHODS = 4
IMPLEMENTATION_CLASS_CLASS = 1
IMPLEMENTATION_CLASS_REPEATED_SLOT_DESCRIPTOR = 4
IMPLEMENTATION_CLASS_INSTANCE_SLOT_DESCRIPTORS = 17
MM_WRAPPER_IMPLEMENTATION_CLASS = 0
MM_WRAPPER_SUBTYPE_MASK = 1
PAIR_HEAD = 0
PAIR_TAIL = 1
SLOT_DESCRIPTOR_GETTER = 4
SIMPLE_OBJECT_VECTOR_SIZE = 0
SIMPLE_OBJECT_VECTOR_DATA = 1
SYMBOL_NAME = 0
UNICODE_STRING_SIZE = 0
UNICODE_STRING_DATA = 1

def ensure_value_class(value, wrapper_symbol_name, class_name):
  wrappersym = dylan_object_wrapper_symbol_name(value)
  if wrappersym != wrapper_symbol_name:
    raise Exception("%#x is not a %s (%s != %s)" % (int(value.address_of.GetValueAsUnsigned()), class_name, wrappersym, wrapper_symbol_name))

def dylan_tag_bits(value):
  return value.GetValueAsUnsigned() & 3

def dylan_boolean_value(value):
  ensure_value_class(value, 'KLbooleanGVKdW', '<boolean>')
  target = lldb.debugger.GetSelectedTarget()
  true_value = target.FindFirstGlobalVariable('KPtrueVKi').AddressOf().GetValueAsUnsigned()
  return value.GetValueAsUnsigned() == true_value

def dylan_byte_character_value(value):
  byte_value = value.GetValueAsUnsigned() >> 2
  if byte_value < 0 or byte_value > 255:
    return chr(0)
  return chr(byte_value)

def dylan_byte_string_data(value):
  ensure_value_class(value, 'KLbyte_stringGVKdW', '<byte-string>')
  size = dylan_integer_value(dylan_slot_element(value, BYTE_STRING_SIZE))
  if size == 0:
    return ''
  target = lldb.debugger.GetSelectedTarget()
  word_size = target.GetAddressByteSize()
  data_start = value.GetValueAsUnsigned() + ((1 + BYTE_STRING_DATA) * word_size)
  error = lldb.SBError()
  data = value.process.ReadMemory(data_start, size, error)
  if error.Success():
    return data
  else:
    raise Exception(error.description)

def dylan_class_name(value):
  # We can't check that this is a <class> here as it might also
  # be a <function-class> or other subclass of <class>.
  class_name = dylan_slot_element(value, CLASS_DEBUG_NAME)
  return dylan_byte_string_data(class_name)

def dylan_double_integer_value(value):
  ensure_value_class(value, 'KLdouble_integerGVKeW', '<double-integer>')
  target = lldb.debugger.GetSelectedTarget()
  int_type = target.GetBasicType(lldb.eBasicTypeInt)
  lo = dylan_slot_element(value, 0).Cast(int_type)
  hi = dylan_slot_element(value, 1).Cast(int_type)
  return lo.GetValueAsSigned() + (hi.GetValueAsSigned() << 32)

def dylan_generic_function_name(value):
  return dylan_byte_string_data(dylan_slot_element(value, GENERIC_FUNCTION_DEBUG_NAME))

def dylan_generic_function_methods(value):
  return dylan_list_elements(dylan_slot_element(value, GENERIC_FUNCTION_METHODS))

def dylan_implementation_class_instance_slot_descriptors(iclass):
  return dylan_slot_element(iclass, IMPLEMENTATION_CLASS_INSTANCE_SLOT_DESCRIPTORS)

def dylan_implementation_class_repeated_slot_descriptor(iclass):
  return dylan_slot_element(iclass, IMPLEMENTATION_CLASS_REPEATED_SLOT_DESCRIPTOR)

def dylan_integer_value(value):
  return value.GetValueAsUnsigned() >> 2

def dylan_list_elements(value):
  elements = []
  while (dylan_object_class_name(value) != '<empty-list>'):
    elements += [dylan_slot_element(value, PAIR_HEAD)]
    value = dylan_slot_element(value, PAIR_TAIL)
  return elements

def dylan_machine_word_value(value):
  ensure_value_class(value, 'KLmachine_wordGVKeW', '<machine-word>')
  return dylan_slot_element(value, 0).GetValueAsUnsigned()

def dylan_object_class(value):
  iclass = dylan_object_implementation_class(value)
  ensure_value_class(iclass, 'KLimplementation_classGVKeW', '<implementation-class>')
  return dylan_slot_element(iclass, IMPLEMENTATION_CLASS_CLASS)

def dylan_object_class_slot_descriptors(value):
  # XXX: Add the repeated slot descriptor to this.
  iclass = dylan_object_implementation_class(value)
  descriptors = dylan_implementation_class_instance_slot_descriptors(iclass)
  return dylan_vector_elements(descriptors)

def dylan_object_class_slot_names(value):
  return [dylan_slot_descriptor_name(d) for d in dylan_object_class_slot_descriptors(value)]

def dylan_object_class_name(value):
  class_object = dylan_object_class(value)
  return dylan_class_name(class_object)

def dylan_object_implementation_class(value):
  wrapper = dylan_object_wrapper(value)
  return dylan_slot_element(wrapper, MM_WRAPPER_IMPLEMENTATION_CLASS)

def dylan_object_wrapper(value):
  address = value.GetValueAsUnsigned()
  target = lldb.debugger.GetSelectedTarget()
  dylan_value_type = target.FindFirstType('dylan_value')
  return value.CreateValueFromAddress("wrapper", address, dylan_value_type)

def dylan_object_wrapper_address(value):
  error = lldb.SBError()
  address = value.process.ReadPointerFromMemory(value.GetValueAsUnsigned(), error)
  if error.Success():
    return address
  else:
    raise Exception(error.description)

def dylan_object_wrapper_class(value):
  ensure_value_class(value, 'KLmm_wrapperGVKiW', '<mm-wrapper>')
  iclass = dylan_slot_element(value, MM_WRAPPER_IMPLEMENTATION_CLASS)
  ensure_value_class(iclass, 'KLimplementation_classGVKeW', '<implementation-class>')
  return dylan_slot_element(iclass, IMPLEMENTATION_CLASS_CLASS)

def dylan_object_wrapper_class_name(value):
  class_object = dylan_object_wrapper_class(value)
  return dylan_class_name(class_object)

def dylan_object_wrapper_subtype_mask(value):
  wrapper = dylan_object_wrapper(value)
  mask = dylan_slot_element(wrapper, MM_WRAPPER_SUBTYPE_MASK)
  return dylan_integer_value(mask)

def dylan_object_wrapper_symbol_name(value):
  # We don't want to get the actual wrapper value object here
  # as that creates circular loops with synthetics by creating
  # an object, which creates a synthetic which looks at the wrapper
  # symbol name, which if it accessed the wrapper value object
  # would create a new object and synthetic, etc.
  target = lldb.debugger.GetSelectedTarget()
  address = lldb.SBAddress(dylan_object_wrapper_address(value), target)
  return address.symbol.name

def dylan_slot_descriptor_getter(value):
  return dylan_slot_element(value, SLOT_DESCRIPTOR_GETTER)

def dylan_slot_descriptor_name(value):
  getter = dylan_slot_descriptor_getter(value)
  return dylan_generic_function_name(getter)

def dylan_slot_element(value, index):
  target = lldb.debugger.GetSelectedTarget()
  word_size = target.GetAddressByteSize()
  dylan_value_type = target.FindFirstType('dylan_value')
  slot_address = value.GetValueAsUnsigned() + ((index + 1) * word_size)
  return value.CreateValueFromAddress("slot %d" % index, slot_address, dylan_value_type)

def dylan_symbol_name(value):
  ensure_value_class(value, 'KLsymbolGVKdW', '<symbol>')
  name = dylan_slot_element(value, SYMBOL_NAME)
  return dylan_byte_string_data(name)

def dylan_unicode_character_value(value):
  return unichr(value.GetValueAsUnsigned() >> 2).encode('utf8')

def dylan_unicode_string_data(value):
  ensure_value_class(value, 'KLunicode_stringGVKdW', '<unicode-string>')
  size = dylan_integer_value(dylan_slot_element(value, UNICODE_STRING_SIZE))
  if size == 0:
    return ''
  # We don't need to read the null termination because we don't want that in
  # our UTF-8 encoded result.
  target = lldb.debugger.GetSelectedTarget()
  word_size = target.GetAddressByteSize()
  data_start = value.GetValueAsUnsigned() + ((1 + UNICODE_STRING_DATA) * word_size)
  error = lldb.SBError()
  data = value.process.ReadMemory(data_start, size * 4, error)
  if error.Success():
    return data.decode('utf-32').encode('utf-8')
  else:
    raise Exception(error.description)

def dylan_vector_size(vector):
  return dylan_integer_value(dylan_slot_element(vector, SIMPLE_OBJECT_VECTOR_SIZE))

def dylan_vector_element(vector, index):
  return dylan_slot_element(vector, SIMPLE_OBJECT_VECTOR_DATA + index)

def dylan_vector_elements(vector):
  elements = []
  for idx in range(0, dylan_vector_size(vector)):
    elements += [dylan_vector_element(vector, idx)]
  return elements
