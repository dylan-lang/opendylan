import lldb
import struct

import mangling

OBJECT_TAG = 0
INTEGER_TAG = 1
BYTE_CHARACTER_TAG = 2
UNICODE_CHARACTER_TAG = 3

BYTE_STRING_SIZE = 0
BYTE_STRING_DATA = 1
CLASS_DEBUG_NAME = 1
DOUBLE_FLOAT_DATA = 0
GENERIC_FUNCTION_DEBUG_NAME = 3
GENERIC_FUNCTION_METHODS = 4
IMPLEMENTATION_CLASS_CLASS = 1
IMPLEMENTATION_CLASS_REPEATED_SLOT_DESCRIPTOR = 3
IMPLEMENTATION_CLASS_INSTANCE_SLOT_DESCRIPTORS = 4
KEYWORD_METHOD_IEP = 3
METHOD_MEP = 2
MM_WRAPPER_IMPLEMENTATION_CLASS = 0
MM_WRAPPER_SUBTYPE_MASK = 1
PAIR_HEAD = 0
PAIR_TAIL = 1
SINGLE_FLOAT_DATA = 0
SLOT_DESCRIPTOR_GETTER = 4
SIMPLE_OBJECT_VECTOR_SIZE = 0
SIMPLE_OBJECT_VECTOR_DATA = 1
SYMBOL_NAME = 0
UNICODE_STRING_SIZE = 0
UNICODE_STRING_DATA = 1

CLASS_SLOT_NAMES_CACHE = {}

def ensure_value_class(value, class_name, module, library):
  wrappersym = dylan_object_wrapper_symbol_name(value)
  wrapper_symbol_name = mangling.dylan_mangle_wrapper(class_name, module, library)
  if wrappersym != wrapper_symbol_name:
    raise Exception("%#x is not a %s (%s != %s)" % (int(value.address_of.GetValueAsUnsigned()), class_name, wrappersym, wrapper_symbol_name))

def check_value_class(value, class_name, module, library):
  actual_wrapper_name = dylan_object_wrapper_symbol_name(value)
  desired_wrapper_name = mangling.dylan_mangle_wrapper(class_name, module, library)
  return actual_wrapper_name == desired_wrapper_name

def dylan_tag_bits(value):
  return value.GetValueAsUnsigned() & 3

def dylan_boolean_value(value):
  ensure_value_class(value, '<boolean>', 'dylan', 'dylan')
  target = lldb.debugger.GetSelectedTarget()
  true_value = target.FindFirstGlobalVariable('KPtrueVKi').AddressOf().GetValueAsUnsigned()
  return value.GetValueAsUnsigned() == true_value

def dylan_byte_character_value(value):
  byte_value = value.GetValueAsUnsigned() >> 2
  if byte_value < 0 or byte_value > 255:
    return chr(0)
  return chr(byte_value)

def dylan_byte_string_data(value):
  ensure_value_class(value, '<byte-string>', 'dylan', 'dylan')
  size = dylan_integer_value(dylan_slot_element(value, BYTE_STRING_SIZE))
  if size == 0:
    return ''
  return dylan_read_raw_data(value, BYTE_STRING_DATA, size)

def dylan_class_name(value):
  # We can't check that this is a <class> here as it might also
  # be a <function-class> or other subclass of <class>.
  class_name = dylan_slot_element(value, CLASS_DEBUG_NAME)
  return dylan_byte_string_data(class_name)

def dylan_double_float_data(value):
  ensure_value_class(value, '<double-float>', 'dylan', 'dylan')
  data = dylan_read_raw_data(value, DOUBLE_FLOAT_DATA, 8)
  return struct.unpack('d', data)[0]

def dylan_double_integer_value(value):
  ensure_value_class(value, '<double-integer>', 'dylan-extensions', 'dylan')
  target = lldb.debugger.GetSelectedTarget()
  int_type = target.GetBasicType(lldb.eBasicTypeInt)
  lo = dylan_slot_element(value, 0).Cast(int_type)
  hi = dylan_slot_element(value, 1).Cast(int_type)
  return lo.GetValueAsSigned() + (hi.GetValueAsSigned() << 32)

def dylan_float_data(value):
  if dylan_is_single_float(value):
    return dylan_single_float_data(value)
  elif dylan_is_double_float(value):
    return dylan_double_float_data(value)
  else:
    class_name = dylan_object_class_name(value)
    raise Exception("%s is a new type of float? %s" % (value, class_name))

def dylan_generic_function_name(value):
  return dylan_byte_string_data(dylan_slot_element(value, GENERIC_FUNCTION_DEBUG_NAME))

def dylan_generic_function_methods(value):
  return dylan_list_elements(dylan_slot_element(value, GENERIC_FUNCTION_METHODS))

def dylan_implementation_class_instance_slot_descriptors(iclass):
  return dylan_slot_element(iclass, IMPLEMENTATION_CLASS_INSTANCE_SLOT_DESCRIPTORS)

def dylan_implementation_class_repeated_slot_descriptor(iclass):
  target = lldb.debugger.GetSelectedTarget()
  false_value = target.FindFirstGlobalVariable('KPfalseVKi').AddressOf().GetValueAsUnsigned()
  repeated_slot = dylan_slot_element(iclass, IMPLEMENTATION_CLASS_REPEATED_SLOT_DESCRIPTOR)
  if repeated_slot.GetValueAsUnsigned() == false_value:
    return None
  return repeated_slot

def dylan_integer_value(value):
  return value.GetValueAsUnsigned() >> 2

def dylan_is_boolean(value):
  return check_value_class(value, '<boolean>', 'dylan', 'dylan')

def dylan_is_byte_string(value):
  return check_value_class(value, '<byte-string>', 'dylan', 'dylan')

def dylan_is_double_float(value):
  return check_value_class(value, '<double-float>', 'dylan', 'dylan')

def dylan_is_float(value):
  return dylan_is_double_float(value) or dylan_is_single_float(value)

def dylan_is_list(value):
  return check_value_class(value, '<pair>', 'dylan', 'dylan')

def dylan_is_simple_vector(value):
  # XXX: Check for just <simple-object-vector> until we start looking at
  #      actual inheritance data.
  return check_value_class(value, '<simple-object-vector>', 'dylan', 'dylan')

def dylan_is_single_float(value):
  return check_value_class(value, '<single-float>', 'dylan', 'dylan')

def dylan_is_stretchy_vector(value):
  # XXX: Check for just <stretchy-object-vector> until we start looking at
  #      actual inheritance data.
  return check_value_class(value, '<stretchy-object-vector>', 'dylan', 'dylan')

def dylan_is_string(value):
  return dylan_is_byte_string(value) or dylan_is_unicode_string(value)

def dylan_is_unicode_string(value):
  return check_value_class(value, '<unicode-string>', 'dylan', 'dylan')

def dylan_list_empty(value):
  target = lldb.debugger.GetSelectedTarget()
  empty_list = target.FindFirstGlobalVariable('KPempty_listVKi').AddressOf().GetValueAsUnsigned()
  return value.GetValueAsUnsigned() == empty_list

def dylan_list_elements(value):
  elements = []
  target = lldb.debugger.GetSelectedTarget()
  empty_list = target.FindFirstGlobalVariable('KPempty_listVKi').AddressOf().GetValueAsUnsigned()
  while value.GetValueAsUnsigned() != empty_list:
    elements += [dylan_slot_element(value, PAIR_HEAD)]
    value = dylan_slot_element(value, PAIR_TAIL)
  return elements

def dylan_machine_word_value(value):
  ensure_value_class(value, '<machine-word>', 'dylan-extensions', 'dylan')
  return dylan_slot_element(value, 0).GetValueAsUnsigned()

def dylan_method_iep_function(value):
  def get_iep(value):
    if dylan_object_wrapper_subtype_mask(value) & 8192:
      return dylan_slot_element(value, KEYWORD_METHOD_IEP)
    else:
      return dylan_slot_element(value, METHOD_MEP)
  iep = get_iep(value)
  target = lldb.debugger.GetSelectedTarget()
  address = lldb.SBAddress(iep.GetValueAsUnsigned(), target)
  return address.GetFunction()

def dylan_object_class(value):
  iclass = dylan_object_implementation_class(value)
  ensure_value_class(iclass, '<implementation-class>', 'dylan-extensions', 'dylan')
  return dylan_slot_element(iclass, IMPLEMENTATION_CLASS_CLASS)

def dylan_object_class_slot_descriptors(value):
  # XXX: Add the repeated slot descriptor to this.
  iclass = dylan_object_implementation_class(value)
  descriptors = dylan_implementation_class_instance_slot_descriptors(iclass)
  return dylan_vector_elements(descriptors)

def dylan_object_class_slot_names(value):
  wrapper_address = dylan_object_wrapper_address(value)
  cached_slot_names = CLASS_SLOT_NAMES_CACHE.get(wrapper_address, None)
  if cached_slot_names:
    return cached_slot_names
  slot_names = [dylan_slot_descriptor_name(d) for d in dylan_object_class_slot_descriptors(value)]
  CLASS_SLOT_NAMES_CACHE[wrapper_address] = slot_names
  return slot_names

def dylan_object_class_repeated_slot_name(value):
  iclass = dylan_object_implementation_class(value)
  slot = dylan_implementation_class_repeated_slot_descriptor(iclass)
  if slot:
    return dylan_slot_descriptor_name(slot)
  return None

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
  ensure_value_class(value, '<mm-wrapper>', 'internal', 'dylan')
  iclass = dylan_slot_element(value, MM_WRAPPER_IMPLEMENTATION_CLASS)
  ensure_value_class(iclass, '<implementation-class>', 'dylan-extensions', 'dylan')
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
  if value.GetValueAsUnsigned() == 0:
    return None
  try:
    wrapper_address = dylan_object_wrapper_address(value)
  except:
    return None
  address = lldb.SBAddress(wrapper_address, target)
  return address.symbol.name

def dylan_read_raw_data(value, slot_index, size):
  target = lldb.debugger.GetSelectedTarget()
  word_size = target.GetAddressByteSize()
  data_start = value.GetValueAsUnsigned() + ((1 + slot_index) * word_size)
  error = lldb.SBError()
  data = value.process.ReadMemory(data_start, size, error)
  if error.Success():
    return data
  else:
    raise Exception(error.description)

def dylan_simple_vector_size(vector):
  return dylan_integer_value(dylan_slot_element(vector, SIMPLE_OBJECT_VECTOR_SIZE))

def dylan_simple_vector_element(vector, index):
  return dylan_slot_element(vector, SIMPLE_OBJECT_VECTOR_DATA + index)

def dylan_single_float_data(value):
  ensure_value_class(value, '<single-float>', 'dylan', 'dylan')
  data = dylan_read_raw_data(value, SINGLE_FLOAT_DATA, 4)
  return struct.unpack('f', data)[0]

def dylan_slot_index(value, name):
  slot_names = dylan_object_class_slot_names(value)
  return slot_names.index(name)

def dylan_slot_descriptor_getter(value):
  return dylan_slot_element(value, SLOT_DESCRIPTOR_GETTER)

def dylan_slot_descriptor_name(value):
  getter = dylan_slot_descriptor_getter(value)
  return dylan_generic_function_name(getter)

def dylan_slot_element(value, index, name=None):
  target = lldb.debugger.GetSelectedTarget()
  word_size = target.GetAddressByteSize()
  dylan_value_type = target.FindFirstType('dylan_value')
  offset = (index + 1) * word_size
  name = name or '[%s]' % index
  return value.CreateChildAtOffset(name, offset, dylan_value_type)

def dylan_slot_element_by_name(value, name):
  slot_index = dylan_slot_index(value, name)
  return dylan_slot_element(value, slot_index, '[' + name + ']')

def dylan_stretchy_vector_size(vector):
  representation = dylan_slot_element_by_name(vector, 'stretchy-representation')
  return dylan_integer_value(dylan_slot_element(representation, SIMPLE_OBJECT_VECTOR_SIZE))

def dylan_stretchy_vector_element(vector, index):
  representation = dylan_slot_element_by_name(vector, 'stretchy-representation')
  return dylan_slot_element(representation, SIMPLE_OBJECT_VECTOR_DATA + index)

def dylan_string_data(value):
  if dylan_is_byte_string(value):
    return dylan_byte_string_data(value)
  elif dylan_is_unicode_string(value):
    return dylan_unicode_string_data(value)
  else:
    class_name = dylan_object_class_name(value)
    raise Exception("%s is a new type of string? %s" % (value, class_name))

def dylan_symbol_name(value):
  ensure_value_class(value, '<symbol>', 'dylan', 'dylan')
  name = dylan_slot_element(value, SYMBOL_NAME)
  return dylan_byte_string_data(name)

def dylan_unicode_character_value(value):
  return unichr(value.GetValueAsUnsigned() >> 2).encode('utf8')

def dylan_unicode_string_data(value):
  ensure_value_class(value, '<unicode-string>', 'dylan', 'dylan')
  size = dylan_integer_value(dylan_slot_element(value, UNICODE_STRING_SIZE))
  if size == 0:
    return ''
  # We don't need to read the null termination because we don't want that in
  # our UTF-8 encoded result.
  data = dylan_read_raw_data(value, UNICODE_STRING_DATA, size * 4)

def dylan_sequence_empty(sequence):
  if dylan_is_simple_vector(sequence):
    return dylan_simple_vector_size(sequence) == 0
  elif dylan_is_stretchy_vector(sequence):
    return dylan_stretchy_vector_size(sequence) == 0
  elif dylan_is_list(sequence):
    return dylan_list_empty(sequence)
  else:
    class_name = dylan_object_class_name(sequence)
    raise Exception("%s is a new type of sequence? %s" % (sequence, class_name))

def dylan_vector_size(vector):
  if dylan_is_simple_vector(vector):
    return dylan_simple_vector_size(vector)
  elif dylan_is_stretchy_vector(vector):
    return dylan_stretchy_vector_size(vector)
  else:
    return 0

def dylan_vector_element(vector, index):
  if dylan_is_simple_vector(vector):
    return dylan_simple_vector_element(vector, index)
  elif dylan_is_stretchy_vector(vector):
    return dylan_stretchy_vector_element(vector, index)
  else:
    return None

def dylan_vector_elements(vector):
  elements = []
  # Specialize a bit to save a lot of extra type checks.
  if dylan_is_simple_vector(vector):
    for idx in range(0, dylan_simple_vector_size(vector)):
      elements += [dylan_simple_vector_element(vector, idx)]
  elif dylan_is_stretchy_vector(vector):
    for idx in range(0, dylan_stretchy_vector_size(vector)):
      elements += [dylan_stretchy_vector_element(vector, idx)]
  else:
    for idx in range(0, dylan_vector_size(vector)):
      elements += [dylan_vector_element(vector, idx)]
  return elements
