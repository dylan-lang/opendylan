"""
Contains routines to work with Dylan objects obtained from
LLDB's SBValue objects
"""

import struct
import lldb
from dylan.mangling import dylan_mangle_wrapper

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
"""Map of wrapper address to list of slot names"""

# Compatibility for 2.7/3
try:
  unichr
except NameError:
  unichr = chr

def ensure_value_class(value, class_name, module, library):
  """Raise an exception if the value is not a member of the given class"""
  wrappersym = dylan_object_wrapper_symbol_name(value)
  wrapper_symbol_name = dylan_mangle_wrapper(class_name, module, library)
  if wrappersym != wrapper_symbol_name:
    raise Exception("%#x is not a %s (%s != %s)" % (int(value.address_of.GetValueAsUnsigned()), class_name, wrappersym, wrapper_symbol_name))

def check_value_class(value, class_name, module, library):
  """Return True if the value is a member of the given class"""
  actual_wrapper_name = dylan_object_wrapper_symbol_name(value)
  desired_wrapper_name = dylan_mangle_wrapper(class_name, module, library)
  return actual_wrapper_name == desired_wrapper_name

def dylan_tag_bits(value):
  """Get type tag bits from a value"""
  return value.GetValueAsUnsigned() & 3

def dylan_boolean_value(value):
  """Return value as True/False"""
  ensure_value_class(value, '<boolean>', 'dylan', 'dylan')
  return not dylan_is_false(value)

def dylan_byte_character_value(value):
  """Return value as a single-character string"""
  byte_value = value.GetValueAsUnsigned() >> 2
  if 0 <= byte_value <= 255:
    return chr(byte_value)
  else:
    return chr(0)

def dylan_byte_string_data(value):
  """Return raw data from a <byte-string> value as a bytes object"""
  ensure_value_class(value, '<byte-string>', 'dylan', 'dylan')
  size = dylan_integer_value(dylan_slot_element(value, BYTE_STRING_SIZE))
  return dylan_read_raw_data(value, BYTE_STRING_DATA, size)

def dylan_class_name(value):
  """Return value's class name as a string"""
  # We can't check that this is a <class> here as it might also
  # be a <function-class> or other subclass of <class>.
  class_name = dylan_slot_element(value, CLASS_DEBUG_NAME)
  return dylan_string(class_name)

def dylan_double_float_data(value):
  """Return value as a double-precision float"""
  ensure_value_class(value, '<double-float>', 'dylan', 'dylan')
  data = dylan_read_raw_data(value, DOUBLE_FLOAT_DATA, 8)
  return struct.unpack('d', data)[0]

def dylan_double_integer_value(value):
  """Return value as an integer"""
  ensure_value_class(value, '<double-integer>', 'dylan-extensions', 'dylan')
  target = lldb.debugger.GetSelectedTarget()
  int_type = target.GetBasicType(lldb.eBasicTypeInt)
  lo = dylan_slot_element(value, 0).Cast(int_type)
  hi = dylan_slot_element(value, 1).Cast(int_type)
  return lo.GetValueAsSigned() + (hi.GetValueAsSigned() << 32)

def dylan_float_data(value):
  """Return value as a floating point number"""
  if dylan_is_single_float(value):
    return dylan_single_float_data(value)
  elif dylan_is_double_float(value):
    return dylan_double_float_data(value)
  else:
    class_name = dylan_object_class_name(value)
    raise Exception("%s is a new type of float? %s" % (value, class_name))

def dylan_generic_function_name(value):
  """Return the debug name of a generic function value as a string""" 
  return  dylan_string(dylan_slot_element(value, GENERIC_FUNCTION_DEBUG_NAME))

def dylan_generic_function_methods(value):
  """Return the list of specializer methods of a generic function value as SBValues"""
  methods = dylan_slot_element(value, GENERIC_FUNCTION_METHODS)
  if methods.GetError().Success():
    return dylan_list_elements(dylan_slot_element(value, GENERIC_FUNCTION_METHODS))
  else:
    raise Exception("'%s' was not a valid GF" % (value.GetName(),))

def dylan_implementation_class_instance_slot_descriptors(iclass):
  """Return the slot descriptors from an implementation class as an SBValue representing a <simple-object-vector>"""
  return dylan_slot_element(iclass, IMPLEMENTATION_CLASS_INSTANCE_SLOT_DESCRIPTORS)

def dylan_implementation_class_repeated_slot_descriptor(iclass):
  """Return the repeated slot descriptor from an implementation class as an SBValue or None if there isn't one"""
  repeated_slot = dylan_slot_element(iclass, IMPLEMENTATION_CLASS_REPEATED_SLOT_DESCRIPTOR)
  if dylan_is_false(repeated_slot):
    return None
  return repeated_slot

def dylan_integer_value(value):
  """Return value as an integer"""
  return value.GetValueAsUnsigned() >> 2

def dylan_is_boolean(value):
  """Return True if this value is a <boolean>"""
  return check_value_class(value, '<boolean>', 'dylan', 'dylan')

def dylan_is_float(value):
  """Return True if this value is a <float>"""
  return dylan_is_double_float(value) or dylan_is_single_float(value)

def dylan_is_single_float(value):
  """Return True if this value is a <single-float>"""
  return check_value_class(value, '<single-float>', 'dylan', 'dylan')

def dylan_is_double_float(value):
  """Return True if this value is a <double-float>"""
  return check_value_class(value, '<double-float>', 'dylan', 'dylan')

def dylan_is_false(value):
  """Return True if this value is #f"""
  target = lldb.debugger.GetSelectedTarget()
  address = lldb.SBAddress(value.GetValueAsUnsigned(), target)
  return address.symbol.name == 'KPfalseVKi'

def dylan_is_list(value):
  """Return True if this value is a list (i.e., an instance of <pair>)"""
  return check_value_class(value, '<pair>', 'dylan', 'dylan')

def dylan_is_simple_vector(value):
  """Return True if this value is a <simple-object-vector>"""
  # XXX: Check for just <simple-object-vector> until we start looking at
  #      actual inheritance data.
  return check_value_class(value, '<simple-object-vector>', 'dylan', 'dylan')

def dylan_is_stretchy_vector(value):
  """Return True if this value is a <stretchy-object-vector>"""
  # XXX: Check for just <stretchy-object-vector> until we start looking at
  #      actual inheritance data.
  return check_value_class(value, '<stretchy-object-vector>', 'dylan', 'dylan')

def dylan_is_string(value):
  """Return True if this value is a <string>"""
  return dylan_is_byte_string(value) or dylan_is_unicode_string(value)

def dylan_is_byte_string(value):
  """Return True if this value is a <byte-string>"""
  return check_value_class(value, '<byte-string>', 'dylan', 'dylan')

def dylan_is_unicode_string(value):
  """Return True if this value is a <unicode-string>"""
  return check_value_class(value, '<unicode-string>', 'dylan', 'dylan')

def dylan_list_empty(value):
  """Return True if this value is the empty list"""
  target = lldb.debugger.GetSelectedTarget()
  empty_list = target.FindFirstGlobalVariable('KPempty_listVKi').AddressOf().GetValueAsUnsigned()
  return value.GetValueAsUnsigned() == empty_list

def dylan_list_elements(value):
  """Assuming value is a list, return all the elements of value as a list of SBValues"""
  elements = []
  target = lldb.debugger.GetSelectedTarget()
  empty_list = target.FindFirstGlobalVariable('KPempty_listVKi').AddressOf().GetValueAsUnsigned()
  while value.GetValueAsUnsigned() != empty_list:
    elements += [dylan_slot_element(value, PAIR_HEAD)]
    value = dylan_slot_element(value, PAIR_TAIL)
  return elements

def dylan_machine_word_value(value):
  """Get the value of a <machine-word> as an integer"""
  ensure_value_class(value, '<machine-word>', 'dylan-extensions', 'dylan')
  return dylan_slot_element(value, 0).GetValueAsUnsigned()

def dylan_method_iep_function(value):
  """Get the internal entry point (IEP) of a function"""
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
  """Return the class of an object as an SBValue"""
  iclass = dylan_object_implementation_class(value)
  ensure_value_class(iclass, '<implementation-class>', 'dylan-extensions', 'dylan')
  return dylan_slot_element(iclass, IMPLEMENTATION_CLASS_CLASS)

def dylan_object_class_slot_descriptors(value):
  """Return an object's slot descriptors as a list of SBValues"""
  # XXX: Add the repeated slot descriptor to this.
  iclass = dylan_object_implementation_class(value)
  descriptors = dylan_implementation_class_instance_slot_descriptors(iclass)
  return dylan_vector_elements(descriptors)

def dylan_object_class_slot_names(value):
  """Return an object's slot names as a list of strings"""
  wrapper_address = dylan_object_wrapper_address(value)
  cached_slot_names = CLASS_SLOT_NAMES_CACHE.get(wrapper_address, None)
  if cached_slot_names:
    return cached_slot_names
  slot_names = [dylan_slot_descriptor_name(d) for d in dylan_object_class_slot_descriptors(value)]
  CLASS_SLOT_NAMES_CACHE[wrapper_address] = slot_names
  return slot_names

def dylan_object_class_repeated_slot_name(value):
  """Get the name of an object's repeated slot name as a string or None if there isn't one"""
  iclass = dylan_object_implementation_class(value)
  slot = dylan_implementation_class_repeated_slot_descriptor(iclass)
  if slot:
    return dylan_slot_descriptor_name(slot)
  return None

def dylan_object_class_name(value):
  """Return the name of an object's class as a string"""
  class_object = dylan_object_class(value)
  return dylan_class_name(class_object)

def dylan_object_implementation_class(value):
  """Return the implementation class of an object as an SBValue"""
  wrapper = dylan_object_wrapper(value)
  return dylan_slot_element(wrapper, MM_WRAPPER_IMPLEMENTATION_CLASS)

def dylan_object_wrapper(value):
  """Return the wrapper for an object as an SBValue"""
  address = value.GetValueAsUnsigned()
  target = lldb.debugger.GetSelectedTarget()
  dylan_value_type = target.FindFirstType('dylan_value')
  return value.CreateValueFromAddress("wrapper", address, dylan_value_type)

def dylan_object_wrapper_address(value):
  """Return the machine address of an object's wrapper"""
  error = lldb.SBError()
  address = value.process.ReadPointerFromMemory(value.GetValueAsUnsigned(), error)
  if error.Success():
    return address
  else:
    raise Exception(error.description)

def dylan_object_wrapper_class(value):
  """Return the implementation class of an object's wrapper as an SBValue"""
  ensure_value_class(value, '<mm-wrapper>', 'internal', 'dylan')
  iclass = dylan_slot_element(value, MM_WRAPPER_IMPLEMENTATION_CLASS)
  ensure_value_class(iclass, '<implementation-class>', 'dylan-extensions', 'dylan')
  return dylan_slot_element(iclass, IMPLEMENTATION_CLASS_CLASS)

def dylan_object_wrapper_class_name(value):
  """Return the name of an object's wrapper class as a string""" 
  class_object = dylan_object_wrapper_class(value)
  return dylan_class_name(class_object)

def dylan_object_wrapper_subtype_mask(value):
  """Return an object's wrapper subtype mask"""
  wrapper = dylan_object_wrapper(value)
  mask = dylan_slot_element(wrapper, MM_WRAPPER_SUBTYPE_MASK)
  return dylan_integer_value(mask)

def dylan_object_wrapper_symbol_name(value):
  """Return wrapper name of value as a string"""
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
  """Copy out and return size bytes from value at the indexed slot as a bytes object"""
  if size == 0:
    return b'';
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
  """Return the size of a vector"""
  return dylan_integer_value(dylan_slot_element(vector, SIMPLE_OBJECT_VECTOR_SIZE))

def dylan_simple_vector_element(vector, index):
  """Return an element from a vector by index as an SBValue"""
  return dylan_slot_element(vector, SIMPLE_OBJECT_VECTOR_DATA + index)

def dylan_single_float_data(value):
  """Return value as a single-precision float"""
  ensure_value_class(value, '<single-float>', 'dylan', 'dylan')
  data = dylan_read_raw_data(value, SINGLE_FLOAT_DATA, 4)
  return struct.unpack('f', data)[0]

def dylan_slot_index(value, name):
  """Return the index of a named slot in an object"""
  slot_names = dylan_object_class_slot_names(value)
  return slot_names.index(name)

def dylan_slot_descriptor_getter(value):
  """Return a value's slot descriptor getter as an SBValue"""
  return dylan_slot_element(value, SLOT_DESCRIPTOR_GETTER)

def dylan_slot_descriptor_name(value):
  """Return a slot getter name as a string"""
  getter = dylan_slot_descriptor_getter(value)
  return dylan_generic_function_name(getter)

def dylan_slot_element(value, index, name=None):
  """Assuming value is a Dylan object, return the slot with the given index as an SBValue"""
  target = lldb.debugger.GetSelectedTarget()
  word_size = target.GetAddressByteSize()
  dylan_value_type = target.FindFirstType('dylan_value')
  offset = (index + 1) * word_size
  name = name or '[%s]' % index
  return value.CreateChildAtOffset(name, offset, dylan_value_type)

def dylan_slot_element_by_name(value, name):
  """Assuming value is a Dylan object, return the named slot as an SBValue"""
  slot_index = dylan_slot_index(value, name)
  return dylan_slot_element(value, slot_index, '[' + name + ']')

def dylan_stretchy_vector_size(vector):
  """Return size of a stretchy vector"""
  representation = dylan_slot_element_by_name(vector, 'stretchy-representation')
  return dylan_integer_value(dylan_slot_element(representation, SIMPLE_OBJECT_VECTOR_SIZE))

def dylan_stretchy_vector_element(vector, index):
  """Return one indexed element of a vector as an SBValue"""
  representation = dylan_slot_element_by_name(vector, 'stretchy-representation')
  return dylan_slot_element(representation, SIMPLE_OBJECT_VECTOR_DATA + index)

def dylan_string_data(value):
  """If it is a string, return contents of value as a bytes object"""
  if dylan_is_byte_string(value):
    return dylan_byte_string_data(value)
  elif dylan_is_unicode_string(value):
    return dylan_unicode_string_data(value)
  else:
    class_name = dylan_object_class_name(value)
    raise Exception("%s is a new type of string? %s" % (value, class_name))

def target_address_format(target):
  """Get the size and struct format for accessing dylan_values on this target"""
  endian = target.GetByteOrder()
  byte_size = target.GetAddressByteSize()
  if endian == lldb.eByteOrderLittle:
    struct_format = '<'
  elif endian == lldb.eByteOrderBig:
    struct_format = '>'
  else:
    raise Exception("Unsupported endianness")
  if byte_size == 8:
    struct_format += 'Q'
  elif byte_size == 4:
    struct_format += 'L'
  else:
    raise Exception("Unsupported address size")
  return (byte_size, struct_format)

def dylan_string(value):
  """Assuming value is a Dylan string, return it as a Python str"""
  if dylan_is_byte_string(value):
    return dylan_byte_string_data(value).decode('utf-8')
  elif dylan_is_unicode_string(value):
    data = dylan_unicode_string_data(value)
    (int_size, data_format) = target_address_format(value.GetTarget())
    s = u''
    i = 0
    while i < len(data):
      c = struct.unpack_from(data_format, data, i)
      c = c[0] >> 2
      if 0 <= c <= 0x10FFFF:
        s += unichr(c)
      else:
        s += '?'
      i += int_size
    return s
  else:
    class_name = dylan_object_class_name(value)
    raise Exception("%s is a new type of string? %s" % (value, class_name))
  
def dylan_symbol_name(value):
  """Return a symbol as a string"""
  ensure_value_class(value, '<symbol>', 'dylan', 'dylan')
  name = dylan_slot_element(value, SYMBOL_NAME)
  return dylan_string(name)

def dylan_unicode_character_value(value):
  """Return a value as a chr"""
  codepoint = value.GetValueAsUnsigned() >> 2
  # May raise here if not valid unicode, will handle it later
  return unichr(codepoint)

def dylan_unicode_string_data(value):
  """Return a value as a bytes object"""
  ensure_value_class(value, '<unicode-string>', 'dylan', 'dylan')
  size = dylan_integer_value(dylan_slot_element(value, UNICODE_STRING_SIZE))
  # Dylan's <unicode-character> is the same as <integer> hence is pointer-sized
  int_size = value.GetTarget().GetAddressByteSize()
  return dylan_read_raw_data(value, UNICODE_STRING_DATA, size * int_size)

def dylan_sequence_empty(sequence):
  """Return True if sequence is empty"""
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
  """Return size of a vector"""
  if dylan_is_simple_vector(vector):
    return dylan_simple_vector_size(vector)
  elif dylan_is_stretchy_vector(vector):
    return dylan_stretchy_vector_size(vector)
  else:
    return 0

def dylan_vector_element(vector, index):
  """Return indexed element of vector as an SBValue"""
  if dylan_is_simple_vector(vector):
    return dylan_simple_vector_element(vector, index)
  elif dylan_is_stretchy_vector(vector):
    return dylan_stretchy_vector_element(vector, index)
  else:
    return None

def dylan_vector_elements(vector):
  """Return all elements of vector as a list of SBValues"""
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
