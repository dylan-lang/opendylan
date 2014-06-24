import lldb

OBJECT_TAG = 0
INTEGER_TAG = 1
BYTE_CHARACTER_TAG = 2
UNICODE_CHARACTER_TAG = 3

def dylan_tag_bits(value):
  return value.GetValueAsUnsigned() & 3

def dylan_boolean_value(value):
  target = lldb.debugger.GetSelectedTarget()
  true_value = target.FindFirstGlobalVariable('KPtrueVKi').AddressOf().GetValueAsUnsigned()
  return value.GetValueAsUnsigned() == true_value

def dylan_byte_character_value(value):
  byte_value = value.GetValueAsUnsigned() >> 2
  if byte_value < 0 or byte_value > 255:
    return chr(0)
  return chr(byte_value)

def dylan_byte_string_data(value):
  target = lldb.debugger.GetSelectedTarget()
  byte_string_type = target.FindFirstType('dylan_byte_string').GetPointerType()
  value = value.Cast(byte_string_type)
  size = dylan_integer_value(value.GetChildMemberWithName('size'))
  if size == 0:
    return ''
  data = value.GetChildMemberWithName('data').GetPointeeData(0, size + 1)
  error = lldb.SBError()
  string = data.GetString(error, 0)
  if error.Fail():
    return '<error: %s>' % error.GetCString()
  else:
    return '%s' % string

def dylan_integer_value(value):
  return value.GetValueAsUnsigned() >> 2

def dylan_object_class(value):
  target = lldb.debugger.GetSelectedTarget()
  # We use 'struct _dylan_object' here rather than 'dylan_object' as the latter
  # fails for some reason at the time this was written.
  object_type = target.FindFirstType('struct _dylan_object').GetPointerType()
  object_value = value.Cast(object_type)
  wrapper = object_value.GetChildMemberWithName('mm_wrapper')
  iclass = wrapper.GetChildMemberWithName('iclass')
  return iclass.GetChildMemberWithName('the_class')

def dylan_object_class_name(value):
  class_object = dylan_object_class(value)
  return dylan_byte_string_data(class_object.GetChildMemberWithName('debug_name'))

def dylan_symbol_name(value):
  target = lldb.debugger.GetSelectedTarget()
  symbol_type = target.FindFirstType('dylan_symbol').GetPointerType()
  value = value.Cast(symbol_type)
  return dylan_byte_string_data(value.GetChildMemberWithName('name'))

def dylan_unicode_character_value(value):
  return unichr(value.GetValueAsUnsigned() >> 2)
