import lldb
from dylan.accessors import *
import dylan.mangling as mangling

def dylan_value_summary(value, internal_dict):
  tag = dylan_tag_bits(value)
  if tag == OBJECT_TAG:
    return dylan_object_summary(value, internal_dict)
  elif tag == INTEGER_TAG:
    return dylan_integer_summary(value, internal_dict)
  elif tag == BYTE_CHARACTER_TAG:
    return dylan_byte_character_summary(value, internal_dict)
  elif tag == UNICODE_CHARACTER_TAG:
    return dylan_unicode_character_summary(value, internal_dict)
  else:
    return 'Invalid tag'

def dylan_object_summary(value, internal_dict):
  # In case we're looking at an object on the stack and not a
  # pointer to it.
  if not value.GetType().IsPointerType():
    value = value.address_of
  wrapper_symbol_name = dylan_object_wrapper_symbol_name(value)
  if not wrapper_symbol_name:
    # We have no valid wrapper symbol, so just find the symbol
    # for the address and use that. This is probably not actually
    # a dylan_value.
    target = lldb.debugger.GetSelectedTarget()
    address = lldb.SBAddress(value.GetValueAsUnsigned(), target)
    return address.symbol.name
  summary_func = SUMMARY_DISPATCH_TABLE.get(wrapper_symbol_name, dylan_user_defined_object_summary)
  return summary_func(value, internal_dict)

def dylan_byte_character_summary(value, internal_dict):
  byte_character = dylan_byte_character_value(value)
  return '{<byte-character>: %s (%s)}' % (byte_character, ord(byte_character))

def dylan_integer_summary(value, internal_dict):
  return '{<integer>: %s}' % dylan_integer_value(value)

def dylan_unicode_character_summary(value, internal_dict):
  unicode_character = dylan_unicode_character_value(value)
  return '{<unicode-character>: %s}' % unicode_character

def dylan_user_defined_object_summary(value, internal_dict):
  try:
    return '{%s}' % dylan_object_class_name(value)
  except:
    return '{uninitialized}'

def register(binding, module, library):
  def _register(function):
    wrapper = mangling.dylan_mangle_wrapper(binding, module, library)
    SUMMARY_DISPATCH_TABLE[wrapper] = function
    return function
  return _register

SUMMARY_DISPATCH_TABLE = {
}
