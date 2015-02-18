import lldb
from accessors import *
import synthetics

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
  class_name = dylan_object_class_name(value)
  summary_func = SUMMARY_DISPATCH_TABLE.get(class_name, dylan_user_defined_object_summary)
  return summary_func(value, internal_dict)

def dylan_boolean_summary(value, internal_dict):
  b = '#f'
  if dylan_boolean_value(value):
    b = '#t'
  return '{<boolean>: %s}' % b

def dylan_byte_character_summary(value, internal_dict):
  byte_character = dylan_byte_character_value(value)
  return '{<byte-character>: %s (%s)}' % (byte_character, ord(byte_character))

def dylan_byte_string_summary(value, internal_dict):
  string_data = dylan_byte_string_data(value)
  if string_data == '':
    return '{<byte-string>: size: 0}'
  else:
    return '{<byte-string>: size: %d, data: "%s"}' % (len(string_data), string_data)

def dylan_double_integer_summary(value, internal_dict):
  di = dylan_double_integer_value(value)
  return '{<double-integer>: %s}' % di

def dylan_empty_list_summary(value, internal_dict):
  return '{<empty-list>: #()}'

def dylan_generic_function_summary(value, internal_dict):
  class_name = dylan_object_class_name(value)
  function_name = dylan_generic_function_name(value)
  return '{%s: %s}' % (class_name, function_name)

def dylan_integer_summary(value, internal_dict):
  return '{<integer>: %s}' % dylan_integer_value(value)

def dylan_simple_object_vector_summary(value, internal_dict):
  size = synthetics.SyntheticDylanValue(value, internal_dict).num_children()
  return '{<simple-object-vector>: size: %s}' % size

def dylan_symbol_summary(value, internal_dict):
  return'{<symbol>: #"%s"}' % dylan_symbol_name(value)

def dylan_unicode_character_summary(value, internal_dict):
  unicode_character = dylan_unicode_character_value(value)
  return '{<unicode-character>: %s}' % unicode_character

def dylan_unicode_string_summary(value, internal_dict):
  unicode_string = dylan_unicode_string_data(value)
  return '{<unicode-string>: %s}' % unicode_string

def dylan_user_defined_object_summary(value, internal_dict):
  return '{%s}' % dylan_object_class_name(value)

SUMMARY_DISPATCH_TABLE = {
  '<boolean>': dylan_boolean_summary,
  '<byte-string>': dylan_byte_string_summary,
  '<double-integer>': dylan_double_integer_summary,
  '<empty-list>': dylan_empty_list_summary,
  '<generic-function>': dylan_generic_function_summary,
  '<incremental-generic-function>': dylan_generic_function_summary,
  '<sealed-generic-function>': dylan_generic_function_summary,
  '<simple-object-vector>': dylan_simple_object_vector_summary,
  '<symbol>': dylan_symbol_summary,
  '<unicode-string>': dylan_unicode_string_summary
}
