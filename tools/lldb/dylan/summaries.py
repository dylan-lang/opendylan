import lldb
from accessors import *
import synthetics

def format_machine_word_value(value):
  target = lldb.debugger.GetSelectedTarget()
  word_size = target.GetAddressByteSize()
  if word_size == 4:
    fmt = "%#010x"
  elif word_size == 8:
    fmt = "%#018x"
  else:
    fmt = "%#x"
  return fmt % value

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
  wrapper_symbol_name = dylan_object_wrapper_symbol_name(value)
  summary_func = SUMMARY_DISPATCH_TABLE.get(wrapper_symbol_name, dylan_user_defined_object_summary)
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
  string_data = string_data.replace('\r\n', '\\n')
  string_data = string_data.replace('\n', '\\n')
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

def dylan_machine_word_summary(value, internal_dict):
  machine_word_value = dylan_machine_word_value(value)
  return '{<machine-word>: %s}' % format_machine_word_value(machine_word_value)

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
  try:
    return '{%s}' % dylan_object_class_name(value)
  except:
    return '{uninitialized}'

SUMMARY_DISPATCH_TABLE = {
  'KLbooleanGVKdW': dylan_boolean_summary,
  'KLbyte_stringGVKdW': dylan_byte_string_summary,
  'KLdouble_integerGVKeW': dylan_double_integer_summary,
  'KLempty_listGVKdW': dylan_empty_list_summary,
  'KLgeneric_functionGVKdW': dylan_generic_function_summary,
  'KLincremental_generic_functionGVKdW': dylan_generic_function_summary,
  'KLmachine_wordGVKeW': dylan_machine_word_summary,
  'KLsealed_generic_functionGVKdW': dylan_generic_function_summary,
  'KLsimple_object_vectorGVKdW': dylan_simple_object_vector_summary,
  'KLsymbolGVKdW': dylan_symbol_summary,
  'KLunicode_stringGVKdW': dylan_unicode_string_summary
}
