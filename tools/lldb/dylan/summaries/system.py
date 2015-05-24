from dylan.accessors import *
from dylan import summaries

@summaries.register('<posix-directory-locator>', 'file-system', 'system')
def posix_directory_locator_summary(value, internal_dict):
  relative = dylan_slot_element_by_name(value, 'locator-relative?')
  path = dylan_slot_element_by_name(value, 'locator-path')
  summary = ''
  if not dylan_boolean_value(relative):
    summary += '/'
  for p in dylan_vector_elements(path):
    summary += dylan_string_data(p)
    summary += '/'
  return summary

@summaries.register('<posix-file-locator>', 'file-system', 'system')
def posix_file_locator_summary(value, internal_dict):
  directory = dylan_slot_element_by_name(value, 'locator-directory')
  base = dylan_slot_element_by_name(value, 'locator-base')
  extension = dylan_slot_element_by_name(value, 'locator-extension')
  summary = ''
  if not dylan_is_false(directory):
    summary += summaries.summary(directory)
  if not dylan_is_false(base):
    summary += dylan_string_data(base)
  if not dylan_is_false(extension):
    summary += '.'
    summary += dylan_string_data(extension)
  return summary
