from dylan.accessors import *
from dylan import summaries

@summaries.register('<&class>', 'dfmc-modeling', 'dfmc-modeling')
def class_summary(value, internal_dict):
  name = dylan_slot_element_by_name(value, 'private-^debug-name')
  return dylan_string_data(name)

@summaries.register('<&raw-type>', 'dfmc-modeling', 'dfmc-modeling')
def raw_type_summary(value, internal_dict):
  name = dylan_slot_element_by_name(value, '^debug-name')
  return dylan_string_data(name)
