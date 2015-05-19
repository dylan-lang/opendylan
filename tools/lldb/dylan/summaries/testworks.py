from dylan.accessors import *
from dylan import summaries

@summaries.register('<benchmark>', '%testworks', 'testworks')
@summaries.register('<test>', '%testworks', 'testworks')
def runnable_summary(value, internal_dict):
  name = dylan_slot_element_by_name(value, 'component-name')
  summary = dylan_string_data(name)
  tags = dylan_slot_element_by_name(value, 'test-tags')
  if not dylan_sequence_empty(tags):
    summary += ' (tags: '
    summary += ','.join([summaries.summary(tag) for tag in dylan_vector_elements(tags)])
    summary += ')'
  return summary

@summaries.register('<suite>', '%testworks', 'testworks')
def suite_summary(value, internal_dict):
  name = dylan_slot_element_by_name(value, 'component-name')
  return dylan_string_data(name)

@summaries.register('<tag>', '%testworks', 'testworks')
def tag_summary(value, internal_dict):
  name = dylan_slot_element_by_name(value, 'tag-name')
  return dylan_string_data(name)
