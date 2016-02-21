from dylan.accessors import *
from dylan import summaries

@summaries.register('<object-reference>', 'dfmc-flow-graph', 'dfmc-flow-graph')
@summaries.register('<immutable-object-reference>', 'dfmc-flow-graph', 'dfmc-flow-graph')
def object_reference_summary(value, internal_dict):
  reference_value = dylan_slot_element_by_name(value, 'reference-value')
  return summaries.summary(reference_value)
