from dylan.accessors import *
from dylan import summaries

@summaries.register('<name-fragment>', 'dfmc-reader', 'dfmc-reader')
@summaries.register('<variable-name-fragment>', 'dfmc-reader', 'dfmc-reader')
@summaries.register('<special-variable-name-fragment>', 'dfmc-reader', 'dfmc-reader')
@summaries.register('<dylan-variable-name-fragment>', 'dfmc-reader', 'dfmc-reader')
@summaries.register('<simple-variable-name-fragment>', 'dfmc-reader', 'dfmc-reader')
@summaries.register('<simple-dylan-variable-name-fragment>', 'dfmc-reader', 'dfmc-reader')
@summaries.register('<simple-classified-variable-name-fragment>', 'dfmc-reader', 'dfmc-reader')
def name_fragment_summary(value, internal_dict):
  name = dylan_slot_element_by_name(value, 'fragment-name')
  return dylan_symbol_name(name)
