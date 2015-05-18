from dylan.accessors import *
from dylan import summaries

@summaries.register('<dfmc-dood>', 'dfmc-namespace', 'dfmc-namespace')
def dfmc_dood_summary(value, internal_dict):
  dood_name = dylan_slot_element_by_name(value, 'dood-name')
  return dylan_symbol_name(dood_name)
