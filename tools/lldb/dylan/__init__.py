import lldb
import lldb.formatters.Logger
import commands
import summaries
import synthetics

import summaries.core

SyntheticDylanValue = synthetics.SyntheticDylanValue
dylan_object_summary = summaries.dylan_object_summary
dylan_value_summary = summaries.dylan_value_summary

# Some core summaries
dylan_byte_string_summary = summaries.core.dylan_byte_string_summary
dylan_simple_object_vector_summary = summaries.core.dylan_simple_object_vector_summary
dylan_symbol_summary = summaries.core.dylan_symbol_summary

dylan_break_gf = commands.dylan_break_gf
dylan_bt = commands.dylan_bt

def __lldb_init_module(debugger, internal_dict):
  debugger.HandleCommand('command script add -f dylan.dylan_break_gf dylan-break-gf')
  debugger.HandleCommand('command script add -f dylan.dylan_bt dylan-bt')
  debugger.HandleCommand('type format    add dylan_value -f hex')
  debugger.HandleCommand('type synthetic add dylan_value -l dylan.SyntheticDylanValue -w dylan')
  debugger.HandleCommand('type synthetic add _K.* --regex -l dylan.SyntheticDylanValue -w dylan')
  debugger.HandleCommand('type summary   add dylan_byte_string -F dylan.dylan_byte_string_summary -w dylan')
  debugger.HandleCommand('type summary   add dylan_object -F dylan.dylan_object_summary -w dylan')
  debugger.HandleCommand('type summary   add dylan_simple_object_vector -F dylan.dylan_simple_object_vector_summary -w dylan')
  debugger.HandleCommand('type summary   add _KL.* --regex -F dylan.dylan_object_summary -w dylan')
  debugger.HandleCommand('type summary   add dylan_symbol -F dylan.dylan_symbol_summary -w dylan')
  debugger.HandleCommand('type summary   add dylan_value -F dylan.dylan_value_summary -e -w dylan')
  debugger.HandleCommand('type category enable dylan')
