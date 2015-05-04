import lldb
import lldb.formatters.Logger
import commands
import summaries
import synthetics

# We import various summary modules here so that the code in them can
# execute the various registration functions.

# Core summaries
import summaries.core

# Compiler (DFMC) summaries
import summaries.dfmc.primitives

# LLVM summaries
import summaries.llvm.types
import summaries.llvm.constant
import summaries.llvm.function
import summaries.llvm.misc

def __lldb_init_module(debugger, internal_dict):
  debugger.HandleCommand('command script add -f dylan.commands.dylan_break_gf dylan-break-gf')
  debugger.HandleCommand('command script add -f dylan.commands.dylan_bt dylan-bt')
  debugger.HandleCommand('type format    add dylan_value -f hex')
  debugger.HandleCommand('type synthetic add dylan_value -l dylan.synthetics.SyntheticDylanValue -w dylan')
  debugger.HandleCommand('type synthetic add _K.* --regex -l dylan.synthetics.SyntheticDylanValue -w dylan')
  debugger.HandleCommand('type summary   add dylan_byte_string -F dylan.summaries.core.dylan_byte_string_summary -w dylan')
  debugger.HandleCommand('type summary   add dylan_object -F dylan.summaries.dylan_object_summary -w dylan')
  debugger.HandleCommand('type summary   add dylan_simple_object_vector -F dylan.summaries.core.dylan_simple_object_vector_summary -w dylan')
  debugger.HandleCommand('type summary   add _KL.* --regex -F dylan.summaries.dylan_object_summary -w dylan')
  debugger.HandleCommand('type summary   add dylan_symbol -F dylan.summaries.core.dylan_symbol_summary -w dylan')
  debugger.HandleCommand('type summary   add dylan_value -F dylan.summaries.dylan_value_summary -e -w dylan')
  debugger.HandleCommand('type category enable dylan')
