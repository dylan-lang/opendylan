import lldb
import lldb.formatters.Logger
from dylan import commands
from dylan import summaries
from dylan import synthetics

# We import various summary modules here so that the code in them can
# execute the various registration functions.

# Core summaries
from dylan.summaries import core
from dylan.summaries import system

# Compiler (DFMC) summaries
from dylan.summaries.dfmc import flow_graph
from dylan.summaries.dfmc import reader
from dylan.summaries.dfmc import modeling
from dylan.summaries.dfmc import primitives

# LLVM summaries
from dylan.summaries.llvm import types
from dylan.summaries.llvm import constant
from dylan.summaries.llvm import function
from dylan.summaries.llvm import misc

# Testworks
from dylan.summaries import testworks

def __lldb_init_module(debugger, internal_dict):
  def cmd(name):
    pyname = name.replace("-", "_")
    helptext = "For more information run '%s -h'" % (name,)
    return 'command script add -f dylan.commands.%s %s --help "%s"' % (pyname, name, helptext)
  debugger.HandleCommand(cmd('dylan-break-gf'))
  debugger.HandleCommand(cmd('dylan-bt'))
  debugger.HandleCommand('type format    add dylan_value -f hex')
  debugger.HandleCommand('type synthetic add dylan_value -l dylan.synthetics.SyntheticDylanValue -w dylan')
  debugger.HandleCommand('type synthetic add _K.* --regex -l dylan.synthetics.SyntheticDylanValue -w dylan')
  debugger.HandleCommand('type summary   add dylan_byte_string -F dylan.summaries.core.dylan_string_summary -w dylan')
  debugger.HandleCommand('type summary   add dylan_object -F dylan.summaries.dylan_object_summary -w dylan')
  debugger.HandleCommand('type summary   add dylan_simple_object_vector -F dylan.summaries.core.dylan_simple_object_vector_summary -w dylan')
  debugger.HandleCommand('type summary   add _KL.* --regex -F dylan.summaries.dylan_object_summary -w dylan')
  debugger.HandleCommand('type summary   add dylan_symbol -F dylan.summaries.core.dylan_symbol_summary -w dylan')
  debugger.HandleCommand('type summary   add dylan_value -F dylan.summaries.dylan_value_summary -e -w dylan')
  debugger.HandleCommand('type category enable dylan')
