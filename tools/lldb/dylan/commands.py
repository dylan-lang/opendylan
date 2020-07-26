import argparse
import shlex

import lldb
from dylan import utils
from dylan import mangling
from dylan.accessors import *

INTERNAL_RUNTIME_FUNCTIONS = [
  'Khandle_missed_dispatchVKgI',
  'KPgf_dispatch_absentVKgI'
]

def dylan_bt(debugger, command, result, internal_dict):
  command_args = shlex.split(command)
  description = 'Print a Dylan-friendly stack trace.'
  parser = argparse.ArgumentParser(description=description, prog='dylan-bt')
  parser.add_argument('-a', '--all-frames', action='store_true', dest='all_frames', help='print all frames', default=False)

  try:
    options = parser.parse_args(command_args)
  except Exception:
    return

  target = debugger.GetSelectedTarget()
  process = target.GetProcess()

  if not process:
    print('Program is not running')
    return

  thread = process.GetSelectedThread()
    
  for frame_idx, frame in enumerate(thread):
    function = frame.GetFunction()
    language = function.GetLanguage()
    function_name = frame.GetFunctionName() or ''
    is_dylan_function = False
    if language == lldb.eLanguageTypeDylan or \
       (function_name and \
        function_name.startswith('K') and \
        function_name.endswith('I')):
      if not function_name in INTERNAL_RUNTIME_FUNCTIONS:
        is_dylan_function = True
    if not options.all_frames:
      if not function or not is_dylan_function:
        continue
    if not is_dylan_function and function_name:
      function_name = '  [' + function_name + ']'
    address = frame.GetPCAddress().GetLoadAddress(target)
    module = frame.GetModule().GetFileSpec().GetFilename()
    file_name = frame.GetLineEntry().GetFileSpec().GetFilename()
    line_number = frame.GetLineEntry().GetLine()
    fmt = '  frame #{num:<4d} {func:60s} {addr:#016x}'
    if module:
      fmt += ' {mod}'
    if file_name:
      fmt += ' at {file}:{line}'
    print (fmt.format(
      num=frame_idx,
      func=function_name,
      mod=module,
      addr=address,
      file=file_name,
      line=line_number
      ))

def dylan_break_gf(debugger, command, result, internal_dict):
  command_args = shlex.split(command)
  description = "Set a breakpoint that covers every method on the generic function."
  epilog="""
It is not possible to set GF breakpoints in libraries that haven't
been loaded yet, so it may be necessary to run the debuggee before
using this command
"""
  parser = argparse.ArgumentParser(description=description, prog='dylan-break-gf', epilog=epilog)
  parser.add_argument('gf', nargs='+', help='binding name in the form "name:module:library"')
  try:
    options = parser.parse_args(command_args)
  except Exception:
    return

  target = debugger.GetSelectedTarget()
  for arg in options.gf:
    try:
      (binding, module, library) = utils.parse_binding(arg)
      symbol_name = mangling.dylan_mangle_binding(binding, module, library)
    except utils.InvalidBindingIdentifier:
      print('Invalid binding name: %s' % (arg,))
      continue
    expression = '(dylan_value)&%s' % (symbol_name,)
    gf = target.CreateValueFromExpression(symbol_name, expression)
    if gf.GetError().Fail():
      print("No generic function %s was found." % (arg,))
      continue
    try:
      methods = dylan_generic_function_methods(gf)
    except Exception:
      print("Not able to determine methods of %s" % (arg,))
      return
    ieps = [dylan_method_iep_function(m) for m in methods]
    # Create a breakpoint for each IEP rather than a single one for
    # all IEPs since there isn't an appropriate typemap in the SWIG
    # bindings for SBTarget::BreakpointCreateByNames().
    for iep in ieps:
      target.BreakpointCreateByName(iep.name)
    print("Set breakpoints on %d entry points." % (len(ieps),))
