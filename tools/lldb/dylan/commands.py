import lldb
import mangling
import optparse
import shlex
import utils

from accessors import *

INTERNAL_RUNTIME_FUNCTIONS = [
  'Khandle_missed_dispatchVKgI',
  'KPgf_dispatch_absentVKgI'
]

def dylan_bt(debugger, command, result, internal_dict):
  command_args = shlex.split(command)
  usage = 'usage: %prog [options]'
  description = 'Print a Dylan-friendly stack trace.'
  parser = optparse.OptionParser(description=description, prog='dylan-bt', usage=usage)
  parser.add_option('-a', '--all-frames', action='store_true', dest='all_frames', help='print all frames', default=False)

  try:
    (options, args) = parser.parse_args(command_args)
  except:
    return

  target = debugger.GetSelectedTarget()
  process = target.GetProcess()
  thread = process.GetSelectedThread()
  for frame_idx, frame in enumerate(thread):
    function = frame.GetFunctionName()
    function_name = frame.GetFunctionName() or ''
    is_dylan_function = False
    if function_name and function_name.startswith('K') and function_name.endswith('I'):
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
    print fmt.format(
      num=frame_idx,
      func=function_name,
      mod=module,
      addr=address,
      file=file_name,
      line=line_number
      )

def dylan_break_gf(debugger, command, result, internal_dict):
  command_args = shlex.split(command)
  usage = 'usage: %prog [gf]'
  description = 'Set a breakpoint that covers every method on the generic function.'
  parser = optparse.OptionParser(description=description, prog='dylan-break-gf', usage=usage)

  try:
    (options, args) = parser.parse_args(command_args)
  except:
    return

  target = debugger.GetSelectedTarget()
  for arg in args:
    try:
      (binding, module, library) = utils.parse_binding(arg)
      symbol_name = mangling.dylan_mangle_binding(binding, module, library)
    except utils.InvalidBindingIdentifier:
      print 'Invalid binding name: %s' % arg
      continue
    expression = '(dylan_value)&%s' % symbol_name
    gf = target.CreateValueFromExpression(symbol_name, expression)
    if gf.GetError().Fail():
      print "No generic function %s was found." % arg
      continue
    methods = dylan_generic_function_methods(gf)
    ieps = [dylan_method_iep_function(m) for m in methods]
    # Create a breakpoint for each IEP rather than a single one for
    # all IEPs since there isn't an appropriate typemap in the SWIG
    # bindings for SBTarget::BreakpointCreateByNames().
    for iep in ieps:
      target.BreakpointCreateByName(iep.name)
    print "Set breakpoints on %d entry points." % len(ieps)
