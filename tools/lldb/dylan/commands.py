import lldb
import optparse
import shlex

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
