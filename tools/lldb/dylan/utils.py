class InvalidBindingIdentifier(Exception):
  def __init__(self, message, binding):
    super(InvalidBindingIdentifier, self).__init__(message, binding)
    self.binding = binding

def parse_binding(binding):
  parts = binding.split(':')
  if len(parts) != 3:
    raise InvalidBindingIdentifier('Invalid binding', binding)
  [name, module, library] = parts
  return [name, module, library]
