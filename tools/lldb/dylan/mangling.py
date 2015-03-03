MODULE_SEPARATOR               = "Y";
LIBRARY_SEPARATOR              = "V";
LOCAL_SUFFIX                   = "_";
HYGIENE_MARKER                 = "F";
ESCAPE_SEPARATOR               = "Z";
CONSTANT_PREFIX                = "K";
SYMBOL_PREFIX                  = "J";
INDIRECTION_PREFIX             = "I";
WRAPPER_SUFFIX                 = "W";
IEP_SUFFIX                     = "I";
METHOD_MANGLED_MARKER_STRING   = "M";
SLOT_MANGLED_MARKER_STRING     = "H";
DYLAN_MODULE_SEPARATOR         = "K";
DOMAIN_MANGLED_MARKER_STRING   = "RD_";

MANGLE_DYLAN_MODULE = {
  "dylan":                 'd',
  "internal":              'i',
  "dylan-primitives":      'p',
  "dylan-extensions":      'e',
  "dylan-c-ffi":           'c',
  "dylan-incremental":     'n',
  "dylan-threads":         't',
  "dispatch-engine":       'g',
  "machine-word-lowlevel": 'm'
}

MANGLE_SYMBOLS = {
  '-': '_',
  '!': 'X',
  '$': 'D',
  '%': 'P',
  '*': 'T',
  '/': 'S',
  '<': 'L',
  '>': 'G',
  '?': 'Q',
  '+': 'A',
  '&': 'B',
  '^': 'C',
  '_': 'U',
  '@': 'O',
  '=': 'E',
  '~': 'N',
}

def dylan_mangle_binding(binding, module, library):
  buf = []
  mangle_raw_into(buf, CONSTANT_PREFIX)
  mangle_name_into(buf, binding)
  mangle_namespace_into(buf, module, library)
  return ''.join(buf)

def dylan_mangle_wrapper(binding, module, library):
  return dylan_mangle_binding(binding, module, library) + WRAPPER_SUFFIX

def mangle_name_into(buf, name):
  for c in name:
    buf.extend(MANGLE_SYMBOLS.get(c, c.lower()))

def mangle_namespace_into(buf, module, library):
  if library == 'dylan' and module in MANGLE_DYLAN_MODULE:
    mangle_raw_into(buf, LIBRARY_SEPARATOR)
    mangle_raw_into(buf, DYLAN_MODULE_SEPARATOR)
    mangle_raw_into(buf, MANGLE_DYLAN_MODULE[module])
  else:
    if library != module:
      mangle_raw_into(buf, MODULE_SEPARATOR)
      mangle_name_into(buf, module)
    mangle_raw_into(buf, LIBRARY_SEPARATOR)
    mangle_name_into(buf, library)

def mangle_raw_into(buf, raw):
  buf.extend(raw)
