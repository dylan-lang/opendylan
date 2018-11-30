#include "demangle.h"

#include <limits.h>
#include <stdbool.h>

#define CONSTANT_PREFIX         'K'
#define DYLAN_MODULE_SEP        'K'
#define MODULE_SEP              'Y'
#define LIBRARY_SEP             'V'
#define ESCAPE_SEP              'Z'
#define METHOD_MARK             'M'
#define IEP_MARK                'I'

#define ARRAY_LEN(x)            (sizeof(x)/sizeof((x)[0]))

// This is adequate for IEPs, but could be improved for other things
// See documentation/hacker-guide/source/runtime/mangling.rst
int dylan_demangle(char *dest, size_t destsize, char *src)
{
  if (destsize == 0) {
    return -1;
  }
  char *limit = dest + destsize - 1;

  static const char * const dylan_abbrev_map[] = {
    ['d'] = "dylan",
    ['i'] = "internal",
    ['p'] = "dylan-primitives",
    ['e'] = "dylan-extensions",
    ['c'] = "dylan-c-ffi",
    ['n'] = "dylan-incremental",
    ['t'] = "dylan-threads",
    ['g'] = "dispatch-engine",
    ['m'] = "machine-word-lowlevel",
  };
  static char demangle_map[1 << CHAR_BIT];
  if (demangle_map['_'] == '\0') {
    char *mapping = "-_!X$D%P*T/S<L>G?Q+A&B^C_U@O=E~N";
    while (*mapping) {
      demangle_map[(unsigned) mapping[1]] = mapping[0];
      mapping += 2;
    }
    for (int c = 'a'; c <= 'z'; ++c) {
      demangle_map[c] = c;
    }
    for (int c = '0'; c <= '9'; ++c) {
      demangle_map[c] = c;
    }
  }

  if (*src == CONSTANT_PREFIX) {
    ++src;
  }

  bool module_seen = false;
  const char *library = NULL;
  while (*src && dest < limit) {
    char c = *src++;
    if (demangle_map[(unsigned) c] != '\0') {
      *dest++ = demangle_map[(unsigned) c];
    }
    else {
      switch (c) {
      case ESCAPE_SEP:
        {
          unsigned value = 0;
          while ('0' <= *src && *src <= '9') {
            value = value * 10 + (*src++ - '0');
          }
          if (*src == ESCAPE_SEP) {
            ++src;
            *dest++ = value;
          }
          else {
            return -1;
          }
        }
        break;

      case DYLAN_MODULE_SEP:
        {
          char abbrev = *src++;
          if (0 <= abbrev && abbrev < ARRAY_LEN(dylan_abbrev_map)) {
            const char *module = dylan_abbrev_map[(unsigned) abbrev];
            if (module != NULL) {
              while (*module && dest < limit) {
                *dest++ = *module++;
              }
              const char *library = ":dylan";
              while (*library && dest < limit) {
                *dest++ = *library++;
              }
            }
          }
          else {
            return -1;
          }
          module_seen = true;
        }
        break;

      case MODULE_SEP:
        module_seen = true;
        *dest++ = ':';
        break;

      case LIBRARY_SEP:
        library = dest;
        *dest++ = ':';
        break;

      case METHOD_MARK:
      case IEP_MARK:
        if (!module_seen && library != NULL) {
          // No module seen implies the module name is the same as the
          // library name
          char *library_end = dest;
          while (library < library_end && dest < limit) {
            *dest++ = *library++;
          }
          module_seen = true;
        }
        if (c == METHOD_MARK) {
          *dest++ = '#';
        }
        break;

      default:
        return -1;
      }
    }
  }

  *dest++ = '\0';
  return 0;
}
