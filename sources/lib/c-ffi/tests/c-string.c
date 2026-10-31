#include <stddef.h>

char c_string_value(const char *x, size_t offset)
{
  return x[offset];
}

char c_string_value_setter(char new, char *x, size_t offset)
{
  return x[offset] = new;
}
