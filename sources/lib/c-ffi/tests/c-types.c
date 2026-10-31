#include <stddef.h>

unsigned long import_unsigned_long(void)
{
  return ~0UL;
}

signed long import_signed_long(void)
{
  return -1;
}

unsigned int import_unsigned_int(void)
{
  return ~0U;
}

signed int import_signed_int(void)
{
  return -1;
}

unsigned short import_unsigned_short(void)
{
  return ~0;
}

signed short import_signed_short(void)
{
  return -1;
}

unsigned char import_unsigned_char(void)
{
  return ~0;
}

signed char import_signed_char(void)
{
  return -1;
}

size_t import_size_t(void)
{
  return ~(size_t) 0;
}

signed long import_ssize_t(void)
{
  return -1;
}
