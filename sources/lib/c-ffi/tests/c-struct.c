#include <stddef.h>

typedef struct sub
{
  unsigned long slot_1;
  unsigned long *slot_2;
} minimal;

typedef struct max
{
  minimal max_1;
  unsigned char max_2;
  minimal max_3[7];
  /*
  unsigned max_4:4;
  unsigned max_5:12;
  */
  unsigned char max_6;
  unsigned char *max_7;
} maximal;

/*
 * check struct sizes
 */

size_t struct_minimal_size()
{
  return sizeof(minimal);
}

size_t struct_maximal_size()
{
  return sizeof(maximal);
}
