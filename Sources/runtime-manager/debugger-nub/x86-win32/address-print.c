/* *********************************************************************** */
/* ** address-print.c                                                   ** */
/* ** Code for generating printable representations of TARGET_ADDRESSes ** */
/* ** ----------------------------------------------------------------- ** */
/* ** Author: Paul Howard   Copyright: (c) 1997 Functional Objects, Inc. ** */
/* **                                   All Rights Reserved.            ** */
/* *********************************************************************** */

#include "nub-core.h"

#define RADIX_DECIMAL 10
#define RADIX_OCTAL 8
#define RADIX_BINARY 2
#define RADIX_HEXADECIMAL 16

#define PADDING_SPACE 1
#define PADDING_ZERO 2

char digit_set[] = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";

DWORD value_of_this_character (char c, DWORD radix)
{
  DWORD   i = 0;
  for (i = 0; i < radix; i++) {
    if (digit_set[i] == c)
      return (i);
  }
  return(0);
}

TARGET_ADDRESS nub_string_to_target_address
  (NUB nub,
   NUBINT size,
   char *buf,
   NUBINT radix,
   NUBINT *overflow)
{
   DWORD  result = 0;
   DWORD  dwSize = (DWORD) size;
   DWORD  dwRadix = (DWORD) radix;
   DWORD  i;

   if (dwSize > 8) {
     (*overflow) = (NUBINT) 1;
     dwSize = 8;
   }
   else {
     (*overflow) = (NUBINT) 0;
   }

   for (i = 0; i < dwSize; i++)
     result = (result * dwRadix) + value_of_this_character(buf[i], dwRadix);

   return ((TARGET_ADDRESS) result);
}


void nub_target_address_to_string
  (NUB nub,
   TARGET_ADDRESS x,
   NUBINT buf_size,
   char *buf,
   NUBINT radix,
   NUBINT padding,
   NUBINT *truncated)
{
   DWORD       value = (DWORD) x;
   DWORD       r = (DWORD) radix;
   DWORD       remainder = 0;
   DWORD       limit = (DWORD) buf_size;
   DWORD       i = limit - 1;
   DWORD       j;
   BOOL        finished = FALSE;

   (*truncated) = (NUBINT) 0;

   for (j = 0; j < limit; j++) {
     if (padding == PADDING_SPACE)
       buf[j] = ' ';
     else
       buf[j] = '0';
   }

   while (!finished) {
     remainder = value % r;
     value = value / r;
     buf[i] = digit_set[remainder];
     if (i == 0) {
       finished = TRUE;
       if (value > 0)
         (*truncated) = (NUBINT) 1;
     }
     else {
       i--;
     }
     if (value <= 0)
       finished = TRUE;
   }
}
