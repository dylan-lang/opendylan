/* ********************************************************************** */
/* ** coff_relocations.c                                               ** */
/* ** Debugger-nub level utilities used by the interactive downloader  ** */
/* ** for performing relocations.                                      ** */
/* ** ---------------------------------------------------------------- ** */
/* ** Author: Paul Howard, Copyright: Functional Objects, Inc. 1996 ** */
/* **                                 All Rights Reserved.             ** */
/* ********************************************************************** */

#include "nub-core.h"

NUBINT nub_perform_absolute_relocation
   (NUB nub, TARGET_ADDRESS ra, TARGET_ADDRESS da)
{
  LPDBGPROCESS   process = (LPDBGPROCESS) nub;
  BOOL           status_read, status_write;
  DWORD          byte_count;
  DWORD          x;

  // To perform an absolute relocation, the destination address (da) has
  // to be added to the value currently stored at the relocation
  // address (ra). This value is stored in x. The result of this addition
  // has to be written back into da.

  // Do the Read.

  //printf("Abs reloc to 0x%x at 0x%x ; ", da, ra);

  status_read =
    ReadProcessMemory(process->ProcessHandle,
                      (LPCVOID) ra,
                      (LPVOID) &x,
                      sizeof(DWORD),
                      &byte_count);

  // Do the calculation.

  //printf ("Existing 0x%x ; ", x);

  x = x + ((DWORD) da);

  //printf ("Calc 0x%x ; ", x);

  // Do the Write.

  status_write =
    ValidatedWriteProcessMemory(process->ProcessHandle,
                               (LPVOID) ra,
                               (LPVOID) &x,
                               sizeof(DWORD),
                               &byte_count);

  // Return the success code.

  if (status_read && status_write) {
    //printf ("Success.\n");
    return ((NUBINT) 1);
  }
  else {
    //printf ("Failed.\n");
    return ((NUBINT) 0);
  }
}


NUBINT nub_perform_relative_relocation
   (NUB nub, TARGET_ADDRESS ra, TARGET_ADDRESS da)
{
  LPDBGPROCESS   process = (LPDBGPROCESS) nub;
  BOOL           status_read, status_write;
  DWORD          byte_count;
  DWORD          x;

  // Identical procedure to the absolute relocation, but the calculation has
  // the extra subtraction to make it relative.

  // Do the Read.

  //printf("Rel reloc to 0x%x at 0x%x ; ", da, ra);

  status_read =
    ReadProcessMemory(process->ProcessHandle,
                      (LPCVOID) ra,
                      (LPVOID) &x,
                      sizeof(DWORD),
                      &byte_count);

  // Do the calculation.

  //printf ("Existing 0x%x ; ", x);

  x = (x + ((DWORD) da)) - (((DWORD) ra) + sizeof(DWORD));

  //printf ("Calc 0x%x ; ", x);

  // Do the Write.

  status_write =
    ValidatedWriteProcessMemory(process->ProcessHandle,
                               (LPVOID) ra,
                               (LPVOID) &x,
                               sizeof(DWORD),
                               &byte_count);

  // Return the success code.

  if (status_read && status_write) {
    //printf ("Success.\n");
    return ((NUBINT) 1);
  }
  else {
    //printf ("Failed.\n");
    return ((NUBINT) 0);
  }
}
