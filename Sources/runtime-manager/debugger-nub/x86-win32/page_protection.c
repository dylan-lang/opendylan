/* ********************************************************************** */
/* ** page_protection.c                                                ** */
/* ** Functions for checking the protection on memory pages to prevent ** */
/* ** access to memory being garbage collected.                        ** */
/* ** ---------------------------------------------------------------- ** */
/* ** Author: Paul Howard, Copyright: Functional Objects, Inc. 1996 ** */
/* **                                 All Rights Reserved              ** */
/* ********************************************************************** */

#include "nub-core.h"

NUBINT nub_page_read_permission 
  (NUB nub, 
   TARGET_ADDRESS address)
{
  LPDBGPROCESS               process = (LPDBGPROCESS) nub;
  MEMORY_BASIC_INFORMATION   memory_information;
  DWORD                      bytes_returned;

  // Perform a virtual query on the page inside the running process.

  bytes_returned = VirtualQueryEx (process->ProcessHandle,
                                   (LPVOID) address,
                                   &memory_information,
                                   sizeof(MEMORY_BASIC_INFORMATION));

  // Check the returned information structure.

  if (memory_information.Protect & PAGE_NOACCESS)
    return ((NUBINT) 0);
  else
    return ((NUBINT) 1);
}


NUBINT nub_page_write_permission 
  (NUB nub, 
   TARGET_ADDRESS address)
{
  LPDBGPROCESS               process = (LPDBGPROCESS) nub;
  MEMORY_BASIC_INFORMATION   memory_information;
  DWORD                      bytes_returned;

  // Perform a virtual query on the page inside the running process.

  bytes_returned = VirtualQueryEx (process->ProcessHandle,
                                   (LPVOID) address,
                                   &memory_information,
                                   sizeof(MEMORY_BASIC_INFORMATION));

  if ((memory_information.Protect & PAGE_NOACCESS) ||
      (memory_information.Protect & PAGE_EXECUTE_READ))
    return ((NUBINT) 0);
  else
    return ((NUBINT) 1);
}


NUBINT nub_page_execute_permission 
  (NUB nub, 
   TARGET_ADDRESS address)
{
  LPDBGPROCESS               process = (LPDBGPROCESS) nub;
  MEMORY_BASIC_INFORMATION   memory_information;
  DWORD                      bytes_returned;

  // Perform a virtual query on the page inside the running process.

  bytes_returned = VirtualQueryEx (process->ProcessHandle,
                                   (LPVOID) address,
                                   &memory_information,
                                   sizeof(MEMORY_BASIC_INFORMATION));

  // Check the returned information structure.

  if (memory_information.Protect & PAGE_NOACCESS)
    return ((NUBINT) 0);
  else
    return ((NUBINT) 1);
}


NUBINT nub_virtual_page_size
  (NUB nub)
{
  LPDBGPROCESS           process = (LPDBGPROCESS) nub;
  SYSTEM_INFO            system_info;
  DWORD                  byte_page_size;
  DWORD                  remote_value_page_size;

  GetSystemInfo(&system_info);
  byte_page_size = system_info.dwPageSize;
  remote_value_page_size = byte_page_size / 4;
  return ((NUBINT) remote_value_page_size);
}


NUBINT nub_remote_value_byte_size
  (NUB nub)
{
  LPDBGPROCESS          process = (LPDBGPROCESS) nub;
  return ((NUBINT) 4);
}


NUBINT nub_page_relative_address
    (NUB nub, TARGET_ADDRESS address, NUBINT *offset)
{
  // TODO:
  // This implementation will work, but it's not quite right.
  // I'm making use of the fact that I know a page to be 4096 bytes.
  // GetSystemInfo would tell us, but slow down this function. With
  // a bit more time, I could cache the page size after the first call.

  DWORD      dwAddress = (DWORD) address;
  (*offset) = ((NUBINT) (dwAddress % 4096));
  return ((NUBINT) (dwAddress / 4096));
}
