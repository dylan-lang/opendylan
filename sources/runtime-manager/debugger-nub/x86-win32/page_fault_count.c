/* ********************************************************************** */
/* ** misc_utils.c                                                     ** */
/* ** Getting the page fault count for a process                       ** */
/* **                                                                  ** */
/* ** ---------------------------------------------------------------- ** */
/* ** Author: Keith Dennison                                           ** */
/* ** Copyright: Functional Objects, Inc. 1997, All Rights Reserved ** */
/* ********************************************************************** */

#include "nub-core.h"
#include "psapi.h"

FARPROC lpfnGetProcessMemoryInfo;

void initialize_get_os_process_page_fault_count(void)
{
  HINSTANCE hModule;

  hModule = LoadLibrary("PSAPI.DLL");
  if (hModule != NULL)
    lpfnGetProcessMemoryInfo = GetProcAddress(hModule, "GetProcessMemoryInfo");
}

NUBINT nub_get_process_page_fault_count
  (NUB nub)
{
  LPDBGPROCESS         process = (LPDBGPROCESS) nub;

  if (lpfnGetProcessMemoryInfo != NULL) {
    PROCESS_MEMORY_COUNTERS pmc;
    (*lpfnGetProcessMemoryInfo)(process->ProcessHandle, &pmc,
                                (DWORD)sizeof(pmc));
    return((NUBINT)(pmc.PageFaultCount));
  }
  else {
    return ((NUBINT)0);
  }
}
