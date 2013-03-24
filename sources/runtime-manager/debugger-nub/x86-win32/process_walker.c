/* ********************************************************************** */
/* ** process_walker.c                                                 ** */
/* ** Functions for walking over the running processes.                ** */
/* ** ---------------------------------------------------------------- ** */
/* ** Author: Paul Howard, Copyright: Functional Objects, Inc. 1998 ** */
/* **                                 All Rights Reserved              ** */
/* ********************************************************************** */


#include "nub-core.h"
#include "psapi.h"
#include "tlhelp32.h"


void copy_process_id_into_buffer
  (LPPROCESSD processd)
{
  DWORD         i, j, id, r;
  char          spare[MAX_PROCESS_ID_LENGTH];
  char         *digits = "0123456789";
  BOOL          done = FALSE;
  BOOL          negative = FALSE;

  id = processd->ActualProcessID;
  i = 0;
  j = 0;

  if ((id & 0x80000000) == 0x80000000) {
    negative = TRUE;
    id = (id ^ 0xFFFFFFFF) + 0x00000001;
  }

  while (!done) {
    r = id % 10;
    id = id / 10;
    spare[i] = digits[r];
    i++;
    if (id == 0)
      done = TRUE;
  }
  if (negative) {
    spare[i] = '-';
    i++;
  }
  for (j = 0; j < i; j++)
    processd->ProcessID[j] = spare[i - j - 1];
  processd->ProcessID[i] = '\0';
  processd->ProcessIDLength = i;
}


void copy_process_name_into_buffer_windows_nt
  (LPPROCESSD processd, HANDLE ph)
{
  HINSTANCE         psapi = LoadLibrary("PSAPI.DLL");
  HMODULE           mh;
  DWORD             size_filled;
  BOOL              status_enum_modules;

  // Declare function pointers for the PSAPI entry points that we
  // need.

  BOOL (WINAPI *lpfEnumProcessModules) (HANDLE, HMODULE*, DWORD, LPDWORD);
  DWORD (WINAPI *lpfGetModuleFileNameEx) (HANDLE, HMODULE, LPTSTR, DWORD);

  // In case we exit early, fill the buffer with safe stuff.
  processd->ProcessName[0] = '?';
  processd->ProcessName[1] = '?';
  processd->ProcessName[2] = '\0';
  processd->ProcessNameLength = 2;

  // If we cannot load PSAPI, then we are stuffed.
  if (psapi == NULL) return;

  // Resolve the pre-declared function pointers using GetProcAddress and
  // these bloody typecasts!

  lpfEnumProcessModules =
    (BOOL (WINAPI*) (HANDLE, HMODULE*, DWORD, LPDWORD))
      GetProcAddress(psapi, "EnumProcessModules");

  lpfGetModuleFileNameEx =
    (DWORD (WINAPI*) (HANDLE, HMODULE, LPTSTR, DWORD))
      GetProcAddress(psapi, "GetModuleFileNameExA");

  // Any failure means we take the early train home.
  if ((lpfEnumProcessModules == NULL) ||
      (lpfGetModuleFileNameEx == NULL))
    return;

  // Enumerate the modules (image files) that are part of the process.
  // The PSAPI documentation tells us that the first entry in the
  // enumeration is going to be the EXE file for the process, therefore
  // we restrict the enumeration to that one entry, and obtain its
  // name.

  status_enum_modules = 
     lpfEnumProcessModules(ph, 
                           &mh, 
                           sizeof(HMODULE),
                           &size_filled);

  if (!status_enum_modules) return;

  size_filled = lpfGetModuleFileNameEx(ph, 
                                       mh,
                                       processd->ProcessName,
                                       MAX_MODULE_NAME_LENGTH);
  processd->ProcessNameLength = size_filled;
}

PROCESSD *find_equivalent_process_descriptor (PROCESSD *target, PROCESSD *head)
{
  PROCESSD    *search = head;

  while (search != NULL) {
    if (search->ActualProcessID == target->ActualProcessID) {
      return(search);  // TODO: Strengthen comparison. Look at names, too.
    }
    search = search->Next;
  }
  return(NULL);
}

PROCESSD *canonicalize_process_list_windows_95 (PROCESSD *head, DWORD *count)
{
  PROCESSENTRY32       process_information;
  HANDLE               process_snapshot;
  HINSTANCE            kernel32 = LoadLibrary("KERNEL32.DLL");
  BOOL                 status;
  PROCESSD            *current = NULL;
  PROCESSD            *new = NULL;
  PROCESSD            *new_head = NULL;
  PROCESSD            *maybe_existing = NULL;
  DWORD                my_good_self = GetCurrentProcessId();

  // Function pointers to ToolHelp routines in kernel32.

  HANDLE (WINAPI *lpfCreateToolhelp32Snapshot) (DWORD, DWORD);
  BOOL (WINAPI *lpfProcess32First) (HANDLE, LPPROCESSENTRY32);
  BOOL (WINAPI *lpfProcess32Next) (HANDLE, LPPROCESSENTRY32);

  // If we do not have the appropriate DLL, then give up immediately.
  if (kernel32 == NULL)
    return(NULL);

  // Otherwise, attempt to resolve the function pointers. Again, take an
  // early failing exit if we cannot get addresses for the functions.

  lpfCreateToolhelp32Snapshot =
    (HANDLE (WINAPI*) (DWORD, DWORD))
      GetProcAddress(kernel32, "CreateToolhelp32Snapshot");

  lpfProcess32First =
    (BOOL (WINAPI*) (HANDLE, LPPROCESSENTRY32))
      GetProcAddress(kernel32, "Process32First");

  lpfProcess32Next =
    (BOOL (WINAPI*) (HANDLE, LPPROCESSENTRY32))
      GetProcAddress(kernel32, "Process32Next");

  if ((lpfCreateToolhelp32Snapshot == NULL) ||
      (lpfProcess32First == NULL) ||
      (lpfProcess32Next == NULL))
    return(NULL);

  // Now call ToolHelp32. Request a snapshot of the data for running
  // processes. Again, quit early if it fails.

  process_snapshot = 
    lpfCreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);

  if (process_snapshot == INVALID_HANDLE_VALUE)
    return(NULL);

  (*count) = 0;
  process_information.dwSize = sizeof(PROCESSENTRY32);
  status = lpfProcess32First(process_snapshot, &process_information);
  while (status) {
    if (process_information.th32ProcessID != my_good_self) {
      DWORD  i = 0;
      new = (PROCESSD*) malloc (sizeof(PROCESSD));
      new->ActualProcessID = process_information.th32ProcessID;
      copy_process_id_into_buffer(new);
      while (process_information.szExeFile[i] != '\0') {
        new->ProcessName[i] = process_information.szExeFile[i];
        i++;
      }
      new->ProcessName[i] = '\0';
      new->ProcessNameLength = i;
      maybe_existing = find_equivalent_process_descriptor(new, head);
      if (maybe_existing != NULL) {
        free(new);
        new = maybe_existing;
      }
      new->Mark = TRUE;
      new->Next = NULL;
      if (new_head == NULL)
        new_head = new;
      else
        current->Next = new;
      current = new;
      (*count)++;
    }
    process_information.dwSize = sizeof(PROCESSENTRY32);
    status = lpfProcess32Next(process_snapshot, &process_information);
  }
  CloseHandle(process_snapshot);
  return(new_head);
}

PROCESSD *canonicalize_process_list_windows_nt (PROCESSD *head, DWORD *count)
{
  HINSTANCE   psapi = LoadLibrary("PSAPI.DLL");
  HANDLE      ph;
  DWORD       *process_id_array;
  DWORD       size_allocated, size_needed, i;
  DWORD       num_processes;
  DWORD       num_inaccessible = 0;
  DWORD       num_processes_guess = 256;
  BOOL        status_enum_processes;
  PROCESSD   *current = NULL;
  PROCESSD   *new = NULL;
  PROCESSD   *new_head = NULL;
  PROCESSD   *maybe_existing = NULL;
  DWORD       my_good_self = GetCurrentProcessId();

  BOOL (WINAPI *lpfEnumProcesses) (DWORD*, DWORD, DWORD*);

  if (psapi == NULL)
    return(NULL);

  lpfEnumProcesses =
    (BOOL (WINAPI*) (DWORD*, DWORD, DWORD*))
       GetProcAddress(psapi, "EnumProcesses");

  if (lpfEnumProcesses == NULL)
    return(NULL);

  size_allocated = num_processes_guess * sizeof(DWORD);
  process_id_array = (DWORD*) malloc (size_allocated);
  status_enum_processes = 
    lpfEnumProcesses
      (process_id_array, num_processes_guess * sizeof(DWORD), &size_needed);
  num_processes = size_needed / sizeof(DWORD);

  // There will probably have been ample room in the allocated array,
  // but if it turns out that an overflow occurred, then we must re-allocate
  // the array, and call the enumerator again.

  if (num_processes > num_processes_guess) {
    free(process_id_array);
    size_allocated = num_processes * sizeof(DWORD);
    process_id_array = (DWORD*) malloc (size_allocated);
    status_enum_processes = 
      lpfEnumProcesses
        (process_id_array, num_processes * sizeof(DWORD), &size_needed);
  }

  for (i = 0; i < num_processes; i++) {
    ph = OpenProcess(PROCESS_QUERY_INFORMATION | PROCESS_VM_READ,
                     FALSE,
                     process_id_array[i]);
    if ((ph != NULL) && (process_id_array[i] != my_good_self)) {
      new = (PROCESSD*) malloc (sizeof(PROCESSD));
      new->ActualProcessID = process_id_array[i];
      copy_process_name_into_buffer_windows_nt(new, ph);
      copy_process_id_into_buffer(new);
      maybe_existing = find_equivalent_process_descriptor(new, head);
      if (maybe_existing != NULL) {
        free(new);
        new = maybe_existing;
      }
      CloseHandle(ph);
      new->Next = NULL;
      new->Mark = TRUE;
      if (new_head == NULL)
        new_head = new;
      else
        current->Next = new;
      current = new;
    }
    else {
      num_inaccessible++;
    }
  }

  free(process_id_array);
  (*count) = (num_processes - num_inaccessible);
  return(new_head);
}

PROCESSD *canonicalize_process_list (PROCESSD *head, DWORD *count)
{
  PROCESSD       *current, *dead;
  BOOL           status_get_platform_data;
  OSVERSIONINFO  platform_data;
  PROCESSD       *new_head;

  (*count) = 0;

  // First, mark all descriptors in the existing list as unvisited.

  current = head;
  while(current != NULL) {
    current->Mark = FALSE;
    current = current->Next;
  }

  platform_data.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
  status_get_platform_data = GetVersionEx(&platform_data);
  if (status_get_platform_data) {
    if (platform_data.dwPlatformId == VER_PLATFORM_WIN32_NT)
      new_head = canonicalize_process_list_windows_nt(head, count);
    else
      new_head = canonicalize_process_list_windows_95(head, count);
  } 
  else {
    // We have to hope this does not happen!
    new_head = NULL;
  }

  // Finally, free all unvisited descriptors in the original list, so as
  // not to leak storage.

  current = head;
  while (current != NULL) {
    if (!current->Mark) {
      dead = current;
      current = current->Next;
      free(dead);
    }
    else {
      current = current->Next;
    }
  }
  return(new_head);
}
