/* ********************************************************************** */
/* ** misc_utils.c                                                     ** */
/* ** Miscellaneous utility functions for the debugger nub.            ** */
/* **                                                                  ** */
/* ** ---------------------------------------------------------------- ** */
/* ** Author: Paul Howard, Copyright: Functional Objects, Inc. 1996 ** */
/* **                                 All Rights Reserved              ** */
/* ********************************************************************** */

#include "nub-core.h"

char global_string_space [GLOBAL_STRING_SPACE_SIZE];

void create_library_debug_map (LPDBGPROCESS process, LPDBGLIBRARY module);

MODULED *update_process_module_list (DWORD, HANDLE, MODULED*);

void pull_image_information (LPDBGLIBRARY module);

void pull_image_header (LPDBGLIBRARY module);

BOOL push_debug_event (LPDBGPROCESS process, DEBUG_EVENT event);

MODULED *module_enumeration_descriptor_from_base_address
    (MODULED *list, DWORD base)
{
  MODULED    *search = list;

  while (search != NULL) {
    if (search->ModuleBase == base) {
      return(search);
    }
    else {
      search = search->Next;
    }
  }
  return(NULL);
}

void attempt_to_read_image_name_from_module_list
    (LPDBGPROCESS process, DWORD base, char *buf)
{
  MODULED    *mod_desc = NULL;
  int        i = 0;

  mod_desc = 
    module_enumeration_descriptor_from_base_address
       (process->ModuleDescriptors, base);

  if (mod_desc == NULL) {
    process->ModuleDescriptors =
      update_process_module_list
        (process->ProcessID, process->ProcessHandle, process->ModuleDescriptors);
    mod_desc = 
      module_enumeration_descriptor_from_base_address
         (process->ModuleDescriptors, base);
  }

  if (mod_desc != NULL) {
    while (mod_desc->ModuleName[i] != '\0') {
      buf[i] = mod_desc->ModuleName[i];
      i++;
    }
    buf[i] = '\0';
  }
  else {
    buf[0] = '\0';
  }
}

void attempt_to_read_image_name_from_process_space
    (LPDBGPROCESS process, LPVOID pointer_pointer, WORD unicode, char *buf)
{
   BOOL   status_read_pointer, status_read_string;
   DWORD  string_pointer;
   DWORD  bytes_read;

   buf[0] = '\0';

   // The operating system passes us a pointer within the process space
   // of the application. We can only read a string if the pointer
   // is nonzero, and if the string is ansi.

   if (pointer_pointer == NULL)
     return;

   // Read the pointer.

   status_read_pointer
     = ReadProcessMemory(process->ProcessHandle,
                         (LPCVOID) pointer_pointer,
                         (LPVOID) &string_pointer,
                         sizeof(DWORD),
                         &bytes_read);

   // If the pointer is NULL. Take an early exit.

   if ((!status_read_pointer) || (string_pointer == 0))
     return;

   // Otherwise, read the string. It is not documented how we determine
   // the length of the string. This implementation assumes that the string
   // is NULL-terminated, and no longer than 255 characters.

   if (unicode == 0x0000) {
     status_read_string
       = ReadProcessMemory(process->ProcessHandle,
                           (LPCVOID) string_pointer,
                           (LPVOID) buf,
                           0xff,
                           &bytes_read);
   }
   else {
     char   this_char;
     DWORD  i = 0;
     do {
       status_read_string =
         ReadProcessMemory(process->ProcessHandle, 
                           (LPCVOID) string_pointer,
                           (LPVOID) &this_char,
                           sizeof(char),
                           &bytes_read);
       string_pointer += 2;
       buf[i] = this_char;
       i++;
     } while ((status_read_string) && (this_char != '\0'));
   }

   if (!status_read_string)
     buf[0] = '\0';

}

void dispose_thread_stack_trace
   (LPDBGPROCESS process, LPDBGTHREAD thread)
{
  LPDBGFRAME       this_frame = thread->CurrentStackTrace;
  LPDBGFRAME       next_frame = NULL;

  while (this_frame != NULL) {
    next_frame = this_frame->PreviousFrame;
    free(this_frame);
    this_frame = next_frame;
  }
  (thread->CurrentStackTrace) = NULL;
  (thread->StackTraceValid) = FALSE;
}

NUBINT nub_primitive_select_low_order_bits
  (TARGET_ADDRESS addr, NUBINT i)
{
  DWORD dwAddr = (DWORD) addr;
  DWORD dwi    = (DWORD) i;
  DWORD result = 0x00000000;
  DWORD mask   = 0x00000001;
  DWORD j = 0;

  for (j = 0; j < dwi; j++) {
    result = result | mask;
    mask = mask << 1;
  };
  result = result & dwAddr;
  return ((NUBINT) result);
}

TARGET_ADDRESS nub_primitive_indexed_remote_value
  (TARGET_ADDRESS base, NUBINT i)
{
  DWORD dwBase = (DWORD) base;
  DWORD dwi = (DWORD) (i * 4);
  DWORD dwResult = dwBase + dwi;
  return ((TARGET_ADDRESS) dwResult);
}

TARGET_ADDRESS nub_primitive_byte_indexed_remote_value
  (TARGET_ADDRESS base, NUBINT i)
{
  DWORD dwBase = (DWORD) base;
  DWORD dwi = (DWORD) i;
  DWORD dwResult = dwBase + dwi;
  return ((TARGET_ADDRESS) dwResult);
}

TARGET_ADDRESS nub_primitive_integer_as_tagged_value (NUBINT value)
{
  DWORD           dw = (DWORD) value;
  DWORD           rval = (dw << 2) + 1;

  return ((TARGET_ADDRESS) rval);
}


TARGET_ADDRESS nub_primitive_character_as_tagged_value (char value)
{
  DWORD           dw = (DWORD) value;
  DWORD           rval = (dw << 2) + 2;

  return ((TARGET_ADDRESS) rval);
}


TARGET_ADDRESS nub_primitive_tagged_value_as_integer 
   (TARGET_ADDRESS value)
{
  int   signed_value = (int) value;
  int   stripped_tag = (signed_value - 1) / 4;
  return((TARGET_ADDRESS) stripped_tag);
}


char nub_primitive_tagged_value_as_character (TARGET_ADDRESS value)
{
  DWORD           dw = (DWORD) value;
  DWORD           primchar = dw >> 2;

  return ((char) primchar);
}


NUBINT nub_primitive_remote_value_as_integer_losing_precision
  (TARGET_ADDRESS value)
{
  /*
  BOOL      sign_bit;
  */
  DWORD     dw_val = (DWORD) value;
  /*
  if ((dw_val & 0x80000000) == 0x80000000) {
    sign_bit = TRUE;
    dw_val = dw_val & 0x7fffffff;
  }
  else {
    sign_bit = FALSE;
  }
  */
  dw_val = dw_val >> 3;  // There goes our precision! Bollox!
  /*
  if (sign_bit) {
    dw_val = dw_val | 0x80000000;
  }
  */
  return ((NUBINT) dw_val);
}


TARGET_ADDRESS nub_primitive_integer_as_remote_value_losing_precision
  (NUBINT value)
{
  BOOL      sign_bit;
  DWORD     dw_val = (DWORD) value;

  if ((dw_val & 0x80000000) == 0x80000000) {
    sign_bit = TRUE;
    dw_val = dw_val & 0x7fffffff;
  }
  else {
    sign_bit = FALSE;
  }

  dw_val = dw_val << 2;

  if (sign_bit) {
    dw_val = dw_val | 0x80000000;
  }

  return ((TARGET_ADDRESS) dw_val);
}


NUBTHREAD thread_descriptor_from_thread_handle (NUB nub, NUBHANDLE handle)
{
  LPDBGPROCESS    process = (LPDBGPROCESS) nub;
  LPDBGTHREAD     top_thread = process->ThreadList;

  // Chain down the list of threads until we encounter one whose
  // handle is the given handle

  while (top_thread != NULL) {
    if (handle == (NUBHANDLE) (top_thread->ThreadHandle)) {
      // Early exit
      return ((NUBTHREAD) top_thread);
    }
    else {
      top_thread = top_thread->Next;
    }
  }

  // We should never get to here!
  return (process->ThreadList);
}


NUBTHREAD thread_descriptor_from_thread_ID (NUB nub, DWORD id)
{
  LPDBGPROCESS      process = (LPDBGPROCESS) nub;
  LPDBGTHREAD       top_thread = process->ThreadList;

  while (top_thread != NULL) {
    if (id == (top_thread->ThreadID)) {
      // Early exit
      return ((NUBTHREAD) top_thread);
    }
    else {
     top_thread = top_thread->Next;
    }
  }

  // We should never get to here!
  return (process->ThreadList);
}


NUBHANDLE thread_handle_from_thread_descriptor 
  (NUB nub, 
   NUBTHREAD descriptor)
{
  // Typecast city!!
  return ((NUBHANDLE) (((LPDBGTHREAD) descriptor)->ThreadHandle));
}


NUBTHREAD thread_descriptor_from_index (NUB nub, NUB_INDEX index)
{
  LPDBGPROCESS     process = (LPDBGPROCESS) nub;
  LPDBGTHREAD      top_thread = process->ThreadList;
  int              this_thread = 1;

  while (top_thread != NULL) {
    if ((int) index == this_thread) {
      // Early exit from the function.
      return ((NUBTHREAD) top_thread);
    }
    else {
      this_thread++;
      top_thread = top_thread->Next;
    }
  }

  // We should certainly never get to here!
  return (process->ThreadList);
}


NUB_INDEX library_index_from_descriptor 
  (LPDBGPROCESS process, 
   LPDBGLIBRARY desc)
{
  LPDBGLIBRARY             top_library = process->LibraryList;
  int                      this_library = 1;

  while (top_library != NULL) {
    if (top_library == desc) {
      return ((NUB_INDEX) this_library);
    }
    else {
      this_library++;
      top_library = top_library->Next;
    }
  }
  return ((NUB_INDEX) 0);
}


LPDBGLIBRARY library_descriptor_from_index 
  (LPDBGPROCESS process, 
   int index)
{
  LPDBGLIBRARY             top_library = process->LibraryList;
  int                      this_library = 1;

  while (top_library != NULL) {
    if (index == this_library) {
      return (top_library);
    }
    else {
      this_library++;
      top_library = top_library->Next;
    }
  }
  return (process->LibraryList);
}


LPDBGLIBRARY library_descriptor_from_address 
  (LPDBGPROCESS process, 
   DWORD address)
{
  LPDBGLIBRARY              top_library = process->LibraryList;
  LPDBGLIBRARY              result = top_library;
  DWORD                     closest_base = 
                              (top_library->ImageInformation).ImageBase;
  DWORD                     base;

  while (top_library != NULL) {

    base = (top_library->ImageInformation).ImageBase;
    if ((base < address) && 
        ((base > closest_base) || (closest_base > address))) {
      closest_base = base;
      result = top_library;
    }
    else {
      top_library = top_library->Next;
    }
  }
  return(result);
}


void nub_error(int code)
{
  // Work out what to do on an error!
}


void add_thread_descriptor 
   (LPDBGPROCESS           process, 
    HANDLE                 thread_handle,
    DWORD                  thread_id,
    LPTHREAD_START_ROUTINE start_routine,
    LPVOID                 tls)
{
  LPDBGTHREAD this_thread;
  LPDBGTHREAD last_thread;
  LPDBGTHREAD new_thread;

  // One thread *must* exist, because it is the primary thread.

  this_thread = process->ThreadList;
  last_thread = this_thread;

  while (this_thread != NULL) {
    last_thread = this_thread;
    this_thread = this_thread->Next;
  }

  // last_thread now points to the last thread in the list. We add the
  // new one at this point.

  new_thread = (LPDBGTHREAD) malloc (sizeof (DBGTHREAD));
  last_thread->Next = new_thread;
  new_thread->Next = NULL;

  // Copy in the data.

  new_thread->ThreadHandle               = thread_handle;
  new_thread->ThreadContext              = NULL;
  new_thread->CachedThreadContext        = NULL;
  new_thread->ThreadLocalStorageAddress  = tls;
  new_thread->ThreadID                   = thread_id;
  new_thread->StartRoutineAddress        = start_routine;
  new_thread->Valid                      = TRUE;
  new_thread->WaitingForDebugger         = FALSE;
  new_thread->NeedsBreakpointReplacement = FALSE;
  new_thread->SingleStepping             = FALSE;
  new_thread->CurrentStackTrace          = NULL;
  new_thread->SteppingCaptureBreakpoints = NULL;
}


void add_library_descriptor 
  (LPDBGPROCESS          process,
   LOAD_DLL_DEBUG_INFO   info)
{
  LPDBGLIBRARY  this_library;
  LPDBGLIBRARY  last_library;
  LPDBGLIBRARY  new_library;


  // Create the new library first.

  new_library = (LPDBGLIBRARY) malloc (sizeof (DBGLIBRARY));
  new_library->Next = NULL;
  new_library->Valid = TRUE;
  new_library->CachedTypeTableBase = FALSE;
  new_library->DebugMap = NULL;
  new_library->SymbolFile = NULL;
  new_library->BoundaryCache.IndexNextEntry = 0;
  new_library->BoundaryCache.IndexLastAddition = 0;
  new_library->BoundaryCache.CacheFull = FALSE;
  this_library = process->LibraryList;

  // As for threads, we know that one library will already exist - the
  // image for the executable.

  while (this_library != NULL) {
    last_library = this_library;
    this_library = this_library->Next;
  }

  last_library->Next = new_library;

  (new_library->ImageInformation).ImageBase = (DWORD) info.lpBaseOfDll;
  (new_library->ImageInformation).ImageFileHandle = info.hFile;
  (new_library->SymbolHandlerWorking) = FALSE;

  if (process->ProcessAttached) {
    attempt_to_read_image_name_from_module_list
      (process, (DWORD) info.lpBaseOfDll, new_library->DefaultImageName);
    if (new_library->DefaultImageName[0] == '\0')
      attempt_to_read_image_name_from_process_space
        (process, info.lpImageName, info.fUnicode, 
         new_library->DefaultImageName);
  }
  else {
    attempt_to_read_image_name_from_process_space
      (process, info.lpImageName, info.fUnicode, new_library->DefaultImageName);
  }

  pull_image_header(new_library);
  new_library->DebugType = NOT_YET_LOADED;
}


DWORD lowest_base_address (LPDBGPROCESS process)
{
  LPDBGLIBRARY    lib = process->LibraryList;
  DWORD           lowest = lib->ImageInformation.ImageBase;
  while (lib != NULL) {
    if (lib->ImageInformation.ImageBase < lowest)
      lowest = lib->ImageInformation.ImageBase;
    lib = lib->Next;
  }
  return(lowest);
}


LPDBGLIBRARY library_descriptor_from_base_address 
  (LPDBGPROCESS process,
   DWORD base)
{
  LPDBGLIBRARY  lib = process->LibraryList;

  while (lib != NULL) {
    if ((lib->ImageInformation).ImageBase == base) {
      return (lib);
    }
    else {
      lib = lib->Next;
    }
  }
  return (process->LibraryList);
}


void ensure_debug_information_for_library
    (LPDBGPROCESS process, LPDBGLIBRARY module)
{
  BOOL success;

  if ((module == NULL) || (module->DebugType == NOT_YET_LOADED)) {
    module->DebugType = NONE;
    create_library_debug_map(process, module);
    pull_image_information(module);

    // Call DbgHelp to load up the module if the symbol handler is working.

    if ((process->SymbolHandlerWorking)
	&& (module->DebugType == NONE || module->DebugType == CODEVIEW_PDB)) {
      DWORD64 base =
        SymLoadModule64(process->ProcessHandle,
			module->ImageInformation.ImageFileHandle,
			module->DefaultImageName,
			NULL,
			(DWORD64) module->ImageInformation.ImageBase,
			0);
      if (base != 0) {
	module->SymbolHandlerWorking = 1;
      }

      memset(&(module->ImagehlpModuleStruct), 0, sizeof(IMAGEHLP_MODULE64));
      module->ImagehlpModuleStruct.SizeOfStruct = sizeof(IMAGEHLP_MODULE64);
      if (module->SymbolHandlerWorking) {
	success = 
          SymGetModuleInfo64(process->ProcessHandle,
		            (DWORD64) module->ImageInformation.ImageBase,
		            &(module->ImagehlpModuleStruct));
      }
    }
  }
}

void housekeep_for_stop_reason 
  (LPDBGPROCESS process,
   LPDEBUG_EVENT event)
{
  DEBUG_EVENT        made_up_event;

  switch (event->dwDebugEventCode) {

  case CREATE_PROCESS_DEBUG_EVENT:
    if (process->ProcessAttached) {
      process->ThreadList->ThreadHandle = event->u.CreateProcessInfo.hThread;
      process->ThreadList->ThreadID = event->dwThreadId;
    }
    (process->LibraryList->ImageInformation).ImageFileHandle 
      = event->u.CreateProcessInfo.hFile;

    (process->ThreadList->ThreadLocalStorageAddress)
      = event->u.CreateProcessInfo.lpThreadLocalBase;
    (process->LibraryList->ImageInformation).ImageBase =
       (DWORD) event->u.CreateProcessInfo.lpBaseOfImage;
    pull_image_header(process->LibraryList);
    (process->LibraryList->DebugType) = NOT_YET_LOADED;

    // Now we need to conjure up a LOAD_DLL debug event so
    // that the nub can signal it to the access path. We make
    // it up as best we can, and then push it onto the queue.

    made_up_event.dwDebugEventCode = LOAD_DLL_DEBUG_EVENT;
    made_up_event.u.LoadDll.lpBaseOfDll
        = event->u.CreateProcessInfo.lpBaseOfImage;
    made_up_event.u.LoadDll.hFile
        = event->u.CreateProcessInfo.hFile;
    made_up_event.u.LoadDll.dwDebugInfoFileOffset
        = event->u.CreateProcessInfo.dwDebugInfoFileOffset;
    made_up_event.u.LoadDll.nDebugInfoSize
        = event->u.CreateProcessInfo.nDebugInfoSize;
    made_up_event.u.LoadDll.lpImageName
        = event->u.CreateProcessInfo.lpImageName;
    made_up_event.u.LoadDll.fUnicode
        = event->u.CreateProcessInfo.fUnicode;

    push_debug_event(process, made_up_event);

    // Initialize DbgHelp's symbol handler for this process.

    SymSetOptions(SYMOPT_UNDNAME | SYMOPT_DEFERRED_LOADS);

    process->SymbolHandlerWorking =
      SymInitialize(process->ProcessHandle, NULL, FALSE);

    if (process->Platform == PLATFORM_WINDOWS_95) {
      // If we have post-mortem attached, and the OS has sent us a
      // synchronization flag, then set it now for Windows 95.

      if (process->ProcessAttached && (process->JITEventHandle != NULL)) {
        SetEvent(process->JITEventHandle);
        process->JITEventHandle = NULL;
      }
    } 

    break;

  case EXIT_PROCESS_DEBUG_EVENT:
    if (process->SymbolHandlerWorking) {
      LPDBGLIBRARY  module = process->LibraryList;

      while (module != NULL) {
        if (module->SymbolHandlerWorking) {
          SymUnloadModule64(process->ProcessHandle,
                            (DWORD64) module->ImageInformation.ImageBase);
        }
        module = module->Next;
      }
      
      SymCleanup(process->ProcessHandle);
    }
    break;

  case CREATE_THREAD_DEBUG_EVENT:
    add_thread_descriptor (process,
                           event->u.CreateThread.hThread,
                           event->dwThreadId,
                           event->u.CreateThread.lpStartAddress,
                           event->u.CreateThread.lpThreadLocalBase);
    break;

  case EXIT_THREAD_DEBUG_EVENT:
    {
       NUBTHREAD dead_thread = thread_descriptor_from_thread_ID
                                      (process, event->dwThreadId);
       LPDBGTHREAD node = (LPDBGTHREAD) dead_thread;
       node->Valid = FALSE; // Mark it for deletion.

      if (process->Platform == PLATFORM_WINDOWS_NT) {
        // If we have post-mortem attached, and the OS has sent us a
        // synchronization flag, then set it now for Windows NT.

        if (process->ProcessAttached && (process->JITEventHandle != NULL)) {
          SetEvent(process->JITEventHandle);
          process->JITEventHandle = NULL;
        }
      }
    }

    break;

  case LOAD_DLL_DEBUG_EVENT:
    add_library_descriptor (process, event->u.LoadDll);
    break;

  case UNLOAD_DLL_DEBUG_EVENT:
    break;

  case EXCEPTION_DEBUG_EVENT:
    if (event->u.Exception.ExceptionRecord.ExceptionCode 
        == EXCEPTION_BREAKPOINT) {
      set_application_state(process, STOPPED_AT_BREAKPOINT);
    }
    break;
  }
}


__int64 FileTimeToQuadWord (PFILETIME pFileTime)
{
  ULARGE_INTEGER qw;
  memcpy(&qw, pFileTime, sizeof qw);
  return qw.QuadPart;
}


int os_time_estimate_counter = 0;

int get_os_thread_cpu_time_estimate
  (LPDBGPROCESS          process,
   LPDBGTHREAD           thread)
{
  return (os_time_estimate_counter);
}

int get_os_thread_cpu_time_accurate
  (LPDBGPROCESS          process,
   LPDBGTHREAD           thread)
{
  FILETIME CreationTime, ExitTime, KernelTime, UserTime;
  double utime, ktime;
  int milliseconds;

  GetThreadTimes (thread->ThreadHandle, &CreationTime, &ExitTime,
                  &KernelTime, &UserTime);
  ktime = (double) FileTimeToQuadWord(&KernelTime) / 10000.0;
  utime = (double) FileTimeToQuadWord(&UserTime) / 10000.0;
  milliseconds = (int)ktime + (int)utime;
  return (milliseconds);
}

int get_os_wall_clock_time (LPDBGPROCESS process)
{
  FILETIME   CurrentTime;
  int        milliseconds;

  GetSystemTimeAsFileTime(&CurrentTime);
  milliseconds = (int)((double)FileTimeToQuadWord(&CurrentTime) / 10000.0);
  return(milliseconds);
}

int (*get_os_thread_cpu_time)(LPDBGPROCESS, LPDBGTHREAD);

void initialize_get_os_thread_cpu_time(void)
{
  FILETIME CreationTime, ExitTime, KernelTime, UserTime;

  if (GetThreadTimes(GetCurrentThread(), &CreationTime, &ExitTime,
                     &KernelTime, &UserTime) == FALSE
        && GetLastError() == ERROR_CALL_NOT_IMPLEMENTED)
    {
      os_time_estimate_counter = 0;
      get_os_thread_cpu_time = get_os_thread_cpu_time_estimate;
    }
  else
    get_os_thread_cpu_time = get_os_thread_cpu_time_accurate;
}

BOOL ValidatedWriteProcessMemory
  (HANDLE hProcess, LPVOID lpBaseAddress, LPVOID lpBuffer,
   DWORD nSize, LPDWORD lpNumberOfBytesRead)
{
  DWORD   dwBase = (DWORD) lpBaseAddress;
  if ((dwBase & 0x80000000) == 0x80000000) {
    OutputDebugString("WARNING: Aborted memory-write: invalid address.");
  }
  return(WriteProcessMemory(hProcess, lpBaseAddress, lpBuffer, nSize,
			    lpNumberOfBytesRead));
}

void extend_path_in_place
    (char *path, char *extension)
{
  int i = 0;
  int j = 0;

  if ((path == NULL) || (extension == NULL)) return;

  while (path[i] != '\0') i++;
  while (extension[j] != '\0') {path[i] = extension[j]; i++; j++;}
  path[i] = '\0';
}

void override_file_path
    (char *original, char *new_path, char *output, char *old_path)
{
  int i = 0; int j = 0; int k = 0; int last_sep = -1;
  if (new_path != NULL) {
    while (new_path[i] != '\0') {
      output[j] = new_path[i];
      i++; j++;
    }
  }
  while (original[k] != '\0') {
    if (original[k] == '\\')
      last_sep = k;
    k++; 
  }
  for (k = 0; k < (last_sep + 1); k++) old_path[k] = original[k];
  old_path[last_sep + 1] = '\0';
  k = last_sep + 1;
  while (original[k] != '\0') {
    output[j] = original[k];
    j++; k++;
  }
  output[j] = '\0';
}

void override_file_extension
    (char *original, char *new_extension, char *output, char *old_ext)
{
  int i = 0; int j = 0; int k = 0; int last_dot = 0;

  while (original[i] != '\0') {
    if (original[i] == '.')
      last_dot = i;
    i++;
  }
  if (original[last_dot] == '.') {
    j = last_dot + 1;
    k = 0;
    while (original[j] != '\0') {
      old_ext[k] = original[j];
      k++; j++;
    }
    old_ext[k] = '\0';
    j = last_dot + 1;
    k = 0;
    while (new_extension[k] != '\0') {
      output[j] = new_extension[k];
      j++; k++;
    }
    output[j] = '\0';
    k = 0;
    while (k <= last_dot) {
      output[k] = original[k];
      k++;
    }
  }
  else {
    old_ext[0] = '\0';
    i = 0;
    while (original[i] != '\0') {
      output[i] = original[i];
      i++;
    }
    output[i] = '.'; i++;
    j = 0;
    while (new_extension[j] != '\0') {
      output[i] = new_extension[j];
      i++; j++;
    }
    output[i] = '\0';
  }
}

char *construct_separated_string_list
    (char **strings, int n, char c)
{
  char   *final_string = NULL;
  int     final_size = 0;
  int     combined_length = 0;
  int     i = 0;
  int     j = 0;
  int     index = 0;
  char    sep = c;

  // Don't do anything if there are no strings.
  if (n == 0) return (NULL);

  // First, find the combined length of the strings.
  for (i = 0; i < n; i++) {
    j = 0;
    while (strings[i][j] != '\0') {
      j++;
      combined_length++;
    }
  }

  // The length of the string we actually need to allocate is
  // the combined length of the supplied strings, plus the number
  // of separating characters, plus the null terminator (which can
  // be considered an extra separating character, making the number
  // of separators equal to the number of strings).

  final_size = combined_length + n;
  final_string = (char*) malloc (final_size);

  // Now loop through the strings again, copying each one, replacing
  // the separator character with NUL on the final iteration.

  for (i = 0; i < n; i++) {
    if (i == (n - 1)) sep = '\0';  // Final iteration
    j = 0;
    while (strings[i][j] != '\0') {
      final_string[index] = strings[i][j];
      index++;
      j++;
    }
    final_string[index] = sep;
    index++;
  }

  // Return the string.
  return(final_string);
}


char *add_in_string(char *str, char *ptr)
{
  char         *i = ptr;
  while ((*str) != '\0') {
    (*i) = (*str);
    str++;
    i++;
  }
  return(i);
}

typedef struct _ALLOCATION_DESCRIPTOR {
  void                          *Address;
  struct _ALLOCATION_DESCRIPTOR *Next;
} ALLOCATION_DESCRIPTOR;

ALLOCATION_DESCRIPTOR *debugger_nub_allocations = NULL;

void *checked_malloc (int sz)
{
  HANDLE       heap_handle = GetProcessHeap();
  void         *ptr = HeapAlloc(heap_handle, 0, sz);
  
  if (ptr != NULL) {
    ALLOCATION_DESCRIPTOR  *new = 
       (ALLOCATION_DESCRIPTOR*) 
          HeapAlloc(heap_handle, 0, sizeof(ALLOCATION_DESCRIPTOR));
    new->Next = debugger_nub_allocations;
    new->Address = ptr;
    debugger_nub_allocations = new;
  }
  else {
    OutputDebugString("******* DEBUGGER NUB MALLOC FAILED *******");
  }
  return(ptr);
}

void checked_free (void *ptr)
{
  HANDLE      heap_handle = GetProcessHeap();
  if (ptr != NULL) {
     ALLOCATION_DESCRIPTOR       *d = debugger_nub_allocations;
     ALLOCATION_DESCRIPTOR       *f = NULL;
     while ((d != NULL) && (f == NULL)) {
       if (d->Address == ptr)
         f = d;
       else
         d = d->Next;
     }
     if (f != NULL) {
       HeapFree(heap_handle, 0, ptr);
     }
     else {
       OutputDebugString("***** NON-MALLOCED POINTER BEING FREED! ******");
       DebugBreak();
     }
  }
  else {
     OutputDebugString("****** NULL POINTER PASSED TO FREE ******");
     DebugBreak();
  }
}

