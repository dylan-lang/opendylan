/* ********************************************************************** */
/* ** server.c                                                         ** */
/* ** Functions for tethering to applications.                         ** */
/* ** ---------------------------------------------------------------- ** */
/* ** Author: Paul Howard, Copyright: Functional Objects, Inc. 1996 ** */
/* **                                 All Rights Reserved              ** */
/* ********************************************************************** */


#include "nub-core.h"
#include "psapi.h"
#include "tlhelp32.h"

// Quick header...
void initialize_process_exception_information (LPDBGPROCESS process);
void initialize_debug_event_queue (LPDBGPROCESS process);
int get_os_wall_clock_time (LPDBGPROCESS process);

MODULED *module_enumeration_descriptor_from_base_address
    (MODULED *list, DWORD base);

#define ENVIRONMENT_VARIABLE_BUFFER_SIZE 1024
#define ENVIRONMENT_PATH "PATH"

void extend_path_environment_variable
    (char *original_path, char *extension, char *extended_path)
{
  BOOL status_query_variable;
  BOOL status_set_variable;
  int i = 0;
  int j = 0;
  int k = 0;

  // Get the current value of the path.

  status_query_variable =
    GetEnvironmentVariable
      (ENVIRONMENT_PATH, original_path, ENVIRONMENT_VARIABLE_BUFFER_SIZE);

  // Speedy in-place string copying.

  while (extension[j] != '\0') 
    {extended_path[i] = extension[j]; i++; j++;}
  extended_path[i] = ';'; i++;
  while (original_path[k] != '\0')
    {extended_path[i] = original_path[k]; i++; k++;}
  extended_path[i] = '\0';

  // Set the extended path.

  status_set_variable =
    SetEnvironmentVariable
      (ENVIRONMENT_PATH, extended_path);

  if (!(status_query_variable && status_set_variable)) 
    OutputDebugString("NUB: Warning - Extension of search path failed.");

}

void restore_path_environment_variable
    (char *original_path)
{
  BOOL status_set_variable;
  status_set_variable = 
    SetEnvironmentVariable(ENVIRONMENT_PATH, original_path);

  if (!status_set_variable)
    OutputDebugString("NUB: Warning - Restoration of search path failed.");
}

MODULED *update_process_module_list_windows_nt 
  (DWORD process_id, HANDLE ph, MODULED *orig)
{
  HINSTANCE         psapi = LoadLibrary("PSAPI.DLL");
  HMODULE           handles_array[MODULE_DESCRIPTOR_ALLOWANCE];
  DWORD             size_filled;
  BOOL              status_enum_modules;
  DWORD             counter, num_modules;
  MODULED          *head = orig;
  MODULED          *current = orig;
  MODULED          *new = NULL;
  MODULEINFO        module_info;

  // Declare function pointers for the PSAPI entry points that we
  // need.

  BOOL (WINAPI *lpfEnumProcessModules) (HANDLE, HMODULE*, DWORD, LPDWORD);
  DWORD (WINAPI *lpfGetModuleFileNameEx) (HANDLE, HMODULE, LPTSTR, DWORD);
  BOOL (WINAPI *lpfGetModuleInformation) (HANDLE, HMODULE, LPMODULEINFO, DWORD);

  // If we cannot load PSAPI, then we are stuffed.
  if (psapi == NULL) return(orig);

  // Resolve the pre-declared function pointers using GetProcAddress and
  // these bloody typecasts!

  lpfEnumProcessModules =
    (BOOL (WINAPI*) (HANDLE, HMODULE*, DWORD, LPDWORD))
      GetProcAddress(psapi, "EnumProcessModules");

  lpfGetModuleFileNameEx =
    (DWORD (WINAPI*) (HANDLE, HMODULE, LPTSTR, DWORD))
      GetProcAddress(psapi, "GetModuleFileNameExA");

  lpfGetModuleInformation =
    (BOOL (WINAPI*) (HANDLE, HMODULE, LPMODULEINFO, DWORD))
      GetProcAddress(psapi, "GetModuleInformation");

  // Any failure means we take the early train home.
  if ((lpfEnumProcessModules == NULL) ||
      (lpfGetModuleFileNameEx == NULL) ||
      (lpfGetModuleInformation == NULL))
    return(orig);

  // Point CURRENT at the last descriptor in the list.
  if (current != NULL) {
    while (current->Next != NULL)
      current = current->Next;
  }

  status_enum_modules = 
     lpfEnumProcessModules(ph, 
                           handles_array, 
                           MODULE_DESCRIPTOR_ALLOWANCE * sizeof(HMODULE),
                           &size_filled);

  if (!status_enum_modules) return(orig);

  num_modules = size_filled / sizeof(HMODULE);
  // If this is more than we have allowed for (which is very unlikely)
  // then we will truncate the list.
  if (num_modules > MODULE_DESCRIPTOR_ALLOWANCE) {
    num_modules = MODULE_DESCRIPTOR_ALLOWANCE;
    OutputDebugString("*** DEBUGGER NUB WARNING: Truncated module list!\n");
  }

  for (counter = 0; counter < num_modules; counter++) {
    lpfGetModuleInformation
      (ph, handles_array[counter], &module_info, sizeof(MODULEINFO));
    if (module_enumeration_descriptor_from_base_address
           (head, (DWORD) module_info.lpBaseOfDll) == NULL) {
      new = (MODULED*) malloc(sizeof(MODULED));
      new->Next = NULL;
      size_filled = lpfGetModuleFileNameEx(ph, 
                                           handles_array[counter],
                                           new->ModuleName,
                                           MAX_MODULE_NAME_LENGTH);
      new->ModuleBase = (DWORD) module_info.lpBaseOfDll;

      // It is convenient that the linked list maintains the iteration
      // order, hence we use the following linking algorithm, with a
      // special case for when the newly created node is the head node.

      if (head == NULL)
        head = new;
      else
        current->Next = new;
      current = new;
    }
  }
  return(head);   
}


MODULED *update_process_module_list_windows_95 
   (DWORD process_id, HANDLE ph, MODULED *orig)
{
  MODULEENTRY32        module_information;
  HANDLE               module_snapshot;
  HINSTANCE            kernel32 = LoadLibrary("KERNEL32.DLL");
  BOOL                 status;
  MODULED             *current = orig;
  MODULED             *new = NULL;
  MODULED             *head = orig;

  // Function pointers to ToolHelp routines in kernel32.

  HANDLE (WINAPI *lpfCreateToolhelp32Snapshot) (DWORD, DWORD);
  BOOL (WINAPI *lpfModule32First) (HANDLE, LPMODULEENTRY32);
  BOOL (WINAPI *lpfModule32Next) (HANDLE, LPMODULEENTRY32);

  // If we do not have the appropriate DLL, then give up immediately.
  if (kernel32 == NULL)
    return(orig);

  // Otherwise, attempt to resolve the function pointers. Again, take an
  // early failing exit if we cannot get addresses for the functions.

  lpfCreateToolhelp32Snapshot =
    (HANDLE (WINAPI*) (DWORD, DWORD))
      GetProcAddress(kernel32, "CreateToolhelp32Snapshot");

  lpfModule32First =
    (BOOL (WINAPI*) (HANDLE, LPMODULEENTRY32))
      GetProcAddress(kernel32, "Module32First");

  lpfModule32Next =
    (BOOL (WINAPI*) (HANDLE, LPMODULEENTRY32))
      GetProcAddress(kernel32, "Module32Next");

  if ((lpfCreateToolhelp32Snapshot == NULL) ||
      (lpfModule32First == NULL) ||
      (lpfModule32Next == NULL))
    return(orig);

  // Now call ToolHelp32. Request a snapshot of the data for loaded
  // modules. Again, quit early if it fails.

  module_snapshot = 
    lpfCreateToolhelp32Snapshot(TH32CS_SNAPMODULE, process_id);

  if (module_snapshot == INVALID_HANDLE_VALUE)
    return(orig);


  // Point CURRENT at the last descriptor in the list.
  if (current != NULL) {
    while (current->Next != NULL)
      current = current->Next;
  }

  module_information.dwSize = sizeof(MODULEENTRY32);
  status = lpfModule32First(module_snapshot, &module_information);
  while (status) {
    if (module_enumeration_descriptor_from_base_address
            (head, (DWORD) module_information.modBaseAddr) == NULL) {
      DWORD    i = 0;
      new = (MODULED*) malloc (sizeof(MODULED));
      new->Next = NULL;
      while (module_information.szExePath[i] != '\0') {
        new->ModuleName[i] = module_information.szExePath[i];
        i++;
      }
      new->ModuleName[i] = '\0';
      new->ModuleBase = (DWORD) module_information.modBaseAddr;
      if (head == NULL)
        head = new;
      else
        current->Next = new;
      current = new;
    }
    module_information.dwSize = sizeof(MODULEENTRY32);
    status = lpfModule32Next(module_snapshot, &module_information);
  }
  CloseHandle(module_snapshot);
  return(head);
}

MODULED *update_process_module_list 
   (DWORD process_id, HANDLE ph, MODULED *orig)
{
  OSVERSIONINFO   platform_data;
  BOOL            status_get_platform_data;

  platform_data.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
  status_get_platform_data = GetVersionEx(&platform_data);
  if (status_get_platform_data) {
    if (platform_data.dwPlatformId == VER_PLATFORM_WIN32_NT)
      return(update_process_module_list_windows_nt(process_id, ph, orig));
    else
      return(update_process_module_list_windows_95(process_id, ph, orig));
  }
  else {
    // We have to hope this does not happen!
    return(orig);
  }
}


NUB create_and_debug_process 
  (SERVER dummy, 
   char *module_name, 
   char *arguments,
   NUBINT symbol_path_count,
   char **symbol_paths,
   NUBINT library_search_path_count,
   char **library_search_paths,
   char *working_directory,
   NUBINT create_shell)

{
  LPDBGPROCESS    process_handle = (LPDBGPROCESS) malloc (sizeof (DBGPROCESS));
  BOOL            create_status;
  DWORD           locate_status;
  DWORD           creation_flags;
  LPDBGTHREAD     init_thread;
  LPDBGLIBRARY    init_library;
  int             i = 0;
  int             j = 0;
  int             k = 0;
  char            path_component[512];
  char            name_component[512];
  char            located_name[512];
  char            *name_starts;
  char            *search_path = NULL;
  char            *default_extension = ".exe";
  char            original_path_variable[ENVIRONMENT_VARIABLE_BUFFER_SIZE];
  char            extended_path_variable[ENVIRONMENT_VARIABLE_BUFFER_SIZE];
  OSVERSIONINFO   platform_data;

  platform_data.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
  GetVersionEx(&platform_data);
  if (platform_data.dwPlatformId == VER_PLATFORM_WIN32_NT)
    process_handle->Platform = PLATFORM_WINDOWS_NT;
  else
    process_handle->Platform = PLATFORM_WINDOWS_95;

  // Handle some NUB functions which depend on the OS platform.

  initialize_get_os_thread_cpu_time();
  initialize_get_os_process_page_fault_count();

  // Searching for the executable file.
  // If the given executable name is path-qualified, then search in that
  // path. Otherwise, adopt the Win32 conventions.

  // Split the module_name into path_component and name_component. Do
  // this by searching for the position of the last backslash in
  // the name.

  j = -1;
  while (module_name[i] != '\0') {
    if (module_name[i] == '\\')
      j = i;
    i++;
  }

  // And then, if necessary, copying the pathname and filename components
  // to separate buffers.

  if (j == -1) {
    search_path = NULL;
    for (i = 0; module_name[i] != '\0'; i++) 
      name_component[i] = module_name[i];
    name_component[i] = '\0';
  }
  else {
    search_path = path_component;
    for (i = 0; i < j; i++) path_component[i] = module_name[i];
    path_component[j] = '\0';
    for (i = (j + 1); module_name[i] != '\0'; i++) {
      name_component[k] = module_name[i];
      k++;
    }
    name_component[k] = '\0';
  }

  // Call Windows to locate the executable file.

  locate_status =
     SearchPath(search_path,
                name_component,
                default_extension,
                512,
                located_name,
                &name_starts);

  if (locate_status == 0)
    return(NULL);

  // Decide on the creation flags

  if (create_shell == 1) 
    creation_flags = DEBUG_PROCESS | CREATE_SUSPENDED | CREATE_NEW_CONSOLE;
  else
    creation_flags = DEBUG_PROCESS | CREATE_SUSPENDED;

  // Make sure we remember the process name and the arguments.

  (process_handle->Command) = module_name;
  (process_handle->Arguments) = arguments;

  if (working_directory[0] == '\0')
    (process_handle->WorkingDirectory) = NULL;
  else
    (process_handle->WorkingDirectory) = working_directory;

  // Also install the set of symbol file search paths.

  process_handle->SymbolPaths =
     construct_separated_string_list
        (symbol_paths, (int) symbol_path_count, ';');

  // ... same deal with library search paths.

  process_handle->LibrarySearchPaths =
     construct_separated_string_list
        (library_search_paths, (int) library_search_path_count, ';');

  // Fill in the startup info for our new process. This is all just
  // default stuff.

  (process_handle->StartupInfoContainer).cb              = sizeof(STARTUPINFO);
  (process_handle->StartupInfoContainer).lpReserved      = NULL;
  (process_handle->StartupInfoContainer).lpDesktop       = NULL;
  (process_handle->StartupInfoContainer).lpTitle         = NULL;
  (process_handle->StartupInfoContainer).dwX             = 0;
  (process_handle->StartupInfoContainer).dwY             = 0;
  (process_handle->StartupInfoContainer).dwXSize         = 0;
  (process_handle->StartupInfoContainer).dwYSize         = 0;
  (process_handle->StartupInfoContainer).dwXCountChars   = 0;
  (process_handle->StartupInfoContainer).dwYCountChars   = 0;
  (process_handle->StartupInfoContainer).dwFillAttribute = 0;
  (process_handle->StartupInfoContainer).dwFlags
     = STARTF_FORCEONFEEDBACK | STARTF_USESHOWWINDOW;
  (process_handle->StartupInfoContainer).wShowWindow      = SW_SHOWNORMAL;
  (process_handle->StartupInfoContainer).cbReserved2      = 0;
  (process_handle->StartupInfoContainer).lpReserved2      = NULL;

 
  if (library_search_path_count != 0)
    extend_path_environment_variable
        (original_path_variable, 
         process_handle->LibrarySearchPaths,
         extended_path_variable);

  create_status = 
    CreateProcess 
      ((LPCSTR) located_name,
       (LPSTR) arguments,
       (LPSECURITY_ATTRIBUTES) NULL,
       (LPSECURITY_ATTRIBUTES) NULL,
       TRUE,
       creation_flags,
       NULL,
       process_handle->WorkingDirectory,
       (LPSTARTUPINFO) &(process_handle->StartupInfoContainer),
       (LPPROCESS_INFORMATION) &(process_handle->ProcessInfoContainer));

  if (library_search_path_count != 0)
    restore_path_environment_variable(original_path_variable);

  if (!create_status)
  {
    // Error case: We could not create the debugee process.
    free(process_handle);
    return (NULL);
  }

  // Copy some information into the structure for the process.

  process_handle->ProcessID 
    = (process_handle->ProcessInfoContainer).dwProcessId;
  process_handle->ProcessHandle 
    = (process_handle->ProcessInfoContainer).hProcess;

  // This is a launched debuggee.
  process_handle->ProcessAttached = FALSE;

  // Create the linked list entry for the main thread.

  init_thread = (LPDBGTHREAD) malloc (sizeof(DBGTHREAD));
  process_handle->ThreadList = init_thread;
  process_handle->PendingEventThread = init_thread;

  // Create the linked list entry for the main library (the exe).

  init_library = (LPDBGLIBRARY) malloc (sizeof(DBGLIBRARY));
  process_handle->LibraryList = init_library;

  process_handle->DebugPointList = NULL;
  process_handle->ModuleDescriptors = NULL;

  // Initialize the interactive code segment table.

  process_handle->InteractiveCodeSegments.FirstSegmentTable = NULL;
  process_handle->InteractiveCodeSegments.CurrentSegmentTable = NULL;

  // Initialize the hard-coded breakpoints counter to zero.

  process_handle->HardCodedBreakpointCounter = 0;

  // Initialize the debug event queue for the process.

  initialize_debug_event_queue(process_handle);

  // Initialize primary thread slots.

  init_thread->Next = NULL;
  init_thread->ThreadHandle = (process_handle->ProcessInfoContainer).hThread;
  init_thread->ThreadContext = NULL;
  init_thread->CachedThreadContext = NULL;
  init_thread->Valid = TRUE;
  init_thread->ThreadID = (process_handle->ProcessInfoContainer).dwThreadId;
  init_thread->WaitingForDebugger = FALSE;
  init_thread->SingleStepping = FALSE;
  init_thread->NeedsBreakpointReplacement = FALSE;
  init_thread->CurrentStackTrace = NULL;
  init_thread->SteppingCaptureBreakpoints = NULL;

  // Initialize primary library slots

  init_library->Next = NULL;
  init_library->Valid = TRUE;
  init_library->CachedTypeTableBase = FALSE;
  init_library->BoundaryCache.IndexNextEntry = 0;
  init_library->BoundaryCache.IndexLastAddition = 0;
  init_library->BoundaryCache.CacheFull = FALSE;
  init_library->SymbolFile = NULL;

  i = 0;
  while (located_name[i] != '\0') {
    init_library->DefaultImageName[i] = located_name[i];
    i++;
  }
  init_library->DefaultImageName[i] = '\0';

  // We have created the primary thread suspended.

  set_application_state(process_handle, UNSTARTED);

  // Initialize the data for receiving first-chance exceptions.

  initialize_process_exception_information (process_handle);

  // Set the base wallclock time.
  process_handle->LastActivationWallClockTime = 
     (NUBINT) get_os_wall_clock_time(process_handle);
  process_handle->CumulativeWallClockTime = (NUBINT) 0;

  process_handle->ExitProcessFunction = (TARGET_ADDRESS)NULL;
  process_handle->ExitingProcess = FALSE;

  // Return it as a handle.
  return ((NUB) process_handle);
}


NUB debug_active_process
  (SERVER dummy,
   NUBPROCESS nubprocess,
   NUBINT symbol_path_count,
   char **symbol_paths,
   char *system_JIT_information)
{
  LPDBGPROCESS      process_handle = (LPDBGPROCESS) malloc(sizeof(DBGPROCESS));
  LPPROCESSD        process_descriptor = (LPPROCESSD) nubprocess;
  BOOL              attach_status;
  LPDBGTHREAD       init_thread;
  LPDBGLIBRARY      init_library;
  HANDLE            hproc;
  int               i;
  OSVERSIONINFO     platform_data;

  platform_data.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
  GetVersionEx(&platform_data);
  if (platform_data.dwPlatformId == VER_PLATFORM_WIN32_NT)
    process_handle->Platform = PLATFORM_WINDOWS_NT;
  else
    process_handle->Platform = PLATFORM_WINDOWS_95;

  // Handle some NUB functions which depend on the OS platform.

  initialize_get_os_thread_cpu_time();
  initialize_get_os_process_page_fault_count();

  // Also install the set of symbol file search paths.

  process_handle->SymbolPaths =
     construct_separated_string_list
        (symbol_paths, (int) symbol_path_count, ';');

  // We may also get passed a string-ized event handle from the
  // system.

  if (system_JIT_information[0] != '\0') {
    DWORD  dwid = 0;
    int    i = 0;
    while (system_JIT_information[i] != '\0') {
      dwid = (10 * dwid) + (((DWORD) system_JIT_information[i]) - (DWORD) '0');
      i++;
    }
    process_handle->JITEventHandle = (HANDLE) dwid;
  }
  else {
    process_handle->JITEventHandle = NULL;
  }

  // Go for the attachment, and open a handle on the process. If
  // either operation fails, then bug out.

  attach_status = DebugActiveProcess(process_descriptor->ActualProcessID);
  hproc = 
    OpenProcess(PROCESS_ALL_ACCESS, FALSE, process_descriptor->ActualProcessID);

  if ((!attach_status) || (hproc == NULL)) {
    free(process_handle);
    return(NULL);
  }

  // Get an enumeration of loaded modules.
  process_handle->ModuleDescriptors = 
     update_process_module_list(process_descriptor->ActualProcessID, 
                                hproc, 
                                NULL);

  // Copy some information into the structure for the process.

  process_handle->ProcessID = process_descriptor->ActualProcessID;
  process_handle->ProcessHandle = hproc;
  process_handle->ProcessAttached = TRUE;

  // Create the linked list entry for the main thread.

  init_thread = (LPDBGTHREAD) malloc (sizeof(DBGTHREAD));
  process_handle->ThreadList = init_thread;
  process_handle->PendingEventThread = init_thread;

  // Create the linked list entry for the main library (the exe).

  init_library = (LPDBGLIBRARY) malloc (sizeof(DBGLIBRARY));
  process_handle->LibraryList = init_library;

  process_handle->DebugPointList = NULL;

  // Initialize the interactive code segment table.

  process_handle->InteractiveCodeSegments.FirstSegmentTable = NULL;
  process_handle->InteractiveCodeSegments.CurrentSegmentTable = NULL;

  // Initialize the hard-coded breakpoints counter to zero.

  process_handle->HardCodedBreakpointCounter = 0;

  // Initialize the debug event queue for the process.

  initialize_debug_event_queue(process_handle);
  // Initialize primary thread slots.

  init_thread->ThreadContext = NULL;
  init_thread->CachedThreadContext = NULL;
  init_thread->Next = NULL;
  init_thread->Valid = TRUE;
  init_thread->WaitingForDebugger = FALSE;
  init_thread->SingleStepping = FALSE;
  init_thread->NeedsBreakpointReplacement = FALSE;
  init_thread->CurrentStackTrace = NULL;
  init_thread->SteppingCaptureBreakpoints = NULL;

  // Initialize primary library slots

  init_library->Next = NULL;
  init_library->Valid = TRUE;
  init_library->CachedTypeTableBase = FALSE;
  init_library->BoundaryCache.IndexNextEntry = 0;
  init_library->BoundaryCache.IndexLastAddition = 0;
  init_library->BoundaryCache.CacheFull = FALSE;
  init_library->SymbolFile = NULL;

  i = 0;
  while (process_descriptor->ProcessName[i] != '\0') {
    init_library->DefaultImageName[i] = process_descriptor->ProcessName[i];
    i++;
  }
  init_library->DefaultImageName[i] = '\0';

  set_application_state(process_handle, RUNNING);

  // Initialize the data for receiving first-chance exceptions.

  initialize_process_exception_information (process_handle);

  // Set the base wallclock time.
  process_handle->LastActivationWallClockTime = 
     (NUBINT) get_os_wall_clock_time(process_handle);
  process_handle->CumulativeWallClockTime = (NUBINT) 0;

  process_handle->ExitProcessFunction = (TARGET_ADDRESS)NULL;
  process_handle->ExitingProcess = FALSE;

  // Return it as a handle.

  return ((NUB) process_handle);
}
