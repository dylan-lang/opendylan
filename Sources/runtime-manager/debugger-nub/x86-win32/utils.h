/* ************************************************************************ */
/* ** utils.h                                                            ** */
/* ** Nifty little routines used all over the debugger nub               ** */
/* ** ------------------------------------------------------------------ ** */
/* ** Author: Paul Howard  Copyright: (c) 1996 Functional Objects, Inc.  ** */
/* **                                 All Rights Reserved                ** */
/* ************************************************************************ */


void register_index_from_codeview_enumerate
  (WORD cvr, NUB_INDEX *hireg, NUB_INDEX *loreg);

void dispose_thread_stack_trace (LPDBGPROCESS process, LPDBGTHREAD thread);

NUBTHREAD thread_descriptor_from_index 
  (NUB nub, 
   NUB_INDEX thread);

NUBTHREAD thread_descriptor_from_thread_handle 
  (NUB nub, 
   NUBHANDLE handle);

NUBTHREAD thread_descriptor_from_thread_ID 
  (NUB nub, 
   DWORD id);

NUBHANDLE thread_handle_from_thread_descriptor 
  (NUB nub, 
   NUBTHREAD thread);

LPDBGLIBRARY library_descriptor_from_base_address 
  (LPDBGPROCESS process,
   DWORD base);

LPDBGLIBRARY library_descriptor_from_address 
  (LPDBGPROCESS process, 
   DWORD address);
// Given an address in the process space of the application, this function
// attempts to find which DLL this address corresponds to. Note: debug 
// information must currently be mapped for this function to work properly, 
// but a better implementation may be possible.

LPDBGLIBRARY library_descriptor_from_index 
  (LPDBGPROCESS process, 
   int index);
// Returns the library descriptor for the given library index.

NUB_INDEX library_index_from_descriptor 
  (LPDBGPROCESS process, 
   LPDBGLIBRARY desc);
// Opposite mapping to the above.

NUBINT set_application_state
  (LPDBGPROCESS process, 
   int state);

int get_application_state
  (LPDBGPROCESS process);

void nub_error 
  (int code);

void add_thread_descriptor 
  (LPDBGPROCESS process, 
   HANDLE thread_handle,
   DWORD thread_id, 
   LPTHREAD_START_ROUTINE start_routine,
   LPVOID tls);

void add_library_descriptor 
  (LPDBGPROCESS process, 
   LOAD_DLL_DEBUG_INFO info);

void housekeep_for_stop_reason 
  (LPDBGPROCESS process, 
   LPDEBUG_EVENT event);

int lift_breakpoint
  (LPDBGPROCESS process,
   LPDEBUG_POINT breakpoint);
// Performs the work of writing the original bytes back into the process
// and deleting the breakpoint instruction.

int drop_breakpoint
  (LPDBGPROCESS process, 
   LPDEBUG_POINT breakpoint);
// Performs the work of actually writing in the breakpoint instruction
// and pulling out the original bytes.

int handle_breakpoint_hit 
  (LPDBGPROCESS process,
   LPDBGTHREAD thread,
   TARGET_ADDRESS address);
// The nub callback for all encountered breakpoints. This function does
// nothing if the encountered breakpoint is not a breakpoint that was
// set by the nub in the first place.

void recover_from_breakpoint
  (LPDBGPROCESS process, LPDBGTHREAD thread, BOOL resume);
// Assuming that the thread performed a single instruction after stopping
// at a breakpoint, this re-writes the breakpoint.

void suspend_all
  (LPDBGPROCESS process);
// Suspends all threads in the process.

void suspend_all_except 
  (LPDBGPROCESS process, 
   LPDBGTHREAD thread);
// Suspends all threads in the process except for the one given as a
// parameter.

void resume_all_except 
  (LPDBGPROCESS process, 
   LPDBGTHREAD thread);
// Resumes all threads in the process except for the one given as a
// parameter.

void resume_all
  (LPDBGPROCESS process);
// resumes all threads in the process.

void apply_appropriate_continuation_to_thread 
  (LPDBGPROCESS process,
   LPDBGTHREAD thread,
   int handling);

void wait_for_stop_reason_internal
   (LPDBGPROCESS process, BOOL timeout, int timeout_value, 
    NUBINT *code, int mode);

void initialize_get_os_thread_cpu_time(void);

void initialize_get_os_process_page_fault_count(void);

// Address of the function to be called for getting a thread's cpu time.
extern int (*get_os_thread_cpu_time)(LPDBGPROCESS, LPDBGTHREAD);

// A counter used to estimate CPU times for threads
extern int os_time_estimate_counter;

// A function to combine strings. Given an array of n nul-terminated
// strings, dynamically allocate a single string that contains the
// concatenation of the array of strings (in order), separated by the
// character c.

char *construct_separated_string_list
    (char **strings, int n, char c);

// A function to override the extension on a filename with a supplied
// extension. The filename can contain any amount of pathname information.

void override_file_extension
    (char *original, char *new_extension, char *output, char *old_ext);

// This does the same thing for the path. It also records the path that
// was swapped out.

void override_file_path
    (char *original, char *new_path, char *output, char *old_path);

void extend_path_in_place
    (char *path, char *extension);

// The function called when debug information is needed for a DLL. This
// function will load whatever debug formats are available.

void ensure_debug_information_for_library
    (LPDBGPROCESS process, LPDBGLIBRARY module);
