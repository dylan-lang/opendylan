/* ********************************************************************** */
/* ** app_control.c                                                    ** */
/* ** Functions for controlling the execution of the debugee.          ** */
/* ** ---------------------------------------------------------------- ** */
/* ** Author: Paul Howard, Copyright: Functional Objects, Inc. 1996 ** */
/* **                                 All Rights Reserved.             ** */
/* ********************************************************************** */

#include "nub-core.h"

extern DWORD last_event_process;
extern DWORD last_event_thread;

void nub_application_restart (NUB nub)
{
  LPDBGPROCESS process = (LPDBGPROCESS) nub;
  LPDBGTHREAD  primary_thread = process->ThreadList;
  LPDBGTHREAD  init_thread;
  LPDBGLIBRARY init_library;
  BOOL         create_status;
  int          state;

  // For now, this function only has effect if the process
  // is being kicked-off for the first time.

  state = get_application_state(process);

  switch (state) {

  case UNSTARTED:
    // Commence execution of the process by "resuming" the
    // primary thread.

    set_application_state(process, RUNNING);
    resume_thread(primary_thread);
    break;

  case STOPPED_BY_COMMAND:
  case STOPPED_BY_EVENT:
  case STOPPED_AT_BREAKPOINT:
    // First, kill the process.

    TerminateProcess (process->ProcessHandle, 0);

    // Now release all mapped information and descriptors, apart
    // from the main process descriptor.

    about_to_kill(process);

    // And fire it up again...

    create_status 
      = CreateProcess ((LPCSTR) (process->Command),
                       (LPSTR) (process->Arguments),
                       (LPSECURITY_ATTRIBUTES) NULL,
                       (LPSECURITY_ATTRIBUTES) NULL,
                       FALSE,
                       DEBUG_PROCESS | CREATE_NEW_CONSOLE,
                       NULL,
                       NULL,
                       (LPSTARTUPINFO) 
                          &(process->StartupInfoContainer),
                       (LPPROCESS_INFORMATION) 
                          &(process->ProcessInfoContainer));

    if (!create_status)
    {
      // Error case: We could not create the debugee process.
      return;
    }

    // Copy some information into the structure for the process.

    process->ProcessID = (process->ProcessInfoContainer).dwProcessId;
    process->ProcessHandle = (process->ProcessInfoContainer).hProcess;

    // Create the linked list entry for the main thread.

    init_thread = (LPDBGTHREAD) malloc (sizeof(DBGTHREAD));
    process->ThreadList = init_thread;

    // Create the linked list entry for the main library (the exe).

    init_library = (LPDBGLIBRARY) malloc (sizeof(DBGLIBRARY));
    process->LibraryList = init_library;
    process->DebugPointList = NULL;

    // Initialize primary thread slots.

    init_thread->Next = NULL;
    init_thread->ThreadHandle = (process->ProcessInfoContainer).hThread;
    init_thread->Valid = TRUE;
    init_thread->ThreadID = (process->ProcessInfoContainer).dwThreadId;
    init_thread->ThreadContext = NULL;
    init_thread->CachedThreadContext = NULL;

    // Initialize primary library slots

    init_library->Next = NULL;
    init_library->Valid = TRUE;
    init_library->CachedTypeTableBase = FALSE;

    set_application_state(process, RUNNING);

    break;

  default:
    break;
  }
}


void nub_application_stop (NUB nub)
{
  LPDBGPROCESS  process = (LPDBGPROCESS) nub;
  LPDBGTHREAD   this_thread = process->ThreadList;

  // First, check to see that the application is running so that
  // we can stop it!

  int state = get_application_state(process);

  dylan_debugger_message("nub_application_stop", 0, 0);

  if (state != RUNNING) return;

  // Chain along all threads and suspend them.
  // Flag them as being "stopped by the debugger".

  while (this_thread != NULL)
  {
    if (!(this_thread->WaitingForDebugger)) {
      suspend_thread(this_thread);
      (this_thread->StoppedState) = THREAD_STOPPED_BY_DEBUGGER;
    }
    this_thread = this_thread->Next;
  }
  set_application_state(process, STOPPED_BY_COMMAND);

  process->PendingEventThread = (LPDBGTHREAD) NULL;
}



// Thread Contexts monitoring for debugging purposes

void report_thread_contexts(LPDBGPROCESS process)
{
  LPDBGTHREAD    thread = process->ThreadList;
  CONTEXT        context;
  int            status;

  suspend_all(process);
  Sleep(50);

  dylan_debugger_message("Thread Contexts of process:", NULL, NULL);


  while (thread != NULL) {

    context.ContextFlags = CONTEXT_FULL;
    status =  GetThreadContext(thread->ThreadHandle, &context);

    dylan_debugger_message("Thread Context: %= : %=",
			   thread->ThreadHandle, status);
    dylan_debugger_message("Esp: %=  Eip: %=",
			   (TARGET_ADDRESS) context.Esp,
			   (TARGET_ADDRESS) context.Eip);
    dylan_debugger_message("Ebp: %=",
			   (TARGET_ADDRESS) context.Ebp, NULL);
    dylan_debugger_message("Eax: %=  Ebx: %=",
			   (TARGET_ADDRESS) context.Eax,
			   (TARGET_ADDRESS) context.Ebx);
    dylan_debugger_message("Ecx: %=  Edx: %=",
			   (TARGET_ADDRESS) context.Ecx,
			   (TARGET_ADDRESS) context.Edx);
    dylan_debugger_message("Esi: %=  Edi: %=",
			   (TARGET_ADDRESS) context.Esi,
			   (TARGET_ADDRESS) context.Edi);
    thread = thread->Next;
  }

  resume_all(process);

  
}

BOOL MonitorApplicationThreads = FALSE;

void monitor_threads(LPDBGPROCESS process)
{
  int i;

  if (MonitorApplicationThreads)
    for (i = 0; i < 10; i++) {
      Sleep(200);
      report_thread_contexts(process);
    }
    
}

void continue_application (NUB nub, int handling)
{
  LPDBGPROCESS   process = (LPDBGPROCESS) nub;
  LPDBGTHREAD    master_thread = process->ThreadList;
  LPDBGTHREAD    this_thread = master_thread;
  LPDBGTHREAD    last_thread = master_thread;

  // Apply specialized "continue" behaviour to each thread in turn.

  dylan_debugger_message("nub_application_continue", 0, 0);

  // process->PendingEvent = NULL;
  set_application_state (process, RUNNING);

  /*
  this_thread = master_thread;
  while (this_thread != NULL) {

  if (this_thread != NULL)
    if (this_thread->NeedsBreakpointReplacement) {
      int status = suspend_thread(this_thread);

      resume_thread(this_thread);
      if (status == 0) {
	dylan_debugger_message("Suspending all threads for breakpoint replacement %=",
			       this_thread->ThreadHandle, 0);
	suspend_all_except(process, this_thread);
        nub_threads_continue((NUB)process);
	return;
      }
    }
  this_thread = this_thread->Next;
  }
  */

  this_thread = master_thread;
  while (this_thread != NULL) {
    apply_appropriate_continuation_to_thread
      (process, this_thread, handling);
    this_thread->StackTraceValid = FALSE;

    // Delete this thread from the thread list after continuing
    // on the thread, if its structure has been invalidated by
    // an exit-thread event; except for the master-thread, this
    // is the last event that will be processed  on that thread

    if ((this_thread != master_thread) && (!this_thread->Valid)) {
      // Drop a node out of the list.
      LPDBGTHREAD    next_thread = this_thread->Next;

      last_thread->Next = next_thread;
      dylan_debugger_message("Deleting thread %= from debugger NUB",
			     this_thread->ThreadHandle, NULL);
      free(this_thread);
      this_thread = next_thread;
    }
    else {
      // This one lives on.
      last_thread = this_thread;
      this_thread = this_thread->Next;
    }
  }

}



void nub_application_continue (NUB nub)
{
  continue_application(nub, EXCEPTIONS_HANDLED);
}


void nub_application_continue_unhandled (NUB nub)
{
  continue_application(nub, EXCEPTIONS_UNHANDLED);
}

void nub_thread_stop (NUB nub, NUBTHREAD thread)
{
  LPDBGPROCESS   process = (LPDBGPROCESS) nub;
  LPDBGTHREAD    Cthread = (LPDBGTHREAD) thread;

  dylan_debugger_message("nub_thread_stop", Cthread->ThreadHandle, 0);
  suspend_thread(Cthread);
}


void nub_thread_continue (NUB nub, NUBTHREAD thread)
{
  LPDBGPROCESS   process = (LPDBGPROCESS) nub;
  LPDBGTHREAD    Cthread = (LPDBGTHREAD) thread;

  dylan_debugger_message("nub_thread_continue", Cthread->ThreadHandle, 0);
  resume_thread(Cthread);
}

void nub_thread_suspended (NUBTHREAD thread)
{
  LPDBGTHREAD    Cthread = (LPDBGTHREAD) thread;

  // Simply flag the fact that this thread is now
  // "waiting for the debugger", so it should be excluded
  // from regular resume & suspend calls.
  // This implies that the thread has been put on permanent
  // hold by the Debugger

  dylan_debugger_message("nub_thread_suspended %=", Cthread->ThreadHandle, 0);
  (Cthread->WaitingForDebugger) = TRUE;
  
}

BOOL nub_thread_suspendedQ (NUBTHREAD thread)
{
  LPDBGTHREAD    Cthread = (LPDBGTHREAD) thread;

  return (Cthread->WaitingForDebugger);
}

void nub_thread_resumed (NUBTHREAD thread)
{
  LPDBGTHREAD    Cthread = (LPDBGTHREAD) thread;

  // Take the thread out of permanent hold by the Debugger

  dylan_debugger_message("nub_thread_resumed %=", Cthread->ThreadHandle, 0);
  (Cthread->WaitingForDebugger) = FALSE;
}


// In a multi-threaded application, the only way to guarantee that
// any particular thread will continue execution is to explicitly
// continue on all threads; this will release the one blocking thread
// on which the debugger event was last received (all the other threads
// in the application are frozen as a direct result), which will have been
// suspended before this call is made, in the process of making a spy
// call for example.
// More robust than relying on last event signalled?


void nub_threads_continue (NUB nub)
{
  LPDBGPROCESS   process = (LPDBGPROCESS) nub;
  LPDBGTHREAD    thread = process->ThreadList;

  dylan_debugger_message("Continuing all threads", 0, 0);
  while (thread != NULL) {
    continue_thread (process,
		     thread,
		     DBG_CONTINUE);
    thread = thread->Next;
  }

}


void nub_recover_breakpoint (NUB nub, NUBTHREAD thread)
{
  LPDBGPROCESS   process = (LPDBGPROCESS) nub;
  LPDBGTHREAD    Cthread = (LPDBGTHREAD) thread;
  CONTEXT        context;
  int            status;

  if (Cthread->NeedsBreakpointReplacement) {
    dylan_debugger_message("nub_recover_breakpoint %=", Cthread->ThreadHandle, 0);
    recover_from_breakpoint(process, Cthread, FALSE);

    context.ContextFlags = CONTEXT_CONTROL;
    status = GetThreadContext(Cthread->ThreadHandle, &context);

    dylan_debugger_message("Suspended Thread Context: %= : %=",
			   Cthread->ThreadHandle, status);
    dylan_debugger_message("Esp: %=  Eip: %=",
			   context.Esp,
			   context.Eip);

  }
}


// TODO:
// These three functions are just dummies - they should eventually
// be required to perform instruction-level stepping (not needed for
// develDBG). There may be the need for a design change, since these
// function do not accept NUBTHREAD parameters. (Which thread do we
// step??)


void nub_application_step (NUB nub, NUBINT n)
{
}


void nub_application_step_over (NUB nub, NUBINT n)
{
}

void nub_application_step_out (NUB nub)
{
}


// NOTE:
// TerminateProcess has crashed my entire machine when the target
// application has blocked threads.
// Try to make sure that all threads are running before shutting
// down.


void nub_register_exit_process_function (NUB nub, TARGET_ADDRESS ExitProcess)
{
  LPDBGPROCESS process = (LPDBGPROCESS) nub;

  process->ExitProcessFunction = ExitProcess;
}

extern BOOL get_next_debug_event_for_this_process
  (LPDBGPROCESS process, LPDEBUG_EVENT event, int timeout);


static TARGET_ADDRESS exit_process_args[1] = {0x0};

NUBINT nub_kill_application (NUB nub)
{
  LPDBGPROCESS process = (LPDBGPROCESS) nub;
  TARGET_ADDRESS ExitProcess = process->ExitProcessFunction;
  LPDBGTHREAD             thread = process->ThreadList;
  LPDBGTHREAD             this_thread = process->ThreadList;
  NUBHANDLE               context_cookie;
  BOOL found = FALSE;

  if (process->ExitingProcess) {
    nub_application_continue(nub);
    return (-1);
  };

  // Attempt to exit the process cleanly by running ExitProcess
  // on a suspended thread that is at a known breakpoint;
  // Only resort to TerminateProcess if this attempt fails

  while (!found && (thread != NULL)) {
    if ((thread->WaitingForDebugger) && (thread->Valid)) {
      nub_thread_resumed((NUBTHREAD)thread);
      found = TRUE;
    }
    else
      thread = thread->Next;
  }

 if ((ExitProcess == NULL) || (thread == NULL)) {
  nub_application_continue(nub);
  TerminateProcess (process->ProcessHandle, 0);
  set_application_state(process, DEAD);
  return (0);
 }
 else {

   process->ExitingProcess = TRUE;

   dylan_debugger_message("Exiting Dylan Application Thread: %=   Function: %=",
			  thread->ThreadHandle,
			  ExitProcess);

      nub_setup_function_call
	(nub,
	 (NUBTHREAD) thread,
	 ExitProcess,
	 0,
	 (TARGET_ADDRESS)NULL,
	 &context_cookie);


   // Suspend all threads except for the one running the
   // ExitProcess function. This is important otherwise
   // race conditions abound. The ExitProcess procedure
   // will wake up each thread in its own time to perform
   // an ExitThread operation.

   suspend_all_except(process, thread);

   continue_thread (process,
		    thread,
		    DBG_CONTINUE);

  return (-1);
 }

}


/*
static TARGET_ADDRESS exit_process_args[1] = {0x0};

NUBINT nub_kill_application (NUB nub)
{
  LPDBGPROCESS            process = (LPDBGPROCESS) nub;
  LPDBGLIBRARY            module = process->LibraryList;
  LPDBGTHREAD             thread = process->ThreadList;
  char                   *exit_process_symbol = "ExitProcess";
  NUBINT                  length_of_symbol = 11;
  TARGET_ADDRESS          address;
  NUBINT                  type;
  NUBINT                  is_function;
  TARGET_ADDRESS          debug_start;
  TARGET_ADDRESS          debug_end;
  NUBINT                  language;
  TARGET_ADDRESS          last_address;
  BOOL                    found = FALSE;
  NUBINT                  int_found;
  NUBHANDLE               context_cookie;

  // If we can find the function "ExitProcess" within the symbol space of
  // the application, we can target a remote call on that function, and
  // this should cause the application to exit cleanly, and generate its
  // final exit-process stop reason.
  // If we cannot find the ExitProcess function, then we call TerminateProcess,
  // which is not quite so clean.

  while ((!found) && (module != NULL)) {

    // We don't really know which DLL "ExitProcess" will be in, but we do know
    // that it will only be in one. Just search linearly until one shows up.

    int_found =
      nub_find_symbol_in_library
        (nub,
         (NUBLIBRARY) module,
         length_of_symbol,
         exit_process_symbol,
         &address, &type, &is_function, &debug_start, &debug_end, &language,
         &last_address);

    if (int_found == 1)
      found = TRUE;
    else
      module = module->Next;
 
  }

  if (!found) {
    nub_application_continue(nub);
    set_application_state(process, DEAD);
    TerminateProcess(process->ProcessHandle, 0);
    return(0);
  }
  else {
    while (thread != NULL) {
      nub_setup_function_call
        (nub,
         (NUBTHREAD) thread,
         address,
         1,
         exit_process_args,
         &context_cookie);
      thread = thread->Next;
    }
    nub_application_continue(nub);
    set_application_state(process, DEAD);
    return(0);
  }
}
*/

void nub_close_application (NUB nub)
{
  about_to_kill((LPDBGPROCESS) nub);
}

void apply_appropriate_continuation_to_thread 
  (LPDBGPROCESS process,
   LPDBGTHREAD thread,
   int handling)
{
  // Assume that the given thread is stopped for some reason.
  // Continue it in such a way as is relevant to the "stopped"
  // state.

  if (thread != NULL) {

    {

      // The thread was stopped, and now is about to continue. The method
      // of continuation depends upon _how_ it came to stop.

      switch (thread->StoppedState) {

      case THREAD_STOPPED_BY_DEBUGGER:
        // This thread was stopped explicitly, not because of
        // a stop reason. Resume it explicitly.
        resume_thread (thread);
        break;

      case THREAD_FROZEN:
        // This thread was stopped by the operating system
        // because of a stop reason being reported on
        // _another_ thread. This does not have to be resumed
        // explicitly.

        // ContinueDebugEvent on the particular thread on which the
        // event happened continues all threads.

        break;

      case THREAD_DEAD:
        // Don't do anything.
        break;

      case THREAD_STOPPED_AT_KNOWN_BREAKPOINT:
      case THREAD_INVOKED_DEBUGGER:
        // The thread stopped at a breakpoint. We always continue
        // breakpoints as handled.
        continue_thread (process,
                         thread,
                         DBG_CONTINUE);
        break;

      case THREAD_STOPPED_AT_FIRST_CHANCE_EXCEPTION:
      case THREAD_STOPPED_AT_SECOND_CHANCE_EXCEPTION:
        // This is the (only) point at which we adopt slightly
        // different behaviour according to the 'handling'
        // parameter.

        if (handling == EXCEPTIONS_UNHANDLED) {
          continue_thread (process,
                           thread,
                           DBG_EXCEPTION_NOT_HANDLED);
        }
        else {
          continue_thread (process,
                           thread,
                           DBG_CONTINUE);
        }
        break;

      case THREAD_STOPPED_AT_DEBUG_EVENT:
        // Just continue.
        continue_thread (process,
                         thread,
                         DBG_CONTINUE);
        break;

      default:
        // This is definitely an error!
        break;
      }
    }
  }
}


NUBINT nub_thread_stop_information 
  (NUB nub, 
   NUBTHREAD thread, 
   NUBINT *fchance,
   NUBINT *stopped_at_function_start,
   TARGET_ADDRESS *thread_return_address)
{
  LPDBGPROCESS   process = (LPDBGPROCESS) nub;
  LPDBGTHREAD    threadC = (LPDBGTHREAD) thread;
  CONTEXT        context;
  BOOL           status;
  DWORD          ra_holder;
  DWORD          bytes_read;
  TARGET_ADDRESS IP;

  // First, give the output parameters "failure" values.
  // (A bit pessimistic, perhaps...)

  (*fchance) = 0;
  (*stopped_at_function_start) = 0;
  (*thread_return_address) = (TARGET_ADDRESS) 0;

  context.ContextFlags = CONTEXT_FULL;

  // Get the thread's register context.

  status = GetThreadContext(threadC->ThreadHandle, &context);
  status = ReadProcessMemory(process->ProcessHandle,
                             (LPCVOID) context.Esp,
                             (LPVOID) &ra_holder,
                             sizeof(DWORD),
                             &bytes_read);

  IP = (TARGET_ADDRESS) context.Eip;

  // Our goal is to find out whether this thread is stopped exactly
  // on the first byte of a function. In this case, the address at the
  // top of the stack will be the return address, so we can at least
  // construct part of a <function-frame>, even if we don't know the
  // frame pointer.
/*
  NUBHANDLE      lookups;
  NUBLIBRARY     lib;
  NUBINT         found_symbols;
  found_symbols
    = nub_nearest_symbols(nub, IP, &lib, &lookups);

  if (found_symbols == 1) {
    NUBINT          is_function;
    TARGET_ADDRESS  entry_point;
    
    is_function = nub_symbol_is_function(nub, lookups, (NUB_INDEX) 1);
    if (is_function == 1) {
      entry_point = nub_lookup_symbol_address(nub, lookups, (NUB_INDEX) 1);
      if (entry_point == IP) {
        (*stopped_at_function_start) = 1;
        (*thread_return_address) = (TARGET_ADDRESS) ra_holder;
      }
    }
  }
*/
  if (threadC->StoppedState == THREAD_STOPPED_AT_FIRST_CHANCE_EXCEPTION)
    (*fchance) = 1;
  else
    (*fchance) = 0;

  return (threadC->NubCodeOfLastEvent);
}


NUB_ERROR nub_thread_begin_instruction_stepping
    (NUB nub, NUBTHREAD nubthread)
{
  LPDBGPROCESS          process = (LPDBGPROCESS) nub;
  LPDBGTHREAD           thread = (LPDBGTHREAD) nubthread;
  CONTEXT               context;
  BOOL                  status_get_context, status_set_context;

  context.ContextFlags = CONTEXT_FULL;

  status_get_context = GetThreadContext(thread->ThreadHandle, &context);
  context.EFlags = context.EFlags | 0x100;
  status_set_context = SetThreadContext(thread->ThreadHandle, &context);
  thread->SingleStepping = TRUE;
  if (status_get_context && status_set_context)
    return ((NUB_ERROR) 0);
  else
    return ((NUB_ERROR) 1);
}


NUB_ERROR nub_thread_end_instruction_stepping
    (NUB nub, NUBTHREAD nubthread)
{
  LPDBGPROCESS          process = (LPDBGPROCESS) nub;
  LPDBGTHREAD           thread = (LPDBGTHREAD) nubthread;
  CONTEXT               context;
  BOOL                  status_get_context, status_set_context;

  context.ContextFlags = CONTEXT_FULL;

  status_get_context = GetThreadContext(thread->ThreadHandle, &context);
  context.EFlags = context.EFlags & 0xFFFFFEFF;
  status_set_context = SetThreadContext(thread->ThreadHandle, &context);
  thread->SingleStepping = FALSE;
  if (status_get_context && status_set_context)
    return ((NUB_ERROR) 0);
  else
    return ((NUB_ERROR) 1);
}


// Threads that have been permanently suspended are excluded from
// these transient operations  on threads

int resume_thread (LPDBGTHREAD thread)
{
  if (!(thread->WaitingForDebugger)) {

    int status;

    status = ResumeThread(thread->ThreadHandle);

    dylan_debugger_message("resume_thread %= : %=", thread->ThreadHandle, status);

    if (status == 1) {
      dylan_debugger_message("Clearing cached thread context for %=",
			     thread->ThreadHandle, NULL);
      thread->ThreadContext = NULL;
    }

    return status;
  };
  return -1;
}

int suspend_thread (LPDBGTHREAD thread)
{
  if (!(thread->WaitingForDebugger)) {

    int status;

    status = SuspendThread(thread->ThreadHandle);

    dylan_debugger_message("suspend_thread %= : %=", thread->ThreadHandle, status);

    if (status == 0) {
      // Make the Thread Context active, which was cached for last
      // debug_event on this thread; this is only relevant for Windows 95

      dylan_debugger_message("Caching thread context for %=",
			     thread->ThreadHandle, NULL);
      thread->ThreadContext = thread->CachedThreadContext;
    }

    return status;
  };
  return -1;
}


// Guarantee that a thread will be put back into execution

int execute_thread (LPDBGTHREAD thread)
{
  int status = ResumeThread(thread->ThreadHandle);

  while (status > 1)
    status = ResumeThread(thread->ThreadHandle);

  dylan_debugger_message("execute_thread %= : %=", thread->ThreadHandle, status);

  thread->ThreadContext = NULL;

  return status;

}

int continue_thread
  (LPDBGPROCESS process,
   LPDBGTHREAD thread,
   int handling)
{
  int status;

  status = 
    ContinueDebugEvent (process->ProcessID,
			thread->ThreadID,
			handling);

  dylan_debugger_message("continue_thread %= : %=", thread->ThreadHandle, status);

  return status;
}

// For Windows 95 OS, thread contexts returned for suspended threads are bogus
// even though the OS claims it successfully obtained the context.
// The context is only believable when a debug-event has just been received on
// the thread in question.
// So we need to cache the thread contexts, and rely on the cache when the
// thread is resumed.

int get_thread_context
  (LPDBGPROCESS process,
   LPDBGTHREAD thread,
   LPCONTEXT context)
{
  int status;

  if ((process->PendingEventThread == thread)
      || (process->Platform == PLATFORM_WINDOWS_NT)
      || (thread->ThreadContext == NULL))
    status = GetThreadContext (thread->ThreadHandle, context);
  else {
    dylan_debugger_message("Using cached Thread Context for %=",
			   thread->ThreadHandle, NULL);
    dylan_debugger_message("Esp: %=  Eip: %=",
			   thread->ThreadContext->Esp,
			   thread->ThreadContext->Eip);
    status = 1;
    *context = *(thread->ThreadContext);
  };
  
  return status;
}
