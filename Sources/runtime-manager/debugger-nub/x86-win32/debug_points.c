/* ********************************************************************** */
/* ** debug_points.c                                                   ** */
/* ** Functions for supporting breakpoints and watchpoints             ** */
/* ** ---------------------------------------------------------------- ** */
/* ** Author: Paul Howard, Copyright: Functional Objects, Inc. 1996 ** */
/* **                                 All Rights Reserved              ** */
/* ********************************************************************** */

#include "nub-core.h"

/*

  HOW BREAKPOINTS ARE IMPLEMENTED IN THE DEBUGGER NUB.
  ----------------------------------------------------

*/

int primitive_drop_breakpoint (HANDLE process, DWORD address, BYTE *holder);
// Performs the work of actually writing in the breakpoint instruction
// and pulling out the original bytes.

int primitive_lift_breakpoint (HANDLE process, DWORD address, BYTE *holder);
// Performs the work of writing the original bytes back into the process
// and deleting the breakpoint instruction.

LPDEBUG_POINT breakpoint_from_address 
  (LPDBGPROCESS process, TARGET_ADDRESS address);
// Tries to find a breakpoint instance whose breakpoint is positioned at
// the given address (else returns NULL).

void set_application_breakpoint (LPDBGPROCESS process, DWORD address);
// Inserts an APPLICATION_BREAKPOINT at the given address inside the
// application.

void handle_application_breakpoint 
  (LPDBGPROCESS process, LPDBGTHREAD thread, LPDEBUG_POINT breakpoint);
// Performs specific handling for an APPLICATION_BREAKPOINT. (This involves
// advancing one instruction from the breakpoint, and then replacing it).

void handle_stepping_capture_breakpoint 
  (LPDBGPROCESS process, LPDBGTHREAD thread, LPDEBUG_POINT breakpoint);
// Performs specific handling for a SOURCE_STEP breakpoint.

void rewind_thread_eip (LPDBGPROCESS process, LPDBGTHREAD thread);
// Rewinds the PC for the current thread by the size of the breakpoint
// function. (This is currently a single byte - the breakpoint instruction
// is 0xcc. In fact, if the breakpoint was any larger, it could potentially
// obscure more than one machine instruction. That would completely screw
// up this implementation!).

void prepare_to_single_step_for_breakpoint_recovery
   (LPDBGPROCESS process, LPDBGTHREAD thread);
// Puts the thread into single-instruction-step mode in order to allow
// an overwritten breakpoint to be replaced.

DWORD thread_return_address (LPDBGPROCESS process, LPDBGTHREAD thread);
// If the thread is about to execute a RET, this function returns the
// address that control is going to be transferred to.
// The thread context is NOT side-effected by this function.

DWORD thread_return_address (LPDBGPROCESS process, LPDBGTHREAD thread)
{
  BOOL     status;
  DWORD    return_address;
  CONTEXT  context;
  DWORD    stack_pointer;
  DWORD    bytes_read;

  context.ContextFlags = CONTEXT_CONTROL;
  status = GetThreadContext(thread->ThreadHandle, &context);

  if (!status) {
    // Internal error.
    return (0);
  }

  stack_pointer = context.Esp;

  // The return address will be popped from the top of the stack for
  // a RET, so grab this value.

  status = ReadProcessMemory (process->ProcessHandle,
                              (LPCVOID) stack_pointer,
                              (LPVOID) &return_address,
                              sizeof(DWORD),
                              &bytes_read);

  if ((!status) || (bytes_read != sizeof(DWORD))) {
    // Internal error.
    return (0);
  }
  return (return_address);
}


void suspend_all_except (LPDBGPROCESS process, LPDBGTHREAD thread)
{
  LPDBGTHREAD this_thread = process->ThreadList;

  while (this_thread != NULL) {

    if (this_thread != thread)
	suspend_thread(this_thread);

    this_thread = this_thread->Next;
  }
}

void suspend_all (LPDBGPROCESS process)
{
  LPDBGTHREAD this_thread = process->ThreadList;

  while (this_thread != NULL) {

    suspend_thread(this_thread);

    this_thread = this_thread->Next;
  }
}


void resume_all_except (LPDBGPROCESS process, LPDBGTHREAD thread)
{
  LPDBGTHREAD this_thread = process->ThreadList;

  while (this_thread != NULL) {

    if (this_thread != thread) 
	resume_thread(this_thread);
 
    this_thread = this_thread->Next;
  }
}

void resume_all (LPDBGPROCESS process)
{
  LPDBGTHREAD this_thread = process->ThreadList;

  while (this_thread != NULL) {

    resume_thread(this_thread);
 
    this_thread = this_thread->Next;
  }
}


BOOL application_breakpoint_exists 
  (LPDBGPROCESS process, TARGET_ADDRESS address);


void clear_application_breakpoint (LPDBGPROCESS process, DWORD address);


void lift_all_stepping_capture_breakpoints
    (LPDBGPROCESS process, LPDBGTHREAD thread)
{
  LPDEBUG_POINT this_one = thread->SteppingCaptureBreakpoints;
  LPDEBUG_POINT next_one = NULL;

  while (this_one != NULL) {
    next_one = this_one->Next;
    lift_breakpoint(process, this_one);
    free(this_one);
    this_one = next_one;
  }
  thread->SteppingCaptureBreakpoints = NULL;
}

void clear_application_breakpoint (LPDBGPROCESS process, DWORD address)
{
  LPDEBUG_POINT this_debug_point, last_debug_point;
  LPDBGTHREAD   this_thread;

  if (!process->ExitingProcess) {

  dylan_debugger_message("clear_application_breakpoint %=", address, 0);

  this_debug_point = process->DebugPointList;
  this_thread = process->ThreadList;
  last_debug_point = NULL;

  while (this_debug_point != NULL) {

    if ((this_debug_point->DebugPointType == DBG_POINT_BREAKPOINT) &&
        (this_debug_point->u.Breakpoint.Type == APPLICATION_BREAKPOINT) &&
        (this_debug_point->u.Breakpoint.Address == address)) {

      // Lift out the breakpoint.

      lift_breakpoint(process, this_debug_point);

      // One or more threads might actually be stopped at this breakpoint.
      // If so, they must be stopped from writing it back into the
      // process.

      this_thread = process->ThreadList;
      while (this_thread != NULL) {
        CONTEXT  context;
        if (this_thread->NeedsBreakpointReplacement) {

          // This thread is waiting at a breakpoint. If that breakpoint
          // is the one we're removing now, we have to hack its state.

          if (this_thread->BreakpointToReplace == this_debug_point) {

            dylan_debugger_message("Thread no longer needs breakpoint replacement %= %=",
                                   this_thread->ThreadHandle, address);

            this_thread->NeedsBreakpointReplacement = FALSE;

            // The thread will have been put into single-step mode in order
            // to recover from the breakpoint. But it might also be in
            // single-step mode anyway! If it isn't, take it out of
            // single-step mode.

            if (!(this_thread->SingleStepping)) {
              context.ContextFlags = CONTEXT_CONTROL;
              get_thread_context(process, this_thread, &context);
              context.EFlags = context.EFlags & 0xFFFFFEFF;
              SetThreadContext(this_thread->ThreadHandle, &context);
              // resume_all_except(process, this_thread);
            }
          }
        }
        this_thread = this_thread->Next;
      }

      // Delete the descriptor from the list.

      if (last_debug_point == NULL) {
        // This shouldn't happen, but we might as well handle it.
        (process->DebugPointList) = this_debug_point->Next;
        free(this_debug_point);
        this_debug_point = (process->DebugPointList);
      }
      else {
        last_debug_point->Next = this_debug_point->Next;
        free(this_debug_point);
        this_debug_point = (last_debug_point->Next);
      }

    }
    else {
        last_debug_point = this_debug_point;
        this_debug_point = this_debug_point->Next;
    }
  }
  }
}

LPDEBUG_POINT stepping_capture_breakpoint_for_this_thread
  (LPDBGPROCESS process, LPDBGTHREAD thread, TARGET_ADDRESS address)
{
  LPDEBUG_POINT this_debug_point = thread->SteppingCaptureBreakpoints;

  while (this_debug_point != NULL) {
    if ((this_debug_point->DebugPointType == DBG_POINT_BREAKPOINT) &&
        (this_debug_point->u.Breakpoint.Address == (DWORD) address)) {
      return(this_debug_point);
    }
    else {
      this_debug_point = this_debug_point->Next;
    }
  } 
  return(NULL);
}

LPDEBUG_POINT stepping_capture_breakpoint_for_any_thread
  (LPDBGPROCESS process, TARGET_ADDRESS address)
{
  LPDEBUG_POINT this_debug_point;
  LPDBGTHREAD   this_thread = process->ThreadList;

  while (this_thread != NULL) {
    this_debug_point = this_thread->SteppingCaptureBreakpoints;
    while (this_debug_point != NULL) {
      if ((this_debug_point->DebugPointType == DBG_POINT_BREAKPOINT) &&
          (this_debug_point->u.Breakpoint.Address == (DWORD) address)) {
        return(this_debug_point);
      }
      else {
        this_debug_point = this_debug_point->Next;
      }
    }
    this_thread = this_thread->Next;
  }
  return(NULL);
}

BOOL check_context_for_stepping_capture
   (LPDBGPROCESS process, LPDBGTHREAD thread, LPDEBUG_POINT bp)
{
  // If the breakpoint is of the STEP_INTO variety, then the context is
  // considered to be correct. Otherwise, we need to compare stack frame
  // pointers.

  if (bp->u.Breakpoint.ThreadContext == thread) {
    if (bp->u.Breakpoint.Type == CAPTURE_STEP_INTO_BREAKPOINT) {
      return(TRUE);
    }
    else {
      nub_initialize_stack_vectors((NUB) process, (NUBTHREAD) thread);
      if ((thread->StackTraceFramePointers[0] 
               == bp->u.Breakpoint.FramePointerContext) ||
          (thread->StackTraceFramePointers[1]
	     == bp->u.Breakpoint.CallingFramePointerContext)) {
        return(TRUE);
      }
      else {
        return(FALSE);
      }
    }
  }
  else {
    return(FALSE);
  }
}

LPDEBUG_POINT breakpoint_from_address 
  (LPDBGPROCESS process, TARGET_ADDRESS address)
{
  LPDEBUG_POINT this_debug_point = process->DebugPointList;

  while (this_debug_point != NULL) {

    if ((this_debug_point->DebugPointType == DBG_POINT_BREAKPOINT) &&
        (this_debug_point->u.Breakpoint.Address == (DWORD) address)) {

      return (this_debug_point);
    }
    else {
      this_debug_point = this_debug_point->Next;
    }
  }
  return (NULL);
}


int handle_breakpoint_hit 
  (LPDBGPROCESS process, LPDBGTHREAD thread, TARGET_ADDRESS address)
{
  LPDEBUG_POINT breakpoint = 
    stepping_capture_breakpoint_for_this_thread (process, thread, address);
  LPDEBUG_POINT global_breakpoint =
    breakpoint_from_address(process, address);

  int return_code = UNKNOWN_BREAKPOINT;

  if ((breakpoint != NULL) && 
      check_context_for_stepping_capture(process, thread, breakpoint)) {
    int stepping_type = breakpoint->u.Breakpoint.Type;
    handle_stepping_capture_breakpoint(process, thread, breakpoint);
    return(stepping_type);
  }
  else {
    breakpoint = 
      stepping_capture_breakpoint_for_any_thread(process, address);
    if (breakpoint != NULL) {
      if (global_breakpoint == NULL)
        handle_application_breakpoint(process, thread, breakpoint);
      return_code = BREAKPOINT_OUT_OF_CONTEXT;
    }
    if (global_breakpoint != NULL) {
      handle_application_breakpoint(process, thread, global_breakpoint);
      return_code = APPLICATION_BREAKPOINT;
    }
    return(return_code); 
  }
}


void add_stepping_capture_breakpoint_for_thread
    (LPDBGPROCESS process, LPDBGTHREAD thread, DWORD address,
     DWORD frame, DWORD calling_frame, int type)
{
  // Allocate the new debug point.

  LPDEBUG_POINT step_point = 
    (LPDEBUG_POINT) malloc (sizeof(DEBUG_POINT));

  // Initialize.

  step_point->DebugPointType = DBG_POINT_BREAKPOINT;
  step_point->u.Breakpoint.Type = type;
  step_point->u.Breakpoint.Address = address;
  step_point->u.Breakpoint.ThreadContext = thread;
  step_point->u.Breakpoint.FramePointerContext = frame;
  step_point->u.Breakpoint.CallingFramePointerContext = calling_frame;

  // Link.
  // (Links to the head of the list, but this is not important).

  step_point->Next = thread->SteppingCaptureBreakpoints;
  thread->SteppingCaptureBreakpoints = step_point;

  // Set breakpoint instruction.

  drop_breakpoint(process, step_point);
}


void rewind_thread_eip (LPDBGPROCESS process, LPDBGTHREAD thread)
{
  CONTEXT context;
  BOOL    status;

  context.ContextFlags = CONTEXT_CONTROL;

  status =
    GetThreadContext (thread->ThreadHandle, &context);

  if (!status) {
    // Really, really, really serious error.
  }
  else {
    // Rewind past the breakpoint instruction.
    context.Eip = context.Eip - sizeof(BYTE);
    SetThreadContext (thread->ThreadHandle, &context);
  };

  dylan_debugger_message("Thread %= : rewound to address %=",
			 thread->ThreadHandle,
			 context.Eip);
}

void prepare_to_single_step_for_breakpoint_recovery
   (LPDBGPROCESS process, LPDBGTHREAD thread)
{
  CONTEXT context;
  BOOL    status;

  context.ContextFlags = CONTEXT_CONTROL;

  status =
    GetThreadContext (thread->ThreadHandle, &context);

  if (!status) {
    // Really, really, really serious error.
  }
  else {
    // Go into single-step mode
    context.EFlags = context.EFlags | 0x100;
    SetThreadContext (thread->ThreadHandle, &context);
  }
}

void recover_from_breakpoint
  (LPDBGPROCESS process, LPDBGTHREAD thread, BOOL resume)
{
  CONTEXT          context;
  LPDEBUG_POINT    breakpoint;
  BOOL             status;

  context.ContextFlags = CONTEXT_CONTROL;
  status = get_thread_context(process, thread, &context);

  thread->NeedsBreakpointReplacement = FALSE;
  breakpoint = thread->BreakpointToReplace;

  drop_breakpoint(process, breakpoint);

  // In order to recover from the breakpoint, we will have put the
  // processor into single-step mode. We now need to take it
  // out of single-step mode UNLESS the thread was being
  // single-stepped anyway!

  if (!(thread->SingleStepping)) {
    context.EFlags = context.EFlags & 0xFFFFFEFF;
    /*
      dylan_debugger_message("Resuming all threads for breakpoint recovery %=",
                             thread->ThreadHandle, 0);
      if (resume)
      resume_all_except(process, thread);
      */
  }

  status = SetThreadContext(thread->ThreadHandle, &context);
}


void handle_application_breakpoint 
  (LPDBGPROCESS process, LPDBGTHREAD thread, LPDEBUG_POINT breakpoint)
{
  DWORD        address = (DWORD) breakpoint->u.Breakpoint.Address;

  // Lift the APPLICATION_BREAKPOINT, but don't delete its list
  // entry, 'coz it'll be going back in a couple of milliseconds.

  lift_breakpoint(process, breakpoint);

  // Rewind the instruction pointer for the affected thread, so that the
  // whole original instruction will be executed. Also put the thread
  // into single-instruction-step mode so that we will get a chance to
  // rewrite the breakpoint.

  rewind_thread_eip (process, thread);

  if (!process->ExitingProcess) {

    prepare_to_single_step_for_breakpoint_recovery(process, thread);

    // Tell this thread that it now has to undergo breakpoint
    // recovery.

    thread->NeedsBreakpointReplacement = TRUE;
    thread->BreakpointToReplace = breakpoint;
  }
}

void handle_stepping_capture_breakpoint 
  (LPDBGPROCESS process, LPDBGTHREAD thread, LPDEBUG_POINT breakpoint)
{
  DWORD        address = (DWORD) breakpoint->u.Breakpoint.Address;

  // Lift the APPLICATION_BREAKPOINT, but don't delete its list
  // entry, 'coz it'll be going back in a couple of milliseconds.

  lift_breakpoint(process, breakpoint);

  // Rewind the instruction pointer for the affected thread, so that the
  // whole original instruction will be executed. We do not need to enter
  // single-step mode, since stepping capture breakpoints are "once only",
  // and do not need to be replaced.

  rewind_thread_eip (process, thread);
}

void set_application_breakpoint (LPDBGPROCESS process, DWORD address)
{
  LPDEBUG_POINT  abp = (LPDEBUG_POINT) malloc (sizeof(DEBUG_POINT));
  LPDEBUG_POINT  this_debug_point, last_debug_point;

  // First of all, find the node last node so we can link to it.

  this_debug_point = process->DebugPointList;
  last_debug_point = NULL;

  while (this_debug_point != NULL) {
    last_debug_point = this_debug_point;
    this_debug_point = this_debug_point->Next;
  }

  // Set up the data for the internal breakpoint.

  abp->DebugPointType = DBG_POINT_BREAKPOINT;
  abp->Next = NULL;
  abp->u.Breakpoint.Type = APPLICATION_BREAKPOINT;
  abp->u.Breakpoint.Address = address;

  // Link to the list.

  if (last_debug_point == NULL)
    process->DebugPointList = abp;
  else
    last_debug_point->Next = abp;

  // Finally, write the breakpoint instruction into the process.

  drop_breakpoint(process, abp);
}


BOOL application_breakpoint_exists 
  (LPDBGPROCESS process, TARGET_ADDRESS address)
{
  LPDEBUG_POINT candidate = breakpoint_from_address(process, address);

  if ((candidate != NULL) && 
      (candidate->u.Breakpoint.Type == APPLICATION_BREAKPOINT))
    return (TRUE);
  else
    return (FALSE);
}


// These functions do the actual memory-manipulations for setting and
// clearing breakpoints.

int drop_breakpoint (LPDBGPROCESS process, LPDEBUG_POINT breakpoint)
{
  breakpoint->u.Breakpoint.BreakpointEnabled = TRUE;

  return(primitive_drop_breakpoint(process->ProcessHandle,
				   breakpoint->u.Breakpoint.Address,
				   (BYTE*) &(breakpoint->u.Breakpoint.SavedCodeSegment)));
}

int primitive_drop_breakpoint (HANDLE process, DWORD address, BYTE *holder)
{
  BYTE    breakpoint_instance = 0xCC;
  DWORD   bytes_read;
  BOOL    read_status, write_status;

  // Read out the byte that we're going to eclipse with the breakpoint.
  // Some other function will have to store this somewhere.

  //printf ("--- NUB: Writing breakpoint at 0x%x\n", address);

  read_status =
    ReadProcessMemory 
      (process, (LPVOID) address, holder, sizeof(BYTE),
       &bytes_read);

  dylan_debugger_message("dropped breakpoint byte %= %=",
                         *holder, read_status);

  // If it doesn't work, don't try to continue.

  if (!read_status)
    return (0);

  // Write in the breakpoint instruction.

  write_status =
    ValidatedWriteProcessMemory 
      (process, (LPVOID) address, &breakpoint_instance,
       sizeof(BYTE), &bytes_read);

  dylan_debugger_message("Dropping breakpoint at %= %=", address, write_status);

  // Again, break if something goes wrong. (We have to assume here that the
  // breakpoint wasn't written if the function call failed).

  if (!write_status)
    return (0);

  // Make sure the breakpoint will be "seen" when encountered...

  FlushInstructionCache (process, (LPCVOID) address, sizeof(BYTE));
  return(1);
}

int lift_breakpoint (LPDBGPROCESS process, LPDEBUG_POINT breakpoint)
{
  breakpoint->u.Breakpoint.BreakpointEnabled = FALSE;

  return(primitive_lift_breakpoint(process->ProcessHandle,
                                   breakpoint->u.Breakpoint.Address,
                                   (BYTE*) &(breakpoint->u.Breakpoint.SavedCodeSegment)));
}

int primitive_lift_breakpoint (HANDLE process, DWORD address, BYTE *holder)
{
  DWORD       bytes_read;
  BOOL        write_status;
  BOOL        read_status;
  BYTE        holder2;

  // Assume we've been supplied the byte that was stored when the breakpoint
  // was installed.

  //printf ("--- NUB: Lifting breakpoint at 0x%x\n", address);

  write_status =
    ValidatedWriteProcessMemory 
      (process, (LPVOID) address, holder,
       sizeof(BYTE), &bytes_read);

  dylan_debugger_message("Lifting breakpoint at %= %=", address, write_status);

  if (!write_status)
    return (0);

  read_status =
    ReadProcessMemory 
      (process, (LPVOID) address, &holder2, sizeof(BYTE),
       &bytes_read);

  dylan_debugger_message("Lifted breakpoint byte %= %=",
                         holder2, read_status);


  // Make sure the processor will see the original instruction.

  FlushInstructionCache (process, (LPCVOID) address, sizeof(BYTE));
  return(1);
}


NUB_ERROR nub_set_breakpoint (NUB nub, TARGET_ADDRESS address)
{
  LPDBGPROCESS  process = (LPDBGPROCESS) nub;
  LPDEBUG_POINT candidate = breakpoint_from_address(process, address);

  if ((candidate != NULL) && 
      (candidate->u.Breakpoint.Type == APPLICATION_BREAKPOINT))
    if (candidate->u.Breakpoint.BreakpointEnabled)
      return ((NUB_ERROR) BREAKPOINT_ALREADY_EXISTS);
    else {

      // Thread sensitive breakpoint was lifted on another thread
      // to do breakpoint recovery; in the mean time, we have switched
      // threads in our multi-threaded app, or spawned a new application
      // thread, and wish to execute code on it; 
      // Re-enable it now explicitly to do interaction on this newly
      // spawned thread

      dylan_debugger_message("Re-enabling breakpoint at %=",
                             candidate->u.Breakpoint.Address, 0);
      drop_breakpoint(process, candidate);
      return (BREAKPOINT_WAS_DISABLED);
    }
  else {
    set_application_breakpoint (process, (DWORD) address);
    return (OK);
  }
}


NUB_ERROR nub_clear_breakpoint (NUB nub, TARGET_ADDRESS address)
{
  LPDBGPROCESS   process = (LPDBGPROCESS) nub;

  if (application_breakpoint_exists(process, address)) {
    clear_application_breakpoint (process, (DWORD) address);
    return ((NUB_ERROR) OK);
  }
  else {
    return ((NUB_ERROR) BREAKPOINT_DOES_NOT_EXIST);
  }
}


NUBINT nub_query_breakpoint (NUB nub, TARGET_ADDRESS address)
{
 LPDBGPROCESS   process = (LPDBGPROCESS) nub;

  if (application_breakpoint_exists (process, address)) {
    return ((NUBINT) 1);
  }
  else {
    return ((NUBINT) 0);
  }
}


NUB_ERROR nub_set_watchpoint 
  (NUB nub, TARGET_ADDRESS address,
   FLAG flag, NUBINT size)
{
  return ((NUB_ERROR) NOT_SUPPORTED);
}


NUB_ERROR nub_clear_watchpoint 
  (NUB nub, TARGET_ADDRESS address,
   FLAG flag, NUBINT size)
{
  return ((NUB_ERROR) NOT_SUPPORTED);
}


NUB_ERROR nub_set_stepping_control_on_thread
  (NUB nub, NUBTHREAD nubthread, 
   TARGET_ADDRESS frame, TARGET_ADDRESS calling_frame, 
   NUBINT location_count, TARGET_ADDRESS *locations,
   NUBINT type)
{
  LPDBGPROCESS       process = (LPDBGPROCESS) nub;
  LPDBGTHREAD        thread = (LPDBGTHREAD) nubthread;
  DWORD              dwAddr;
  CONTEXT            context;
  int                i;

  // We need the thread's context information.
  context.ContextFlags = CONTEXT_CONTROL;
  GetThreadContext(thread->ThreadHandle, &context);

  // Iterate over each of the supplied stepping capture locations, and
  // add them to the list for this thread.
  // It is vital that, if one of the locations is identical to the
  // thread's current instruction pointer, we do _not_ set it. If we
  // did, it would be impossible for the thread to make any progress
  // while stepping!

  for (i = 0; i < (int) location_count; i++) {
    dwAddr = (DWORD) locations[i];
    if (dwAddr != context.Eip)
      add_stepping_capture_breakpoint_for_thread
        (process, thread, dwAddr, (DWORD) frame, (DWORD) calling_frame,
         (int) type);
  }

  return ((NUB_ERROR) 1);
}


NUB_ERROR nub_clear_stepping_control_on_thread
  (NUB nub, NUBTHREAD nubthread)
{
  LPDBGPROCESS        process = (LPDBGPROCESS) nub;
  LPDBGTHREAD         thread = (LPDBGTHREAD) nubthread;

  lift_all_stepping_capture_breakpoints(process, thread);
  return ((NUB_ERROR) 1);
}
