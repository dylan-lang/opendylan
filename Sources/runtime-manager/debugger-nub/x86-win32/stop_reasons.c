/* ********************************************************************** */
/* ** stop_reasons.c                                                   ** */
/* ** Functions for receiving debug events, and passing information    ** */
/* ** about them back down the access path.                            ** */
/* ** ---------------------------------------------------------------- ** */
/* ** Author: Paul Howard, Copyright: Functional Objects, Inc. 1996 ** */
/* **                                 All Rights Reserved              ** */
/* ********************************************************************** */

#include "nub-core.h"

DWORD    last_event_process;
DWORD    last_event_thread;

NUBINT stop_reason_to_nub_code (LPDEBUG_EVENT);

NUBINT exception_to_nub_code (LPDEBUG_EVENT);
int get_os_wall_clock_time(LPDBGPROCESS);

BOOL filter_first_chance 
  (LPDBGPROCESS process, LPDBGTHREAD thread, LPDEBUG_EVENT event,
   int *event_code);

BOOL filter_single_step_exception
  (LPDBGPROCESS process, LPDBGTHREAD thread, LPDEBUG_EVENT event,
   int *event_code);

BOOL filter_breakpoint_exception
  (LPDBGPROCESS process, LPDBGTHREAD thread, LPDEBUG_EVENT event,
   int *event_code);

BOOL get_next_debug_event_for_this_process
    (LPDBGPROCESS process, LPDEBUG_EVENT event, int timeout);

void flag_all_threads_frozen (LPDBGPROCESS process);

/* An access-path event handler for create-thread events received during spy calls */

extern void create_thread_stop_reason_handler
  (LPDBGPROCESS process, LPDBGTHREAD thread, NUBINT priority);


NUBINT set_application_state (LPDBGPROCESS process, int state)
{
  NUBINT current = get_os_wall_clock_time(process);

  /* Housekeeping for wall-clock time recording. If the state is
     being set to RUNNING, then update the "last activation"
     time, otherwise update the cumulative time */

  if (state == RUNNING) {
    process->LastActivationWallClockTime = current;
  }
  else {
    NUBINT   last_act = process->LastActivationWallClockTime;
    process->CumulativeWallClockTime += (current - last_act);
  }

  process->State = state;

  return(current);
}


int get_application_state (LPDBGPROCESS process)
{
  return (process->State);
}


BOOL push_debug_event
  (LPDBGPROCESS process, DEBUG_EVENT event)
{
   DEBUG_EVENT_QUEUE_NODE       *new_node =
       (DEBUG_EVENT_QUEUE_NODE*) malloc (sizeof(DEBUG_EVENT_QUEUE_NODE));

   // Copy the given debug event.
   new_node->EventSpace = event;

   // And terminate.
   new_node->Next = NULL;

   // And link to the queue, using a special case for when the queue is
   // empty.

   if (process->EventQueue.Size == 0) {
     process->EventQueue.Size = 1;
     process->EventQueue.Head = new_node;
     process->EventQueue.Tail = new_node;
     return(TRUE);
   }
   else {
     process->EventQueue.Size++;
     process->EventQueue.Tail->Next = new_node;
     process->EventQueue.Tail = new_node;
     return(TRUE);
   }
}


BOOL pop_debug_event
  (LPDBGPROCESS process, DEBUG_EVENT *event)
{
  /*
  DEBUG_EVENT_QUEUE_NODE      *old_node;
  if (process->EventQueue.Size == 0) {
    return(FALSE);
  }
  else {
    process->EventQueue.Size--;
    (*event) = process->EventQueue.Head->EventSpace;
    old_node = process->EventQueue.Head;
    process->EventQueue.Head = process->EventQueue.Head->Next;
    free(old_node);
    return(TRUE);
  }
  */
  return(FALSE);
}


void initialize_debug_event_queue
   (LPDBGPROCESS process)
{
  process->EventQueue.Size = 0;
  process->EventQueue.Head = NULL;
  process->EventQueue.Tail = NULL;
}


void clear_debug_event_queue
   (LPDBGPROCESS process)
{
  DEBUG_EVENT_QUEUE_NODE      *old_node;

  // Chain down the queue from head to tail, unlinking and disposing
  // each resident node.

  while(process->EventQueue.Head != NULL) {
     old_node = process->EventQueue.Head;
     process->EventQueue.Head = process->EventQueue.Head->Next;
     free(old_node);
  }

  // And set the size to zero, of course.

  process->EventQueue.Size = 0;
  process->EventQueue.Tail = NULL;
}


NUBINT nub_can_receive_first_chance (NUB nub, NUBINT code)
{
  LPDBGPROCESS  process = (LPDBGPROCESS) nub;
  int           i = (int) code;

  if (process->ExceptionInformation[i].ReceivableFirstChance)
    return ((NUBINT) 1);
  else
    return ((NUBINT) 0);
}


void nub_set_first_chance (NUB nub, NUBINT code)
{
  LPDBGPROCESS  process = (LPDBGPROCESS) nub;
  int           i = (int) code;

  process->ExceptionInformation[i].ReceivingFirstChance = TRUE;
}


void nub_unset_first_chance (NUB nub, NUBINT code)
{
  LPDBGPROCESS  process = (LPDBGPROCESS) nub;
  int           i = (int) code;

  process->ExceptionInformation[i].ReceivingFirstChance = FALSE;
}

void initialize_process_exception_information (LPDBGPROCESS process)
{
  int i;

  // For each receivable exception, indicate that first-chance
  // occurrences are possible, but are set to OFF for everything
  // except breakpoints.

  i = ACCESS_VIOLATION_EXCEPTION_DBG_EVENT;
  process->ExceptionInformation[i].ReceivingFirstChance = FALSE;
  process->ExceptionInformation[i].ReceivableFirstChance = TRUE;
  i = ARRAY_BOUNDS_EXCEPTION_DBG_EVENT;
  process->ExceptionInformation[i].ReceivingFirstChance = FALSE;
  process->ExceptionInformation[i].ReceivableFirstChance = TRUE;
  i = ILLEGAL_INSTRUCTION_EXCEPTION_DBG_EVENT;
  process->ExceptionInformation[i].ReceivingFirstChance = FALSE;
  process->ExceptionInformation[i].ReceivableFirstChance = TRUE;
  i = PRIVILEGED_INSTRUCTION_EXCEPTION_DBG_EVENT;
  process->ExceptionInformation[i].ReceivingFirstChance = FALSE;
  process->ExceptionInformation[i].ReceivableFirstChance = TRUE;
  i = DENORMAL_EXCEPTION_DBG_EVENT;
  process->ExceptionInformation[i].ReceivingFirstChance = FALSE;
  process->ExceptionInformation[i].ReceivableFirstChance = TRUE;
  i = FLOAT_DIVIDE_BY_ZERO_EXCEPTION_DBG_EVENT;
  process->ExceptionInformation[i].ReceivingFirstChance = FALSE;
  process->ExceptionInformation[i].ReceivableFirstChance = TRUE;
  i = INEXACT_RESULT_EXCEPTION_DBG_EVENT;
  process->ExceptionInformation[i].ReceivingFirstChance = FALSE;
  process->ExceptionInformation[i].ReceivableFirstChance = TRUE;
  i = INVALID_OPERATION_EXCEPTION_DBG_EVENT;
  process->ExceptionInformation[i].ReceivingFirstChance = FALSE;
  process->ExceptionInformation[i].ReceivableFirstChance = TRUE;
  i = FLOAT_OVERFLOW_EXCEPTION_DBG_EVENT;
  process->ExceptionInformation[i].ReceivingFirstChance = FALSE;
  process->ExceptionInformation[i].ReceivableFirstChance = TRUE;
  i = FLOAT_UNDERFLOW_EXCEPTION_DBG_EVENT;
  process->ExceptionInformation[i].ReceivingFirstChance = FALSE;
  process->ExceptionInformation[i].ReceivableFirstChance = TRUE;
  i = FLOAT_STACK_CHECK_EXCEPTION_DBG_EVENT;
  process->ExceptionInformation[i].ReceivingFirstChance = FALSE;
  process->ExceptionInformation[i].ReceivableFirstChance = TRUE;
  i = INTEGER_DIVIDE_BY_ZERO_EXCEPTION_DBG_EVENT;
  process->ExceptionInformation[i].ReceivingFirstChance = FALSE;
  process->ExceptionInformation[i].ReceivableFirstChance = TRUE;
  i = NONCONTINUABLE_EXCEPTION_DBG_EVENT;
  process->ExceptionInformation[i].ReceivingFirstChance = FALSE;
  process->ExceptionInformation[i].ReceivableFirstChance = TRUE;
  i = BREAKPOINT_EXCEPTION_DBG_EVENT;
  process->ExceptionInformation[i].ReceivingFirstChance = TRUE;
  process->ExceptionInformation[i].ReceivableFirstChance = TRUE;
  i = HARD_CODED_BREAKPOINT_DBG_EVENT;
  process->ExceptionInformation[i].ReceivingFirstChance = TRUE;
  process->ExceptionInformation[i].ReceivableFirstChance = TRUE;
  i = SINGLE_STEP_DBG_EVENT;
  process->ExceptionInformation[i].ReceivingFirstChance = FALSE;
  process->ExceptionInformation[i].ReceivableFirstChance = TRUE;
  i = STACK_OVERFLOW_EXCEPTION_DBG_EVENT;
  process->ExceptionInformation[i].ReceivingFirstChance = FALSE;
  process->ExceptionInformation[i].ReceivableFirstChance = TRUE;
  i = UNCLASSIFIED_DBG_EVENT;
  process->ExceptionInformation[i].ReceivingFirstChance = FALSE;
  process->ExceptionInformation[i].ReceivableFirstChance = TRUE;
  i = INTEGER_OVERFLOW_EXCEPTION_DBG_EVENT;
  process->ExceptionInformation[i].ReceivingFirstChance = FALSE;
  process->ExceptionInformation[i].ReceivableFirstChance = TRUE;
  
}

NUBINT nub_first_hard_coded_breakpoint (NUB nub)
{
  LPDBGPROCESS   process   = (LPDBGPROCESS) nub;
  if (process->HardCodedBreakpointCounter == 1)
    return((NUBINT) 1);
  else
    return((NUBINT) 0);
}

NUBINT stop_reason_to_nub_code (LPDEBUG_EVENT event)
{
  switch (event->dwDebugEventCode) {

  // Map debug event codes onto Dylan stop-reason codes.

  case EXCEPTION_DEBUG_EVENT:
    return (exception_to_nub_code(event));
    break;

  case CREATE_THREAD_DEBUG_EVENT:
    return ((NUBINT) CREATE_THREAD_DBG_EVENT);
    break;

  case EXIT_THREAD_DEBUG_EVENT:
    return ((NUBINT) EXIT_THREAD_DBG_EVENT);
    break;

  case CREATE_PROCESS_DEBUG_EVENT:
    return ((NUBINT) CREATE_PROCESS_DBG_EVENT);
    break;

  case EXIT_PROCESS_DEBUG_EVENT:
    return ((NUBINT) EXIT_PROCESS_DBG_EVENT);
    break;

  case LOAD_DLL_DEBUG_EVENT:
    return ((NUBINT) LOAD_DLL_DBG_EVENT);
    break;

  case UNLOAD_DLL_DEBUG_EVENT:
    return ((NUBINT) UNLOAD_DLL_DBG_EVENT);
    break;

  case RIP_EVENT:
    return ((NUBINT) RIP_DBG_EVENT);
    break;

  case OUTPUT_DEBUG_STRING_EVENT:
    return ((NUBINT) OUTPUT_DEBUG_STRING_DBG_EVENT);
    break;

  default:
    return ((NUBINT) UNCLASSIFIED_DBG_EVENT);
  }
}


NUBINT exception_to_nub_code (LPDEBUG_EVENT event)
{
 switch (event->u.Exception.ExceptionRecord.ExceptionCode) {

 // Map exception codes onto Dylan stop reason codes.

  case EXCEPTION_ACCESS_VIOLATION:
    return ((NUBINT) ACCESS_VIOLATION_EXCEPTION_DBG_EVENT);
    break;

  case EXCEPTION_ARRAY_BOUNDS_EXCEEDED:
    return ((NUBINT) ARRAY_BOUNDS_EXCEPTION_DBG_EVENT);
    break;

  case EXCEPTION_FLT_DENORMAL_OPERAND:
    return ((NUBINT) DENORMAL_EXCEPTION_DBG_EVENT);
    break;

  case EXCEPTION_FLT_DIVIDE_BY_ZERO:
    return ((NUBINT) FLOAT_DIVIDE_BY_ZERO_EXCEPTION_DBG_EVENT);
    break;

  case EXCEPTION_FLT_INEXACT_RESULT:
    return ((NUBINT) INEXACT_RESULT_EXCEPTION_DBG_EVENT);
    break;

  case EXCEPTION_FLT_INVALID_OPERATION:
    return ((NUBINT) INVALID_OPERATION_EXCEPTION_DBG_EVENT);
    break;

  case EXCEPTION_FLT_OVERFLOW:
    return ((NUBINT) FLOAT_OVERFLOW_EXCEPTION_DBG_EVENT);
    break;

  case EXCEPTION_FLT_UNDERFLOW:
    return ((NUBINT) FLOAT_UNDERFLOW_EXCEPTION_DBG_EVENT);
    break;

  case EXCEPTION_FLT_STACK_CHECK:
    return ((NUBINT) FLOAT_STACK_CHECK_EXCEPTION_DBG_EVENT);
    break;

  case EXCEPTION_ILLEGAL_INSTRUCTION:
    return ((NUBINT) ILLEGAL_INSTRUCTION_EXCEPTION_DBG_EVENT);
    break;

  case EXCEPTION_PRIV_INSTRUCTION:
    return ((NUBINT) PRIVILEGED_INSTRUCTION_EXCEPTION_DBG_EVENT);
    break;

  case EXCEPTION_BREAKPOINT:
    return ((NUBINT) BREAKPOINT_EXCEPTION_DBG_EVENT);
    break;

  case EXCEPTION_SINGLE_STEP:
    return ((NUBINT) SINGLE_STEP_DBG_EVENT);
    break;

  case EXCEPTION_INT_OVERFLOW:
    return ((NUBINT) INTEGER_OVERFLOW_EXCEPTION_DBG_EVENT);
    break;

  case EXCEPTION_STACK_OVERFLOW:
    return ((NUBINT) STACK_OVERFLOW_EXCEPTION_DBG_EVENT);
    break;

  default:
    return ((NUBINT) UNCLASSIFIED_DBG_EVENT);

  }
}

BOOL get_next_debug_event_for_this_process
    (LPDBGPROCESS process, LPDEBUG_EVENT event, int timeout)
{
  BOOL  event_received = FALSE;
  BOOL  wrong_process = TRUE;

  while (wrong_process) {
    event_received = WaitForDebugEvent(event, timeout);
    if (event_received) {
      if (event->dwProcessId != process->ProcessID) {
        ContinueDebugEvent
           (event->dwProcessId, event->dwThreadId, DBG_CONTINUE);
      }
      else {
        wrong_process = FALSE;
      }
    }
    else {
      return(FALSE);
    }
  }
  return(event_received);
}


NUBINT process_this_first_chance_breakpoint
    (LPDBGPROCESS process, LPDBGTHREAD thread,
     LPDEBUG_EVENT event, NUBINT mode)
{
  int      breakpoint_handler_code;
  DWORD    address = 
             (DWORD) event->u.Exception.ExceptionRecord.ExceptionAddress;

  // Call the breakpoint system to classify this breakpoint occurrence.
  breakpoint_handler_code =
    handle_breakpoint_hit(process, thread, (TARGET_ADDRESS) address);

  dylan_debugger_message("First-chance Breakpoint Hit %= %=",
			 address, breakpoint_handler_code);

  switch (breakpoint_handler_code) {
  case UNKNOWN_BREAKPOINT:
    /*
    {
      DWORD       bytes_read;
      BOOL        read_status;
      BYTE        holder;

      read_status =
	ReadProcessMemory 
	(process, (LPVOID) address, &holder, sizeof(BYTE),
	 &bytes_read);

      dylan_debugger_message("Unknown breakpoint byte %= %=",
			     holder, read_status);
    };
    */

    // This is a hard-coded breakpoint, which we interpret as a
    // programmatic entry to the debugger.
    thread->StoppedState = THREAD_INVOKED_DEBUGGER;
    (process->HardCodedBreakpointCounter)++;
    return (HARD_CODED_BREAKPOINT_DBG_EVENT);
    break;

  case INTERNAL_BREAKPOINT:
  case BREAKPOINT_OUT_OF_CONTEXT:
    thread->StoppedState = THREAD_STOPPED_AT_KNOWN_BREAKPOINT;
    return(FILTER_HANDLED_DBG_EVENT);
    break;

  case CAPTURE_STEP_OVER_BREAKPOINT:
    thread->StoppedState = THREAD_STOPPED_AT_KNOWN_BREAKPOINT;
    if (mode == STOP_REASON_WAIT_SPY)
      return(FILTER_HANDLED_DBG_EVENT);
    else
      return(SOURCE_STEP_OVER_DBG_EVENT);
    break;

  case CAPTURE_STEP_OUT_BREAKPOINT:
    thread->StoppedState = THREAD_STOPPED_AT_KNOWN_BREAKPOINT;
    if (mode == STOP_REASON_WAIT_SPY)
      return(FILTER_HANDLED_DBG_EVENT);
    else
      return(SOURCE_STEP_OUT_DBG_EVENT);
    break;

  case CAPTURE_STEP_INTO_BREAKPOINT:
    thread->StoppedState = THREAD_STOPPED_AT_KNOWN_BREAKPOINT;
    if (mode == STOP_REASON_WAIT_SPY)
      return(FILTER_HANDLED_DBG_EVENT);
    else
      return(SOURCE_STEP_INTO_DBG_EVENT);
    break;

  default:
    thread->StoppedState = THREAD_STOPPED_AT_KNOWN_BREAKPOINT;
    if (mode == STOP_REASON_WAIT_SPY)
      return(FILTER_HANDLED_DBG_EVENT);
    else
      return(BREAKPOINT_EXCEPTION_DBG_EVENT);
    break;
  }
}

NUBINT process_this_first_chance_single_step
    (LPDBGPROCESS process, LPDBGTHREAD thread,
     LPDEBUG_EVENT event, NUBINT mode)
{
  // If the thread requires breakpoint replacement, perform this now.
  if (thread->NeedsBreakpointReplacement)
    recover_from_breakpoint(process, thread, TRUE);

  if ((thread->SingleStepping) && (mode != STOP_REASON_WAIT_SPY)) {
    thread->StoppedState = THREAD_STOPPED_AT_FIRST_CHANCE_EXCEPTION;
    return(SINGLE_STEP_DBG_EVENT);
  }
  else {
    thread->StoppedState = THREAD_STOPPED_AT_FIRST_CHANCE_EXCEPTION;
    return(FILTER_HANDLED_DBG_EVENT);
  }
}

NUBINT process_this_first_chance_exception
    (LPDBGPROCESS process, LPDBGTHREAD thread,
     LPDEBUG_EVENT event, NUBINT mode)
{
   NUBINT nubcode = exception_to_nub_code(event);

   switch (event->u.Exception.ExceptionRecord.ExceptionCode) {

   case EXCEPTION_BREAKPOINT:
     if ((mode == STOP_REASON_WAIT_SPY) &&
         ((DWORD) event->u.Exception.ExceptionRecord.ExceptionAddress ==
             thread->AddressOfSpyBreakpoint)) {
       dylan_debugger_message
	 ("Thread %= : Spy Breakpoint Hit", thread->ThreadHandle, 0);
       thread->StoppedState = THREAD_STOPPED_AT_KNOWN_BREAKPOINT;
       return(SPY_RETURN_DBG_EVENT);
     }
     else
       return(process_this_first_chance_breakpoint
               (process, thread, event, mode));
     break;

   case EXCEPTION_SINGLE_STEP:
     return(process_this_first_chance_single_step
             (process, thread, event, mode));
     break;

   default:
     // Apart from the BREAKPOINT and SINGLE_STEP exceptions, which are
     // always handled at first chance by the debugger nub, decide
     // whether this first-chance exception should be passed back to
     // the application, or reported to the debugger.
     // Note that, if we are waiting for a spy function, we pass ALL
     // first-chance exceptions back to the application in the hope
     // that they will be handled.

     if (process->ExceptionInformation[nubcode].ReceivingFirstChance &&
         (mode != STOP_REASON_WAIT_SPY)) {
       thread->StoppedState = THREAD_STOPPED_AT_FIRST_CHANCE_EXCEPTION;
       return(nubcode);
     }
     else {
       thread->StoppedState = THREAD_STOPPED_AT_FIRST_CHANCE_EXCEPTION;
       return(FILTER_UNHANDLED_DBG_EVENT);
     }
     break;
   }
}


NUBINT process_this_second_chance_exception
    (LPDBGPROCESS process, LPDBGTHREAD thread,
     LPDEBUG_EVENT event, NUBINT mode)
{
  switch (event->u.Exception.ExceptionRecord.ExceptionCode) {
  case EXCEPTION_BREAKPOINT:
    thread->StoppedState = THREAD_INVOKED_DEBUGGER;
    return (HARD_CODED_BREAKPOINT_DBG_EVENT);
    break;

  default:
    thread->StoppedState = THREAD_STOPPED_AT_SECOND_CHANCE_EXCEPTION;
    return (exception_to_nub_code(event));
  }
}


NUBINT process_this_debug_event
    (LPDBGPROCESS process, LPDBGTHREAD thread,
     LPDEBUG_EVENT event, NUBINT mode)
{
   NUBINT            decision;


   dylan_debugger_message("Thread %= : Debug Event %= received",
			  thread->ThreadHandle,
			  event->dwDebugEventCode);

   switch (event->dwDebugEventCode) {

   case CREATE_PROCESS_DEBUG_EVENT:
     decision = CREATE_PROCESS_DBG_EVENT;
     thread->NubCodeOfLastEvent = decision;
     thread->StoppedState = THREAD_STOPPED_AT_DEBUG_EVENT;
     break;

   case EXIT_PROCESS_DEBUG_EVENT:
     decision = EXIT_PROCESS_DBG_EVENT;
     thread->NubCodeOfLastEvent = decision;
     thread->StoppedState = THREAD_STOPPED_AT_DEBUG_EVENT;
     break;

   case CREATE_THREAD_DEBUG_EVENT:
     if (mode == STOP_REASON_WAIT_SPY) {

       /* If a thread is being created during a spy call,
	  call back into the access-path to register it
	  explicitly */

       create_thread_stop_reason_handler
	 (process, thread,
	  nub_thread_os_priority((NUB)process, (NUBTHREAD)thread));
       decision = FILTER_HANDLED_DBG_EVENT;
     }
     else
       decision = CREATE_THREAD_DBG_EVENT;
     thread->NubCodeOfLastEvent = decision;
     thread->StoppedState = THREAD_STOPPED_AT_DEBUG_EVENT;
     break;

   case EXIT_THREAD_DEBUG_EVENT:
     decision = EXIT_THREAD_DBG_EVENT;
     thread->NubCodeOfLastEvent = decision;
     thread->StoppedState = THREAD_STOPPED_AT_DEBUG_EVENT;
     break;

   case LOAD_DLL_DEBUG_EVENT:
     decision = LOAD_DLL_DBG_EVENT;
     thread->NubCodeOfLastEvent = decision;
     thread->StoppedState = THREAD_STOPPED_AT_DEBUG_EVENT;
     break;

   case UNLOAD_DLL_DEBUG_EVENT:
     decision = UNLOAD_DLL_DBG_EVENT;
     thread->NubCodeOfLastEvent = decision;
     thread->StoppedState = THREAD_STOPPED_AT_DEBUG_EVENT;
     break;

   case OUTPUT_DEBUG_STRING_EVENT:
     decision = OUTPUT_DEBUG_STRING_DBG_EVENT;
     thread->NubCodeOfLastEvent = decision;
     thread->StoppedState = THREAD_STOPPED_AT_DEBUG_EVENT;
     break;

   case RIP_EVENT:
     decision = CREATE_PROCESS_DBG_EVENT;
     thread->NubCodeOfLastEvent = decision;
     thread->StoppedState = THREAD_STOPPED_AT_DEBUG_EVENT;
     break;

   case EXCEPTION_DEBUG_EVENT:
     if (event->u.Exception.dwFirstChance != 0)
       decision = process_this_first_chance_exception
                    (process, thread, event, mode);
     else
       decision = process_this_second_chance_exception
                    (process, thread, event, mode);
     thread->NubCodeOfLastEvent = decision;
     break;

   default:
     decision = UNCLASSIFIED_DBG_EVENT;
     thread->NubCodeOfLastEvent = decision;
     thread->StoppedState = THREAD_STOPPED_AT_DEBUG_EVENT;
     break;

   }
   return(decision);
}


// For Windows 95 OS, thread contexts returned for suspended threads are bogus
// even though the OS claims it successfully obtained the context.
// The context is only believable when a debug-event has just been received on
// the thread in question.
// So we need to cache the thread contexts, and rely on the cache when the
// thread is resumed.


void set_thread_context(LPDBGTHREAD Cthread)
{
  int            status;

  dylan_debugger_message("set_thread_context %=", Cthread->ThreadHandle, 0);

  if (Cthread->CachedThreadContext == NULL) {
    Cthread->CachedThreadContext = (LPCONTEXT) malloc (sizeof(CONTEXT));
  };
  Cthread->CachedThreadContext->ContextFlags = CONTEXT_FULL;
  status = GetThreadContext (Cthread->ThreadHandle, Cthread->CachedThreadContext);

  dylan_debugger_message("Suspended Thread Context: %= : %=",
			 Cthread->ThreadHandle, status);
  dylan_debugger_message("Esp: %=  Eip: %=",
			 Cthread->CachedThreadContext->Esp,
			 Cthread->CachedThreadContext->Eip);
  /*
  dylan_debugger_message("Ebp: %=",
			 Cthread->CachedThreadContext->Ebp, NULL);
  dylan_debugger_message("Eax: %=  Ebx: %=",
			 Cthread->CachedThreadContext->Eax,
			 Cthread->CachedThreadContext->Ebx);
  dylan_debugger_message("Ecx: %=  Edx: %=",
			 Cthread->CachedThreadContext->Ecx,
			 Cthread->CachedThreadContext->Edx);
  dylan_debugger_message("Esi: %=  Edi: %=",
			 Cthread->CachedThreadContext->Esi,
			 Cthread->CachedThreadContext->Edi);
			 */

}

void free_thread_context(LPDBGTHREAD Cthread)
{
  if (Cthread->CachedThreadContext != NULL) {
    dylan_debugger_message("free_thread_context %=", Cthread->ThreadHandle, 0);
    free(Cthread->CachedThreadContext);
  };
  Cthread->CachedThreadContext = NULL;
}


void wait_for_stop_reason_internal
    (LPDBGPROCESS process, BOOL timeout, NUBINT timeout_value, 
     NUBINT *event_code, int mode)
{
  NUBINT           decision;
  BOOL             wait_again = TRUE;
  BOOL             event_received;
  LPDBGTHREAD      thread;
  DEBUG_EVENT      debug_event;
  int              LastActivationWallClockTime = 0;
  int              CumulativeWallClockTime = 0;
  NUBINT           current_timeout_value = timeout_value;


  if (timeout && (mode == STOP_REASON_WAIT_NORMAL)) {
    // Initialize the wall-clock time; use this to ensure that
    // incoming events do not defeat the requested timeout;
    // the application will interrupted as soon as the cumulative
    // time measured from this point in time exceeds the timeout
    LastActivationWallClockTime = get_os_wall_clock_time(process);
  }

  while (wait_again) {
    if (timeout) {
      // use updated timeout_value, adjusted by elapsed time so far
      // from all incoming events in this timeout interval
      event_received = 
        get_next_debug_event_for_this_process
          (process, &debug_event, (int) current_timeout_value);
    }
    else
      event_received =
        get_next_debug_event_for_this_process
          (process, &debug_event, INFINITE);

    if (event_received) {
      if (mode == STOP_REASON_WAIT_NORMAL) {
        // Don't perform these heavyweight operations if we are only calling
        // the spy.

        // set application state, returning the current wall-clock time
        NUBINT current_time = set_application_state(process, STOPPED_BY_EVENT);

        if (timeout) {
          // Update the cumulative time with time taken between successive
          // incoming events within the same timeout interval
          CumulativeWallClockTime += current_time - LastActivationWallClockTime;
        }
        flag_all_threads_frozen(process);
        housekeep_for_stop_reason(process, &debug_event);
      }

      if ((mode == STOP_REASON_WAIT_SPY) &&
          ((debug_event.dwDebugEventCode) == CREATE_THREAD_DEBUG_EVENT)) {
        housekeep_for_stop_reason(process, &debug_event);
      }

      thread = thread_descriptor_from_thread_ID(process, debug_event.dwThreadId);

      if (mode == STOP_REASON_WAIT_NORMAL) {
        process->PendingEvent = debug_event;
        thread->LastReceivedEvent = debug_event;
      }

      if ((mode == STOP_REASON_WAIT_SPY) &&
          ((debug_event.dwDebugEventCode) == CREATE_THREAD_DEBUG_EVENT)) {
        process->PendingEvent = debug_event;
        thread->LastReceivedEvent = debug_event;
      }

      process->PendingEventThread = thread;

      decision = process_this_debug_event(process, thread, &debug_event, mode);

      // For Windows 95 only, cache the Thread Context
      // _after_ processing current debug-event
      if (process->Platform == PLATFORM_WINDOWS_95)
        set_thread_context(thread);

    }
    else {
      decision = TIMED_OUT;
    };

    switch (decision) {

    case FILTER_HANDLED_DBG_EVENT:
      if (mode == STOP_REASON_WAIT_NORMAL) {
        if (!(process->ExitingProcess)
            && timeout && (CumulativeWallClockTime >= timeout_value)) {
          // Let the requested timeout enforce a break to the debugger
          // if sum total of time between incoming events exceeds timeout
          (*event_code) = TIMED_OUT_HANDLED;
          wait_again = FALSE;
          }
        else {
          nub_application_continue((NUB) process);
          // Update the current timeout, taking into account total elapsed
          // time during this timeout interval
          current_timeout_value = timeout_value - CumulativeWallClockTime;
          LastActivationWallClockTime = process->LastActivationWallClockTime;
          }
        }
      else {

        /* If a thread was created during a spy call, we have to 
           continue on that thread; we rely on the runtime to make
           the creating thread wait for the created synchronous thread
           to initialize before continuing execution; the created thread
           will in turn wait for a go-ahead from the debugger immediately
           after releasing the creating thread */

        if (thread->StoppedState == THREAD_STOPPED_AT_DEBUG_EVENT) {
          process->ThreadCreatedDuringSpyCall = thread;
          dylan_debugger_message("Interactive Thread %= created on Spy-Thread %=",
                                 thread->ThreadHandle,
                                 process->ThreadRunningSpy->ThreadHandle);

          apply_appropriate_continuation_to_thread
            (process, thread, EXCEPTIONS_HANDLED);
        }
        else
          apply_appropriate_continuation_to_thread
            (process, process->ThreadRunningSpy, EXCEPTIONS_HANDLED);
      };
      break;

    case FILTER_UNHANDLED_DBG_EVENT:
      if (mode == STOP_REASON_WAIT_NORMAL) {
        if (!(process->ExitingProcess)
            && timeout && (CumulativeWallClockTime >= timeout_value)) {
          // Let the requested timeout enforce a break to the debugger
          // if sum total of time between incoming events exceeds timeout
          (*event_code) = TIMED_OUT_UNHANDLED;
          wait_again = FALSE;
          }
        else {
          nub_application_continue_unhandled((NUB) process);
          // Update the current timeout, taking into account total elapsed
          // time during this timeout interval
          current_timeout_value = timeout_value - CumulativeWallClockTime;
          LastActivationWallClockTime = process->LastActivationWallClockTime;
          }
        }
      else
        apply_appropriate_continuation_to_thread
          (process, process->ThreadRunningSpy, EXCEPTIONS_UNHANDLED);
      break;

    default:
      (*event_code) = decision;
      wait_again = FALSE;

      if ((process->ExitingProcess) && (mode == STOP_REASON_WAIT_NORMAL))
        switch (decision) {

          case EXIT_THREAD_DBG_EVENT:
          case EXIT_PROCESS_DBG_EVENT:
            break;

          case TIMED_OUT:
            dylan_debugger_message
            ("Timed out while terminating process",
             NULL, NULL);
            wait_again = TRUE;
            nub_application_continue((NUB) process);
            break;

          default:
            dylan_debugger_message
            ("Unexpected event %= while terminating process on Thread %=",
             decision, thread->ThreadHandle);
            wait_again = TRUE;
            nub_application_continue((NUB) process);
            break;

         };

      break;

    }
  }
}


void nub_wait_for_stop_reason_with_timeout 
  (NUB nub,
   NUBINT timeout,
   NUBINT *event_code)
{
  wait_for_stop_reason_internal
    ((LPDBGPROCESS) nub, TRUE, timeout, event_code, STOP_REASON_WAIT_NORMAL);
}


void nub_wait_for_stop_reason_no_timeout (NUB nub, NUBINT *event_code)
{
  wait_for_stop_reason_internal
    ((LPDBGPROCESS) nub, FALSE, 0, event_code, STOP_REASON_WAIT_NORMAL);
}


void nub_flush_all_stop_reasons (NUB nub)
{
  LPDBGPROCESS         process = (LPDBGPROCESS) nub;
  clear_debug_event_queue(process);
}


TARGET_ADDRESS nub_stop_reason_debug_point_address (NUB nub)
{
  LPDBGPROCESS      process = (LPDBGPROCESS) nub;
  LPDEBUG_EVENT     eventC = &(process->PendingEvent);

  if (eventC == NULL) {
    // Complete and utter bastard.
    return (NULL);
  }
  else {
    return 
      ((TARGET_ADDRESS) eventC->u.Exception.ExceptionRecord.ExceptionAddress);
  }
}


TARGET_ADDRESS nub_stop_reason_exception_address (NUB nub)
{
  return (nub_stop_reason_debug_point_address (nub));
}

TARGET_ADDRESS nub_stop_reason_violation_address (NUB nub)
{
  LPDBGPROCESS      process = (LPDBGPROCESS) nub;
  LPDEBUG_EVENT     eventC = &(process->PendingEvent);

  if (eventC == NULL)
    return((TARGET_ADDRESS) 0);
  else
    return((TARGET_ADDRESS) 
            (eventC->u.Exception.ExceptionRecord.ExceptionInformation[1]));
}

NUBINT nub_stop_reason_violation_op (NUB nub)
{
  LPDBGPROCESS     process = (LPDBGPROCESS) nub;
  LPDEBUG_EVENT    eventC = &(process->PendingEvent);
  DWORD            info_member;

  if (eventC == NULL)
    return ((NUBINT) 0);

  info_member = eventC->u.Exception.ExceptionRecord.ExceptionInformation[0];
  if (info_member == 0)
    return ((NUBINT) 1);
  else
    return ((NUBINT) 2);
}

NUBINT nub_exception_first_chance (NUB nub)
{
  LPDBGPROCESS     process = (LPDBGPROCESS) nub;
  LPDEBUG_EVENT    eventC = &(process->PendingEvent);

  if (eventC == NULL)
    return ((NUBINT) 0);
  else if (eventC->u.Exception.dwFirstChance == 0)
    return ((NUBINT) 0);
  else
    return ((NUBINT) 1);
}

NUBPROCESS nub_stop_reason_process (NUB nub)
{
  LPDBGPROCESS      process = (LPDBGPROCESS) nub;
  LPDEBUG_EVENT     eventC = &(process->PendingEvent);

  if (eventC == NULL) {
    // Error case
    return (NULL);
  }
  else {
    return ((NUBPROCESS) (eventC->dwProcessId));
  }
}


NUBTHREAD nub_stop_reason_thread (NUB nub)
{
  LPDBGPROCESS       process = (LPDBGPROCESS) nub;
  LPDEBUG_EVENT      eventC = &(process->PendingEvent);

  if (eventC == NULL) {
    // Error case
    return (NULL);
  }
  else {
    NUBTHREAD thread_descriptor;
    thread_descriptor = 
      thread_descriptor_from_thread_ID (nub, eventC->dwThreadId);
    return (thread_descriptor);
  }
}


NUBLIBRARY nub_stop_reason_library (NUB nub)
{
  LPDBGPROCESS       process = (LPDBGPROCESS) nub;
  LPDEBUG_EVENT      eventC = &(process->PendingEvent);

  if (eventC == NULL) {
    // Error case, can't return anything meaningful.
    return (NULL);
  }
  else {

    // Find the library from its base address?

    switch (eventC->dwDebugEventCode) {

    case CREATE_PROCESS_DEBUG_EVENT:
      return ((NUBLIBRARY) (process->LibraryList));
      break;

    case LOAD_DLL_DEBUG_EVENT:
      return ((NUBLIBRARY) library_descriptor_from_base_address
                             (process,
                              (DWORD) (eventC->u.LoadDll.lpBaseOfDll)));
      break;

    case UNLOAD_DLL_DEBUG_EVENT:
      return ((NUBLIBRARY) library_descriptor_from_base_address
                             (process,
                              (DWORD) (eventC->u.UnloadDll.lpBaseOfDll)));
      break;

    default:
      // Tried to get a library for a non-library debug event.
      // So somebody's being a bit damn silly.
      return (NULL);
    }
  }
}


NUBINT nub_stop_reason_process_exit_code (NUB nub)
{
  LPDBGPROCESS     process = (LPDBGPROCESS) nub;
  LPDEBUG_EVENT    eventC = &(process->PendingEvent);

  if (eventC == NULL) {
    // Error case
    return ((NUBINT) 0);
  }
  else {
    return ((NUBINT) (eventC->u.ExitProcess.dwExitCode));
  }
}


NUBINT nub_stop_reason_thread_exit_code (NUB nub)
{
  LPDBGPROCESS      process = (LPDBGPROCESS) nub;
  LPDEBUG_EVENT     eventC = &(process->PendingEvent);

  if (eventC == NULL) {
    // Error case
    return ((NUBINT) 0);
  }
  else {
    return ((NUBINT) (eventC->u.ExitThread.dwExitCode));
  }
}


NUBINT nub_stop_reason_RIP_exit_code (NUB nub)
{
  LPDBGPROCESS       process = (LPDBGPROCESS) nub;
  LPDEBUG_EVENT      eventC = &(process->PendingEvent);

  return ((NUBINT) 0);
}


TARGET_ADDRESS nub_stop_reason_debug_string_address (NUB nub)
{
  LPDBGPROCESS       process = (LPDBGPROCESS) nub;
  LPDEBUG_EVENT      eventC = &(process->PendingEvent);

  return ((TARGET_ADDRESS) (eventC->u.DebugString.lpDebugStringData));
}


NUBINT nub_stop_reason_debug_string_length (NUB nub)
{
  LPDBGPROCESS       process = (LPDBGPROCESS) nub;
  LPDEBUG_EVENT      eventC = &(process->PendingEvent);

  // The nDebugStringLength field actually gives the length _including_
  // the NULL character according to Microsoft's documentation. The actual
  // length of the meaningful part of the string is therefore one less
  // than the quoted length. Hence this accessor subtracts one.

  return ((NUBINT) (eventC->u.DebugString.nDebugStringLength - 1));
}


NUBINT nub_stop_reason_debug_string_is_unicode (NUB nub)
{
  LPDBGPROCESS       process = (LPDBGPROCESS) nub;
  LPDEBUG_EVENT      eventC = &(process->PendingEvent);

  if (eventC->u.DebugString.fUnicode == 0)
    return ((NUBINT) 0);
  else
    return ((NUBINT) 1);
}


void flag_all_threads_frozen (LPDBGPROCESS process)
{
  LPDBGTHREAD      thread = process->ThreadList;

  while (thread != NULL) {
    thread->StoppedState = THREAD_FROZEN;
    thread = thread->Next;
  }
}

