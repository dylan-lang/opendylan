/* ********************************************************************** */
/* ** remote_object_registration.c                                     ** */
/* ** Functions for remote object registration.                        ** */
/* ** ---------------------------------------------------------------- ** */
/* ** Author: Paul Howard, Copyright: Functional Objects, Inc. 1996 ** */
/* **                                 All Rights Reserved              ** */
/* ********************************************************************** */

#include "nub-core.h"

void debugger_error(char*, TARGET_ADDRESS, TARGET_ADDRESS);


// This is the way we call the spy,
// Call the spy,
// Call the spy.
// This is the way we call the spy,
// On a cold and frosty morning.

BOOL push_debug_event
  (LPDBGPROCESS process, DEBUG_EVENT event);

TARGET_ADDRESS nub_allocate_stack_space
  (NUB nub,
   NUBTHREAD nubthread,
   NUBINT byte_count);

/*

  DETAILS:
  --------

  Functions in the SPY are assumed to adopt the C calling convention.
  They are also assumed to be totally safe, and incapable of generating
  debug events during their execution. (When those spy functions are
  written, somebody should _prove_ them to be correct!)

  A small section of code:

           CALL    ESI;
           INT     3;

  is pushed onto the thread's stack. Its instruction pointer is then pointed
  to the first of these instructions. ESI (in the thread's context) is
  also loaded with the entry point of the spy function being called.

  Any arguments being passed to the spy function are then pushed onto the
  stack.

  This manipulated context is then set for the thread. When the app
  continues, the CALL ESI instruction will call the spy function. The
  arguments will be properly placed according to the C calling
  convention. Upon completion of the spy function, the app will break
  out to the debugger (due to the presence of the INT 3 instruction).


*/


typedef struct _SPY_CALL_INSTRUCTION_SEQUENCE {

  WORD         CallInstruction;
  BYTE         BreakInstruction;

} SPY_CALL_INSTRUCTION_SEQUENCE;


#define SECOND_CHANCE_TOLERANCE 10

TARGET_ADDRESS nub_remote_call_spy 
  (NUB nub, 
   NUBTHREAD thread,
   TARGET_ADDRESS function,
   NUBINT arg_count, 
   TARGET_ADDRESS *args,
   NUB_ERROR* aborted)
{
  LPDBGPROCESS                   process = (LPDBGPROCESS) nub;
  LPDBGTHREAD                    threadC = (LPDBGTHREAD) thread;
  LPDBGTHREAD                    SpyThread;
  NUBINT                         code;
  NUBINT                         i;
  BOOL                           status;
  DWORD                          thread_sp;
  CONTEXT                        context;
  CONTEXT                        saved_context;
  DWORD                          bytes;
  DWORD                          result;
  DWORD                          instruction_position;
  SPY_CALL_INSTRUCTION_SEQUENCE  the_code;
  DWORD                          expected_bp_address;
  DWORD                          sp;
  BOOL                           spy_has_returned = FALSE;
  int                            second_chance_counter = 0;
  LPDEBUG_POINT                  BreakpointToReplace;
  BOOL                           NeedsBreakpointReplacement;

  // We always want to affect the entire context of a thread.

  context.ContextFlags = CONTEXT_FULL;
  saved_context.ContextFlags = CONTEXT_FULL;
 
  // Get the context of the thread and save it before we muck about
  // with it.
  // When the spy function returns, we can just restore this context
  // back again, and it will be as if nothing ever happened.

  status = get_thread_context(process, threadC, &context);

  // The breakpoint state of the thread needs to be preserved to allow
  // for Spy calls hitting the occasional breakpoint
  BreakpointToReplace = threadC->BreakpointToReplace;
  NeedsBreakpointReplacement = threadC->NeedsBreakpointReplacement;

  dylan_debugger_message("nub_remote_call_spy: Thread Context before: %= : %=",
			 threadC->ThreadHandle, status);
  dylan_debugger_message("Esp: %=  Eip: %=",
			 context.Esp,
			 context.Eip);

  saved_context = context;

  if (threadC->NeedsBreakpointReplacement)
    context.EFlags = context.EFlags & 0xFFFFFEFF;

  if (!status)
    return (NULL);

  // Call a seperate micro-spy function to allocate space on the stack for
  // the arguments.

  thread_sp =
    (DWORD) nub_allocate_stack_space
      (nub,
       thread,
       sizeof(SPY_CALL_INSTRUCTION_SEQUENCE) +
         (sizeof(TARGET_ADDRESS) * ((DWORD) arg_count)));

  if (thread_sp == 0x0) {
    debugger_error("Serious Error: Failed to allocate stack in Spy call on Thread %=",
		   (TARGET_ADDRESS)threadC->ThreadHandle,
		   (TARGET_ADDRESS)NULL);
    (*aborted) = (NUB_ERROR) 1;
    return(NULL);
  }

  sp = thread_sp;
  instruction_position = thread_sp + 
                              (sizeof(TARGET_ADDRESS) * ((DWORD) arg_count));

  // Push the arguments required by the spy function.

  for (i = 0; i < arg_count; i++) {

    status 
      = ValidatedWriteProcessMemory
          (process->ProcessHandle,
           (LPVOID) thread_sp,
           (LPVOID) &(args[i]),
           sizeof (TARGET_ADDRESS),
           &bytes);

    thread_sp += sizeof(TARGET_ADDRESS);
    if ((!status) || (bytes != sizeof(TARGET_ADDRESS)))
      return (NULL);
  }

  // STAGE 4:
  // Remember some important information for a consistency check
  // when we hit the INT 3 instruction.

  expected_bp_address = instruction_position + sizeof(WORD);

  // Push a small instruction sequence onto the stack. This sequence
  // will be executed by the thread, and will simply tell it to call
  // the spy function, and then break out to the debugger.

  // Fill in the instruction sequence:  CALL [SPY_FUNC]; INT 3

  the_code.CallInstruction = 0xd6ff;    // CALL <reg ESI> = ffd6
  the_code.BreakInstruction = 0xcc;     // INT 3

  // And since we're calling Esi, make sure Esi holds our function.

  context.Esi = (DWORD) function;

  // Plant it on the stack, and remember its position, since we
  // need to point the thread's IP at it.

  // Note the constant use of sizeof. This is to ensure that the data
  // on the stack is always DWORD-aligned (since sizeof adds a
  // padding value when applied to a struct).

  status 
    = ValidatedWriteProcessMemory 
        (process->ProcessHandle,
         (LPVOID) instruction_position,
         (LPVOID) &the_code,
         sizeof(SPY_CALL_INSTRUCTION_SEQUENCE),
         &bytes);

  if ((!status) || (bytes != sizeof(SPY_CALL_INSTRUCTION_SEQUENCE)))
    return (NULL);

  // And since that was a piece of code, we'd better do a bit of
  // this...

  FlushInstructionCache (process->ProcessHandle,
                         (LPCVOID) instruction_position,
                         sizeof (SPY_CALL_INSTRUCTION_SEQUENCE));


  // STAGE 5:
  // Set the thread context.

  context.Eip = instruction_position;
  context.Esp = sp;

  status = SetThreadContext (threadC->ThreadHandle, &context);
  if (!status)
    return (NULL);

  // STAGE 6:
  // Get things moving so the function call will happen.

  process->ThreadCreatedDuringSpyCall = NULL;

  suspend_all_except(process, threadC);
  threadC->AddressOfSpyBreakpoint = expected_bp_address;
  process->ThreadRunningSpy = threadC;
  apply_appropriate_continuation_to_thread 
     (process, threadC, EXCEPTIONS_HANDLED);
  wait_for_stop_reason_internal
     (process, TRUE, 5000, &code, STOP_REASON_WAIT_SPY);

  if (code != SPY_RETURN_DBG_EVENT)
    (*aborted) = (NUB_ERROR) 1;
  else
    (*aborted) = (NUB_ERROR) 0;

  if (code != SPY_RETURN_DBG_EVENT) {
    // OutputDebugString("WARNING: Spy call failed on Thread");

    nub_debug_message("Error: Spy call %= failed on Thread %=",
                      function,
		      (TARGET_ADDRESS)threadC->ThreadHandle);

    return(NULL);
  }


  // STAGE 7:
  // Get the function result and restore the context.

  status = GetThreadContext (threadC->ThreadHandle, &context);

  dylan_debugger_message("nub_remote_call_spy: Thread Context after: %= : %=",
			 threadC->ThreadHandle, status);
  dylan_debugger_message("Esp: %=  Eip: %=",
			 context.Esp,
			 context.Eip);

  result = context.Eax;


  // STAGE 6b:
  // Allow creation of a remote thread during the Spy call

  // If a thread was created during a spy call, we have to 
  // continue on that thread; we rely on the runtime to make
  // the creating thread wait for the created synchronous thread
  // to initialize before continuing execution; the created thread
  // will in turn wait for a go-ahead from the debugger immediately
  // after releasing the creating thread.

  // Use interprocess synchronization to achieve this by receiving
  // the event handle from the new thread, duplicating its handle,
  // and signaling new thread to continue execution. Continue on the
  // Spy thread after suspending it, to put all this into action.
  // Then wait patiently for the expected breakpoint to be signaled
  // on the running interactive thread.


  SpyThread = process->ThreadCreatedDuringSpyCall;
  if (SpyThread != NULL)
  {
    BOOL         status;
    DWORD        number_bytes_transferred;
    HANDLE       event, new_event;
 
  status 
    = ReadProcessMemory(process->ProcessHandle,
                        (LPCVOID) ((INT32)result + sizeof(INT32)),
                        (LPVOID)  &event,
                        sizeof(INT32),
                        &number_bytes_transferred);
  
    DuplicateHandle(process->ProcessHandle, event,
                    GetCurrentProcess(), &new_event,
		    0, FALSE, DUPLICATE_SAME_ACCESS);
    SetEvent(new_event);
    CloseHandle(new_event);

    suspend_thread(threadC);

    apply_appropriate_continuation_to_thread 
      (process, threadC, EXCEPTIONS_HANDLED);

    SpyThread->AddressOfSpyBreakpoint = 0;
    process->ThreadRunningSpy = SpyThread;
    wait_for_stop_reason_internal
       (process, TRUE, 5000, &code, STOP_REASON_WAIT_SPY);

    if (code == TIMED_OUT) {
      nub_debug_message
        ("Error: Breakpoint not received on Interactive Thread %=",
         (TARGET_ADDRESS)SpyThread->ThreadHandle,
         (TARGET_ADDRESS)NULL);
    }

    resume_all_except(process, SpyThread);
   }
 else
   // Resume all those threads that we suspended.

   resume_all_except(process, threadC);



  // STAGE 7b:

  // Just restore the context.

  status = SetThreadContext (threadC->ThreadHandle, &saved_context);

  // The breakpoint state of the thread needs to be preserved to allow
  // for Spy calls hitting the occasional breakpoint
  threadC->BreakpointToReplace = BreakpointToReplace;
  threadC->NeedsBreakpointReplacement = NeedsBreakpointReplacement;

  // And return, leaving everything spotless.

  return ((TARGET_ADDRESS) result);
}

