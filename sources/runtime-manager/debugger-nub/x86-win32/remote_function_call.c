/* ********************************************************************** */
/* ** remote_function_call.c                                           ** */
/* ** Functions for setting up remote function calls in the debugee    ** */
/* ** ---------------------------------------------------------------- ** */
/* ** Author: Paul Howard, Copyright: Functional Objects, Inc. 1996 ** */
/* **                                 All Rights Reserved              ** */
/* ********************************************************************** */

#include "nub-core.h"

void debugger_error(char*, TARGET_ADDRESS, TARGET_ADDRESS);

/*
void print_context (char *message, CONTEXT *context)  // Debugging function.
{
  printf(message);
  printf("\n");
  printf("Eax       =   0x%x\n", context->Eax);
  printf("Edx       =   0x%x\n", context->Edx);
  printf("Ecx       =   0x%x\n", context->Ecx);
  printf("Ebx       =   0x%x\n", context->Ebx);
  printf("Ebp       =   0x%x\n", context->Ebp);
  printf("Esp       =   0x%x\n", context->Esp);
  printf("Edi       =   0x%x\n", context->Edi);
  printf("Eip       =   0x%x\n", context->Eip);
  printf("Flags     =   0x%x\n", context->EFlags);
  printf("\n");
}
*/

TARGET_ADDRESS nub_get_function_result 
  (NUB nub, 
   NUBTHREAD thread)
{
  LPDBGPROCESS        process = (LPDBGPROCESS) nub;
  LPDBGTHREAD         threadC = (LPDBGTHREAD) thread;
  CONTEXT             context;
  BOOL                status;
  TARGET_ADDRESS      function_result;

  // Now get context information.

  context.ContextFlags = CONTEXT_FULL;

  status = GetThreadContext (threadC->ThreadHandle, &context);

  if (!status) {
    // Internal nub error.
    return (function_result);
  }

  // Grab the function result. C call convention means that the result is a
  // word in the Eax register.

  function_result = (TARGET_ADDRESS) context.Eax;
  return (function_result);
}


void nub_restore_context 
  (NUB nub, 
   NUBTHREAD thread,
   NUBHANDLE context_cookie)
{
  LPDBGPROCESS         process = (LPDBGPROCESS) nub;
  LPDBGTHREAD          threadC = (LPDBGTHREAD) thread;
  THREAD_MEMORY        *saved_thread = (THREAD_MEMORY*) context_cookie;

  // Set the context for the thread.

  saved_thread->ThreadContext.ContextFlags = CONTEXT_FULL;

  SetThreadContext (threadC->ThreadHandle, &(saved_thread->ThreadContext));

  // For Windows 95 only, cache the Thread Context
  if (process->Platform == PLATFORM_WINDOWS_95)
    set_thread_context(threadC);

  //print_context("Restoring context to", &(saved_thread->ThreadContext));

  // And also free the CONTEXT structure.

  free(saved_thread);
}


typedef struct _ALLOCATOR_INSTRUCTION_SEQUENCE {
  BYTE        Nop1;
  BYTE        Nop2;
  BYTE        SubInstruction;
  BYTE        SpecifyEsp;
  DWORD       Immediate32;
  BYTE        BreakInstruction;
} ALLOCATOR_INSTRUCTION_SEQUENCE;


TARGET_ADDRESS nub_allocate_stack_space
  (NUB nub,
   NUBTHREAD nubthread,
   NUBINT byte_count)
{
  LPDBGPROCESS                   process = (LPDBGPROCESS) nub;
  LPDBGTHREAD                    thread = (LPDBGTHREAD) nubthread;
  NUBINT                         code;
  DWORD                          count = (DWORD) byte_count;
  CONTEXT                        context;
  CONTEXT                        context_as_was;
  BOOL                           status_get_context, 
                                 status_set_context, 
                                 status_write,
                                 status_read;
  ALLOCATOR_INSTRUCTION_SEQUENCE instruction_sequence;
  ALLOCATOR_INSTRUCTION_SEQUENCE saved_stack_memory;
  DWORD                          SP;
  DWORD                          IP;
  DWORD                          instruction_position;
  DWORD                          bytes_written,
                                 bytes_read;
  DWORD                          expected_bp_address;
  BOOL                           spy_has_returned = FALSE;
  int                            second_chance_counter = 0;

  // Get the context for the thread.

  context.ContextFlags = CONTEXT_FULL;
  status_get_context =  get_thread_context(process, thread, &context);

  dylan_debugger_message("nub_allocate_stack_space: Thread Context before: %= : %=",
			 thread->ThreadHandle, status_get_context);
  dylan_debugger_message("Esp: %=  Eip: %=",
			 context.Esp,
			 context.Eip);

  context_as_was = context;

  if (thread->NeedsBreakpointReplacement)
    context.EFlags = context.EFlags & 0xFFFFFEFF;

  // Fill in the instruction sequence SUB ESP, <count>; INT 3

  instruction_sequence.Nop1 = 0x90;
  instruction_sequence.Nop2 = 0x90;
  instruction_sequence.SubInstruction = 0x81;
  instruction_sequence.SpecifyEsp = 0xEC;
  instruction_sequence.Immediate32 = count;
  instruction_sequence.BreakInstruction = 0xCC;

  SP = context.Esp;
  IP = context.Eip;

  // Write the instruction sequence onto the stack. The number of words we
  // are writing is small enough that this should work on both NT and
  // 95.

//context.Esp -= sizeof(ALLOCATOR_INSTRUCTION_SEQUENCE);
  instruction_position = context.Esp;
  context.Eip = instruction_position;

  // Save the data that was there originally. As a precaution, we will
  // re-write this back again if we have to abort the whole procedure.

  status_read =
     ReadProcessMemory
         (process->ProcessHandle,
          (LPCVOID) instruction_position,
          (LPVOID) &saved_stack_memory,
          sizeof(ALLOCATOR_INSTRUCTION_SEQUENCE),
          &bytes_read);

  // Out with the old, in with the new...

  status_write =
     ValidatedWriteProcessMemory 
         (process->ProcessHandle,
          (LPVOID) instruction_position,
          (LPVOID) &instruction_sequence,
          sizeof(ALLOCATOR_INSTRUCTION_SEQUENCE),
          &bytes_written);

  // Flush the instruction cache because we have written a segment of
  // code.

  FlushInstructionCache(process->ProcessHandle,
                        (LPCVOID) instruction_position,
                        sizeof(ALLOCATOR_INSTRUCTION_SEQUENCE));

  // Set the context of the thread so that it will execute the SUB instruction
  // and then return control to the debugger.

  status_set_context = SetThreadContext(thread->ThreadHandle, &context);

  if (!status_set_context) {
    status_write =
       ValidatedWriteProcessMemory 
           (process->ProcessHandle,
            (LPVOID) instruction_position,
            (LPVOID) &saved_stack_memory,
            sizeof(ALLOCATOR_INSTRUCTION_SEQUENCE),
            &bytes_written);
    SetThreadContext(thread->ThreadHandle, &context_as_was); // Just in case
    return(NULL);
  }

  // Remember the address at which we expect the breakpoint.

  expected_bp_address = instruction_position + 8;

  // Let the thread execute the fragment of code. Hopefully, that will give
  // us space on the stack that Windows 95 won't crap all over.

  suspend_all(process);
  process->ThreadRunningSpy = thread;
  thread->AddressOfSpyBreakpoint = expected_bp_address;

  // Explicitly continue all threads to release frozen threads;
  // they are all suspended at this point so won't be put back
  // into execution

  nub_threads_continue(nub);

  // Now do what it takes to put this Spy running Thread alone into execution

  execute_thread(thread);

  wait_for_stop_reason_internal
     (process, TRUE, 1000, &code, STOP_REASON_WAIT_SPY);

  if (code == SPY_RETURN_DBG_EVENT) {
    // Resume the suspended threads.
    resume_all_except(process, thread);

    // Write back the stack data that we crapped all over.
    status_write =
       ValidatedWriteProcessMemory 
           (process->ProcessHandle,
            (LPVOID) instruction_position,
            (LPVOID) &saved_stack_memory,
            sizeof(ALLOCATOR_INSTRUCTION_SEQUENCE),
            &bytes_written);

    // Get the context again. This should have ESP correctly set.
    status_get_context = GetThreadContext(thread->ThreadHandle, &context);

    dylan_debugger_message("nub_allocate_stack_space: Thread Context after: %= : %=",
			   thread->ThreadHandle, status_get_context);
    dylan_debugger_message("Esp: %=  Eip: %=",
			   context.Esp,
			   context.Eip);


    return((TARGET_ADDRESS) (context.Esp));
  }
  else {
    resume_all_except(process, thread);
    status_write =
       ValidatedWriteProcessMemory 
           (process->ProcessHandle,
            (LPVOID) instruction_position,
            (LPVOID) &saved_stack_memory,
            sizeof(ALLOCATOR_INSTRUCTION_SEQUENCE),
            &bytes_written);
    SetThreadContext(thread->ThreadHandle, &context_as_was);
    nub_debug_message("Error: Micro Spy call failed on Thread %=",
		      (TARGET_ADDRESS)thread->ThreadHandle,
		      (TARGET_ADDRESS)NULL);
    return (NULL);
  }
  // Resume the suspended threads.
  resume_all_except(process, thread);

  // Write back the stack data that we crapped all over.

  status_write =
     ValidatedWriteProcessMemory 
         (process->ProcessHandle,
          (LPVOID) instruction_position,
          (LPVOID) &saved_stack_memory,
          sizeof(ALLOCATOR_INSTRUCTION_SEQUENCE),
          &bytes_written);

  // Get the context again. This should have ESP correctly set.
  status_get_context = GetThreadContext(thread->ThreadHandle, &context);

  return((TARGET_ADDRESS) (context.Esp));
}


TARGET_ADDRESS nub_setup_function_call 
  (NUB nub, 
   NUBTHREAD thread,
   TARGET_ADDRESS function, 
   NUBINT arg_count,
   TARGET_ADDRESS *args,
   NUBHANDLE *context_cookie)
{
  LPDBGPROCESS        process = (LPDBGPROCESS) nub;
  LPDBGTHREAD         threadC = (LPDBGTHREAD) thread;
  CONTEXT             context;
  THREAD_MEMORY       *saved_thread 
                       = (THREAD_MEMORY*) malloc (sizeof(THREAD_MEMORY));
  BOOL                status;
  DWORD               stack_position;
  DWORD               original_IP;
  DWORD               i = 0;
  BOOL                write_status;
  DWORD               bytes_written;
  TARGET_ADDRESS      address_to_break;

  // suspend_thread(threadC);

  // Now get context information. We need to know the return address
  // for our frame-to-be, and also the stack pointer + frame pointer
  // as is.

  context.ContextFlags = CONTEXT_FULL;

  status = get_thread_context(process, threadC, &context);

  dylan_debugger_message("nub_setup_function_call: Thread Context: %= : %=",
			 threadC->ThreadHandle, status);
  dylan_debugger_message("Esp: %=  Eip: %=",
			 context.Esp,
			 context.Eip);

  //print_context("Context pulled from thread state", &context);

  // Now remember everything about the debug state of this thread.

  saved_thread->ThreadState = threadC->ThreadState;
  saved_thread->WaitingForDebugger = threadC->WaitingForDebugger;
  saved_thread->SingleStepping = threadC->SingleStepping;
  saved_thread->NeedsBreakpointReplacement 
     = threadC->NeedsBreakpointReplacement;
  saved_thread->BreakpointToReplace = threadC->BreakpointToReplace;
  saved_thread->StoppedState = threadC->StoppedState;
  saved_thread->LastReceivedEvent = threadC->LastReceivedEvent;
  saved_thread->NubCodeOfLastEvent = threadC->NubCodeOfLastEvent;
  saved_thread->ThreadContext = context;

  // Allocate enough space on the stack to hold the arguments to the
  // remote function, and the return address.

  stack_position = 
     (DWORD) nub_allocate_stack_space
       (nub, 
        thread,
        ((DWORD) (arg_count + 1)) * sizeof(DWORD));

  if (stack_position == 0x0) {
    debugger_error("Serious Error: Failed to allocate stack in Spy call on Thread %=",
		   (TARGET_ADDRESS)threadC->ThreadHandle,
		   (TARGET_ADDRESS)NULL);
    // Internal error
    return(NULL);
  }

  // And get ready for the remote call. If the thread was stopped at a
  // breakpoint, we need to override that, because we are going to alter
  // the instruction pointer.

  if (saved_thread->NeedsBreakpointReplacement) {
    LPDEBUG_POINT breakpoint = saved_thread->BreakpointToReplace;
    drop_breakpoint(process, breakpoint);
    threadC->NeedsBreakpointReplacement = FALSE;
    if (!(saved_thread->SingleStepping)) {
      context.EFlags = context.EFlags & 0xFFFFFEFF;
    }
    // And resume those threads that will have been suspended.
    // resume_all_except(process, thread);
  }

  if (!status) {
    // Internal nub error.
    return (NULL);
  }

  //print_context("Context being saved", &(saved_thread->ThreadContext));

  // Grab the return address so that the access path chappies can set a
  // breakpoint on it to clean up the stack.

  address_to_break = (TARGET_ADDRESS) context.Eip;
  original_IP = context.Eip;

  // DIY stack frame!!!!
  // We are using the C calling convention to bring about our remote
  // call. At the point of call, the new stack frame must have all the
  // arguments pushed, followed by the return address.

  context.Esp = stack_position;

  // And make the instruction pointer point to our function.

  context.Eip = (DWORD) function;

  // The stack should now be fooling this thread into thinking that
  // it has to execute our remote function, which it will go off and do
  // as soon as the application resumes. But we have to set the context.

  status = SetThreadContext (threadC->ThreadHandle, &context);

  if (!status) {
    // Internal nub error.
    return(NULL);
  }

  // print_context("Context set back to thread", &context);
  // Push the return address - ie, the next instruction that was going
  // to be executed, before we started messing about...

  write_status =
    ValidatedWriteProcessMemory 
      (process->ProcessHandle,
       (LPVOID) stack_position,
       (LPVOID) &(original_IP),
       sizeof(TARGET_ADDRESS),
       &bytes_written);

  stack_position += sizeof(TARGET_ADDRESS);

  if ((!write_status) || (bytes_written != sizeof(TARGET_ADDRESS))) {
    // Internal nub error.
    return (NULL);
  }

  // Push the argument array.

  for (i = 0; i < (DWORD) arg_count; i++) {

    write_status =
      ValidatedWriteProcessMemory 
        (process->ProcessHandle,
         (LPVOID) stack_position,
         (LPVOID) &(args[i]),
         sizeof(TARGET_ADDRESS),
         &bytes_written);

    if ((!write_status) || (bytes_written != sizeof(TARGET_ADDRESS))) {
      // Internal nub error.
      return (NULL);
    }
    else {
      //printf ("Wrote the argument %x at %x.\n", args[i], stack_position);
    }
    stack_position += sizeof(TARGET_ADDRESS);
  }


  //print_context("Context set back to thread", &context);

  (*context_cookie) = (NUBHANDLE) saved_thread;
  //resume_thread(threadC);
  return (address_to_break);
}


void nub_stack_push_values
   (NUB nub, NUBTHREAD nubthread, NUBINT count, TARGET_ADDRESS *vals)
{
  LPDBGPROCESS          process = (LPDBGPROCESS) nub;
  LPDBGTHREAD           thread = (LPDBGTHREAD) nubthread;
  DWORD                 dwCount = (DWORD) count;
  DWORD                 i = 0;
  NUBINT                byte_space = count * sizeof(TARGET_ADDRESS);
  DWORD                 stack_address = 0;
  DWORD                 bytes_written;

  stack_address = (DWORD) nub_allocate_stack_space(nub, nubthread, byte_space);

  if (stack_address != 0) {
    for (i = 0; i < dwCount; i++) {
      ValidatedWriteProcessMemory
        (process->ProcessHandle,
         (LPVOID) stack_address,
         (LPVOID) &(vals[i]),
         sizeof(TARGET_ADDRESS),
         &bytes_written);
      stack_address += sizeof(TARGET_ADDRESS);
    }
  }
}


void nub_stack_pop_values
   (NUB nub, NUBTHREAD nubthread, NUBINT count, TARGET_ADDRESS *vals)
{
  LPDBGPROCESS          process = (LPDBGPROCESS) nub;
  LPDBGTHREAD           thread = (LPDBGTHREAD) nubthread;
  DWORD                 dwCount = (DWORD) count;
  DWORD                 i = 0;
  DWORD                 byte_space = dwCount * sizeof(TARGET_ADDRESS);
  DWORD                 stack_position;
  CONTEXT               context;
  BOOL                  status_get_context, status_set_context, status_read;
  DWORD                 bytes_read;

  context.ContextFlags = CONTEXT_FULL;
  status_get_context = GetThreadContext(thread->ThreadHandle, &context);
  stack_position = context.Esp;

  for (i = 0; i < dwCount; i++) {
    status_read = ReadProcessMemory(process->ProcessHandle,
                                    (LPCVOID) stack_position,
                                    (LPVOID) &(vals[i]),
                                    sizeof(TARGET_ADDRESS),
                                    &bytes_read);
    stack_position += sizeof(TARGET_ADDRESS);
  }

  context.Esp += byte_space;
  status_set_context = SetThreadContext(thread->ThreadHandle, &context);

}
