/* ********************************************************************** */
/* ** stack_trace.c                                                    ** */
/* ** Functions for stack walking, getting frame info.                 ** */
/* ** ---------------------------------------------------------------- ** */
/* ** Author: Paul Howard, Copyright: Functional Objects, Inc. 1996 ** */
/* **                                 All Rights Reserved.             ** */
/* ********************************************************************** */

#include "nub-core.h"

#define DYNAMIC_CHAIN_TERMINATOR 0
#define CODE_NOT_IN_ANY_DLL 0xDEADC0DE

LPDBGPROCESS     process_being_traced = NULL;

/*
NUBINT nub_number_of_frames_on_stack (NUB nub, NUBTHREAD nubthread)
{
  NUBFRAME   frame;
  int        count = 0;

  frame = nub_initialize_stack_trace(nub, nubthread);
  while (frame != NULL) {
    count++;
    frame = nub_previous_frame(nub, frame);
  }
  return ((NUBINT) count);
}
*/

void nub_register_interactive_code_segment
  (NUB nub, TARGET_ADDRESS lower, TARGET_ADDRESS upper)
{
  LPDBGPROCESS               process = (LPDBGPROCESS) nub;
  INTERACTIVE_SEGMENT_TABLE  *table_to_use = NULL;
  DWORD                      i = 0;

  // If this is the first segment of interactive code to be registered for
  // this process, then allocate and initialize the first segment table,
  // and make it the head of the list

  if (process->InteractiveCodeSegments.FirstSegmentTable == NULL) {
    table_to_use = 
      (INTERACTIVE_SEGMENT_TABLE*) malloc (sizeof(INTERACTIVE_SEGMENT_TABLE));
    table_to_use->Next = NULL;
    table_to_use->NextFreeIndex = 0;
    process->InteractiveCodeSegments.FirstSegmentTable = table_to_use;
    process->InteractiveCodeSegments.CurrentSegmentTable = table_to_use;
  }

  // If the table currently being used to store segment boundaries has
  // become fill, then allocate/initialize a new table, and join it onto
  // the list.

  else if 
    (process->InteractiveCodeSegments.CurrentSegmentTable->NextFreeIndex ==
           INTERACTIVE_CODE_TABLE_GRANULARITY) {
    table_to_use = 
      (INTERACTIVE_SEGMENT_TABLE*) malloc (sizeof(INTERACTIVE_SEGMENT_TABLE));
    table_to_use->Next = NULL;
    table_to_use->NextFreeIndex = 0;
    process->InteractiveCodeSegments.CurrentSegmentTable->Next = table_to_use;
    process->InteractiveCodeSegments.CurrentSegmentTable = table_to_use;
  }

  // Otherwise, just add this new boundary to the current table.

  else {
    table_to_use = process->InteractiveCodeSegments.CurrentSegmentTable;
  }

  // Grab the array index at which we are going to store the boundary, and
  // also increment for the next registration.

  i = table_to_use->NextFreeIndex;
  (table_to_use->NextFreeIndex)++;

  // Go ahead and store the boundary.

  table_to_use->Segments[i].SegmentLowerBoundary = (DWORD) lower;
  table_to_use->Segments[i].SegmentUpperBoundary = (DWORD) upper;
}


BOOL code_address_is_interactive(LPDBGPROCESS process, DWORD IP, DWORD *base)
{
  INTERACTIVE_SEGMENT_TABLE    *this_table = 
                            process->InteractiveCodeSegments.FirstSegmentTable;


  while (this_table != NULL) {
    DWORD i = 0;
    for (i = 0; i < this_table->NextFreeIndex; i++) {
      if ((this_table->Segments[i].SegmentLowerBoundary <= IP) &&
          (this_table->Segments[i].SegmentUpperBoundary >= IP)) {
        (*base) = this_table->Segments[i].SegmentLowerBoundary;
        return(TRUE);
      }
    }
    this_table = this_table->Next;
  }
  return (FALSE);
}


DWORD __stdcall generic_DLL_base_address_finder (HANDLE process_handle, DWORD IP)
{
  DWORD           base_address;
  LPDBGLIBRARY    module;

  if (code_address_is_interactive(process_being_traced, IP, &base_address)) {
    return(process_being_traced->LibraryList->ImageInformation.ImageBase);
  }
  else {
    module = library_descriptor_from_address(process_being_traced, IP);
    return(module->ImageInformation.ImageBase);
  }
}


LPVOID __stdcall generic_function_table_finder (HANDLE process_handle, DWORD IP_base)
{
  DWORD           base;
  LPDBGLIBRARY    module;

  if (IP_base == 0) return (NULL);

  if (code_address_is_interactive(process_being_traced, IP_base, &base)) {
    return(NULL);
  }
  else {
    module = 
      library_descriptor_from_base_address(process_being_traced, IP_base);
    ensure_debug_information_for_library(process_being_traced, module);
    if (module->DebugMap) {
      return((LPVOID) (module->DebugMap->FunctionTableEntries));
    }
    else {
      return(NULL);
    }
  }
}


NUBINT nub_initialize_stack_vectors (NUB nub, NUBTHREAD nubthread)
{
  LPDBGPROCESS      process = (LPDBGPROCESS) nub;
  LPDBGTHREAD       thread = (LPDBGTHREAD) nubthread;
  STACKFRAME        frame;
  CONTEXT           context;
  BOOL              status;
  DWORD             counter = 0;
  DWORD             fp_new = 0;
  DWORD             fp_in_hand = 0;

  process_being_traced = process;

  // There's a chance that we already have this information cached.
  // Don't walk the stack if we don't need to!

  // if (thread->StackTraceValid)
  //   return ((NUBINT) thread->StackTraceSize);

  // Fill in part of the STACKFRAME structure that will allow us to get
  // the topmost frame. For this, we need the thread's register set
  // (the context).

  context.ContextFlags = CONTEXT_FULL;
  status = GetThreadContext(thread->ThreadHandle, &context);

  // HACK: ZeroMemory uses memset which isn't part of our minimal C runtime ...
  {
    BYTE *p = (BYTE*)&frame;
    int  i;
    for (i = 0; i < sizeof(STACKFRAME); i++)
      *p++ = 0;
  }
  frame.AddrPC.Offset = context.Eip;
  frame.AddrPC.Mode = AddrModeFlat;
  frame.AddrStack.Offset = context.Esp;
  frame.AddrStack.Mode = AddrModeFlat;
  frame.AddrFrame.Offset = context.Ebp;
  frame.AddrFrame.Mode = AddrModeFlat;
  frame.Virtual = FALSE; // The need for this is not documented by Win32!!

  while (status) {

    // Call Win32 StackWalk to get the data for the frame.
 
    status =
      StackWalk(IMAGE_FILE_MACHINE_I386,
                process->ProcessHandle,
                thread->ThreadHandle,
                &frame,
                (LPVOID) &context,
                NULL,  // Use ReadProcessMemory by default.
                (PFUNCTION_TABLE_ACCESS_ROUTINE) generic_function_table_finder,
                (PGET_MODULE_BASE_ROUTINE) generic_DLL_base_address_finder,
                NULL);   // No need to translate 16/32-bit addresses

    fp_in_hand = fp_new;
    fp_new = frame.AddrFrame.Offset;

    if ((counter > 0) && (!status)) {
      // Terminating case 1
      // StackWalk fails after reading at least one stack frame.
      // (This should be the 'norm')
      thread->StackTraceSize = (int) counter;
      thread->StackTraceValid = TRUE;
      thread->StackTraceUsingOverflow = FALSE;
      return ((NUBINT) counter);
    }
    else if ((counter > 0) && (fp_new <= fp_in_hand)) {
      // Terminating case 2
      // StackWalk succeeded, but the frame that has been generated seems
      // inconsistent with a normal downward-growing stack, so we second-guess
      // it, and terminate the trace.
      thread->StackTraceSize = (int) counter;
      thread->StackTraceValid = TRUE;
      thread->StackTraceUsingOverflow = FALSE;
      return ((NUBINT) counter);
    }
    else if (counter == STACK_TRACE_MAX_SIZE) {
      // Terminating case 3
      // StackWalk succeeded, but the trace has just grown too large to
      // accommodate.
      thread->StackTraceSize = (int) counter;
      thread->StackTraceValid = TRUE;
      thread->StackTraceUsingOverflow = FALSE;
      return ((NUBINT) counter);
    }
    else {
      // Contination case.
      // StackWalk succeeded, or failed having generated no frames at all.
      // Since we cannot tolerate stack traces of zero size, we will not
      // terminate in that latter circumstance.

      // Fill in the elements in the stack trace cache before we go on
      // to destructively modify the frame descriptor
      thread->StackTraceStackPointers[counter] = frame.AddrStack.Offset;
      thread->StackTraceFramePointers[counter] = fp_new;
      thread->StackTraceInstrPointers[counter] = frame.AddrPC.Offset;
      thread->StackTraceRetrnPointers[counter] = frame.AddrReturn.Offset;
      counter++;
    }
  }
  thread->StackTraceSize = (int) counter;
  thread->StackTraceValid = TRUE;
  thread->StackTraceUsingOverflow = FALSE;
  return ((NUBINT) counter);
}

void nub_read_stack_vectors (NUB nub, NUBTHREAD nubthread,
                              NUBINT frame_count,
                              TARGET_ADDRESS *frame_pointers,
                              TARGET_ADDRESS *instruction_pointers,
                              TARGET_ADDRESS *return_addresses)
{
  LPDBGPROCESS     process = (LPDBGPROCESS) nub;
  LPDBGTHREAD      thread = (LPDBGTHREAD) nubthread;
  DWORD            final = (DWORD) (thread->StackTraceSize - 1);
  DWORD            i;

  if (!thread->StackTraceValid) {
    return;
  }

  for (i = 0; i <= final; i++) {
    frame_pointers[i] = 
       (TARGET_ADDRESS) (thread->StackTraceFramePointers[i]);
    instruction_pointers[i] =
       (TARGET_ADDRESS) (thread->StackTraceInstrPointers[i]);
    return_addresses[i] =
       (TARGET_ADDRESS) (thread->StackTraceRetrnPointers[i]);
  }
}


/* ********************************************************************* 
   **         OBSOLETE FROM HERE TO NEXT LINE OF ASTRISKS             ** 


NUBFRAME nub_initialize_stack_trace (NUB nub, NUBTHREAD thread)
{
  LPDBGTHREAD      threadC = (LPDBGTHREAD) thread;
  LPDBGPROCESS     process = (LPDBGPROCESS) nub;
  BOOL             status;
  CONTEXT          context;
  LPDBGFRAME       frame;

  // While stopped, the stack trace will be cached in a linked list.
  // If we have this list available to us now, just return it.

  if (threadC->CurrentStackTrace != NULL) 
    return ((NUBFRAME) (threadC->CurrentStackTrace));

  // Otherwise, we definitely have to return a frame, so malloc the 
  // descriptor.

  frame = (LPDBGFRAME) malloc (sizeof(DBGFRAME));
  frame->PreviousFrame = NULL;
  frame->IsLastFrame = FALSE;

  // Get the thread context.

  context.ContextFlags = CONTEXT_FULL;

  status = GetThreadContext (threadC->ThreadHandle, &context);

  if (!status) {
    // Internal error. Just build a pants stack frame and be 'appy.
    frame->StackFrameThread = threadC;
    frame->IsStackWalkFrame = FALSE;
    frame->FramePointer = DYNAMIC_CHAIN_TERMINATOR;
    frame->InstructionAddress = 0;
    frame->ReturnAddress = 0;
    frame->IsFpoFrame = FALSE;
    (threadC->CurrentStackTrace) = frame;
    return ((NUBFRAME) frame);
  }
  else {
    // We are going to try StackWalk to get the stack frame.
    // First, we have to fill in a certain amount of information
    // in the STACKFRAME structure.

    // But, before we do anything, make sure the stack frame points
    // back to its thread.

    frame->StackFrameThread = threadC;

    // Fill in the slots we are responsible for.

    frame->StackWalkFrame.AddrPC.Offset = context.Eip;
    frame->StackWalkFrame.AddrPC.Segment = context.SegCs;
    frame->StackWalkFrame.AddrPC.Mode = AddrModeFlat;
    frame->StackWalkFrame.AddrStack.Offset = context.Esp;
    frame->StackWalkFrame.AddrStack.Segment = context.SegSs;
    frame->StackWalkFrame.AddrStack.Mode = AddrModeFlat;
    frame->StackWalkFrame.AddrFrame.Offset = context.Ebp;
    frame->StackWalkFrame.AddrFrame.Segment = context.SegSs;
    frame->StackWalkFrame.AddrFrame.Mode = AddrModeFlat;
    frame->StackWalkFrame.Virtual = FALSE;

    // Call NT StackWalk for the frame. If it succeeds, polish the
    // descriptor and make the current stack trace point to it.

    status =
      StackWalk(IMAGE_FILE_MACHINE_I386,
                process->ProcessHandle,
                threadC->ThreadHandle,
                &(frame->StackWalkFrame),
                NULL,
                NULL,
                (PFUNCTION_TABLE_ACCESS_ROUTINE) SymFunctionTableAccess,
                (PGET_MODULE_BASE_ROUTINE) SymGetModuleBase,
                NULL);

    if (status) {
      // StackWalk at least _thinks_ it worked. Unfortunately, we can't
      // really do very much to check this.

      frame->IsStackWalkFrame = TRUE;
      
    }
    else {
      // StackWalk went tits-up on us, so we can only take our best guess
      // at the stack frame, based on the thread context registers, and
      // the return address read from the stack itself.

      DWORD        return_address;
      DWORD        bytes_read;

      // If you want something doing round here, you have to do it
      // yourself...

      status =
        ReadProcessMemory(process->ProcessHandle,
                          (LPCVOID) (context.Ebp + sizeof(DWORD)),
                          (LPVOID) &return_address,
                          sizeof(DWORD),
                          &bytes_read);
                          
      frame->IsStackWalkFrame = FALSE;
      frame->FramePointer = context.Ebp;
      frame->InstructionAddress = context.Eip;
      frame->ReturnAddress = return_address;
    }
    (threadC->CurrentStackTrace) = frame;
    return ((NUBFRAME) frame);
  }
}


TARGET_ADDRESS nub_stack_pointer (NUB nub, NUBTHREAD thread)
{
  LPDBGTHREAD      threadC = (LPDBGTHREAD) thread;
  LPDBGPROCESS     process = (LPDBGPROCESS) nub;
  BOOL             status;
  CONTEXT          context;
 
  // Get the thread context. The initial frame is that pointed to
  // by Ebp in the context.

  context.ContextFlags = CONTEXT_FULL;

  status = GetThreadContext (threadC->ThreadHandle, &context);

  if (!status) {
    // Internal error.
    return ((TARGET_ADDRESS) DYNAMIC_CHAIN_TERMINATOR);
  }
  return ((TARGET_ADDRESS) context.Esp);
}


NUBFRAME nub_previous_frame (NUB nub, NUBFRAME nubframe)
{
  LPDBGPROCESS       process = (LPDBGPROCESS) nub;
  LPDBGFRAME         frame = (LPDBGFRAME) nubframe;
  LPDBGTHREAD        thread = frame->StackFrameThread;
  BOOL               status;
  DWORD              frame_pointer;
  DWORD              bytes_read;

  // If this frame already has a previous frame associated with
  // it, ie. one that has been build already, just return it.

  if (frame->PreviousFrame != NULL)
    return ((NUBFRAME) frame->PreviousFrame);

  // If this frame is known to be the last available frame, just
  // return NULL.

  if (frame->IsLastFrame)
    return ((NUBFRAME) NULL);

  // If this frame is a StackWalk frame, then we will call StackWalk
  // again to generate the previous frame.

  if (frame->IsStackWalkFrame) {
    LPDBGFRAME          previous = (LPDBGFRAME) malloc (sizeof(DBGFRAME));
    CONTEXT             context;

    previous->PreviousFrame = NULL;
    previous->StackFrameThread = frame->StackFrameThread;
    previous->IsLastFrame = FALSE;

    // Copy the StackWalk frame we have already.
    (previous->StackWalkFrame) = (frame->StackWalkFrame);

    // I'm not sure how necessary this is.
    GetThreadContext(thread->ThreadHandle, &context);

    status =
       StackWalk(IMAGE_FILE_MACHINE_I386,
                 process->ProcessHandle,
                 thread->ThreadHandle,
                 &(previous->StackWalkFrame),
                 NULL,
                 NULL,
                 (PFUNCTION_TABLE_ACCESS_ROUTINE) SymFunctionTableAccess,
                 (PGET_MODULE_BASE_ROUTINE) SymGetModuleBase,
                 NULL);
                 
    if (!status) {
      // Assume this time that StackWalk has failed because the stack trace
      // has just plain run out!. In this case, the frame we have in hand
      // can be flagged as being the end of the stack.

      free(previous);
      frame->IsLastFrame = TRUE;
      return((NUBFRAME) NULL);
    }
    else {

      // Take care. StackWalk can produce infinite stack traces, so ensure
      // that this frame pointer makes sense.

      DWORD   fp_in_hand = 0;
      DWORD   fp_new = 0;

      previous->IsStackWalkFrame = TRUE;

      fp_in_hand = (DWORD) nub_frame_pointer(nub, nubframe);
      fp_new = (DWORD) nub_frame_pointer(nub, (NUBFRAME) previous);

      if (fp_new <= fp_in_hand) {
        // This is inconsistent with a downwards-growing stack.
        free(previous);
        frame->IsLastFrame = TRUE;
        return((NUBFRAME) NULL);
      }

      // Link this frame to the stack trace.

      frame->PreviousFrame = previous;
      return((NUBFRAME) previous);
    }
  }
  else {
    // The semantics of frame linkage dictate that the frame pointer
    // points to the saved frame pointer on the stack, maintaining a
    // dynamic chain. All we have to do is read this, as long as we
    // haven't already chained as far as we can.

    LPDBGFRAME          previous = (LPDBGFRAME) malloc (sizeof(DBGFRAME));
    DWORD               prev_frame_pointer, frame_pointer;
    DWORD               return_address;
    BOOL                fp_status, ra_status;

    frame_pointer = (DWORD) nub_frame_pointer(nub, nubframe);
    previous->PreviousFrame = NULL;
    previous->StackFrameThread = frame->StackFrameThread;
    previous->IsLastFrame = FALSE;  
    // Read the saved frame pointer.

    fp_status 
      = ReadProcessMemory 
         (process->ProcessHandle,
          (LPCVOID) frame_pointer,
          (LPVOID) &prev_frame_pointer,
          sizeof(DWORD),
          &bytes_read);

    // And the return address

    ra_status 
      = ReadProcessMemory 
         (process->ProcessHandle,
          (LPCVOID) (frame_pointer + sizeof(DWORD)),
          (LPVOID) &return_address,
          sizeof(DWORD),
          &bytes_read);

    if (fp_status && ra_status 
        && (prev_frame_pointer != 0) && (prev_frame_pointer < frame_pointer)) {
      previous->FramePointer = prev_frame_pointer;
      previous->ReturnAddress = return_address;
      previous->InstructionAddress = frame->ReturnAddress;
      previous->IsFpoFrame = FALSE;
      frame->PreviousFrame = previous;
      return ((NUBFRAME) previous);      
    }
    else {
      free(previous);
      frame->IsLastFrame = TRUE;
      return((NUBFRAME) NULL);
    }
  }
}


TARGET_ADDRESS nub_frame_pointer (NUB nub, NUBFRAME nubframe)
{
  LPDBGPROCESS       process = (LPDBGPROCESS) nub;
  LPDBGFRAME         frame = (LPDBGFRAME) nubframe;
  LPDBGTHREAD        thread;

  // We don't wanna do no indirection though no null pointer!

  if (frame == NULL) 
    return ((TARGET_ADDRESS) DYNAMIC_CHAIN_TERMINATOR);

  thread = frame->StackFrameThread;

  // If this frame was generated by StackWalk, return StackWalk's
  // interpretation of the frame pointer. Otherwise, return the
  // debugger nub's own interpretation of it.

  if (frame->IsStackWalkFrame)
    if (frame->StackWalkFrame.AddrFrame.Offset == 0)
      return ((TARGET_ADDRESS) (frame->StackWalkFrame.AddrStack.Offset));
    else
      return ((TARGET_ADDRESS) (frame->StackWalkFrame.AddrFrame.Offset));
  else
    return ((TARGET_ADDRESS) (frame->FramePointer));
}


TARGET_ADDRESS nub_frame_return_address (NUB nub, NUBFRAME nubframe)
{
  LPDBGPROCESS       process = (LPDBGPROCESS) nub;
  LPDBGFRAME         frame = (LPDBGFRAME) nubframe;
  LPDBGTHREAD        thread;

  // We don't wanna do no indirection though no null pointer!

  if (frame == NULL) 
    return ((TARGET_ADDRESS) 0);

  thread = frame->StackFrameThread;

  // If this frame was generated by StackWalk, return StackWalk's
  // interpretation of the return address. Otherwise, return the
  // debugger nub's own interpretation of it.

  if (frame->IsStackWalkFrame)
    return ((TARGET_ADDRESS) (frame->StackWalkFrame.AddrReturn.Offset));
  else
    return ((TARGET_ADDRESS) (frame->ReturnAddress));
}


TARGET_ADDRESS nub_thread_next_instruction_address (NUB nub, NUBTHREAD thread)
{
  LPDBGPROCESS       process = (LPDBGPROCESS) nub;
  LPDBGTHREAD        threadC = (LPDBGTHREAD) thread;
  BOOL               status;
  CONTEXT            context;
 
  // The next instruction for the top frame is equal to the thread's
  // Eip.

  context.ContextFlags = CONTEXT_FULL;

  status = GetThreadContext (threadC->ThreadHandle, &context);

  if (!status) {
    // Internal error
    return ((TARGET_ADDRESS) NULL);
  }
  return ((TARGET_ADDRESS) context.Eip);
}

 *********************************************************************** */


void nub_all_frame_lexicals 
  (NUB nub, TARGET_ADDRESS frame, TARGET_ADDRESS address,
   NUB_INDEX *first, NUB_INDEX *last,
   NUBHANDLE *lookups)
{
  LPDBGPROCESS      process = (LPDBGPROCESS) nub;
  DWORD             IP = (DWORD) address;
  DWORD             FP = (DWORD) frame;

  // Find out which module we should be looking in for the debug info.

  LPDBGLIBRARY      module = library_descriptor_from_address (process, IP);
  LOOKUP_TABLE      *table;
  DWORD             dwFirst, dwLast;

  if (module == NULL) {

    // No module descriptor for the code we're running...

    (*first) = (NUB_INDEX) 1;
    (*last)  = (NUB_INDEX) 0;
    (*lookups) = NULL;
    return;

  }

  switch (module->DebugType) {

  case CODEVIEW_IMAGE:
    table = all_lexicals_from_debug_map 
              (process, module, FP, IP, &dwFirst, &dwLast);
    break;

  case NONE:
  case COFF_IMAGE:
  case CODEVIEW_PDB:
    // Can't handle this yet.
    dwFirst = 1;
    dwLast = 0;
    table = NULL;
    break;

  default:
    dwFirst = 1;
    dwLast = 0;
    table = NULL;
  }

  (*first) = (NUB_INDEX) dwFirst;
  (*last) = (NUB_INDEX) dwLast;
  (*lookups) = (NUBHANDLE) table;
}
