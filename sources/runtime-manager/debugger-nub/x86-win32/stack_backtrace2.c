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
