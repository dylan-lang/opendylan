/* ************************************************************************* */
/* ** quick_and_dirty.c                                                   ** */
/* ** Bare minimal interpretation of CALL and JUMP instructions           ** */
/* ** ------------------------------------------------------------------- ** */
/* ** Author: Paul Howard    Copyright: (c) 1996 Functional Objects, Inc. ** */
/* **                                   All Rights Reserved.              ** */
/* ************************************************************************* */

#include "nub-core.h"

// This code is so bad it smells

#define INSTRUCTION_FLOW_LINEAR 1
#define INSTRUCTION_FLOW_CALL_DIRECT 2
#define INSTRUCTION_FLOW_CALL_INDIRECT 3
#define INSTRUCTION_FLOW_JUMP_DIRECT 4
#define INSTRUCTION_FLOW_JUMP_INDIRECT 5
#define INSTRUCTION_FLOW_RETURN 6
#define INSTRUCTION_FLOW_INTERRUPT 7
#define INSTRUCTION_FLOW_ILLEGAL 8

void nub_interpret_instruction_at_current_location
  (NUB nub,
   NUBTHREAD nubthread,
   NUBINT *flow,
   TARGET_ADDRESS *destination,
   NUBINT *size)
{
  LPDBGPROCESS       process = (LPDBGPROCESS) nub;
  LPDBGTHREAD        thread = (LPDBGTHREAD) nubthread;
  DWORD              IP;
  CONTEXT            context;
  BOOL               status;
  DWORD              dummy;
  BYTE               instruction[30];
  BYTE               opcode;

  context.ContextFlags = CONTEXT_FULL;
  status = GetThreadContext(thread->ThreadHandle, &context);
  (*flow) = (NUBINT) 0;
  (*size) = (NUBINT) 1;
  (*destination) = (TARGET_ADDRESS) 0;

  if (!status)
    return;

  // Save the instruction pointer from the thread.
  IP = context.Eip;

  // Read the instruction at this location out of the process memory.
  status = ReadProcessMemory(process->ProcessHandle,
                             (LPCVOID) IP,
                             (LPVOID) instruction,
                             (DWORD) 30,
                             &dummy);
  if (!status)
    return;

  // The opcode is the first byte of the instruction
  opcode = instruction[0];

  if (opcode == 0xE8) { // Can only cope with 32-bit standard offset CALL
    DWORD branch_from_IP = IP + 5;
    DWORD offset;
    DWORD *offset_ptr;
    DWORD dest;
    offset_ptr = (DWORD*) &(instruction[1]);
    offset = (*offset_ptr);
    dest = branch_from_IP + offset;
    (*flow) = (NUBINT) INSTRUCTION_FLOW_CALL_DIRECT;
    (*destination) = (TARGET_ADDRESS) dest;
    (*size) = (NUBINT) 5;
  }
  else {
    // Return pants values
    (*flow) = (NUBINT) INSTRUCTION_FLOW_LINEAR;
    (*destination) = (TARGET_ADDRESS) IP;
    (*size) = (NUBINT) 1;
  }
}
