/* ************************************************************************* */
/* ** dylan-extensions.c                                                  ** */
/* ** A debugger nub extension unit that knows about Dylan code.          ** */
/* ** ------------------------------------------------------------------- ** */
/* ** Author: Paul Howard    Copyright: (c) 1996 Functional Objects, Inc. ** */
/* **                                   All Rights Reserved.              ** */
/* ************************************************************************* */

#include "nub-core.h"
#include "dylan-extensions.h"


static DYLAN_CALLING_CONVENTION our_dylan_calling_convention =
  {1, 1, {NUB_REGISTER_ILLEGAL, NUB_REGISTER_EAX}, NUB_REGISTER_EBX};

#define MAX_INSTRUCTION_SPACE 30

// Conveniently define some Intel instruction codes. This is basically
// a threadbare disassembler...

#define CALL_DIRECT 0xE8
#define BREAKPOINT 0xcc
#define CALL_INDIRECT 0xFF
#define CROSS_DLL_BOUNDARY 0x15
#define THROUGH_FUNCTION_REGISTER 0x53

TARGET_ADDRESS calculate_step_into_destination
    (NUB nub, NUBTHREAD nubthread, NUBINT *function_register_live,
     NUBINT *success)
{
   LPDBGPROCESS     process = (LPDBGPROCESS) nub;
   LPDBGTHREAD      thread = (LPDBGTHREAD) nubthread;
   CONTEXT          context;
   DWORD            bytes_read;
   BYTE             instruction[MAX_INSTRUCTION_SPACE];
   BOOL             status_get_context, status_read_instruction;
   BYTE             main_opcode, indirection_parameter;
   DWORD            destination = 0xDEADC0DE;
   DWORD            IP;
   BYTE             chances = 0;
   BOOL             candidate = FALSE;
   LPDBGLIBRARY     lib_origin, lib_destination;

   // Initialize the return values that are required to be
   // integers, otherwise any old crap can get returned.

   (*function_register_live) = (NUBINT) 0;
   (*success) = (NUBINT) 0;                 // Assume failure.

   context.ContextFlags = CONTEXT_CONTROL;
   status_get_context = GetThreadContext(thread->ThreadHandle, &context);
   IP = context.Eip;
   main_opcode = 0x00;

   while ((!candidate) && (chances < 100)) {
     status_read_instruction =
        ReadProcessMemory
            (process->ProcessHandle,
             (LPCVOID) IP,
             (LPVOID) instruction,
             MAX_INSTRUCTION_SPACE,
             &bytes_read);
     main_opcode = instruction[0];
     indirection_parameter = instruction[1];

     if (main_opcode == CALL_DIRECT)
       candidate = TRUE;
     else if ((main_opcode == CALL_INDIRECT) &&
               ((indirection_parameter == CROSS_DLL_BOUNDARY) || 
                (indirection_parameter == THROUGH_FUNCTION_REGISTER)))
       candidate = TRUE;
     else {
       IP++;
       chances++;
     }
   }

   switch (main_opcode) {
   case CALL_DIRECT:
     (*success) = 1;
     (*function_register_live) = 0;
     destination = (*((DWORD*) (instruction + 1))) + IP + 0x00000005;
     lib_origin = library_descriptor_from_address(process, context.Eip);
     lib_destination = library_descriptor_from_address(process, destination);
     if (lib_origin != lib_destination)
       (*success) = 0;
     break;

   case CALL_INDIRECT:
     switch(indirection_parameter) {
     case CROSS_DLL_BOUNDARY:
       (*success) = 1;
       (*function_register_live) = 0;
       ReadProcessMemory(process->ProcessHandle,
                         (LPCVOID) (*((DWORD*) (instruction + 2))),
                         (LPVOID) &destination,
                         sizeof(DWORD),
                         &bytes_read);
       break;

     case THROUGH_FUNCTION_REGISTER:
       (*success) = 1;
       (*function_register_live) = 1;
       break;

     default:
       (*success) = 0;
     }
     break;

   default:
     (*success) = 0;
   }
   return((TARGET_ADDRESS) destination);
}

void nub_dylan_range_of_enregistered_arguments
  (NUB nub,
   NUB_INDEX *first,
   NUB_INDEX *last)
{
   (*first) = (NUB_INDEX) our_dylan_calling_convention.FirstRegisterArg;
   (*last) = (NUB_INDEX) our_dylan_calling_convention.LastRegisterArg;
}


NUB_INDEX nub_dylan_argument_register_index
  (NUB nub,
   NUB_INDEX index)
{
  return 
    ((NUB_INDEX) our_dylan_calling_convention.RegisterIndices[(int) index]);
}


TARGET_ADDRESS nub_dylan_thread_environment_block_address
  (NUB nub,
   NUBTHREAD nubthread,
   NUBINT *valid)
{
   LPDBGPROCESS                       process = (LPDBGPROCESS) nub;
   LPDBGTHREAD                        thread = (LPDBGTHREAD) nubthread;
   GENERIC_THREAD_LOCAL_STORAGE_AREA  thread_local_storage;
   BOOL                               status;
   DWORD                              dummy;
   
   (*valid) = (NUBINT) 0;  // Assume we'll succeed!!

   // Read the generic thread local storage for the thread.
   // TODO: Verify that this is a dylan thread? (Or should a higher
   //       level be responsible for that?

   status =
     ReadProcessMemory
       (process->ProcessHandle,
        (LPCVOID) thread->ThreadLocalStorageAddress,
        (LPVOID) &thread_local_storage,
        sizeof(GENERIC_THREAD_LOCAL_STORAGE_AREA),
        &dummy);

   if (!status) {
     (*valid) = 0;
     return ((TARGET_ADDRESS) 0);
   }
   else {
     (*valid) = 1;
     return ((TARGET_ADDRESS) thread_local_storage.ThreadEnvironmentBlock);
   }
}


TARGET_ADDRESS nub_dylan_current_unwind_protect_frame
  (NUB nub,
   NUBTHREAD nubthread,
   NUBINT *valid)
{
   LPDBGPROCESS                       process = (LPDBGPROCESS) nub;
   LPDBGTHREAD                        thread = (LPDBGTHREAD) nubthread;
   GENERIC_THREAD_LOCAL_STORAGE_AREA  thread_local_storage;
   DYLAN_THREAD_ENVIRONMENT_BLOCK     thread_environment_block;
   BOOL                               status;
   DWORD                              dummy;
   
   (*valid) = (NUBINT) 0;  // Assume we'll succeed!!

   // Read the generic thread local storage for the thread.
   // TODO: Verify that this is a dylan thread? (Or should a higher
   //       level be responsible for that?

   status =
     ReadProcessMemory
       (process->ProcessHandle,
        (LPCVOID) thread->ThreadLocalStorageAddress,
        (LPVOID) &thread_local_storage,
        sizeof(GENERIC_THREAD_LOCAL_STORAGE_AREA),
        &dummy);

   if (!status) {
     (*valid) = 0;
     return ((TARGET_ADDRESS) 0);
   }

   // Read the DYLAN thread environment block

   status =
     ReadProcessMemory
       (process->ProcessHandle,
        (LPCVOID) thread_local_storage.ThreadEnvironmentBlock,
        (LPVOID) &thread_environment_block,
        sizeof(DYLAN_THREAD_ENVIRONMENT_BLOCK),
        &dummy);

   if (!status) {
     (*valid) = 0;
     return ((TARGET_ADDRESS) 0);
   }

   return ((TARGET_ADDRESS) thread_environment_block.DynamicEnvironment);
}


TARGET_ADDRESS nub_dylan_installed_handlers
  (NUB nub,
   NUBTHREAD nubthread,
   NUBINT *valid)
{
   LPDBGPROCESS                       process = (LPDBGPROCESS) nub;
   LPDBGTHREAD                        thread = (LPDBGTHREAD) nubthread;
   GENERIC_THREAD_LOCAL_STORAGE_AREA  thread_local_storage;
   DYLAN_THREAD_ENVIRONMENT_BLOCK     thread_environment_block;
   BOOL                               status;
   DWORD                              dummy;
   
   (*valid) = (NUBINT) 1;  // Assume we'll succeed!!

   // Read the generic thread local storage for the thread.
   // TODO: Verify that this is a dylan thread? (Or should a higher
   //       level be responsible for that?

   status =
     ReadProcessMemory
       (process->ProcessHandle,
        (LPCVOID) thread->ThreadLocalStorageAddress,
        (LPVOID) &thread_local_storage,
        sizeof(GENERIC_THREAD_LOCAL_STORAGE_AREA),
        &dummy);

   if ((!status) || (dummy != sizeof(GENERIC_THREAD_LOCAL_STORAGE_AREA))) {
     (*valid) = 0;
     return ((TARGET_ADDRESS) 0);
   }

   // Read the DYLAN thread environment block

   status =
     ReadProcessMemory
       (process->ProcessHandle,
        (LPCVOID) thread_local_storage.ThreadEnvironmentBlock,
        (LPVOID) &thread_environment_block,
        sizeof(DYLAN_THREAD_ENVIRONMENT_BLOCK),
        &dummy);

   if ((!status) || (dummy != sizeof(DYLAN_THREAD_ENVIRONMENT_BLOCK))) {
     (*valid) = 0;
     return ((TARGET_ADDRESS) 0);
   }

   if (thread_environment_block.CurrentHandlers == 0x000000) {
     (*valid) = 0;
     return ((TARGET_ADDRESS) 0);
   }

   return ((TARGET_ADDRESS) thread_environment_block.CurrentHandlers);
}


// On this platform, our function register is stored in Ebx.
// This function does not attempt to determine whether or not the function
// register is live. That determination should already have been made.

TARGET_ADDRESS nub_dylan_current_function
  (NUB nub,
   NUBTHREAD nubthread)
{
  LPDBGPROCESS           process = (LPDBGPROCESS) nub;
  LPDBGTHREAD            thread = (LPDBGTHREAD) nubthread;
  BOOL                   status;
  CONTEXT                context;

  context.ContextFlags = CONTEXT_FULL;
  status = GetThreadContext(thread->ThreadHandle, &context);
  return ((TARGET_ADDRESS) context.Ebx);
}


NUBINT nub_dylan_thread_mv_buffer_live
  (NUB nub,
   NUBTHREAD nubthread)
{
  LPDBGPROCESS           process = (LPDBGPROCESS) nub;
  LPDBGTHREAD            thread = (LPDBGTHREAD) nubthread;
  BOOL                   status;
  CONTEXT                context;
  DWORD                  flags;
  DWORD                  mask = 0x00000400;

  context.ContextFlags = CONTEXT_FULL;
  status = GetThreadContext(thread->ThreadHandle, &context);
  flags = context.EFlags;
  if ((flags & mask) > 0x00000000)
    return((NUBINT) 0);
  else
    return((NUBINT) 1);
}


void nub_dylan_unwind_protect_frame_contents
  (NUB nub,
   TARGET_ADDRESS uwp,
   TARGET_ADDRESS *parent,
   TARGET_ADDRESS *prev_uwp,
   TARGET_ADDRESS *cleanup)
{
   LPDBGPROCESS          process = (LPDBGPROCESS) nub;
   UNWIND_PROTECT_FRAME  the_frame;
   BOOL                  status;
   DWORD                 dummy;

   // Read in the entire unwind-protect frame from the stack.

   status = 
     ReadProcessMemory(process->ProcessHandle,
                       (LPCVOID) uwp,
                       (LPVOID) &the_frame,
                       sizeof(UNWIND_PROTECT_FRAME),
                       &dummy);

   // Just do type conversions on the members of the struct.

   (*parent) = (TARGET_ADDRESS) the_frame.ParentFramePointer;
   (*prev_uwp) = (TARGET_ADDRESS) the_frame.PreviousUnwindProtectFrame;
   (*cleanup) = (TARGET_ADDRESS) the_frame.CleanupCodeAddress;
}


void nub_dylan_bind_exit_frame_contents
  (NUB nub,
   TARGET_ADDRESS bx,
   TARGET_ADDRESS *parent,
   TARGET_ADDRESS *continuation,
   TARGET_ADDRESS *stored_uwp,
   TARGET_ADDRESS *mv_vec)
{
   LPDBGPROCESS          process = (LPDBGPROCESS) nub;
   BIND_EXIT_FRAME       the_frame;
   BOOL                  status;
   DWORD                 dummy;

   // Read in the entire bind-exit frame from the stack.

   status = 
     ReadProcessMemory(process->ProcessHandle,
                       (LPCVOID) bx,
                       (LPVOID) &the_frame,
                       sizeof(BIND_EXIT_FRAME),
                       &dummy);

   // Just do type conversions on the members of the struct.

   (*parent) = (TARGET_ADDRESS) the_frame.ParentFramePointer;
   (*continuation) = (TARGET_ADDRESS) the_frame.ContinuationAddress;
   (*stored_uwp) = (TARGET_ADDRESS) the_frame.StoredUnwindProtectFrame;
   (*mv_vec) = (TARGET_ADDRESS) the_frame.MVPointer;
}


NUBINT nub_older_stack_frame
  (NUB nub,
   TARGET_ADDRESS this_one,
   TARGET_ADDRESS than_this_one)
{

   // The advantage of this function is that it can compare stack frames
   // of all varieties: unwind-protects, bind-exits and calls.

   DWORD first = (DWORD) this_one;
   DWORD second = (DWORD) than_this_one;

   // Roses are red,
   // Violets are blue,
   // Our stacks grow downwards,
   // How about you?

   if (first > second)
     return ((NUBINT) 1);
   else
     return ((NUBINT) 0);
}


TARGET_ADDRESS nub_dylan_resolve_keyword
  (NUB nub,
   char *keyword_string,
   NUBINT string_length,
   TARGET_ADDRESS vector,
   TARGET_ADDRESS cursor,
   NUBINT *found)
{
   LPDBGPROCESS   process = (LPDBGPROCESS) nub;
   DWORD          keys = (DWORD) vector;
   DWORD          end = keys + (DWORD) cursor;
   DWORD          this_symbol;
   DWORD          this_symbol_name;
   BOOL           status;
   DWORD          dummy;
   BOOL           located = FALSE;
   DWORD          our_length = (DWORD) string_length;
   DWORD          candidate_length;
   char           candidate_buffer[1000]; // Assume no symbol can be bigger??

   (*found) = (NUBINT) 0;

   // The vector is a SOV instance, so there is a wrapper field and a
   // size field to skip.

   keys += 8;

   while ((!located) && (keys < end)) {

     // Read an element from the vector. This should be a pointer to a
     // <symbol> instance.
     // At any time, if ReadProcessMemory goes tits-up, just return a
     // failure code. (This is all temporary anyway).

     status =
       ReadProcessMemory(process->ProcessHandle,
                         (LPCVOID) keys,
                         (LPVOID) &this_symbol,
                         sizeof(DWORD),
                         &dummy);
     if (!status) {
       keys += 4;
       continue;
     }

     // Read the pointer to the symbol's name. This follows the wrapper
     // field in the symbol object, so its 4 bytes ahead of the start
     // of the symbol object.

     status =
       ReadProcessMemory(process->ProcessHandle,
                         (LPCVOID) (this_symbol + 4),
                         (LPVOID) &this_symbol_name,
                         sizeof(DWORD),
                         &dummy);
     if (!status) {
       keys += 4;
       continue;
     }

     // The name should be a <byte-string>, meaning it will have a wrapper
     // field, a tagged-integer size field, and the string itself.
     // First, read the size.

     status = ReadProcessMemory(process->ProcessHandle,
                                (LPCVOID) (this_symbol_name + 4),
                                (LPVOID) &candidate_length,
                                sizeof(DWORD),
                                &dummy);
     if (!status) {
       keys += 4;
       continue;
     }

     // Untag the integer.

     candidate_length = candidate_length >> 2;

     // Now, read the data into a buffer.

     status = ReadProcessMemory(process->ProcessHandle,
                                (LPCVOID) (this_symbol_name + 8),
                                (LPVOID) candidate_buffer,
                                candidate_length,
                                &dummy);
     if (!status) {
       keys += 4;
       continue;
     }

     // If the two sizes are equal, then we test the characters against
     // each other for a match. Otherwise, this isn't a match.

     if (candidate_length == our_length) {
       DWORD i = 0;
       BOOL  match = TRUE;
       while ((match) && (i < candidate_length)) {
         if (keyword_string[i] != candidate_buffer[i])
           match = FALSE;
         else
           i++;
       }
       if (match) {
         located = TRUE;
         (*found) = 1;
         return ((TARGET_ADDRESS) this_symbol);
       }
       else {
         keys += 4;
       }
     }
     else {
       keys += 4;
     }
   }
   return((TARGET_ADDRESS) NULL);
}


TARGET_ADDRESS nub_dylan_resolve_keyword_CBE
  (NUB nub,
   char *keyword_string,
   NUBINT string_length,
   TARGET_ADDRESS vector,
   TARGET_ADDRESS cursor,
   NUBINT *found)
{
   LPDBGPROCESS   process = (LPDBGPROCESS) nub;
   DWORD          keys = (DWORD) vector;
   DWORD          end = keys + (((DWORD) cursor) * 4);
   DWORD          this_symbol;
   DWORD          this_symbol_name;
   BOOL           status;
   DWORD          dummy;
   BOOL           located = FALSE;
   DWORD          our_length = (DWORD) string_length;
   DWORD          candidate_length;
   char           candidate_buffer[1000]; // Assume no symbol can be bigger??

   (*found) = (NUBINT) 0;

   while ((!located) && (keys < end)) {

     // Read an element from the vector. This should be a pointer to a
     // <symbol> instance.
     // At any time, if ReadProcessMemory goes tits-up, just return a
     // failure code. (This is all temporary anyway).

     status =
       ReadProcessMemory(process->ProcessHandle,
                         (LPCVOID) keys,
                         (LPVOID) &this_symbol,
                         sizeof(DWORD),
                         &dummy);
     if (!status)
       return((TARGET_ADDRESS) NULL);

     // Read the pointer to the symbol's name. This follows the wrapper
     // field in the symbol object, so its 4 bytes ahead of the start
     // of the symbol object.

     status =
       ReadProcessMemory(process->ProcessHandle,
                         (LPCVOID) (this_symbol + 4),
                         (LPVOID) &this_symbol_name,
                         sizeof(DWORD),
                         &dummy);
     if (!status)
       return((TARGET_ADDRESS) NULL);

     // The name should be a <byte-string>, meaning it will have a wrapper
     // field, a tagged-integer size field, and the string itself.
     // First, read the size.

     status = ReadProcessMemory(process->ProcessHandle,
                                (LPCVOID) (this_symbol_name + 4),
                                (LPVOID) &candidate_length,
                                sizeof(DWORD),
                                &dummy);
     if (!status)
       return((TARGET_ADDRESS) NULL);

     // Untag the integer.

     candidate_length = candidate_length >> 2;

     // Now, read the data into a buffer.

     status = ReadProcessMemory(process->ProcessHandle,
                                (LPCVOID) (this_symbol_name + 8),
                                (LPVOID) candidate_buffer,
                                candidate_length,
                                &dummy);
     if (!status)
       return((TARGET_ADDRESS) NULL);

     // If the two sizes are equal, then we test the characters against
     // each other for a match. Otherwise, this isn't a match.

     if (candidate_length == our_length) {
       DWORD i = 0;
       BOOL  match = TRUE;
       while ((match) && (i < candidate_length)) {
         if (keyword_string[i] != candidate_buffer[i])
           match = FALSE;
         else
           i++;
       }
       if (match) {
         located = TRUE;
         (*found) = 1;
         return ((TARGET_ADDRESS) this_symbol);
       }
       else {
         keys += 4;
       }
     }
     else {
       keys += 4;
     }
   }
}

// PANTS!!!!!!!!!!

DWORD nub_primitive_raw_remote_value
   (TARGET_ADDRESS x)
{
  return((DWORD) x);
}
