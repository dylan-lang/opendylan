/* ********************************************************************** */
/* ** memory_access.c                                                  ** */
/* ** Functions for reading and writing primitive data types from the  ** */
/* ** memory space of the application.                                 ** */
/* ** ---------------------------------------------------------------- ** */
/* ** Author: Paul Howard, Copyright: Functional Objects, Inc. 1996 ** */
/* **                                 All Rights Reserved              ** */
/* ********************************************************************** */

#include "nub-core.h"

// Memory transactions can operate with "integer" types of various
// specific sizes, but the values communicated between the nub and
// the access-path are always in terms of NUBINT. These functions
// ensure the preservation of sign between NUBINT and (INT8, INT16,
// INT32, INT64). The words "extend" and "reduce" might not be
// perfect...


INT8 sign_reduce_integer_to_8 (NUBINT x)
{
  return ((INT8) x);
}


INT16 sign_reduce_integer_to_16 (NUBINT x)
{
  return ((INT16) x);
}


INT32 sign_reduce_integer_to_32 (NUBINT x)
{
  return ((INT32) x);
}


INT64 sign_reduce_integer_to_64 (NUBINT x)
{
  return ((INT64) x);
}


NUBINT sign_extend_8_to_integer (INT8 x)
{
  BYTE         byte = (BYTE) x;
  int          val = 0;
  BOOL         negative;

  if (byte & 0x80) {
    negative = TRUE;
    byte = -byte;
  }
  else {
    negative = FALSE;
  }

  val = (int) byte;

  if (negative)
    return ((NUBINT) -val);
  else
    return ((NUBINT) val);
}


NUBINT sign_extend_16_to_integer (INT16 x)
{
  WORD         word = (WORD) x;
  int          val = 0;
  BOOL         negative;

  if (word & 0x8000) {
    negative = TRUE;
    word = -word;
  }
  else {
    negative = FALSE;
  }

  val = (int) word;

  if (negative)
    return ((NUBINT) -val);
  else
    return ((NUBINT) val);
}


NUBINT sign_extend_32_to_integer (INT32 x)
{
  return ((NUBINT) x);
}


NUBINT sign_extend_64_to_integer (INT64 x)
{
  return ((NUBINT) x);
}


TARGET_ADDRESS read_value_from_process_memory 
  (NUB nub, 
   TARGET_ADDRESS address,
   NUBINT *error_code)
{
  BOOL         status;
  DWORD        number_bytes_transferred;
  NUBHANDLE    holder;
  LPDBGPROCESS process = (LPDBGPROCESS) nub;
 
  status 
    = ReadProcessMemory(process->ProcessHandle,
                        (LPCVOID) address,
                        (LPVOID)  &holder,
                        sizeof(TARGET_ADDRESS),
                        &number_bytes_transferred);

  if (!status || (number_bytes_transferred != sizeof(TARGET_ADDRESS))) {
    (*error_code) = (NUBINT) ACCESS_VIOLATION_ERROR;
    return (NULL);
  }
  else {
    (*error_code) = ACCESS_OK;
    return (holder);
  }
}


void write_value_to_process_memory 
  (NUB nub, 
   TARGET_ADDRESS address,
   TARGET_ADDRESS value, 
   NUBINT *error_code)
{
  BOOL         status;
  DWORD        number_bytes_transferred;
  LPDBGPROCESS process = (LPDBGPROCESS) nub;

  status 
    = ValidatedWriteProcessMemory (process->ProcessHandle,
                                   (LPVOID) address,
                                   (LPVOID) &value,
                                   sizeof(TARGET_ADDRESS),
                                   &number_bytes_transferred);

  if (!status || (number_bytes_transferred != sizeof(TARGET_ADDRESS))) {
    (*error_code) = (NUBINT) ACCESS_VIOLATION_ERROR;
  }
  else {
    (*error_code) = ACCESS_OK;
  }
}


NUBINT read_8b_from_process_memory 
  (NUB nub, 
   TARGET_ADDRESS address,
   NUBINT *error_code)
{
  BOOL         status;
  DWORD        number_bytes_transferred;
  INT8         holder;
  LPDBGPROCESS process = (LPDBGPROCESS) nub;
 
  status 
    = ReadProcessMemory(process->ProcessHandle,
                        (LPCVOID) address,
                        (LPVOID)  &holder,
                        sizeof(INT8),
                        &number_bytes_transferred);

  if (!status || (number_bytes_transferred != sizeof(INT8))) {
    (*error_code) = (NUBINT) ACCESS_VIOLATION_ERROR;
    return ((NUBINT) 0);
  }
  else {
    (*error_code) = (NUBINT) ACCESS_OK;
    return (sign_extend_8_to_integer(holder));
  }
}


void write_8b_to_process_memory 
  (NUB nub, 
   TARGET_ADDRESS address,
   NUBINT value, 
   NUBINT *error_code)
{
  BOOL         status;
  DWORD        number_bytes_transferred;
  LPDBGPROCESS process = (LPDBGPROCESS) nub;
  INT8         holder = sign_reduce_integer_to_8(value);

  status 
    = ValidatedWriteProcessMemory (process->ProcessHandle,
                                  (LPVOID) address,
                                  (LPVOID) &holder,
                                  sizeof(INT8),
                                  &number_bytes_transferred);

  if (!status || (number_bytes_transferred != sizeof(INT8))) {
    (*error_code) = (NUBINT) ACCESS_VIOLATION_ERROR;
  }
  else {
    (*error_code) = ACCESS_OK;
  }
}


NUBINT read_16b_from_process_memory 
  (NUB nub, 
   TARGET_ADDRESS address,
   NUBINT *error_code)
{
  BOOL         status;
  DWORD        number_bytes_transferred;
  INT16        holder;
  LPDBGPROCESS process = (LPDBGPROCESS) nub;
 
  status 
    = ReadProcessMemory(process->ProcessHandle,
                        (LPCVOID) address,
                        (LPVOID)  &holder,
                        sizeof(INT16),
                        &number_bytes_transferred);

  if (!status || (number_bytes_transferred != sizeof(INT16))) {
    (*error_code) = (NUBINT) ACCESS_VIOLATION_ERROR;
    return ((NUBINT) 0);
  }
  else {
    (*error_code) = (NUBINT) ACCESS_OK;
    return (sign_extend_16_to_integer(holder));
  }
}


void write_16b_to_process_memory 
  (NUB nub, 
   TARGET_ADDRESS address,
   NUBINT value, 
   NUBINT *error_code)
{
  BOOL         status;
  DWORD        number_bytes_transferred;
  LPDBGPROCESS process = (LPDBGPROCESS) nub;
  INT16        holder = sign_reduce_integer_to_16(value);

  status 
    = ValidatedWriteProcessMemory (process->ProcessHandle,
                                   (LPVOID) address,
                                   (LPVOID) &holder,
                                   sizeof(INT16),
                                   &number_bytes_transferred);

  if (!status || (number_bytes_transferred != sizeof(INT16))) {
    (*error_code) = (NUBINT) ACCESS_VIOLATION_ERROR;
  }
  else {
    (*error_code) = ACCESS_OK;
  }
}


NUBINT read_32b_from_process_memory 
  (NUB nub, 
   TARGET_ADDRESS address,
   NUBINT *error_code)
{
  BOOL         status;
  DWORD        number_bytes_transferred;
  INT32        holder;
  LPDBGPROCESS process = (LPDBGPROCESS) nub;
 
  status 
    = ReadProcessMemory(process->ProcessHandle,
                        (LPCVOID) address,
                        (LPVOID)  &holder,
                        sizeof(INT32),
                        &number_bytes_transferred);

  if (!status || (number_bytes_transferred != sizeof(INT32))) {
    (*error_code) = (NUBINT) ACCESS_VIOLATION_ERROR;
    return ((NUBINT) 0);
  }
  else {
    (*error_code) = (NUBINT) ACCESS_OK;
    return (sign_extend_32_to_integer(holder));
  }
}


void write_32b_to_process_memory 
  (NUB nub, 
   TARGET_ADDRESS address,
   NUBINT value, 
   NUBINT *error_code)
{
  BOOL         status;
  DWORD        number_bytes_transferred;
  LPDBGPROCESS process = (LPDBGPROCESS) nub;
  INT32        holder = sign_reduce_integer_to_32(value);

  status 
    = ValidatedWriteProcessMemory (process->ProcessHandle,
                                   (LPVOID) address,
                                   (LPVOID) &holder,
                                   sizeof(INT32),
                                   &number_bytes_transferred);

  if (!status || (number_bytes_transferred != sizeof(INT32))) {
    (*error_code) = (NUBINT) ACCESS_VIOLATION_ERROR;
  }
  else {
    (*error_code) = ACCESS_OK;
  }
}


NUBINT read_64b_from_process_memory 
  (NUB nub, 
   TARGET_ADDRESS address,
   NUBINT *error_code)
{
  BOOL         status;
  DWORD        number_bytes_transferred;
  INT64        holder;
  LPDBGPROCESS process = (LPDBGPROCESS) nub;
 
  status 
    = ReadProcessMemory(process->ProcessHandle,
                        (LPCVOID) address,
                        (LPVOID)  &holder,
                        sizeof(INT64),
                        &number_bytes_transferred);

  if (!status || (number_bytes_transferred != sizeof(INT64))) {
    (*error_code) = (NUBINT) ACCESS_VIOLATION_ERROR;
    return ((NUBINT) 0);
  }
  else {
    (*error_code) = (NUBINT) ACCESS_OK;
    return (sign_extend_64_to_integer(holder));
  }
}


void write_64b_to_process_memory 
  (NUB nub, 
   TARGET_ADDRESS address,
   NUBINT value, 
   NUBINT *error_code)
{
  BOOL         status;
  DWORD        number_bytes_transferred;
  LPDBGPROCESS process = (LPDBGPROCESS) nub;
  INT64        holder = sign_reduce_integer_to_64(value);

  status 
    = ValidatedWriteProcessMemory (process->ProcessHandle,
                                  (LPVOID) address,
                                  (LPVOID) &holder,
                                  sizeof(INT64),
                                  &number_bytes_transferred);

  if (!status || (number_bytes_transferred != sizeof(INT64))) {
    (*error_code) = (NUBINT) ACCESS_VIOLATION_ERROR;
  }
  else {
    (*error_code) = ACCESS_OK;
  }
}


FLOAT read_single_float_from_process_memory 
  (NUB nub, 
   TARGET_ADDRESS address,
   NUBINT *error_code)
{
  BOOL         status;
  DWORD        number_bytes_transferred;
  FLOAT        holder;
  LPDBGPROCESS process = (LPDBGPROCESS) nub;
 
  status 
    = ReadProcessMemory(process->ProcessHandle,
                        (LPCVOID) address,
                        (LPVOID)  &holder,
                        sizeof(FLOAT),
                        &number_bytes_transferred);

  if (!status || (number_bytes_transferred != sizeof(FLOAT))) {
    (*error_code) = (NUBINT) ACCESS_VIOLATION_ERROR;
    return ((FLOAT) 0);
  }
  else {
    (*error_code) = ACCESS_OK;
    return (holder);
  }
}


void write_single_float_to_process_memory 
  (NUB nub, 
   TARGET_ADDRESS address,
   FLOAT value, 
   NUBINT *error_code)
{
  BOOL         status;
  DWORD        number_bytes_transferred;
  LPDBGPROCESS process = (LPDBGPROCESS) nub;

  status 
    = ValidatedWriteProcessMemory (process->ProcessHandle,
                                  (LPVOID) address,
                                  (LPVOID) &value,
                                  sizeof(FLOAT),
                                  &number_bytes_transferred);

  if (!status || (number_bytes_transferred != sizeof(FLOAT))) {
    (*error_code) = (NUBINT) ACCESS_VIOLATION_ERROR;
  }
  else {
    (*error_code) = ACCESS_OK;
  }
}


DOUBLE read_double_float_from_process_memory 
  (NUB nub, 
   TARGET_ADDRESS address,
   NUBINT *error_code)
{
  BOOL         status;
  DWORD        number_bytes_transferred;
  DOUBLE       holder;
  LPDBGPROCESS process = (LPDBGPROCESS) nub;
 
  status 
    = ReadProcessMemory(process->ProcessHandle,
                        (LPCVOID) address,
                        (LPVOID)  &holder,
                        sizeof(DOUBLE),
                        &number_bytes_transferred);

  if (!status || (number_bytes_transferred != sizeof(DOUBLE))) {
    (*error_code) = (NUBINT) ACCESS_VIOLATION_ERROR;
    return ((DOUBLE) 0);
  }
  else {
    (*error_code) = ACCESS_OK;
    return (holder);
  }
}


void write_double_float_to_process_memory 
  (NUB nub, 
   TARGET_ADDRESS address,
   DOUBLE value, 
   NUBINT *error_code)
{
  BOOL         status;
  DWORD        number_bytes_transferred;
  LPDBGPROCESS process = (LPDBGPROCESS) nub;

  status 
    = ValidatedWriteProcessMemory (process->ProcessHandle,
                                  (LPVOID) address,
                                  (LPVOID) &value,
                                  sizeof(DOUBLE),
                                  &number_bytes_transferred);

  if (!status || (number_bytes_transferred != sizeof(DOUBLE))) {
    (*error_code) = (NUBINT) ACCESS_VIOLATION_ERROR;
  }
  else {
    (*error_code) = ACCESS_OK;
  }
}


void read_byte_string_from_process_memory 
  (NUB nub, 
   TARGET_ADDRESS address,
   NUBINT size,
   char *buf,
   NUBINT *error_code)
{
  BOOL         status;
  DWORD        number_bytes_transferred;
  DWORD        read_size = sizeof(char) * ((DWORD) size);
  LPDBGPROCESS process = (LPDBGPROCESS) nub;
 
  status 
    = ReadProcessMemory(process->ProcessHandle,
                        (LPCVOID) address,
                        (LPVOID)  buf,
                        read_size,
                        &number_bytes_transferred);

  if (!status || (number_bytes_transferred != read_size)) {
    (*error_code) = (NUBINT) ACCESS_VIOLATION_ERROR;
  }
  else {
    (*error_code) = ACCESS_OK;
  }
}


void write_byte_string_to_process_memory 
  (NUB nub, 
   TARGET_ADDRESS address,
   NUBINT size,
   char *buf, 
   NUBINT *error_code)
{
  BOOL         status;
  DWORD        number_bytes_transferred;
  DWORD        write_size = sizeof(char) * ((DWORD) size);
  LPDBGPROCESS process = (LPDBGPROCESS) nub;

  status 
    = ValidatedWriteProcessMemory (process->ProcessHandle,
                                  (LPVOID) address,
                                  (LPVOID) buf,
                                  write_size,
                                  &number_bytes_transferred);

  if (!status || (number_bytes_transferred != write_size)) {
    (*error_code) = (NUBINT) ACCESS_VIOLATION_ERROR;
  }
  else {
    (*error_code) = ACCESS_OK;
  }
}


void read_unicode_string_from_process_memory 
  (NUB nub, 
   TARGET_ADDRESS address,
   NUBINT size,
   UNICODE *buf,
   NUBINT *error_code)
{
  BOOL         status;
  DWORD        number_bytes_transferred;
  DWORD        read_size = sizeof(UNICODE) * ((DWORD) size);
  LPDBGPROCESS process = (LPDBGPROCESS) nub;
 
  status 
    = ReadProcessMemory(process->ProcessHandle,
                        (LPCVOID) address,
                        (LPVOID)  buf,
                        read_size,
                        &number_bytes_transferred);

  if (!status || (number_bytes_transferred != read_size)) {
    (*error_code) = (NUBINT) ACCESS_VIOLATION_ERROR;
  }
  else {
    (*error_code) = ACCESS_OK;
  }
}


void write_unicode_string_to_process_memory 
  (NUB nub, 
   TARGET_ADDRESS address,
   NUBINT size,
   UNICODE *buf, 
   NUBINT *error_code)
{
  BOOL         status;
  DWORD        number_bytes_transferred;
  DWORD        write_size = sizeof(UNICODE) * ((DWORD) size);
  LPDBGPROCESS process = (LPDBGPROCESS) nub;

  status 
    = ValidatedWriteProcessMemory (process->ProcessHandle,
                                  (LPVOID) address,
                                  (LPVOID) buf,
                                  write_size,
                                  &number_bytes_transferred);

  if (!status || (number_bytes_transferred != write_size)) {
    (*error_code) = (NUBINT) ACCESS_VIOLATION_ERROR;
  }
  else {
    (*error_code) = ACCESS_OK;
  }
}


TARGET_ADDRESS nub_calculate_stack_address
  (NUB nub, NUBTHREAD nubthread, NUBINT offset)
{
  LPDBGPROCESS            process = (LPDBGPROCESS) nub;
  LPDBGTHREAD             thread = (LPDBGTHREAD) nubthread;
  DWORD                   dw_offset = (DWORD) offset;
  DWORD                   result = 0;
  CONTEXT                 context;
  BOOL                    status;

  context.ContextFlags = CONTEXT_FULL;
  status = GetThreadContext(thread->ThreadHandle, &context);
  result = (context.Esp + (sizeof(TARGET_ADDRESS) * dw_offset));
  return ((TARGET_ADDRESS) result);
}

