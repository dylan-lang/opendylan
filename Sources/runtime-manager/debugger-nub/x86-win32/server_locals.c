/* *********************************************************************** */
/* ** server_locals.c                                                   ** */
/* ** Local versions of server APIs.                                    ** */
/* ** These are linked into the local debugger nub, as well as the      ** */
/* ** standalone connection server.                                     ** */
/* ** ----------------------------------------------------------------- ** */
/* ** Author: Paul Howard                                               ** */
/* ** Copyright: (c) 1998 Functional Objects, Inc.                      ** */
/* **            All Rights Reserved.                                   ** */
/* *********************************************************************** */

#include "nub-core.h"

PROCESSD  *canonicalize_process_list (PROCESSD*, DWORD*);

PROCESSD  *canonical_process_list = NULL;

NUBINT password_size = 0;
char   password[1024];

NUBINT verify_local_password (NUBINT buf_size, char *buf)
{
  NUBINT i;
  if (buf_size == password_size) {
    for (i = 0; i < password_size; i++) {
      if (password[i] != buf[i])
        return(0);
    }
    return(1);
  }
  else {
    return(0);
  }
}

NUBINT get_local_hostname_length ()
{
  char        dummy_buffer[MAX_COMPUTERNAME_LENGTH + 1];
  DWORD       sz = MAX_COMPUTERNAME_LENGTH + 1;

  GetComputerName((LPSTR) dummy_buffer, &sz);
  return((NUBINT) sz);
}

void get_local_hostname (NUBINT buf_size, char *buffer)
{
  DWORD      sz = MAX_COMPUTERNAME_LENGTH + 1;
  GetComputerName((LPSTR) buffer, &sz);
}

NUBINT update_local_process_list ()
{
  DWORD     count;
  canonical_process_list =
    canonicalize_process_list(canonical_process_list, &count);
  return((NUBINT) count);
}

PROCESSD *process_descriptor_from_index (NUB_INDEX i)
{
  PROCESSD   *this_one = canonical_process_list;
  NUB_INDEX   j = 0;
  while (j < i) {
    this_one = this_one->Next;
    j++;
  }
  return(this_one);
}

NUBINT local_process_name_length
  (NUB_INDEX proc_index)
{
  PROCESSD   *procd = process_descriptor_from_index(proc_index);
  return((NUBINT) procd->ProcessNameLength);
}

void local_process_name
  (NUB_INDEX proc_index, NUBINT buf_size, char *buf)
{
  PROCESSD   *procd = process_descriptor_from_index(proc_index);
  NUBINT     i = 0;
  while (procd->ProcessName[i] != '\0') {
    buf[i] = procd->ProcessName[i];
    i++;
  }
  if (i < buf_size)
    buf[i] = '\0';
}


NUBINT local_process_system_identifier_length
  (NUB_INDEX proc_index)
{
  PROCESSD   *procd = process_descriptor_from_index(proc_index);
  return((NUBINT) procd->ProcessIDLength);
}

void local_process_system_identifier
  (NUB_INDEX proc_index, NUBINT buf_size, char *buf)
{
  PROCESSD   *procd = process_descriptor_from_index(proc_index);
  NUBINT     i = 0;
  while (procd->ProcessID[i] != '\0') {
    buf[i] = procd->ProcessID[i];
    i++;
  }
  if (i < buf_size)
    buf[i] = '\0';
}

unsigned long local_process_actual_identifier
  (NUB_INDEX proc_index)
{
  PROCESSD   *procd = process_descriptor_from_index(proc_index);
  return((unsigned long) (procd->ActualProcessID));
}

NUBPROCESS local_process_nub_descriptor
   (NUB_INDEX proc_index)
{
  PROCESSD   *procd = process_descriptor_from_index(proc_index);
  return((NUBPROCESS) procd);
}
