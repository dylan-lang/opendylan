// Original Code Copyright 1995-2004 Functional Objects, Inc.
// All rights reserved.
// License: See License.txt in this distribution for details.
// Distributed WITHOUT WARRANTY OF ANY KIND

typedef int            NUBINT;

#if defined OPEN_DYLAN_PLATFORM_WINDOWS

#include <windows.h>

NUBINT get_local_hostname_length (void)
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

#elif defined OPEN_DYLAN_PLATFORM_UNIX

#include <stddef.h>
#include <string.h>
#include <unistd.h>

NUBINT get_local_hostname_length (void)
{
  size_t host_name_max = (size_t) sysconf(_SC_HOST_NAME_MAX);
  char dummy_buffer[host_name_max + 1];
  if (gethostname(dummy_buffer, host_name_max + 1) == 0) {
    return strlen(dummy_buffer);
  }
  else {
    return 0;
  }
}

void get_local_hostname (NUBINT buf_size, char *buffer)
{
  gethostname(buffer, buf_size + 1);
}

#else
#error No local hostname support
#endif
