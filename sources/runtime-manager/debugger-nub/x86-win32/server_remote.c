/* *********************************************************************** */
/* ** server_remote.c                                                   ** */
/* ** Remote versions of the server APIs.                               ** */
/* ** These are linked only into the standalone connection server, and  ** */
/* ** then defer to the local implementations.                          ** */
/* ** ----------------------------------------------------------------- ** */
/* ** Author: Paul Howard                                               ** */
/* ** Copyright: (c) 1998 Functional Objects, Inc.                      ** */
/* **             All Rights Reserved.                                  ** */
/* *********************************************************************** */

#include "nub-core.h"
#include "transport_protocols.h"
#include "nubserve.h"

NUBINT svr_server_get_hostname_length (handle_t s)
{
  return(get_local_hostname_length());
}

void svr_server_get_hostname (handle_t s, NUBINT buf_size, unsigned char *buffer)
{
  get_local_hostname(buf_size, buffer);
}

NUBINT svr_server_verify_password (handle_t s, NUBINT buf_size, unsigned char *buf)
{
  return(verify_local_password(buf_size - 1, buf));
}

void svr_server_shutdown (handle_t s)
{
   RpcMgmtStopServerListening(NULL);
}

NUBINT svr_update_server_process_list (handle_t s)
{
  return(update_local_process_list());
}

RNUBPROCESS svr_server_process_nub_descriptor (handle_t s, NUB_INDEX i)
{
  NUBPROCESS       local_process = local_process_nub_descriptor(i);
  return(REMOTIZE_NUBPROCESS( local_process ));
}

unsigned long svr_server_process_actual_id
  (handle_t s, RNUBPROCESS nubprocess)
{
  NUBPROCESS       local_nubprocess = LOCALIZE_RNUBPROCESS( nubprocess );
  PROCESSD        *procd = (PROCESSD*) local_nubprocess;
  return((unsigned long) (procd->ActualProcessID));
}

NUBINT svr_server_process_name_length (handle_t s, NUB_INDEX i)
{
  return(local_process_name_length(i));
}

void svr_server_process_name (handle_t s, NUB_INDEX i, NUBINT sz, unsigned char *b)
{
  local_process_name(i, sz, b);
}

NUBINT svr_server_process_system_identifier_length (handle_t s, NUB_INDEX i)
{
  return(local_process_system_identifier_length(i));
}

void svr_server_process_system_identifier 
   (handle_t s, NUB_INDEX i, NUBINT sz, unsigned char *b)
{
  local_process_system_identifier(i, sz, b);
}

NUB_ID    global_nub_counter = 0;

NUBINT open_new_debugger_nub 
   (handle_t s, DBG_TRANSPORT_INDEX protocol, 
    int buf_size, unsigned char *endpoint_name)
{
  char                  command_line[256];
  char                  proto_as_string[256];
  NUB_ID                nub_id = global_nub_counter;
  STARTUPINFO           startup_info;
  PROCESS_INFORMATION   received_info;
  BOOL                  creation_status;

  /* Generate the debugger nub's endpoint name, and increment the
     global counter so that the next one will be different. */
  generate_debugger_nub_endpoint_name(protocol, nub_id, endpoint_name);
  global_nub_counter++;
  
  /* Generate the debugger nub's command line. */
  debugger_nub_integer_to_string(NULL, (int) protocol, proto_as_string);
  construct_debugger_nub_command_line(proto_as_string, 
                                      endpoint_name,
                                      command_line);

  /* The debugger nub is to be a new process, so fill in all of
     its default information. */

  startup_info.cb                  = sizeof(STARTUPINFO);
  startup_info.lpReserved          = NULL;
  startup_info.lpDesktop           = NULL;
  startup_info.lpTitle             = NULL;
  startup_info.dwX                 = 0;
  startup_info.dwY                 = 0;
  startup_info.dwXSize             = 0;
  startup_info.dwYSize             = 0;
  startup_info.dwXCountChars       = 0;
  startup_info.dwYCountChars       = 0;
  startup_info.dwFillAttribute     = 0;
  startup_info.dwFlags             = STARTF_FORCEONFEEDBACK |
                                     STARTF_USESHOWWINDOW;

  /* The debugger nub is a console process, but we don't want it
     farting all over the screen, so we use SW_SHOWMINNOACTIVE so
     that its appearance causes no disturbances. */

  startup_info.wShowWindow         = SW_SHOWMINNOACTIVE;
  startup_info.cbReserved2         = 0;
  startup_info.lpReserved2         = NULL;

  /* Post a diagnostic. Just post the command line being used. */
  debugger_nub_rpc_diagnostic("open_new_debugger_nub",
                              command_line);

  /* Spawn the debugger nub, with its generated command line. */

  creation_status = CreateProcess(DEBUGGER_NUB_EXECUTABLE_NAME,
                                  command_line,
                                  NULL,
                                  NULL,
                                  FALSE,
                                  CREATE_NEW_CONSOLE,
                                  NULL,
                                  NULL,
                                  &startup_info,
                                  &received_info);

  /* We are contracted to return zero on failure. The caller will
     deal with this. */

  if (creation_status)
    return((NUBINT) 1);
  else
    return((NUBINT) 0);
}
