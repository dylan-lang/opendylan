/* ********************************************************************** */
/* ** transport_protocols.c                                            ** */
/* ** Descriptors for network transport (RPC) protocols                ** */
/* ** ---------------------------------------------------------------- ** */
/* ** Author: Paul Howard                                              ** */
/* ** Copyright: (c) 1998 Functional Objects, Inc.                     ** */
/* **            All Rights Reserved.                                  ** */
/* ********************************************************************** */

#include "transport_protocols.h"

DBG_TRANSPORT_PROTOCOL_DESCRIPTOR
  dbg_transport_protocols[DBG_TRANSPORT_NUM_PROTOCOLS] =
     {{"ncacn_nb_nb", "NetBEUI", "101"},
      {"ncacn_ip_tcp", "TCP/IP", "1025"},
      {"ncacn_np", "Named pipe", SERVER_PIPE_NAME}};

void transport_get_enumerations
   (DBG_TRANSPORT_INDEX *first, DBG_TRANSPORT_INDEX *last)
{
  (*first) = 0;
  (*last) = DBG_TRANSPORT_NUM_PROTOCOLS - 1;
}

int transport_get_description_length
    (DBG_TRANSPORT_INDEX index)
{
  int   i = 0;
  char *descr = dbg_transport_protocols[index].Description;
  while (descr[i] != '\0') i++;
  return(i);
}

void transport_get_description
    (DBG_TRANSPORT_INDEX index, int buf_size, char *buf)
{
  int    i = 0;
  char  *d = dbg_transport_protocols[index].Description;
  while (d[i] != '\0') {buf[i] = d[i]; i++;}
  if (i < buf_size) buf[i] = '\0';
}

void generate_debugger_nub_endpoint_name
    (DBG_TRANSPORT_INDEX protocol, NUB_ID nub_id, char *buffer)
{
  switch(protocol) {
  case DBG_TRANSPORT_NCACN_NB_NB:
    debugger_nub_integer_to_string(NULL, 102 + (int) nub_id, buffer);
    break;
  case DBG_TRANSPORT_NCACN_IP_TCP:
    debugger_nub_integer_to_string(NULL, 1301 + (int) nub_id, buffer);
    break;
  case DBG_TRANSPORT_NCACN_NP:
    debugger_nub_integer_to_string(NUB_PIPE_NAME, (int) nub_id, buffer);
    break;
  default:
    buffer[0] = 'B'; buffer[1] = 'A'; buffer[2] = 'D'; buffer[3] = '\0';
    debugger_nub_rpc_error
      ("generate_debugger_nub_endpoint_name", "Illegal protocol.");
    break;
  }
}

int debugger_nub_string_to_integer(char *buf)
{
  int result = 0;
  int i = 0;
  while (buf[i] != '\0') {
    result = (10 * result) + ((int) buf[i] - (int) '0');
    i++;
  }
  return(result);
}

void debugger_nub_integer_to_string(char *prefix, int x, char *buf)
{
  int    i        = 0;
  int    k        = 0;
  int    r        = 0;
  BOOL   finished = FALSE;
  char   spare[256]; // Assume this is big enough for the conversion.
  char  *digits = "0123456789";

  while (!finished) {
    r = x % 10; x = x / 10;
    spare[k] = digits[r];
    k++;
    if (x <= 0) finished = TRUE;
  }
  spare[k] = '\0';
  if (prefix != NULL) {
    int j = 0;
    while (prefix[j] != '\0') {
      buf[i] = prefix[j];
      i++;
      j++;
    }
  }
  for (k = k - 1; k >= 0; k--, i++) buf[i] = spare[k];
  buf[i] = '\0';
}

void construct_debugger_nub_command_line
  (char *protocol, char *endpoint, char *buffer)
{
  int   i = 0;
  int   j = 0;
  char  *command = "rnub";
  while (command[j] != '\0') {buffer[i] = command[j]; i++; j++;}
  buffer[i] = ' '; i++; j = 0;
  while (protocol[j] != '\0') {buffer[i] = protocol[j]; i++; j++;}
  buffer[i] = ' '; i++; j = 0;
  while (endpoint[j] != '\0') {buffer[i] = endpoint[j]; i++; j++;}
  buffer[i] = '\0';
}

void debugger_nub_rpc_error
  (char *function_name, char *reason)
{
  OutputDebugString("**** REMOTE DEBUGGER INTERFACE FAILED\n");
  OutputDebugString(function_name);
  OutputDebugString(reason);
}

void debugger_nub_rpc_diagnostic
  (char *function_name, char *reason)
{
  OutputDebugString("++ Remote debugger diagnostic message\n");
  OutputDebugString(function_name);
  OutputDebugString(reason);
}
