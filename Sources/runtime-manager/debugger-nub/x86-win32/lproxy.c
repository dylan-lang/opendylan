/* ************************************************************************ */
/* ** eproxy.c                                                           ** */
/* ** The API for the proxy debugger nub, linked to the environment      ** */
/* ** along with the local debugger nub. (Personal/Professional).        ** */
/* ** ------------------------------------------------------------------ ** */
/* ** Author: Paul Howard, Copyright: (c) 1996 Functional Objects, Inc.   ** */
/* **                                 All Rights Reserved                ** */
/* ************************************************************************ */

#include "nub-core.h"

/* NULL IMPLEMENTATIONS */

SERVER establish_connection_to_server 
   (NUBINT protocol, char *net_addr, NUBINT *success)
{
  (*success) = (NUBINT) DBG_TRANSPORT_NOT_SUPPORTED;
  return(NULL);
}

NUB open_remote_tether 
  (SERVER server, 
   NUBINT command_size, NUBINT arg_size,
   char *command, char *arguments,
   NUBINT sym_path_count, char **paths,
   NUBINT lib_path_count, char **lib_paths,
   NUBINT workdirsz,
   char *working_directory,
   NUBINT create_shell,
   NUBINT *success)
{
  (*success) = (NUBINT) DBG_TRANSPORT_NOT_SUPPORTED;
  return(NULL);
}

void remote_debugger_nub_shutdown
  (NUB nub)
{

}

/*
 *    ----------------------- SERVER PROXY FUNCTIONS -----------------------
 */

NUBINT server_get_hostname_length (SERVER server, NUBINT *success)
{
  (*success) = 1;
  return(0);
}

void server_get_hostname
  (SERVER server, NUBINT buf_size, char *buf)
{
  buf[0] = '\0';
}

NUBINT server_shutdown
  (SERVER server)
{
  return(DBG_TRANSPORT_NOT_SUPPORTED);
}
