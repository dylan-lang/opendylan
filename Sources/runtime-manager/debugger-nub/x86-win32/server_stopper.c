/* *********************************************************************** */
/* ** server_stopper.c                                                  ** */
/* ** Simple stub program to connect and close down the debug server.   ** */
/* ** ----------------------------------------------------------------- ** */
/* ** Author: Paul Howard                                               ** */
/* ** Copyright: (c) 1998 Functional Objects, Inc.                      ** */
/* **             All Rights Reserved.                                  ** */
/* *********************************************************************** */

#include <windows.h>
#include "transport_protocols.h"
#include "nubserve.h"

/* Allocation routines required by the RPC Runtime Library. */

void __RPC_FAR * __RPC_USER MIDL_user_allocate(size_t x)
{
  return ((void __RPC_FAR*) malloc (x));
}

void __RPC_USER MIDL_user_free(void __RPC_FAR * x)
{
  free(x);
}

int WINAPI WinMain (HINSTANCE hInstance,
                    HINSTANCE hPrevInstance,
                    LPSTR lpCommandLine,
                    int nCommandShow)
{
  RPC_STATUS          status;
  BOOL                overall_success = TRUE;
  char                *local_string_binding;
  RPC_IF_HANDLE       local_server_binding;
  DBG_TRANSPORT_INDEX protocol = DBG_TRANSPORT_DEFAULT;

  /* Call the RPC library to generate a string encoding of the
     interface binding. Note that NULL is supplied as the network
     address, since we are only interested in establishing a connection
     to servers running on our own machine. */

  status = RpcStringBindingCompose
             (CONNECTION_SERVER_UUID,
              dbg_transport_protocols[protocol].ProtSeqEncoding,
              NULL,
              NULL,
              NULL,
              &local_string_binding);

  /* Deal with failure */

  if (status != RPC_S_OK) {
    debugger_nub_rpc_error
      ("Main (stop-dbg-server)",
       "The generated string binding was not legal.");
    overall_success = FALSE;
  }

  /* Call the RPC library to convert the string representation into
     an actual interface binding. */

  status = RpcBindingFromStringBinding(local_string_binding,
                                       &local_server_binding);

  /* Again, deal with failure. */

  if (status != RPC_S_OK) {
    debugger_nub_rpc_error
        ("Main (stop-dbg-server)",
         "Failed to convert string binding to Rpc binding.");
    overall_success = FALSE;
  }

  /* With exception handling in place, call the server_shutdown
     interface. */

  RpcTryExcept {
    svr_server_shutdown(local_server_binding);
  }
  RpcExcept(1) {
    debugger_nub_rpc_error
        ("Main (stop-dbg-server)",
         "Failed to make the call to svr_server_shutdown.");
    overall_success = FALSE;
  } 
  RpcEndExcept;

  /* Ping the user! */

  if (overall_success) {
    return(
      MessageBox(NULL,
                 "The debug server program has been closed down.",
                 "Functional Developer Remote Debugger",
                 MB_OK | MB_ICONINFORMATION | MB_SETFOREGROUND));
  }
  else {
    return(
      MessageBox(NULL,
                 "The debug server program cannot be closed down.",
                 "Functional Developer Remote Debugger",
                 MB_OK | MB_ICONWARNING | MB_SETFOREGROUND));
  }
}
