/* *********************************************************************** */
/* ** server_main.c                                                     ** */
/* ** The main execution loop for the standalone connection server.     ** */
/* ** ----------------------------------------------------------------- ** */
/* ** Author: Paul Howard                                               ** */
/* ** Copyright: (c) 1998 Functional Objects, Inc.                       ** */
/* **            All Rights Reserved                                    ** */
/* *********************************************************************** */

#include "nub-core.h"
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

extern NUBINT password_size;
extern char   password[];

void main (int argc, char **argv)
{
  DBG_TRANSPORT_INDEX protocol = DBG_TRANSPORT_DEFAULT;
  RPC_STATUS          status;
  RPC_BINDING_VECTOR  *binding_vector;
  UUID_VECTOR         uuid_vector;  /* Statically allocated, since it has
                                       room for one pointer, and we only
                                       want to register one UUID */
  UUID                our_uuid;

  /* The server-starter should have sent the password on the command-line.
     Therefore, the total number of command-line arguments, including the
     program name itself, should be 2. Verify this, and silently quit
     if this is an illegal invocation. */

  if (argc != 2) {
    debugger_nub_rpc_error
       ("main (Server)", "No command-line argument");
    ExitProcess(1);
  }

  /* Copy the password for subsequent validations. */
  while (argv[1][password_size] != '\0') {
    password[password_size] = argv[1][password_size];
    password_size++;
  }
  password[password_size] = '\0'; // Not strictly necessary, but I like to
                                  // keep strings NUL terminated.

  /* Initialize the UUID vector. */

  uuid_vector.Count = 1;
  status =
    UuidFromString
      (CONNECTION_SERVER_UUID,
       &our_uuid);
  uuid_vector.Uuid[0] = &our_uuid;

  /* Deal with failure */

  if (status != RPC_S_OK) {
    debugger_nub_rpc_error
      ("main (Server)", "Conversion of server UUID to binary form failed!");
    ExitProcess(1);
  }

  /* Establish the server communication protocol, calling the
     RPC library. */

  status =
    RpcServerUseAllProtseqs
       (/* dbg_transport_protocols[protocol].ProtSeqEncoding, */
        RPC_C_PROTSEQ_MAX_REQS_DEFAULT,
        NULL);

  /* Deal with failure */

  if (status != RPC_S_OK) {
    debugger_nub_rpc_error
      ("main (Server)", "RPC Runtime did not accept endpoint protocol");
    ExitProcess(1);
  }

  /* We are going to use dynamic endpoint-allocation, since there are
     no endpoints that are known to be available at any one time. 
     We need a vector of the possible bindings that are available
     to us. */

  status = RpcServerInqBindings(&binding_vector);

  /* Deal with failure */

  if (status != RPC_S_OK) {
    debugger_nub_rpc_error
      ("main (Server)", "There are no available bindings.");
    ExitProcess(1);
  }

  /* Register the possible endpoints in the database. */

  status =
    RpcEpRegister
      (nubserve_v1_0_s_ifspec,
       binding_vector,
       &uuid_vector,
       "Debug connections server");

  /* Deal with failure */

  if (status != RPC_S_OK) {
    debugger_nub_rpc_error
      ("main (Server)", "Call to RpcEpRegister failed.");
    ExitProcess(1);
  }

  /* Free the vector */

  RpcBindingVectorFree(&binding_vector);

  /* Now register the interface. */

  status =
    RpcServerRegisterIf(nubserve_v1_0_s_ifspec, NULL, NULL);

  /* Deal with failure. */

  if (status != RPC_S_OK) {
    debugger_nub_rpc_error
      ("main (server)", "RPC Runtime did not register the interface");
    ExitProcess(1);
  }

  /* And our life is spent listening for connections. */

  status =
    RpcServerListen(1, 
                    SERVER_MAX_CONCURRENT_REQUESTS, 
                    0 /* Wait */);

  if (status != RPC_S_OK) {
    debugger_nub_rpc_error
      ("main (server)", "Server interface could not listen for connections");
    ExitProcess(1);
  }

  /* Someone has shut the server down, so it is time to go through the
     cleanup procedures.... */

  RpcServerUnregisterIf(NULL, NULL, FALSE);
  RpcEpUnregister(nubserve_v1_0_s_ifspec, binding_vector, &uuid_vector);
  ExitProcess(0);
}
