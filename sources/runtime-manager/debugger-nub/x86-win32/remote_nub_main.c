/* *********************************************************************** */
/* ** remote_nub_main.c                                                 ** */
/* ** The main processing loop for the remote standalone debugger nub.  ** */
/* ** ----------------------------------------------------------------- ** */
/* ** Author: Paul Howard                                               ** */
/* ** Copyright: (c) 1998 Functional Objects, Inc.                      ** */
/* **            All Rights Reserved.                                   ** */
/* *********************************************************************** */

#include "nub-core.h"
#include "transport_protocols.h"
#include "rnub.h"

/* Global resources and synchronization objects */

LPDBGPROCESS          global_process;
PROCESSD              global_attachment_descriptor;
HANDLE                global_nub_available;
HANDLE                global_call_requested;
HANDLE                global_results_available;
BOOL                  global_still_listening;
NUB_ARGUMENT_BUFFER   global_argument_buffer;

/* Allocation routines required by the RPC Runtime Library. */

void __RPC_FAR * __RPC_USER MIDL_user_allocate(size_t x)
{
  return ((void __RPC_FAR*) malloc (x));
}

void __RPC_USER MIDL_user_free(void __RPC_FAR * x)
{
  free(x);
}


NUB create_and_debug_process 
  (SERVER dummy, 
   char *module_name, 
   char *arguments,
   NUBINT path_count, char **paths,
   NUBINT lib_path_count, char **lib_paths,
   char *workdir,
   NUBINT create_shell);

NUB debug_active_process
  (SERVER dummy,
   NUBPROCESS nubprocess,
   NUBINT symbol_path_count,
   char **symbol_paths,
   char *system_JIT_information);

/* The operator thread */

DWORD run_operator_thread (DWORD dummy)
{
  debugger_nub_rpc_diagnostic("run_operator_thread:", "Started");
  ReleaseSemaphore(global_nub_available, 1, NULL);
  while (global_still_listening) {
    WaitForSingleObject(global_call_requested, INFINITE);
    switch (global_argument_buffer.FunctionCode) {
 
   case NUB_CREATE_AND_DEBUG_PROCESS:
      debugger_nub_rpc_diagnostic
        ("Operator: ", "NUB_CREATE_AND_DEBUG_PROCESS");
      global_argument_buffer.u.NubCreateAndDebugProcess.Result =
        create_and_debug_process(
          global_argument_buffer.u.NubCreateAndDebugProcess.Server,
          global_argument_buffer.u.NubCreateAndDebugProcess.Module,
          global_argument_buffer.u.NubCreateAndDebugProcess.Arguments,
          global_argument_buffer.u.NubCreateAndDebugProcess.PathCount,
          global_argument_buffer.u.NubCreateAndDebugProcess.Paths,
          global_argument_buffer.u.NubCreateAndDebugProcess.LibCount,
          global_argument_buffer.u.NubCreateAndDebugProcess.Libs,
          global_argument_buffer.u.NubCreateAndDebugProcess.WorkingDirectory,
          global_argument_buffer.u.NubCreateAndDebugProcess.CreateShell);
      global_process = 
        (LPDBGPROCESS) 
           global_argument_buffer.u.NubCreateAndDebugProcess.Result;
      break;

   case NUB_DEBUG_ACTIVE_PROCESS:
      debugger_nub_rpc_diagnostic
        ("Operator: ", "NUB_DEBUG_ACTIVE_PROCESS");
      global_argument_buffer.u.NubDebugActiveProcess.Result =
        debug_active_process(
          global_argument_buffer.u.NubDebugActiveProcess.Server,
          global_argument_buffer.u.NubDebugActiveProcess.Process,
          global_argument_buffer.u.NubDebugActiveProcess.PathCount,
          global_argument_buffer.u.NubDebugActiveProcess.Paths,
          global_argument_buffer.u.NubDebugActiveProcess.JITInfo);
      global_process = 
        (LPDBGPROCESS) 
           global_argument_buffer.u.NubDebugActiveProcess.Result;
      break;

    case NUB_SETUP_FUNCTION_CALL:
      global_argument_buffer.u.NubSetupFunctionCall.Result =
        nub_setup_function_call(
          global_argument_buffer.u.NubSetupFunctionCall.Nub,
          global_argument_buffer.u.NubSetupFunctionCall.Nubthread,
          global_argument_buffer.u.NubSetupFunctionCall.Function,
          global_argument_buffer.u.NubSetupFunctionCall.ArgCount,
          global_argument_buffer.u.NubSetupFunctionCall.Args,
          global_argument_buffer.u.NubSetupFunctionCall.ContextHandle);
      break;

    case NUB_REMOTE_CALL_SPY:
      global_argument_buffer.u.NubRemoteCallSpy.Result =
        nub_remote_call_spy(
          global_argument_buffer.u.NubRemoteCallSpy.Nub,
          global_argument_buffer.u.NubRemoteCallSpy.Nubthread,
          global_argument_buffer.u.NubRemoteCallSpy.Function,
          global_argument_buffer.u.NubRemoteCallSpy.ArgCount,
          global_argument_buffer.u.NubRemoteCallSpy.Args,
          global_argument_buffer.u.NubRemoteCallSpy.Status);
      break;

    case NUB_APPLICATION_RESTART:
      nub_application_restart
        (global_argument_buffer.u.NubApplicationRestart.Nub);
      break;

    case NUB_APPLICATION_STOP:
      nub_application_stop
        (global_argument_buffer.u.NubApplicationStop.Nub);
      break;

    case NUB_APPLICATION_CONTINUE:
      nub_application_continue
        (global_argument_buffer.u.NubApplicationContinue.Nub);
      break;

    case NUB_APPLICATION_CONTINUE_UNHANDLED:
      nub_application_continue_unhandled
        (global_argument_buffer.u.NubApplicationContinueUnhandled.Nub);
      break;

    case NUB_WAIT_FOR_STOP_REASON_WITH_TIMEOUT:
      nub_wait_for_stop_reason_with_timeout
        (global_argument_buffer.u.NubWaitForStopReasonWithTimeout.Nub,
         global_argument_buffer.u.NubWaitForStopReasonWithTimeout.Timeout,
         global_argument_buffer.u.NubWaitForStopReasonWithTimeout.Code);
      break;

    case NUB_WAIT_FOR_STOP_REASON_NO_TIMEOUT:
      nub_wait_for_stop_reason_no_timeout
        (global_argument_buffer.u.NubWaitForStopReasonNoTimeout.Nub,
         global_argument_buffer.u.NubWaitForStopReasonNoTimeout.Code);
      break;

    case NUB_PROFILE_WAIT_FOR_STOP_REASON_WITH_TIMEOUT:
      nub_profile_wait_for_stop_reason_with_timeout
        (global_argument_buffer.u.NubProfileWaitForStopReasonWithTimeout.Nub,
         global_argument_buffer.u.NubProfileWaitForStopReasonWithTimeout.
           Timeout,
         global_argument_buffer.u.NubProfileWaitForStopReasonWithTimeout.
           ProfileInterval,
         global_argument_buffer.u.NubProfileWaitForStopReasonWithTimeout.
           Code);
      break;

    case NUB_PROFILE_WAIT_FOR_STOP_REASON_NO_TIMEOUT:
      nub_profile_wait_for_stop_reason_no_timeout
        (global_argument_buffer.u.NubProfileWaitForStopReasonNoTimeout.Nub,
         global_argument_buffer.u.NubProfileWaitForStopReasonNoTimeout.
           ProfileInterval,
         global_argument_buffer.u.NubProfileWaitForStopReasonNoTimeout.Code);
      break;

    case NUB_KILL_APPLICATION:
      global_argument_buffer.u.NubKillApplication.Result
        = nub_kill_application
            (global_argument_buffer.u.NubKillApplication.Nub);
      break;

    case NUB_CLOSE_APPLICATION:
      nub_close_application
        (global_argument_buffer.u.NubCloseApplication.Nub);
      break;

    default:
      debugger_nub_rpc_error
        ("run_operator_thread",
         "Received an illegal function code. Ignoring.");
      break;
    }
    ReleaseSemaphore(global_results_available, 1, NULL);
  }

  return(0);
}

void main (int argc, char **argv)
{
  DBG_TRANSPORT_INDEX  protocol = DBG_TRANSPORT_DEFAULT;
  RPC_STATUS           status;
  HANDLE               operator_thread_handle;
  DWORD                operator_thread_id;
  RPC_BINDING_VECTOR  *binding_vector;
  UUID_VECTOR         uuid_vector;  /* Statically allocated, since it has
                                       room for one pointer, and we only
                                       want to register one UUID */
  UUID                our_uuid;

  /* Initialize the UUID vector. */

  uuid_vector.Count = 1;
  status =
    UuidFromString
      (DEBUGGER_NUB_UUID,
       &our_uuid);
  uuid_vector.Uuid[0] = &our_uuid;

  /* Deal with failure */

  if (status != RPC_S_OK) {
    debugger_nub_rpc_error
      ("main (Nub)", "Conversion of server UUID to binary form failed!");
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
      ("main (Nub)", "RPC Runtime did not accept endpoint protocol");
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
      (rnub_v1_0_s_ifspec,
       binding_vector,
       &uuid_vector,
       "Remote Debugger Nub");

  /* Deal with failure */

  if (status != RPC_S_OK) {
    debugger_nub_rpc_error
      ("main (Nub)", "Call to RpcEpRegister failed.");
    ExitProcess(1);
  }

  /* Free the vector */

  RpcBindingVectorFree(&binding_vector);

  /* Now register the interface. */

  status =
    RpcServerRegisterIf(rnub_v1_0_s_ifspec, NULL, NULL);

  /* Deal with failure. */

  if (status != RPC_S_OK) {
    debugger_nub_rpc_error
      ("main (Nub)", "RPC Runtime did not register the interface");
    ExitProcess(1);
  }

  /* Create the global synchronization objects */

  global_nub_available = CreateSemaphore(NULL, 0, 100, NULL);
  global_results_available = CreateSemaphore(NULL, 0, 100, NULL);
  global_call_requested = CreateSemaphore(NULL, 0, 100, NULL);
  global_still_listening = TRUE;

  debugger_nub_rpc_diagnostic("main (Nub):", "Created synch objects.");

  /* Spawn the operator thread.
     A few interfaces in the debugger nub are required to run on
     the same thread, but the RPC runtime generates a new thread
     for every RPC call. These two implementation details conflict
     with each other, and we have to resolve the conflict manually.
     The operator thread is used to run the sensitive functions. */

  operator_thread_handle =
    CreateThread(NULL,     /* Use default security attributes */
                 0,        /* Use the default stack size */
                 (LPTHREAD_START_ROUTINE) run_operator_thread,
                 NULL,
                 0,
                 &operator_thread_id);

  /* Deal with failure. This instance of the debugger nub is useless
     if the operator thread cannot be spawned. */

  if (operator_thread_handle == NULL) {
    debugger_nub_rpc_error
      ("main (Nub)", "Failed to spawn the operator thread");
    ExitProcess(1);
  }

  debugger_nub_rpc_diagnostic("main (Nub):", "Created operator thread.");

  /* And our life is spent listening for connections. */

  status =
    RpcServerListen(1, 
                    NUB_MAX_CONCURRENT_REQUESTS, 
                    0 /* Wait */);

  if (status != RPC_S_OK) {
    debugger_nub_rpc_error
      ("main (Nub)", "Nub interface could not listen for connections");
    ExitProcess(1);
  }
  else {
    /* When another thread shuts us down, the listener function
       should return, and we can quit. Close global handles and
       exit with a success code. */

    RpcServerUnregisterIf(NULL, NULL, FALSE);
    RpcEpUnregister(rnub_v1_0_s_ifspec, binding_vector, &uuid_vector);

    CloseHandle(operator_thread_handle);
    CloseHandle(global_call_requested);
    CloseHandle(global_results_available);
    CloseHandle(global_nub_available);
    ExitProcess(0);
  }
}
