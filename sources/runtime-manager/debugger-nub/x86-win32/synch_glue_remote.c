/* ********************************************************************** */
/* ** synch_glue_remote.c                                              ** */
/* ** Wrapper functions and synchronization code for the remote        ** */
/* ** debugger nub. These dispatch to the local implementations.       ** */
/* ** ---------------------------------------------------------------- ** */
/* ** Author: Paul Howard                                              ** */
/* ** Copyright: (c) 1998 Functional Objects, Inc.                     ** */
/* **            All Rights Reserved.                                  ** */
/* ********************************************************************** */

#include "nub-core.h"
#include "transport_protocols.h"
#include "rnub.h"

/* Global resources and synchronization objects */

extern LPDBGPROCESS          global_process;
extern PROCESSD              global_attachment_descriptor;
extern HANDLE                global_nub_available;
extern HANDLE                global_call_requested;
extern HANDLE                global_results_available;
extern BOOL                  global_still_listening;
extern NUB_ARGUMENT_BUFFER   global_argument_buffer;

/* ********************************************************************** */
/* ** Starting the ball rolling!                                       ** */
/* ********************************************************************** */

char *dummy[] = {"One", "Two", "Three"};

RNUB rnub_create_and_debug_process
  (handle_t nub,
   NUBINT command_size, NUBINT arg_size,
   unsigned char *command, unsigned char *arguments,
   NUBINT path_count,
   NUBINT lib_count,
   NUBINT working_dir_size,
   unsigned char *working_dir,
   NUBINT create_shell)
{
  RNUB     result;

  debugger_nub_rpc_diagnostic
    ("rnub_create_and_debug_process: ", "Entry");
  WaitForSingleObject(global_nub_available, INFINITE);
  debugger_nub_rpc_diagnostic
    ("rnub_create_and_debug_process: ", "Got global_nub_available");
  global_argument_buffer.FunctionCode = NUB_CREATE_AND_DEBUG_PROCESS;
  global_argument_buffer.u.NubCreateAndDebugProcess.Server = (SERVER) 0;
  global_argument_buffer.u.NubCreateAndDebugProcess.Module
    = command;
  global_argument_buffer.u.NubCreateAndDebugProcess.Arguments 
    = arguments;
  global_argument_buffer.u.NubCreateAndDebugProcess.PathCount
    = 0;
  global_argument_buffer.u.NubCreateAndDebugProcess.Paths
    = dummy;
  global_argument_buffer.u.NubCreateAndDebugProcess.LibCount
    = 0;
  global_argument_buffer.u.NubCreateAndDebugProcess.Libs
    = dummy;
  global_argument_buffer.u.NubCreateAndDebugProcess.WorkingDirectory
    = working_dir;
  global_argument_buffer.u.NubCreateAndDebugProcess.CreateShell
    = create_shell;
  debugger_nub_rpc_diagnostic
    ("rnub_create_and_debug_process: ", "Filled arguments");
  ReleaseSemaphore(global_call_requested, 1, NULL);
  WaitForSingleObject(global_results_available, INFINITE);
  debugger_nub_rpc_diagnostic
    ("rnub_create_and_debug_process: ", "Results available");
  result =
    (REMOTIZE_NUB( global_argument_buffer.u.NubCreateAndDebugProcess.Result ));
  ReleaseSemaphore(global_nub_available, 1, NULL);
  debugger_nub_rpc_diagnostic
    ("rnub_create_and_debug_process: ", "Return");
  return(result);
}

RNUB rnub_debug_active_process
  (handle_t nub,
   NUBINT name_length, unsigned char *name_string,
   NUBINT id_length, unsigned char *id_string,
   unsigned long actual_id,
   NUBINT path_count,
   NUBINT jit_info_sz,
   unsigned char *jit_info)
{
  RNUB     result;
  NUBINT   i;

  debugger_nub_rpc_diagnostic
    ("rnub_debug_active_process: ", "Entry");

  /* Fill in the Global Attachment Descriptor. */
  /* We subtract one from string sizes since the client will deliberately
     have added one in order for the NUL terminator to be included for
     RPC transport */

  global_attachment_descriptor.ProcessNameLength = name_length - 1;
  global_attachment_descriptor.ProcessIDLength = id_length - 1;
  for (i = 0; i < name_length; i++)
    global_attachment_descriptor.ProcessName[i] = name_string[i];
  for (i = 0; i < id_length; i++)
    global_attachment_descriptor.ProcessID[i] = id_string[i];
  global_attachment_descriptor.ActualProcessID = (DWORD) actual_id;

  WaitForSingleObject(global_nub_available, INFINITE);
  debugger_nub_rpc_diagnostic
    ("rnub_debug_active_process: ", "Got global_nub_available");
  global_argument_buffer.FunctionCode = NUB_DEBUG_ACTIVE_PROCESS;
  global_argument_buffer.u.NubDebugActiveProcess.Server = (SERVER) 0;
  global_argument_buffer.u.NubDebugActiveProcess.Process
    = (NUBPROCESS) &global_attachment_descriptor;
  global_argument_buffer.u.NubDebugActiveProcess.PathCount
    = 0;
  global_argument_buffer.u.NubDebugActiveProcess.Paths
    = dummy;
  global_argument_buffer.u.NubDebugActiveProcess.JITInfo
    = jit_info;
  debugger_nub_rpc_diagnostic
    ("rnub_debug_active_process: ", "Filled arguments");
  ReleaseSemaphore(global_call_requested, 1, NULL);
  WaitForSingleObject(global_results_available, INFINITE);
  debugger_nub_rpc_diagnostic
    ("rnub_debug_active_process: ", "Results available");
  result =
    (REMOTIZE_NUB( global_argument_buffer.u.NubDebugActiveProcess.Result ));
  ReleaseSemaphore(global_nub_available, 1, NULL);
  debugger_nub_rpc_diagnostic
    ("rnub_debug_active_process: ", "Return");
  return(result);
}

/* ********************************************************************** */
/* ** Functions for accessing system-level information.                ** */
/* ********************************************************************** */

NUBINT rnub_remote_value_byte_size
  (handle_t nub)
{
  return(nub_remote_value_byte_size((NUB) global_process));
}

NUBINT rnub_get_process_page_fault_count
  (handle_t nub)
{
  return(nub_get_process_page_fault_count((NUB) global_process));
}

/* ********************************************************************** */
/* ** Accessors on the NUBTHREAD descriptor.                           ** */
/* ********************************************************************** */

NUBINT rnub_thread_os_priority
  (handle_t nub, RNUBTHREAD nubthread)
{
  return
    (nub_thread_os_priority((NUB) global_process,
                            LOCALIZE_RNUBTHREAD( nubthread )));
}

NUBINT rnub_get_thread_cpu_time
  (handle_t nub, RNUBTHREAD nubthread)
{
  return
    (nub_get_thread_cpu_time((NUB) global_process,
                             LOCALIZE_RNUBTHREAD( nubthread )));
}

/* ********************************************************************** */
/* ** Accessors on the NUBLIBRARY descriptor.                          ** */
/* ********************************************************************** */

RTARGET_ADDRESS rnub_get_library_base_address
  (handle_t nub, RNUBLIBRARY nublibrary)
{
  TARGET_ADDRESS local_address =
     nub_get_library_base_address((NUB) global_process,
                                  LOCALIZE_RNUBLIBRARY( nublibrary ));
  return(REMOTIZE_TARGET_ADDRESS( local_address ));
}

void rnub_get_library_version
  (handle_t nub, RNUBLIBRARY nublibrary, NUBINT *maj, NUBINT *min)
{
  nub_get_library_version((NUB) global_process,
                          LOCALIZE_RNUBLIBRARY( nublibrary ),
                          maj,
                          min);
}

NUBINT rnub_get_library_filename_length
  (handle_t nub, RNUBLIBRARY nublibrary)
{
  return
    (nub_get_library_filename_length((NUB) global_process,
                                     LOCALIZE_RNUBLIBRARY( nublibrary )));
}

void rnub_get_library_filename
   (handle_t nub, RNUBLIBRARY nublibrary, NUBINT sz,
    unsigned char *buffer)
{
  nub_get_library_filename((NUB) global_process,
                           LOCALIZE_RNUBLIBRARY( nublibrary ),
                           sz,
                           buffer);
}

NUBINT rnub_get_library_undecorated_name_length
  (handle_t nub, RNUBLIBRARY nublibrary)
{
  return
    (nub_get_library_undecorated_name_length
       ((NUB) global_process,
        LOCALIZE_RNUBLIBRARY( nublibrary )));
}

void rnub_get_library_undecorated_name
  (handle_t nub, RNUBLIBRARY nublibrary, NUBINT sz, unsigned char *buf)
{
  nub_get_library_undecorated_name
    ((NUB) global_process,
     LOCALIZE_RNUBLIBRARY( nublibrary ),
     sz,
     buf);
}


/* ********************************************************************** */
/* ** Accessors on the register descriptors.                           ** */
/* ********************************************************************** */


NUBINT rnub_get_register_name_length
  (handle_t nub, NUB_INDEX reg)
{
  return(nub_get_register_name_length((NUB) global_process, reg));
}

void rnub_get_register_name
  (handle_t nub, NUB_INDEX reg, NUBINT sz, unsigned char *buf)
{
  nub_get_register_name((NUB) global_process, reg, sz, buf);
}

NUBINT rnub_get_register_enumeration_code
  (handle_t nub, NUB_INDEX reg)
{
  return(nub_get_register_enumeration_code((NUB) global_process, reg));
}

void rnub_all_registers
  (handle_t nub, NUB_INDEX *first, NUB_INDEX *last)
{
  nub_all_registers((NUB) global_process, first, last);
}

void rnub_general_registers
  (handle_t nub, NUB_INDEX *first, NUB_INDEX *last)
{
  nub_general_registers((NUB) global_process, first, last);
}

void rnub_special_registers
  (handle_t nub, NUB_INDEX *first, NUB_INDEX *last)
{
  nub_special_registers((NUB) global_process, first, last);
}

void rnub_floating_registers
  (handle_t nub, NUB_INDEX *first, NUB_INDEX *last)
{
  nub_floating_registers((NUB) global_process, first, last);
}


/* ********************************************************************** */
/* ** Virtual memory page information.                                 ** */
/* ********************************************************************** */


NUBINT rnub_page_read_permission
  (handle_t nub, RTARGET_ADDRESS address)
{
  return(nub_page_read_permission((NUB) global_process,
                                  LOCALIZE_RTARGET_ADDRESS( address )));
}

NUBINT rnub_page_write_permission
  (handle_t nub, RTARGET_ADDRESS address)
{
  return(nub_page_write_permission((NUB) global_process,
                                   LOCALIZE_RTARGET_ADDRESS( address )));
}

NUBINT rnub_page_relative_address
  (handle_t nub, RTARGET_ADDRESS address, NUBINT *offset)
{
  return(nub_page_relative_address
           ((NUB) global_process,
            LOCALIZE_RTARGET_ADDRESS( address ),
            offset));
}

NUBINT rnub_virtual_page_size
  (handle_t nub)
{
  return(nub_virtual_page_size((NUB) global_process));
}


/* ********************************************************************** */
/* ** Basic memory access.                                             ** */
/* ********************************************************************** */


RTARGET_ADDRESS rnub_read_value_from_process_memory
  (handle_t nub, RTARGET_ADDRESS location, NUB_ERROR *status)
{
  TARGET_ADDRESS    local_address =
    read_value_from_process_memory
      ((NUB) global_process, LOCALIZE_RTARGET_ADDRESS( location ), status);
  return(REMOTIZE_TARGET_ADDRESS( local_address ));
}

void rnub_write_value_to_process_memory
  (handle_t nub, RTARGET_ADDRESS location, RTARGET_ADDRESS value,
   NUB_ERROR *status)
{
  write_value_to_process_memory
    ((NUB) global_process,
     LOCALIZE_RTARGET_ADDRESS( location ),
     LOCALIZE_RTARGET_ADDRESS( value ),
     status);
}

RTARGET_ADDRESS rnub_calculate_stack_address
  (handle_t nub, RNUBTHREAD nubthread, NUBINT offset)
{
  TARGET_ADDRESS local_address =
    nub_calculate_stack_address((NUB) global_process,
                                LOCALIZE_RNUBTHREAD( nubthread ),
                                offset);
  return(REMOTIZE_TARGET_ADDRESS( local_address ));
}


/* ********************************************************************** */
/* ** Translation between remote values and printable forms.           ** */
/* ********************************************************************** */


void rnub_target_address_to_string
  (handle_t nub, RTARGET_ADDRESS x, NUBINT sz, unsigned char *buf,
   NUBINT radix, NUBINT pad, NUBINT *truncated)
{
  nub_target_address_to_string((NUB) global_process,
                               LOCALIZE_RTARGET_ADDRESS( x ),
                               sz,
                               buf,
                               radix,
                               pad,
                               truncated);
}

RTARGET_ADDRESS rnub_string_to_target_address
  (handle_t nub, NUBINT sz, unsigned char *buf, NUBINT radix, NUBINT *overflow)
{
  TARGET_ADDRESS local_address =
    nub_string_to_target_address((NUB) global_process,
                                 sz, buf, radix, overflow);
  return(REMOTIZE_TARGET_ADDRESS( local_address ));
}


/* ********************************************************************** */
/* ** Floating Point and Byte String transactions.                     ** */
/* ********************************************************************** */


FLOAT rnub_read_single_float_from_process_memory
  (handle_t nub, RTARGET_ADDRESS location, NUB_ERROR *status)
{
  return(read_single_float_from_process_memory
          ((NUB) global_process,
            LOCALIZE_RTARGET_ADDRESS( location ),
            status));
}

void rnub_write_single_float_to_process_memory
  (handle_t nub, RTARGET_ADDRESS location, FLOAT value, 
   NUB_ERROR *status)
{
  write_single_float_to_process_memory
    ((NUB) global_process,
     LOCALIZE_RTARGET_ADDRESS( location ),
     value,
     status);
}

DOUBLE rnub_read_double_float_from_process_memory
  (handle_t nub, RTARGET_ADDRESS location, NUB_ERROR *status)
{
  return(read_double_float_from_process_memory
          ((NUB) global_process,
            LOCALIZE_RTARGET_ADDRESS( location ),
            status));
}

void rnub_write_double_float_to_process_memory
  (handle_t nub, RTARGET_ADDRESS location, DOUBLE value, 
   NUB_ERROR *status)
{
  write_double_float_to_process_memory
    ((NUB) global_process,
     LOCALIZE_RTARGET_ADDRESS( location ),
     value,
     status);
}

void rnub_read_byte_string_from_process_memory
  (handle_t nub, RTARGET_ADDRESS location, NUBINT sz, unsigned char *buf,
   NUB_ERROR *status)
{
  read_byte_string_from_process_memory((NUB) global_process,
                                       LOCALIZE_RTARGET_ADDRESS( location ),
                                       sz,
                                       buf,
                                       status);
}

void rnub_write_byte_string_to_process_memory
  (handle_t nub, RTARGET_ADDRESS location, NUBINT sz, unsigned char *buf,
   NUB_ERROR *status)
{
  write_byte_string_to_process_memory((NUB) global_process,
                                       LOCALIZE_RTARGET_ADDRESS( location ),
                                       sz,
                                       buf,
                                       status);
}

RTARGET_ADDRESS rnub_read_value_from_process_register_in_stack_frame
  (handle_t nub, RNUBTHREAD nubthread, NUB_INDEX register_index, 
   NUB_INDEX frame_index, NUB_ERROR *status)
{
  TARGET_ADDRESS local_address =
    read_value_from_process_register_in_stack_frame
      ((NUB) global_process,
       LOCALIZE_RNUBTHREAD( nubthread ),
       register_index,
       frame_index,
       status);
  return(REMOTIZE_TARGET_ADDRESS( local_address ));
}

RTARGET_ADDRESS rnub_read_value_from_process_register
  (handle_t nub, RNUBTHREAD nubthread, NUB_INDEX register_index, 
   NUB_ERROR *status)
{
  TARGET_ADDRESS local_address =
    read_value_from_process_register
      ((NUB) global_process,
       LOCALIZE_RNUBTHREAD( nubthread ),
       register_index,
       status);
  return(REMOTIZE_TARGET_ADDRESS( local_address ));
}

void rnub_write_value_to_process_register
  (handle_t nub, RNUBTHREAD nubthread, NUB_INDEX register_index,
   RTARGET_ADDRESS value, NUB_ERROR *status)
{
  write_value_to_process_register((NUB) global_process,
                                  LOCALIZE_RNUBTHREAD( nubthread ),
                                  register_index,
                                  LOCALIZE_RTARGET_ADDRESS( value ),
                                  status);
}

FLOAT rnub_read_single_float_from_process_register
  (handle_t nub, RNUBTHREAD nubthread, NUB_INDEX register_index,
   NUB_ERROR *status)
{
  return(read_single_float_from_process_register
           ((NUB) global_process,
            LOCALIZE_RNUBTHREAD( nubthread ),
            register_index,
            status));
}

void rnub_write_single_float_to_process_register
  (handle_t nub, RNUBTHREAD nubthread, NUB_INDEX register_index,
   FLOAT value, NUB_ERROR *status)
{
  write_single_float_to_process_register((NUB) global_process,
                                         LOCALIZE_RNUBTHREAD( nubthread ),
                                         register_index,
                                         value,
                                         status);
}

DOUBLE rnub_read_double_float_from_process_register
  (handle_t nub, RNUBTHREAD nubthread, NUB_INDEX register_index,
   NUB_ERROR *status)
{
  return(read_double_float_from_process_register
           ((NUB) global_process,
            LOCALIZE_RNUBTHREAD( nubthread ),
            register_index,
            status));
}

void rnub_write_double_float_to_process_register
  (handle_t nub, RNUBTHREAD nubthread, NUB_INDEX register_index,
   DOUBLE value, NUB_ERROR *status)
{
  write_double_float_to_process_register((NUB) global_process,
                                         LOCALIZE_RNUBTHREAD( nubthread ),
                                         register_index,
                                         value,
                                         status);
}

void rnub_application_restart
  (handle_t nub)
{
  WaitForSingleObject(global_nub_available, INFINITE);
  global_argument_buffer.FunctionCode = NUB_APPLICATION_RESTART;
  global_argument_buffer.u.NubApplicationRestart.Nub = (NUB) global_process;
  ReleaseSemaphore(global_call_requested, 1, NULL);
  WaitForSingleObject(global_results_available, INFINITE);
  ReleaseSemaphore(global_nub_available, 1, NULL);
}


void rnub_application_stop
  (handle_t nub)
{
  WaitForSingleObject(global_nub_available, INFINITE);
  global_argument_buffer.FunctionCode = NUB_APPLICATION_STOP;
  global_argument_buffer.u.NubApplicationStop.Nub = (NUB) global_process;
  ReleaseSemaphore(global_call_requested, 1, NULL);
  WaitForSingleObject(global_results_available, INFINITE);
  ReleaseSemaphore(global_nub_available, 1, NULL);
}


void rnub_application_continue
  (handle_t nub)
{
  WaitForSingleObject(global_nub_available, INFINITE);
  global_argument_buffer.FunctionCode = NUB_APPLICATION_CONTINUE;
  global_argument_buffer.u.NubApplicationContinue.Nub = (NUB) global_process;
  ReleaseSemaphore(global_call_requested, 1, NULL);
  WaitForSingleObject(global_results_available, INFINITE);
  ReleaseSemaphore(global_nub_available, 1, NULL);
}


void rnub_application_continue_unhandled
  (handle_t nub)
{
  WaitForSingleObject(global_nub_available, INFINITE);
  global_argument_buffer.FunctionCode = NUB_APPLICATION_CONTINUE_UNHANDLED;
  global_argument_buffer.u.NubApplicationContinueUnhandled.Nub 
    = (NUB) global_process;
  ReleaseSemaphore(global_call_requested, 1, NULL);
  WaitForSingleObject(global_results_available, INFINITE);
  ReleaseSemaphore(global_nub_available, 1, NULL);
}

void rnub_application_step
  (handle_t nub, NUBINT m)
{
  /* Currently not implemented. Probably obsolete. */
}

void rnub_application_step_over
  (handle_t nub, NUBINT m)
{
  /* Currently not implemented. Probably obsolete. */
}

void rnub_application_step_out
  (handle_t nub)
{
  /* Currently not implemented. Probably obsolete. */
}

NUB_ERROR rnub_set_stepping_control_on_thread
  (handle_t nub, RNUBTHREAD nubthread,
   RTARGET_ADDRESS fp, RTARGET_ADDRESS calling_fp,
   NUBINT location_count, RTARGET_ADDRESS *locations,
   NUBINT operation)
{
  return(nub_set_stepping_control_on_thread
           ((NUB) global_process,
             LOCALIZE_RNUBTHREAD( nubthread ),
             LOCALIZE_RTARGET_ADDRESS( fp ),
             LOCALIZE_RTARGET_ADDRESS( calling_fp ),
             location_count,
             LOCALIZE_RTARGET_ADDRESS_ARRAY( locations ),
             operation));
}

void rnub_clear_stepping_control_on_thread
  (handle_t nub, RNUBTHREAD nubthread)
{
  nub_clear_stepping_control_on_thread((NUB) global_process,
                                       LOCALIZE_RNUBTHREAD( nubthread ));
}

void rnub_thread_stop
  (handle_t nub, RNUBTHREAD nubthread)
{
  nub_thread_stop((NUB) global_process, LOCALIZE_RNUBTHREAD( nubthread ));
}

void rnub_thread_continue
  (handle_t nub, RNUBTHREAD nubthread)
{
  nub_thread_continue((NUB) global_process, 
                      LOCALIZE_RNUBTHREAD( nubthread ));
}

NUB_ERROR rnub_kill_application
  (handle_t nub)
{
  NUB_ERROR result;
  WaitForSingleObject(global_nub_available, INFINITE);
  global_argument_buffer.FunctionCode = NUB_KILL_APPLICATION;
  global_argument_buffer.u.NubKillApplication.Nub = (NUB) global_process;
  ReleaseSemaphore(global_call_requested, 1, NULL);
  WaitForSingleObject(global_results_available, INFINITE);
  result = global_argument_buffer.u.NubKillApplication.Result;
  ReleaseSemaphore(global_nub_available, 1, NULL);
  return(result);
}

void rnub_close_application
  (handle_t nub)
{
  WaitForSingleObject(global_nub_available, INFINITE);
  global_argument_buffer.FunctionCode = NUB_CLOSE_APPLICATION;
  global_argument_buffer.u.NubCloseApplication.Nub = (NUB) global_process;
  ReleaseSemaphore(global_call_requested, 1, NULL);
  WaitForSingleObject(global_results_available, INFINITE);
  ReleaseSemaphore(global_nub_available, 1, NULL);
}

void rnub_close_remote_tether
  (handle_t nub)
{
  RpcMgmtStopServerListening(NULL);
}

RTARGET_ADDRESS rnub_setup_function_call
  (handle_t nub, RNUBTHREAD nubthread, RTARGET_ADDRESS func,
   NUBINT arg_count, RTARGET_ADDRESS *args, RNUBHANDLE *context)
{
  NUBHANDLE        local_context;
  TARGET_ADDRESS   local_return_address;
  WaitForSingleObject(global_nub_available, INFINITE);
  global_argument_buffer.FunctionCode = NUB_SETUP_FUNCTION_CALL;
  global_argument_buffer.u.NubSetupFunctionCall.Nub 
    = (NUB) global_process;
  global_argument_buffer.u.NubSetupFunctionCall.Nubthread 
    = LOCALIZE_RNUBTHREAD( nubthread );
  global_argument_buffer.u.NubSetupFunctionCall.Function 
    = LOCALIZE_RTARGET_ADDRESS( func );
  global_argument_buffer.u.NubSetupFunctionCall.ArgCount 
    = arg_count;
  global_argument_buffer.u.NubSetupFunctionCall.Args 
    = LOCALIZE_RTARGET_ADDRESS_ARRAY( args );
  global_argument_buffer.u.NubSetupFunctionCall.ContextHandle 
    = &local_context;
  ReleaseSemaphore(global_call_requested, 1, NULL);
  WaitForSingleObject(global_results_available, INFINITE);
  local_return_address =
    global_argument_buffer.u.NubSetupFunctionCall.Result;
  (*context) = REMOTIZE_NUBHANDLE( local_context );
  ReleaseSemaphore(global_nub_available, 1, NULL);
  return(REMOTIZE_TARGET_ADDRESS( local_return_address ));
}

RTARGET_ADDRESS rnub_remote_call_spy
  (handle_t nub, RNUBTHREAD nubthread, RTARGET_ADDRESS func,
   NUBINT arg_count, RTARGET_ADDRESS *args,
   NUB_ERROR *status)
{
  TARGET_ADDRESS      local_return_result;
  WaitForSingleObject(global_nub_available, INFINITE);
  global_argument_buffer.FunctionCode = NUB_REMOTE_CALL_SPY;
  global_argument_buffer.u.NubRemoteCallSpy.Nub
    = (NUB) global_process;
  global_argument_buffer.u.NubRemoteCallSpy.Nubthread
    = LOCALIZE_RNUBTHREAD( nubthread );
  global_argument_buffer.u.NubRemoteCallSpy.Function
    = LOCALIZE_RTARGET_ADDRESS( func );
  global_argument_buffer.u.NubRemoteCallSpy.ArgCount
    = arg_count;
  global_argument_buffer.u.NubRemoteCallSpy.Args
    = LOCALIZE_RTARGET_ADDRESS_ARRAY( args );
  global_argument_buffer.u.NubRemoteCallSpy.Status
    = status;
  ReleaseSemaphore(global_call_requested, 1, NULL);
  WaitForSingleObject(global_results_available, INFINITE);
  local_return_result =
    global_argument_buffer.u.NubRemoteCallSpy.Result;
  ReleaseSemaphore(global_nub_available, 1, NULL);
  return(REMOTIZE_TARGET_ADDRESS( local_return_result ));
}

RTARGET_ADDRESS rnub_get_function_result
  (handle_t nub, RNUBTHREAD nubthread)
{
  TARGET_ADDRESS local_return_result =
    nub_get_function_result((NUB) global_process,
                            LOCALIZE_RNUBTHREAD( nubthread ));
  return(REMOTIZE_TARGET_ADDRESS( local_return_result ));
}

void rnub_restore_context
  (handle_t nub, RNUBTHREAD nubthread, RNUBHANDLE context)
{
  nub_restore_context((NUB) global_process,
                      LOCALIZE_RNUBTHREAD( nubthread ),
                      LOCALIZE_RNUBHANDLE( context ));
}

NUB_ERROR rnub_set_breakpoint
  (handle_t nub, RTARGET_ADDRESS location)
{
  return(nub_set_breakpoint((NUB) global_process,
                            LOCALIZE_RTARGET_ADDRESS( location )));
}

NUB_ERROR rnub_clear_breakpoint
  (handle_t nub, RTARGET_ADDRESS location)
{
  return(nub_clear_breakpoint((NUB) global_process,
                              LOCALIZE_RTARGET_ADDRESS( location )));
}

NUBINT rnub_query_breakpoint
  (handle_t nub, RTARGET_ADDRESS location)
{
  return(nub_query_breakpoint((NUB) global_process,
                              LOCALIZE_RTARGET_ADDRESS( location )));
}

void rnub_wait_for_stop_reason_with_timeout
  (handle_t nub, NUBINT timeout, NUBINT *code)
{
  WaitForSingleObject(global_nub_available, INFINITE);
  global_argument_buffer.FunctionCode 
    = NUB_WAIT_FOR_STOP_REASON_WITH_TIMEOUT;
  global_argument_buffer.u.NubWaitForStopReasonWithTimeout.Nub 
    = (NUB) global_process;
  global_argument_buffer.u.NubWaitForStopReasonWithTimeout.Timeout
    = timeout;
  global_argument_buffer.u.NubWaitForStopReasonWithTimeout.Code
    = code;
  ReleaseSemaphore(global_call_requested, 1, NULL);
  WaitForSingleObject(global_results_available, INFINITE);
  ReleaseSemaphore(global_nub_available, 1, NULL);
}

void rnub_wait_for_stop_reason_no_timeout
  (handle_t nub, NUBINT *code)
{
  WaitForSingleObject(global_nub_available, INFINITE);
  global_argument_buffer.FunctionCode 
    = NUB_WAIT_FOR_STOP_REASON_NO_TIMEOUT;
  global_argument_buffer.u.NubWaitForStopReasonNoTimeout.Nub 
    = (NUB) global_process;
  global_argument_buffer.u.NubWaitForStopReasonNoTimeout.Code
    = code;
  ReleaseSemaphore(global_call_requested, 1, NULL);
  WaitForSingleObject(global_results_available, INFINITE);
  ReleaseSemaphore(global_nub_available, 1, NULL);
}

void rnub_profile_wait_for_stop_reason_with_timeout
  (handle_t nub, NUBINT timeout, NUBINT profile_interval, NUBINT *code)
{
  WaitForSingleObject(global_nub_available, INFINITE);
  global_argument_buffer.FunctionCode 
    = NUB_PROFILE_WAIT_FOR_STOP_REASON_WITH_TIMEOUT;
  global_argument_buffer.u.NubProfileWaitForStopReasonWithTimeout.Nub 
    = (NUB) global_process;
  global_argument_buffer.u.NubProfileWaitForStopReasonWithTimeout.Timeout
    = timeout;
  global_argument_buffer.u.NubProfileWaitForStopReasonWithTimeout.
    ProfileInterval = profile_interval;
  global_argument_buffer.u.NubProfileWaitForStopReasonWithTimeout.Code
    = code;
  ReleaseSemaphore(global_call_requested, 1, NULL);
  WaitForSingleObject(global_results_available, INFINITE);
  ReleaseSemaphore(global_nub_available, 1, NULL);
}

void rnub_profile_wait_for_stop_reason_no_timeout
  (handle_t nub, NUBINT profile_interval, NUBINT *code)
{
  WaitForSingleObject(global_nub_available, INFINITE);
  global_argument_buffer.FunctionCode 
    = NUB_PROFILE_WAIT_FOR_STOP_REASON_NO_TIMEOUT;
  global_argument_buffer.u.NubProfileWaitForStopReasonNoTimeout.Nub 
    = (NUB) global_process;
  global_argument_buffer.u.NubProfileWaitForStopReasonNoTimeout.
    ProfileInterval = profile_interval;
  global_argument_buffer.u.NubProfileWaitForStopReasonNoTimeout.Code
    = code;
  ReleaseSemaphore(global_call_requested, 1, NULL);
  WaitForSingleObject(global_results_available, INFINITE);
  ReleaseSemaphore(global_nub_available, 1, NULL);
}

void rnub_inform_profiling_started
  (handle_t nub)
{
  nub_inform_profiling_started((NUB) global_process);
}

void rnub_inform_profiling_stopped
  (handle_t nub)
{
  nub_inform_profiling_stopped((NUB) global_process);
}

NUBINT rnub_can_receive_first_chance
  (handle_t nub, NUBINT ecode)
{
  return(nub_can_receive_first_chance((NUB) global_process, ecode));
}

void rnub_set_first_chance
  (handle_t nub, NUBINT ecode)
{
  nub_set_first_chance((NUB) global_process, ecode);
}

void rnub_unset_first_chance
  (handle_t nub, NUBINT ecode)
{
  nub_unset_first_chance((NUB) global_process, ecode);
}

NUBINT rnub_thread_stop_information
  (handle_t nub, RNUBTHREAD nubthread,
   NUBINT *fchance, NUBINT *fstart, RTARGET_ADDRESS *ret_addr)
{
  TARGET_ADDRESS   local_ret_addr;
  NUBINT           result =
    nub_thread_stop_information((NUB) global_process,
                                LOCALIZE_RNUBTHREAD( nubthread ),
                                fchance,
                                fstart,
                                &local_ret_addr);
  (*ret_addr) = REMOTIZE_TARGET_ADDRESS( local_ret_addr );
  return(result);
}

RNUB rnub_stop_reason_process
  (handle_t nub)
{
  return(REMOTIZE_NUB( nub_stop_reason_process((NUB) global_process) ));
}

RNUBTHREAD rnub_stop_reason_thread
  (handle_t nub)
{
  return(REMOTIZE_NUBTHREAD( nub_stop_reason_thread((NUB) global_process) ));
}

NUBINT rnub_first_hard_coded_breakpoint
  (handle_t nub)
{
  return(nub_first_hard_coded_breakpoint((NUB) global_process));
}

NUBINT rnub_stop_reason_process_exit_code
  (handle_t nub)
{
  return(nub_stop_reason_process_exit_code((NUB) global_process));
}

NUBINT rnub_stop_reason_thread_exit_code
  (handle_t nub)
{
  return(nub_stop_reason_thread_exit_code((NUB) global_process));
}

RNUBLIBRARY rnub_stop_reason_library
  (handle_t nub)
{
  return(REMOTIZE_NUBLIBRARY( nub_stop_reason_library((NUB) global_process) ));
}

NUBINT rnub_stop_reason_violation_op
  (handle_t nub)
{
  return(nub_stop_reason_violation_op((NUB) global_process));
}

NUBINT rnub_exception_first_chance
  (handle_t nub)
{
  return(nub_exception_first_chance((NUB) global_process));
}

RTARGET_ADDRESS rnub_stop_reason_exception_address
  (handle_t nub)
{
  return(REMOTIZE_TARGET_ADDRESS(
           nub_stop_reason_exception_address((NUB) global_process) ));
}

RTARGET_ADDRESS rnub_stop_reason_violation_address
  (handle_t nub)
{
  return(REMOTIZE_TARGET_ADDRESS( 
           nub_stop_reason_violation_address((NUB) global_process) ));
}

RTARGET_ADDRESS rnub_stop_reason_debug_string_address
  (handle_t nub)
{
  return(REMOTIZE_TARGET_ADDRESS(
           nub_stop_reason_debug_string_address((NUB) global_process) ));
}

NUBINT rnub_stop_reason_debug_string_length
  (handle_t nub)
{
  return(nub_stop_reason_debug_string_length((NUB) global_process));
}

NUBINT rnub_stop_reason_debug_string_is_unicode
  (handle_t nub)
{
  return(nub_stop_reason_debug_string_is_unicode((NUB) global_process));
}

NUBINT rnub_initialize_stack_vectors
  (handle_t nub, RNUBTHREAD nubthread)
{
  return(nub_initialize_stack_vectors((NUB) global_process,
                                       LOCALIZE_RNUBTHREAD( nubthread )));
}

void rnub_read_stack_vectors
  (handle_t nub, RNUBTHREAD nubthread, NUBINT frame_count,
   RTARGET_ADDRESS *frame_pointers, RTARGET_ADDRESS *instruction_ptrs,
   RTARGET_ADDRESS *return_addresses)
{
  nub_read_stack_vectors((NUB) global_process,
                         LOCALIZE_RNUBTHREAD( nubthread ),
                         frame_count,
                         LOCALIZE_RTARGET_ADDRESS_ARRAY( frame_pointers ),
                         LOCALIZE_RTARGET_ADDRESS_ARRAY( instruction_ptrs ),
                         LOCALIZE_RTARGET_ADDRESS_ARRAY( return_addresses ));
}

void rnub_all_frame_lexicals
  (handle_t nub, RTARGET_ADDRESS frame, RTARGET_ADDRESS ip,
   NUB_INDEX *first, NUB_INDEX *last, RNUBHANDLE *table)
{
  NUBHANDLE      local_handle;
  nub_all_frame_lexicals((NUB) global_process,
                         LOCALIZE_RTARGET_ADDRESS( frame ),
                         LOCALIZE_RTARGET_ADDRESS( ip ),
                         first,
                         last,
                         &local_handle);
  (*table) = REMOTIZE_NUBHANDLE( local_handle );
}

void rnub_register_interactive_code_segment
  (handle_t nub, RTARGET_ADDRESS lower, RTARGET_ADDRESS upper)
{
  nub_register_interactive_code_segment((NUB) global_process,
                                        LOCALIZE_RTARGET_ADDRESS( lower ),
                                        LOCALIZE_RTARGET_ADDRESS( upper ));
}

NUBINT rnub_get_lexical_variable_name_length
  (handle_t nub, RNUBHANDLE table, NUB_INDEX index)
{
  return(nub_get_lexical_variable_name_length((NUB) global_process,
                                              LOCALIZE_RNUBHANDLE( table ),
                                              index));
}

void rnub_get_lexical_variable_name
  (handle_t nub, RNUBHANDLE table, NUB_INDEX index,
   NUBINT sz, unsigned char *buf)
{
  nub_get_lexical_variable_name((NUB) global_process,
                                LOCALIZE_RNUBHANDLE( table ),
                                index,
                                sz,
                                buf);
}

RTARGET_ADDRESS rnub_lexical_variable_address
  (handle_t nub, RTARGET_ADDRESS fp,
   RNUBHANDLE table, NUB_INDEX index,
   NUBINT *reg_lookup, NUB_INDEX *hireg, NUB_INDEX *loreg, NUBINT *arg)
{
  TARGET_ADDRESS local_address =
    nub_lexical_variable_address((NUB) global_process,
                                 LOCALIZE_RTARGET_ADDRESS( fp ),
                                 LOCALIZE_RNUBHANDLE( table ),
                                 index,
                                 reg_lookup,
                                 hireg,
                                 loreg,
                                 arg);
  return(REMOTIZE_TARGET_ADDRESS( local_address ));
}

NUBINT rnub_lookup_symbol_name_length
  (handle_t nub, RNUBHANDLE table, NUB_INDEX index)
{
  return(nub_lookup_symbol_name_length((NUB) global_process,
                                       LOCALIZE_RNUBHANDLE( table ),
                                       index));
}

void rnub_lookup_symbol_name
  (handle_t nub, RNUBHANDLE table, NUB_INDEX index,
   NUBINT sz, unsigned char *buf)
{
  nub_lookup_symbol_name((NUB) global_process,
                         LOCALIZE_RNUBHANDLE( table ),
                         index,
                         sz,
                         buf);
}

RTARGET_ADDRESS rnub_lookup_symbol_address
  (handle_t nub, RNUBHANDLE table, NUB_INDEX index)
{
  TARGET_ADDRESS local_address =
    nub_lookup_symbol_address((NUB) global_process,
                              LOCALIZE_RNUBHANDLE( table ),
                              index);
  return(REMOTIZE_TARGET_ADDRESS( local_address ));
}

RTARGET_ADDRESS rnub_lookup_function_debug_start
  (handle_t nub, RNUBHANDLE table, NUB_INDEX index)
{
  TARGET_ADDRESS local_address =
    nub_lookup_function_debug_start((NUB) global_process,
                                    LOCALIZE_RNUBHANDLE( table ),
                                    index);
  return(REMOTIZE_TARGET_ADDRESS( local_address ));
}

RTARGET_ADDRESS rnub_lookup_function_debug_end
  (handle_t nub, RNUBHANDLE table, NUB_INDEX index)
{
  TARGET_ADDRESS local_address =
    nub_lookup_function_debug_end((NUB) global_process,
                                  LOCALIZE_RNUBHANDLE( table ),
                                  index);
  return(REMOTIZE_TARGET_ADDRESS( local_address ));
}

NUBINT rnub_lookup_symbol_language
  (handle_t nub, RNUBHANDLE table, NUB_INDEX index)
{
  return(nub_lookup_symbol_language_code((NUB) global_process,
                                         LOCALIZE_RNUBHANDLE( table ),
                                         index));
}

RTARGET_ADDRESS rnub_lookup_function_end
  (handle_t nub, RNUBHANDLE table, NUB_INDEX index)
{
  TARGET_ADDRESS local_address =
    nub_lookup_function_end((NUB) global_process,
                            LOCALIZE_RNUBHANDLE( table ),
                            index);
  return(REMOTIZE_TARGET_ADDRESS( local_address ));
}

NUBINT rnub_symbol_is_function
  (handle_t nub, RNUBHANDLE table, NUB_INDEX index)
{
  return(nub_symbol_is_function((NUB) global_process,
                                 LOCALIZE_RNUBHANDLE( table ),
                                 index));
}

NUBINT rnub_nearest_symbols
  (handle_t nub, RTARGET_ADDRESS address,
   RNUBLIBRARY *lib, RNUBHANDLE *table)
{
  NUBLIBRARY local_library;
  NUBHANDLE  local_handle;
  NUBINT     result = nub_nearest_symbols((NUB) global_process,
                                          LOCALIZE_RTARGET_ADDRESS( address ),
                                          &local_library,
                                          &local_handle);
  (*lib) = REMOTIZE_NUBLIBRARY( local_library );
  (*table) = REMOTIZE_NUBHANDLE( local_handle );
  return(result);
}

NUBINT rnub_closest_symbol
  (handle_t nub, RTARGET_ADDRESS address,
   RNUBLIBRARY *lib, RTARGET_ADDRESS *actual_address,
   NUBINT *offset, NUBINT *name_length, NUBINT *type,
   NUBINT *is_function, RTARGET_ADDRESS *debug_start,
   RTARGET_ADDRESS *debug_end, NUBINT *language,
   RTARGET_ADDRESS *final_address)
{
  NUBLIBRARY      local_library;
  TARGET_ADDRESS  local_actual_address, local_debug_start,
                  local_debug_end, local_final_address;
  NUBINT          result = nub_closest_symbol
                             ((NUB) global_process,
                              LOCALIZE_RTARGET_ADDRESS( address ),
                              &local_library,
                              &local_actual_address,
                              offset, name_length, type, is_function,
                              &local_debug_start, &local_debug_end,
                              language,
                              &local_final_address);
  (*lib) = REMOTIZE_NUBLIBRARY( local_library );
  (*actual_address) = REMOTIZE_TARGET_ADDRESS( local_actual_address );
  (*debug_start) = REMOTIZE_TARGET_ADDRESS( local_debug_start );
  (*debug_end) = REMOTIZE_TARGET_ADDRESS( local_debug_end );
  (*final_address) = REMOTIZE_TARGET_ADDRESS( local_final_address );
  return(result);
}

void rnub_function_bounding_addresses
  (handle_t nub, RTARGET_ADDRESS address,
   RTARGET_ADDRESS *lower, RTARGET_ADDRESS *upper)
{
  TARGET_ADDRESS   local_lower, local_upper;
  nub_function_bounding_addresses((NUB) nub,
                                  LOCALIZE_RTARGET_ADDRESS( address ),
                                  &local_lower,
                                  &local_upper);
  (*lower) = REMOTIZE_TARGET_ADDRESS( local_lower );
  (*upper) = REMOTIZE_TARGET_ADDRESS( local_upper );
}

void rnub_closest_symbol_name
  (handle_t nub, NUBINT sz, unsigned char *buffer)
{
  nub_closest_symbol_name((NUB) global_process, sz, buffer);
}

NUBINT rnub_find_symbol_in_library
  (handle_t nub, RNUBLIBRARY nublibrary, NUBINT sz,
   unsigned char *name,
   RTARGET_ADDRESS *address, NUBINT *type,
   NUBINT *is_function,
   RTARGET_ADDRESS *debug_start, RTARGET_ADDRESS *debug_end,
   NUBINT *symbol_language,
   RTARGET_ADDRESS *final_address)
{
  TARGET_ADDRESS   local_address, local_debug_start, local_debug_end,
                   local_final_address;
  NUBINT           result =
                     nub_find_symbol_in_library
                       ((NUB) global_process,
                        LOCALIZE_RNUBLIBRARY( nublibrary ),
                        sz,
                        name,
                        &local_address,
                        type,
                        is_function,
                        &local_debug_start, &local_debug_end,
                        symbol_language,
                        &local_final_address);
  (*address) = REMOTIZE_TARGET_ADDRESS( local_address );
  (*debug_start) = REMOTIZE_TARGET_ADDRESS( local_debug_start );
  (*debug_end) = REMOTIZE_TARGET_ADDRESS( local_debug_end );
  (*final_address) = REMOTIZE_TARGET_ADDRESS( local_final_address );
  return(result);
}

void rnub_dispose_lookups
  (handle_t nub, RNUBHANDLE lookups)
{
  nub_dispose_lookups((NUB) global_process,
                      LOCALIZE_RNUBHANDLE( lookups ));
}

RTARGET_ADDRESS rnub_resolve_source_location
   (handle_t nub, RNUBLIBRARY nublibrary, unsigned char *filename,
    NUBINT line_number, NUBINT column_number,
    NUBINT *valid, NUBINT *path, RNUBHANDLE *search,
    NUBINT *exact)
{
  TARGET_ADDRESS local_address;
  NUBHANDLE      local_search;

  local_address = nub_resolve_source_location
                    ((NUB) global_process,
                     LOCALIZE_RNUBLIBRARY( nublibrary ),
                     filename,
                     line_number,
                     column_number,
                     valid,
                     path,
                     &local_search,
                     exact);
  (*search) = REMOTIZE_NUBHANDLE( local_search );
  return(REMOTIZE_TARGET_ADDRESS( local_address ));
}

RNUBHANDLE rnub_fetch_source_locations
  (handle_t nub, RTARGET_ADDRESS lower, RTARGET_ADDRESS upper)
{
  NUBHANDLE local_table =
    nub_fetch_source_locations((NUB) global_process,
                                LOCALIZE_RTARGET_ADDRESS( lower ),
                                LOCALIZE_RTARGET_ADDRESS( upper ));
  return(REMOTIZE_NUBHANDLE( local_table ));
}

NUBINT rnub_source_location_address
  (handle_t nub, RNUBHANDLE table, NUB_INDEX index)
{
  return(nub_source_location_address((NUB) global_process,
                                     LOCALIZE_RNUBHANDLE( table ),
                                     index));
}

NUBINT rnub_source_location_linenumber
  (handle_t nub, RNUBHANDLE table, NUB_INDEX index)
{
  return(nub_source_location_linenumber((NUB) global_process,
                                        LOCALIZE_RNUBHANDLE( table ),
                                        index));
}

NUBINT rnub_source_location_filename_length
  (handle_t nub, RNUBHANDLE table)
{
  return(nub_source_location_filename_length((NUB) global_process,
                                             LOCALIZE_RNUBHANDLE( table )));
}

void rnub_source_location_filename
  (handle_t nub, RNUBHANDLE table, NUBINT sz, unsigned char *buf)
{
  nub_source_location_filename((NUB) global_process,
                               LOCALIZE_RNUBHANDLE( table ),
                               sz,
                               buf);
}

NUBINT rnub_number_of_source_locations
  (handle_t nub, RNUBHANDLE table)
{
  return(nub_number_of_source_locations((NUB) global_process,
                                        LOCALIZE_RNUBHANDLE( table )));
}

void rnub_dispose_source_locations
  (handle_t nub, RNUBHANDLE lookups)
{
  nub_dispose_source_locations((NUB) global_process,
                               LOCALIZE_RNUBHANDLE( lookups ));
}

void rnub_interpret_instruction_at_current_location
  (handle_t nub, RNUBTHREAD nubthread,
   NUBINT *flow, RTARGET_ADDRESS *destination, NUBINT *sz)
{
  TARGET_ADDRESS     local_destination;
  nub_interpret_instruction_at_current_location
    ((NUB) global_process,
     LOCALIZE_RNUBTHREAD( nubthread ),
     flow,
     &local_destination,
     sz);
  (*destination) = REMOTIZE_TARGET_ADDRESS( local_destination );
}

RTARGET_ADDRESS rnub_dylan_calculate_step_into
  (handle_t nub, RNUBTHREAD nubthread, NUBINT *flive, NUBINT *ok)
{
  TARGET_ADDRESS     local_address;
  local_address = calculate_step_into_destination
                    ((NUB) global_process,
                     LOCALIZE_RNUBTHREAD( nubthread ),
                     flive,
                     ok);
  return(REMOTIZE_TARGET_ADDRESS( local_address ));
}

RTARGET_ADDRESS rnub_dylan_thread_environment_block_address
  (handle_t nub, RNUBTHREAD nubthread, NUBINT *valid)
{
  TARGET_ADDRESS  local_address;
  local_address = 
    nub_dylan_thread_environment_block_address
                                           ((NUB) global_process,
                                           LOCALIZE_RNUBTHREAD( nubthread ),
                                           valid);
  return(REMOTIZE_TARGET_ADDRESS( local_address ));
}

NUBINT rnub_dylan_thread_mv_buffer_live
  (handle_t nub, RNUBTHREAD nubthread)
{
  return(nub_dylan_thread_mv_buffer_live
           ((NUB) global_process,
            LOCALIZE_RNUBTHREAD( nubthread )));
}

NUBINT rnub_older_stack_frame
  (handle_t nub, RTARGET_ADDRESS this_one, RTARGET_ADDRESS than_this)
{
  return(nub_older_stack_frame((NUB) global_process,
                               LOCALIZE_RTARGET_ADDRESS( this_one ),
                               LOCALIZE_RTARGET_ADDRESS( than_this )));
}

RTARGET_ADDRESS rnub_dylan_current_function
  (handle_t nub, RNUBTHREAD nubthread)
{
  TARGET_ADDRESS  local_address;
  local_address = 
    nub_dylan_current_function((NUB) global_process,
                               LOCALIZE_RNUBTHREAD( nubthread ));
  return(REMOTIZE_TARGET_ADDRESS( local_address ));
}

NUBINT rnub_perform_absolute_relocation
  (handle_t nub, RTARGET_ADDRESS address, RTARGET_ADDRESS dest)
{
  return(nub_perform_absolute_relocation
           ((NUB) global_process,
            LOCALIZE_RTARGET_ADDRESS( address ),
            LOCALIZE_RTARGET_ADDRESS( dest )));
}

NUBINT rnub_perform_relative_relocation
  (handle_t nub, RTARGET_ADDRESS address, RTARGET_ADDRESS dest)
{
  return(nub_perform_relative_relocation
           ((NUB) global_process,
            LOCALIZE_RTARGET_ADDRESS( address ),
            LOCALIZE_RTARGET_ADDRESS( dest )));
}
