/* ************************************************************************ */
/* ** eproxy.c                                                           ** */
/* ** The API for the proxy debugger nub, linked to the environment      ** */
/* ** along with the local debugger nub. (Enterprise)                    ** */
/* ** ------------------------------------------------------------------ ** */
/* ** Author: Paul Howard, Copyright: (c) 1996 Functional Objects, Inc.   ** */
/* **                                 All Rights Reserved                ** */
/* ************************************************************************ */

#include "nub-core.h"
#include "transport_protocols.h"
#include "nubserve.h"
#include "rnub.h"

/* Allocation routines required by the RPC Runtime Library. */

void __RPC_FAR * __RPC_USER MIDL_user_allocate(size_t x)
{
  return ((void __RPC_FAR*) malloc (x));
}

void __RPC_USER MIDL_user_free(void __RPC_FAR * x)
{
  free(x);
}

NUB proxy_nub_create_and_debug_process
  (NUB nub, 
   NUBINT command_size, NUBINT arg_size,
   char *command, char *arguments, 
   NUBINT path_count, char **paths,
   NUBINT lib_path_count, char **lib_paths,
   NUBINT workdirsz,
   char *workdir,
   NUBINT create_shell)
{
  RNUB               result = (RNUB) 0;
  LPNUB_CONNECTION   connection = (LPNUB_CONNECTION) nub;

  result 
      = rnub_create_and_debug_process 
          (connection->RpcHandle,
           command_size,
           arg_size,
           command,
           arguments,
           path_count,
           lib_path_count,
           workdirsz,
           workdir,
           create_shell);

  return(LOCALIZE_RNUB( result ));
}

NUB proxy_nub_debug_active_process
  (NUB nub, 
   NUBINT name_length, char *name_string, 
   NUBINT id_string_length, char *id_string,
   unsigned long actual_id,
   NUBINT path_count, char **paths,
   NUBINT jit_info_sz,
   char *jit_info)
{
  RNUB               result = (RNUB) 0;
  LPNUB_CONNECTION   connection = (LPNUB_CONNECTION) nub;

  result 
      = rnub_debug_active_process
          (connection->RpcHandle,
           name_length + 1,         /* Add one to include NUL terminator */
           name_string,
           id_string_length + 1,    /* Add one to include NUL terminator */
           id_string,
           actual_id,
           path_count,
           jit_info_sz,
           jit_info);

  return(LOCALIZE_RNUB( result ));
}


/* *********************************************************************** */
/* ** Full set of proxy functions for the debugger nub interface.       ** */
/* *********************************************************************** */


/* ********************************************************************** */
/* ** Functions for accessing system-level information.                ** */
/* ********************************************************************** */

NUBINT proxy_nub_remote_value_byte_size
  (NUB nub)
{
  return(rnub_remote_value_byte_size(((LPNUB_CONNECTION) nub)->RpcHandle));
}

NUBINT proxy_nub_get_process_page_fault_count
  (NUB nub)
{
  return(rnub_get_process_page_fault_count(((LPNUB_CONNECTION) nub)->RpcHandle));
}

/* ********************************************************************** */
/* ** Accessors on the NUBTHREAD descriptor.                           ** */
/* ********************************************************************** */

NUBINT proxy_nub_thread_os_priority
  (NUB nub, NUBTHREAD nubthread)
{
  return
    (rnub_thread_os_priority(((LPNUB_CONNECTION) nub)->RpcHandle,
                            REMOTIZE_NUBTHREAD( nubthread )));
}

NUBINT proxy_nub_get_thread_cpu_time
  (NUB nub, NUBTHREAD nubthread)
{
  return
    (rnub_get_thread_cpu_time(((LPNUB_CONNECTION) nub)->RpcHandle,
                             REMOTIZE_NUBTHREAD( nubthread )));
}

/* ********************************************************************** */
/* ** Accessors on the NUBLIBRARY descriptor.                          ** */
/* ********************************************************************** */

TARGET_ADDRESS proxy_nub_get_library_base_address
  (NUB nub, NUBLIBRARY nublibrary)
{
  RTARGET_ADDRESS rem_address =
    rnub_get_library_base_address
      (((LPNUB_CONNECTION) nub)->RpcHandle,
       REMOTIZE_NUBLIBRARY( nublibrary ));
  return(LOCALIZE_RTARGET_ADDRESS( rem_address ));
}

void proxy_nub_get_library_version
  (NUB nub, NUBLIBRARY nublibrary, NUBINT *maj, NUBINT *min)
{
  rnub_get_library_version(((LPNUB_CONNECTION) nub)->RpcHandle,
                          REMOTIZE_NUBLIBRARY( nublibrary ),
                          maj,
                          min);
}

NUBINT proxy_nub_get_library_filename_length
  (NUB nub, NUBLIBRARY nublibrary)
{
  return
    (rnub_get_library_filename_length(((LPNUB_CONNECTION) nub)->RpcHandle,
                                     REMOTIZE_NUBLIBRARY( nublibrary )));
}

void proxy_nub_get_library_filename
   (NUB nub, NUBLIBRARY nublibrary, NUBINT sz,
    char *buffer)
{
  rnub_get_library_filename(((LPNUB_CONNECTION) nub)->RpcHandle,
                           REMOTIZE_NUBLIBRARY( nublibrary ),
                           sz,
                           buffer);
}

NUBINT proxy_nub_get_library_undecorated_name_length
  (NUB nub, NUBLIBRARY nublibrary)
{
  return
    (rnub_get_library_undecorated_name_length
       (((LPNUB_CONNECTION) nub)->RpcHandle,
        REMOTIZE_NUBLIBRARY( nublibrary )));
}

void proxy_nub_get_library_undecorated_name
  (NUB nub, NUBLIBRARY nublibrary, NUBINT sz, char *buf)
{
  rnub_get_library_undecorated_name
    (((LPNUB_CONNECTION) nub)->RpcHandle,
     REMOTIZE_NUBLIBRARY( nublibrary ),
     sz,
     buf);
}


/* ********************************************************************** */
/* ** Accessors on the register descriptors.                           ** */
/* ********************************************************************** */


NUBINT proxy_nub_get_register_name_length
  (NUB nub, NUB_INDEX reg)
{
  return(rnub_get_register_name_length
           (((LPNUB_CONNECTION) nub)->RpcHandle, reg));
}

void proxy_nub_get_register_name
  (NUB nub, NUB_INDEX reg, NUBINT sz, char *buf)
{
  rnub_get_register_name(((LPNUB_CONNECTION) nub)->RpcHandle, reg, sz, buf);
}

NUBINT proxy_nub_get_register_enumeration_code
  (NUB nub, NUB_INDEX reg)
{
  return(rnub_get_register_enumeration_code
            (((LPNUB_CONNECTION) nub)->RpcHandle, reg));
}

void proxy_nub_all_registers
  (NUB nub, NUB_INDEX *first, NUB_INDEX *last)
{
  rnub_all_registers(((LPNUB_CONNECTION) nub)->RpcHandle, first, last);
}

void proxy_nub_general_registers
  (NUB nub, NUB_INDEX *first, NUB_INDEX *last)
{
  rnub_general_registers(((LPNUB_CONNECTION) nub)->RpcHandle, first, last);
}

void proxy_nub_special_registers
  (NUB nub, NUB_INDEX *first, NUB_INDEX *last)
{
  rnub_special_registers(((LPNUB_CONNECTION) nub)->RpcHandle, first, last);
}

void proxy_nub_floating_registers
  (NUB nub, NUB_INDEX *first, NUB_INDEX *last)
{
  rnub_floating_registers(((LPNUB_CONNECTION) nub)->RpcHandle, first, last);
}


/* ********************************************************************** */
/* ** Virtual memory page information.                                 ** */
/* ********************************************************************** */


NUBINT proxy_nub_page_read_permission
  (NUB nub, TARGET_ADDRESS address)
{
  return(rnub_page_read_permission(((LPNUB_CONNECTION) nub)->RpcHandle,
                                  REMOTIZE_TARGET_ADDRESS( address )));
}

NUBINT proxy_nub_page_write_permission
  (NUB nub, TARGET_ADDRESS address)
{
  return(rnub_page_write_permission(((LPNUB_CONNECTION) nub)->RpcHandle,
                                   REMOTIZE_TARGET_ADDRESS( address )));
}

NUBINT proxy_nub_page_relative_address
  (NUB nub, TARGET_ADDRESS address, NUBINT *offset)
{
  return(rnub_page_relative_address
           (((LPNUB_CONNECTION) nub)->RpcHandle,
            REMOTIZE_TARGET_ADDRESS( address ),
            offset));
}

NUBINT proxy_nub_virtual_page_size
  (NUB nub)
{
  return(rnub_virtual_page_size(((LPNUB_CONNECTION) nub)->RpcHandle));
}


/* ********************************************************************** */
/* ** Basic memory access.                                             ** */
/* ********************************************************************** */


TARGET_ADDRESS proxy_read_value_from_process_memory
  (NUB nub, TARGET_ADDRESS location, NUB_ERROR *status)
{
  RTARGET_ADDRESS    rem_address =
    rnub_read_value_from_process_memory
      (((LPNUB_CONNECTION) nub)->RpcHandle, 
       REMOTIZE_TARGET_ADDRESS( location ), status);
  return(LOCALIZE_RTARGET_ADDRESS( rem_address ));
}

void proxy_write_value_to_process_memory
  (NUB nub, TARGET_ADDRESS location, TARGET_ADDRESS value,
   NUB_ERROR *status)
{
  rnub_write_value_to_process_memory
    (((LPNUB_CONNECTION) nub)->RpcHandle,
     REMOTIZE_TARGET_ADDRESS( location ),
     REMOTIZE_TARGET_ADDRESS( value ),
     status);
}

TARGET_ADDRESS proxy_nub_calculate_stack_address
  (NUB nub, NUBTHREAD nubthread, NUBINT offset)
{
  RTARGET_ADDRESS rem_address =
    rnub_calculate_stack_address(((LPNUB_CONNECTION) nub)->RpcHandle,
                                REMOTIZE_NUBTHREAD( nubthread ),
                                offset);
  return(LOCALIZE_RTARGET_ADDRESS( rem_address ));
}


/* ********************************************************************** */
/* ** Translation between remote values and printable forms.           ** */
/* ********************************************************************** */


void proxy_nub_target_address_to_string
  (NUB nub, TARGET_ADDRESS x, NUBINT sz, char *buf,
   NUBINT radix, NUBINT pad, NUBINT *truncated)
{
  rnub_target_address_to_string(((LPNUB_CONNECTION) nub)->RpcHandle,
                               REMOTIZE_TARGET_ADDRESS( x ),
                               sz,
                               buf,
                               radix,
                               pad,
                               truncated);
}

TARGET_ADDRESS proxy_nub_string_to_target_address
  (NUB nub, NUBINT sz, char *buf, NUBINT radix, NUBINT *overflow)
{
  RTARGET_ADDRESS rem_address =
    rnub_string_to_target_address(((LPNUB_CONNECTION) nub)->RpcHandle,
                                 sz, buf, radix, overflow);
  return(LOCALIZE_RTARGET_ADDRESS( rem_address ));
}


/* ********************************************************************** */
/* ** Floating Point and Byte String transactions.                     ** */
/* ********************************************************************** */


FLOAT proxy_read_single_float_from_process_memory
  (NUB nub, TARGET_ADDRESS location, NUB_ERROR *status)
{
  return(rnub_read_single_float_from_process_memory
          (((LPNUB_CONNECTION) nub)->RpcHandle,
            REMOTIZE_TARGET_ADDRESS( location ),
            status));
}

void proxy_write_single_float_to_process_memory
  (NUB nub, TARGET_ADDRESS location, FLOAT value, 
   NUB_ERROR *status)
{
  rnub_write_single_float_to_process_memory
    (((LPNUB_CONNECTION) nub)->RpcHandle,
     REMOTIZE_TARGET_ADDRESS( location ),
     value,
     status);
}

DOUBLE proxy_read_double_float_from_process_memory
  (NUB nub, TARGET_ADDRESS location, NUB_ERROR *status)
{
  return(rnub_read_double_float_from_process_memory
          (((LPNUB_CONNECTION) nub)->RpcHandle,
            REMOTIZE_TARGET_ADDRESS( location ),
            status));
}

void proxy_write_double_float_to_process_memory
  (NUB nub, TARGET_ADDRESS location, DOUBLE value, 
   NUB_ERROR *status)
{
  rnub_write_double_float_to_process_memory
    (((LPNUB_CONNECTION) nub)->RpcHandle,
     REMOTIZE_TARGET_ADDRESS( location ),
     value,
     status);
}

void proxy_read_byte_string_from_process_memory
  (NUB nub, TARGET_ADDRESS location, NUBINT sz, char *buf,
   NUB_ERROR *status)
{
  rnub_read_byte_string_from_process_memory
        (((LPNUB_CONNECTION) nub)->RpcHandle,
         REMOTIZE_TARGET_ADDRESS( location ),
         sz,
         buf,
         status);
}

void proxy_write_byte_string_to_process_memory
  (NUB nub, TARGET_ADDRESS location, NUBINT sz, char *buf,
   NUB_ERROR *status)
{
  rnub_write_byte_string_to_process_memory
                                      (((LPNUB_CONNECTION) nub)->RpcHandle,
                                       REMOTIZE_TARGET_ADDRESS( location ),
                                       sz,
                                       buf,
                                       status);
}

TARGET_ADDRESS proxy_read_value_from_process_register_in_stack_frame
  (NUB nub, NUBTHREAD nubthread, NUB_INDEX register_index, 
   NUB_INDEX frame_index, NUB_ERROR *status)
{
  RTARGET_ADDRESS rem_address =
    rnub_read_value_from_process_register_in_stack_frame
      (((LPNUB_CONNECTION) nub)->RpcHandle,
       REMOTIZE_NUBTHREAD( nubthread ),
       register_index,
       frame_index,
       status);
  return(LOCALIZE_RTARGET_ADDRESS( rem_address ));
}

TARGET_ADDRESS proxy_read_value_from_process_register
  (NUB nub, NUBTHREAD nubthread, NUB_INDEX register_index, 
   NUB_ERROR *status)
{
  RTARGET_ADDRESS rem_address =
    rnub_read_value_from_process_register
      (((LPNUB_CONNECTION) nub)->RpcHandle,
       REMOTIZE_NUBTHREAD( nubthread ),
       register_index,
       status);
  return(LOCALIZE_RTARGET_ADDRESS( rem_address ));
}

void proxy_write_value_to_process_register
  (NUB nub, NUBTHREAD nubthread, NUB_INDEX register_index,
   TARGET_ADDRESS value, NUB_ERROR *status)
{
  rnub_write_value_to_process_register(((LPNUB_CONNECTION) nub)->RpcHandle,
                                        REMOTIZE_NUBTHREAD( nubthread ),
                                        register_index,
                                        REMOTIZE_TARGET_ADDRESS( value ),
                                        status);
}

FLOAT proxy_read_single_float_from_process_register
  (NUB nub, NUBTHREAD nubthread, NUB_INDEX register_index,
   NUB_ERROR *status)
{
  return(rnub_read_single_float_from_process_register
           (((LPNUB_CONNECTION) nub)->RpcHandle,
            REMOTIZE_NUBTHREAD( nubthread ),
            register_index,
            status));
}

void proxy_write_single_float_to_process_register
  (NUB nub, NUBTHREAD nubthread, NUB_INDEX register_index,
   FLOAT value, NUB_ERROR *status)
{
  rnub_write_single_float_to_process_register
                                        (((LPNUB_CONNECTION) nub)->RpcHandle,
                                         REMOTIZE_NUBTHREAD( nubthread ),
                                         register_index,
                                         value,
                                         status);
}

DOUBLE proxy_read_double_float_from_process_register
  (NUB nub, NUBTHREAD nubthread, NUB_INDEX register_index,
   NUB_ERROR *status)
{
  return(rnub_read_double_float_from_process_register
           (((LPNUB_CONNECTION) nub)->RpcHandle,
            REMOTIZE_NUBTHREAD( nubthread ),
            register_index,
            status));
}

void proxy_write_double_float_to_process_register
  (NUB nub, NUBTHREAD nubthread, NUB_INDEX register_index,
   DOUBLE value, NUB_ERROR *status)
{
  rnub_write_double_float_to_process_register
                                       (((LPNUB_CONNECTION) nub)->RpcHandle,
                                         REMOTIZE_NUBTHREAD( nubthread ),
                                         register_index,
                                         value,
                                         status);
}

void proxy_nub_application_restart
  (NUB nub)
{
  LPNUB_CONNECTION     connection = (LPNUB_CONNECTION) nub;
  rnub_application_restart(connection->RpcHandle);
}


void proxy_nub_application_stop
  (NUB nub)
{
  LPNUB_CONNECTION     connection = (LPNUB_CONNECTION) nub;
  rnub_application_stop(connection->RpcHandle);
}


void proxy_nub_application_continue
  (NUB nub)
{
  LPNUB_CONNECTION     connection = (LPNUB_CONNECTION) nub;
  rnub_application_continue(connection->RpcHandle);
}


void proxy_nub_application_continue_unhandled
  (NUB nub)
{
  LPNUB_CONNECTION     connection = (LPNUB_CONNECTION) nub;
  rnub_application_continue_unhandled(connection->RpcHandle);
}

void proxy_nub_application_step
  (NUB nub, NUBINT m)
{
  /* Currently not implemented. Probably obsolete. */
}

void proxy_nub_application_step_over
  (NUB nub, NUBINT m)
{
  /* Currently not implemented. Probably obsolete. */
}

void proxy_nub_application_step_out
  (NUB nub)
{
  /* Currently not implemented. Probably obsolete. */
}

NUB_ERROR proxy_nub_set_stepping_control_on_thread
  (NUB nub, NUBTHREAD nubthread,
   TARGET_ADDRESS fp, TARGET_ADDRESS calling_fp,
   NUBINT location_count, TARGET_ADDRESS *locations,
   NUBINT operation)
{
  return(rnub_set_stepping_control_on_thread
           (((LPNUB_CONNECTION) nub)->RpcHandle,
             REMOTIZE_NUBTHREAD( nubthread ),
             REMOTIZE_TARGET_ADDRESS( fp ),
             REMOTIZE_TARGET_ADDRESS( calling_fp ),
             location_count,
             REMOTIZE_TARGET_ADDRESS_ARRAY( locations ),
             operation));
}

void proxy_nub_clear_stepping_control_on_thread
  (NUB nub, NUBTHREAD nubthread)
{
  rnub_clear_stepping_control_on_thread(((LPNUB_CONNECTION) nub)->RpcHandle,
                                         REMOTIZE_NUBTHREAD( nubthread ));
}

void proxy_nub_thread_stop
  (NUB nub, NUBTHREAD nubthread)
{
  rnub_thread_stop
    (((LPNUB_CONNECTION) nub)->RpcHandle, REMOTIZE_NUBTHREAD( nubthread ));
}

void proxy_nub_thread_continue
  (NUB nub, NUBTHREAD nubthread)
{
  rnub_thread_continue(((LPNUB_CONNECTION) nub)->RpcHandle, 
                       REMOTIZE_NUBTHREAD( nubthread ));
}

NUB_ERROR proxy_nub_kill_application
  (NUB nub)
{
  LPNUB_CONNECTION     connection = (LPNUB_CONNECTION) nub;
  return(rnub_kill_application(connection->RpcHandle));
}

void proxy_nub_close_application
  (NUB nub)
{
  LPNUB_CONNECTION     connection = (LPNUB_CONNECTION) nub;
  rnub_kill_application(connection->RpcHandle);
}

TARGET_ADDRESS proxy_nub_setup_function_call
  (NUB nub, NUBTHREAD nubthread, TARGET_ADDRESS func,
   NUBINT arg_count, TARGET_ADDRESS *args, NUBHANDLE *context)
{
  LPNUB_CONNECTION     connection = (LPNUB_CONNECTION) nub;
  RTARGET_ADDRESS      rem_result;
  RNUBHANDLE           rem_context;

  rem_result =
    rnub_setup_function_call
      (connection->RpcHandle,
       REMOTIZE_NUBTHREAD( nubthread ),
       REMOTIZE_TARGET_ADDRESS( func ),
       arg_count,
       REMOTIZE_TARGET_ADDRESS_ARRAY( args ),
       &rem_context);

  (*context) = LOCALIZE_RNUBHANDLE( rem_context );
  return(LOCALIZE_RTARGET_ADDRESS( rem_result ));
}

TARGET_ADDRESS proxy_nub_remote_call_spy
  (NUB nub, NUBTHREAD nubthread, TARGET_ADDRESS func,
   NUBINT arg_count, TARGET_ADDRESS *args,
   NUB_ERROR *status)
{
  LPNUB_CONNECTION     connection = (LPNUB_CONNECTION) nub;
  RTARGET_ADDRESS      rem_result;

  rem_result =
    rnub_remote_call_spy
      (connection->RpcHandle,
       REMOTIZE_NUBTHREAD( nubthread ),
       REMOTIZE_TARGET_ADDRESS( func ),
       arg_count,
       REMOTIZE_TARGET_ADDRESS_ARRAY( args ),
       status);

  return(LOCALIZE_RTARGET_ADDRESS( rem_result ));
}

TARGET_ADDRESS proxy_nub_get_function_result
  (NUB nub, NUBTHREAD nubthread)
{
  RTARGET_ADDRESS rem_return_result =
    rnub_get_function_result(((LPNUB_CONNECTION) nub)->RpcHandle,
                             REMOTIZE_NUBTHREAD( nubthread ));
  return(LOCALIZE_RTARGET_ADDRESS( rem_return_result ));
}

void proxy_nub_restore_context
  (NUB nub, NUBTHREAD nubthread, NUBHANDLE context)
{
  rnub_restore_context(((LPNUB_CONNECTION) nub)->RpcHandle,
                       REMOTIZE_NUBTHREAD( nubthread ),
                       REMOTIZE_NUBHANDLE( context ));
}

NUB_ERROR proxy_nub_set_breakpoint
  (NUB nub, TARGET_ADDRESS location)
{
  return(rnub_set_breakpoint(((LPNUB_CONNECTION) nub)->RpcHandle,
                             REMOTIZE_TARGET_ADDRESS( location )));
}

NUB_ERROR proxy_nub_clear_breakpoint
  (NUB nub, TARGET_ADDRESS location)
{
  return(rnub_clear_breakpoint(((LPNUB_CONNECTION) nub)->RpcHandle,
                              REMOTIZE_TARGET_ADDRESS( location )));
}

NUBINT proxy_nub_query_breakpoint
  (NUB nub, TARGET_ADDRESS location)
{
  return(rnub_query_breakpoint(((LPNUB_CONNECTION) nub)->RpcHandle,
                               REMOTIZE_TARGET_ADDRESS( location )));
}

void proxy_nub_wait_for_stop_reason_with_timeout
  (NUB nub, NUBINT timeout, NUBINT *code)
{
  rnub_wait_for_stop_reason_with_timeout
     (((LPNUB_CONNECTION) nub)->RpcHandle, timeout, code);
}

void proxy_nub_wait_for_stop_reason_no_timeout
  (NUB nub, NUBINT *code)
{
  rnub_wait_for_stop_reason_no_timeout
     (((LPNUB_CONNECTION) nub)->RpcHandle, code);
}

void proxy_nub_profile_wait_for_stop_reason_with_timeout
  (NUB nub, NUBINT timeout, NUBINT profile_interval, NUBINT *code)
{
  rnub_profile_wait_for_stop_reason_with_timeout
     (((LPNUB_CONNECTION) nub)->RpcHandle, timeout, profile_interval, code);
}

void proxy_nub_profile_wait_for_stop_reason_no_timeout
  (NUB nub, NUBINT profile_interval, NUBINT *code)
{
  rnub_profile_wait_for_stop_reason_no_timeout
     (((LPNUB_CONNECTION) nub)->RpcHandle, profile_interval, code);
}

void proxy_nub_inform_profiling_started
  (NUB nub)
{
  rnub_inform_profiling_started(((LPNUB_CONNECTION) nub)->RpcHandle);
}

void proxy_nub_inform_profiling_stopped
  (NUB nub)
{
  rnub_inform_profiling_stopped(((LPNUB_CONNECTION) nub)->RpcHandle);
}

NUBINT proxy_nub_can_receive_first_chance
  (NUB nub, NUBINT ecode)
{
  return
    (rnub_can_receive_first_chance
      (((LPNUB_CONNECTION) nub)->RpcHandle, ecode));
}

void proxy_nub_set_first_chance
  (NUB nub, NUBINT ecode)
{
  rnub_set_first_chance(((LPNUB_CONNECTION) nub)->RpcHandle, ecode);
}

void proxy_nub_unset_first_chance
  (NUB nub, NUBINT ecode)
{
  rnub_unset_first_chance(((LPNUB_CONNECTION) nub)->RpcHandle, ecode);
}

NUBINT proxy_nub_thread_stop_information
  (NUB nub, NUBTHREAD nubthread,
   NUBINT *fchance, NUBINT *fstart, TARGET_ADDRESS *ret_addr)
{
  RTARGET_ADDRESS  rem_ret_addr;
  NUBINT           result =
    rnub_thread_stop_information(((LPNUB_CONNECTION) nub)->RpcHandle,
                                 REMOTIZE_NUBTHREAD( nubthread ),
                                 fchance,
                                 fstart,
                                 &rem_ret_addr);
  (*ret_addr) = LOCALIZE_RTARGET_ADDRESS( rem_ret_addr );
  return(result);
}

NUB proxy_nub_stop_reason_process
  (NUB nub)
{
  return(LOCALIZE_RNUB( 
          rnub_stop_reason_process(((LPNUB_CONNECTION) nub)->RpcHandle) ));
}

NUBTHREAD proxy_nub_stop_reason_thread
  (NUB nub)
{
  return(LOCALIZE_RNUBTHREAD( 
           rnub_stop_reason_thread(((LPNUB_CONNECTION) nub)->RpcHandle) ));
}

NUBINT proxy_nub_first_hard_coded_breakpoint
  (NUB nub)
{
  return(rnub_first_hard_coded_breakpoint
           (((LPNUB_CONNECTION) nub)->RpcHandle));
}

NUBINT proxy_nub_stop_reason_process_exit_code
  (NUB nub)
{
  return(rnub_stop_reason_process_exit_code
           (((LPNUB_CONNECTION) nub)->RpcHandle));
}

NUBINT proxy_nub_stop_reason_thread_exit_code
  (NUB nub)
{
  return(rnub_stop_reason_thread_exit_code
           (((LPNUB_CONNECTION) nub)->RpcHandle));
}

NUBLIBRARY proxy_nub_stop_reason_library
  (NUB nub)
{
  return(LOCALIZE_RNUBLIBRARY
           ( rnub_stop_reason_library(((LPNUB_CONNECTION) nub)->RpcHandle) ));
}

NUBINT proxy_nub_stop_reason_violation_op
  (NUB nub)
{
  return(rnub_stop_reason_violation_op(((LPNUB_CONNECTION) nub)->RpcHandle));
}

NUBINT proxy_nub_exception_first_chance
  (NUB nub)
{
  return(rnub_exception_first_chance(((LPNUB_CONNECTION) nub)->RpcHandle));
}

TARGET_ADDRESS proxy_nub_stop_reason_exception_address
  (NUB nub)
{
  return(LOCALIZE_RTARGET_ADDRESS(
           rnub_stop_reason_exception_address
             (((LPNUB_CONNECTION) nub)->RpcHandle) ));
}

TARGET_ADDRESS proxy_nub_stop_reason_violation_address
  (NUB nub)
{
  return(LOCALIZE_RTARGET_ADDRESS( 
           rnub_stop_reason_violation_address
             (((LPNUB_CONNECTION) nub)->RpcHandle) ));
}

TARGET_ADDRESS proxy_nub_stop_reason_debug_string_address
  (NUB nub)
{
  return(LOCALIZE_RTARGET_ADDRESS(
           rnub_stop_reason_debug_string_address
             (((LPNUB_CONNECTION) nub)->RpcHandle) ));
}

NUBINT proxy_nub_stop_reason_debug_string_length
  (NUB nub)
{
  return(rnub_stop_reason_debug_string_length
          (((LPNUB_CONNECTION) nub)->RpcHandle));
}

NUBINT proxy_nub_stop_reason_debug_string_is_unicode
  (NUB nub)
{
  return(rnub_stop_reason_debug_string_is_unicode
           (((LPNUB_CONNECTION) nub)->RpcHandle));
}

NUBINT proxy_nub_initialize_stack_vectors
  (NUB nub, NUBTHREAD nubthread)
{
  return(rnub_initialize_stack_vectors(((LPNUB_CONNECTION) nub)->RpcHandle,
                                       REMOTIZE_NUBTHREAD( nubthread )));
}

void proxy_nub_read_stack_vectors
  (NUB nub, NUBTHREAD nubthread, NUBINT frame_count,
   TARGET_ADDRESS *frame_pointers, TARGET_ADDRESS *instruction_ptrs,
   TARGET_ADDRESS *return_addresses)
{
  rnub_read_stack_vectors(((LPNUB_CONNECTION) nub)->RpcHandle,
                          REMOTIZE_NUBTHREAD( nubthread ),
                          frame_count,
                          REMOTIZE_TARGET_ADDRESS_ARRAY( frame_pointers ),
                          REMOTIZE_TARGET_ADDRESS_ARRAY( instruction_ptrs ),
                          REMOTIZE_TARGET_ADDRESS_ARRAY( return_addresses ));
}

void proxy_nub_all_frame_lexicals
  (NUB nub, TARGET_ADDRESS frame, TARGET_ADDRESS ip,
   NUB_INDEX *first, NUB_INDEX *last, NUBHANDLE *table)
{
  RNUBHANDLE     rem_handle;
  rnub_all_frame_lexicals(((LPNUB_CONNECTION) nub)->RpcHandle,
                          REMOTIZE_TARGET_ADDRESS( frame ),
                          REMOTIZE_TARGET_ADDRESS( ip ),
                          first,
                          last,
                          &rem_handle);
  (*table) = LOCALIZE_RNUBHANDLE( rem_handle );
}

void proxy_nub_register_interactive_code_segment
  (NUB nub, TARGET_ADDRESS lower, TARGET_ADDRESS upper)
{
  rnub_register_interactive_code_segment(((LPNUB_CONNECTION) nub)->RpcHandle,
                                         REMOTIZE_TARGET_ADDRESS( lower ),
                                         REMOTIZE_TARGET_ADDRESS( upper ));
}

NUBINT proxy_nub_get_lexical_variable_name_length
  (NUB nub, NUBHANDLE table, NUB_INDEX index)
{
  return
    (rnub_get_lexical_variable_name_length(((LPNUB_CONNECTION) nub)->RpcHandle,
                                            REMOTIZE_NUBHANDLE( table ),
                                            index));
}

void proxy_nub_get_lexical_variable_name
  (NUB nub, NUBHANDLE table, NUB_INDEX index,
   NUBINT sz, char *buf)
{
  rnub_get_lexical_variable_name(((LPNUB_CONNECTION) nub)->RpcHandle,
                                REMOTIZE_NUBHANDLE( table ),
                                index,
                                sz,
                                buf);
}

TARGET_ADDRESS proxy_nub_lexical_variable_address
  (NUB nub, TARGET_ADDRESS fp,
   NUBHANDLE table, NUB_INDEX index,
   NUBINT *reg_lookup, NUB_INDEX *hireg, NUB_INDEX *loreg, NUBINT *arg)
{
  RTARGET_ADDRESS rem_address =
    rnub_lexical_variable_address(((LPNUB_CONNECTION) nub)->RpcHandle,
                                  REMOTIZE_TARGET_ADDRESS( fp ),
                                  REMOTIZE_NUBHANDLE( table ),
                                  index,
                                  reg_lookup,
                                  hireg,
                                  loreg,
                                  arg);
  return(LOCALIZE_RTARGET_ADDRESS( rem_address ));
}

NUBINT proxy_nub_lookup_symbol_name_length
  (NUB nub, NUBHANDLE table, NUB_INDEX index)
{
  return(rnub_lookup_symbol_name_length(((LPNUB_CONNECTION) nub)->RpcHandle,
                                        REMOTIZE_NUBHANDLE( table ),
                                        index));
}

void proxy_nub_lookup_symbol_name
  (NUB nub, NUBHANDLE table, NUB_INDEX index,
   NUBINT sz, char *buf)
{
  rnub_lookup_symbol_name(((LPNUB_CONNECTION) nub)->RpcHandle,
                          REMOTIZE_NUBHANDLE( table ),
                          index,
                          sz,
                          buf);
}

TARGET_ADDRESS proxy_nub_lookup_symbol_address
  (NUB nub, NUBHANDLE table, NUB_INDEX index)
{
  RTARGET_ADDRESS rem_address =
    rnub_lookup_symbol_address(((LPNUB_CONNECTION) nub)->RpcHandle,
                               REMOTIZE_NUBHANDLE( table ),
                               index);
  return(LOCALIZE_RTARGET_ADDRESS( rem_address ));
}

TARGET_ADDRESS proxy_nub_lookup_function_debug_start
  (NUB nub, NUBHANDLE table, NUB_INDEX index)
{
  RTARGET_ADDRESS rem_address =
    rnub_lookup_function_debug_start(((LPNUB_CONNECTION) nub)->RpcHandle,
                                     REMOTIZE_NUBHANDLE( table ),
                                     index);
  return(LOCALIZE_RTARGET_ADDRESS( rem_address ));
}

TARGET_ADDRESS proxy_nub_lookup_function_debug_end
  (NUB nub, NUBHANDLE table, NUB_INDEX index)
{
  RTARGET_ADDRESS rem_address =
    rnub_lookup_function_debug_end(((LPNUB_CONNECTION) nub)->RpcHandle,
                                   REMOTIZE_NUBHANDLE( table ),
                                   index);
  return(LOCALIZE_RTARGET_ADDRESS( rem_address ));
}

NUBINT proxy_nub_lookup_symbol_language_code
  (NUB nub, NUBHANDLE table, NUB_INDEX index)
{
  return(rnub_lookup_symbol_language(((LPNUB_CONNECTION) nub)->RpcHandle,
                                     REMOTIZE_NUBHANDLE( table ),
                                     index));
}

TARGET_ADDRESS proxy_nub_lookup_function_end
  (NUB nub, NUBHANDLE table, NUB_INDEX index)
{
  RTARGET_ADDRESS rem_address =
    rnub_lookup_function_end(((LPNUB_CONNECTION) nub)->RpcHandle,
                             REMOTIZE_NUBHANDLE( table ),
                             index);
  return(LOCALIZE_RTARGET_ADDRESS( rem_address ));
}

NUBINT proxy_nub_symbol_is_function
  (NUB nub, NUBHANDLE table, NUB_INDEX index)
{
  return(rnub_symbol_is_function(((LPNUB_CONNECTION) nub)->RpcHandle,
                                  REMOTIZE_NUBHANDLE( table ),
                                  index));
}

NUBINT proxy_nub_nearest_symbols
  (NUB nub, TARGET_ADDRESS address,
   NUBLIBRARY *lib, NUBHANDLE *table)
{
  RNUBLIBRARY rem_library;
  RNUBHANDLE  rem_handle;
  NUBINT     result = rnub_nearest_symbols(((LPNUB_CONNECTION) nub)->RpcHandle,
                                           REMOTIZE_TARGET_ADDRESS( address ),
                                           &rem_library,
                                           &rem_handle);
  (*lib) = LOCALIZE_RNUBLIBRARY( rem_library );
  (*table) = LOCALIZE_RNUBHANDLE( rem_handle );
  return(result);
}

NUBINT proxy_nub_closest_symbol
  (NUB nub, TARGET_ADDRESS address,
   NUBLIBRARY *lib, TARGET_ADDRESS *actual_address,
   NUBINT *offset, NUBINT *name_length, NUBINT *type,
   NUBINT *is_function, TARGET_ADDRESS *debug_start,
   TARGET_ADDRESS *debug_end, NUBINT *language,
   TARGET_ADDRESS *final_address)
{
  RNUBLIBRARY      rem_library;
  RTARGET_ADDRESS  rem_actual_address, rem_debug_start,
                   rem_debug_end, rem_final_address;
  NUBINT          result = rnub_closest_symbol
                             (((LPNUB_CONNECTION) nub)->RpcHandle,
                              REMOTIZE_TARGET_ADDRESS( address ),
                              &rem_library,
                              &rem_actual_address,
                              offset, name_length, type, is_function,
                              &rem_debug_start, &rem_debug_end,
                              language,
                              &rem_final_address);
  (*lib) = LOCALIZE_RNUBLIBRARY( rem_library );
  (*actual_address) = LOCALIZE_RTARGET_ADDRESS( rem_actual_address );
  (*debug_start) = LOCALIZE_RTARGET_ADDRESS( rem_debug_start );
  (*debug_end) = LOCALIZE_RTARGET_ADDRESS( rem_debug_end );
  (*final_address) = LOCALIZE_RTARGET_ADDRESS( rem_final_address );
  return(result);
}

void proxy_nub_function_bounding_addresses
  (NUB nub, TARGET_ADDRESS address,
   TARGET_ADDRESS *lower, TARGET_ADDRESS *upper)
{
  RTARGET_ADDRESS   rem_lower, rem_upper;
  rnub_function_bounding_addresses((NUB) nub,
                                   REMOTIZE_TARGET_ADDRESS( address ),
                                   &rem_lower,
                                   &rem_upper);
  (*lower) = LOCALIZE_RTARGET_ADDRESS( rem_lower );
  (*upper) = LOCALIZE_RTARGET_ADDRESS( rem_upper );
}

void proxy_nub_closest_symbol_name
  (NUB nub, NUBINT sz, char *buffer)
{
  rnub_closest_symbol_name(((LPNUB_CONNECTION) nub)->RpcHandle, sz, buffer);
}

NUBINT proxy_nub_find_symbol_in_library
  (NUB nub, NUBLIBRARY nublibrary, NUBINT sz,
   char *name,
   TARGET_ADDRESS *address, NUBINT *type,
   NUBINT *is_function,
   TARGET_ADDRESS *debug_start, TARGET_ADDRESS *debug_end,
   NUBINT *symbol_language,
   TARGET_ADDRESS *final_address)
{
  RTARGET_ADDRESS   rem_address, rem_debug_start, rem_debug_end,
                    rem_final_address;
  NUBINT           result =
                     rnub_find_symbol_in_library
                       (((LPNUB_CONNECTION) nub)->RpcHandle,
                        REMOTIZE_NUBLIBRARY( nublibrary ),
                        sz,
                        name,
                        &rem_address,
                        type,
                        is_function,
                        &rem_debug_start, &rem_debug_end,
                        symbol_language,
                        &rem_final_address);
  (*address) = LOCALIZE_RTARGET_ADDRESS( rem_address );
  (*debug_start) = LOCALIZE_RTARGET_ADDRESS( rem_debug_start );
  (*debug_end) = LOCALIZE_RTARGET_ADDRESS( rem_debug_end );
  (*final_address) = LOCALIZE_RTARGET_ADDRESS( rem_final_address );
  return(result);
}

void proxy_nub_dispose_lookups
  (NUB nub, NUBHANDLE lookups)
{
  rnub_dispose_lookups(((LPNUB_CONNECTION) nub)->RpcHandle,
                       REMOTIZE_NUBHANDLE( lookups ));
}

TARGET_ADDRESS proxy_nub_resolve_source_location
   (NUB nub, NUBLIBRARY nublibrary, char *filename,
    NUBINT line_number, NUBINT column_number,
    NUBINT *valid, NUBINT *path, NUBHANDLE *search,
    NUBINT *exact)
{
  RTARGET_ADDRESS rem_address;
  RNUBHANDLE      rem_search;

  rem_address = rnub_resolve_source_location
                    (((LPNUB_CONNECTION) nub)->RpcHandle,
                     REMOTIZE_NUBLIBRARY( nublibrary ),
                     filename,
                     line_number,
                     column_number,
                     valid,
                     path,
                     &rem_search,
                     exact);
  (*search) = LOCALIZE_RNUBHANDLE( rem_search );
  return(LOCALIZE_RTARGET_ADDRESS( rem_address ));
}

NUBHANDLE proxy_nub_fetch_source_locations
  (NUB nub, TARGET_ADDRESS lower, TARGET_ADDRESS upper)
{
  RNUBHANDLE rem_table =
    rnub_fetch_source_locations(((LPNUB_CONNECTION) nub)->RpcHandle,
                                 REMOTIZE_TARGET_ADDRESS( lower ),
                                 REMOTIZE_TARGET_ADDRESS( upper ));
  return(LOCALIZE_RNUBHANDLE( rem_table ));
}

NUBINT proxy_nub_source_location_address
  (NUB nub, NUBHANDLE table, NUB_INDEX index)
{
  return(rnub_source_location_address(((LPNUB_CONNECTION) nub)->RpcHandle,
                                      REMOTIZE_NUBHANDLE( table ),
                                      index));
}

NUBINT proxy_nub_source_location_linenumber
  (NUB nub, NUBHANDLE table, NUB_INDEX index)
{
  return(rnub_source_location_linenumber(((LPNUB_CONNECTION) nub)->RpcHandle,
                                         REMOTIZE_NUBHANDLE( table ),
                                         index));
}

NUBINT proxy_nub_source_location_filename_length
  (NUB nub, NUBHANDLE table)
{
  return(rnub_source_location_filename_length
           (((LPNUB_CONNECTION) nub)->RpcHandle,
            REMOTIZE_NUBHANDLE( table )));
}

void proxy_nub_source_location_filename
  (NUB nub, NUBHANDLE table, NUBINT sz, char *buf)
{
  rnub_source_location_filename(((LPNUB_CONNECTION) nub)->RpcHandle,
                                REMOTIZE_NUBHANDLE( table ),
                                sz,
                                buf);
}

NUBINT proxy_nub_number_of_source_locations
  (NUB nub, NUBHANDLE table)
{
  return(rnub_number_of_source_locations(((LPNUB_CONNECTION) nub)->RpcHandle,
                                         REMOTIZE_NUBHANDLE( table )));
}

void proxy_nub_dispose_source_locations
  (NUB nub, NUBHANDLE lookups)
{
  rnub_dispose_source_locations(((LPNUB_CONNECTION) nub)->RpcHandle,
                                REMOTIZE_NUBHANDLE( lookups ));
}

void proxy_nub_interpret_instruction_at_current_location
  (NUB nub, NUBTHREAD nubthread,
   NUBINT *flow, TARGET_ADDRESS *destination, NUBINT *sz)
{
  RTARGET_ADDRESS     rem_destination;
  rnub_interpret_instruction_at_current_location
    (((LPNUB_CONNECTION) nub)->RpcHandle,
     REMOTIZE_NUBTHREAD( nubthread ),
     flow,
     &rem_destination,
     sz);
  (*destination) = LOCALIZE_RTARGET_ADDRESS( rem_destination );
}

TARGET_ADDRESS proxy_calculate_step_into_destination
  (NUB nub, NUBTHREAD nubthread, NUBINT *flive, NUBINT *ok)
{
  RTARGET_ADDRESS     rem_address;
  rem_address = rnub_dylan_calculate_step_into
                    (((LPNUB_CONNECTION) nub)->RpcHandle,
                     REMOTIZE_NUBTHREAD( nubthread ),
                     flive,
                     ok);
  return(LOCALIZE_RTARGET_ADDRESS( rem_address ));
}

TARGET_ADDRESS proxy_nub_dylan_thread_environment_block_address
  (NUB nub, NUBTHREAD nubthread, NUBINT *valid)
{
  RTARGET_ADDRESS  rem_address;
  rem_address = 
    rnub_dylan_thread_environment_block_address
      (((LPNUB_CONNECTION) nub)->RpcHandle,
       REMOTIZE_NUBTHREAD( nubthread ),
       valid);
  return(LOCALIZE_RTARGET_ADDRESS( rem_address ));
}

NUBINT proxy_nub_dylan_thread_mv_buffer_live
  (NUB nub, NUBTHREAD nubthread)
{
  return(rnub_dylan_thread_mv_buffer_live
           (((LPNUB_CONNECTION) nub)->RpcHandle,
            REMOTIZE_NUBTHREAD( nubthread )));
}

NUBINT proxy_nub_older_stack_frame
  (NUB nub, TARGET_ADDRESS this_one, TARGET_ADDRESS than_this)
{
  return(rnub_older_stack_frame(((LPNUB_CONNECTION) nub)->RpcHandle,
                                 REMOTIZE_TARGET_ADDRESS( this_one ),
                                 REMOTIZE_TARGET_ADDRESS( than_this )));
}

TARGET_ADDRESS proxy_nub_dylan_current_function
  (NUB nub, NUBTHREAD nubthread)
{
  RTARGET_ADDRESS  rem_address;
  rem_address = 
    rnub_dylan_current_function(((LPNUB_CONNECTION) nub)->RpcHandle,
                                REMOTIZE_NUBTHREAD( nubthread ));
  return(LOCALIZE_RTARGET_ADDRESS( rem_address ));
}

NUBINT proxy_nub_perform_absolute_relocation
  (NUB nub, TARGET_ADDRESS address, TARGET_ADDRESS dest)
{
  return(rnub_perform_absolute_relocation
           (((LPNUB_CONNECTION) nub)->RpcHandle,
            REMOTIZE_TARGET_ADDRESS( address ),
            REMOTIZE_TARGET_ADDRESS( dest )));
}

NUBINT proxy_nub_perform_relative_relocation
  (NUB nub, TARGET_ADDRESS address, TARGET_ADDRESS dest)
{
  return(rnub_perform_relative_relocation
           (((LPNUB_CONNECTION) nub)->RpcHandle,
            REMOTIZE_TARGET_ADDRESS( address ),
            REMOTIZE_TARGET_ADDRESS( dest )));
}

/*
  The following three functions ALWAYS run in the local debugger nub, and
  are hence part of the "proxy" file.
*/

SERVER establish_connection_to_server 
   (DBG_TRANSPORT_INDEX protocol, char *net_addr, NUBINT *success)
{
  RPC_STATUS           status;
  LPSERVER_CONNECTION  server
       = (LPSERVER_CONNECTION) malloc (sizeof(SERVER_CONNECTION));
  int  i  = 0;

  server->TransportProtocol = protocol;
  while (net_addr[i] != '\0') {
    server->NetworkAddress[i] = net_addr[i];
    i++;
  }
  server->NetworkAddress[i] = '\0';

  /* Call the RPC library to generate a string encoding of the
     interface binding */

  status = RpcStringBindingCompose
             (CONNECTION_SERVER_UUID,
              dbg_transport_protocols[protocol].ProtSeqEncoding,
              net_addr,
              NULL,
              NULL,
              &(server->StringBinding));

  /* Deal with failure */

  if (status != RPC_S_OK) {
    debugger_nub_rpc_error
      ("establish_connection_to_server",
       "The generated string binding was not legal.");
    (*success) = (NUBINT) DBG_TRANSPORT_ILLEGAL_BINDING;
    RpcStringFree(&(server->StringBinding));
    free(server);
    return (NULL);
  }

  /* Call the RPC library to convert the string representation into
     an actual interface binding. */

  status = RpcBindingFromStringBinding(server->StringBinding,
                                       &(server->RpcHandle));

  /* Again, deal with failure. */

  if (status != RPC_S_OK) {
    debugger_nub_rpc_error
        ("establish_connection_to_server",
         "Failed to convert string binding to Rpc binding.");
    (*success) = (NUBINT) DBG_TRANSPORT_COULD_NOT_FIND_SERVER;
    RpcStringFree(&(server->StringBinding));
    free(server);
    return (NULL);
  }

  /* Otherwise, signal success and return a pointer to the server. */
  (*success) = (NUBINT) DBG_TRANSPORT_OK;
  return ((SERVER) server);
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
  LPNUB_CONNECTION     tether 
       = (LPNUB_CONNECTION) malloc (sizeof(NUB_CONNECTION));
  RPC_STATUS           status;
  NUB                  process;
  LPSERVER_CONNECTION  debugger_connection
       = (LPSERVER_CONNECTION) server;
  DBG_TRANSPORT_INDEX  protocol = debugger_connection->TransportProtocol;
  int                  nub_spawn_success;

  /* STAGE 1:
     Call a function in the server to spawn a new instance of
     the debugger nub */

  RpcTryExcept {
    nub_spawn_success
      = open_new_debugger_nub(debugger_connection->RpcHandle,
                              protocol, 
                              MAX_ENDPOINT_NAME_SIZE, 
                              tether->EndpointName);
  }
  RpcExcept(1) {
    nub_spawn_success = 0;
  }
  RpcEndExcept;

  if (nub_spawn_success == 0) {
    debugger_nub_rpc_error
       ("open_remote_tether",
        "Rpc error instructing server to spawn debugger nub object");
    (*success) = (NUBINT) DBG_TRANSPORT_SERVER_SPAWN_NUB_FAILURE;
    RpcStringFree(&(tether->StringBinding));
    free(tether);
    return(NULL);
  }

  Sleep(1000);

  /* STAGE 2:
     The debugger nub should be spawned, and listening for our
     connection request. Now we connect. */

  status = RpcStringBindingCompose
    (DEBUGGER_NUB_UUID,
     dbg_transport_protocols[protocol].ProtSeqEncoding,
     debugger_connection->NetworkAddress,
     NULL,
     NULL,
     &(tether->StringBinding));

  if (status != RPC_S_OK) {
    debugger_nub_rpc_error
       ("open_remote_tether",
        "String binding was illegal.");
    (*success) = (NUBINT) DBG_TRANSPORT_ILLEGAL_BINDING;
    free(tether);
    return (NULL);
  }

  status = RpcBindingFromStringBinding(tether->StringBinding,
                                       &(tether->RpcHandle));

  if (status != RPC_S_OK) {
    debugger_nub_rpc_error
      ("open_remote_tether",
       "Failed to generate binding handle to debugger nub.");
    (*success) = (NUBINT) DBG_TRANSPORT_COULD_NOT_FIND_NUB;
    RpcStringFree(&(tether->StringBinding));
    free(tether);
    return (NULL);
  }

  /* STAGE 3:
     Call a function in the remote debugger nub to start 
     up our process. */

  process = proxy_nub_create_and_debug_process
     ((NUB) tether, 
       command_size, arg_size,
       command, arguments,
       sym_path_count, paths, lib_path_count, lib_paths, 
       workdirsz, working_directory,
       create_shell);

  if (process == NULL) {
    debugger_nub_rpc_error
       ("open_remote_tether",
        "Remote debugger nub did not spawn process");
    (*success) = (NUBINT) DBG_TRANSPORT_NUB_SPAWN_PROCESS_FAILURE;
    remote_debugger_nub_shutdown((NUB) tether);
    return(NULL);
  }

  (*success) = (NUBINT) DBG_TRANSPORT_OK;
  
  tether->ConnectionServer = debugger_connection;
  return ((NUB) tether);
}


NUB attach_remote_tether 
  (SERVER server, 
   NUBPROCESS processd,
   NUBINT name_length, char *name,
   NUBINT id_length, char *id,
   NUBINT path_count, char **paths,
   NUBINT jit_info_sz,
   char *jit_info,
   NUBINT *success)
{
  LPNUB_CONNECTION     tether 
       = (LPNUB_CONNECTION) malloc (sizeof(NUB_CONNECTION));
  RPC_STATUS           status;
  NUB                  process;
  LPSERVER_CONNECTION  debugger_connection
       = (LPSERVER_CONNECTION) server;
  DBG_TRANSPORT_INDEX  protocol = debugger_connection->TransportProtocol;
  int                  nub_spawn_success;
  DWORD                actual_id = 0xABCDABCD;  /* Recognisable failure value. */

  /* Firstly, we need the actual process ID, which will be held by the
     server */

  RpcTryExcept {
    actual_id = svr_server_process_actual_id
                  (debugger_connection->RpcHandle,
                   REMOTIZE_NUBPROCESS( processd ));
  }
  RpcExcept(1) {
    debugger_nub_rpc_error
       ("attach_remote_tether",
        "Failed to get the true process ID from the server.");
  }
  RpcEndExcept;

  /* STAGE 1:
     Call a function in the server to spawn a new instance of
     the debugger nub */

  RpcTryExcept {
    nub_spawn_success
      = open_new_debugger_nub(debugger_connection->RpcHandle,
                              protocol, 
                              MAX_ENDPOINT_NAME_SIZE, 
                              tether->EndpointName);
  }
  RpcExcept(1) {
    nub_spawn_success = 0;
  }
  RpcEndExcept;

  if (nub_spawn_success == 0) {
    debugger_nub_rpc_error
       ("attach_remote_tether",
        "Rpc error instructing server to spawn debugger nub object");
    (*success) = (NUBINT) DBG_TRANSPORT_SERVER_SPAWN_NUB_FAILURE;
    RpcStringFree(&(tether->StringBinding));
    free(tether);
    return(NULL);
  }

  Sleep(1000);

  /* STAGE 2:
     The debugger nub should be spawned, and listening for our
     connection request. Now we connect. */

  status = RpcStringBindingCompose
    (DEBUGGER_NUB_UUID,
     dbg_transport_protocols[protocol].ProtSeqEncoding,
     debugger_connection->NetworkAddress,
     NULL,
     NULL,
     &(tether->StringBinding));

  if (status != RPC_S_OK) {
    debugger_nub_rpc_error
       ("attach_remote_tether",
        "String binding was illegal.");
    (*success) = (NUBINT) DBG_TRANSPORT_ILLEGAL_BINDING;
    free(tether);
    return (NULL);
  }

  status = RpcBindingFromStringBinding(tether->StringBinding,
                                       &(tether->RpcHandle));

  if (status != RPC_S_OK) {
    debugger_nub_rpc_error
      ("attach_remote_tether",
       "Failed to generate binding handle to debugger nub.");
    (*success) = (NUBINT) DBG_TRANSPORT_COULD_NOT_FIND_NUB;
    RpcStringFree(&(tether->StringBinding));
    free(tether);
    return (NULL);
  }

  /* STAGE 3:
     Call a function in the remote debugger nub to start 
     up our process. */

  process = proxy_nub_debug_active_process
     ((NUB) tether, 
       name_length, name,
       id_length, id,
       actual_id,
       path_count, paths, 
       jit_info_sz, jit_info);

  if (process == NULL) {
    debugger_nub_rpc_error
       ("attach_remote_tether",
        "Remote debugger nub did not attach to process");
    (*success) = (NUBINT) DBG_TRANSPORT_NUB_SPAWN_PROCESS_FAILURE;
    remote_debugger_nub_shutdown((NUB) tether);
    return(NULL);
  }

  (*success) = (NUBINT) DBG_TRANSPORT_OK;
  
  tether->ConnectionServer = debugger_connection;
  return ((NUB) tether);
}


void remote_debugger_nub_shutdown
  (NUB nub)
{
  LPNUB_CONNECTION         tether = (LPNUB_CONNECTION) nub;
  rnub_close_remote_tether(tether->RpcHandle);
  RpcStringFree(&(tether->StringBinding));
  RpcBindingFree(&(tether->RpcHandle));
  free(tether);
}

/*
 *    ----------------------- SERVER PROXY FUNCTIONS -----------------------
 */

NUBINT server_get_hostname_length (SERVER server, NUBINT *success)
{
  NUBINT                 result = (NUBINT) 0;
  LPSERVER_CONNECTION    connection = (LPSERVER_CONNECTION) server;
  RpcTryExcept {
    result = (NUBINT) svr_server_get_hostname_length
                        (connection->RpcHandle);
    (*success) = 1;
  }
  RpcExcept(1) {
    result = (NUBINT) 0;
    debugger_nub_rpc_error
      ("server_get_hostname_length", "RPC Exception occured.");
    (*success) = 0;
  }
  RpcEndExcept;
  return(result);
}

void server_get_hostname
  (SERVER server, NUBINT buf_size, char *buf)
{
  LPSERVER_CONNECTION      connection = (LPSERVER_CONNECTION) server;
  RpcTryExcept {
    svr_server_get_hostname
      (connection->RpcHandle, buf_size, buf);
  }
  RpcExcept(1) {
    debugger_nub_rpc_error
      ("server_get_hostname", "Rpc Exception occured.");
    buf[0] = '\0';
  }
  RpcEndExcept;
}

NUBINT server_verify_password
  (SERVER server, NUBINT buf_size, char *buf)
{
  LPSERVER_CONNECTION     connection = (LPSERVER_CONNECTION) server;
  NUBINT                  verified = 0;
  RpcTryExcept {
    verified = svr_server_verify_password(connection->RpcHandle,
                                          buf_size + 1,
                                          buf);
  }
  RpcExcept(1) {
    debugger_nub_rpc_error
      ("server_verify_password", "Rpc exception occured.");
    verified = 0;
  }
  RpcEndExcept;
  return(verified);
}

NUBINT update_server_process_list
  (SERVER server)
{
  LPSERVER_CONNECTION      connection = (LPSERVER_CONNECTION) server;
  NUBINT                   count = 0;
  RpcTryExcept {
    count = svr_update_server_process_list(connection->RpcHandle);
  }
  RpcExcept(1) {
    debugger_nub_rpc_error
      ("server_update_process_list", "Rpc Exception occured.");
    count = 0;
  }
  RpcEndExcept;
  return(count);
}

void server_release_connection
  (SERVER server)
{
  LPSERVER_CONNECTION     connection = (LPSERVER_CONNECTION) server;
  RpcBindingFree(&(connection->RpcHandle));
  RpcStringFree(&(connection->StringBinding));
  free(connection);
}

NUBPROCESS server_process_nub_descriptor
  (SERVER server, NUB_INDEX i)
{
  LPSERVER_CONNECTION      connection = (LPSERVER_CONNECTION) server;
  RNUBPROCESS              remote_process;
  RpcTryExcept {
    remote_process = svr_server_process_nub_descriptor
       (connection->RpcHandle, i);
  }
  RpcExcept(1) {
    debugger_nub_rpc_error
      ("server_process_nub_descriptor", "Rpc Exception occured.");
  }
  RpcEndExcept;
  return(LOCALIZE_RNUBPROCESS( remote_process ));
}

NUBINT server_process_name_length
  (SERVER server, NUB_INDEX i)
{
  LPSERVER_CONNECTION      connection = (LPSERVER_CONNECTION) server;
  NUBINT                   length = 0;
  RpcTryExcept {
    length = svr_server_process_name_length
      (connection->RpcHandle, i);
  }
  RpcExcept(1) {
    debugger_nub_rpc_error
      ("server_process_name_length", "Rpc Exception occured.");
    length = 0;
  }
  RpcEndExcept;
  return(length);
}

void server_process_name
  (SERVER server, NUB_INDEX i, NUBINT buf_size, char *buf)
{
  LPSERVER_CONNECTION      connection = (LPSERVER_CONNECTION) server;
  RpcTryExcept {
    svr_server_process_name
      (connection->RpcHandle, i, buf_size, buf);
  }
  RpcExcept(1) {
    debugger_nub_rpc_error
       ("server_process_name", "Rpc Exception occured.");
    buf[0] = '\0';
  }
  RpcEndExcept;
}

NUBINT server_process_system_identifier_length
  (SERVER server, NUB_INDEX i)
{
  LPSERVER_CONNECTION      connection = (LPSERVER_CONNECTION) server;
  NUBINT                   length = 0;
  RpcTryExcept {
    length = svr_server_process_system_identifier_length
      (connection->RpcHandle, i);
  }
  RpcExcept(1) {
    debugger_nub_rpc_error
      ("server_process_system_identifier_length", "Rpc Exception occured.");
    length = 0;
  }
  RpcEndExcept;
  return(length);
}

void server_process_system_identifier
  (SERVER server, NUB_INDEX i, NUBINT buf_size, char *buf)
{
  LPSERVER_CONNECTION      connection = (LPSERVER_CONNECTION) server;
  RpcTryExcept {
    svr_server_process_system_identifier
      (connection->RpcHandle, i, buf_size, buf);
  }
  RpcExcept(1) {
    debugger_nub_rpc_error
       ("server_process_system_identifier", "Rpc Exception occured.");
    buf[0] = '\0';
  }
  RpcEndExcept;
}

NUBINT server_shutdown
  (SERVER server)
{
  LPSERVER_CONNECTION      connection = (LPSERVER_CONNECTION) server;
  RpcTryExcept {
    svr_server_shutdown(connection->RpcHandle);
  }
  RpcExcept(1) {
    debugger_nub_rpc_error
      ("server_shutdown", "Rpc Exception occured.");
    return(0);
  }
  RpcEndExcept;

  RpcStringFree(&(connection->StringBinding));
  RpcBindingFree(&(connection->RpcHandle));
  free(connection);
  return(DBG_TRANSPORT_OK);
}
