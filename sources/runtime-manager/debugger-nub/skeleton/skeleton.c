#include "nub-core.h"
#include <stdio.h>

// Temporary C primitives to strip/add tags for conversion between
// <remote-value> and <integer> or <character>. These won't make
// up part of the nub API in the end, because these manipulations
// can be done on dylan <machine-word>s

NUBINT nub_primitive_select_low_order_bits
  (TARGET_ADDRESS addr, NUBINT i)
{
  printf("NYI: nub_primitive_select_low_order_bits\n");
  return 0;
}

TARGET_ADDRESS
  nub_primitive_indexed_remote_value
    (TARGET_ADDRESS base, NUBINT i)
{
  printf("NYI: nub_primitive_indexed_remote_value\n");
  return 0;
}


TARGET_ADDRESS
  nub_primitive_byte_indexed_remote_value
    (TARGET_ADDRESS base, NUBINT i)
{
  printf("NYI: nub_primitive_byte_indexed_remote_value\n");
  return 0;
}


TARGET_ADDRESS
  nub_primitive_tagged_value_as_integer
    (TARGET_ADDRESS x)
{
  printf("NYI: nub_primitive_tagged_value_as_integer\n");
  return 0;
}


char
  nub_primitive_tagged_value_as_character
    (TARGET_ADDRESS x)
{
  printf("NYI: nub_primitive_tagged_value_as_character\n");
  return 0;
}

NUBINT nub_primitive_remote_value_as_integer_losing_precision
  (TARGET_ADDRESS value)
{
  printf("NYI: nub_primitive_remote_value_as_integer_losing_precision\n");
  return 0;
}


TARGET_ADDRESS nub_primitive_integer_as_remote_value_losing_precision
  (NUBINT value)
{
  printf("NYI: nub_primitive_integer_as_remote_value_losing_precision\n");
  return 0;
}

TARGET_ADDRESS
  nub_primitive_integer_as_tagged_value
    (NUBINT i)
{
  printf("NYI: nub_primitive_integer_as_tagged_value\n");
  return 0;
}


TARGET_ADDRESS
  nub_primitive_character_as_tagged_value
    (char c)
{
  printf("NYI: nub_primitive_character_as_tagged_value\n");
  return 0;
}


// Debugger nub API

// Process creation and tethering.

// open_local_tether
// Calls the local debugger nub to create a new target process. Returns
// a NUB descriptor which must subsequently be passed to all requests
// to the local nub to perform transactions on that process.

NUB
  open_local_tether
    (char *command,          // IN: The name and relative path of the EXE
     char *arguments,        // IN: The additional command line to pass.
     NUBINT sym_path_count,  // IN: The number of symbol search paths.
     char **paths,           // IN: The symbol search paths.
     NUBINT lib_path_count,  // IN: The number of DLL search paths.
     char **lib_paths,       // IN: The DLL search paths themselves.
     char *workdir,          // IN: The working directory pathname.
     NUBINT create_shell,    // IN: nonzero means allow process to create its
                             //     own shell or console window.
     NUBINT *success         // OUT: 1 indicates success, 0 indicates failure.
  )
{
  printf("NYI: open_local_tether\n");
  return 0;
}


NUB attach_local_tether
  (NUBPROCESS process, NUBINT path_count, char **paths, char *jit,
   NUBINT *success)
{
  printf("NYI: attach_local_tether\n");
  return 0;
}

NUBINT nub_remote_value_byte_size
  (NUB nub)
{
  printf("NYI: nub_remote_value_byte_size\n");
  return 0;
}

NUBINT nub_get_process_page_fault_count
  (NUB nub)
{
  printf("NYI: nub_get_process_page_fault_count\n");
  return 0;
}

NUBINT nub_thread_os_priority
  (NUB nub,  NUBTHREAD nubthread)
{
  printf("NYI: nub_thread_os_priority\n");
  return 0;
}

NUBINT nub_get_thread_cpu_time
  (NUB nub,  NUBTHREAD nubthread)
{
  printf("NYI: nub_get_thread_cpu_time\n");
  return 0;
}


TARGET_ADDRESS nub_get_library_base_address
  (NUB nub, NUBLIBRARY dll)
{
  printf("NYI: nub_get_library_base_address\n");
  return 0;
}

void nub_get_library_version
  (NUB nub,  NUBLIBRARY dll,  NUBINT *maj,  NUBINT *min)
{
  printf("NYI: nub_get_library_version\n");
  *maj = 0;
  *min = 0;
}

NUBINT nub_get_library_filename_length
  (NUB nub,  NUBLIBRARY dll)
{
  printf("NYI: nub_get_library_filename_length\n");
  return 0;
}

void nub_get_library_filename
  (NUB nub,  NUBLIBRARY dll,  NUBINT sz,
    char *buf)
{
  printf("NYI: nub_get_library_filename\n");
}

NUBINT nub_get_library_undecorated_name_length
  (NUB nub,  NUBLIBRARY dll)
{
  printf("NYI: nub_get_library_undecorated_name_length\n");
  return 0;
}

void nub_get_library_undecorated_name
  (NUB nub,  NUBLIBRARY dll,  NUBINT sz,
    char *buf)
{
  printf("NYI: nub_get_library_undecorated_name\n");
}


NUBINT nub_get_register_name_length
  (NUB nub,  NUB_INDEX reg)
{
  printf("NYI: nub_get_register_name_length\n");
  return 0;
}

void nub_get_register_name
  (NUB nub,  NUB_INDEX reg,  NUBINT sz,
    char *buf)
{
  printf("NYI: nub_get_register_name\n");
}

NUBINT nub_get_register_enumeration_code
  (NUB nub,   NUB_INDEX reg)
{
  printf("NYI: nub_get_register_enumeration_code\n");
  return 0;
}

void nub_all_registers
  (NUB nub,  NUBINT *first,  NUBINT *last)
{
  printf("NYI: nub_all_registers\n");
  *first = *last = 0;
}

void nub_general_registers
  (NUB nub,  NUBINT *first,  NUBINT *last)
{
  printf("NYI: nub_general_registers\n");
  *first = *last = 0;
}

void nub_special_registers
  (NUB nub,  NUBINT *first,  NUBINT *last)
{
  printf("NYI: nub_special_registers\n");
  *first = *last = 0;
}

void nub_floating_registers
  (NUB nub,  NUBINT *first,  NUBINT *last)
{
  printf("NYI: nub_floating_registers\n");
  *first = *last = 0;
}


NUBINT nub_page_read_permission
  (NUB nub,  TARGET_ADDRESS address)
{
  printf("NYI: nub_page_read_permission\n");
  return 0;
}

NUBINT nub_page_write_permission
  (NUB nub,  TARGET_ADDRESS address)
{
  printf("NYI: nub_page_write_permission\n");
  return 0;
}


NUBINT nub_page_relative_address
  (NUB nub,  TARGET_ADDRESS address,  NUBINT *offset)
{
  printf("NYI: nub_page_relative_address\n");
  return 0;
}

NUBINT nub_virtual_page_size
  (NUB nub)
{
  printf("NYI: nub_virtual_page_size\n");
  return 0;
}


TARGET_ADDRESS read_value_from_process_memory
   (NUB nub,  TARGET_ADDRESS address,  NUB_ERROR *status)
{
  printf("NYI: read_value_from_process_memory\n");
  return 0;
}

void write_value_to_process_memory
   (NUB nub,  TARGET_ADDRESS address,  TARGET_ADDRESS val,
     NUBINT *status)
{
  printf("NYI: write_value_to_process_memory\n");
}


TARGET_ADDRESS nub_calculate_stack_address
  (NUB nub,  NUBTHREAD nubthread,  NUBINT offset)
{
  printf("NYI: nub_calculate_stack_address\n");
  return 0;
}


void nub_target_address_to_string
  (NUB nub,  TARGET_ADDRESS x,  NUBINT sz,
    char *buffer,  NUBINT radix,  NUBINT pad,
    NUBINT *truncated)
{
  printf("NYI: nub_target_address_to_string\n");
}

TARGET_ADDRESS nub_string_to_target_address
  (NUB nub,  NUBINT sz, char *buffer,
    NUBINT radix,
    NUBINT *overflow)
{
  printf("NYI: nub_string_to_target_address\n");
  return 0;
}


FLOAT read_single_float_from_process_memory
  (NUB nub,  TARGET_ADDRESS address,  NUB_ERROR *status)
{
  printf("NYI: read_single_float_from_process_memory\n");
  return 0;
}

void write_single_float_to_process_memory
  (NUB nub,  TARGET_ADDRESS address,  FLOAT value,
    NUB_ERROR *status)
{
  printf("NYI: write_single_float_to_process_memory\n");
}

DOUBLE read_double_float_from_process_memory
  (NUB nub,  TARGET_ADDRESS address,  NUB_ERROR *status)
{
  printf("NYI: read_double_float_from_process_memory\n");
  return 0;
}

void write_double_float_to_process_memory
  (NUB nub,  TARGET_ADDRESS address,  DOUBLE value,
    NUB_ERROR *status)
{
  printf("NYI: write_double_float_to_process_memory\n");
}

void read_byte_string_from_process_memory
  (NUB nub,  TARGET_ADDRESS address,  NUBINT sz,
   char *buffer,  NUB_ERROR *status)
{
  printf("NYI: read_byte_string_from_process_memory\n");
}

void write_byte_string_to_process_memory
  (NUB nub,  TARGET_ADDRESS address,  NUBINT sz,
   char *buffer,  NUB_ERROR *status)
{
  printf("NYI: write_byte_string_to_process_memory\n");
}


TARGET_ADDRESS read_value_from_process_register_in_stack_frame
  (NUB nub,  NUBTHREAD nubthread,  NUB_INDEX reg,
    NUB_INDEX frame_index,  NUB_ERROR *status)
{
  printf("NYI: read_value_from_process_register_in_stack_frame\n");
  return 0;
}


TARGET_ADDRESS read_value_from_process_register
  (NUB nub,  NUBTHREAD nubthread,  NUB_INDEX reg,
    NUB_ERROR *status)
{
  printf("NYI: read_value_from_process_register\n");
  return 0;
}

void write_value_to_process_register
  (NUB nub,  NUBTHREAD nubthread,  NUB_INDEX reg,
    TARGET_ADDRESS value,  NUB_ERROR *status)
{
  printf("NYI: write_value_to_process_register\n");
}

FLOAT read_single_float_from_process_register
  (NUB nub,  NUBTHREAD nubthread,  NUB_INDEX reg,
    NUB_ERROR *status)
{
  printf("NYI: read_single_float_from_process_register\n");
  return 0;
}

void write_single_float_to_process_register
  (NUB nub,  NUBTHREAD nubthread,  NUB_INDEX reg,
    FLOAT value,  NUB_ERROR *status)
{
  printf("NYI: write_single_float_to_process_register\n");
}

DOUBLE read_double_float_from_process_register
  (NUB nub,  NUBTHREAD nubthread,  NUB_INDEX reg,
    NUB_ERROR *status)
{
  printf("NYI: read_double_float_from_process_register\n");
  return 0;
}

void write_double_float_to_process_register
  (NUB nub,  NUBTHREAD nubthread,  NUB_INDEX reg,
    DOUBLE value,  NUB_ERROR *status)
{
  printf("NYI: write_double_float_to_process_register\n");
}


void nub_application_restart
  (NUB nub)
{
  printf("NYI: nub_application_restart\n");
}

void nub_application_stop
  (NUB nub)
{
  printf("NYI: nub_application_stop\n");
}

void nub_application_continue
  (NUB nub)
{
  printf("NYI: nub_application_continue\n");
}

void nub_application_continue_unhandled
  (NUB nub)
{
  printf("NYI: nub_application_continue_unhandled\n");
}

void nub_application_step
  (NUB nub,  NUBINT n)
{
  printf("NYI: nub_application_step\n");
}

void nub_application_step_over
  (NUB nub,  NUBINT n)
{
  printf("NYI: nub_application_step_over\n");
}

void nub_application_step_out
  (NUB nub)
{
  printf("NYI: nub_application_step_out\n");
}

void nub_register_exit_process_function
  (NUB nub, TARGET_ADDRESS ExitProcess)
{
  printf("NYI: nub_register_exit_process_function\n");
}

void nub_threads_continue
  (NUB nub)
{
  printf("NYI: nub_threads_continue\n");
}

void nub_recover_breakpoint
  (NUB nub, NUBTHREAD thread)
{
  printf("NYI: nub_recover_breakpoint\n");
}

NUB_ERROR nub_set_stepping_control_on_thread
  (NUB nub,  NUBTHREAD nubthread,
    TARGET_ADDRESS fp,  TARGET_ADDRESS calling_fp,
    NUBINT location_count,
    TARGET_ADDRESS *locs,
    NUBINT operation)
{
  printf("NYI: nub_set_stepping_control_on_thread\n");
  return 0;
}

NUB_ERROR nub_clear_stepping_control_on_thread
  (NUB nub,  NUBTHREAD nubthread)
{
  printf("NYI: nub_clear_stepping_control_on_thread\n");
  return 0;
}


void nub_thread_stop
  (NUB nub,  NUBTHREAD nubthread)
{
  printf("NYI: nub_thread_stop\n");
}

void nub_thread_continue
  (NUB nub,  NUBTHREAD nubthread)
{
  printf("NYI: nub_thread_continue\n");
}

void nub_thread_suspended (NUBTHREAD thread)
{
  printf("NYI: nub_thread_suspended\n");
}

bool nub_thread_suspendedQ (NUBTHREAD thread)
{
  printf("NYI: nub_thread_suspendedQ\n");
  return 0;
}

void nub_thread_resumed (NUBTHREAD thread)
{
  printf("NYI: nub_thread_resumed\n");
}

NUB_ERROR nub_kill_application
  (NUB nub)
{
  printf("NYI: nub_kill_application\n");
  return 0;
}

void nub_close_application
  (NUB nub)
{
  printf("NYI: nub_close_application\n");
}

void remote_debugger_nub_shutdown
  (NUB nub)
{
  printf("NYI: remote_debugger_nub_shutdown\n");
}

void nub_debug_message
  (char* message, TARGET_ADDRESS addr1, TARGET_ADDRESS addr2)
{
  printf("NYI: nub_debug_message\n");
}



TARGET_ADDRESS nub_setup_function_call
  (NUB nub,  NUBTHREAD nubthread,  TARGET_ADDRESS func,
    NUBINT arg_count, TARGET_ADDRESS *args,
    NUBHANDLE *cx_handle)
{
  printf("NYI: nub_setup_function_call\n");
  return 0;
}

TARGET_ADDRESS nub_remote_call_spy
  (NUB nub,  NUBTHREAD nubthread,  TARGET_ADDRESS func,
    NUBINT arg_count, TARGET_ADDRESS *args,
    NUB_ERROR *status)
{
  printf("NYI: nub_remote_call_spy\n");
  return 0;
}

TARGET_ADDRESS nub_get_function_result
  (NUB nub,  NUBTHREAD nubthread)
{
  printf("NYI: nub_get_function_result\n");
  return 0;
}

void nub_restore_context
  (NUB nub,  NUBTHREAD nubthread,  NUBHANDLE context)
{
  printf("NYI: nub_restore_context\n");
}


NUB_ERROR nub_set_breakpoint
  (NUB nub,  TARGET_ADDRESS address)
{
  printf("NYI: nub_set_breakpoint\n");
  return 0;
}

NUB_ERROR nub_clear_breakpoint
  (NUB nub,  TARGET_ADDRESS address)
{
  printf("NYI: nub_clear_breakpoint\n");
  return 0;
}

NUBINT nub_query_breakpoint
  (NUB nub,  TARGET_ADDRESS address)
{
  printf("NYI: nub_query_breakpoint\n");
  return 0;
}


void nub_wait_for_stop_reason_with_timeout
  (NUB nub,  NUBINT timeout,  NUBINT *code)
{
  printf("NYI: nub_wait_for_stop_reason_with_timeout\n");
}

void nub_profile_wait_for_stop_reason_with_timeout
  (NUB nub,  NUBINT timeout,  NUBINT profiling_interval,
    NUBINT *code)
{
  printf("NYI: nub_profile_wait_for_stop_reason_with_timeout\n");
}

void nub_inform_profiling_started
  (NUB nub)
{
  printf("NYI: nub_inform_profiling_started\n");
}

void nub_inform_profiling_stopped
  (NUB nub)
{
  printf("NYI: nub_inform_profiling_stopped\n");
}

NUBINT nub_get_process_wall_clock_time (NUB nub)
{
  printf("NYI: nub_get_process_wall_clock_time\n");
  return 0;
}

NUBINT nub_can_receive_first_chance
  (NUB nub,  NUBINT ecode)
{
  printf("NYI: nub_can_receive_first_chance\n");
  return 0;
}

void nub_set_first_chance
  (NUB nub,  NUBINT ecode)
{
  printf("NYI: nub_set_first_chance\n");
}

void nub_unset_first_chance
  (NUB nub,  NUBINT ecode)
{
  printf("NYI: nub_unset_first_chance\n");
}

NUBINT nub_thread_stop_information
  (NUB nub,  NUBTHREAD nubthread,
    NUBINT *fchance,  NUBINT *fstart,
    TARGET_ADDRESS *ret_addr)
{
  printf("NYI: nub_thread_stop_information\n");
  return 0;
}

void nub_wait_for_stop_reason_no_timeout
  (NUB nub,  NUBINT *ecode)
{
  printf("NYI: nub_wait_for_stop_reason_no_timeout\n");
}

void nub_profile_wait_for_stop_reason_no_timeout
  (NUB nub,  NUBINT profile_interval,  NUBINT *ecode)
{
  printf("NYI: nub_profile_wait_for_stop_reason_no_timeout\n");
}

NUB nub_stop_reason_process
  (NUB nub)
{
  printf("NYI: nub_stop_reason_process\n");
  return 0;
}

NUBTHREAD nub_stop_reason_thread
  (NUB nub)
{
  printf("NYI: nub_stop_reason_thread\n");
  return 0;
}

NUBINT nub_first_hard_coded_breakpoint
  (NUB nub)
{
  printf("NYI: nub_first_hard_coded_breakpoint\n");
  return 0;
}

NUBINT nub_stop_reason_process_exit_code
  (NUB nub)
{
  printf("NYI: nub_stop_reason_process_exit_code\n");
  return 0;
}

NUBINT nub_stop_reason_thread_exit_code
  (NUB nub)
{
  printf("NYI: nub_stop_reason_thread_exit_code\n");
  return 0;
}

NUBLIBRARY nub_stop_reason_library
  (NUB nub)
{
  printf("NYI: nub_stop_reason_library\n");
  return 0;
}

NUBINT nub_stop_reason_violation_op
  (NUB nub)
{
  printf("NYI: nub_stop_reason_library\n");
  return 0;
}

NUBINT nub_exception_first_chance
  (NUB nub)
{
  printf("NYI: nub_exception_first_chance\n");
  return 0;
}

TARGET_ADDRESS nub_stop_reason_violation_address
  (NUB nub)
{
  printf("NYI: nub_stop_reason_violation_address\n");
  return 0;
}

TARGET_ADDRESS nub_stop_reason_exception_address
  (NUB nub)
{
  printf("NYI: nub_stop_reason_exception_address\n");
  return 0;
}

TARGET_ADDRESS nub_stop_reason_debug_string_address
  (NUB nub)
{
  printf("NYI: nub_stop_reason_debug_string_address\n");
  return 0;
}

NUBINT nub_stop_reason_debug_string_length
  (NUB nub)
{
  printf("NYI: nub_stop_reason_debug_string_length\n");
  return 0;
}

NUBINT nub_stop_reason_debug_string_is_unicode
  (NUB nub)
{
  printf("NYI: nub_stop_reason_debug_string_is_unicode\n");
  return 0;
}


NUBINT nub_initialize_stack_vectors
  (NUB nub,  NUBTHREAD nubthread)
{
  printf("NYI: nub_initialize_stack_vectors\n");
  return 0;
}

void nub_read_stack_vectors
  (NUB nub,  NUBTHREAD nubthread,  NUBINT frame_count,
   TARGET_ADDRESS *frame_pointers,
   TARGET_ADDRESS *instruction_pointers,
   TARGET_ADDRESS *return_addresses)
{
  printf("NYI: nub_read_stack_vectors\n");
}

void nub_all_frame_lexicals
  (NUB nub,  TARGET_ADDRESS frame,  TARGET_ADDRESS ip,
    NUB_INDEX *first,  NUB_INDEX *last,
    NUBHANDLE *table)
{
  printf("NYI: nub_all_frame_lexicals\n");
}

void nub_register_interactive_code_segment
  (NUB nub,  TARGET_ADDRESS lo,  TARGET_ADDRESS hi)
{
  printf("NYI: nub_register_interactive_code_segment\n");
}

NUBINT nub_get_lexical_variable_name_length
  (NUB nub,  NUBHANDLE table,  NUB_INDEX variable)
{
  printf("NYI: nub_get_lexical_variable_name_length\n");
  return 0;
}

void nub_get_lexical_variable_name
  (NUB nub,  NUBHANDLE table,  NUB_INDEX variable,
    NUBINT sz,  char *buffer)
{
  printf("NYI: nub_get_lexical_variable_name\n");
}

TARGET_ADDRESS nub_lexical_variable_address
  (NUB nub,  TARGET_ADDRESS fp, NUBHANDLE table,  NUB_INDEX variable,
    NUBINT *in_reg,  NUB_INDEX *hireg,  NUB_INDEX *loreg,
    NUBINT *arg)
{
  printf("NYI: nub_lexical_variable_address\n");
  return 0;
}


NUBINT nub_lookup_symbol_name_length
  (NUB nub,  NUBHANDLE table,  NUB_INDEX sym)
{
  printf("NYI: nub_lookup_symbol_name_length\n");
  return 0;
}

void nub_lookup_symbol_name
  (NUB nub,  NUBHANDLE table,  NUB_INDEX sym,
    NUBINT sz,  char *buffer)
{
  printf("NYI: nub_lookup_symbol_name\n");
}

TARGET_ADDRESS nub_lookup_symbol_address
  (NUB nub,  NUBHANDLE table,  NUB_INDEX sym)
{
  printf("NYI: nub_lookup_symbol_address\n");
  return 0;
}

TARGET_ADDRESS nub_lookup_function_debug_start
  (NUB nub,  NUBHANDLE table,  NUB_INDEX sym)
{
  printf("NYI: nub_lookup_function_debug_start\n");
  return 0;
}

TARGET_ADDRESS nub_lookup_function_debug_end
  (NUB nub,  NUBHANDLE table,  NUB_INDEX sym)
{
  printf("NYI: nub_lookup_function_debug_end\n");
  return 0;
}

NUBINT nub_lookup_symbol_language
  (NUB nub,  NUBHANDLE table,  NUB_INDEX sym)
{
  printf("NYI: nub_lookup_symbol_language\n");
  return 0;
}

NUBINT nub_lookup_symbol_language_code
  (NUB nub,  NUBHANDLE table,  NUB_INDEX index)
{
  printf("NYI: nub_lookup_symbol_language_code\n");
  return 0;
}

TARGET_ADDRESS nub_lookup_function_end
  (NUB nub,  NUBHANDLE table,  NUB_INDEX sym)
{
  printf("NYI: nub_lookup_function_end\n");
  return 0;
}

NUBINT nub_symbol_is_function
  (NUB nub,  NUBHANDLE table,  NUB_INDEX sym)
{
  printf("NYI: nub_symbol_is_function\n");
  return 0;
}

NUBINT nub_nearest_symbols
  (NUB nub,  TARGET_ADDRESS address,
    NUBLIBRARY *lib,  NUBHANDLE *table)
{
  printf("NYI: nub_nearest_symbols\n");
  return 0;
}

NUBINT nub_closest_symbol
  (NUB nub,  TARGET_ADDRESS address,
    NUBLIBRARY *lib,
    TARGET_ADDRESS *actual_address,
    NUBINT *offset,
    NUBINT *name_length,
    NUBINT *type,
    NUBINT *is_function,
    TARGET_ADDRESS *debug_start,
    TARGET_ADDRESS *debug_end,
    NUBINT *language,
    TARGET_ADDRESS *final_address_of_definition)
{
  printf("NYI: nub_closest_symbol\n");
  return 0;
}

void nub_function_bounding_addresses
  (NUB nub,  TARGET_ADDRESS address,
    TARGET_ADDRESS *lower,  TARGET_ADDRESS *upper)
{
  printf("NYI: nub_function_bounding_addresses\n");
}

void nub_closest_symbol_name
  (NUB nub,  NUBINT sz,
    char *buffer)
{
  printf("NYI: nub_closest_symbol_name\n");
}

NUBINT nub_find_symbol_in_library
  (NUB nub,  NUBLIBRARY nublibrary,  NUBINT sz,
    char *name,
    TARGET_ADDRESS *address,
    NUBINT *type,
    NUBINT *is_function,
    TARGET_ADDRESS *debug_start,
    TARGET_ADDRESS *debug_end,
    NUBINT *symbol_language,
    TARGET_ADDRESS *final_address_of_definition)
{
  printf("NYI: nub_find_symbol_in_library\n");
  return 0;
}

void nub_dispose_lookups
  (NUB nub,  NUBHANDLE lookups)
{
  printf("NYI: nub_dispose_lookups\n");
}


TARGET_ADDRESS nub_resolve_source_location
  (NUB nub,  NUBLIBRARY nublibrary, char *filename,
    NUBINT line_number,  NUBINT column_number,
    NUBINT *valid,  NUBINT *path,  NUBHANDLE *search,
    NUBINT *exact)
{
  printf("NYI: nub_resolve_source_location\n");
  return 0;
}

NUBHANDLE nub_fetch_source_locations
  (NUB nub,
    TARGET_ADDRESS start_loc,  TARGET_ADDRESS end_loc)
{
  printf("NYI: nub_fetch_source_locations\n");
  return 0;
}

NUBINT nub_source_location_address
  (NUB nub,  NUBHANDLE table,  NUB_INDEX index)
{
  printf("NYI: nub_source_location_address\n");
  return 0;
}

NUBINT nub_source_location_linenumber
  (NUB nub,  NUBHANDLE table,  NUB_INDEX index)
{
  printf("NYI: nub_source_location_linenumber\n");
  return 0;
}

NUBINT nub_source_location_filename_length
  (NUB nub,  NUBHANDLE table)
{
  printf("NYI: nub_source_location_filename_length\n");
  return 0;
}

void nub_source_location_filename
  (NUB nub,  NUBHANDLE table,
    NUBINT sz,
    char *buffer)
{
  printf("NYI: nub_source_location_filename\n");
}

NUBINT nub_number_of_source_locations
  (NUB nub,  NUBHANDLE table)
{
  printf("NYI: nub_number_of_source_locations\n");
  return 0;
}

void nub_dispose_source_locations
  (NUB nub,  NUBHANDLE table)
{
  printf("NYI: nub_dispose_source_locations\n");
}


void nub_interpret_instruction_at_current_location
  (NUB nub,  NUBTHREAD nubthread,
    NUBINT *flow,  TARGET_ADDRESS *destination,
    NUBINT *instruction_size)
{
  printf("NYI: nub_interpret_instruction_at_current_location\n");
}


TARGET_ADDRESS calculate_step_into_destination
  (NUB nub,  NUBTHREAD nubthread,
    NUBINT *function_register_live,  NUBINT *ok)
{
  printf("NYI: calculate_step_into_destination\n");
  return 0;
}

TARGET_ADDRESS nub_dylan_thread_environment_block_address
  (NUB nub,  NUBTHREAD thread,  NUBINT *valid)
{
  printf("NYI: nub_dylan_thread_environment_block_address\n");
  return 0;
}

NUBINT nub_dylan_thread_mv_buffer_live
  (NUB nub,  NUBTHREAD thread)
{
  printf("NYI: nub_dylan_thread_mv_buffer_live\n");
  return 0;
}

NUBINT nub_older_stack_frame
  (NUB nub,
    TARGET_ADDRESS this_one,  TARGET_ADDRESS than_this_one)
{
  printf("NYI: nub_older_stack_frame\n");
  return 0;
}

TARGET_ADDRESS nub_dylan_current_function
  (NUB nub,  NUBTHREAD nubthread)
{
  printf("NYI: nub_dylan_current_function\n");
  return 0;
}


NUBINT nub_perform_absolute_relocation
  (NUB nub,
    TARGET_ADDRESS address,  TARGET_ADDRESS destination)
{
  printf("NYI: nub_perform_absolute_relocation\n");
  return 0;
}

NUBINT nub_perform_relative_relocation
  (NUB nub,
    TARGET_ADDRESS address,  TARGET_ADDRESS destination)
{
  printf("NYI: nub_perform_relative_relocation\n");
  return 0;
}




///// SERVER FUNCTIONS (Local versions)

NUBINT get_local_hostname_length ()
{
  printf("NYI: get_local_hostname_length\n");
  return 0;
}

void get_local_hostname(NUBINT buf_size, char *buf)
{
  printf("NYI: get_local_hostname\n");
}

NUBINT update_local_process_list ()
{
  printf("NYI: update_local_process_list\n");
  return 0;
}

NUBPROCESS local_process_nub_descriptor (NUB_INDEX i)
{
  printf("NYI: local_process_nub_descriptor\n");
  return 0;
}

NUBINT local_process_name_length (NUB_INDEX i)
{
  printf("NYI: local_process_name_length\n");
  return 0;
}

void local_process_name (NUB_INDEX i, NUBINT sz, char *buf)
{
  printf("NYI: local_process_name\n");
}

NUBINT local_process_system_identifier_length (NUB_INDEX i)
{
  printf("NYI: local_process_system_identifier_length\n");
  return 0;
}

void local_process_system_identifier (NUB_INDEX i, NUBINT sz, char *buf)
{
  printf("NYI: local_process_system_identifier\n");
}
