/* ******************************************************************* */
/* ** nub_interface.h                                               ** */
/* ** Formally describes the debugger nub API                       ** */
/* ** ------------------------------------------------------------- ** */
/* ** Author: Paul Howard. Copyright: 1996, Functional Objects, Inc. ** */
/* **                                 All Rights Reserved           ** */
/* ******************************************************************* */

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
  );

NUB attach_local_tether
  (NUBPROCESS process, NUBINT path_count, char **paths, char *jit,
   NUBINT *success);
NUBINT nub_remote_value_byte_size
  (NUB nub);
NUBINT nub_get_process_page_fault_count 
  (NUB nub);
NUBINT nub_thread_os_priority 
  (NUB nub,  NUBTHREAD nubthread);
NUBINT nub_get_thread_cpu_time 
  (NUB nub,  NUBTHREAD nubthread);

TARGET_ADDRESS nub_get_library_base_address
  (NUB nub, NUBLIBRARY dll);
void nub_get_library_version 
  (NUB nub,  NUBLIBRARY dll,  NUBINT *maj,  NUBINT *min);
NUBINT nub_get_library_filename_length
  (NUB nub,  NUBLIBRARY dll);
void nub_get_library_filename
  (NUB nub,  NUBLIBRARY dll,  NUBINT sz,
    char *buf);
NUBINT nub_get_library_undecorated_name_length
  (NUB nub,  NUBLIBRARY dll);
void nub_get_library_undecorated_name
  (NUB nub,  NUBLIBRARY dll,  NUBINT sz,
    char *buf);

NUBINT nub_get_register_name_length
  (NUB nub,  NUB_INDEX reg);
void nub_get_register_name
  (NUB nub,  NUB_INDEX reg,  NUBINT sz,
    char *buf);
NUBINT nub_get_register_enumeration_code
  (NUB nub,   NUB_INDEX reg);
void nub_all_registers
  (NUB nub,  NUBINT *first,  NUBINT *last);
void nub_general_registers
  (NUB nub,  NUBINT *first,  NUBINT *last);
void nub_special_registers
  (NUB nub,  NUBINT *first,  NUBINT *last);
void nub_floating_registers
  (NUB nub,  NUBINT *first,  NUBINT *last);

NUBINT nub_page_read_permission
  (NUB nub,  TARGET_ADDRESS address);
NUBINT nub_page_write_permission
  (NUB nub,  TARGET_ADDRESS address);

NUBINT nub_page_relative_address
  (NUB nub,  TARGET_ADDRESS address,  NUBINT *offset);
NUBINT nub_virtual_page_size
  (NUB nub);

TARGET_ADDRESS read_value_from_process_memory
   (NUB nub,  TARGET_ADDRESS address,  NUB_ERROR *status);
void write_value_to_process_memory
   (NUB nub,  TARGET_ADDRESS address,  TARGET_ADDRESS val,
     NUBINT *status);

TARGET_ADDRESS nub_calculate_stack_address
  (NUB nub,  NUBTHREAD nubthread,  NUBINT offset);

void nub_target_address_to_string
  (NUB nub,  TARGET_ADDRESS x,  NUBINT sz,
    char *buffer,  NUBINT radix,  NUBINT pad,
    NUBINT *truncated);
TARGET_ADDRESS nub_string_to_target_address
  (NUB nub,  NUBINT sz, char *buffer, 
    NUBINT radix,
    NUBINT *overflow);

FLOAT read_single_float_from_process_memory
  (NUB nub,  TARGET_ADDRESS address,  NUB_ERROR *status);
void write_single_float_to_process_memory
  (NUB nub,  TARGET_ADDRESS address,  FLOAT value,
    NUB_ERROR *status);
DOUBLE read_double_float_from_process_memory
  (NUB nub,  TARGET_ADDRESS address,  NUB_ERROR *status);
void write_double_float_to_process_memory
  (NUB nub,  TARGET_ADDRESS address,  DOUBLE value,
    NUB_ERROR *status);
void read_byte_string_from_process_memory
  (NUB nub,  TARGET_ADDRESS address,  NUBINT sz,
   char *buffer,  NUB_ERROR *status);
void write_byte_string_to_process_memory
  (NUB nub,  TARGET_ADDRESS address,  NUBINT sz,
   char *buffer,  NUB_ERROR *status);

TARGET_ADDRESS read_value_from_process_register_in_stack_frame
  (NUB nub,  NUBTHREAD nubthread,  NUB_INDEX reg,
    NUB_INDEX frame_index,  NUB_ERROR *status);

TARGET_ADDRESS read_value_from_process_register
  (NUB nub,  NUBTHREAD nubthread,  NUB_INDEX reg,
    NUB_ERROR *status);
void write_value_to_process_register
  (NUB nub,  NUBTHREAD nubthread,  NUB_INDEX reg,
    TARGET_ADDRESS value,  NUB_ERROR *status);
FLOAT read_single_float_from_process_register
  (NUB nub,  NUBTHREAD nubthread,  NUB_INDEX reg,
    NUB_ERROR *status);
void write_single_float_to_process_register
  (NUB nub,  NUBTHREAD nubthread,  NUB_INDEX reg,
    FLOAT value,  NUB_ERROR *status);
DOUBLE read_double_float_from_process_register
  (NUB nub,  NUBTHREAD nubthread,  NUB_INDEX reg,
    NUB_ERROR *status);
void write_double_float_to_process_register
  (NUB nub,  NUBTHREAD nubthread,  NUB_INDEX reg,
    DOUBLE value,  NUB_ERROR *status);

void nub_application_restart
  (NUB nub);
void nub_application_stop
  (NUB nub);
void nub_application_continue
  (NUB nub);
void nub_application_continue_unhandled
  (NUB nub);
void nub_application_step
  (NUB nub,  NUBINT n);
void nub_application_step_over
  (NUB nub,  NUBINT n);
void nub_application_step_out
  (NUB nub);
void nub_threads_continue
  (NUB nub);
int resume_thread
  (LPDBGTHREAD thread);
int suspend_thread
  (LPDBGTHREAD thread);
int execute_thread
  (LPDBGTHREAD thread);
int continue_thread
  (LPDBGPROCESS process,
   LPDBGTHREAD thread,
   int handling);

void set_thread_context(LPDBGTHREAD Cthread);

int get_thread_context
  (LPDBGPROCESS process,
   LPDBGTHREAD thread,
   LPCONTEXT context);

NUB_ERROR nub_set_stepping_control_on_thread
  (NUB nub,  NUBTHREAD nubthread,
    TARGET_ADDRESS fp,  TARGET_ADDRESS calling_fp,
    NUBINT location_count, 
    TARGET_ADDRESS *locs,
    NUBINT operation);
NUB_ERROR nub_clear_stepping_control_on_thread
  (NUB nub,  NUBTHREAD nubthread);

void nub_thread_stop
  (NUB nub,  NUBTHREAD nubthread);
void nub_thread_continue
  (NUB nub,  NUBTHREAD nubthread);
NUB_ERROR nub_kill_application
  (NUB nub);
void nub_close_application
  (NUB nub);
void remote_debugger_nub_shutdown
  (NUB nub);

void nub_debug_message(char*, TARGET_ADDRESS, TARGET_ADDRESS);


TARGET_ADDRESS nub_setup_function_call
  (NUB nub,  NUBTHREAD nubthread,  TARGET_ADDRESS func,
    NUBINT arg_count, TARGET_ADDRESS *args,
    NUBHANDLE *cx_handle);
TARGET_ADDRESS nub_remote_call_spy
  (NUB nub,  NUBTHREAD nubthread,  TARGET_ADDRESS func,
    NUBINT arg_count, TARGET_ADDRESS *args,
    NUB_ERROR *status);
TARGET_ADDRESS nub_get_function_result
  (NUB nub,  NUBTHREAD nubthread);
void nub_restore_context
  (NUB nub,  NUBTHREAD nubthread,  NUBHANDLE context);

NUB_ERROR nub_set_breakpoint
  (NUB nub,  TARGET_ADDRESS address);
NUB_ERROR nub_clear_breakpoint
  (NUB nub,  TARGET_ADDRESS address);
NUBINT nub_query_breakpoint
  (NUB nub,  TARGET_ADDRESS address);

void nub_wait_for_stop_reason_with_timeout
  (NUB nub,  NUBINT timeout,  NUBINT *code);
void nub_profile_wait_for_stop_reason_with_timeout
  (NUB nub,  NUBINT timeout,  NUBINT profiling_interval,
    NUBINT *code);
void nub_inform_profiling_started
  (NUB nub);
void nub_inform_profiling_stopped
  (NUB nub);
NUBINT nub_can_receive_first_chance
  (NUB nub,  NUBINT ecode);
void nub_set_first_chance
  (NUB nub,  NUBINT ecode);
void nub_unset_first_chance
  (NUB nub,  NUBINT ecode);
NUBINT nub_thread_stop_information
  (NUB nub,  NUBTHREAD nubthread,
    NUBINT *fchance,  NUBINT *fstart,
    TARGET_ADDRESS *ret_addr);
void nub_wait_for_stop_reason_no_timeout
  (NUB nub,  NUBINT *ecode);
void nub_profile_wait_for_stop_reason_no_timeout
  (NUB nub,  NUBINT profile_interval,  NUBINT *ecode);
NUB nub_stop_reason_process
  (NUB nub);
NUBTHREAD nub_stop_reason_thread
  (NUB nub);
NUBINT nub_first_hard_coded_breakpoint
  (NUB nub);
NUBINT nub_stop_reason_process_exit_code
  (NUB nub);
NUBINT nub_stop_reason_thread_exit_code
  (NUB nub);
NUBLIBRARY nub_stop_reason_library
  (NUB nub);
NUBINT nub_stop_reason_violation_op
  (NUB nub);
NUBINT nub_exception_first_chance
  (NUB nub);
TARGET_ADDRESS nub_stop_reason_violation_address
  (NUB nub);
TARGET_ADDRESS nub_stop_reason_exception_address
  (NUB nub);
TARGET_ADDRESS nub_stop_reason_debug_string_address
  (NUB nub);
NUBINT nub_stop_reason_debug_string_length
  (NUB nub);
NUBINT nub_stop_reason_debug_string_is_unicode
  (NUB nub);

NUBINT nub_initialize_stack_vectors
  (NUB nub,  NUBTHREAD nubthread);
void nub_read_stack_vectors
  (NUB nub,  NUBTHREAD nubthread,  NUBINT frame_count,
   TARGET_ADDRESS *frame_pointers,
   TARGET_ADDRESS *instruction_pointers,
   TARGET_ADDRESS *return_addresses);
void nub_all_frame_lexicals
  (NUB nub,  TARGET_ADDRESS frame,  TARGET_ADDRESS ip,
    NUB_INDEX *first,  NUB_INDEX *last,
    NUBHANDLE *table);
void nub_register_interactive_code_segment
  (NUB nub,  TARGET_ADDRESS lo,  TARGET_ADDRESS hi);
NUBINT nub_get_lexical_variable_name_length
  (NUB nub,  NUBHANDLE table,  NUB_INDEX variable);
void nub_get_lexical_variable_name
  (NUB nub,  NUBHANDLE table,  NUB_INDEX variable,
    NUBINT sz,  char *buffer);
TARGET_ADDRESS nub_lexical_variable_address
  (NUB nub,  TARGET_ADDRESS fp, NUBHANDLE table,  NUB_INDEX variable,
    NUBINT *in_reg,  NUB_INDEX *hireg,  NUB_INDEX *loreg,
    NUBINT *arg);

NUBINT nub_lookup_symbol_name_length
  (NUB nub,  NUBHANDLE table,  NUB_INDEX sym);
void nub_lookup_symbol_name
  (NUB nub,  NUBHANDLE table,  NUB_INDEX sym,
    NUBINT sz,  char *buffer);
TARGET_ADDRESS nub_lookup_symbol_address
  (NUB nub,  NUBHANDLE table,  NUB_INDEX sym);
TARGET_ADDRESS nub_lookup_function_debug_start
  (NUB nub,  NUBHANDLE table,  NUB_INDEX sym);
TARGET_ADDRESS nub_lookup_function_debug_end
  (NUB nub,  NUBHANDLE table,  NUB_INDEX sym);
NUBINT nub_lookup_symbol_language
  (NUB nub,  NUBHANDLE table,  NUB_INDEX sym);
NUBINT nub_lookup_symbol_language_code
  (NUB nub,  NUBHANDLE table,  NUB_INDEX index);
TARGET_ADDRESS nub_lookup_function_end
  (NUB nub,  NUBHANDLE table,  NUB_INDEX sym);
NUBINT nub_symbol_is_function
  (NUB nub,  NUBHANDLE table,  NUB_INDEX sym);
NUBINT nub_nearest_symbols
  (NUB nub,  TARGET_ADDRESS address,
    NUBLIBRARY *lib,  NUBHANDLE *table);
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
    TARGET_ADDRESS *final_address_of_definition);
void nub_function_bounding_addresses
  (NUB nub,  TARGET_ADDRESS address,
    TARGET_ADDRESS *lower,  TARGET_ADDRESS *upper);
void nub_closest_symbol_name
  (NUB nub,  NUBINT sz,
    char *buffer);
NUBINT nub_find_symbol_in_library
  (NUB nub,  NUBLIBRARY nublibrary,  NUBINT sz,
    char *name, 
    TARGET_ADDRESS *address,
    NUBINT *type,
    NUBINT *is_function,
    TARGET_ADDRESS *debug_start,
    TARGET_ADDRESS *debug_end,
    NUBINT *symbol_language,
    TARGET_ADDRESS *final_address_of_definition);
void nub_dispose_lookups
  (NUB nub,  NUBHANDLE lookups);

TARGET_ADDRESS nub_resolve_source_location
  (NUB nub,  NUBLIBRARY nublibrary, char *filename,
    NUBINT line_number,  NUBINT column_number,
    NUBINT *valid,  NUBINT *path,  NUBHANDLE *search,
    NUBINT *exact);
NUBHANDLE nub_fetch_source_locations
  (NUB nub, 
    TARGET_ADDRESS start_loc,  TARGET_ADDRESS end_loc);
NUBINT nub_source_location_address
  (NUB nub,  NUBHANDLE table,  NUB_INDEX index);
NUBINT nub_source_location_linenumber
  (NUB nub,  NUBHANDLE table,  NUB_INDEX index);
NUBINT nub_source_location_filename_length
  (NUB nub,  NUBHANDLE table);
void nub_source_location_filename
  (NUB nub,  NUBHANDLE table,
    NUBINT sz,
    char *buffer);
NUBINT nub_number_of_source_locations
  (NUB nub,  NUBHANDLE table);
void nub_dispose_source_locations
  (NUB nub,  NUBHANDLE table);

void nub_interpret_instruction_at_current_location
  (NUB nub,  NUBTHREAD nubthread,
    NUBINT *flow,  TARGET_ADDRESS *destination,
    NUBINT *instruction_size);

TARGET_ADDRESS calculate_step_into_destination
  (NUB nub,  NUBTHREAD nubthread,
    NUBINT *function_register_live,  NUBINT *ok);
TARGET_ADDRESS nub_dylan_thread_environment_block_address
  (NUB nub,  NUBTHREAD thread,  NUBINT *valid);
NUBINT nub_dylan_thread_mv_buffer_live
  (NUB nub,  NUBTHREAD thread);
NUBINT nub_older_stack_frame
  (NUB nub,
    TARGET_ADDRESS this_one,  TARGET_ADDRESS than_this_one);
TARGET_ADDRESS nub_dylan_current_function
  (NUB nub,  NUBTHREAD nubthread);

NUBINT nub_perform_absolute_relocation
  (NUB nub, 
    TARGET_ADDRESS address,  TARGET_ADDRESS destination);
NUBINT nub_perform_relative_relocation
  (NUB nub, 
    TARGET_ADDRESS address,  TARGET_ADDRESS destination);



///// SERVER FUNCTIONS (Local and remote versions)

NUBINT get_local_hostname_length ();
void get_local_hostname(NUBINT buf_size, char *buf);
NUBINT verify_local_password(NUBINT buf_size, char *buf);
NUBINT update_local_process_list ();
NUBPROCESS local_process_nub_descriptor (NUB_INDEX i);
NUBINT local_process_name_length (NUB_INDEX i);
void local_process_name (NUB_INDEX i, NUBINT sz, char *buf);
NUBINT local_process_system_identifier_length (NUB_INDEX i);
void local_process_system_identifier (NUB_INDEX i, NUBINT sz, char *buf);

NUBINT server_get_hostname_length (SERVER s, NUBINT *success);
void server_get_hostname (SERVER s, NUBINT buf_size, char *buf);
NUBINT server_verify_password (SERVER s, NUBINT buf_size, char *buf);
NUBINT update_server_process_list (SERVER s);
NUBPROCESS server_process_nub_descriptor (SERVER s, NUB_INDEX i);
NUBINT server_process_name_length (SERVER s, NUB_INDEX i);
void server_process_name (SERVER s, NUB_INDEX i, NUBINT sz, char *buf);
NUBINT server_process_system_identifier_length (SERVER s, NUB_INDEX i);
void server_process_system_identifier 
  (SERVER s, NUB_INDEX i, NUBINT sz, char *buf);
