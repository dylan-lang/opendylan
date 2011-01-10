module:        access-path-implementation
synopsis:      FFI declarations to allow access-path to call the debugger
               nub on demand
author:        Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// C type aliases to make our FFI declarations look more like the
// prototypes in the nub header.

define C-struct <DESCRIPTOR>
end C-struct;

define C-pointer-type <DESCRIPTOR-POINTER> => <DESCRIPTOR>;

define constant <NUBHANDLE>            = <DESCRIPTOR-POINTER>;
define constant <NUBINT>               = <C-int>;
define constant <NUBPROCESS>           = <NUBHANDLE>;
define constant <NUBTHREAD>            = <NUBHANDLE>;
define constant <NUBLIBRARY>           = <NUBHANDLE>;
define constant <NUB>                  = <NUBHANDLE>;
define constant <NUBFLOAT>             = <C-float>;
define constant <NUBDOUBLE>            = <C-double>;
define constant <TARGET-ADDRESS>       = <C-raw-unsigned-long>;
define constant <REMOTE-ARG-ARRAY>     = <C-raw-unsigned-long*>;
define constant <POINTER-VECTOR>       = <C-raw-unsigned-long*>;
define constant <NUB-INDEX>            = <C-int>;
define constant <NUB-ERROR>            = <C-int>;

define class <THREAD-CONTEXT> (<object>)
  constant slot thread-was-suspended-by-debugger? :: <boolean>,
    required-init-keyword: suspended?:;
  constant slot nub-descriptor :: <NUBHANDLE>,
    required-init-keyword: nub-descriptor:;
end class;

define C-pointer-type <NUBHANDLE-POINTER>
                                => <NUBHANDLE>;

define C-pointer-type <NUBINT-POINTER>
                                => <NUBINT>;

define C-pointer-type <NUB-ERROR-POINTER>
                                => <NUB-ERROR>;

define C-pointer-type <NUB-INDEX-POINTER>
                                => <NUB-INDEX>;

define C-pointer-type <NUBLIBRARY-POINTER>
                                => <NUBLIBRARY>;

define C-pointer-type <TARGET-ADDRESS-POINTER>
                                => <TARGET-ADDRESS>;



// PRIMITIVES TO DO TAG STRIPPING/ADDING

define C-function nub-primitive-tagged-value-as-integer
       parameter         x :: <TARGET-ADDRESS>;
       result            i :: <TARGET-ADDRESS>;
       c-name: "nub_primitive_tagged_value_as_integer";
end C-function;

define C-function nub-primitive-tagged-value-as-character
       parameter         x :: <TARGET-ADDRESS>;
       result            c :: <C-char>;
       c-name: "nub_primitive_tagged_value_as_character";
end C-function;

define C-function nub-primitive-integer-as-tagged-value
       parameter         i :: <NUBINT>;
       result            x :: <TARGET-ADDRESS>;
       c-name: "nub_primitive_integer_as_tagged_value";
end C-function;

define C-function nub-primitive-character-as-tagged-value
       parameter         c :: <C-char>;
       result            x :: <TARGET-ADDRESS>;
       c-name: "nub_primitive_character_as_tagged_value";
end C-function;

define C-function nub-primitive-indexed-remote-value
       parameter         x :: <TARGET-ADDRESS>;
       parameter         i :: <NUBINT>;
       result            r :: <TARGET-ADDRESS>;
       c-name: "nub_primitive_indexed_remote_value";
end C-function;

define C-function nub-primitive-byte-indexed-remote-value
       parameter         x :: <TARGET-ADDRESS>;
       parameter         i :: <NUBINT>;
       result            r :: <TARGET-ADDRESS>;
       c-name: "nub_primitive_byte_indexed_remote_value";
end C-function;

define C-function nub-primitive-remote-value-as-integer-losing-precision
       parameter         x :: <TARGET-ADDRESS>;
       result            i :: <NUBINT>;
       c-name: "nub_primitive_remote_value_as_integer_losing_precision";
end C-function;

define C-function nub-primitive-integer-as-remote-value-losing-precision
       parameter         i :: <NUBINT>;
       result            x :: <TARGET-ADDRESS>;
       c-name: "nub_primitive_integer_as_remote_value_losing_precision";
end C-function;

define C-function nub-primitive-select-low-order-bits
       parameter         x :: <TARGET-ADDRESS>;
       parameter         n :: <NUBINT>;
       result            k :: <NUBINT>;
       c-name: "nub_primitive_select_low_order_bits";
end C-function;

define C-function open-local-tether
       parameter command          :: <C-string>;
       parameter arguments        :: <C-string>;
       parameter path-count       :: <NUBINT>;
       parameter paths            :: <C-string*>;
       parameter libsearch-count  :: <NUBINT>;
       parameter libsearch        :: <C-string*>;
       parameter workingdir       :: <C-string>;
       parameter own-shell        :: <NUBINT>;
       output parameter success   :: <NUBINT-POINTER>;
       result tether              :: <NUB>;
       c-name: "open_local_tether";
end C-function;

define C-function attach-local-tether
       parameter process          :: <NUBPROCESS>;
       parameter path-count       :: <NUBINT>;
       parameter paths            :: <C-string*>;
       parameter system-JIT-info  :: <C-string>;
       output parameter success   :: <NUBINT-POINTER>;
       result tether              :: <NUB>;
       c-name: "attach_local_tether";
end C-function;

define C-function update-local-process-list
       result count               :: <NUBINT>;
       c-name: "update_local_process_list";
end C-function;

define C-function local-process-nub-descriptor
       parameter index            :: <NUB-INDEX>;
       result length              :: <NUBPROCESS>;
       c-name: "local_process_nub_descriptor";
end C-function;

define C-function local-process-name-length
       parameter index            :: <NUB-INDEX>;
       result length              :: <NUBINT>;
       c-name: "local_process_name_length";
end C-function;

define C-function local-process-name
       parameter index            :: <NUB-INDEX>;
       parameter length           :: <NUBINT>;
       parameter buf              :: <C-string>;
       c-name: "local_process_name";
end C-function;

define C-function local-process-system-identifier-length
       parameter index            :: <NUB-INDEX>;
       result length              :: <NUBINT>;
       c-name: "local_process_system_identifier_length";
end C-function;

define C-function local-process-system-identifier
       parameter index            :: <NUB-INDEX>;
       parameter length           :: <NUBINT>;
       parameter buf              :: <C-string>;
       c-name: "local_process_system_identifier";
end C-function;

define C-function get-local-hostname-length
       result sz                  :: <NUBINT>;
       c-name: "get_local_hostname_length";
end C-function;

define C-function get-local-hostname
       parameter buf-size         :: <NUBINT>;
       parameter buffer           :: <C-string>;
       c-name: "get_local_hostname";
end C-function;

/*
define C-function enumerate-local-processes
end C-function;

define C-function local-process-description
end C-function;

*/

// DEBUGGER-NUB FUNCTION DESCRIPTIONS

define debugger-nub-interface nub-remote-value-byte-size
       parameter nub              :: <NUB>;
       result byte-size           :: <NUBINT>;
       c-name: "nub_remote_value_byte_size";
end debugger-nub-interface;

define debugger-nub-interface nub-get-process-page-fault-count
       parameter nub              :: <NUB>;
       result count               :: <NUBINT>;
       c-name: "nub_get_process_page_fault_count";
end debugger-nub-interface;

define debugger-nub-interface nub-thread-os-priority
       parameter nub              :: <NUB>;
       parameter thread           :: <NUBTHREAD>;
       result prio                :: <NUBINT>;
       c-name: "nub_thread_os_priority";
end debugger-nub-interface;

define debugger-nub-interface nub-get-thread-cpu-time
       parameter nub              :: <NUB>;
       parameter thread           :: <NUBTHREAD>;
       result time-ms             :: <NUBINT>;
       c-name: "nub_get_thread_cpu_time";
end debugger-nub-interface;

define debugger-nub-interface nub-get-process-wall-clock-time
       parameter nub              :: <NUB>;
       result time-millisecs      :: <NUBINT>;
       c-name: "nub_get_process_wall_clock_time";
end debugger-nub-interface;

define debugger-nub-interface nub-get-library-base-address
       parameter nub              :: <NUB>;
       parameter library          :: <NUBLIBRARY>;
       result base-addr           :: <TARGET-ADDRESS>;
       c-name: "nub_get_library_base_address";
end debugger-nub-interface;

define debugger-nub-interface nub-get-library-version
       parameter nub              :: <NUB>;
       parameter library          :: <NUBLIBRARY>;
       output parameter maj       :: <NUBINT-POINTER>;
       output parameter min       :: <NUBINT-POINTER>;
       c-name: "nub_get_library_version";
end debugger-nub-interface;

define debugger-nub-interface nub-get-library-filename-length
       parameter nub              :: <NUB>;
       parameter library          :: <NUBLIBRARY>;
       result length              :: <NUBINT>;
       c-name: "nub_get_library_filename_length";
end debugger-nub-interface;

define debugger-nub-interface nub-get-library-filename
       parameter nub              :: <NUB>;
       parameter library          :: <NUBLIBRARY>;
       parameter buf-size         :: <NUBINT>;
       parameter buf              :: <C-string>;
       c-name: "nub_get_library_filename";
end debugger-nub-interface;

define debugger-nub-interface nub-get-library-undecorated-name-length
       parameter nub              :: <NUB>;
       parameter library          :: <NUBLIBRARY>;
       result length              :: <NUBINT>;
       c-name: "nub_get_library_undecorated_name_length";
end debugger-nub-interface;

define debugger-nub-interface nub-get-library-undecorated-name
       parameter nub              :: <NUB>;
       parameter library          :: <NUBLIBRARY>;
       parameter buf-size         :: <NUBINT>;
       parameter buf              :: <C-string>;
       c-name: "nub_get_library_undecorated_name";
end debugger-nub-interface;
                                           
define debugger-nub-interface nub-get-register-name-length
       parameter nub              :: <NUB>;
       parameter register         :: <NUB-INDEX>;
       result length              :: <NUBINT>;
       c-name: "nub_get_register_name_length";
end debugger-nub-interface;

define debugger-nub-interface nub-get-register-enumeration-code
       parameter nub              :: <NUB>;
       parameter register         :: <NUB-INDEX>;
       result code                :: <NUBINT>;
       c-name: "nub_get_register_enumeration_code";
end debugger-nub-interface;

define debugger-nub-interface nub-get-register-name
       parameter nub              :: <NUB>;
       parameter register         :: <NUB-INDEX>;
       parameter buf-size         :: <NUBINT>;
       parameter buf              :: <C-string>;
       c-name: "nub_get_register_name";
end debugger-nub-interface;

define debugger-nub-interface nub-all-registers
       parameter nub              :: <NUB>;
       output parameter first     :: <NUB-INDEX-POINTER>;
       output parameter last      :: <NUB-INDEX-POINTER>;
       c-name: "nub_all_registers";
end debugger-nub-interface;

define debugger-nub-interface nub-general-registers
       parameter nub              :: <NUB>;
       output parameter first     :: <NUB-INDEX-POINTER>;
       output parameter last      :: <NUB-INDEX-POINTER>;
       c-name: "nub_general_registers";
end debugger-nub-interface;

define debugger-nub-interface nub-special-registers
       parameter nub              :: <NUB>;
       output parameter first     :: <NUB-INDEX-POINTER>;
       output parameter last      :: <NUB-INDEX-POINTER>;
       c-name: "nub_special_registers";
end debugger-nub-interface;

/* Not yet used

define debugger-nub-interface nub-floating-registers
       parameter nub              :: <NUB>;
       output parameter first     :: <NUB-INDEX-POINTER>;
       output parameter last      :: <NUB-INDEX-POINTER>;
       c-name: "nub_floating_registers";
end debugger-nub-interface;
*/

define debugger-nub-interface nub-page-read-permission
       parameter nub              :: <NUB>;
       parameter address          :: <TARGET-ADDRESS>;
       result protected           :: <NUBINT>;
       c-name: "nub_page_read_permission";
end debugger-nub-interface;

define debugger-nub-interface nub-page-write-permission
       parameter nub              :: <NUB>;
       parameter address          :: <TARGET-ADDRESS>;
       result protected           :: <NUBINT>;
       c-name: "nub_page_write_permission";
end debugger-nub-interface;

define debugger-nub-interface nub-page-relative-address
       parameter nub              :: <NUB>;
       parameter address          :: <TARGET-ADDRESS>;
       output parameter offset    :: <NUBINT-POINTER>;
       result num                 :: <NUBINT>;
       c-name: "nub_page_relative_address";
end debugger-nub-interface;

define debugger-nub-interface nub-virtual-page-size
       parameter nub              :: <NUB>;
       result page-size           :: <NUBINT>;
       c-name: "nub_virtual_page_size";
end debugger-nub-interface;

define debugger-nub-interface nub-read-value-from-process-memory
       parameter nub              :: <NUB>;
       parameter address          :: <TARGET-ADDRESS>;
       output parameter errcode   :: <NUB-ERROR-POINTER>;
       result value               :: <TARGET-ADDRESS>;
       c-name: "read_value_from_process_memory";
end debugger-nub-interface;

define debugger-nub-interface nub-write-value-to-process-memory
       parameter nub              :: <NUB>;
       parameter address          :: <TARGET-ADDRESS>;
       parameter value            :: <TARGET-ADDRESS>;
       output parameter errcode   :: <NUB-ERROR-POINTER>;
       c-name: "write_value_to_process_memory";
end debugger-nub-interface;

define debugger-nub-interface nub-calculate-stack-address
       parameter nub              :: <NUB>;
       parameter nubthread        :: <NUBTHREAD>;
       parameter offset           :: <NUBINT>;
       result addr                :: <TARGET-ADDRESS>;
       c-name: "nub_calculate_stack_address";
end debugger-nub-interface;

define debugger-nub-interface nub-target-address-to-string
       parameter nub              :: <NUB>;
       parameter x                :: <TARGET-ADDRESS>;
       parameter buf-size         :: <NUBINT>;
       parameter buf              :: <C-string>;
       parameter radix            :: <NUBINT>;
       parameter padding          :: <NUBINT>;
       output parameter trunc?    :: <NUBINT-POINTER>;
       c-name: "nub_target_address_to_string";
end debugger-nub-interface;

define debugger-nub-interface nub-string-to-target-address
       parameter nub              :: <NUB>;
       parameter sz               :: <NUBINT>;
       parameter str              :: <C-string>;
       parameter radix            :: <NUBINT>;
       output parameter overflow? :: <NUBINT-POINTER>;
       result rv                  :: <TARGET-ADDRESS>;
       c-name: "nub_string_to_target_address";
end debugger-nub-interface;

/*

// TODO: Add these commented-out functions...

define debugger-nub-interface nub-read-8b-from-memory
       parameter nub              :: <NUB>;
       parameter address          :: <TARGET-ADDRESS>;
       output parameter errcode   :: <NUB-ERROR-POINTER>;
       result value               :: <INT8>;
       c-name: "read_8b_from_process_memory";
end debugger-nub-interface;

define debugger-nub-interface nub-write-8b-to-memory
       parameter nub              :: <NUB>;
       parameter address          :: <TARGET-ADDRESS>;
       parameter value            :: <INT8>;
       output parameter errcode   :: <NUB-ERROR-POINTER>;
       c-name: "write_8b_to_process_memory";
end debugger-nub-interface;

define debugger-nub-interface nub-read-16b-from-memory
       parameter nub              :: <NUB>;
       parameter address          :: <TARGET-ADDRESS>;
       output parameter errcode   :: <NUB-ERROR-POINTER>;
       result value               :: <INT16>;
       c-name: "read_16b_from_process_memory";
end debugger-nub-interface;

define debugger-nub-interface nub-write-16b-to-memory
       parameter nub              :: <NUB>;
       parameter address          :: <TARGET-ADDRESS>;
       parameter value            :: <INT16>;
       output parameter errcode   :: <NUB-ERROR-POINTER>;
       c-name: "write_16b_to_process_memory";
end debugger-nub-interface;

define debugger-nub-interface nub-read-32b-from-memory
       parameter nub              :: <NUB>;
       parameter address          :: <TARGET-ADDRESS>;
       output parameter errcode   :: <NUB-ERROR-POINTER>;
       result value               :: <INT32>;
       c-name: "read_32b_from_process_memory";
end debugger-nub-interface;

define debugger-nub-interface nub-write-32b-to-memory
       parameter nub              :: <NUB>;
       parameter address          :: <TARGET-ADDRESS>;
       parameter value            :: <INT32>;
       output parameter errcode   :: <NUB-ERROR-POINTER>;
       c-name: "write_32b_to_process_memory";
end debugger-nub-interface;

define debugger-nub-interface nub-read-64b-from-memory
       parameter nub              :: <NUB>;
       parameter address          :: <TARGET-ADDRESS>;
       output parameter errcode   :: <NUB-ERROR-POINTER>;
       result value               :: <INT64>;
       c-name: "read_64b_from_process_memory";
end debugger-nub-interface;

define debugger-nub-interface nub-write-64b-to-memory
       parameter nub              :: <NUB>;
       parameter address          :: <TARGET-ADDRESS>;
       parameter value            :: <INT64>;
       output parameter errcode   :: <NUB-ERROR-POINTER>;
       c-name: "write_64b_to_process_memory";
end debugger-nub-interface;
*/

define debugger-nub-interface nub-read-single-float-from-process-memory
       parameter nub              :: <NUB>;
       parameter address          :: <TARGET-ADDRESS>;
       output parameter errcode   :: <NUB-ERROR-POINTER>;
       result value               :: <NUBFLOAT>;
       c-name: "read_single_float_from_process_memory";
end debugger-nub-interface;

define debugger-nub-interface nub-write-single-float-to-process-memory
       parameter nub              :: <NUB>;
       parameter address          :: <TARGET-ADDRESS>;
       parameter value            :: <NUBFLOAT>;
       output parameter errcode   :: <NUB-ERROR-POINTER>;
       c-name: "write_single_float_to_process_memory";
end debugger-nub-interface;

define debugger-nub-interface nub-read-double-float-from-process-memory
       parameter nub              :: <NUB>;
       parameter address          :: <TARGET-ADDRESS>;
       output parameter errcode   :: <NUB-ERROR-POINTER>;
       result value               :: <NUBDOUBLE>;
       c-name: "read_double_float_from_process_memory";
end debugger-nub-interface;

define debugger-nub-interface nub-write-double-float-to-process-memory
       parameter nub              :: <NUB>;
       parameter address          :: <TARGET-ADDRESS>;
       parameter value            :: <NUBDOUBLE>;
       output parameter errcode   :: <NUB-ERROR-POINTER>;
       c-name: "write_double_float_to_process_memory";
end debugger-nub-interface;

define debugger-nub-interface nub-read-byte-string-from-process-memory
       parameter nub              :: <NUB>;
       parameter address          :: <TARGET-ADDRESS>;
       parameter length           :: <NUBINT>;
       parameter buf              :: <C-string>;
       output parameter errcode   :: <NUB-ERROR-POINTER>;
       c-name: "read_byte_string_from_process_memory";
end debugger-nub-interface;

define debugger-nub-interface nub-write-byte-string-to-process-memory
       parameter nub              :: <NUB>;
       parameter address          :: <TARGET-ADDRESS>;
       parameter length           :: <NUBINT>;
       parameter buf              :: <C-string>;
       output parameter errcode   :: <NUB-ERROR-POINTER>;
       c-name: "write_byte_string_to_process_memory";
end debugger-nub-interface;

/*
define debugger-nub-interface nub-read-unicode-string-from-memory
       parameter nub              :: <NUB>;
       parameter address          :: <TARGET-ADDRESS>;
       parameter length           :: <NUBINT>;
       parameter buf              :: <UNICODE-POINTER>;
       output parameter errcode   :: <NUB-ERROR-POINTER>;
       c-name: "read_unicode_string_from_process_memory";
end debugger-nub-interface;

define debugger-nub-interface nub-write-unicode-string-to-memory
       parameter nub              :: <NUB>;
       parameter address          :: <TARGET-ADDRESS>;
       parameter length           :: <NUBINT>;
       parameter buf              :: <UNICODE-POINTER>;
       output parameter errcode   :: <NUB-ERROR-POINTER>;
       c-name: "write_unicode_string_to_process_memory";
end debugger-nub-interface;

*/

define debugger-nub-interface nub-read-value-from-process-register
       parameter nub              :: <NUB>;
       parameter thread           :: <NUBTHREAD>;
       parameter register         :: <NUB-INDEX>;
       output parameter errcode   :: <NUB-ERROR-POINTER>;
       result value               :: <TARGET-ADDRESS>;
       c-name: "read_value_from_process_register";
end debugger-nub-interface;

define debugger-nub-interface nub-read-value-from-process-register-in-stack-frame
       parameter nub              :: <NUB>;
       parameter thread           :: <NUBTHREAD>;
       parameter register         :: <NUB-INDEX>;
       parameter frame-index      :: <NUB-INDEX>;
       output parameter errcode   :: <NUB-ERROR-POINTER>;
       result value               :: <TARGET-ADDRESS>;
       c-name: "read_value_from_process_register_in_stack_frame";
end debugger-nub-interface;

define debugger-nub-interface nub-write-value-to-process-register
       parameter nub              :: <NUB>;
       parameter thread           :: <NUBTHREAD>;
       parameter register         :: <NUB-INDEX>;
       parameter value            :: <TARGET-ADDRESS>;
       output parameter errcode   :: <NUB-ERROR-POINTER>;
       c-name: "write_value_to_process_register";
end debugger-nub-interface;

/*

// TODO: Add these commented-out functions...

define debugger-nub-interface nub-read-8b-from-register
       parameter nub              :: <NUB>;
       parameter thread           :: <NUBTHREAD>;
       parameter register         :: <NUB-INDEX>;
       output parameter errcode   :: <NUB-ERROR-POINTER>;
       result value               :: <INT8>;
       c-name: "read_8b_from_process_register";
end debugger-nub-interface;

define debugger-nub-interface nub-write-8b-to-register
       parameter nub              :: <NUB>;
       parameter thread           :: <NUBTHREAD>;
       parameter register         :: <NUB-INDEX>;
       parameter value            :: <INT8>;
       output parameter errcode   :: <NUB-ERROR-POINTER>;
       c-name: "write_8b_to_process_register";
end debugger-nub-interface;

define debugger-nub-interface nub-read-16b-from-register
       parameter nub              :: <NUB>;
       parameter thread           :: <NUBTHREAD>;
       parameter register         :: <NUB-INDEX>;
       output parameter errcode   :: <NUB-ERROR-POINTER>;
       result value               :: <INT16>;
       c-name: "read_16b_from_process_register";
end debugger-nub-interface;

define debugger-nub-interface nub-write-16b-to-register
       parameter nub              :: <NUB>;
       parameter thread           :: <NUBTHREAD>;
       parameter register         :: <NUB-INDEX>;
       parameter value            :: <INT16>;
       output parameter errcode   :: <NUB-ERROR-POINTER>;
       c-name: "write_16b_to_process_register";
end debugger-nub-interface;

define debugger-nub-interface nub-read-32b-from-register
       parameter nub              :: <NUB>;
       parameter thread           :: <NUBTHREAD>;
       parameter register         :: <NUB-INDEX>;
       output parameter errcode   :: <NUB-ERROR-POINTER>;
       result value               :: <INT32>;
       c-name: "read_32b_from_process_register";
end debugger-nub-interface;

define debugger-nub-interface nub-write-32b-to-register
       parameter nub              :: <NUB>;
       parameter thread           :: <NUBTHREAD>;
       parameter register         :: <NUB-INDEX>;
       parameter value            :: <INT32>;
       output parameter errcode   :: <NUB-ERROR-POINTER>;
       c-name: "write_32b_to_process_register";
end debugger-nub-interface;

define debugger-nub-interface nub-read-64b-from-register
       parameter nub              :: <NUB>;
       parameter thread           :: <NUBTHREAD>;
       parameter register         :: <NUB-INDEX>;
       output parameter errcode   :: <NUB-ERROR-POINTER>;
       result value               :: <INT64>;
       c-name: "read_64b_from_process_register";
end debugger-nub-interface;

define debugger-nub-interface nub-write-64b-to-register
       parameter nub              :: <NUB>;
       parameter thread           :: <NUBTHREAD>;
       parameter register         :: <NUB-INDEX>;
       parameter value            :: <INT64>;
       output parameter errcode   :: <NUB-ERROR-POINTER>;
       c-name: "write_64b_to_process_register";
end debugger-nub-interface;
*/

define debugger-nub-interface nub-read-single-float-from-process-register
       parameter nub              :: <NUB>;
       parameter thread           :: <NUBTHREAD>;
       parameter register         :: <NUB-INDEX>;
       output parameter errcode   :: <NUB-ERROR-POINTER>;
       result value               :: <NUBFLOAT>;
       c-name: "read_single_float_from_process_register";
end debugger-nub-interface;

define debugger-nub-interface nub-write-single-float-to-process-register
       parameter nub              :: <NUB>;
       parameter thread           :: <NUBTHREAD>;
       parameter register         :: <NUB-INDEX>;
       parameter value            :: <NUBFLOAT>;
       output parameter errcode   :: <NUB-ERROR-POINTER>;
       c-name: "write_single_float_to_process_register";
end debugger-nub-interface;

define debugger-nub-interface nub-read-double-float-from-process-register
       parameter nub              :: <NUB>;
       parameter thread           :: <NUBTHREAD>;
       parameter register         :: <NUB-INDEX>;
       output parameter errcode   :: <NUB-ERROR-POINTER>;
       result value               :: <NUBDOUBLE>;
       c-name: "read_double_float_from_process_register";
end debugger-nub-interface;

define debugger-nub-interface nub-write-double-float-to-process-register
       parameter nub              :: <NUB>;
       parameter thread           :: <NUBTHREAD>;
       parameter register         :: <NUB-INDEX>;
       parameter value            :: <NUBDOUBLE>;
       output parameter errcode   :: <NUB-ERROR-POINTER>;
       c-name: "write_double_float_to_process_register";
end debugger-nub-interface;

define debugger-nub-interface nub-application-restart
       parameter nub              :: <NUB>;
       c-name: "nub_application_restart";
end debugger-nub-interface;

define debugger-nub-interface nub-application-stop
       parameter nub              :: <NUB>;
       c-name: "nub_application_stop";
end debugger-nub-interface;

define debugger-nub-interface nub-application-continue
       parameter nub              :: <NUB>;
       c-name: "nub_application_continue";
end debugger-nub-interface;

define debugger-nub-interface nub-application-continue-unhandled
       parameter nub              :: <NUB>;
       c-name: "nub_application_continue_unhandled";
end debugger-nub-interface;

define debugger-nub-interface nub-application-step
       parameter nub              :: <NUB>;
       parameter n                :: <NUBINT>;
       c-name: "nub_application_step";
end debugger-nub-interface;

define debugger-nub-interface nub-application-step-over
       parameter nub              :: <NUB>;
       parameter n                :: <NUBINT>;
       c-name: "nub_application_step_over";
end debugger-nub-interface;

define debugger-nub-interface nub-application-step-out
       parameter nub              :: <NUB>;
       c-name: "nub_application_step_out";
end debugger-nub-interface;

define debugger-nub-interface nub-set-stepping-control-on-thread
       parameter nub              :: <NUB>;
       parameter thread           :: <NUBTHREAD>;
       parameter fp               :: <TARGET-ADDRESS>;
       parameter calling-fp       :: <TARGET-ADDRESS>;
       parameter count            :: <NUBINT>;
       parameter locations        :: <REMOTE-ARG-ARRAY>;
       parameter operation        :: <NUBINT>;
       result errcode             :: <NUB-ERROR>;
       c-name: "nub_set_stepping_control_on_thread";
end debugger-nub-interface;

define debugger-nub-interface nub-clear-stepping-control-on-thread
       parameter nub              :: <NUB>;
       parameter thread           :: <NUBTHREAD>;
       c-name: "nub_clear_stepping_control_on_thread";
end debugger-nub-interface;

define debugger-nub-interface nub-thread-stop
       parameter nub              :: <NUB>;
       parameter thread           :: <NUBTHREAD>;
       c-name: "nub_thread_stop";
end debugger-nub-interface;

define debugger-nub-interface nub-thread-continue
       parameter nub              :: <NUB>;
       parameter thread           :: <NUBTHREAD>;
       c-name: "nub_thread_continue";
end debugger-nub-interface;

define debugger-nub-interface nub-thread-suspended?
       parameter thread           :: <NUBTHREAD>;
       result suspended?          :: <C-boolean>;
       c-name: "nub_thread_suspendedQ";
end debugger-nub-interface;

define debugger-nub-interface nub-thread-suspended
       parameter thread           :: <NUBTHREAD>;
       c-name: "nub_thread_suspended";
end debugger-nub-interface;

define debugger-nub-interface nub-thread-resumed
       parameter thread           :: <NUBTHREAD>;
       c-name: "nub_thread_resumed";
end debugger-nub-interface;

define debugger-nub-interface nub-kill-application
       parameter nub              :: <NUB>;
       result errcode             :: <NUB-ERROR>;
       c-name: "nub_kill_application";
end debugger-nub-interface;

define debugger-nub-interface nub-register-exit-process-function
       parameter nub              :: <NUB>;
       parameter exit-function    :: <TARGET-ADDRESS>;
       c-name: "nub_register_exit_process_function";
end debugger-nub-interface;

define debugger-nub-interface nub-close-application
       parameter nub              :: <NUB>;
       c-name: "nub_close_application";
end debugger-nub-interface;

define debugger-nub-interface nub-setup-function-call
       parameter nub              :: <NUB>;
       parameter thread           :: <NUBTHREAD>;
       parameter function         :: <TARGET-ADDRESS>;
       parameter arg-count        :: <NUBINT>;
       parameter arg-vector       :: <REMOTE-ARG-ARRAY>;
       output parameter cx-handle :: <NUBHANDLE-POINTER>;
       result return-address      :: <TARGET-ADDRESS>;
       c-name: "nub_setup_function_call";
end debugger-nub-interface;

define debugger-nub-interface nub-remote-call-spy
       parameter nub              :: <NUB>;
       parameter thread           :: <NUBTHREAD>;
       parameter function         :: <TARGET-ADDRESS>;
       parameter arg-count        :: <NUBINT>;
       parameter args             :: <TARGET-ADDRESS-POINTER>;
       output parameter errcode   :: <NUB-ERROR-POINTER>;
       result r                   :: <TARGET-ADDRESS>;
       c-name: "nub_remote_call_spy";
end debugger-nub-interface;

define debugger-nub-interface nub-get-function-result
       parameter nub              :: <NUB>;
       parameter thread           :: <NUBTHREAD>;
       result r                   :: <TARGET-ADDRESS>;
       c-name: "nub_get_function_result";
end debugger-nub-interface;

define debugger-nub-interface nub-restore-context
       parameter nub              :: <NUB>;
       parameter thread           :: <NUBTHREAD>;
       parameter ctx-handle       :: <NUBHANDLE>;
       c-name: "nub_restore_context";
end debugger-nub-interface;

define debugger-nub-interface nub-set-breakpoint
       parameter nub              :: <NUB>;
       parameter address          :: <TARGET-ADDRESS>;
       result errcode             :: <NUB-ERROR>;
       c-name: "nub_set_breakpoint";
end debugger-nub-interface;

define debugger-nub-interface nub-recover-breakpoint
       parameter nub              :: <NUB>;
       parameter thread           :: <NUBTHREAD>;
       c-name: "nub_recover_breakpoint";
end debugger-nub-interface;

define debugger-nub-interface nub-clear-breakpoint
       parameter nub              :: <NUB>;
       parameter address          :: <TARGET-ADDRESS>;
       result errcode             :: <NUB-ERROR>;
       c-name: "nub_clear_breakpoint";
end debugger-nub-interface;

define debugger-nub-interface nub-query-breakpoint
       parameter nub              :: <NUB>;
       parameter address          :: <TARGET-ADDRESS>;
       result exists              :: <NUBINT>;
       c-name: "nub_query_breakpoint";
end debugger-nub-interface;

define debugger-nub-interface nub-wait-for-stop-reason-with-timeout
       parameter nub              :: <NUB>;
       parameter timeout          :: <NUBINT>;
       output parameter event     :: <NUBINT-POINTER>;
       c-name: "nub_wait_for_stop_reason_with_timeout";
end debugger-nub-interface;

define debugger-nub-interface nub-profile-wait-for-stop-reason-with-timeout
       parameter nub              :: <NUB>;
       parameter timeout          :: <NUBINT>;
       parameter profile-interval :: <NUBINT>;
       output parameter event     :: <NUBINT-POINTER>;
       c-name: "nub_profile_wait_for_stop_reason_with_timeout";
end debugger-nub-interface;

define debugger-nub-interface nub-inform-profiling-started
       parameter nub              :: <NUB>;
       c-name: "nub_inform_profiling_started";
end debugger-nub-interface;

define debugger-nub-interface nub-inform-profiling-stopped
       parameter nub              :: <NUB>;
       c-name: "nub_inform_profiling_stopped";
end debugger-nub-interface;

define debugger-nub-interface nub-can-receive-first-chance
       parameter nub              :: <NUB>;
       parameter ecode            :: <NUBINT>;
       result yes-or-no           :: <NUBINT>;
       c-name: "nub_can_receive_first_chance";
end debugger-nub-interface;

define debugger-nub-interface nub-set-first-chance
       parameter nub              :: <NUB>;
       parameter ecode            :: <NUBINT>;
       c-name: "nub_set_first_chance";
end debugger-nub-interface;

define debugger-nub-interface nub-unset-first-chance
       parameter nub              :: <NUB>;
       parameter ecode            :: <NUBINT>;
       c-name: "nub_unset_first_chance";
end debugger-nub-interface;

define debugger-nub-interface nub-thread-stop-information
       parameter nub              :: <NUB>;
       parameter t                :: <NUBTHREAD>;
       output parameter fchance   :: <NUBINT-POINTER>;
       output parameter fstart    :: <NUBINT-POINTER>;
       output parameter raddr     :: <TARGET-ADDRESS-POINTER>;
       result ecode               :: <NUBINT>;
       c-name: "nub_thread_stop_information";
end debugger-nub-interface;

define debugger-nub-interface nub-wait-for-stop-reason-no-timeout
       parameter nub              :: <NUB>;
       output parameter event     :: <NUBINT-POINTER>;
       c-name: "nub_wait_for_stop_reason_no_timeout";
end debugger-nub-interface;

define debugger-nub-interface nub-profile-wait-for-stop-reason-no-timeout
       parameter nub              :: <NUB>;
       parameter profile-interval :: <NUBINT>;
       output parameter event     :: <NUBINT-POINTER>;
       c-name: "nub_profile_wait_for_stop_reason_no_timeout";
end debugger-nub-interface;

define debugger-nub-interface nub-stop-reason-process
       parameter nub              :: <NUB>;
       result process             :: <NUB>;
       c-name: "nub_stop_reason_process";
end debugger-nub-interface;

define debugger-nub-interface nub-first-hard-coded-breakpoint
       parameter nub              :: <NUB>;
       result is-first            :: <NUBINT>;
       c-name: "nub_first_hard_coded_breakpoint";
end debugger-nub-interface;

define debugger-nub-interface nub-stop-reason-thread
       parameter nub              :: <NUB>;
       result thread              :: <NUBTHREAD>;
       c-name: "nub_stop_reason_thread";
end debugger-nub-interface;

define debugger-nub-interface nub-stop-reason-process-exit-code
       parameter nub              :: <NUB>;
       result code                :: <NUBINT>;
       c-name: "nub_stop_reason_process_exit_code";
end debugger-nub-interface;

define debugger-nub-interface nub-stop-reason-thread-exit-code
       parameter nub              :: <NUB>;
       result code                :: <NUBINT>;
       c-name: "nub_stop_reason_thread_exit_code";
end debugger-nub-interface;


define debugger-nub-interface nub-stop-reason-library 
       parameter nub              :: <NUB>;
       result library             :: <NUBLIBRARY>;
       c-name: "nub_stop_reason_library";
end debugger-nub-interface;

define debugger-nub-interface nub-stop-reason-violation-op
       parameter nub              :: <NUB>;
       result op                  :: <NUBINT>;
       c-name: "nub_stop_reason_violation_op";
end debugger-nub-interface;

define debugger-nub-interface nub-exception-first-chance
       parameter nub              :: <NUB>;
       result int-answer          :: <NUBINT>;
       c-name: "nub_exception_first_chance";
end debugger-nub-interface;

define debugger-nub-interface nub-stop-reason-violation-address
       parameter nub              :: <NUB>;
       result addr                :: <TARGET-ADDRESS>;
       c-name: "nub_stop_reason_violation_address";
end debugger-nub-interface;

define debugger-nub-interface nub-stop-reason-exception-address
       parameter nub              :: <NUB>;
       result address             :: <TARGET-ADDRESS>;
       c-name: "nub_stop_reason_exception_address";
end debugger-nub-interface;

define debugger-nub-interface nub-stop-reason-debug-string-address
       parameter nub              :: <NUB>;
       result address             :: <TARGET-ADDRESS>;
       c-name: "nub_stop_reason_debug_string_address";
end debugger-nub-interface;

define debugger-nub-interface nub-stop-reason-debug-string-length
       parameter nub              :: <NUB>;
       result sz                  :: <NUBINT>;
       c-name: "nub_stop_reason_debug_string_length";
end debugger-nub-interface;

define debugger-nub-interface nub-stop-reason-debug-string-is-unicode
       parameter nub              :: <NUB>;
       result answer              :: <NUBINT>;
       c-name: "nub_stop_reason_debug_string_is_unicode";
end debugger-nub-interface;

define debugger-nub-interface nub-initialize-stack-vectors
       parameter nub              :: <NUB>;
       parameter thread           :: <NUBTHREAD>;
       result frame-count         :: <NUBINT>;
       c-name: "nub_initialize_stack_vectors";
end debugger-nub-interface;

define debugger-nub-interface nub-read-stack-vectors
       parameter nub              :: <NUB>;
       parameter thread           :: <NUBTHREAD>;
       parameter count            :: <NUBINT>;
       parameter fps              :: <TARGET-ADDRESS-POINTER>;
       parameter ips              :: <TARGET-ADDRESS-POINTER>;
       parameter ras              :: <TARGET-ADDRESS-POINTER>;
       c-name: "nub_read_stack_vectors";
end debugger-nub-interface;

define debugger-nub-interface nub-all-frame-lexicals
       parameter nub              :: <NUB>;
       parameter frame            :: <TARGET-ADDRESS>;
       parameter IP               :: <TARGET-ADDRESS>;
       output parameter first     :: <NUB-INDEX-POINTER>;
       output parameter last      :: <NUB-INDEX-POINTER>;
       output parameter lookups   :: <NUBHANDLE-POINTER>;
       c-name: "nub_all_frame_lexicals";
end debugger-nub-interface;

define debugger-nub-interface nub-register-interactive-code-segment
       parameter nub              :: <NUB>;
       parameter low-address      :: <TARGET-ADDRESS>;
       parameter high-address     :: <TARGET-ADDRESS>;
       c-name: "nub_register_interactive_code_segment";
end debugger-nub-interface;

define debugger-nub-interface nub-get-lexical-variable-name-length
       parameter nub              :: <NUB>;
       parameter lookups          :: <NUBHANDLE>;
       parameter lexical          :: <NUB-INDEX>;
       result length              :: <NUBINT>;
       c-name: "nub_get_lexical_variable_name_length";
end debugger-nub-interface;

define debugger-nub-interface nub-get-lexical-variable-name
       parameter nub              :: <NUB>;
       parameter lookups          :: <NUBHANDLE>;
       parameter lexical          :: <NUB-INDEX>;
       parameter buf-size         :: <NUBINT>;
       parameter buf              :: <C-string>;
       c-name: "nub_get_lexical_variable_name";
end debugger-nub-interface;

define debugger-nub-interface nub-lexical-variable-address
       parameter nub              :: <NUB>;
       parameter frame            :: <TARGET-ADDRESS>;
       parameter lookups          :: <NUBHANDLE>;
       parameter lexical          :: <NUB-INDEX>;
       output parameter reg?      :: <NUBINT-POINTER>;
       output parameter hireg     :: <NUB-INDEX-POINTER>;
       output parameter loreg     :: <NUB-INDEX-POINTER>;
       output parameter arg?      :: <NUBINT-POINTER>;
       result address             :: <TARGET-ADDRESS>;
       c-name: "nub_lexical_variable_address";
end debugger-nub-interface;

define debugger-nub-interface nub-lookup-symbol-name-length
       parameter nub              :: <NUB>;
       parameter lookups          :: <NUBHANDLE>;
       parameter symbol           :: <NUB-INDEX>;
       result length              :: <NUBINT>;
       c-name: "nub_lookup_symbol_name_length";
end debugger-nub-interface;

define debugger-nub-interface nub-lookup-symbol-name
       parameter nub              :: <NUB>;
       parameter lookups          :: <NUBHANDLE>;
       parameter symbol           :: <NUB-INDEX>;
       parameter buf-size         :: <NUBINT>;
       parameter buf              :: <C-string>;
       c-name: "nub_lookup_symbol_name";
end debugger-nub-interface;

define debugger-nub-interface nub-lookup-symbol-address
       parameter nub              :: <NUB>;
       parameter lookups          :: <NUBHANDLE>;
       parameter symbol           :: <NUB-INDEX>;
       result address             :: <TARGET-ADDRESS>;
       c-name: "nub_lookup_symbol_address";
end debugger-nub-interface;

define debugger-nub-interface nub-lookup-function-debug-start
       parameter nub              :: <NUB>;
       parameter lookups          :: <NUBHANDLE>;
       parameter symbol           :: <NUB-INDEX>;
       result address             :: <TARGET-ADDRESS>;
       c-name: "nub_lookup_function_debug_start";
end debugger-nub-interface;

define debugger-nub-interface nub-lookup-function-debug-end
       parameter nub              :: <NUB>;
       parameter lookups          :: <NUBHANDLE>;
       parameter symbol           :: <NUB-INDEX>;
       result address             :: <TARGET-ADDRESS>;
       c-name: "nub_lookup_function_debug_end";
end debugger-nub-interface;

/* We don't use this one any more.
define debugger-nub-interface nub-lookup-symbol-language
       parameter nub              :: <NUB>;
       parameter lookups          :: <NUBHANDLE>;
       parameter symbol           :: <NUB-INDEX>;
       result lang                :: <NUBINT>;
       c-name: "nub_lookup_symbol_language_code";
end debugger-nub-interface;
*/

define debugger-nub-interface nub-lookup-function-end
       parameter nub              :: <NUB>;
       parameter lookups          :: <NUBHANDLE>;
       parameter symbol           :: <NUB-INDEX>;
       result addr                :: <TARGET-ADDRESS>;
       c-name: "nub_lookup_function_end";
end debugger-nub-interface;

define debugger-nub-interface nub-symbol-is-function
       parameter nub              :: <NUB>;
       parameter lookups          :: <NUBHANDLE>;
       parameter symbol           :: <NUB-INDEX>;
       result answer              :: <NUBINT>;
       c-name: "nub_symbol_is_function";
end debugger-nub-interface;

define debugger-nub-interface nub-nearest-symbols
       parameter nub              :: <NUB>;
       parameter address          :: <TARGET-ADDRESS>;
       output parameter library   :: <NUBLIBRARY-POINTER>;
       output parameter lookups   :: <NUBHANDLE-POINTER>;
       result success             :: <NUBINT>;
       c-name: "nub_nearest_symbols";
end debugger-nub-interface;

define debugger-nub-interface nub-closest-symbol
       parameter nub              :: <NUB>;
       parameter address          :: <TARGET-ADDRESS>;
       output parameter lib       :: <NUBLIBRARY-POINTER>;
       output parameter actual    :: <TARGET-ADDRESS-POINTER>;
       output parameter offset    :: <NUBINT-POINTER>;
       output parameter nameln    :: <NUBINT-POINTER>;
       output parameter type      :: <NUBINT-POINTER>;
       output parameter is-fun    :: <NUBINT-POINTER>;
       output parameter debugs    :: <TARGET-ADDRESS-POINTER>;
       output parameter debuge    :: <TARGET-ADDRESS-POINTER>;
       output parameter lang      :: <NUBINT-POINTER>;
       output parameter last      :: <TARGET-ADDRESS-POINTER>;
       result found               :: <NUBINT>;
       c-name: "nub_closest_symbol";
end debugger-nub-interface;

define debugger-nub-interface nub-function-bounding-addresses
       parameter nub              :: <NUB>;
       parameter address          :: <TARGET-ADDRESS>;
       output parameter lower     :: <TARGET-ADDRESS-POINTER>;
       output parameter upper     :: <TARGET-ADDRESS-POINTER>;
       c-name: "nub_function_bounding_addresses";
end debugger-nub-interface;

define debugger-nub-interface nub-closest-symbol-name
       parameter nub              :: <NUB>;
       parameter buf-size         :: <NUBINT>;
       parameter buf              :: <C-string>;
       c-name: "nub_closest_symbol_name";
end debugger-nub-interface;

define debugger-nub-interface nub-find-symbol-in-library
       parameter nub              :: <NUB>;
       parameter library          :: <NUBLIBRARY>;
       parameter name-length      :: <NUBINT>;
       parameter name             :: <C-string>;
       output parameter address   :: <TARGET-ADDRESS-POINTER>;
       output parameter type      :: <NUBINT-POINTER>;
       output parameter is-fun    :: <NUBINT-POINTER>;
       output parameter debugs    :: <TARGET-ADDRESS-POINTER>;
       output parameter debuge    :: <TARGET-ADDRESS-POINTER>;
       output parameter lang      :: <NUBINT-POINTER>;
       output parameter last-addr :: <TARGET-ADDRESS-POINTER>;
       result found               :: <NUBINT>;
       c-name: "nub_find_symbol_in_library";
end debugger-nub-interface;

define debugger-nub-interface nub-dispose-lookups
       parameter nub              :: <NUB>;
       parameter lookups          :: <NUBHANDLE>;
       c-name: "nub_dispose_lookups";
end debugger-nub-interface;

define debugger-nub-interface nub-resolve-source-location
       parameter nub              :: <NUB>;
       parameter lib              :: <NUBLIBRARY>;
       parameter filename         :: <C-string>;
       parameter linenumber       :: <NUBINT>;
       parameter column           :: <NUBINT>;
       output parameter valid?    :: <NUBINT-POINTER>;
       output parameter path?     :: <NUBINT-POINTER>;
       output parameter search    :: <NUBHANDLE-POINTER>;
       output parameter exact?    :: <NUBINT-POINTER>;
       result code-location       :: <TARGET-ADDRESS>;
       c-name: "nub_resolve_source_location";
end debugger-nub-interface;

define debugger-nub-interface nub-fetch-source-locations
       parameter nub              :: <NUB>;
       parameter start-addr       :: <TARGET-ADDRESS>;
       parameter end-addr         :: <TARGET-ADDRESS>;
       result lookups             :: <NUBHANDLE>;
       c-name: "nub_fetch_source_locations";
end debugger-nub-interface;

define debugger-nub-interface nub-source-location-address
       parameter nub              :: <NUB>;
       parameter lookups          :: <NUBHANDLE>;
       parameter index            :: <NUB-INDEX>;
       result address             :: <NUBINT>;
       c-name: "nub_source_location_address";
end debugger-nub-interface;

define debugger-nub-interface nub-source-location-linenumber
       parameter nub              :: <NUB>;
       parameter lookups          :: <NUBHANDLE>;
       parameter index            :: <NUB-INDEX>;
       result linenumber          :: <NUBINT>;
       c-name: "nub_source_location_linenumber";
end debugger-nub-interface;

define debugger-nub-interface nub-source-location-filename-length
       parameter nub              :: <NUB>;
       parameter lookups          :: <NUBHANDLE>;
       result length              :: <NUBINT>;
       c-name: "nub_source_location_filename_length";
end debugger-nub-interface;

define debugger-nub-interface nub-source-location-filename
       parameter nub              :: <NUB>;
       parameter lookups          :: <NUBHANDLE>;
       parameter size             :: <NUBINT>;
       parameter buf              :: <C-string>;
       c-name: "nub_source_location_filename";
end debugger-nub-interface;

define debugger-nub-interface nub-number-of-source-locations
       parameter nub              :: <NUB>;
       parameter lookups          :: <NUBHANDLE>;
       result count               :: <NUBINT>;
       c-name: "nub_number_of_source_locations";
end debugger-nub-interface;

define debugger-nub-interface nub-dispose-source-locations
       parameter nub              :: <NUB>;
       parameter lookups          :: <NUBHANDLE>;
       c-name: "nub_dispose_source_locations";
end debugger-nub-interface;

define debugger-nub-interface nub-interpret-instruction-at-current-location
       parameter nub              :: <NUB>;
       parameter thread           :: <NUBTHREAD>;
       output parameter flow      :: <NUBINT-POINTER>;
       output parameter dest      :: <TARGET-ADDRESS-POINTER>;
       output parameter size      :: <NUBINT-POINTER>;
       c-name: "nub_interpret_instruction_at_current_location";
end debugger-nub-interface;

define debugger-nub-interface nub-dylan-calculate-step-into
       parameter nub              :: <NUB>;
       parameter thread           :: <NUBTHREAD>;
       output parameter flive     :: <NUBINT-POINTER>;
       output parameter ok?       :: <NUBINT-POINTER>;
       result destination         :: <TARGET-ADDRESS>;
       c-name: "calculate_step_into_destination";
end debugger-nub-interface;

define debugger-nub-interface nub-dylan-thread-environment-block-address
       parameter nub              :: <NUB>;
       parameter thread           :: <NUBTHREAD>;
       output parameter valid?    :: <NUBINT-POINTER>;
       result teb                 :: <TARGET-ADDRESS>;
       c-name: "nub_dylan_thread_environment_block_address";
end debugger-nub-interface;

define debugger-nub-interface nub-dylan-thread-mv-buffer-live
       parameter nub              :: <NUB>;
       parameter thread           :: <NUBTHREAD>;
       result code                :: <NUBINT>;
       c-name: "nub_dylan_thread_mv_buffer_live";
end debugger-nub-interface;

define debugger-nub-interface nub-older-stack-frame
       parameter nub              :: <NUB>;
       parameter this-one         :: <TARGET-ADDRESS>;
       parameter than-this-one    :: <TARGET-ADDRESS>;
       result answer              :: <NUBINT>;
       c-name: "nub_older_stack_frame";
end debugger-nub-interface;

define debugger-nub-interface nub-dylan-current-function
       parameter nub              :: <NUB>;
       parameter thread           :: <NUBTHREAD>;
       result f                   :: <TARGET-ADDRESS>;
       c-name: "nub_dylan_current_function";
end debugger-nub-interface;

define debugger-nub-interface nub-perform-absolute-relocation
       parameter nub              :: <NUB>;
       parameter ra               :: <TARGET-ADDRESS>;
       parameter da               :: <TARGET-ADDRESS>;
       result errcode             :: <NUBINT>;
       c-name: "nub_perform_absolute_relocation";
end debugger-nub-interface;

define debugger-nub-interface nub-perform-relative-relocation
       parameter nub              :: <NUB>;
       parameter ra               :: <TARGET-ADDRESS>;
       parameter da               :: <TARGET-ADDRESS>;
       result errcode             :: <NUBINT>;
       c-name: "nub_perform_relative_relocation";
end debugger-nub-interface;
