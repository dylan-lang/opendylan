module:      debugger-server
synopsis:    FFI definitions for interfaces to the server
author:      Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define constant <NUBHANDLE>            = <C-both-int>;
define constant <NUBINT>               = <C-int>;
define constant <NUBPROCESS>           = <NUBHANDLE>;
define constant <NUB>                  = <NUBHANDLE>;
define constant <NUB-INDEX>            = <C-int>;


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

define C-function local-process-actual-identifier
       parameter index            :: <NUB-INDEX>;
       result proc-id             :: <NUB>;
       c-name: "local_process_actual_identifier";
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
