module:      access-path-implementation
synopsis:    FFI definitions for interfaces to the server
author:      Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define macro connection-server-interface-definer
  { define connection-server-interface ?dylan-name:name ?specs end }
    => { define C-function ?dylan-name ?specs end }
specs:
  { } => { }
  { ?spec:* ; ...} => { ?spec ; ...}
end macro;

define C-function establish-connection-to-server
  parameter protocol         :: <NUBINT>;
  parameter network-address  :: <C-string>;
  output parameter success   :: <NUBINT-POINTER>;
  result server              :: <SERVER>;
  c-name: "establish_connection_to_server";
end C-function;

define connection-server-interface server-get-hostname-length
  parameter server         :: <SERVER>;
  output parameter success :: <NUBINT-POINTER>;
  result name-length       :: <NUBINT>;
  c-name: "server_get_hostname_length";
end connection-server-interface;

define connection-server-interface server-get-hostname
  parameter server         :: <SERVER>;
  parameter bufsize        :: <NUBINT>;
  parameter buffer         :: <C-string>;
  c-name: "server_get_hostname";
end connection-server-interface;

define connection-server-interface server-verify-password
  parameter server         :: <SERVER>;
  parameter bufsize        :: <NUBINT>;
  parameter buffer         :: <C-string>;
  result verified          :: <NUBINT>;
  c-name: "server_verify_password";
end connection-server-interface;

define connection-server-interface server-release-connection
  parameter server         :: <SERVER>;
  c-name: "server_release_connection";
end connection-server-interface;

define connection-server-interface open-remote-tether
  parameter server           :: <SERVER>;
  parameter command-len      :: <NUBINT>;
  parameter arg-len          :: <NUBINT>;
  parameter command          :: <C-string>;
  parameter arguments        :: <C-string>;
  parameter path-count       :: <NUBINT>;
  parameter paths            :: <C-string*>;
  parameter libsearch-count  :: <NUBINT>;
  parameter libsearch        :: <C-string*>;
  parameter workdirsz        :: <NUBINT>;
  parameter workingdir       :: <C-string>;
  parameter own-shell        :: <NUBINT>;
  output parameter success   :: <NUBINT-POINTER>;
  result tether              :: <NUB>;
  c-name: "open_remote_tether";
end connection-server-interface;

define connection-server-interface attach-remote-tether
       parameter server           :: <SERVER>;
       parameter process          :: <NUBPROCESS>;
       parameter proc-name-length :: <NUBINT>;
       parameter proc-name        :: <C-string>;
       parameter id-string-length :: <NUBINT>;
       parameter id-string        :: <C-string>;
       parameter path-count       :: <NUBINT>;
       parameter paths            :: <C-string*>;
       parameter system-JIT-sz    :: <NUBINT>;
       parameter system-JIT-info  :: <C-string>;
       output parameter success   :: <NUBINT-POINTER>;
       result tether              :: <NUB>;
       c-name: "attach_remote_tether";
end connection-server-interface;

define connection-server-interface update-server-process-list
       parameter server           :: <SERVER>;
       result count               :: <NUBINT>;
       c-name: "update_server_process_list";
end connection-server-interface;

define connection-server-interface server-process-nub-descriptor
       parameter server           :: <SERVER>;
       parameter index            :: <NUB-INDEX>;
       result length              :: <NUBPROCESS>;
       c-name: "server_process_nub_descriptor";
end connection-server-interface;

define connection-server-interface server-process-name-length
       parameter server           :: <SERVER>;
       parameter index            :: <NUB-INDEX>;
       result length              :: <NUBINT>;
       c-name: "server_process_name_length";
end connection-server-interface;

define connection-server-interface server-process-name
       parameter server           :: <SERVER>;
       parameter index            :: <NUB-INDEX>;
       parameter length           :: <NUBINT>;
       parameter buf              :: <C-string>;
       c-name: "server_process_name";
end connection-server-interface;

define connection-server-interface server-process-system-identifier-length
       parameter server           :: <SERVER>;
       parameter index            :: <NUB-INDEX>;
       result length              :: <NUBINT>;
       c-name: "server_process_system_identifier_length";
end connection-server-interface;

define connection-server-interface server-process-system-identifier
       parameter server           :: <SERVER>;
       parameter index            :: <NUB-INDEX>;
       parameter length           :: <NUBINT>;
       parameter buf              :: <C-string>;
       c-name: "server_process_system_identifier";
end connection-server-interface;

define C-function remote-debugger-nub-shutdown
   parameter nub             :: <NUB>;
   c-name: "remote_debugger_nub_shutdown";
end C-function;

