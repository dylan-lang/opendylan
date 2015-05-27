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

define class <THREAD-CONTEXT> (<object>)
  constant slot thread-was-suspended-by-debugger? :: <boolean>,
    required-init-keyword: suspended?:;
  constant slot nub-descriptor :: <NUBHANDLE>,
    required-init-keyword: nub-descriptor:;
end class;

define C-function get-local-hostname-length
       result sz                  :: <NUBINT>;
       c-name: "get_local_hostname_length";
end C-function;

define C-function get-local-hostname
       parameter buf-size         :: <NUBINT>;
       parameter buffer           :: <C-string>;
       c-name: "get_local_hostname";
end C-function;

