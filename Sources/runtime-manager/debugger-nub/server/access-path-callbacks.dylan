Module:        remote-nub
Synopsis:      Callbacks for the CORBA Debugger Nub of the Remote Debugger
Author:        Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define c-callable-wrapper create-thread-stop-reason-handler-wrapper
    of create-thread-stop-reason-handler
  parameter process          :: <NUBPROCESS>;
  parameter thread           :: <NUBTHREAD>;
  parameter priority         :: <NUBINT>;
  c-name: "create_thread_stop_reason_handler";
end;

ignore(create-thread-stop-reason-handler-wrapper);

define c-callable-wrapper debugger-message-wrapper
    of debugger-message
  parameter message           :: <C-string>;
  parameter arg1              :: <TARGET-ADDRESS>;
  parameter arg2              :: <TARGET-ADDRESS>;
  c-name: "debugger_message"
end;

ignore(debugger-message-wrapper);

define c-callable-wrapper nub-debug-message-wrapper
    of nub-debug-message
  parameter message           :: <C-string>;
  parameter arg1              :: <TARGET-ADDRESS>;
  parameter arg2              :: <TARGET-ADDRESS>;
  c-name: "nub_debug_message"
end;

ignore(nub-debug-message-wrapper);

define c-callable-wrapper debugger-error-wrapper
    of debugger-error
  parameter message           :: <C-string>;
  parameter arg1              :: <TARGET-ADDRESS>;
  parameter arg2              :: <TARGET-ADDRESS>;
  c-name: "debugger_error"
end;

ignore(debugger-error-wrapper);



define inline function create-thread-stop-reason-handler
    (process, thread, priority) => ()
  Rtmgr/AccessPath/create-thread-stop-reason-handler
  (*remote-debugger-nub*.nub-access-path,
   import-<abstract-integer>(process),
   import-<abstract-integer>(thread),
   priority)
end function;

define inline function debugger-message
    (string :: <string>, arg1, arg2) => ()
  Rtmgr/AccessPath/debugger-message
  (*remote-debugger-nub*.nub-access-path, string,
   import-<abstract-integer>(arg1), import-<abstract-integer>(arg2))
end function;

define inline function nub-debug-message
    (string :: <string>, arg1, arg2) => ()
  Rtmgr/AccessPath/nub-debug-message
  (*remote-debugger-nub*.nub-access-path, string,
   import-<abstract-integer>(arg1), import-<abstract-integer>(arg2))
end function;

define inline function debugger-error
    (string :: <string>, arg1, arg2) => ()
  Rtmgr/AccessPath/debugger-error
  (*remote-debugger-nub*.nub-access-path, string,
   import-<abstract-integer>(arg1), import-<abstract-integer>(arg2))
end function;
