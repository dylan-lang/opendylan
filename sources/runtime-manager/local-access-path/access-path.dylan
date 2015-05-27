module:     access-path-implementation
synopsis:   Implementation of the <access-path> class
author:     Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define sideways method make-access-connection
    (ap :: <access-path>, conn :: <local-debugger-connection>,
     #key description = ap.access-path-application)
 => (conn :: <local-access-connection>)
  make(<local-access-connection>,
       debugger-connection: conn,
       description: description)
end method;

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
