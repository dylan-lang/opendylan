Module:        access-path-implementation
Synopsis:      Callbacks to the Access Path
Author:        Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// Callback functions for the Debugger NUB to do explicit
// stop-reason handling for clients during spy calls

// This registers a thread created as part of a spy call in
// the client

define c-callable-wrapper create-thread-stop-reason-handler-wrapper
    of create-thread-stop-reason-handler
  parameter process          :: <NUBPROCESS>;
  parameter thread           :: <NUBTHREAD>;
  parameter priority         :: <NUBINT>;
  c-name: "create_thread_stop_reason_handler";
end;

ignore(create-thread-stop-reason-handler-wrapper);
