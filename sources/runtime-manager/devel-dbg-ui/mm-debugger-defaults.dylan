module:    devel-dbg-ui
synopsis:  Default option settings for the console debugger.
author:    Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// *RUNNING-IN-BATCH-MODE?*
//    Are we the console debugger or the batch debugger?

define variable *running-in-batch-mode?* = #f;


///// $DEBUGGER-INITIALIZATION-SCRIPT-NAME
//    The filename of the debugger's initialization script.

define constant $debugger-initialization-script-name = "cd-init.cds";


///// *COMMAND-QUEUEING-ENABLED?*
//    Can this version of the debugger accept commands posted from
//    multiple threads, then?

define variable *command-queueing-enabled?* = #t;


///// *CURRENT-DEBUGGER-OPTIONS*
//    Used to hold the prevailing set of debugger options.

define variable *current-debugger-options* :: <debugger-option-set>
  = make(<debugger-option-set>,
         signalling-thread-detail: #"verbose",
         other-threads-detail: #"ignore",
         spawn-new-console?: #t,
         stack-trace-limit: 1000,
         large-object-threshhold: 50,
         show-debug-output?: #t,
         show-application-messages?: #t,
         show-dylan-debug-messages?: #t,
         stop-at-system-initialization?: #t);


