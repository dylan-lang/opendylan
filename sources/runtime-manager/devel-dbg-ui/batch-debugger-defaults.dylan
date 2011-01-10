module:    devel-dbg-ui
synopsis:  Default option settings for the batch debugger.
author:    Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// *RUNNING-IN-BATCH-MODE?*
//    Are we the console debugger or the batch debugger?

define variable *running-in-batch-mode?* = #t;


///// $DEBUGGER-INITIALIZATION-SCRIPT-NAME
//    The filename of the debugger's initialization script.

define constant $debugger-initialization-script-name = "bd-init";


///// *COMMAND-QUEUEING-ENABLED?*
//    Can this version of the debugger accept commands posted from
//    multiple threads, then?

define variable *command-queueing-enabled?* = #f;


///// *CURRENT-DEBUGGER-OPTIONS*
//    Used to hold the prevailing set of debugger options.

define variable *current-debugger-options* :: <debugger-option-set>
  = make(<debugger-option-set>,
         signalling-thread-detail: #"verbose",
         other-threads-detail: #"verbose",
         spawn-new-console?: #f,
         stack-trace-limit: 1000,
         large-object-threshhold: 50,
         show-debug-output?: #f,
         show-application-messages?: #f,
         show-dylan-debug-messages?: #f,
         stop-at-system-initialization?: #t);
