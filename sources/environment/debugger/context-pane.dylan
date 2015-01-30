Module:    environment-debugger
Author:    Jason Trenouth, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// $DEBUGGER-CONTEXT-DOC

define constant $debugger-context-doc :: <string> = "Context";

/// <DEBUGGER-CONTEXT-PANE> (internal)

define sealed pane <debugger-context-pane> ()
  pane %context-pane (pane)
    make(<text-editor>,
         lines: 2, fixed-height?: #t,
         scroll-bars: #"vertical",
         read-only?: #t, tab-stop?: #t,
         documentation: $debugger-pane-tooltips? & $debugger-context-doc);
  layout (pane)
    pane.%context-pane;
end pane <debugger-context-pane>;

/// UPDATE-DEBUGGER-CONTEXT-PANE (internal)

define function update-context-pane
    (pane :: <debugger-context-pane>) => ()
  let debugger = pane.sheet-frame;
  let project = debugger.frame-project;
  let application = project.project-application;
  let thread = application & application.application-stopped-thread;
  let message
    = case
        thread & (thread ~= debugger.debugger-thread) =>
          format-to-string("Stopped in %s",
                           frame-default-object-name(debugger, thread));
        application =>
          application.application-stop-reason-message | "";
        otherwise =>
          "";
      end;
  pane.%context-pane.gadget-value := message
end function update-context-pane;
