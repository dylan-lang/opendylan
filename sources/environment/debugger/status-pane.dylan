Module:    environment-debugger
Author:    Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// ---*** DEBUGGER: what is effect on noting-progress?
/// ---*** DEBUGGER: maybe display debug level here?

define pane <debugger-status-bar> ()
  pane %status-message-pane (pane)
    make(<label>,
	 label: "",
	 width: 100, min-width: 100, max-width: $fill,
	 documentation: "Message Area");
  pane %thread-status-pane (pane)
    make(<label>,
	 label: "",
	 width: 100, min-width: 100, max-width: $fill,
	 documentation: "Thread Status");
  pane %application-status-pane (pane)
    make(<label>,
	 label: "",
	 width: 100, min-width: 100, max-width: $fill,
	 documentation: "Application Status");
  pane %updated-pane (pane)
    make(<label>,
	 label: "",
	 width: 50, min-width: 50, max-width: 50,
	 documentation: "Needs Refreshing?");
  pane %enabled-pane (pane)
    make(<label>,
	 label: "",
	 width: 50, min-width: 50, max-width: 50,
	 documentation: "Currently Disabled?");
  layout (pane)
    make-debugger-status-bar(pane);
end pane <debugger-status-bar>;

/// MAKE-DEBUGGER-STATUS-BAR (internal)

define function make-debugger-status-bar 
    (status-bar :: <debugger-status-bar>)
 => (status-bar :: <status-bar>)
  let framem = frame-manager(status-bar);
  with-frame-manager (framem)
    make(<status-bar>,
	 label-pane: status-bar.%status-message-pane,
	 children:
	   vector(status-bar.%status-message-pane,
		  status-bar.%thread-status-pane,
		  status-bar.%application-status-pane,
		  status-bar.%updated-pane,
		  status-bar.%enabled-pane))
  end;
end function make-debugger-status-bar;


/// UPDATE-DEBUGGER-STATUS-BAR (internal)

define function update-debugger-status-bar (debugger :: <debugger>) => ()
  let project :: <project-object> = debugger.frame-project;
  let thread :: false-or(<thread-object>) = debugger.debugger-thread;
  let status-bar = debugger.debugger-status-bar;
  status-bar.%status-message-pane.gadget-label := "";
  status-bar.%thread-status-pane.gadget-label
    := if (thread)
	 let state = thread-state(project, thread);
	 format-to-string("%s (was %s)",
			  frame-default-object-name(debugger, thread),
			  thread-state-label(project, state))
       else
	 "No thread"
       end if;
  status-bar.%application-status-pane.gadget-label
    := format-to-string("%s (%s)",
			environment-object-primitive-name(project, project),
			application-state-label(project));
  status-bar.%updated-pane.gadget-label
    := if (debugger.debugger-updated?)
	 ""
       else
	 "Outdated"
       end if;
  status-bar.%enabled-pane.gadget-label
    := if (debugger.debugger-enabled?)
	 ""
       else
	 "Disabled"
       end if;
end function update-debugger-status-bar;
