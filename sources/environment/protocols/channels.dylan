Module:    environment-protocols
Synopsis:  Environment protocols
Author:    Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// PROJECT-CHANNEL
///
/// Some channel stuff for communicating project messages to interested
/// parties.

define constant $project-channel :: <channel> = make(<channel>);

/// Current messages:
///
/// MESSAGE			ARGUMENTS PASSED
///
/// #"make-application"		project :: <project-object>


/// Some useful project message classes

/// Messages about projects

define abstract class <project-message> (<object>)
end class <project-message>;

define abstract class <project-object-message> (<project-message>)
  sealed constant slot message-project :: <project-object>,
    required-init-keyword: project:;
end class <project-object-message>;

define sealed domain make (singleton(<project-object-message>));
define sealed domain initialize (<project-object-message>);

define class <project-opened-message> (<project-object-message>)
end class <project-opened-message>;

define class <project-closed-message> (<project-object-message>)
end class <project-closed-message>;

define class <project-now-active-message> (<project-object-message>)
end class <project-now-active-message>;

define class <no-active-project-message> (<project-message>)
end class <no-active-project-message>;

define class <project-sources-updated-message> (<project-object-message>)
end class <project-sources-updated-message>;


/// Messages about compilation

define class <project-database-updated-message> (<project-object-message>)
end class <project-database-updated-message>;

define class <project-warnings-updated-message> (<project-object-message>)
end class <project-warnings-updated-message>;


/// Messages about breakpoints

define abstract class <breakpoint-state-change-message>
    (<project-object-message>)
  sealed constant slot message-breakpoint-state :: <breakpoint-state>,
    required-init-keyword: state:;
end class <breakpoint-state-change-message>;

define class <all-breakpoints-state-change-message>
    (<breakpoint-state-change-message>)
end class <all-breakpoints-state-change-message>;

define class <single-breakpoint-state-change-message>
    (<breakpoint-state-change-message>)
  sealed constant slot message-breakpoint :: <breakpoint-object>,
    required-init-keyword: breakpoint:;
end class <single-breakpoint-state-change-message>;

define class <breakpoint-state-changes-failed-message>
    (<breakpoint-state-change-message>)
  sealed constant slot message-breakpoints
      :: <sequence> /* of: <breakpoint-object> */,
    required-init-keyword: breakpoints:;
end class <breakpoint-state-changes-failed-message>;


/// Messages about profiling

define abstract class <profiling-message> (<project-object-message>)
end class <profiling-message>;

define class <profiling-state-change-message> (<profiling-message>)
  sealed constant slot message-enabled? :: <boolean>,
    required-init-keyword: enabled?:;
end class <profiling-state-change-message>;


/// Messages about applications

define abstract class <application-message>
    (<project-object-message>)
end class <application-message>;

define class <run-application-requested-message>
    (<application-message>)
end class <run-application-requested-message>;

define class <run-application-failed-message>
    (<application-message>)
end class <run-application-failed-message>;

define class <application-initialized-message>
    (<application-message>)
end class <application-initialized-message>;

define class <application-state-changed-message>
    (<application-message>)
end class <application-state-changed-message>;

define class <application-threads-changed-message> (<application-message>)
end class <application-threads-changed-message>;


/// Thread messages

define abstract class <thread-message> (<application-message>)
  sealed constant slot message-thread :: <thread-object>,
    required-init-keyword: thread:;
end class <thread-message>;

define class <thread-interactive-warnings-message> (<thread-message>)
  sealed constant slot message-transaction-id :: <object>,
    required-init-keyword: transaction-id:;
  sealed constant slot message-warnings :: <object>,
    required-init-keyword: warnings:;
end class <thread-interactive-warnings-message>;
