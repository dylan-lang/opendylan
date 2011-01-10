Module:    environment-application-commands
Synopsis:  The application commands provided by the environment
Author:	   Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Stack context

define method ensure-context-stack-frame
    (context :: <environment-context>)
 => (frame :: <stack-frame-object>)
  let project = context.context-project;
  let application-context = context.context-application-context;
  let old-frame = application-context.context-stack-frame;
  let thread = application-context.context-thread;
  let stack = thread-complete-stack-trace(project, thread);
  if (old-frame & member?(old-frame, stack))
    old-frame
  else
    assert(~empty?(stack),
	   "The thread stack was unexpectedly empty!");
    let frame = stack[0];
    application-context.context-stack-frame := frame;
    frame
  end
end method ensure-context-stack-frame;

define method ensure-context-bug-report
    (context :: <environment-context>)
 => (report :: <bug-report>)
  let application-context = context.context-application-context;
  application-context.context-bug-report
    | begin
	application-context.context-bug-report
	  := make(<bug-report>,
		  project: context.context-project,
		  format:  #"text")
      end
end method ensure-context-bug-report;


/// Stack properties

define class <current-frame-property> (<application-property>)
end class <current-frame-property>;

define command-property frame => <current-frame-property>
  (summary:       "Current stack frame",
   documentation: "The current stack frame.",
   type:          <stack-frame-object>)
end command-property frame;

define method show-property
    (context :: <environment-context>, property :: <current-frame-property>)
 => ()
  let application-context = context.context-application-context;
  let project = context.context-project;
  let thread = application-context.context-thread;
  let frame = context.ensure-context-stack-frame;
  let stream = context.context-server.server-output-stream;
  let report = context.ensure-context-bug-report;
  let index = stack-frame-index(project, thread, frame);
  format(stream, "#%d ", index + 1);
  write-bug-report-stack-frame(stream, report, frame, show-variables?: #f)
end method show-property;

define method set-property
    (context :: <environment-context>, property :: <current-frame-property>,
     frame :: <stack-frame-object>,
     #key save?)
 => ()
  let application-context = context.context-application-context;
  application-context.context-stack-frame := frame
end method set-property;


/// Up command

define class <thread-up-command> (<thread-command>)
  constant slot %count :: <integer> = 1,
    init-keyword: count:;
end class <thread-up-command>;

define command-line up => <thread-up-command>
    (summary:       "selects a frame further up the stack",
     documentation: "Selects a frame further up the stack.")
  optional count :: <integer> = "number of frames to move up";
end command-line up;

define method do-execute-command
    (context :: <environment-context>, command :: <thread-up-command>)
 => ()
  let application-context = context.context-application-context;
  let project = context.context-project;
  let thread = application-context.context-thread;
  let frame = context.ensure-context-stack-frame;
  let index = stack-frame-index(project, thread, frame);
  let new-index = max(index - command.%count, 0);
  let new-frame = find-indexed-stack-frame(project, thread, new-index);
  context-named-property(context, #"frame") := new-frame
end method do-execute-command;


/// Down command

define class <thread-down-command> (<thread-command>)
  constant slot %count :: <integer> = 1,
    init-keyword: count:;
end class <thread-down-command>;

define command-line down => <thread-down-command>
    (summary:       "selects a frame further down the stack",
     documentation: "Selects a frame further down the stack.")
  optional count :: <integer> = "number of frames to move down";
end command-line down;

define method do-execute-command
    (context :: <environment-context>, command :: <thread-down-command>)
 => ()
  let application-context = context.context-application-context;
  let project = context.context-project;
  let thread = application-context.context-thread;
  let frame = context.ensure-context-stack-frame;
  let index = stack-frame-index(project, thread, frame);
  let stack = thread-complete-stack-trace(project, thread);
  let new-index = min(index + command.%count, stack.size - 1);
  let new-frame = find-indexed-stack-frame(project, thread, new-index);
  context-named-property(context, #"frame") := new-frame
end method do-execute-command;


/// Top command

define class <thread-top-command> (<thread-command>)
end class <thread-top-command>;

define command-line top => <thread-top-command>
    (summary:       "selects the top stack frame",
     documentation: "Selects the top stack frame.")
end command-line top;

define method do-execute-command
    (context :: <environment-context>, command :: <thread-top-command>)
 => ()
  let application-context = context.context-application-context;
  let project = context.context-project;
  let thread = application-context.context-thread;
  let new-frame = find-indexed-stack-frame(project, thread, 0);
  context-named-property(context, #"frame") := new-frame
end method do-execute-command;


/// Bottom command

define class <thread-bottom-command> (<thread-command>)
end class <thread-bottom-command>;

define command-line bottom => <thread-bottom-command>
    (summary:       "selects the bottom stack frame",
     documentation: "Selects the bottom stack frame.")
end command-line bottom;

define method do-execute-command
    (context :: <environment-context>, command :: <thread-bottom-command>)
 => ()
  let application-context = context.context-application-context;
  let project = context.context-project;
  let thread = application-context.context-thread;
  let stack = thread-complete-stack-trace(project, thread);
  let new-index = stack.size - 1;
  let new-frame = find-indexed-stack-frame(project, thread, new-index);
  context-named-property(context, #"frame") := new-frame
end method do-execute-command;


/// Backtrace command

define class <thread-backtrace-command> (<thread-command>)
  constant slot %count :: <integer> = 20,
    init-keyword: count:;
  constant slot %all? :: <boolean> = #f,
    init-keyword: all?:;
end class <thread-backtrace-command>;

define command-line backtrace => <thread-backtrace-command>
    (summary:       "displays the stack backtrace",
     documentation: "Displays the stack backtrace.")
  optional count :: <integer> = "number of frames to display";
  flag all = "display all of the stack frames [off by default]";
end command-line backtrace;

define method do-execute-command
    (context :: <environment-context>, command :: <thread-backtrace-command>)
 => ()
  let application-context = context.context-application-context;
  let project = context.context-project;
  let thread = application-context.context-thread;
  let frame = context.ensure-context-stack-frame;
  let index = stack-frame-index(project, thread, frame);
  let count = command.%count;
  let report = context.ensure-context-bug-report;
  let stream = context.context-server.server-output-stream;
  let all? = command.%all?;
  write-bug-report-thread-backtrace
    (stream, report, thread, 
     start: if (all?) 0 else index end, 
     end:   unless (all?) index + count end)
end method do-execute-command;


///---*** To do...

/*
  :more
*/


/// Stack commands

define command-group stack into environment
    (summary: "stack commands",
     documentation: "Commands to handle an applications stack.")
  property frame;
  command  up;
  command  down;
  command  top;
  command  bottom;
  command  backtrace;
  alias bt = backtrace;
end command-group stack;
