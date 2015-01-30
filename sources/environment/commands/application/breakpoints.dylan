Module:    environment-application-commands
Synopsis:  The application commands provided by the environment
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Breakpoint argument parsing

define sideways method parameter-type-name
    (type == <breakpoint-object>) => (name :: <string>)
  "breakpoint"
end method parameter-type-name;

define sideways method parse-next-argument
    (context :: <environment-context>, type == <breakpoint-object>,
     text :: <string>,
     #key start :: <integer> = 0, end: stop = #f)
 => (value :: <breakpoint-object>, next-index :: <integer>)
  let (index, next-index) = string-to-integer(text, start: start, end: stop);
  if (index)
    block (return)
      let project = context.context-project;
      let breakpoint = project & find-indexed-breakpoint(project, index);
      if (breakpoint)
        values(breakpoint, next-index)
      else
        parse-error("No breakpoint %d", index)
      end
    end
  else
    parse-error("Missing breakpoint argument")
  end
end method parse-next-argument;

define method find-indexed-breakpoint
    (project :: <project-object>, index :: <integer>)
 => (breakpoint :: false-or(<breakpoint-object>))
  let application = project.project-application;
  if (application)
    block (return)
      let breakpoints = project.project-breakpoints;
      for (breakpoint :: <breakpoint-object> in breakpoints,
           breakpoint-index :: <integer> from 1)
        if (index == breakpoint-index) return(breakpoint) end
      end;
      #f
    end
  end
end method find-indexed-breakpoint;


/// Debugging properties

// Breakpoints property

define class <breakpoints-property> (<project-property>)
end class <breakpoints-property>;

define command-property breakpoints => <breakpoints-property>
  (summary:       "Application breakpoints",
   documentation: "The current contents of the application's breakpoints.")
end command-property breakpoints;

define method show-property
    (context :: <environment-context>, property :: <breakpoints-property>)
 => ()
  let project = context.context-project;
  let module = context.context-project-context.context-module;
  let breakpoints = project.project-breakpoints;
  let stream = context.context-server.server-output-stream;
  if (~empty?(breakpoints))
    for (breakpoint in breakpoints,
         index :: <integer> from 1)
      format(stream, "%d: ", index);
      select (breakpoint by instance?)
        <source-location-breakpoint-object> =>
          print-source-location(stream, breakpoint.breakpoint-object);
        <environment-object-breakpoint-object> =>
          let object = breakpoint.breakpoint-object;
          print-environment-object-name
            (stream, project, object, namespace: module);
      end;
      new-line(stream)
    end
  else
    message(context, "No breakpoints")
  end
end method show-property;


/// Break application command

define class <break-application-command> (<project-command>)
  constant slot %object :: false-or(<definition-object>) = #f,
    init-keyword: object:;
  constant slot %file :: false-or(<file-locator>) = #f,
    init-keyword: file:;
  constant slot %line :: false-or(<integer>) = #f,
    init-keyword: line:;
end class <break-application-command>;

define command-line break => <break-application-command>
    (summary:       "sets a breakpoint",
     documentation: "Sets a breakpoint in the project's application.")
  optional object :: <definition-object> = "object to break on entry to";
  keyword file :: <file-locator> = "file to set a breakpoint in";
  keyword line :: <integer>      = "line to set a breakpoint at";
end command-line break;

define method do-execute-command
    (context :: <environment-context>, command :: <break-application-command>)
 => ()
  let project = context.context-project;
  let object = command.%object;
  let file = command.%file;
  let line = command.%line;
  case
    object =>
      select (object by instance?)
        <function-object>, <class-object> =>
          make(<breakpoint-object>, project: project, object: object);
        otherwise =>
          command-error("Cannot set breakpoint for %s",
                        environment-object-display-name(project, object, #f));
      end;
    file | line =>
      if (file & line)
        command-error("Line breakpointing not yet implemented!")
      else
        command-error("You must supply both of the /file and /line arguments")
      end;
  end
end method do-execute-command;


/// Trace command

define class <trace-command> (<project-command>)
  constant slot %function :: false-or(<dylan-function-object>) = #f,
    init-keyword: function:;
end class <trace-command>;

define command-line trace => <trace-command>
    (summary:       "traces a function",
     documentation: "Traces a function in the project's application.")
  optional function :: <dylan-function-object> = "function to trace";
end command-line trace;

define method do-execute-command
    (context :: <environment-context>, command :: <trace-command>)
 => ()
  let project = context.context-project;
  let function = command.%function;
  if (function)
    trace-function(project, function)
  else
    error("Only function tracepoints are implemented...")
  end
end method do-execute-command;


/// Untrace command

define class <untrace-command> (<project-command>)
  constant slot %breakpoint :: false-or(<breakpoint-object>) = #f,
    init-keyword: breakpoint:;
end class <untrace-command>;

define command-line untrace => <untrace-command>
    (summary:       "untraces a function",
     documentation: "Untraces a function in the project's application.")
  optional breakpoint :: <breakpoint-object> = "breakpoint to untrace";
end command-line untrace;

define method do-execute-command
    (context :: <environment-context>, command :: <untrace-command>)
 => ()
  let project = context.context-project;
  let breakpoint = command.%breakpoint;
  if (breakpoint)
    destroy-breakpoint(breakpoint)
  else
    for (breakpoint in project.project-breakpoints)
      if (~breakpoint.breakpoint-stop?)
        destroy-breakpoint(breakpoint)
      end
    end
  end
end method do-execute-command;


///---*** To do

/*
  step into/over/out

  clear
  disable-breakpoint
  enable-breakpoint
  ignore-breakpoint-for-n
*/


/// Debugging commands

define command-group breakpoints into environment
    (summary: "breakpoint commands",
     documentation: "Commands to handle an application's breakpoints.")
  property breakpoints;
  command  break;
  command  trace;
  command  untrace;
end command-group breakpoints;
