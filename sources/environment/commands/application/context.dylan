Module:    environment-application-commands
Synopsis:  The application commands provided by the environment
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <application-context> (<object>)
  slot context-stack-frame :: false-or(<stack-frame-object>) = #f;
  slot context-bug-report :: false-or(<bug-report>) = #f;
  slot context-thread :: false-or(<thread-object>) = #f;
  slot context-show-messages? :: <boolean> = #f;
  slot context-last-transaction-id = #f;
  slot context-interactive-warnings :: <sequence> = #[];
  slot context-start-command :: false-or(<start-application-command>) = #f;
  slot context-debug-request? :: <boolean> = #f;
end class <application-context>;

define method context-application-context
    (context :: <environment-context>)
 => (context :: <application-context>)
  let project-context = context.context-project-context;
  let properties = project-context.context-properties;
  get-property(properties, #"application", default: #f)
    | begin
        let application-context :: <application-context>
          = make(<application-context>);
        project-context.context-properties
          := put-property!(properties, #"application", application-context);
        application-context
      end
end method context-application-context;

define method context-application-tethered?
    (context :: <environment-context>) => (tethered? :: <boolean>)
  let project-context = context.context-project-context;
  let project = project-context & project-context.context-project;
  project & project.application-tethered?
end method context-application-tethered?;


/// Application command library

define constant $interactive-command-prompt = "? ";

define class <application-command-library> (<command-library>)
end class <application-command-library>;

define constant $application-command-library
  = make(<application-command-library>);

register-default-command-library($application-command-library);

define method command-library-prompt
    (context :: <environment-context>,
     library :: <application-command-library>)
 => (prompt :: false-or(<string>))
  if (context.context-application-tethered?)
    $interactive-command-prompt
  end
end method command-library-prompt;

define method command-library-default-command-class
    (context :: <environment-context>,
     library :: <application-command-library>)
 => (class :: false-or(subclass(<command>)))
  if (context.context-application-tethered?)
    <evaluate-code-command>
  end
end method command-library-default-command-class;


/// Useful command classes

define abstract class <application-command> (<project-command>)
end class <application-command>;

define abstract class <thread-command> (<application-command>)
end class <thread-command>;

define abstract class <application-property> (<project-property>)
end class <application-property>;

define method ensure-command-available
    (context :: <environment-context>, command :: <application-command>)
 => ()
  unless (context.context-application-tethered?)
    command-error("Application command '%s' requires an open application",
                  command-title(context, command))
  end
end method ensure-command-available;

define method ensure-property-available
    (context :: <server-context>, property :: <application-property>)
 => ()
  unless (context.context-application-tethered?)
    command-error("Application property '%s' requires an open application", property.command-info-title)
  end
end method ensure-property-available;


/// Thread argument parsing

define sideways method parameter-type-name
    (type == <thread-object>) => (name :: <string>)
  "thread"
end method parameter-type-name;

define sideways method parse-next-argument
    (context :: <environment-context>, type == <thread-object>,
     text :: <string>,
     #key start :: <integer> = 0, end: stop = #f)
 => (value :: <thread-object>, next-index :: <integer>)
  let (index, next-index) = string-to-integer(text, start: start, end: stop);
  if (index)
    block (return)
      let project = context.context-project;
      let thread = project & find-indexed-thread(project, index);
      if (thread)
        values(thread, next-index)
      else
        parse-error("No thread %d", index)
      end
    end
  else
    parse-error("Missing thread argument")
  end
end method parse-next-argument;

define method find-indexed-thread
    (project :: <project-object>, index :: <integer>)
 => (thread :: false-or(<thread-object>))
  let application = project.project-application;
  if (application)
    block (return)
      for (thread :: <thread-object> in application.application-threads)
        let thread-index = thread-index(application, thread);
        if (index == thread-index) return(thread) end
      end;
      #f
    end
  end
end method find-indexed-thread;


/// Restart argument parsing

define sideways method parameter-type-name
    (type == <restart-object>) => (name :: <string>)
  "restart"
end method parameter-type-name;

define sideways method parse-next-argument
    (context :: <environment-context>, type == <restart-object>,
     text :: <string>,
     #key start :: <integer> = 0, end: stop = #f)
 => (value :: <restart-object>, next-index :: <integer>)
  let (index, next-index) = string-to-integer(text, start: start, end: stop);
  if (index)
    block (return)
      let project = context.context-project;
      let application-context = project & context.context-application-context;
      let thread = application-context & application-context.context-thread;
      let restart = thread & find-indexed-restart(project, thread, index);
      if (restart)
        values(restart, next-index)
      else
        parse-error("No restart %d", index)
      end
    end
  else
    parse-error("Missing restart argument")
  end
end method parse-next-argument;

define method find-indexed-restart
    (project :: <project-object>, thread :: <thread-object>,
     index :: <integer>)
 => (restart :: false-or(<restart-object>))
  let application = project.project-application;
  if (application)
    index := index - 1;        // restarts are one indexed
    let restarts = application-thread-restarts(project, thread);
    if (index >= 0 & index < restarts.size) restarts[index] end
  end
end method find-indexed-restart;


/// Stack frame argument parsing

define sideways method parameter-type-name
    (type == <stack-frame-object>) => (name :: <string>)
  "frame"
end method parameter-type-name;

define sideways method parse-next-argument
    (context :: <environment-context>, type == <stack-frame-object>,
     text :: <string>,
     #key start :: <integer> = 0, end: stop = #f)
 => (value :: <stack-frame-object>, next-index :: <integer>)
  let (index, next-index) = string-to-integer(text, start: start, end: stop);
  if (index)
    block (return)
      let project = context.context-project;
      let thread
        = if (project)
            let application-context = context.context-application-context;
            application-context.context-thread
          end;
      let stack-frame
        = thread & find-indexed-stack-frame(project, thread, index - 1);
      if (stack-frame)
        values(stack-frame, next-index)
      else
        parse-error("No stack-frame %d", index)
      end
    end
  else
    parse-error("Missing stack frame argument")
  end
end method parse-next-argument;

define method stack-frame-index
    (project :: <project-object>, thread :: <thread-object>,
     frame :: <stack-frame-object>)
 => (index :: false-or(<integer>))
  let application = project.project-application;
  if (application)
    let stack = thread-complete-stack-trace(project, thread);
    position(stack, frame)
  end
end method stack-frame-index;

define method find-indexed-stack-frame
    (project :: <project-object>, thread :: <thread-object>, index :: <integer>)
 => (frame :: false-or(<stack-frame-object>))
  let application = project.project-application;
  if (application)
    let stack = thread-complete-stack-trace(project, thread);
    if (index >= 0 & index < stack.size) stack[index] end
  end
end method find-indexed-stack-frame;


/// Remote machine argument parsing

define sideways method parameter-type-name
    (type == <machine>) => (name :: <string>)
  "machine"
end method parameter-type-name;

define sideways method parse-next-argument
    (context :: <environment-context>, type == <machine>,
     text :: <string>,
     #key start :: <integer> = 0, end: stop = #f)
 => (value :: <machine>, next-index :: <integer>)
  let (address, next-index)
    = parse-next-word(text, start: start, end: stop);
  if (address)
    let project = context.context-project;
    let machine = find-remote-connection(context, address);
    if (machine)
      values(machine, next-index)
    else
      parse-error("No machine named '%s'", address)
    end
  else
    parse-error("Missing machine argument")
  end
end method parse-next-argument;
