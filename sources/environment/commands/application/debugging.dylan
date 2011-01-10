Module:    environment-application-commands
Synopsis:  The application commands provided by the environment
Author:	   Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Debugging properties

// Messages property

define class <messages-property> (<project-property>)
end class <messages-property>;

define command-property messages => <messages-property>
  (summary:       "Level of application messages to display",
   documentation: "The current level of application messages to display.",
   type:          <symbol>)
end command-property messages;

define method show-property
    (context :: <environment-context>, property :: <messages-property>)
 => ()
  let project = context.context-project;
  let application-context = context.context-application-context;
  let show-messages? = application-context.context-show-messages?;
  message(context, "Messages: %s",
	  case
	    show-messages? => "verbose";
	    otherwise      => "quiet";
	  end)
end method show-property;

define method set-property
    (context :: <environment-context>, property :: <messages-property>, 
     messages :: <symbol>,
     #key save?)
 => ()
  let application-context = context.context-application-context;
  application-context.context-show-messages? := messages == #"verbose"
end method set-property;


// Thread property

define class <current-thread-property> (<application-property>)
end class <current-thread-property>;

define command-property thread => <current-thread-property>
  (summary:       "Current thread",
   documentation: "The current thread.",
   type:          <thread-object>)
end command-property thread;

define method show-property
    (context :: <environment-context>, property :: <current-thread-property>)
 => ()
  let project = context.context-project;
  let application-context = context.context-application-context;
  let thread = application-context.context-thread;
  if (thread)
    message(context, "%s",
	    environment-object-display-name(project, thread, #f))
  else
    message(context, "No active thread")
  end
end method show-property;

define method set-property
    (context :: <environment-context>, property :: <current-thread-property>,
     thread :: <thread-object>,
     #key save?)
 => ()
  let application-context = context.context-application-context;
  application-context.context-thread := thread
end method set-property;


// Threads property

define class <threads-property> (<application-property>)
end class <threads-property>;

define command-property threads => <threads-property>
  (summary:       "Application threads",
   documentation: "The set of active threads in the application.")
end command-property threads;

define method show-property
    (context :: <environment-context>, property :: <threads-property>)
 => ()
  let project = context.context-project;
  let application = project & project.project-application;
  case
    ~project =>
      message(context, "No project");
    ~application | ~application.application-tethered? =>
      message(context, "No application");
    otherwise =>
      let application-context = context.context-application-context;
      let current-thread = application-context.context-thread;
      let threads = application.application-threads;
      for (thread :: <thread-object> in threads)
	message(context, "  Thread %d: %s%s%s",
		thread-index(project, thread),
		environment-object-primitive-name(project, thread),
		if (thread.thread-suspended?) " [suspended]" else "" end,
		if (thread == current-thread) " [*]" else "" end)
      end;
  end
end method show-property;


// Restarts property

define class <restarts-property> (<application-property>)
end class <restarts-property>;

define command-property restarts => <restarts-property>
  (summary:       "Thread restarts",
   documentation: "The set of active restarts for the current thread.")
end command-property restarts;

define method show-property
    (context :: <environment-context>, property :: <restarts-property>)
 => ()
  let project = context.context-project;
  let thread = context.context-application-context.context-thread;
  case
    ~project =>
      message(context, "No project");
    otherwise =>
      let restarts         = application-thread-restarts(project, thread);
      let continue-restart = application-continue-restart(project, thread);
      let abort-restart    = application-abort-restart(project, thread);
      for (restart :: <restart-object> in restarts,
	   index :: <integer> from 1)
	message(context, "  %d%s: %s",
		index,
		select (restart)
		  abort-restart    => " (abort)";
		  continue-restart => " (continue)";
		  otherwise        => "";
		end,
		application-restart-message(project, restart))
      end;
  end
end method show-property;


// Machine property

define class <default-machine-property> (<project-property>)
end class <default-machine-property>;

define command-property machine => <default-machine-property>
  (summary:       "Default remote machine",
   documentation: "Default remote machine for this project.",
   type:          <machine>)
end command-property machine;

define method show-property
    (context :: <environment-context>, property :: <default-machine-property>)
 => ()
  let project = context.context-project;
  message(context, "%s",
	  project.project-debug-machine-address)
end method show-property;

define method set-property
    (context :: <environment-context>, property :: <default-machine-property>,
     machine :: <machine>,
     #key save?)
 => ()
  let project = context.context-project;
  project.project-debug-machine := machine
end method set-property;

// Machines property

define class <machines-property> (<environment-property>)
end class <machines-property>;

define command-property machines => <machines-property>
  (summary:       "Remote machine connections",
   documentation: "The current set of open remote machine connections.",
   type:          <symbol>)
end command-property machines;

define method show-property
    (context :: <environment-context>, property :: <machines-property>)
 => ()
  let machines = available-machines(include-local?: #f);
  if (~empty?(machines))
    for (machine :: <machine> in machines)
      message(context, "  %s",
	      machine.machine-network-address)
    end
  else
    message(context, "Local machine only")
  end
end method show-property;

define method set-property
    (context :: <environment-context>, property :: <machines-property>,
     messages :: <symbol>,
     #key save?)
 => ()
  let application-context = context.context-application-context;
  application-context.context-show-messages? := messages == #"verbose"
end method set-property;

define function available-machines
    (#key include-local? :: <boolean> = #t)
 => (machines :: <sequence>)
  let machines = make(<stretchy-vector>);
  do-machine-connections
    (method (machine :: <machine>)
       if (include-local? | machine ~== environment-host-machine())
	 add!(machines, machine)
       end
     end);
  machines
end function available-machines;


/// Connect command

define class <connect-to-remote-machine-command> (<environment-command>)
  constant slot %machine :: <string>,
    required-init-keyword: machine:;
  constant slot %password :: false-or(<string>) = #f,
    init-keyword: password:;
end class <connect-to-remote-machine-command>;

define command-line connect => <connect-to-remote-machine-command>
    (summary:       "connects to a remote machine",
     documentation: "Connects to a machine to enable remote debugging.")
  required machine :: <word> = "remote machine to start application on";
  keyword password :: <word> = "password for remote machine";
end command-line connect;

define method do-execute-command
    (context :: <environment-context>, 
     command :: <connect-to-remote-machine-command>)
 => ()
  let machine  = command.%machine;
  let password = command.%password;
  if (password)
    open-remote-connection(context, machine, password);
    message(context, "Successfully connected to %s", machine)
  else
    message(context, "You must supply a password")
  end
end method do-execute-command;

define method find-remote-connection
    (context :: <environment-context>, address :: <string>)
 => (machine :: false-or(<machine>))
  block (return)
    let machines = available-machines();
    for (machine :: <machine> in machines)
      if (address = machine.machine-network-address)
	return(machine)
      end
    end
  end
end method find-remote-connection;

define method open-remote-connection
    (context :: <environment-context>, address :: <string>, 
     password :: false-or(<string>))
 => (machine :: <machine>)
  block ()
    find-remote-connection(context, address)
      | make(<machine>, network-address: address, password: password)
  exception (<remote-connection-failed-error>)
    command-error("Failed to open remote machine address %s", address);
  exception (<remote-connection-password-mismatch-error>)
    command-error("Invalid password supplied for machine address %s", address);
  end
end method open-remote-connection;


/// Start application command

define class <start-application-command> (<project-command>)
  constant slot %filename :: false-or(<file-locator>) = #f,
    init-keyword: filename:;
  constant slot %arguments :: false-or(<string>) = #f,
    init-keyword: arguments:;
  constant slot %debug? :: <boolean> = #f,
    init-keyword: debug?:;
  constant slot %profile? :: <boolean> = #f,
    init-keyword: profile?:;
  constant slot %share-console? :: <boolean> = #f,
    init-keyword: share-console?:;
  constant slot %machine :: false-or(<string>) = #f,
    init-keyword: machine:;
  constant slot %password :: false-or(<string>) = #f,
    init-keyword: password:;
  constant slot %messages :: false-or(<symbol>) = #f,
    init-keyword: messages:;
end class <start-application-command>;

define command-line start => <start-application-command>
    (summary:       "starts the project's application",
     documentation: "Starts the project's application.")
  keyword filename :: <file-locator> = "filename for the application";
  keyword arguments :: <string> = "arguments for the application";
  keyword machine :: <word> = "remote machine to start application on";
  keyword password :: <word> = "password for remote machine";
  keyword messages :: <symbol> = "level of application messages to display";
  flag debug         = "debug the application once it has initialized [off by default]";
  flag profile       = "profile the application [off by default]";
  flag share-console = "share the console with the application [off by default]";
end command-line start;

define method do-execute-command
    (context :: <environment-context>, command :: <start-application-command>)
 => ()
  let project        = context.context-project;
  let machine        = command.%machine;
  let password       = command.%password;
  let arguments      = command.%arguments;
  let messages       = command.%messages;
  let debug?         = command.%debug?;
  let profile?       = command.%profile?;
  let share-console? = command.%share-console?;
  let filename       = command.%filename | project-full-build-filename(project);
  case
    project.project-can-be-built? & ~project.project-compiler-database =>
      command-error("Project %s has not been built, so cannot be run",
		    project.project-name);
    ~file-exists?(filename) =>
      command-error("File '%s' does not exist", filename);
  end;
  let machine = machine & open-remote-connection(context, machine, password);
  let application-context = context.context-application-context;
  if (messages)
    application-context.context-show-messages? := messages == #"verbose";
  end;
  application-context.context-debug-request? := debug?;
  application-context.context-start-command  := command;
  if (profile?)
    let command = make(<profile-application-command>, server: context);
    execute-command(command)
  end;
  message(context, "Starting: %s %s",
	  project.project-name,
	  arguments | "");
  synchronize-application-call
    (context,
     run-application, project,
     client:                    context,
     machine:                   machine | unsupplied(),
     startup-option:            case
				  debug?    => #"debug";
				  otherwise => #"start";
				end,
     share-console?:            share-console?,
     filename:                  filename,
     arguments:                 arguments,
     pause-before-termination?: #f)
end method do-execute-command;


/// Debug application command

define class <debug-application-command> (<start-application-command>)
  keyword debug?: = #t;
end class <debug-application-command>;

define command-line debug => <debug-application-command>
    (summary:       "debugs the project's application",
     documentation: "Starts the project's application under the debugger.")
  keyword filename :: <file-locator> = "filename for the application";
  keyword arguments :: <string> = "arguments for the application";
  keyword machine :: <word> = "remote machine to start application on";
  keyword password :: <word> = "password for remote machine";
  keyword messages :: <symbol> = "level of application messages to display";
  flag share-console = "share the console with the application [off by default]";
end command-line debug;


/// Play command

define class <play-command> (<environment-command>)
end class <play-command>;

define command-line play => <play-command>
    (summary:       "opens and debugs the playground project",
     documentation: "Opens and debugs the playground project.")
end command-line play;

define method do-execute-command
    (context :: <environment-context>, command :: <play-command>)
 => ()
  local method run
	    (class :: subclass(<command>), #rest arguments) => ()
	  let command = apply(make, class, server: context, arguments);
	  execute-command(command)
	end method run;
  let name = playground-project-name();
  run(<open-project-command>, file: as(<file-locator>, name));
  run(<debug-application-command>)
end method do-execute-command;


/// Abort thread command

define class <abort-thread-command> (<project-command>)
end class <abort-thread-command>;

define command-line abort => <abort-thread-command>
    (summary:       "aborts the thread",
     documentation: "Aborts an thread that is in the debugger.")
end command-line abort;

define method do-execute-command
    (context :: <environment-context>, command :: <abort-thread-command>)
 => ()
  let project = context.context-project;
  let thread = context.context-application-context.context-thread;
  let abort-restart = application-abort-restart(project, thread);
  if (abort-restart)
    synchronize-application-call
      (context, invoke-application-restart,
       project, thread, abort-restart)
  else
    message(context, "No abort restart available in this thread.")
  end
end method do-execute-command;


/// Create thread command

define class <create-thread-command> (<project-command>)
  constant slot %title :: <string>,
    required-init-keyword: title:;
end class <create-thread-command>;

define command-line create-thread => <create-thread-command>
    (summary:       "creates an application thread",
     documentation: "Creates a thread in the application.")
  argument title :: <word> = "the title for the new thread";
end command-line create-thread;

define method do-execute-command
    (context :: <environment-context>, command :: <create-thread-command>)
 => ()
  let project = context.context-project;
  let title   = command.%title;
  let thread = create-application-thread(project, title);
  message(context, "%s created",
	  environment-object-display-name(project, thread, #f))
end method do-execute-command;


/// Suspend thread command

define class <suspend-thread-command> (<project-command>)
  constant slot %thread :: false-or(<thread-object>) = #f,
    init-keyword: thread:;
end class <suspend-thread-command>;

define command-line suspend => <suspend-thread-command>
    (summary:       "suspends an application thread",
     documentation: "Suspends a thread in the application.")
  optional thread :: <thread-object> = "the thread to suspend";
end command-line suspend;

define method do-execute-command
    (context :: <environment-context>, command :: <suspend-thread-command>)
 => ()
  let project = context.context-project;
  let thread  = command.%thread | context.context-application-context.context-thread;
  suspend-application-thread(project, thread);
  message(context, "Thread %s suspended",
	  environment-object-display-name(project, thread, #f))
end method do-execute-command;


/// Resume thread command

define class <resume-thread-command> (<project-command>)
  constant slot %thread :: false-or(<thread-object>) = #f,
    init-keyword: thread:;
end class <resume-thread-command>;

define command-line resume => <resume-thread-command>
    (summary:       "resumes an application thread",
     documentation: "Resumes a thread in the application.")
  optional thread :: <thread-object> = "the thread to resume";
end command-line resume;

define method do-execute-command
    (context :: <environment-context>, command :: <resume-thread-command>)
 => ()
  let project = context.context-project;
  let thread  = command.%thread | context.context-application-context.context-thread;
  resume-application-thread(project, thread);
  message(context, "Thread %s resumed",
	  environment-object-display-name(project, thread, #f))
end method do-execute-command;


/// Continue application command

define class <continue-application-command> (<project-command>)
  constant slot %restart :: false-or(<restart-object>) = #f,
    init-keyword: restart:;
end class <continue-application-command>;

define command-line continue => <continue-application-command>
    (summary:       "continues the application",
     documentation: "Continues an application that is in the debugger.")
  optional restart :: <restart-object> = "restart to take";
end command-line continue;

define method do-execute-command
    (context :: <environment-context>, 
     command :: <continue-application-command>)
 => ()
  let project = context.context-project;
  let application-context = context.context-application-context;
  let restart = command.%restart;
  application-context.context-stack-frame := #f;
  synchronize-application-call
    (context,
     method ()
       if (restart)
	 let thread = application-context.context-thread;
	 invoke-application-restart(project, thread, restart)
       else
	 continue-application(project)
       end
     end)
end method do-execute-command;


/// Stop application command

define class <stop-application-command> (<application-command>)
end class <stop-application-command>;

define command-line stop => <stop-application-command>
    (summary:       "stops the project's application",
     documentation: "Stops the project's application.")
end command-line stop;

define method do-execute-command
    (context :: <environment-context>, command :: <stop-application-command>)
 => ()
  let project = context.context-project;
  close-application(project, wait-for-termination?: #t)
end method do-execute-command;


/// Restart application command

define class <restart-application-command> (<application-command>)
end class <restart-application-command>;

define command-line restart => <restart-application-command>
    (summary:       "restarts the project's application",
     documentation: "Restarts the project's application.")
end command-line restart;

define method do-execute-command
    (context :: <environment-context>, 
     command :: <restart-application-command>)
 => ()
  let start-command
    = context.context-application-context.context-start-command
        | make(<start-application-command>, server: context);
  execute-command(make(<stop-application-command>, server: context));
  execute-command(start-command)
end method do-execute-command;


/// Evaluate command

define class <evaluate-code-command> (<application-command>)
  constant slot command-code :: <string>,
    required-init-keyword: code:;
end class <evaluate-code-command>;

define command-line evaluate => <evaluate-code-command>
    (summary:       "evaluates the given code",
     documentation: "Evaluates the given code.")
  argument code :: <string> = "the code to evaluate";
end command-line evaluate;

define method command-complete?
    (context :: <environment-context>, command :: <evaluate-code-command>)
 => (complete? :: <boolean>)
  let project = context.context-project;
  let project-context = context.context-project-context;
  let application-context = context.context-application-context;
  let thread = application-context.context-thread;
  let module = project-context.context-module;
  let stack-frame = application-context.context-stack-frame;
  let (complete?, warnings) 
    = project-valid-code?(project, command.command-code, thread, 
			  module: module,
			  stack-frame: stack-frame);
  complete?
end method command-complete?;

define method do-execute-command
    (context :: <environment-context>, command :: <evaluate-code-command>)
 => ()
  let project = context.context-project;
  let project-context = context.context-project-context;
  let application-context = context.context-application-context;
  let thread = application-context.context-thread;
  let module = project-context.context-module;
  let stack-frame = application-context.context-stack-frame;
  //--- This is a bit of a hack, since the evaluation may cause
  //--- an error and fall into the debugger, in which case we do
  //--- want to show that. Really we need a way to distinguish conditions
  //--- from other stop reasons.
  application-context.context-debug-request? := #t;
  synchronize-application-call
    (context,
     method ()
       let transaction-id
	 = project-execute-code(project, command.command-code, thread,
				module: module,
				stack-frame: stack-frame);
       ignore(transaction-id)
     end)
end method do-execute-command;


/// Word parsing
///---*** Should be in environment-commands

define constant <word> = singleton(#"word");

define method parameter-type-name
    (type == <word>) => (name == #f)
  #f
end method parameter-type-name;

define method parse-next-argument
    (context :: <server-context>, type == <word>,
     text :: <string>,
     #key start :: <integer> = 0, end: stop = #f)
 => (value :: <sequence>, next-index :: <integer>)
  let (word, next-index)
    = parse-next-word(text, start: start, end: stop);
  if (word)
    values(word, next-index)
  else
    parse-error("Missing keyword argument")
  end
end method parse-next-argument;


/// Application callbacks

define sideways method initialize-application-client
    (context :: <environment-context>, application :: <application>) => ()
  register-application-callbacks
    (application,
     // initialized-callback:         note-application-initialized,
     // process-created-callback:     note-application-process-started,
     debugging-callback:           start-debugging,
     thread-message-callback:      note-application-thread-message,
     // thread-finished-callback:     note-application-thread-finished,
     // just-interacted-callback:     note-application-just-interacted,
     interactive-results-callback: note-application-interactive-results,
     process-finished-callback:    note-application-process-finished,
     finished-execution-callback:  note-application-finished-execution
     );
  tune-in($project-channel,
	  method (message :: <thread-interactive-warnings-message>)
	    let application-context = context.context-application-context;
	    let warnings = message.message-warnings;
	    application-context.context-interactive-warnings := warnings
	  end,
	  message-type: <thread-interactive-warnings-message>);
end method initialize-application-client;


define method start-debugging
    (context :: <environment-context>, thread :: false-or(<thread-object>),
     startup-option :: false-or(<application-startup-option>))
 => ()
  synchronize-application-release
    (context,
     method ()
     end,
     thread: thread)
end method start-debugging;

define method note-application-thread-message
    (context :: <environment-context>, thread :: <thread-object>,
     thread-message :: <string>)
 => ()
  let application-context = context.context-application-context;
  // Need to distinguish debug and application messages
  if (application-context.context-show-messages?)
    let project = context.context-project;
    let index = thread-index(project, thread);
    let prefix = ""; // format-to-string("Thread %d: ", index);
    message(context, "%s%s", prefix, thread-message)
  end
end method note-application-thread-message;

define method note-application-interactive-results
    (context :: <environment-context>, thread :: <thread-object>, 
     transaction-id)
 => ()
  let application-context = context.context-application-context;
  application-context.context-last-transaction-id := transaction-id
end method note-application-interactive-results;

define method note-application-finished-execution
    (context :: <environment-context>)
 => ()
  let application-context = context.context-application-context;
  let project = context.context-project;
  let thread = application-context.context-thread;
  if (project.application-pause-before-termination?
	& true?(project.project-last-profile))
    message(context, "Application paused before closing to view profiling results");
    synchronize-application-release
      (context,
       method ()
       end,
       thread: thread)
  end
end method note-application-finished-execution;

define method note-application-process-finished
    (context :: <environment-context>, exit-code :: false-or(<integer>)) => ()
  synchronize-application-release
    (context,
     method ()
       let application-context = context.context-application-context;
       application-context.context-thread := #f;
       if (exit-code)
	 message(context, "Application exited with exit code %d",
		 exit-code)
       else
	 message(context, "Application exited suddenly with no exit code")
       end
     end)
end method note-application-process-finished;

define method refresh-application-context
    (context :: <environment-context>) => ()
  let project = context.context-project;
  let project-context = context.context-project-context;
  let application-context = context.context-application-context;
  let module = project-context.context-module;
  let application = project & project.project-application;
  if (application & application.application-tethered?)
    let threads = application.application-threads;
    let old-thread = application-context.context-thread;
    unless (old-thread & member?(old-thread, threads))
      let new-thread = ~empty?(threads) & threads[0];
      application-context.context-thread := new-thread
    end
  else
    application-context.context-thread := #f;
  end;
  let warnings = application-context.context-interactive-warnings;
  unless (empty?(warnings))
    let stream = context.context-server.server-output-stream;
    for (warning :: <compiler-warning-object> in warnings)
      print-environment-object-name
	(stream, project, warning, 
	 namespace: module,
	 full-message?: #t);
      new-line(stream)
    end;
    application-context.context-interactive-warnings := #[]
  end;
  if (application-context.context-debug-request?)
    application-context.context-debug-request? := #f
  else
    let project = context.context-project;
    let application = project.project-application;
    let thread = application-context.context-thread;
    let stop-reason-message = application.application-stop-reason-message;
    if (stop-reason-message)
      message(context, "%s", stop-reason-message);
      message(context, "Restarts for %s:",
	      environment-object-display-name(project, thread, #f));
      show-named-property(context, #"restarts")
    end
  end;
  let transaction-id = application-context.context-last-transaction-id;
  if (transaction-id)
    let results = fetch-interactor-return-values(project, transaction-id);
    dispose-interactor-return-values(project, transaction-id);
    for (result :: <pair> in results)
      let (name :: <string>, value :: <environment-object>)
	= values(result.head, result.tail);
      message(context, "  %s = %s",
	      name,
	      print-environment-object-to-string
		(project, value, namespace: module))
    end;
    application-context.context-last-transaction-id := #f
  end
end method refresh-application-context;

define function application-abort-restart
    (project :: <project-object>, thread :: <thread-object>)
 => (abort-restart :: false-or(<restart-object>))
  block (return)
    for (restart in application-thread-restarts(project, thread))
      if (application-restart-abort?(project, restart))
	return(restart)
      end
    end
  end
end function application-abort-restart;

define function application-continue-restart
    (project :: <project-object>, thread :: <thread-object>)
 => (continue-restart :: false-or(<restart-object>))
  block (return)
    for (restart in application-thread-restarts(project, thread))
      if (~application-restart-abort?(project, restart))
	return(restart)
      end
    end
  end
end function application-continue-restart;


/// Thread synchronization

define method synchronize-application-call
    (context :: <environment-context>, function :: <function>, #rest args)
 => (#rest objects)
  let notification = context.context-notification;
  with-lock (associated-lock(notification))
    apply(function, args);
    wait-for(notification);
    refresh-application-context(context)
  end
end method synchronize-application-call;

define method synchronize-application-release
    (context :: <environment-context>, function :: <function>,
     #key thread :: false-or(<thread-object>) = #f)
 => (#rest objects)
  let notification = context.context-notification;
  with-lock (associated-lock(notification))
    function();
    if (thread)
      let application-context = context.context-application-context;
      application-context.context-thread := thread
    end;
    release(notification)
  end
end method synchronize-application-release;


///---*** To do

/*
  options

  print
  describe
  explode

  walk
  evaluate
*/


/// Debugging commands

define command-group debugging into environment
    (summary: "debugging commands",
     documentation: "Commands to drive debugging of an application.")
  property messages;
  property thread;
  property threads;
  property restarts;
  command  play;
  command  start;
  command  debug;
  command  create-thread;
  command  suspend;
  command  resume;
  command  abort;
  command  continue;
  command  evaluate;
  command  stop;
  command  restart;
  alias interact = debug;
  alias eval     = evaluate;
  alias a        = abort;
  alias c        = continue;
end command-group debugging;

define command-group remote-debugging into environment
    (summary: "remote debugging commands",
     documentation: "Commands to drive remote debugging.")
  property machine;
  property machines;
  command connect;
end command-group remote-debugging;
