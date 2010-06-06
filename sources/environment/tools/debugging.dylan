Module:    environment-tools
Synopsis:  Environment tools
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Debugger protocols

define open generic find-debugger
    (frame :: <environment-frame>,
     #key startup-option :: <application-startup-option>)
 => (interactor);

define sealed method find-debugger
    (frame :: <environment-frame>,
     #key startup-option :: <application-startup-option> = #"debug")
 => (interactor)
  let zoom
    = select (startup-option)
	#"start", #"debug" => #"zoom-debugging";
	#"interact"	   => #"zoom-interacting";
      end;
  find-debugger-from-environment(frame,
				 project: frame.ensure-frame-project,
				 zoom: zoom)
end method find-debugger;


/// Just-in-time debugging

define method just-in-time-debugging-arguments
    ()
 => (process :: false-or(<string>), id :: false-or(<string>))
  let arguments = as(<deque>, os/application-arguments());
  local method next-argument
            () => (argument :: <string>, option :: false-or(<string>))
          let argument = as-lowercase(pop(arguments));
          let first-char = argument[0];
          let colon-position = position(argument, ':');
          if ((first-char == '/' | first-char == '-') & colon-position)
            values(copy-sequence(argument, start: 1, end: colon-position),
                   copy-sequence(argument, start: colon-position + 1))
          else
            values(argument, #f)
          end
        end method next-argument;
  let process = #f;
  let id = #f;
  while (~empty?(arguments))
    let (argument, option) = next-argument();
    if (option)
      select (argument by \=)
        "p", "process" =>
          process := option;
        "e", "id" =>
          id := option;
        otherwise =>
          #f;
      end
    end;
  end;
  values(process, id)
end method just-in-time-debugging-arguments;

define method frame-open-just-in-time-project
    (frame :: <environment-frame>, process :: <string>, 
     id :: false-or(<string>))
 => ()
  let process = lookup-process-by-id(process);
  if (process)
    frame-open-just-in-time-project(frame, process, id)
  else
    let message
      = format-to-string("Failed to find process '%s' to debug", process);
    environment-error-message(message, owner: frame)
  end
end method frame-open-just-in-time-project;

define method frame-open-just-in-time-project
    (frame :: <environment-frame>, process :: <process>, 
     id :: false-or(<string>))
 => ()
  let filename = process-executable-file(process);
  let project-filename = frame-choose-project-for-filename(frame, filename);
  if (project-filename)
    let project = coerce-project(project-filename);
    if (project)
      ensure-project-browser-showing-project
	(project,
	 application-process: process,
	 application-id:      id)
    end
  end
end method frame-open-just-in-time-project;


/// Choose project for filename

define variable $choose-project-dialog-width :: <integer> = 500;

define frame <choose-project-dialog> (<dialog-frame>)
  slot %filename :: <file-locator>,
    required-init-keyword: filename:;
  pane project-pane (frame)
    make(<combo-box>,
	 items: map(curry(as, <string>), most-recent-projects()),
	 value: guess-recent-project-for-filename(frame.%filename),
	 min-width: 300,
	 activate-callback: exit-dialog);
  pane browse-button (frame)
    make(<button>,
	 label: "Browse...",
	 activate-callback: 
	   method (gadget)
	     let project = gadget-value(frame.project-pane);
	     let default = project & as(<file-locator>, project);
	     let filename
	       = environment-choose-file
	           (title:  "Open",
		    owner:   frame,
		    default: default,
		    filters: #[#"common-locate-project", #"project", #"lid"]);
	     if (filename)
	       frame.%filename := filename;
	       gadget-text(frame.project-pane) := as(<string>, filename)
	     end
	   end);
  pane debug-project-button (frame)
    make(<radio-button>,
	 id: #"project",
	 label: "Use Dylan project database");
  pane debug-native-button (frame)
    make(<radio-button>,
	 id: #"native",
	 label: "Use native debug information only");
  pane debug-project-pane (frame)
    vertically (spacing: 8)
      frame.debug-project-button;
      horizontally (spacing: 4)
        make(<null-pane>, width: 20, fixed-width?: #t);
	frame.project-pane;
	frame.browse-button;
      end;
    end;
  pane debug-options (frame)
    make(<radio-box>,
	 child: vertically (spacing: 8)
	          frame.debug-project-pane;
	          frame.debug-native-button
                end,
	 value-changed-callback:
	   method (gadget)
	     let value = gadget-value(gadget);
	     let enable? = value == #"project";
	     gadget-enabled?(frame.project-pane)  := enable?;
	     gadget-enabled?(frame.browse-button) := enable?;
	   end);
  layout (frame)
    grouping (format-to-string("Debug '%s'", frame.%filename),
	      max-width: $fill)
      frame.debug-options
    end;
  keyword title: = release-product-name();
  keyword center?: = #t;
end frame <choose-project-dialog>;

define method guess-recent-project-for-filename
    (filename :: <file-locator>) => (guess :: false-or(<string>))
  let projects = most-recent-projects();
  let base = filename.locator-base;
  let filename
    = block (return)
	for (project in projects)
	  if (base == project.locator-base)
	    return(project)
	  end
	end;
	~empty?(projects) & projects[0]
      end;
  filename & as(<string>, filename)
end method guess-recent-project-for-filename;

define method frame-choose-project-for-filename
    (frame :: <environment-frame>, filename :: <file-locator>)
 => (filename :: false-or(<file-locator>))
  let dialog
    = make(<choose-project-dialog>,
	   filename: filename,
	   owner: frame,
	   width: max($choose-project-dialog-width, 350));
  if (start-dialog(dialog))
    let (width, height) = frame-size(dialog);
    $choose-project-dialog-width := width;
    let choice = gadget-value(dialog.debug-options);
    select (choice)
      #"project" => 
	let name = gadget-value(dialog.project-pane);
	name & as(<file-locator>, name);
      #"native" => 
	filename;
    end
  end
end method frame-choose-project-for-filename;


/// Attach application

define variable $application-attach-dialog-width :: <integer> = 450;

define frame <application-attach-dialog> (<dialog-frame>)
  slot %machine :: <machine> = environment-host-machine(),
    init-keyword: machine:;
  pane machine-pane (frame)
    make(<option-box>,
	 items: available-machines(),
	 value: frame.%machine,
	 label-key: machine-hostname,
	 enabled?: #t,
	 value-changed-callback: method (gadget)
				   frame.%machine := gadget-value(gadget);
				   update-dialog-processes(frame)
				 end);
  pane process-pane (frame)
    make(<table-control>,
	 items: keyed-sort(available-processes(machine: frame.%machine),
			   key: process-name),
	 headings:   #["Process", "ID", "File"],
	 widths:     #[120, 50, 400],
	 alignments: #[#"left", #"right", #"left"],
	 generators: vector(process-name,
			    process-id,
			    process-executable-file),
	 callbacks: vector(rcurry(sort-processes, #"process"),
			   rcurry(sort-processes, #"id"),
			   rcurry(sort-processes, #"file")),
	 lines: 15,
	 min-width: 400,
	 activate-callback: method (gadget)
			      exit-dialog(sheet-frame(gadget))
			    end);
  pane new-connection-button (frame)
    make(<button>,
	 label: "Open New Connection...",
	 activate-callback: method (button)
			      let machine 
				= open-remote-connection(owner: frame);
			      if (machine)
				update-dialog-machines(frame, machine: machine);
                                frame.%machine := machine;
                                update-dialog-processes(frame)
			      end
			    end);
  layout (frame)
    grouping ("Attach to a running process:", max-width: $fill)
      vertically (spacing: 8)
        horizontally (spacing: 4)
          make(<label>, label: "Machine:");
          frame.machine-pane
        end;
        frame.process-pane;
	frame.new-connection-button
      end
    end;
  keyword title: = release-product-name();
end frame <application-attach-dialog>;

define method sort-processes
    (gadget :: <table-control>, option :: <symbol>) => ()
  let items = gadget-items(gadget);
  gadget-items(gadget)
    := keyed-sort(items,
		  key: select (option)
			 #"process" => process-name;
			 #"id"      => process-id;
			 #"file"    => process-executable-file
		       end)
end method sort-processes;

define method process-name
    (process :: <process>) => (name :: <string>)
  let filename = process.process-executable-file;
  filename.locator-name
end method process-name;

define method update-dialog-machines
    (frame :: <application-attach-dialog>, #key machine) => ()
  let gadget = frame.machine-pane;
  gadget-items(gadget) := available-machines();
  if (machine) gadget-value(gadget) := machine end;
  update-dialog-processes(frame)
end method update-dialog-machines;

define method update-dialog-processes
    (frame :: <application-attach-dialog>) => ()
  let machine = frame.%machine;
  gadget-items(frame.process-pane) := available-processes(machine: machine)
end method update-dialog-processes;

define function available-machines
    () => (machines :: <sequence>)
  let machines = make(<stretchy-vector>);
  do-machine-connections
    (method (machine :: <machine>)
       add!(machines, machine)
     end);
  machines
end function available-machines;

define function available-processes
    (#key machine = environment-host-machine())
 => (processes :: <sequence>)
  let processes = make(<stretchy-vector>);
  do-active-processes
    (method (process :: <process>)
       if (process-debuggable?(process))
	 add!(processes, process)
       end
     end,
     machine: machine);
  keyed-sort!(processes,
	      key: method (process :: <process>)
		     let filename = process.process-executable-file;
		     filename.locator-name
		   end);
  processes
end function available-processes;

define method frame-attach-application
    (frame :: <environment-frame>, 
     #key process :: false-or(<process>),
          id :: false-or(<string>))
 => ()
  let project = frame.frame-current-project;
  let process
    = process
        | begin
	    let machine
	      = if (project)
		  project.project-debug-machine
		end
	          | environment-host-machine();
	    let dialog
	      = make(<application-attach-dialog>,
		     owner:   frame,
		     machine: machine,
		     width:   max($application-attach-dialog-width, 450));
	    if (start-dialog(dialog))
	      let (width, height) = frame-size(dialog);
	      $application-attach-dialog-width := width;
	      gadget-value(dialog.process-pane)
	    end
	  end;
  if (process)
    if (project)
      attach-live-application(project, process, system-data: id)
    else
      frame-open-just-in-time-project(frame, process, id)
    end
  end
end method frame-attach-application;


/// Remote connections

define variable $new-connection-dialog-width :: <integer> = 250;

define frame <new-connection-dialog> (<dialog-frame>)
  slot %machine :: false-or(<machine>) = #f;
  constant slot %default-machine-address :: <string>,
    init-value: "",
    init-keyword: machine-address:;
  pane address-pane (frame)
    make(<text-field>,
         text: frame.%default-machine-address,
	 min-width: 200,
	 activate-callback: method (gadget)
			      %open-remote-connection(frame)
			    end);
  pane password-pane (frame)
    make(<text-field>,
	 min-width: 200,
	 activate-callback: method (gadget)
			      %open-remote-connection(frame)
			    end);
  layout (frame)
    vertically (spacing: 8)
      make(<label>, label: "Connect to a remote machine:");
      make(<table-layout>,
	   columns: 2,
	   spacing: 4,
	   x-alignment: #[#"right", #"left"],
	   children: vector(make(<label>, label: "IP address:"),
			    frame.address-pane));
      make(<table-layout>,
	   columns: 2,
	   spacing: 4,
	   x-alignment: #[#"right", #"left"],
	   children: vector(make(<label>, label: "Password:"),
			    frame.password-pane));
    end;
  keyword exit-callback: = %open-remote-connection;
  keyword title: = "Open New Connection";
end frame <new-connection-dialog>;

define method open-remote-connection
    (#key owner :: <frame>, default-address = "")
 => (machine :: false-or(<machine>))
  let dialog = make(<new-connection-dialog>, 
                    owner: owner,
                    machine-address: default-address,
		    width: max($new-connection-dialog-width, 250));
  when (start-dialog(dialog))
    let (width, height) = frame-size(dialog);
    $new-connection-dialog-width := width;
    dialog.%machine
  end
end method open-remote-connection;

define method %open-remote-connection
    (dialog :: <new-connection-dialog>) => ()
  let address = gadget-value(dialog.address-pane);
  let password = gadget-value(dialog.password-pane);
  block ()
    let machine = make(<machine>, network-address: address, password: password);
    dialog.%machine := machine;
    exit-dialog(dialog)
  exception (<remote-connection-failed-error>)
    let message
      = format-to-string("Failed to open remote machine address %s", address);
    environment-error-message(message, owner: dialog)
  exception (<remote-connection-password-mismatch-error>)
    let message
      = format-to-string
         ("Invalid password supplied for machine address %s", address);
    environment-error-message(message, owner: dialog)
  end
end method %open-remote-connection;

// Close all connections when the environment shuts down
tune-in($environment-channel,
	method (message :: <environment-stopping-message>)
	  do-machine-connections
	    (close-connection-to-machine,
	     include-local?: #f)
	end,
	message-type: <environment-stopping-message>);


/// Application commands

define open generic frame-start-application (frame :: <environment-frame>) => ();
define open generic frame-attach-application (frame :: <environment-frame>, #key process, id) => ();
define open generic frame-restart-application (frame :: <environment-frame>) => ();
define open generic frame-start-or-resume-application (frame :: <environment-frame>) => ();
define open generic frame-debug-application (frame :: <environment-frame>) => ();
define open generic frame-interact (frame :: <environment-frame>) => ();
define open generic frame-browse-threads (frame :: <environment-frame>) => ();
define open generic frame-pause-application (frame :: <environment-frame>, #key thread, startup-option) => ();
define open generic frame-resume-application (frame :: <environment-frame>) => ();
define open generic frame-stop-application (frame :: <environment-frame>) => ();
define open generic frame-create-thread (frame :: <environment-frame>) => ();

define open generic frame-continue-application (frame :: <environment-frame>) => ();

define open generic find-debugger-from-environment
    (portd :: type-union(<port>, <frame>),
     #key project :: <project-object>,
     thread :: false-or(<thread-object>),
     zoom :: <symbol>)
 => ();

define open generic find-profiler-from-environment
    (portd :: type-union(<port>, <frame>), #key project :: <project-object>)
 => ();

define method frame-start-application
    (frame :: <environment-frame>) => ()
  do-frame-start-application(frame, verify-start-function?: #f);
end method frame-start-application;

define method frame-debug-application
    (frame :: <environment-frame>) => ()
  do-frame-debug-application(frame, startup-option: #"debug");
end method frame-debug-application;

define method frame-profiling?-setter
    (profiling? :: <boolean>, frame :: <environment-frame>)
 => (profiling? :: <boolean>)
  let project = frame.ensure-frame-project;
  if (profiling?)
    start-profiling-application(project)
  else
    stop-profiling-application(project)
  end;
  profiling?
end method frame-profiling?-setter;

define method frame-find-profiler
    (frame :: <environment-frame>) => ()
  let project = frame.ensure-frame-project;
  find-profiler-from-environment(frame, project: project)
end method frame-find-profiler;

define method verify-application-start-function
    (frame :: <environment-frame>)
 => (start-function-okay? :: <boolean>)
  let project = frame.ensure-frame-project;
  let name = project.project-start-function-name | "";
  if (~empty?(name) & ~project.project-start-function)
    let message
      = format-to-string("The start function '%s' was not found. Start anyway?",
			 name);
    environment-question(message, owner: frame, style: #"warning")
  else
    #t
  end
end method verify-application-start-function;

define method do-frame-start-application
    (frame :: <environment-frame>,
     #key startup-option :: <application-startup-option> = #"start",
          verify-start-function? = #t)
 => ()
  let project = frame.ensure-frame-project;
  local method do-it ()
	  let filename = project-full-build-filename(project);
	  if (file-exists?(filename))
	    if (~verify-start-function?
		  | verify-application-start-function(frame))
	      with-busy-cursor (frame)
		frame-do-run-application
		  (frame, project, startup-option: startup-option)
	      end
	    end
	  end
	end method do-it;
  if (project-can-be-built?(project) & ~project-compiler-database(project))
    with-project-database (frame, link?: #t)
      do-it()
    end
  else
    with-built-project (frame)
      do-it()
    end
  end
end method do-frame-start-application;

define method do-frame-debug-application
    (frame :: <environment-frame>,
     #key startup-option :: <application-startup-option> = #"debug") => ()
  let project = frame.ensure-frame-project;
  let (tethered?, state) = frame-application-tethered?(frame, project);
  select (tethered? & state)
    #f =>
      do-frame-start-application(frame, startup-option: startup-option);
    #"running" =>
      frame-pause-application(frame, startup-option: startup-option);
    #"stopped" =>
      find-debugger(frame, startup-option: startup-option);
  end select;
end method do-frame-debug-application;

define method frame-interact
    (frame :: <environment-frame>) => ()
  do-frame-debug-application(frame, startup-option: #"interact");
end method frame-interact;

define method frame-browse-threads
    (frame :: <environment-frame>) => ()
  let project = frame.ensure-frame-project;
  let application = project.project-application;
  if (application)
    with-busy-cursor (frame)
      browse-object(project, application, page: #"threads");
    end
  else 
    environment-error-message("No tethered application.", owner: frame);
  end if;
end method frame-browse-threads;

define method frame-pause-application
    (frame :: <environment-frame>, 
     #key thread, startup-option)
 => ()
  let project = frame.ensure-frame-project;
  with-busy-cursor (frame)
    stop-application(project, client-data: pair(thread, startup-option))
  end
end method frame-pause-application;

define method frame-resume-application
    (frame :: <environment-frame>) => ()
  with-busy-cursor (frame)
    frame-continue-application(frame)
  end
end method frame-resume-application;

define method frame-continue-application
    (frame :: <environment-frame>) => ()
  let project = frame.ensure-frame-project;

  continue-application(project)
end method frame-continue-application;

define method frame-stop-application
    (frame :: <environment-frame>) => ()
  let project = frame.ensure-frame-project;
  let check? = environment-application-confirm-stop?();
  if (~check?
	| environment-question
	    (format-to-string
	       ("All application state will be lost by stopping it.\n"
		"Are you sure?",
		frame-default-object-name(frame, project)),
	     owner: frame,
	     style: #"warning",
	     exit-style: #"ok-cancel"))
    with-busy-cursor (frame)
      close-application(project, wait-for-termination?: #t)
    end
  end
end method frame-stop-application;

define method frame-restart-application
    (frame :: <environment-frame>) => ()
  let project = frame.ensure-frame-project;
  with-busy-cursor (frame)
    let startup-option = project.application-startup-option;
    close-application(project, wait-for-termination?: #t);
    frame-do-run-application
      (frame, project, startup-option: startup-option)
  end
end method frame-restart-application;

define method frame-start-or-resume-application 
    (frame :: <environment-frame>) => ()
  let project = frame.ensure-frame-project;
  let (tethered?, state) = frame-application-tethered?(frame, project);
  select (tethered? & state)
    #f => frame-start-application(frame);
    #"stopped" => frame-resume-application(frame);
    #"running" => #f;
  end select;
end method frame-start-or-resume-application;

define variable $create-thread-dialog-width :: <integer> = 250;

define frame <create-thread-dialog> (<dialog-frame>)
  pane thread-title-pane (dialog)
    make(<text-field>,
	 documentation: "Enter the name for the new thread.");
  layout (dialog)
    horizontally (spacing: 4)
      make(<label>, label: "Title:");
      dialog.thread-title-pane;
    end;
  input-focus (dialog)
    dialog.thread-title-pane;
  keyword title: = "New Thread...";
end frame <create-thread-dialog>;

define method frame-create-thread
    (frame :: <environment-frame>) => ()
  let project = frame.ensure-frame-project;
  let dialog = make(<create-thread-dialog>,
		    owner: frame,
		    width: max($create-thread-dialog-width, 250));
  if (start-dialog(dialog))
    let (width, height) = frame-size(dialog);
    $browse-object-dialog-width := width;
    let title = dialog.thread-title-pane.gadget-value;
    create-application-thread(project, title)
  end
end method frame-create-thread;

define command-table *basic-run-command-table* (*global-command-table*)
  menu-item "Start"    = frame-start-application,
    accelerator:   make-keyboard-gesture(#"f8"),
    documentation: "Starts the executable.";
  menu-item "Attach..."    = frame-attach-application,
    documentation: $attach-doc;
  separator;
  menu-item "Debug"    = frame-debug-application,
    accelerator:   make-keyboard-gesture(#"f8", #"shift"),
    documentation: $debug-doc;
  menu-item "Interact" = frame-interact,
    accelerator:   make-keyboard-gesture(#"f8", #"control"),
    documentation: $interact-doc;
  separator;
  menu-item "Pause"    = frame-pause-application,
    documentation: "Pauses the executable.";
  menu-item "Resume"   = frame-resume-application,
    documentation: "Resumes the executable.";
  separator;
  menu-item "Stop"     = frame-stop-application,
    documentation: "Exits the executable.";
  menu-item "Restart"  = frame-restart-application,
    documentation: "Exits the executable then starts it.";
  separator;
  menu-item "New Thread..." = frame-create-thread,
    documentation: "Creates a new thread in the application.";
end command-table *basic-run-command-table*;

define command-table *run-command-table* (*global-command-table*)
  include *basic-run-command-table*;
  separator;
  include *all-breakpoints-command-table*;
end command-table *run-command-table*;

define method enable-application-command-table
    (frame :: <environment-frame>,
     state :: false-or(<application-state>))
  => ()
  let project = frame.ensure-frame-project;
  let application-can-be-debugged? = project-can-be-debugged?(project);

  local method enabled?-setter
	    (enabled? :: <boolean>, command :: <function>)
	  command-enabled?(command, frame)
	    := application-can-be-debugged? & enabled?
	end method enabled?-setter;

  let application-running? = state == #"running";
  let application-stopped? = state == #"stopped";
  let application-started? = application-running? | application-stopped?;
  let application-not-started? = ~application-started?;
  let application-not-running? = ~application-running?;
  let attaching-available? = application-not-started?;

  enabled?(frame-start-application)           := application-not-started?;
  enabled?(frame-attach-application)          := attaching-available?;

  enabled?(frame-debug-application)           := #t;
  enabled?(frame-interact)                    := #t;

  enabled?(frame-browse-threads)              := application-started?;
  enabled?(frame-stop-application)            := application-started?;
  enabled?(frame-restart-application)         := application-started?;
  enabled?(frame-create-thread)               := application-started?;
  
  enabled?(frame-pause-application)           := application-running?;
  enabled?(frame-resume-application)          := application-stopped?;
  enabled?(frame-start-or-resume-application) := application-not-running?;
end method enable-application-command-table;

