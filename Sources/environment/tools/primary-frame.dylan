Module:    environment-tools
Synopsis:  Environment tools
Author:    Chris Page
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Primary frame

define frame <environment-primary-frame>
    (<frame-search-mixin>,
     <frame-help-mixin>,
     <frame-reuse-mixin>, // NB needed for choose-matching-frame used in environment-primary-frame
     <frame-window-settings-mixin>,
     <environment-tool>)
  tool-bar      (frame) make-environment-tool-bar(frame, max-width: $fill);
  command-table (frame) *primary-frame-command-table*;
  keyword max-width: = $fill;
  keyword x: = 0;
  keyword y: = 0;
  keyword width: = 2000;	//---*** andrewa: quick hack!
  keyword fixed-height?: = #t;
  keyword icon: = $main-window-small-icon;
  keyword frame-class-name:, init-value: #"primary-frame";
end frame <environment-primary-frame>;

define window-settings 
  primary-window :: <environment-primary-frame> = "Primary Window";

define method initialize
    (frame :: <environment-primary-frame>, #key) => ()
  next-method();
  command-enabled?(frame-attach-application, frame) := just-in-time-debugging?();
  if (release-internal?())
    add-command-table-menu-item
      (*primary-frame-command-table*, "Internal", <command-table>,
       *primary-frame-internal-command-table*,
       after: "Window")
  end
end method initialize;

// The startup action is performed via creation of the primary frame.
define method handle-event
    (frame :: <environment-primary-frame>, event :: <frame-mapped-event>)
 => ()
  next-method();
  handle-environment-startup(frame)
end method handle-event;

/// Primary window commands that affect all windows

//--- cpage: 1997.10.10 Provide a default method so that we can call this on
//           all frames. Eventually, we should have a formal model for
//           documents, and we can iterate over them instead of frames.
define sideways method frame-save-all (frame :: <frame>) => ()
  ignore(frame);
end method frame-save-all;

// Save All
define method frame-save-all (frame :: <environment-primary-frame>) => ()
  //--- cpage: 1997.09.29 This isn't quite right. We probably need a method
  //           on <environment-frame> that iterates over instances of
  //           (the fictional class) <document>, registered centrally when
  //           frames create or open documents.
  do-frames(method (_frame :: <frame>)
              unless (frame = _frame)
                frame-save-all(_frame)
              end
            end method);
end method frame-save-all;

// Exit the environment
define method handle-event
    (frame :: <environment-primary-frame>, event :: <frame-exit-event>)
 => ()
  when (frame-exit-application(frame))
    next-method();
  end;
end method handle-event;

define method handle-event
    (frame :: <environment-primary-frame>, event :: <frame-destroyed-event>)
 => ()
  next-method();
  broadcast($environment-channel, make(<environment-stopping-message>));
end method handle-event;


/// Toolbar buttons

// New file buttons
define variable $new-text-file-bitmap    :: <label-type> = "New Text File";
define variable $new-project-file-bitmap :: <label-type> = "New Project";

define constant $new-text-file-title    = "New Text File";
define constant $new-project-file-title = "New Project";

define method make-new-file-tool-bar-buttons
    (frame :: <environment-frame>)
 => (buttons :: <sequence>)
  vector(make(<button>,
              label: $new-text-file-bitmap,
              documentation: $new-text-file-title,
              command: editor-new-file,
              activate-callback: method (sheet)
                                   ignore(sheet);
                                   editor-new-file()
                                 end),
         make(<button>,
              label: $new-project-file-bitmap,
              documentation: $new-project-file-title,
              command: create-new-project,
              activate-callback: method (sheet)
				   let frame = sheet-frame(sheet);
                                   create-new-project(frame: frame)
                                 end))
end method make-new-file-tool-bar-buttons;

// File I/O buttons
define constant $open-file-title  = "Open";
define variable $playground-bitmap :: <label-type> = "Playground Icon";

define method make-file-io-tool-bar-buttons
    (frame :: <environment-frame>)
 => (buttons :: <sequence>)
  vector(make(<button>,
              label: $open-bitmap,
              documentation: $open-file-title,
              command: frame-open-file,
              activate-callback: method (sheet)
                                   frame-open-file(sheet-frame(sheet))
                                 end))
end method make-file-io-tool-bar-buttons;

/*---*** Not currently used
define method make-find-tool-bar-buttons
    (frame :: <environment-frame>)
 => (buttons :: <sequence>)
  vector(make(<button>,
              label: $find-bitmap,
              documentation: $find-title,
              command: frame-edit-search-options,
              activate-callback: method (sheet)
                                   frame-edit-search-options(sheet-frame(sheet))
                                 end))
end method make-find-tool-bar-buttons;
*/

// Help buttons
define variable $tutorial-bitmap :: <label-type> = "Tutorial";
define variable $examples-bitmap :: <label-type> = "Examples";
define constant $examples-doc        = "Open Example Project";
define constant $examples-title      = "Open Example Project...";
define constant $help-topics-doc     = "Help Contents and Index";
define constant $playground-title    = "Open Playground";

define method make-help-tool-bar-buttons
    (frame :: <environment-frame>)
 => (buttons :: <sequence>)
  vector(make(<button>,
              label: $examples-bitmap,
              documentation: $examples-doc,
              command: frame-open-example,
              activate-callback: method (sheet)
                                   frame-open-example(sheet-frame(sheet))
                                 end),
         /*---*** Remove the tutorial for the beta
         make(<button>,
              label: $tutorial-bitmap,
              documentation: $tutorial-title,
              command: frame-start-tutorial,
              activate-callback: method (sheet)
                                   frame-start-tutorial(sheet-frame(sheet))
                                 end),
         */
         make(<button>,
              label: $playground-bitmap,
              documentation: $playground-title,
              command: frame-open-playground,
              activate-callback: method (sheet)
				   frame-open-playground(sheet-frame(sheet))
				 end),
         make(<button>,
	      label: $help-bitmap,
	      documentation: $help-topics-doc,
              command: frame-help-contents-and-index,
              activate-callback: method (sheet)
                                   frame-help-contents-and-index(sheet-frame(sheet))
                                 end))
end method make-help-tool-bar-buttons;


/// Active project gadget

define constant $active-project-title = "Select Active Project";

define method make-active-project-gadget
    (frame :: <environment-primary-frame>) => (gadget :: <option-box>)
  local method project-name
	    (project :: <project-object>) => (name :: <string>)
	  environment-object-primitive-name(project, project)
	end method project-name;
  let gadget
    = make(<option-box>,
	   documentation: $active-project-title,
	   label-key: project-name,
	   width: 150, fixed-width?: #t,
	   value-changed-callback: method (gadget :: <gadget>)
				     let project = gadget-value(gadget);
				     active-project() := project
				   end);
  tune-in($project-channel,
	  method (message :: <project-message>)
	    select (message by instance?)
	      <project-opened-message>, <project-closed-message> =>
		let projects = copy-sequence(open-projects());
		gadget-items(gadget) 
		  := keyed-sort(projects, key: project-name);
	      <project-now-active-message> =>
		let project = message.message-project;
		gadget-value(gadget) := project;
	      <no-active-project-message> =>
		gadget-value(gadget) := #f;
	      otherwise =>
		#f;
	    end
	  end,
	  message-type: <project-message>);
  gadget
end method make-active-project-gadget;


/// Primary frame's toolbar

define method make-environment-tool-bar-buttons
    (frame :: <environment-primary-frame>)
 => (buttons :: <sequence>)
  with-frame-manager (frame-manager(frame))
    let new-file-buttons = make-new-file-tool-bar-buttons(frame);
    let file-io-buttons  = make-file-io-tool-bar-buttons(frame);
    let project-gadget   = make-active-project-gadget(frame);
    let help-buttons     = make-help-tool-bar-buttons(frame);
    vector(make(<row-layout>, children: new-file-buttons, spacing: 0),
           make(<row-layout>, children: file-io-buttons,  spacing: 0),
	   project-gadget,
           make(<row-layout>, children: help-buttons,     spacing: 0))
   end
end method make-environment-tool-bar-buttons;


/// Primary frame for the environment

define sideways method environment-primary-frame
    ()
 => (frame :: <environment-frame>)
  let _port = default-port();
  let frame = _port & choose-matching-frame(_port, <environment-primary-frame>);
  assert(frame, "There is no primary environment frame");
  frame
end method environment-primary-frame;


/// Internal-only commands

define method frame-full-gc
    (frame :: <environment-frame>) => ()
  with-busy-cursor (frame)
    collect-garbage()
  end;
  environment-message("Garbage collection completed.", owner: frame);
  beep(frame)
end method frame-full-gc;

define method frame-full-gc-and-report
    (frame :: <environment-frame>) => ()
  let filename 
    = make(<file-locator>,
	   directory: working-directory(),
	   name:      "dylan-runtime.log");
  with-busy-cursor (frame)
    collect-garbage(print-stats?: #t)
  end;
  beep(frame);
  let message = format-to-string("Heap statistics dumped to %s", filename);
  environment-message(message, owner: frame);
  /*---*** This doesn't work because the file is locked...
  let message
    = format-to-string("Heap statistics dumped to %s\n\n"
		       "Would you like to view this file?",
		       filename);
  if (environment-question(message, owner: frame))
    frame-open-file(frame, filename: as(<string>, filename))
  end
  */
end method frame-full-gc-and-report;

define method frame-break-environment
    (frame :: <environment-frame>) => ()
  break("User requested break")
end method frame-break-environment;

define method frame-signal-error
    (frame :: <environment-frame>) => ()
  error("User requested error")
end method frame-signal-error;

define variable $shell-execute-dialog-width :: <integer> = 250;

define frame <shell-execute-dialog> (<dialog-frame>)
  pane shell-command-pane (dialog)
    make(<text-field>,
	 documentation: "Enter a command line to execute.");
  layout (dialog)
    horizontally (spacing: 4)
      make(<label>, label: "Command:");
      dialog.shell-command-pane;
    end;
  input-focus (dialog)
    dialog.shell-command-pane;
  keyword title: = "Shell Command";
end frame <shell-execute-dialog>;

define method frame-execute-shell-command
    (frame :: <environment-frame>) => ()
  let dialog = make(<shell-execute-dialog>,
		    owner: frame,
		    width: max($shell-execute-dialog-width, 250));
  if (start-dialog(dialog))
    let (width, height) = frame-size(dialog);
    $shell-execute-dialog-width := width;
    let name = dialog.shell-command-pane.gadget-value;
    unless (empty?(name))
      os/run-application(name)
    end
  end
end method frame-execute-shell-command;

define method frame-load-library-command
    (frame :: <environment-frame>) => ()
  let filename = choose-file(title: "Load Library",
			     owner: frame,
			     direction: #"input",
			     //---*** NOTE: Should add this to filters-for-file-types!
			     filters: #[#["Libraries", "*.dll"]]);
  when (filename)
    load-library(filename)
  end
end method frame-load-library-command;

define variable $find-registry-project-dialog-width :: <integer> = 250;

define frame <find-registry-project-dialog> (<dialog-frame>)
  pane library-name-pane (dialog)
    make(<text-field>,
	 documentation: "Enter a library name to lookup in the registry.");
  layout (dialog)
    horizontally (spacing: 4)
      make(<label>, label: "Library:");
      dialog.library-name-pane;
    end;
  input-focus (dialog)
    dialog.library-name-pane;
  keyword title: = "Open Registry Project";
end frame <find-registry-project-dialog>;

define method frame-open-project-for-library
    (frame :: <environment-frame>) => ()
  let dialog
    = make(<find-registry-project-dialog>,
	   owner: frame,
	   width: max($find-registry-project-dialog-width, 250));
  if (start-dialog(dialog))
    let (width, height) = frame-size(dialog);
    $find-registry-project-dialog-width := width;
    let name = dialog.library-name-pane.gadget-value;
    unless (empty?(name))
      open-project-file(as(<file-locator>, name), frame: frame)
    end
  end
end method frame-open-project-for-library;


/// Command-line windows

define open generic find-command-line-window
    (frame :: <environment-frame>) => ();


/// Primary frame command table

define command-table *primary-frame-file-open-command-table* (*global-command-table*)
  menu-item "New..."  = frame-new-file,
    accelerator:   make-keyboard-gesture(#"n", #"control"),
    documentation: "Creates a new document.";
  menu-item "Open..." = frame-open-file,
    accelerator:   make-keyboard-gesture(#"o", #"control"),
    documentation: "Opens an existing document.";
end command-table *primary-frame-file-open-command-table*;

define command-table *primary-frame-file-io-command-table* (*global-command-table*)
  include *primary-frame-file-open-command-table*;
  include *recent-projects-command-table*;
  include *recent-files-command-table*;
  include *exit-command-table*;
end command-table *primary-frame-file-io-command-table*;

define command-table *primary-frame-internal-command-table* (*global-command-table*)
  menu-item "Open Registry Project..." = frame-open-project-for-library,
    documentation: "Opens a project for a supplied library name.";
  separator;
  menu-item "Dump Environment Heap" = frame-full-gc-and-report,
    documentation: "Dumps heap statistics for the environment to dylan-runtime.log";
  separator;
  menu-item "Full GC" = frame-full-gc,
    documentation: "Forces a complete garbage collection of the environment.";
  separator;
  menu-item "Shell Command" = frame-execute-shell-command,
    documentation: "Executes a specified Windows command.";
  menu-item "Load Library" = frame-load-library-command,
    documentation: "Loads a DLL into the environment.";
  separator;
  menu-item "Signal Error" = frame-signal-error,
    documentation: "Causes the environment to signal an error.";
  menu-item "Enter Debugger" = frame-break-environment,
    documentation: "Causes the environment to break in order to enter the debugger.";
end command-table *primary-frame-internal-command-table*;

define command-table *primary-frame-tools-command-table* (*global-command-table*)
  menu-item "Attach Debugger..." = frame-attach-application,
    documentation: "Attach a debugger to a running process.";
  separator;
  menu-item $examples-title = frame-open-example,
    documentation: "Opens an example Dylan project.";
  menu-item $playground-title = frame-open-playground,
    documentation: "Opens a Dylan project to interact with.";
  separator;
  menu-item "Command Line..." = find-command-line-window,
    documentation: "Open a command line interface window.";
  separator;
  menu-item "Environment Options..." = frame-edit-options,
    documentation: "Enables you to change application options.";
end command-table *primary-frame-tools-command-table*;

define command-table *primary-frame-command-table* (*global-command-table*)
  menu-item "File"    = *primary-frame-file-io-command-table*;
  menu-item "Tools"   = *primary-frame-tools-command-table*;
  menu-item "Window"  = *single-window-command-table*;
  menu-item "Help"    = *environment-help-command-table*;
end command-table *primary-frame-command-table*;
