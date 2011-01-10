Module:    environment-debugger
Author:    Scott McKay, Andy Armstrong, Jason Trenouth
Version:   $HopeName: D-environment-debugger!interactor.dylan(trunk.14)
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// <DEBUGGER-INTERACTOR> (internal)

define class <debugger-interactor> (<dylan-interactor>)
end class <debugger-interactor>;

/// INTERACTOR-REMOTE-THREAD (environment-deuce)

define sealed method interactor-remote-thread 
    (interactor :: <debugger-interactor>)
 => (thread :: <thread-object>)
  let debugger = interactor.sheet-frame;
  debugger.debugger-thread
end method interactor-remote-thread;

/// INTERACTOR-STACK-FRAME-CONTEXT (environment-deuce)

define sealed method interactor-stack-frame-context
    (interactor :: <debugger-interactor>) 
 => (maybe-frame :: false-or(<stack-frame-object>))
  let debugger = interactor.sheet-frame;
  debugger.debugger-current-stack-frame
end method interactor-stack-frame-context;

// Register the transaction ID in the debugger frame at the start
// of the interaction

define method note-application-started-interaction
    (project :: <project-object>, thread :: <thread-object>,
     transaction-id) => ()
  do-project-debuggers
  (rcurry(debugger-started-interaction, transaction-id),
   project, thread: thread)
end method note-application-started-interaction;

define function debugger-started-interaction
    (debugger :: <debugger>, transaction-id)
  debugger.debugger-interactor-transaction := transaction-id
end function;

define method note-application-just-interacted
    (project :: <project-object>, thread :: <thread-object>) => ()
  unless (ignore-interactive-breakpoint-on-thread?(project, thread))
    do-project-debuggers
      (update-debugger-with-interaction, project, thread: thread)
  end;
end method note-application-just-interacted;

// Updates the debugger frame following an interaction.
// NOTE: the keyword argument is important here -- the Debugger
// Manager thread supplies this argument to update-debugger which
// will run in the Debugger Frame thread; cannot use function
// application-just-interacted? in update-debugger because of race
// conditions with the DM thread

define function update-debugger-with-interaction
    (debugger :: <debugger>)
  update-debugger(debugger, interacted?: #t)
end function;

define method note-application-interactive-results
    (project :: <project-object>, thread :: <thread-object>, transaction-id)
 => (stop? :: <boolean>)
  // We mark one of the interactors on this thread to indicate that
  // a result is pending, it will then update the buffer which in turn
  // will update all of the windows on that buffer.
  block (return)
    do-project-debuggers
       (method (debugger :: <debugger>)
	 let interactor-pane = debugger.debugger-interactor-pane;
	 interactor-pane.interactor-transaction := transaction-id;
	 interactor-pane.interactor-results-pending? := #t;
	 return();
       end,
       project, thread: thread, in-frame?: #f);
  end;
  do-frames(method (frame :: <frame>) => ()
	      if (instance?(frame, <environment-frame>))
		call-in-frame(frame, frame-note-interaction-returned, 
			      frame, thread, transaction-id)
	      end
	    end);
  let application :: <application> = project.project-application;
  let running? =
    application-state-at-code-entry(transaction-id) == #"running";
  application.application-target-app.application-running-on-code-entry?
    := running?;
  #t;
end method note-application-interactive-results;


/// $DEBUGGER-INTERACTOR-DOC

define constant $debugger-interactor-doc :: <string> = "Interactor";

/// <INTERACTOR-PANE> (internal)

define sealed pane <interactor-pane> 
    (/* <environment-frame> */)
  sealed slot interactor-project :: <project-object>,
    required-init-keyword: project:;
  sealed slot interactor-thread :: false-or(<thread-object>) = #f,
    setter: %interactor-thread-setter,
    init-keyword: thread:;
  sealed slot interactor-results-pending? :: <boolean> = #f;
  sealed slot interactor-transaction :: <object> = #f;

  pane %interactor-control (pane)
    begin
      let project = pane.interactor-project;
      let thread  = pane.interactor-thread;
      let buffer  = interactor-buffer-for-thread(project, thread);
      let interactor
	= make-dylan-interactor
	    (class: <debugger-interactor>,
	     buffer: buffer,
	     documentation: $debugger-pane-tooltips? & $debugger-interactor-doc);
      when (thread & ~buffer)
	interactor-buffer-for-thread(project, thread) 
	  := window-buffer(interactor)
      end;
      interactor
    end;
  pane %interactor-pane (pane)
    scrolling (scroll-bars: #"both")
      pane.%interactor-control
    end;
  layout (pane)
    pane.%interactor-pane
end pane <interactor-pane>;


define method interactor-thread-setter
    (thread :: false-or(<thread-object>), pane :: <interactor-pane>)
 => (thread :: false-or(<thread-object>))
  pane.%interactor-thread := thread;
  let project = pane.interactor-project;
  let buffer  = interactor-buffer-for-thread(project, thread);
  let window  = pane.%interactor-control;
  with-editor-state-bound (window)
    select-buffer(window, buffer);
    queue-redisplay(window, $display-all);
    redisplay-window(window)
  end;
  thread
end method interactor-thread-setter;

define method interactor-pane-enabled?-setter
    (enabled? :: <boolean>, pane :: <interactor-pane>)
 => (enabled? :: <boolean>)
  pane.%interactor-control.gadget-enabled? := enabled?
end method interactor-pane-enabled?-setter;

define method interactor-pane-default-focus
    (pane :: <interactor-pane>) => (sheet :: <sheet>)
  pane.%interactor-control
end method interactor-pane-default-focus;


/// Interactor buffer management
///
/// We share a single buffer for each thread in the application, so that
/// multiple debuggers on that thread show the same interactor contents.

define method interactor-buffer-table
    (project :: <project-object>) => (table :: <object-table>)
  get-property(project.project-properties, #"thread-buffer", default: #f)
    | begin
	let table = make(<object-table>);
	put-property!(project.project-properties, #"thread-buffer", table);
	table
      end
end method interactor-buffer-table;

define method interactor-buffer-for-thread
    (project :: <project-object>, thread == #f)
 => (buffer :: <buffer>)
  environment-empty-buffer()
end method interactor-buffer-for-thread;

define method interactor-buffer-for-thread
    (project :: <project-object>, thread :: <thread-object>)
 => (buffer :: <buffer>)
  let table = interactor-buffer-table(project);
  element(table, thread, default: #f)
    | begin
	let buffer = make-interactor-buffer();
	interactor-buffer-for-thread(project, thread) := buffer
      end
end method interactor-buffer-for-thread;

define method interactor-buffer-for-thread-setter
    (buffer :: false-or(<buffer>), project :: <project-object>, 
     thread :: <thread-object>)
 => (buffer :: false-or(<buffer>))
  let table = interactor-buffer-table(project);
  if (buffer)
    table[thread] := buffer
  else
    let old-buffer = element(table, thread, default: #f);
    old-buffer & kill-buffer(old-buffer, frame: #f, editor: $environment-editor);
    remove-key!(table, thread)
  end;
  buffer
end method interactor-buffer-for-thread-setter;

define function note-application-thread-finished
    (project :: <project-object>, thread :: <thread-object>) => ()
  //---*** andrewa: how do we ensure the buffer gets destroyed?
  interactor-buffer-for-thread(project, thread) := #f
end function note-application-thread-finished;


/// FRAME-NOTE-INTERACTIVE-COMPILATION-WARNINGS (Environment Tools)
//  The important method on the <debugger> frame, which ships the
//  compiler warnings to Deuce for display in the interactor pane.

define method frame-note-interactive-compilation-warnings
    (frame :: <debugger>, thread :: <thread-object>, 
     id :: <object>, warnings :: <sequence>) => ()
  if (frame.debugger-thread == thread)
    let interactor-pane = frame.debugger-interactor1-pane;
    let control = interactor-pane.%interactor-control;
    interactor-receive-warnings(control, warnings, transaction-id: id);
  end if;
end method frame-note-interactive-compilation-warnings;


/// UPDATE-DEBUGGER-INTERACTOR-PANE (internal)

define function update-debugger-interactor-pane
    (debugger :: <debugger>, #key refresh? :: <boolean> = #f) => ()
  //--- We just update one interactor pane, the others will redisplay
  //--- automatically because they share a buffer.
  let interactor-pane = debugger.debugger-interactor-pane;
  if (interactor-pane.interactor-results-pending?)
    let id = interactor-pane.interactor-transaction;
    let environment-object-sequence 
      = fetch-interactor-return-values(debugger.frame-project, id);
    interactor-receive-values
      (interactor-pane.%interactor-control,
       environment-object-sequence,
       transaction-id: id);
    dispose-interactor-return-values(debugger.frame-project, id);
    interactor-pane.interactor-results-pending? := #f;
  end if;
  let window = interactor-pane.%interactor-control;
  redisplay-debugger-editor-window(window, refresh?: refresh?)
end function update-debugger-interactor-pane;


/// FRAME-SHOW-CONTENTS (internal)

define constant <object-with-contents>
  = type-union(<application-object>, <result-subset>);

define function frame-show-target-contents
    (frame :: <debugger>) => ()
  frame-show-contents(frame, frame-target-to-browse(frame))
end function frame-show-target-contents;

define method frame-show-contents
    (frame :: <debugger>, object :: <object-with-contents>) => ()
  with-busy-cursor (frame)
    interactor-show-contents
      (frame.debugger-interactor-pane.%interactor-control, object)
  end
end method frame-show-contents;

define method update-frame-commands-for-browse-target
    (frame :: <debugger>, object :: <object>) => ()
  next-method();
  let project = frame-current-project(frame);
  let contents? = instance?(object, <object-with-contents>);
  command-enabled?(frame-show-target-contents, frame) := contents?
end method update-frame-commands-for-browse-target;


/// COMMAND-TABLE-FOR-TARGET (environment-tools)

define command-table *source-form-menu-comtab* (*global-command-table*)
  //---*** andrewa: this isn't implemented yet!
  // menu-item "Redo" = frame-redo-target,
  //  documentation: "Redoes the selected interactive command";
  include *popup-menu-clipboard-command-table*;
  include *popup-menu-properties-command-table*;
end command-table *source-form-menu-comtab*;

define method command-table-for-target
    (frame :: <debugger>, object :: <shell-input>)
 => (comtab :: <command-table>)
  *source-form-menu-comtab*
end method command-table-for-target;


// FRAME-EXTRA-COMMAND-TABLE-FOR-TARGET (environment-tools)
//
// Add the 'Show Contents' menu item to all debugger popup menus for
// all application objects.

define command-table *debugger-extra-command-table* (*global-command-table*)
  menu-item "Show Contents" = frame-show-target-contents,
    documentation: "Shows the contents of the selected object in the interactor.";
end command-table *debugger-extra-command-table*;

define method frame-extra-command-table-for-target
    (frame :: <debugger>, object :: <object-with-contents>)
 => (comtab :: <command-table>)
  *debugger-extra-command-table*
end method frame-extra-command-table-for-target;


// FRAME-EXTRA-COMMAND-TABLE-FOR-TARGET (environment-tools)
//
// Add the 'Run to Cursor' menu item to all debugger popup menus for
// all breakpoint locations.

define command-table *debugger-breakpoint-extra-command-table* (*global-command-table*)
  menu-item "Run to Cursor" = frame-run-to-target,
    documentation: "Sets temporary breakpoint at cursor and resumes execution of application.";
end command-table *debugger-breakpoint-extra-command-table*;

define method frame-extra-command-table-for-target
    (frame :: <debugger>, object :: <breakpoint-location>)
 => (comtab :: <command-table>)
  *debugger-breakpoint-extra-command-table*
end method frame-extra-command-table-for-target;


// FRAME-AVAILABLE-MODULES (environment-tools)
//
// Only make the interactive modules available in the module gadget.

define method frame-available-modules
    (frame :: <debugger>) => (items :: <sequence>)
  let project = frame.frame-project;
  let modules = make(<stretchy-vector>);
  for (module in next-method())
    let library = environment-object-library(project, module);
    if (library-interactive?(project, library))
      add!(modules, module)
    end
  end;
  modules
end method frame-available-modules;

