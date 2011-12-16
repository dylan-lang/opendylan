Module:    environment-debugger
Author:    Bill Chiles, Jason Trenouth, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// <DEBUGGER-COMMAND> (internal)

define sealed class <debugger-command> (<basic-command>)
  sealed constant slot command-label :: <string> = "that",
    init-keyword: label:;
  sealed constant slot progress-format-string :: <string> = "",
    init-keyword: format-string:;
  sealed constant slot progress-format-arguments :: <sequence> = #[],
    init-keyword: format-arguments:;
end class;

/// <DEBUGGER-ENABLED-COMMAND> (internal)

define sealed class <debugger-enabled-command> (<debugger-command>)
end class;

/// <DEBUGGER-ZOOM-COMMAND> (internal)

define sealed class <debugger-zoom-command> (<debugger-enabled-command>)
  sealed constant each-subclass slot zoom-command-zoom :: <symbol>,
    init-keyword: zoom:;
end class;

/// <DEBUGGER-STEP-COMMAND> (internal)

define sealed class <debugger-step-command> (<debugger-enabled-command>)
end class;

/// <DEBUGGER-ABORT-COMMAND> (internal)

define sealed class <debugger-abort-command> (<debugger-enabled-command>)
end class;

/// MAKE (dylan)

define sealed domain make (subclass(<debugger-command>));

/// INITIALIZE (dylan)

define sealed domain initialize (<debugger-command>);


/// DEBUGGER-COMMANDs
///
/// ---*** DEBUGGER: add command-label slots so that we can be more specific
/// about a command in a user message

define sealed class <debugger-abort-current-command> (<debugger-abort-command>)
  keyword format-string: = "Aborting current operation ...";
end class;

define sealed class <debugger-abort-all-command> (<debugger-abort-command>)
  keyword format-string: = "Aborting entire nested operation ...";
end class;

define sealed class <debugger-choose-restart-command> (<debugger-enabled-command>)
  keyword format-string: = "Raising restart chooser ...";
end class;

define sealed class <debugger-step-into-command> (<debugger-step-command>)
  keyword format-string: = "Stepping into next function call ...";
end class;

define sealed class <debugger-step-over-command> (<debugger-step-command>)
  keyword format-string: = "Stepping over next function call ...";
end class;

define sealed class <debugger-step-out-command> (<debugger-step-command>)
  keyword format-string: = "Stepping out of current function call ...";
end class;

define sealed class <debugger-find-symbols-near-command> (<debugger-enabled-command>)
  keyword format-string: = "Raising symbols browser ...";
end class;

define sealed class <debugger-show-registers-command> (<debugger-enabled-command>)
  keyword format-string: = "Raising registers browser ...";
end class;

define sealed class <debugger-display-memory-command> (<debugger-enabled-command>)
  keyword format-string: = "Raising memory browser ...";
end class;

define sealed class <debugger-set-value-command> (<debugger-enabled-command>)
  keyword format-string: = "Raising value setter ...";
end class;

define sealed class <debugger-evaluate-command> (<debugger-enabled-command>)
  keyword format-string: = "Raising evaluation dialog ...";
end class <debugger-evaluate-command>;

define sealed class <debugger-top-stack-frame-command> (<debugger-enabled-command>)
end class;

define sealed class <debugger-up-stack-frame-command> (<debugger-enabled-command>)
end class;

define sealed class <debugger-down-stack-frame-command> (<debugger-enabled-command>)
end class;

define sealed class <debugger-bottom-stack-frame-command> (<debugger-enabled-command>)
end class;

define sealed class <debugger-zoom-debugging-command> (<debugger-zoom-command>)
  keyword zoom: = #"zoom-debugging";
end class;

define sealed class <debugger-zoom-interacting-command> (<debugger-zoom-command>)
  keyword zoom: = #"zoom-interacting";
end class;

define sealed class <debugger-expand-all-command> (<debugger-enabled-command>)
end class;

define sealed class <debugger-expand-command> (<debugger-enabled-command>)
end class;

define sealed class <debugger-collapse-command> (<debugger-enabled-command>)
end class;

define sealed class <debugger-collapse-all-command> (<debugger-enabled-command>)
end class;

define sealed class <debugger-refresh-all-debuggers-command> (<debugger-command>)
end class;

/// *THREAD-COMMAND-TABLE* (internal)
///
/// NB motley assortment of debugger specific commands

define variable $step-over-bitmap :: <label-type> = "->";
define variable $step-into-bitmap :: <label-type> = "-v";
define variable $step-out-bitmap  :: <label-type> = "-^";

define constant $step-over-doc = "Steps to current function\'s next known source location (skips over function calls).";
define constant $step-into-doc = "Steps to called function\'s next known source location (enters function calls)";
define constant $step-out-doc = "Steps to caller function\'s next known source location (leaves current function).";

define constant $step-over-title = "Step Over";
define constant $step-into-title = "Step Into";
define constant $step-out-title = "Step Out";

define command-table *thread-command-table* (*global-command-table*)
  menu-item "Abort"     = <debugger-abort-current-command>,
    documentation: "Aborts current nested operation (without quitting connected application).";
  menu-item "Abort All" = <debugger-abort-all-command>,
    documentation: "Aborts whole operation (without quitting connected application)";
  menu-item "Continue..." = <debugger-choose-restart-command>,
    documentation: "Chooses and invokes application restart.";
  separator;
  menu-item $step-over-title = <debugger-step-over-command>,
    accelerator:   make-keyboard-gesture(#"f11"),
    image: $step-over-bitmap,
    documentation: $step-over-doc;
  menu-item $step-into-title = <debugger-step-into-command>,
    accelerator:   make-keyboard-gesture(#"f12"),
    image: $step-into-bitmap,
    documentation: $step-into-doc;
  menu-item $step-out-title  = <debugger-step-out-command>,
    accelerator:   make-keyboard-gesture(#"f12", #"shift"),
    image: $step-out-bitmap,
    documentation: $step-out-doc;
  separator;
  menu-item "Select Thread..." = debugger-select-thread,
    documentation: "Selects a thread for this debugger.";
end command-table *thread-command-table*;


/// *DEBUGGER-GO-COMMAND-TABLE* (internal)
///
/// NB adds stack navigation commands to history navigation ones

define variable $top-of-stack-bitmap    :: <label-type> = "|<";
define variable $bottom-of-stack-bitmap :: <label-type> = "<";
define variable $up-stack-bitmap        :: <label-type> = ">";
define variable $down-stack-bitmap      :: <label-type> = ">|";

/*
define constant $top-of-stack-doc = "Selects newest stack frame.";
define constant $bottom-of-stack-doc = "Selects oldest stack frame.";
define constant $down-stack-doc = "Selects next older stack frame.";
define constant $up-stack-doc = "Selects next newer stack frame.";

define constant $top-of-stack-title = "Top of Stack";
define constant $up-stack-title = "Up Stack";
define constant $down-stack-title = "Down Stack";
define constant $bottom-of-stack-title = "Bottom of Stack";
*/

/// *DEBUGGER-ZOOM-COMMANDS-COMMAND-TABLE* (internal)
///
/// NB different views of the debugger (eg can just show interactor)
/// --- hughg, 1998/01/31: Try something simpler for switching 2 layouts.

define command-table *debugger-zoom-commands-command-table* (*global-command-table*)
  menu-item "Debugging Layout"   = <debugger-zoom-debugging-command>;
  menu-item "Interacting Layout" = <debugger-zoom-interacting-command>;
end command-table;

/// *DEBUGGER-TREE-NODE-COMMAND-TABLE* (internal)

define command-table *debugger-tree-node-command-table* (*global-command-table*)
  menu-item "Expand All"   = <debugger-expand-all-command>;
  menu-item "Expand"       = <debugger-expand-command>;
  menu-item "Collapse"     = <debugger-collapse-command>;
  menu-item "Collapse All" = <debugger-collapse-all-command>;
end command-table;


/// *DEBUGGER-BAR-OPTIONS-COMMAND-TABLE* (internal)

define command-table *debugger-bar-options-command-table* (*global-command-table*)
end command-table *debugger-bar-options-command-table*;

add-command-table-menu-item
  (*debugger-bar-options-command-table*,
   "", <check-box>, vector(#"tool-bar", #"status-bar", #"context"),
   items: #[#["Toolbar",        #"tool-bar"],
	    #["Status Bar",     #"status-bar"],
	    #["Context Window", #"context"]],
   label-key: first, value-key: second,
   callback: method (menu-box)
               frame-show-bars?(sheet-frame(menu-box), gadget-value(menu-box))
             end);

define method frame-show-bars?
    (debugger :: <debugger>, bars :: <sequence>) => ()
  let top-sheet   = top-level-sheet(debugger);
  let tool-bar    = frame-tool-bar(debugger);
  let status-bar  = frame-status-bar(debugger);
  let context     = debugger-context-pane(debugger);
  let tool-bar?   = member?(#"tool-bar",   bars);
  let status-bar? = member?(#"status-bar", bars);
  let context?    = member?(#"context",    bars);
  let relayout?   = #f;
  local method show-or-hide (sheet, present?) => ()
	  // Work extra hard to ensure that everything gets re-layed out,
	  // since bars can have associated "decorations"
	  when (sheet & sheet-withdrawn?(sheet) == present?)
	    sheet-withdrawn?(sheet) := ~present?;
	    for (s = sheet then sheet-parent(s),
		 until: s == top-sheet)
	      sheet-layed-out?(s) := #f
	    end;
	    relayout? := #t
	  end
	end method;
  show-or-hide(tool-bar,   tool-bar?);
  show-or-hide(status-bar, status-bar?);
  show-or-hide(context,    context?);
  when (relayout?)
    relayout-children(top-sheet);
    relayout-parent(tool-bar | status-bar | context);
    sheet-mapped?(tool-bar)   := tool-bar?;
    sheet-mapped?(status-bar) := status-bar?;
    sheet-mapped?(context)    := context?;
  end
end method frame-show-bars?;


/// *DEBUGGER-VIEW-COMMAND-TABLE* (internal)
///
/// NB adds zoom and tree commands to common view menu.

define command-table *debugger-view-command-table* (*global-command-table*)
  include *debugger-bar-options-command-table*;
  include *debugger-zoom-commands-command-table*;
  separator;
  include *debugger-tree-node-command-table*;
  separator;
  include *view-refresh-command-table*;
  menu-item "Refresh All Debuggers" = <debugger-refresh-all-debuggers-command>,
    accelerator:   make-keyboard-gesture(#"f5", #"alt"),
    documentation: "Refreshes all debugger windows for current project.";
  menu-item "Debugger Options..." = frame-edit-options,
    documentation: "Enables you to change debugger options.";
end command-table;


/// DEBUGGER-GO-COMMAND-TABLE (internal)

define command-table *debugger-go-command-table* (*global-command-table*)
  include *browse-locations-command-table*;
  separator;
  menu-item "Registers" = debugger-show-registers,
    documentation: "Shows the contents of the current thread's registers";
end command-table *debugger-go-command-table*;


///  FRAME-EDIT-OPTIONS (environment-framework)

define sealed method frame-edit-options (debugger :: <debugger>) => ()
  let dialog = make(<options-dialog>, owner: debugger);
  if (start-dialog(dialog))
    update-from-dialog(debugger, dialog)
  end
end method frame-edit-options;

/// *DEBUGGER-FILE-COMMAND-TABLE* (internal)

define command-table *debugger-file-command-table* (*global-command-table*)
  menu-item "New Window" = clone-tool,
    accelerator:   make-keyboard-gesture(#"n", #"control"),
    documentation: "Opens a new debugger window.";
//---*** andrewa: removed for 1.0
//  menu-item "New Linked Window" = clone-and-link-tool,
//    documentation: "Opens another debugger window, linked to its selection.";
  include *export-command-table*;
  // include *print-command-table*;
  menu-item "Close" = frame-close-file,
    accelerator:   make-keyboard-gesture(#"f4", #"alt"),
    documentation: "Closes the debugger window.";
end command-table *debugger-file-command-table*;

/// *DEBUGGER-COMMAND-TABLE* (internal)

define command-table *debugger-command-table* (*global-command-table*)
  menu-item "File"        = *debugger-file-command-table*;
  menu-item "Edit"        = *edit-command-table*;
  menu-item "View"        = *debugger-view-command-table*;
  menu-item "Go"          = *debugger-go-command-table*;
  menu-item "Project"     = *project-command-table*;
  menu-item "Application" = *run-command-table*;
  menu-item "Thread"      = *thread-command-table*;
  menu-item "Window"      = *windows-command-table*;
  menu-item "Help"        = *environment-help-command-table*;
end command-table *debugger-command-table*;


/// EXECUTE-DEBUGGER-FUNCTION (internal)

define function execute-debugger-function 
    (function :: <function>, debugger :: <debugger>,
     #key message :: <string> = "Processing ...")
 => (#rest values)
  let return-values :: <sequence> = #[];
  with-busy-cursor (debugger)
    noting-progress (debugger, message)
      let (#rest function-values) = function(debugger);
      return-values := function-values;
      note-progress (1, 1, label: "");
    end;
  end;
  apply(values, return-values)
end function;  


/// EXECUTE-COMMAND (duim-frames)

define sealed method do-execute-command
    (debugger :: <debugger>, command :: <debugger-command>)
 => (#rest values)
  execute-debugger-function
    (curry(execute-debugger-command, command),
     debugger,
     message: apply(format-to-string,
		    command.progress-format-string, command.progress-format-arguments))
end method;


/// ---*** DEBUGGER: may want to offer option to stop application or
/// refresh debugger here or may want to disable commands in menu bars
/// and tool bars etc
    
define sealed method do-execute-command
    (debugger :: <debugger>, command :: <debugger-enabled-command>) => (#rest values)
  case
    // ---*** DEBUGGER: probably tested before we get here (if so how do we present message)
    ~command-enabled?(command, debugger) =>
      environment-error-message
	(format-to-string
	   ("You need to stop the application in order to do %s.", 
	    command.command-label),
	 owner: debugger);
    ~debugger.debugger-updated? =>
      environment-error-message
	(format-to-string
	   ("You need to refresh the debugger in order to do %s.", 
	    command.command-label),
	 owner: debugger);
    otherwise =>
      next-method();
  end case;
end method;

/// EXECUTE-DEBUGGER-COMMAND (internal)

define sealed method execute-debugger-command
    (command :: <debugger-abort-current-command>, debugger :: <debugger>)
  abort-application(debugger)
end method;

define sealed method execute-debugger-command
    (command :: <debugger-abort-all-command>, debugger :: <debugger>)
  abort-application(debugger, order-restarts: reverse!)
end method;

define sealed method abort-application (debugger :: <debugger>, #key order-restarts = identity)
  let project :: <project-object> = debugger.frame-project;
  let thread :: <thread-object> = debugger.debugger-thread;
  let restarts :: <sequence> = order-restarts(application-thread-restarts(project, thread));
  let abort-restart :: false-or(<restart-object>) = application-abort-restart(project, restarts);
  if (abort-restart)
    invoke-application-restart(project, thread, abort-restart);
  else
    environment-error-message
      ("No abort restart is defined in this thread.",
       owner: debugger);
  end if;
end method;

define function application-abort-restart (project :: <project-object>, restarts :: <sequence>)
 => (abort-restart :: false-or(<restart-object>))
  block (return)
    for (restart in restarts)
      if (application-restart-abort?(project, restart))
	return(restart);
      end if;
    end for;
  end block;
end function;

define variable $choose-restart-dialog-width  :: <integer> = 500;
define variable $choose-restart-dialog-height :: <integer> = 300;

define sealed method execute-debugger-command
    (command :: <debugger-choose-restart-command>, debugger :: <debugger>)
  let project = debugger.frame-project;
  let thread = debugger.debugger-thread;
  let restarts = application-thread-restarts(project, thread);
  if (empty?(restarts))
    environment-message
      (format-to-string("No available restarts in %s.",
			environment-object-display-name(project, thread, #f)),
       owner: debugger)
  else
    let (chosen :: false-or(<restart-object>), success?, width, height)
      = choose-from-dialog(restarts,
			   title: "Choose Restart",
			   default-item: restarts[0],
			   label-key: curry(application-restart-message, project),
			   width:  $choose-restart-dialog-width,
			   height: $choose-restart-dialog-height);
    when (chosen & success?)
      $choose-restart-dialog-width  := width;
      $choose-restart-dialog-height := height;
      invoke-application-restart(project, debugger.debugger-thread, chosen);
    end
  end
end method;

/// ---*** DEBUGGER: these step commands need to have debug-point-handlers

define sealed method execute-debugger-command
    (command :: <debugger-step-into-command>, debugger :: <debugger>)
  step-application-into(debugger.frame-project, debugger.debugger-thread)
end method;

define sealed method execute-debugger-command
    (command :: <debugger-step-over-command>, debugger :: <debugger>)
  step-application-over(debugger.frame-project, 
			debugger.debugger-thread,
			stack-frame: debugger.debugger-stepping-stack-frame)
end method;

define sealed method execute-debugger-command
    (command :: <debugger-step-out-command>, debugger :: <debugger>)
  let frame = debugger.debugger-stepping-out-stack-frame;
  step-application-out(debugger.frame-project, 
		       debugger.debugger-thread,
		       stack-frame: frame)
end method;

define sealed method execute-debugger-command
    (command :: <debugger-find-symbols-near-command>, debugger :: <debugger>)
  // ---*** DEBUGGER: blat symbols into table in non-modal dialog
  // ---*** APPLICATION: ask for find-symbols-near-to API
end method;

define sealed method execute-debugger-command
    (command :: <debugger-show-registers-command>, debugger :: <debugger>)
  // ---*** DEBUGGER: blat registers in hex into table in non-modal dialog
  // ---*** ENVIRONMENT: may be add registers tab to project controller?
  // ---*** APPLICATION: ask for show-registers API
end method;

define sealed method execute-debugger-command
    (command :: <debugger-display-memory-command>, debugger :: <debugger>)
  // ---*** DEBUGGER: blat memory in hex into table in non-modal dialog
  // ---*** ENVIRONMENT: may be add memory tab to project controller?
  // ---*** APPLICATION: ask for DISPLAY MEMORY API
end method;

define sealed method execute-debugger-command
    (command :: <debugger-set-value-command>, debugger :: <debugger>)
  // ---*** APPLICATION: stopgap: snarf console debugger code to set value in runtime
end method;

define sealed method execute-debugger-command
    (command :: <debugger-evaluate-command>, debugger :: <debugger>)
  // ---*** APPLICATION: stopgap: snarf console debugger code to evaluate call in runtime
end method;

define sealed method execute-debugger-command
    (command :: <debugger-top-stack-frame-command>, debugger :: <debugger>)
  let top = debugger.debugger-filtered-stack[0];
  stack-pane-change-frame(debugger, top);
end method;

/// ---*** DEBUGGER: this search to find the next frame might be too
/// inefficient so could build an up+down table when stack is
/// refiltered

define sealed method execute-debugger-command
    (command :: <debugger-up-stack-frame-command>, debugger :: <debugger>)
  let current = debugger-current-stack-frame(debugger);
  if (current)
    let filtered-stack = debugger.debugger-filtered-stack;
    let index = find-key(filtered-stack, curry(\==, current));
    let up = filtered-stack[max(0, index - 1)];
    stack-pane-change-frame(debugger, up);
  end
end method;

define sealed method execute-debugger-command
    (command :: <debugger-down-stack-frame-command>, debugger :: <debugger>)
  let current = debugger-current-stack-frame(debugger);
  if (current)
    let filtered-stack = debugger.debugger-filtered-stack;
    let index = find-key(filtered-stack, curry(\==, current));
    let down = filtered-stack[min(filtered-stack.size - 1, index + 1)];
    stack-pane-change-frame(debugger, down);
  end
end method;

define sealed method execute-debugger-command
    (command :: <debugger-bottom-stack-frame-command>, debugger :: <debugger>)
  let filtered-stack = debugger.debugger-filtered-stack;
  let bottom = filtered-stack[filtered-stack.size - 1];
  stack-pane-change-frame(debugger, bottom);
end method;

define sealed method execute-debugger-command
    (command :: <debugger-zoom-command>, debugger :: <debugger>)
  debugger.debugger-zoom := command.zoom-command-zoom;
  update-debugger(debugger)
end method;

define sealed method execute-debugger-command
    (command :: <debugger-expand-all-command>, debugger :: <debugger>)
  walk-nodes(expand-node, debugger.debugger-stack-gadget);
end method;

define sealed method execute-debugger-command
    (command :: <debugger-expand-command>, debugger :: <debugger>)
  let tree = debugger.debugger-stack-gadget;
  let object = tree.gadget-value;
  if (object)
    expand-node(tree, find-node(tree, object));
  end if;
end method;

define sealed method execute-debugger-command
    (command :: <debugger-collapse-command>, debugger :: <debugger>)
  let tree = debugger.debugger-stack-gadget;
  let object = tree.gadget-value;
  if (object)
    contract-node(tree, find-node(tree, object));
  end if;
end method;

define sealed method execute-debugger-command
    (command :: <debugger-collapse-all-command>, debugger :: <debugger>)
  walk-nodes(contract-node, debugger.debugger-stack-gadget);
end method;

define sealed method execute-debugger-command
    (command :: <debugger-refresh-all-debuggers-command>, debugger :: <debugger>)
  refresh-all-debuggers(debugger.frame-project)
end method;


/// WALK-NODES (internal)
/// 
/// ---*** DUIM: perhaps DUIM should supply WALK-NODES

define function walk-nodes (function :: <function>, tree :: <tree-control>)
 => ()
  let nqueue = make(<deque>);
  local method walk-tree ()
	  unless (empty?(nqueue))
	    let node = pop(nqueue);
	    function(tree, node);
	    for (child in node-children(node))
	      push(nqueue, child);
	    end for;
	    walk-tree();
	  end unless;
	end method;
  for (object in tree-control-roots(tree))
    push(nqueue, find-node(tree, object));
  end for;
  walk-tree();
end function;


/// UPDATE-DEBUGGER-COMMAND-TABLE (internal)
///
/// This should take care of menubars and toolbars

define function update-debugger-command-table (debugger :: <debugger>)
  local method update (item, comtab :: <command-table>)
	  let object = decorator-object(item);
	  let type   = decorator-type(item);
	  select (type)
	    <command>, <function> => update-debugger-command(debugger, object);
	    <command-table>       => do-command-table-menu-items(update, object);
	    otherwise             => #f;	// <separator> etc
	  end select;
	end method;
  do-command-table-menu-items(update, frame-command-table(debugger));
end function;


/// UPDATE-DEBUGGER-COMMAND (internal)

define sealed method update-debugger-command
    (debugger :: <debugger>, commandoid) => ()
  #f
end method update-debugger-command;

/// ---*** DEBUGGER: I hope the following enabling and disabling based
/// on function equality works

define sealed method update-debugger-command
    (debugger :: <debugger>, command == refresh-frame)
 => ()
  command-enabled?(command, debugger) := debugger.debugger-enabled?
end method update-debugger-command;

define sealed method update-debugger-command
    (debugger :: <debugger>, command == debugger-select-thread)
 => ()
  let can-switch?
    = ~$debugger-settings.one-debugger-per-thread
        & debugger.debugger-enabled?;
  command-enabled?(command, debugger) := can-switch?
end method update-debugger-command;

define sealed method update-debugger-command
    (debugger :: <debugger>, command :: <debugger-enabled-command>)
 => ()
  command-enabled?(command, debugger) := debugger.debugger-enabled?
end method update-debugger-command;

define sealed method update-debugger-command
    (debugger :: <debugger>, command :: <debugger-step-command>)
 => ()
  command-enabled?(command, debugger)
    := debugger.debugger-enabled?
         & begin
	     let application = debugger.frame-project.project-application;
	     select (application & application.application-state)
	       #"stopped" => #t;
	       otherwise  => #f;
	     end
	   end
end method update-debugger-command;

define sealed method update-debugger-command
    (debugger :: <debugger>, command :: <debugger-abort-command>)
 => ()
  command-enabled?(command, debugger)
    := debugger.debugger-enabled?
         & begin
	     let project = debugger.frame-project;
	     let thread = debugger.debugger-thread;
	     let restarts = application-thread-restarts(project, thread);
	     application-abort-restart(project, restarts)
	   end
         & #t
end method update-debugger-command;

define sealed method update-debugger-command
    (debugger :: <debugger>, command :: <debugger-choose-restart-command>)
 => ()
  command-enabled?(command, debugger)
    := debugger.debugger-enabled?
         & begin
	     let project = debugger.frame-project;
	     let thread = debugger.debugger-thread;
	     ~empty?(application-thread-restarts(project, thread));
	   end
end method update-debugger-command;


/// FRAME-NOTE-APPLICATION-STATE-CHANGED
///
/// ---*** DEBUGGER: could do these with specialist debugger command subclass

define sealed method frame-note-application-state-changed
    (debugger :: <debugger>, state :: false-or(<application-state>))
 => ()
  next-method();
  if (debugger.frame-mapped?)
    update-debugger-command-table(debugger);
  end if;
end method;


/// MAKE-STACK-NAVIGATION-TOOL-BAR-BUTTONS (internal)

/*---*** andrewa: We've decided to remove these for the moment
define function make-stack-navigation-tool-bar-buttons
    (frame :: <environment-frame>)
 => (buttons :: <sequence>)
  vector(make(<button>,
              label: $top-of-stack-bitmap,
	      documentation: $top-of-stack-title,
              command: <debugger-top-stack-frame-command>),
         make(<button>, 
              label: $up-stack-bitmap,
	      documentation: $up-stack-title,
              command: <debugger-up-stack-frame-command>),
         make(<button>, 
              label: $down-stack-bitmap,
	      documentation: $down-stack-title,
              command: <debugger-down-stack-frame-command>),
         make(<button>, 
              label: $bottom-of-stack-bitmap,
	      documentation: $bottom-of-stack-title,
              command: <debugger-bottom-stack-frame-command>))
end function;
*/

/// MAKE-STEP-TOOL-BAR-BUTTONS (internal)

define function make-stepping-tool-bar-buttons
    (frame :: <environment-frame>)
 => (buttons :: <sequence>)
  vector(make(<button>,
              label: $step-over-bitmap,
	      documentation: $step-over-title,
              command: <debugger-step-over-command>),
         make(<button>, 
              label: $step-into-bitmap,
	      documentation: $step-into-title,
              command: <debugger-step-into-command>),
	 make(<button>, 
              label: $step-out-bitmap,
	      documentation: $step-out-title,
              command: <debugger-step-out-command>))
end function;


/// MAKE-ENVIRONMENT-TOOL-BAR-BUTTONS (environment-tools)

define sealed method make-environment-tool-bar-buttons (debugger :: <debugger>)
 => (buttons :: <sequence>)
  with-frame-manager (frame-manager(debugger))
    // let stack-navigation-buttons = make-stack-navigation-tool-bar-buttons(debugger);
    let step-buttons = make-stepping-tool-bar-buttons(debugger);
    let module-buttons = make-module-tool-bar-buttons(debugger);
    let next-buttons = next-method();
    concatenate-as(<vector>, 
		   next-buttons,
		   vector(/* make(<row-layout>, children: stack-navigation-buttons, spacing: 0), */
			  make(<row-layout>, children: step-buttons, spacing: 0),
			  make(<row-layout>, children: module-buttons, spacing: 0)));
  end;
end method;


/// EDIT AND FIND PROTOCOLS
///
/// Mostly trampolined depending on which debugger pane has the focus.

// This contains a vector of pairs for for "interesting" debugger panes.
// The head is the pane accessor and the tail is a function to apply to
// the pane to get the actual pane to copy/paste etc. on.
define constant $debugger-focus-panes :: <vector>
  = vector(compose(%context-pane,                 debugger-context-pane),
	   compose(displayer-collection-gadget, %displayer, debugger-stack-pane),
	   compose(%source-gadget,                debugger-source-pane),
	   compose(interactor-pane-default-focus, debugger-interactor1-pane),
	   compose(interactor-pane-default-focus, debugger-interactor2-pane));

define sealed method debugger-sheet-with-focus
    (frame :: <debugger>)
 => (sheet :: false-or(<sheet>))
  when (frame-mapped?(frame))
    // Get sheet with focus and check it's one we care about in this frame.
    let _port = port(frame);
    let sheet = frame-input-focus(frame);
    // debug-out(#"environment-debugger", "Frame input focus: %=", sheet);
    sheet := sheet
	     & sheet-mapped?(sheet) // & (debug-out(#"environment-debugger", "Mapped") | #t)
	     & (sheet-frame(sheet) = frame)
             //& (debug-out(#"environment-debugger", "Frame OK") | #t)
	     & any?(method (pane-getter)
		      /* debug-out(#"environment-debugger",
                                "Checking %=: %=",
                                pane-getter, pane-getter(frame)); */
		      when (sheet = pane-getter(frame))
		        // debug-out(#"environment-debugger", "Matched");
			sheet
		      end;
		    end,
		    $debugger-focus-panes);
    // debug-out(#"environment-debugger", "debugger-sheet-with-focus: %=", sheet);
    sheet
  end;
end method;

define macro delegate-to-debugger-sheet-with-focus
 { delegate-to-debugger-sheet-with-focus ?commands end }
    => { ?commands }
commands:
  { } => { }
  { ?env-command:name () => ( ?values:* ); ... }
    => { define sealed method ?env-command (frame :: <debugger>)
          => ( ?values )
           let sheet-with-focus = debugger-sheet-with-focus(frame);
	   sheet-with-focus
	     & "debugger-sheet-" ## ?env-command (sheet-with-focus)
	 end method ?env-command;
         ... }
  { ?env-command:name ( ?keys:* ) => ( ?values:* ); ... }
    => { define sealed method ?env-command
	     (frame :: <environment-editor>, #key ?keys)
          => ( ?values )
           let sheet-with-focus = debugger-sheet-with-focus(frame);
	   sheet-with-focus
	     & "debugger-sheet-" ## ?env-command (sheet-with-focus, ?keys)
	 end method ?env-command;
         ... }
end macro delegate-to-debugger-sheet-with-focus;


delegate-to-debugger-sheet-with-focus
  // ---*** hughg, 1997/11/11: Searching is NYI in the debugger for beta 1.0
  frame-can-find? () => (can? :: <boolean>);
  frame-can-find-again? () => (can? :: <boolean>);
  frame-can-replace? () => (can? :: <boolean>);
  frame-find () => ();
  frame-find-next () => ();
  frame-find-previous () => ();
  frame-replace () => ();
  // Don't need 'note-frame-selection-updated'.
  // These two idiosyncratically allow for multi-pane frames, so
  // I don't have to delegate them:
  // 'frame-selection'
  // 'frame-selected-text'
  frame-selection-empty? () => (empty? :: <boolean>);
  // Use default method (returning #f) for 'frame-primary-collection-gadget'.
  // ---*** Or, could return stack pane when it has the focus, #f otherwise.
end delegate-to-debugger-sheet-with-focus;

// --- Now, back to what we have to implement.

// --- searching.dylan --------------------------------------------

// ---*** hughg, 1997/11/11: The next 9 ("find" stuff) are NYI for beta 1.0
define sealed method debugger-sheet-frame-can-find?
    (sheet :: <sheet>)
 => (can-find? :: <boolean>)
  ignore(sheet);
  #f
end method;

define sealed method debugger-sheet-frame-can-find-again?
    (sheet :: <sheet>)
 => (can-find-again? :: <boolean>)
  ignore(sheet);
  #f
end method;

define sealed method debugger-sheet-frame-can-replace?
    (sheet :: <sheet>)
 => (can-replace? :: <boolean>)
  ignore(sheet);
  #f
end method;

define sealed method debugger-sheet-frame-find
   (sheet :: <sheet>, #key string, backwards?) => ()
  not-yet-implemented(owner: sheet-frame(sheet))
end method;

define sealed method debugger-sheet-frame-find-next (sheet :: <sheet>) => ()
  not-yet-implemented(owner: sheet-frame(sheet))
end method;

define sealed method debugger-sheet-frame-find-previous (sheet :: <sheet>) => ()
  not-yet-implemented(owner: sheet-frame(sheet))
end method;

define sealed method debugger-sheet-frame-replace (sheet :: <sheet>) => ()
  not-yet-implemented(owner: sheet-frame(sheet))
end method;

// --- selection.dylan --------------------------------------------

// Covers context and stack panes.
define sealed method debugger-sheet-frame-selection-empty?
    (sheet :: type-union(<text-gadget>, <tree-control>))
 => (empty? :: <boolean>)
  // We duplicate this from framework/selection.dylan only because
  // we also need the cheaper case for the <deuce-gadget>s.
  let frame = sheet-frame(sheet);
  // Need this test as this predicate can be called during initialzation,
  // when the sheet has no sheet-frame.
  when (frame)
    let selection = frame-selection(frame);
    select (selection by instance?)
      <collection> => empty?(selection);
      otherwise    => ~selection;
    end
  end
end method;

// Covers interactor and message panes.
define sealed method debugger-sheet-frame-selection-empty?
    (sheet :: <deuce-gadget>)
 => (empty? :: <boolean>)
  ~(window-mark(sheet))
end method;


define method frame-sheet-with-selection
    (frame :: <debugger>)
 => (sheet :: false-or(<sheet>))
  debugger-sheet-with-focus(frame)
end method;


// Covers context, interactor and message panes.
// ---*** Maybe do something cleverer for objects in interactor?
define sealed method frame-sheet-selection
    (frame :: <debugger>, sheet :: <text-gadget>)
 => (selection)
  // The default method returns the gadget-value, but we want to return
  // only the selected portion.
  frame-sheet-selected-text(frame, sheet)
end method;

// Covers just stack pane.
define sealed method frame-sheet-selection
    (frame :: <debugger>, sheet :: <tree-control>)
 => (selection)
  let value = gadget-value(sheet);
  value
end method;


/// RUN COMMANDS
///
/// Specialised methods on commands from the *run-command-table*.

define method frame-pause-application
    (frame :: <debugger>, 
     #key thread,
          startup-option) => ()
  next-method(frame, 
              thread: thread | frame.debugger-thread,
              startup-option: startup-option)
end method frame-pause-application;

