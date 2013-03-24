Module:    environment-deuce
Synopsis:  Environment Deuce
Author:    Hugh Greene
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Auxiliary commands

define function make-deuce-command
    (function :: <function>) => (command :: <function>)
  let command = method (frame)
		  // Prefer the Deuce frame to the DUIM frame...
		  let frame = find-relevant-editor-frame(frame);
		  when (frame)
		    deuce/execute-command-in-frame(frame, function)
		  end
		end method;
  command
end function make-deuce-command;

define macro make-named-deuce-commands
  { make-named-deuce-commands ?commands end }
    => { ?commands }
commands:
  { } => { }
  { ?:name; ... }
    => { define constant "frame-" ## ?name = make-deuce-command(?name);
         ... }
end macro make-named-deuce-commands;

// Other tools, like the Browser and Debugger, can return the Deuce pane
// that has the input focus
//---*** It sucks that we have to do things this way, but we really
//---*** need for the commands to have the sheet as well as the frame
define open generic find-relevant-editor-frame
    (frame :: <frame>) => (editor-frame :: false-or(<editor-frame>));

define method find-relevant-editor-frame
    (frame :: <frame>) => (editor-frame :: false-or(<editor-frame>))
  #f
end method find-relevant-editor-frame;

define method find-relevant-editor-frame
    (frame :: <environment-editor>) => (editor-frame :: false-or(<editor-frame>))
  frame
end method find-relevant-editor-frame;


define macro delegate-to-deuce
  { delegate-to-deuce ?commands end }
    => { ?commands }
commands:
  { } => { }
  { ?env-command:name => ?deuce-command:name; ... }
    => { define sealed method ?env-command (frame :: <environment-editor>) => ()
	   deuce/execute-command-in-frame(frame, ?deuce-command)
	 end method ?env-command;
         ... }
  { ?env-command:name ( ?ignored-keys:* ) => ?deuce-command:name; ... }
    => { define sealed method ?env-command
	     (frame :: <environment-editor>, #key ?ignored-keys) => ()
	   ignore(?ignored-keys);
	   deuce/execute-command-in-frame(frame, ?deuce-command)
	 end method ?env-command;
         ... }
end macro delegate-to-deuce;


/// From the "File" menu ...

// ... I/O commands

define method frame-open-file
    (frame :: <environment-editor>, #rest keys,
     #key filename, deuce-frame, #all-keys)
 => ()
  //---*** cpage: 1998.04.01 Note that this function really only takes #all-keys so that
  //              this method can pass on parameters to open-file via next-method. One
  //              example oddity here is that this function must accept deuce-frame: even
  //              though it's really a "private" piece of information being sent from
  //              this method to open-file.
  //              We really should just export open-file, or something like it, because
  //              this is the wrong abstraction level in which to be doing this.
  let buffer   = frame-buffer(frame);
  let default  = buffer & buffer-default-pathname(buffer);
  let type     = buffer & source-file-type(buffer-major-mode(buffer));
  // _This_ is a Deuce frame, so supply it as the 'deuce-frame:' keyword arg
  apply(next-method, frame,
	deuce-frame:  frame,
	default:      default & as(<string>, default),
	default-type: type,
	keys)
end method frame-open-file;

delegate-to-deuce
  frame-revert-file  (filename)	=> revert-file;
  frame-close-file   (filename)	=> close-file;
  frame-save-file    (filename)	=> save-file;
  frame-save-file-as (filename)	=> save-file-as;
  frame-save-all		=> save-all-files;
end;


// ... printing commands

//---*** ???


// ... exit-frame: see editor.dylan:handle-event(..., <frame-exit-event>)

//---*** Do we want some command that asks whether to kill all of
//---*** DW or just the editor?  Could also offer to save buffers before closing.

/// From the "Edit" menu ...

// ... undo commands

define method command-available-for-focus?
    (gadget :: <environment-deuce-pane>, command == <frame-undo-command>)
 => (available? :: <boolean>)
  let buffer = window-buffer(gadget);
  let history = buffer-undo-history(buffer);
  if (history)
    let (n-undo, n-redo) = undo-history-state(history);
    ~zero?(n-undo)
  end
end method command-available-for-focus?;

define method command-available-for-focus?
    (gadget :: <environment-deuce-pane>, command == <frame-redo-command>)
 => (available? :: <boolean>)
  let buffer = window-buffer(gadget);
  let history = buffer-undo-history(buffer);
  if (history)
    let (n-undo, n-redo) = undo-history-state(history);
    ~zero?(n-redo)
  end
end method command-available-for-focus?;

define method execute-command-for-focus
    (gadget :: <environment-deuce-pane>, command :: <frame-undo-command>)
 => ()
  deuce/execute-command-in-frame(window-frame(gadget), deuce/undo-command)
end method execute-command-for-focus;

define method execute-command-for-focus
    (gadget :: <environment-deuce-pane>, command :: <frame-redo-command>)
 => ()
  deuce/execute-command-in-frame(window-frame(gadget), deuce/redo-command)
end method execute-command-for-focus;


// ... editing commands


// ... clipboard commands

define method frame-selection
    (frame :: <environment-editor>) => (selection :: false-or(<string>))
  let pane = frame.%window;
  selected-text(pane)
  // Do we have to deal with the "copy-line" policy here?
end method frame-selection;

define method frame-selection-empty?
    (frame :: <environment-editor>) => (empty? :: <boolean>)
  let window = frame-window(frame);
  window & ~window-mark(window)
end method frame-selection-empty?;

delegate-to-deuce
  editor-frame-yank          => yank;
end;


// ... selection commands

/*---*** Hopefully not needed, since the Deuce gadget obeys 
//---*** the text gadget protocols
define method do-execute-command
    (frame :: <environment-editor>, command :: <frame-select-all>) => ()
  deuce/execute-command-in-frame(frame, mark-buffer)
end method do-execute-command;

define method do-execute-command
    (frame :: <environment-editor>, command :: <frame-deselect-all>) => ()
  let window = frame-window(frame);
  when (window)
    clear-mark!(window: window, redisplay?: #t)
  end
end method do-execute-command;

define method command-available-for-focus?
    (frame :: <environment-editor>, command == <frame-select-all>)
 => (available? :: <boolean>)
  frame-window(frame) & frame-buffer(frame) & #t
end method command-available-for-focus?;

define method command-available-for-focus?
    (frame :: <environment-editor>, command == <frame-deselect-all>)
 => (available? :: <boolean>)
  let window = frame-window(frame);
  (window & frame-buffer(frame) & window-mark(window)) & #t
end method command-available-for-focus?;
*/

// ... searching commands

define method find-string (frame :: <environment-editor>) => ()
  frame-edit-search-options(frame)
end method find-string;

// We can get here via 'incremental-search-forward', so be careful
// to pop up a search dialog if there's already a search string
define method find-next-string (frame :: <environment-editor>) => ()
  if (frame-can-find?(frame))
    frame-find-next(frame)
  else
    frame-edit-search-options(frame)
  end
end method find-next-string;

// Same deal w.r.t. 'incremental-search-backward'
define method find-previous-string (frame :: <environment-editor>) => ()
  if (frame-can-find?(frame))
    frame-find-previous(frame)
  else
    frame-edit-search-options(frame)
  end
end method find-previous-string;

define method replace-string (frame :: <environment-editor>) => ()
  frame-edit-search-options(frame)
end method replace-string;

define method query-replace-string (frame :: <environment-editor>) => ()
  frame-edit-search-options(frame)
end method query-replace-string;

delegate-to-deuce
  editor-frame-goto => goto-line;
end;


/// ... searching commands for editor gadgets

//---*** cpage: 1998.08.29 Note that we assume the frame command functions will
//             end up searching the given gadget. We probably need to change
//             this so there are command functions for sheets, with common
//             implementation functions that take a frame and a sheet.

define method find-string (gadget :: <environment-deuce-gadget>) => ()
  frame-edit-search-options(sheet-frame(gadget))
end method find-string;

define method find-next-string (gadget :: <environment-deuce-gadget>) => ()
  frame-find-next(sheet-frame(gadget))
end method find-next-string;

define method find-previous-string (gadget :: <environment-deuce-gadget>) => ()
  frame-find-previous(sheet-frame(gadget))
end method find-previous-string;

define method replace-string (gadget :: <environment-deuce-gadget>) => ()
  frame-edit-search-options(sheet-frame(gadget))
end method replace-string;

define method query-replace-string (gadget :: <environment-deuce-gadget>) => ()
  frame-edit-search-options(sheet-frame(gadget))
end method query-replace-string;


/// From the "View" menu ...

// ... refresh and options

define method refresh-frame (frame :: <environment-editor>) => ()
  //---*** Will this potentially do too much redisplay?
  next-method();
  deuce/execute-command-in-frame(frame, force-redisplay)
end method refresh-frame;

define sealed method editor-frame-show-cursor-position
    (frame :: <environment-editor>) => ()
  deuce/execute-command-in-frame(frame, deuce/show-position)
end method editor-frame-show-cursor-position;

delegate-to-deuce
  frame-edit-options => choose-configuration;
end;


/// From the "Project" and "Application" menus ...

/*---*** andrewa: let's try not delegating these at all...
define sealed method do-delegate-to-project-browser
    (frame :: <environment-editor>, command :: <function>) => ()
  block ()
    let current-project = editor-frame-current-project(frame);
    if (current-project)
      with-environment-frame (project-browser = frame, <project-browser>,
			      project: current-project)
	command(project-browser);
      end;
    else
      command-error("The document in this window is not in the active project.");
    end;
  exception (not-found :: type-union(<file-project-not-found-warning>,
				     <file-library-not-found-warning>))
    command-error(condition-to-string(not-found))
  end;
end method do-delegate-to-project-browser;

define macro delegate-to-project-browser
  { delegate-to-project-browser ?commands end }
    => { ?commands }
commands:
  { } => { }
  { ?command:name; ... }
    => { define sealed method ?command (frame :: <environment-editor>) => ()
	   do-delegate-to-project-browser(frame, ?command)
	 end method ?command;
	 ... }
  { ?command:name ( ?ignored-keys:* ) ; ... }
    => { define sealed method ?command
	     (frame :: <environment-editor>, #key ?ignored-keys) => ()
	   ignore(?ignored-keys);
	   do-delegate-to-project-browser(frame, ?command)
	 end method ?command;
	 ... }
end macro delegate-to-project-browser;

//---*** These should probably all become delegated to deuce.
delegate-to-project-browser
  // Project Menu
  frame-advanced-build-dialog;		// Advanced Build...
  frame-edit-project-settings;		// Settings...
  // Go Menu
  frame-browse-threads;			// Threads
  // Application Menu
  frame-start-application;		// Start
  frame-debug-application;		// Debug
  frame-interact;			// Interact
  frame-pause-application
    (thread, startup-option);		// Pause
  frame-resume-application;		// Resume
  frame-stop-application;		// Stop
  frame-restart-application;		// Restart
  frame-start-or-resume-application;	// [Run] toolbar button
end;
*/

/*---*** andrewa: Let's not delegate these, since we can do a better job
  ---*** sharing the environment's code.
delegate-to-deuce
  // Project Menu
  frame-build-project       (process-subprojects?) => deuce/build-project;
  frame-clean-build-project (process-subprojects?) => deuce/clean-build-project;
end;
*/


/// From the "Object" menu ...

delegate-to-deuce
  frame-describe-primary-object                => deuce/describe-object;
  frame-document-primary-object                => deuce/show-documentation;
  frame-edit-primary-object                    => deuce/edit-definition;
  frame-browse-primary-object                  => deuce/browse-object;
  frame-browse-primary-object-class            => deuce/browse-class;
  frame-browse-primary-object-generic-function => deuce/browse-function;
  //---*** Ditch or disable the next one:
  frame-display-primary-object-properties      => deuce/describe-object;
end;



/// From the "Tools" menu ...

// ... tool cloning

define method make-clone
    (frame :: <environment-editor>, #rest initargs)
 => (frame :: <environment-editor>)
  apply(next-method, frame,
	// editor: $environment-editor,
	buffer: frame-buffer(frame),
	initargs);
end method make-clone;


/// From the "History" menu ...


/// From the "Help" menu ...


/// Glue between Deuce and the Source Control Manager

define sealed method do-source-control-operation
    (window :: <environment-deuce-pane>, operation :: <source-control-operation>,
     #key pathname :: false-or(<pathname>),
	  reason?  :: <boolean>)
 => (success? :: <boolean>, pathname :: false-or(<pathname>), message :: false-or(<string>))
  ignore(reason?);
  let sccs = current-source-control-system();
  if (sccs)
    block ()
      let buffer = window-buffer(window);
      let class
	= select (operation)
	    #"claim"     => <sccs-claim-command>;
	    #"check-out" => <sccs-check-out-command>;
	    #"check-in"  => <sccs-check-in-command>;
	    #"abandon"   => <sccs-abandon-command>;
	    #"merge"     => <sccs-merge-command>;
	    #"diff"      => <sccs-diff-command>;
	    #"report"    => <sccs-report-command>;
	    #"add"       => <sccs-add-command>;
	    #"remove"    => <sccs-remove-command>;
	  end;
      let (logged-in?, login-failure-message)
	= source-control-maybe-login(sccs, class, owner: window);
      if (logged-in?)
	let filename = pathname & as(<file-locator>, pathname);
	let previous-options
	  = get-property(buffer-properties(buffer), #"source-control-options");
	let info
	  = source-control-command-info
	      (sccs, class, pathname: filename, defaults: previous-options);
	let options
	  = get-source-control-arguments(sccs, info, owner: window, pathname: filename);
	if (options)
	  put-property!(buffer-properties(buffer), #"source-control-options", options);
	  execute-command(make(class, options: options))
	else
	  values(#f, #f, #f)
	end
      else
	values(#f, #f, login-failure-message)
      end
    exception (condition :: <source-control-condition>)
      values(#f, #f, condition-to-string(condition))
    end
  else
    values(#f, #f, "Source code control is not available")
  end
end method do-source-control-operation;

define method source-control-maybe-login
    (sccs :: <source-control-system>, 
     class :: subclass(<source-code-control-command>),
     #key owner :: <environment-deuce-pane>)
 => (logged-in? :: <boolean>, message :: false-or(<string>))
  let login-info = source-control-login-info(sccs, class);
  if (login-info)
    let options
      = get-source-control-arguments(sccs, login-info, owner: owner);
    if (options)
      source-control-login(sccs, options)
    else
      values(#f, "Login cancelled")
    end
  else
    values(#t, #f)
  end
end method source-control-maybe-login;

define method get-source-control-arguments
    (sccs :: <source-control-system>, info :: <source-control-info>,
     #key owner :: <environment-deuce-pane>,
          pathname :: false-or(<file-locator>) = #f)
 => (command-options :: false-or(<source-control-options>))
  let frame   = sheet-frame(top-level-sheet(owner));
  let framem  = frame-manager(frame);
  let class   = info.command-class;
  let title   = info.command-title;
  let options = info.command-options;
  with-frame-manager (framem)
    let min-width = 250;
    let contents :: <stretchy-object-vector> = make(<stretchy-object-vector>);
    local method dialog-complete?
	      (dialog :: <dialog-frame>) => (complete? :: <boolean>)
	    ignore(dialog);
	    let complete? :: <boolean> = #t;
	    for (gadgets in contents)
	      let gadget :: <gadget> = gadgets[1];
	      let option :: <source-control-option> = gadget.gadget-id;
	      if (option-required?(option) & empty?(gadget-value(gadget)))
		complete? := #f
	      end
	    end;
	    complete?
	  end method dialog-complete?;
    for (option :: <source-control-option> in options)
      add!(contents,
	   vector(make(<label>,
		       label: option.option-label),
		  begin
		    let type = option.option-type;
		    select (type)
		      <string>, #"password" =>
			make(if (type == #"password")
			       <password-field>
			     else
			       <text-field>
			     end,
			     min-width: min-width,
			     id: option,
			     documentation: option.option-documentation,
			     value: option.option-default,
			     value-changing-callback:
			       method (gadget)
				 let dialog = sheet-frame(gadget);
				 dialog-exit-enabled?(dialog)
				   := dialog-complete?(dialog)
			       end);
		    end
		  end))
    end;
    let layout
      = make(<table-layout>,
	     x-spacing: 8, y-spacing: 2,
	     x-alignment: #[#"right", #"left"],
	     contents: contents);
    let dialog
      = make(<dialog-frame>,
	     title:  title,
	     layout: layout,
	     mode:   #"modal",
	     owner:  frame);
    if (start-dialog(dialog))
      let keys :: <stretchy-object-vector> = make(<stretchy-object-vector>);
      for (gadgets in contents)
	let gadget :: <gadget> = gadgets[1];
	let keyword = gadget.gadget-id.option-keyword;
	add!(keys, keyword);
	add!(keys, gadget-value(gadget))
      end;
      apply(make, class, pathname: pathname, keys)
    end
  end
end method get-source-control-arguments;


/// Editor command tables


/// File menu

define command-table *editor-file-open-command-table* (*global-command-table*)
  menu-item "New..."         = frame-new-file,
    accelerator:   make-keyboard-gesture(#"n", #"control"),
    documentation: "Creates a new document.";
  menu-item "Open..."        = frame-open-file,
    accelerator:   make-keyboard-gesture(#"o", #"control"),
    documentation: "Opens an existing document.";
  menu-item "Revert"         = frame-revert-file,
    documentation: "Discards unsaved changes to the document.";
end command-table *editor-file-open-command-table*;

define constant editor-frame-switch-buffers = make-deuce-command(switch-buffers);

define command-table *editor-file-io-command-table* (*global-command-table*)
  include *editor-file-open-command-table*;
  include *file-save-command-table*;
  include *export-command-table*;
  // include *print-command-table*;
  //--- hughg, 1997/01/23: Should we alter Env-Framework/Tools to
  //--- provide "Files..." for the Project window as well?
  menu-item "Files..." = editor-files-dialog,
    accelerator:   make-keyboard-gesture(#"f10", #"control"),
    documentation: "Selects an already open document to edit.";
  menu-item "Other File" = editor-frame-switch-buffers,
    accelerator:   make-keyboard-gesture(#"f10", #"alt"),
    documentation: "Selects the open document most recently edited in this window.";
  separator;
  include *recent-projects-command-table*;
  separator;
  include *recent-files-command-table*;
  separator;
  menu-item "Close File"     = frame-close-file,
    accelerator:   make-keyboard-gesture(#"f4", #"control"),
    documentation: "Closes the document.";
  menu-item "Close Window"   = exit-frame,
    accelerator:   make-keyboard-gesture(#"f4", #"alt"),
    documentation: "Closes the window.";
end command-table *editor-file-io-command-table*;

define sealed method editor-files-dialog (frame :: <environment-editor>)
  let window = frame-window(frame);
  let buffer = choose-buffer-dialog(window);
  when (buffer)
    find-deuce-frame(buffer: buffer, deuce-frame: frame)
  end
end method editor-files-dialog;


/// Edit menu

define command-table *editor-edit-command-table* (*global-command-table*)
  //--- Instead the next four, could we just use *edit-command-table*?
  include *undo-command-table*;
  include *clipboard-command-table*;
  include *selection-command-table*;
  include *searching-command-table*;
  menu-item "Go To..." = editor-frame-goto,
    accelerator:   make-keyboard-gesture(#"g", #"control"),
    documentation: "Goes to an exact position within the current file.";
  //--- Maybe add this, but need to update its state appropriately
  // [check-button] menu-item "Read-only?" = ...
  separator;
  menu-item "Capitalize Word" = make-deuce-command(capitalize-word),
    accelerator:   make-keyboard-gesture(#"k", #"control"),
    documentation: "Capitalizes the word after the insertion point.";
  menu-item "Lowercase Selection" = make-deuce-command(downcase-region),
    accelerator:   make-keyboard-gesture(#"l", #"control"),
    documentation: "Converts the selected text to lower case.";
  menu-item "Uppercase Selection" = make-deuce-command(upcase-region),
    accelerator:   make-keyboard-gesture(#"u", #"control"),
    documentation: "Converts the selected text to upper case.";
end command-table *editor-edit-command-table*;


/// View Menu

define command-table *editor-coloring-command-table* (*global-command-table*)
end command-table *editor-coloring-command-table*;

add-command-table-menu-item
  (*editor-coloring-command-table*,
   "", <check-box>, #[],
   items: #[#["Color &Dispatch Optimizations", #"dispatch-coloring"]],
   label-key: first, value-key: second,
   documentation:
     "Colors the buffer contents based on function dispatch optimizations.",
   callback: color-dispatch-optimizations-callback);

define function color-dispatch-optimizations-callback
    (box :: <value-gadget>) => ()
  let frame    = sheet-frame(box);
  let function = if (member?(#"dispatch-coloring", gadget-value(box)))
		   color-dispatch-optimizations
		 else
		   reset-optimization-colors
		 end;
  deuce/execute-command-in-frame(frame, function)
end function color-dispatch-optimizations-callback;

define method update-optimization-commands
    (window :: <environment-deuce-pane>, buffer :: <buffer>) => ()
  // Don't need to do anything if we're not within a Deuce editor
  #f
end method update-optimization-commands;

define method update-optimization-commands
    (window :: <environment-editor-pane>, buffer :: <buffer>) => ()
  let frame  = sheet-frame(window);
  let color? = get-property(buffer-contents-properties(buffer), #"optimization-colors");
  local method update-command (gadget)
	  if (color?)
	    gadget-value(gadget) := add-new(gadget-value(gadget), #"dispatch-coloring")
	  else
	    gadget-value(gadget) := remove(gadget-value(gadget), #"dispatch-coloring")
	  end
	end method;
  do-command-menu-gadgets(update-command, frame, color-dispatch-optimizations-callback)
end method update-optimization-commands;

define command-table *editor-basic-view-command-table* (*global-command-table*)
  include *view-refresh-command-table*;
  menu-item "Recenter" = make-deuce-command(force-recenter),
    accelerator:   make-keyboard-gesture(#"f5", #"control"),
    documentation: "Refresh the window and move the line with the cursor to the center.";
  separator;
  menu-item "Editor Options..." = frame-edit-options,
    documentation: "Enables you to change application options.";
end command-table *editor-basic-view-command-table*;

define command-table *editor-view-command-table* (*global-command-table*)
  include *bar-options-command-table*;
  menu-item "Cursor Location" = editor-frame-show-cursor-position,
    accelerator:   make-keyboard-gesture(#"=", #"control"),
    documentation: "Shows information about the cursor location in the status bar.";
  include *editor-coloring-command-table*;
  include *editor-basic-view-command-table*;
end command-table *editor-view-command-table*;

define command-table *editor-go-command-table* (*global-command-table*)
  include *browse-locations-command-table*;
  separator;
  menu-item "Edit Compiler Warnings" = make-deuce-command(edit-compiler-warnings),
    documentation: "Start editing the compiler warnings for this project.";
end command-table *editor-go-command-table*;


/// Object Menu

define command-table *editor-primary-object-edit-command-table* (*global-command-table*)
  menu-item "Edit Source"           = frame-edit-primary-object,
    accelerator:   make-keyboard-gesture(#"f2", #"shift"),
    documentation: "Edit this definition in its home buffer.";
  menu-item "Edit Subclasses"       = frame-edit-primary-object-subclasses,
    documentation: "Edit a document containing the subclasses of the selected class.";
  menu-item "Edit Superclasses"     = frame-edit-primary-object-superclasses,
    documentation: "Edit a document containing the superclasses of the selected class.";
  menu-item "Edit Class Methods"    = frame-edit-primary-object-class-methods,
    documentation: "Edit a document containing the methods of the selected class.";
  menu-item "Edit Generic Methods"  = frame-edit-primary-object-generic-methods,
    documentation: "Edit a document containing the methods of the selected generic function.";
  menu-item "Edit Clients"          = frame-edit-primary-object-clients,
    documentation: "Edit a document containing the users of the selected definition.";
  menu-item "Edit Used Definitions" = frame-edit-primary-object-used-definitions,
    documentation: "Edit a document containing the definitions used by the selected definition.";
end command-table *editor-primary-object-edit-command-table*;

define command-table *editor-primary-object-command-table* (*global-command-table*)
  include *primary-object-browse-command-table*;
  include *primary-object-documentation-command-table*;
  include *editor-primary-object-edit-command-table*;
  include *primary-object-properties-command-table*;
end command-table *editor-primary-object-command-table*;


/// Project menu

define command-table *editor-project-command-table* (*global-command-table*)
  menu-item "Compile Selection" = make-deuce-command(evaluate-region),
    accelerator:   make-keyboard-gesture(#"f7", #"control", #"shift"),
    documentation: "Compile and download the selection into the running application.";
  menu-item "Macroexpand Selection" = make-deuce-command(macroexpand-region),
    documentation: "Insert the macroxpanded selection into the buffer.";
  include *project-command-table*;
end command-table *editor-project-command-table*;

define command-table *editor-buffer-popup-command-table* (*global-command-table*)
  include *build-command-table*;
  separator;
  menu-item "Compile Selection" = make-deuce-command(evaluate-region),
    documentation: "Interactively compile the selection or the current definition.";
  menu-item "Macroexpand Selection" = make-deuce-command(macroexpand-region),
    documentation: "Macroexpand the selection or the current definition.";
  separator;
  include *editor-coloring-command-table*;
  separator;
  include *clipboard-command-table*;
end command-table *editor-buffer-popup-command-table*;

define method command-table-for-target
    (frame :: <environment-editor>, target :: <buffer-command-target>)
 => (comtab :: <command-table>)
  let buffer = target-buffer(target);
  if (instance?(buffer-major-mode(buffer), <dylanworks-mode>))
    *editor-buffer-popup-command-table*
  else
    next-method()
  end
end method command-table-for-target;


/// Menu for Dylan definition lines

define command-table *editor-section-line-popup-command-table* (*global-command-table*)
  menu-item "Edit Source" = make-deuce-command(edit-home-definition-from-target),
    documentation: "Edit this definition in its home buffer.";
  separator;
  include *editor-buffer-popup-command-table*;
end command-table *editor-section-line-popup-command-table*;

define method command-table-for-target
    (frame :: <environment-editor>, target :: <section-command-target>)
 => (comtab :: <command-table>)
  let buffer = target-buffer(target);
  if (instance?(buffer-major-mode(buffer), <dylan-mode>))
    *editor-section-line-popup-command-table*
  else
    next-method()
  end
end method command-table-for-target;

define method edit-home-definition-from-target
    (frame :: <environment-editor>) => ()
  let target :: <section-command-target> = frame-command-target(frame);
  let section = target-section(target);
  edit-home-definition(frame, section: section)
end method edit-home-definition-from-target;


/// Application menu

define method frame-create-or-toggle-breakpoint
    (frame :: <environment-editor>) => ()
  set-editor-breakpoint-popup-target(frame);
  when (frame-command-target(frame))
    next-method()
  end
end method frame-create-or-toggle-breakpoint;

define method frame-clear-breakpoint
    (frame :: <environment-editor>) => ()
  set-editor-breakpoint-popup-target(frame);
  when (frame-command-target(frame))
    next-method()
  end
end method frame-clear-breakpoint;

define method frame-edit-breakpoint-options
    (frame :: <environment-editor>) => ()
  set-editor-breakpoint-popup-target(frame);
  when (frame-command-target(frame))
    next-method()
  end
end method frame-edit-breakpoint-options;

define method frame-run-to-cursor
    (frame :: <environment-editor>, target :: <deuce-command-target>) => ()
  //--- We should really do something with the target...
  ignore(target);
  set-editor-breakpoint-popup-target(frame);
  when (frame-command-target(frame))
    next-method()
  end
end method frame-run-to-cursor;

define method set-editor-breakpoint-popup-target
    (frame :: <environment-editor>) => ()
  let window = frame-window(frame);
  with-editor-state-bound (buffer = window)
    let line = if (instance?(primary-object-interval(window), <interval>))
		 // We're here via a pop-up menu...
		 bp-line(interval-start-bp(primary-object-interval(window)))
	       else
		 // We're here via a pull-down menu...
		 bp-line(window-point(window))
	       end;
    let target
      = get-property(line-properties(line), #"breakpoint-object")
	| line-source-location(buffer-major-mode(buffer), line, shadow?: #t);
    when (target)
      frame-command-target(frame) := make-command-target(window, target)
    end
  end
end method set-editor-breakpoint-popup-target;

/*---*** hughg, 1998/03/31: OR possibly this version...
define method set-editor-breakpoint-popup-target
    (frame :: <environment-editor>) => ()
  unless (frame-command-target(frame))
    let window = frame-window(frame);
    with-editor-state-bound (buffer = window)
      let line = bp-line(window-point(window));
      let target
        = get-property(line-properties(line), #"breakpoint-object")
	  | line-source-location(buffer-major-mode(buffer), line, shadow?: #t);
      when (target)
	frame-command-target(frame) := make-command-target(window, target)
      end
    end
  end
end method set-editor-breakpoint-popup-target;
*/

define command-table *editor-breakpoint-command-table* (*global-command-table*)
  include *all-breakpoints-command-table*;
  separator;
  menu-item "Set or Enable/Disable Breakpoint" = frame-create-or-toggle-breakpoint,
    accelerator: make-keyboard-gesture(#"f9"),
    documentation: "Sets a line breakpoint or toggles an existing one.";
  menu-item "Clear Breakpoint"                 = frame-clear-breakpoint,
    documentation: "Clears a breakpoint.";
  menu-item "Edit Breakpoint Options..."       = frame-edit-breakpoint-options,
    accelerator: make-keyboard-gesture(#"f9", #"control", #"shift"),
    documentation: "Modifies a breakpoint's options.";
end command-table *editor-breakpoint-command-table*;

define command-table *editor-run-command-table* (*global-command-table*)
  include *basic-run-command-table*;
  separator;
  include *editor-breakpoint-command-table*;
end command-table *editor-run-command-table*;


/// Source control menu

make-named-deuce-commands
  vc-claim;
  vc-check-out;
  vc-check-in;
  vc-abandon;
  vc-merge;
  vc-diff;
  vc-report;
  vc-add;
  vc-remove;
end;

define command-table *editor-source-control-command-table* (*global-command-table*)
  menu-item "&Get Latest Version..." = frame-vc-check-out,
    documentation: "Gets the latest version of files from source code control";
  menu-item "Check &Out..." = frame-vc-claim,
    documentation: "Checks out the files from source code control";
  menu-item "Check &In..." = frame-vc-check-in,
    documentation: "Checks in the files to source code control";
  menu-item "&Undo Check Out..." = frame-vc-abandon,
    documentation: "Undoes the checkout of files, without retaining changes";
  separator;
  menu-item "&Merge with Source Control..." = frame-vc-merge,
    documentation: "Merges the files with the latest versions under source code control";
  separator;
  menu-item "&Add to Source Control..." = frame-vc-add,
    documentation: "Puts project files under source code control";
  menu-item "&Remove from Source Control..." = frame-vc-remove,
    documentation: "Removes project files from source code control";
  separator;
  menu-item "Show &History..." = frame-vc-report,
    documentation: "Shows the history of a file or files under source code control";
  menu-item "Show &Differences..." = frame-vc-diff,
    documentation: "Shows the changes made to a file since it was checked out";
end command-table *editor-source-control-command-table*;

define function disable-unimplemeted-sccs-commands
    (frame :: <environment-editor>, sccs :: <source-control-system>) => ()
  command-enabled?(frame-vc-claim, frame)
    := sccs-command-implemented?(sccs, <sccs-claim-command>);
  command-enabled?(frame-vc-check-out, frame)
    := sccs-command-implemented?(sccs, <sccs-check-out-command>);
  command-enabled?(frame-vc-check-in, frame)
    := sccs-command-implemented?(sccs, <sccs-check-in-command>);
  command-enabled?(frame-vc-abandon, frame)
    := sccs-command-implemented?(sccs, <sccs-abandon-command>);
  command-enabled?(frame-vc-merge, frame)
    := sccs-command-implemented?(sccs, <sccs-merge-command>);
  command-enabled?(frame-vc-diff, frame)
    := sccs-command-implemented?(sccs, <sccs-diff-command>);
  command-enabled?(frame-vc-report, frame)
    := sccs-command-implemented?(sccs, <sccs-report-command>);
  command-enabled?(frame-vc-add, frame)
    := sccs-command-implemented?(sccs, <sccs-add-command>);
  command-enabled?(frame-vc-remove, frame)
    := sccs-command-implemented?(sccs, <sccs-remove-command>);
end function disable-unimplemeted-sccs-commands;

/// Help menu

define command-table *editor-help-command-table* (*global-command-table*)
  //--- It would be nice if we could just 'include *help-command-table*;'
  //--- include *help-command-table*;
  menu-item "Contents and Index" = frame-help-contents-and-index,
    documentation: "Opens Help.";
  menu-item "Help on Selection" = make-deuce-command(deuce/show-documentation),
    accelerator: make-keyboard-gesture(#"f1"),
    documentation: "Lookup up help on selected text.";
  separator;
  menu-item "Key Bindings" = make-deuce-command(editor-key-bindings),
    documentation: "Show the current set of key bindings for the editor.";
  separator;
  include *environment-specific-help-command-table*;
end command-table *editor-help-command-table*;


/// Overall menu bar

define command-table *editor-command-table* (*global-command-table*)
  menu-item "File"        = *editor-file-io-command-table*;
  menu-item "Edit"        = *editor-edit-command-table*;
  menu-item "View"        = *editor-view-command-table*;
  menu-item "Go"          = *editor-go-command-table*;
  menu-item "Object"      = *editor-primary-object-command-table*;
  menu-item "Project"     = *editor-project-command-table*;
  menu-item "Application" = *editor-run-command-table*;
  menu-item "Window"      = *windows-command-table*;
  menu-item "Help"        = *editor-help-command-table*;
end command-table *editor-command-table*;


/// All the project specific command tables...

define constant $dylanworks-mode-command-tables :: <vector>
  = vector(*primary-object-browse-command-table*,
           *primary-object-documentation-command-table*,
           *editor-primary-object-edit-command-table*,
           *primary-object-properties-command-table*,
           *editor-primary-object-command-table*,
           *build-command-table*,
           *project-settings-command-table*,
           *editor-project-command-table*,
           *editor-run-command-table*,
           *basic-run-command-table*,
           *editor-breakpoint-command-table*,
           *all-breakpoints-command-table*);

/// Initially disabled commands

define constant $initially-disabled-commands
  = vector(// File Menu
	   frame-revert-file,
	   frame-close-file,
	   // Edit Menu
	   frame-find-next, frame-find-previous,
	   // Object Menu
	   frame-describe-primary-object,
	   frame-document-primary-object,
	   frame-browse-primary-object,
	   frame-browse-primary-object-class,
	   frame-browse-primary-object-generic-function,
	   frame-edit-primary-object,
	   frame-display-primary-object-properties,
	   // Project Menu
	   frame-parse-project,
	   frame-compile-project,
	   frame-clean-compile-project,
	   frame-link-project,
	   frame-build-project,
	   frame-clean-build-project,
	   frame-build-release,
	   frame-edit-project-settings,
	   // Go Menu
	   frame-browse-threads,
	   // Application Menu
	   frame-start-application,
	   frame-debug-application,
	   frame-interact,
	   frame-start-or-resume-application,
	   frame-pause-application,
	   frame-resume-application,
	   frame-stop-application,
	   frame-restart-application,
	   // Window Menu
	   clone-tool,
	   clone-and-link-tool);

/// Method on 'make' for <environment-editor>s.
// This needs to come after $initially-disabled-commands, and all the
// methods/functions that variable refers to.

define method make
    (class == <environment-editor>, #rest keys, #key disabled-commands)
 => (editor :: <environment-editor>)
  apply(next-method, class,
	disabled-commands: disabled-commands | $initially-disabled-commands,
	keys)
end method make;
