Module:    environment-deuce
Synopsis:  Environment Deuce
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Deuce-based command-line window

define class <command-line-pane> (<environment-shell-window>)
  slot command-line-server :: false-or(<command-line-server>);
end class <command-line-pane>;

define method do-destroy-sheet
    (pane :: <command-line-pane>) => ()
  next-method();
  let buffer = window-buffer(pane);
  when (instance?(buffer, <basic-shell-buffer>))
    // Lose any Dylan objects we might be hanging on to...
    do-lines(method (line, si, ei, last?)
	       ignore(si, ei, last?);
	       remove-property!(line-properties(line), #"object");
	     end method, buffer)
  end
end method do-destroy-sheet;

define function make-command-line-pane
    (#rest initargs,
     #key class = <command-line-pane>,
          project, frame, remote-thread, buffer, #all-keys)
 => (window :: <command-line-pane>)
  ignore(frame, remote-thread);
  let window = apply(make, class, initargs);
  dynamic-bind (*editor-frame* = window)
    let buffer = buffer | make-command-line-pane-buffer();
    let stream
      = make(<interval-stream>,
	     interval: buffer,
	     direction: #"output");
    let server
      = make-environment-command-line-server
          (input-stream: stream,	// ignored, so this is safe!
	   output-stream: stream);
    window.command-line-server := server;
    dynamic-bind (*buffer* = buffer)
      select-buffer(window, buffer)
    end
  end;
  window
end function make-command-line-pane;

define method make-command-line-pane-buffer
    (#key editor = $environment-editor) => (buffer :: <buffer>)
  make-dylan-shell(anonymous?: #t,
		   section-class: <dylanworks-shell-section>,
		   major-mode: find-mode(<dylanworks-shell-mode>),
		   editor: editor)
end method make-command-line-pane-buffer;

define method shell-parse-input
    (pane :: <command-line-pane>, text :: <string>)
 => (complete? :: <boolean>, message :: false-or(<string>))
  let server = pane.command-line-server;
  block ()
    let command = parse-command-line(server, text);
    //---*** It's a pity we lose the command object here...
    command-complete?(server.server-context, command)
  exception (<parse-error>)
    #t
  end
end method shell-parse-input;

define method shell-execute-code
    (pane :: <command-line-pane>, command-line :: <string>, bp :: <basic-bp>) => ()
  let server = pane.command-line-server;
  let stream = server.server-output-stream;
  let buffer = pane.window-buffer;
  stream-position(stream) := buffer.interval-end-bp;
  block ()
    let exit? = execute-command-line(server, command-line);
    exit? & exit-frame(sheet-frame(pane))
  exception (<abort>)
    #f
  end
end method shell-execute-code;


/// Command line tool

define frame <command-line-window>
    (<basic-editor-frame>,
     <frame-undo-mixin>,
     <frame-refresh-mixin>,
     <frame-cascading-window-mixin>,
     <basic-environment-tool>)
  pane %window (frame)
    make-command-line-pane
      (frame: frame,
       lines:   30,
       columns: 80);
  layout (frame)
    scrolling (scroll-bars: #"both")
      frame.%window
    end;
  tool-bar (frame)
    make-environment-tool-bar(frame);
  status-bar (frame)
    make-environment-status-bar(frame);
  command-table (frame)
    *command-line-window-command-table*;
  keyword frame-class-name:, init-value: #"command-line-window";
  keyword editor: = $environment-editor;
  keyword icon: = $interact-bitmap;	//---*** We need a real icon!
end frame <command-line-window>;

define cascading-window-settings 
  command-line-window :: <command-line-window> = "Command Line Window";

define sideways method find-command-line-window
    (frame :: <environment-frame>) => ()
  ensure-environment-frame(frame, <command-line-window>)
end method find-command-line-window;

define method generate-frame-title
    (frame :: <command-line-window>) => (title :: <string>)
  concatenate("Command Line", " - ", release-product-name())
end method generate-frame-title;

define command-table *command-line-window-file-command-table* (*global-command-table*)
  //---*** andrewa: remove printing options for 1.0
  // include *print-command-table*;
  include *recent-projects-command-table*;
  include *recent-files-command-table*;
  menu-item "Close"       = frame-close-file,
    accelerator:   make-keyboard-gesture(#"f4", #"alt"),
    documentation: "Closes the window.";
end command-table *command-line-window-file-command-table*;

define command-table *command-line-window-command-table* (*global-command-table*)
  menu-item "File"    = *command-line-window-file-command-table*;
  menu-item "Edit"    = *edit-command-table*;
  menu-item "View"    = *view-command-table*;
  menu-item "Window"  = *windows-command-table*;
  menu-item "Help"    = *environment-help-command-table*;
end command-table *command-line-window-command-table*;

define method make-environment-tool-bar-buttons
    (frame :: <command-line-window>)
 => (buttons :: <sequence>)
  with-frame-manager (frame-manager(frame))
    let buttons :: <stretchy-object-vector> = make(<stretchy-object-vector>);
    local method add-buttons
	      (make-buttons-function :: <function>)
	   => ()
	    add!(buttons,
		 make(<row-layout>,
		      children: make-buttons-function(frame),
		      spacing: 0))
	  end method add-buttons;
    add-buttons(make-clipboard-tool-bar-buttons);
    add-buttons(make-undo-tool-bar-buttons);
    add-buttons(make-search-tool-bar-buttons);
    buttons
  end
end method make-environment-tool-bar-buttons;
