Module:    environment-deuce
Synopsis:  Environment Deuce
Author:    Scott McKay, Hugh Greene, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Environment editor class

define sealed class <deuce-editor> (<basic-editor>)
end class <deuce-editor>;

define method initialize (editor :: <deuce-editor>, #key)
  next-method();
  load-policy-settings(editor-policy(editor));
  install-command-set(editor, command-set-policy(editor-policy(editor)));
  tune-in($project-channel,
	  deuce-editor-project-message-receiver,
	  message-type: <project-message>);
end method initialize;

define function deuce-editor-project-message-receiver
    (message :: <project-message>) => ()
  let project :: false-or(<project-object>)
    = instance?(message, <project-object-message>) & message-project(message);
  select (message by instance?)
    <no-active-project-message>, <project-now-active-message> =>
      deuce-note-active-project-changed();
    <project-closed-message> =>
      deuce-note-project-closed(project);
    <project-sources-updated-message> =>
      deuce-note-project-sources-updated(project);
    <all-breakpoints-state-change-message> =>
      let state = message-breakpoint-state(message);
      deuce-note-all-breakpoints-changed(project, state);
    <single-breakpoint-state-change-message> =>
      let breakpoint = message-breakpoint(message);
      let state = message-breakpoint-state(message);
      deuce-note-single-breakpoint-changed(project, breakpoint, state);
    <project-database-updated-message> =>
      deuce-note-project-products-changed(project);
    otherwise =>
      #f;
  end
end function deuce-editor-project-message-receiver;

define method deuce-note-active-project-changed () => ()
  // Uncache the buffer->project mapping, to be recached below.
  for (buffer in editor-buffers($environment-editor))
    remove-property!(buffer-properties(buffer), #"project")
  end;
  // Notify all editor frames that their frame-current-project
  // may have changed.
  call-in-editor-frame-duim-frames(frame-note-project-updated);
  do-environment-editor-windows
    (method (window :: <window>)
       when (instance?(sheet-frame(window), <environment-editor>))
	 queue-redisplay(window, $display-all);
	 sheet-mapped?(window) & redisplay-window(window)
       end
     end method)
end method deuce-note-active-project-changed;

define sealed method do-environment-editor-windows
    (function :: <function>, #key project)
  for (window :: <basic-window> in editor-windows($environment-editor))
    when (~project
	    | begin
		let frame = sheet-frame(window);
		frame & project == frame-current-project(frame)
	      end)
      function(window)
    end
  end
end method do-environment-editor-windows;

// Apply FUNCTION to all DUIM frames containing (or being) one of the
// $environment-editor's editor frames.  Apply it to each DUIM frame only
// once (e.g., the debugger contains several editor-frames).
define method call-in-editor-frame-duim-frames 
    (function :: <function>, #key project) => ()
  // Other threads might modify the list of editor frames,
  // so we take a (very) little care to avoid race conditions
  let _frames = editor-frames($environment-editor);	// Deuce frames
  let frames  = make(<stretchy-object-vector>);		// DUIM frames
  for (_frame in _frames)
    let window = _frame & frame-window(_frame);
    let frame  = window & sheet-frame(window);
    frame & add-new!(frames, frame)
  end;
  for (frame in frames)
    let include? = ~project | frame-current-project(frame) = project;
    include? & frame-mapped?(frame) & function(frame)
  end
end method call-in-editor-frame-duim-frames;


/// Environment editor pane classes

//---*** Lose this class and everything that messes with 'primary-object-interval'
//---*** Only 'dylanworks-breakpoint-menu' and 'set-editor-breakpoint-popup-target' seem to care
define abstract class <primary-object-interval-mixin> (<object>)
  sealed slot primary-object-interval :: false-or(type-union(<interval>, <string>)) = #f
end class <primary-object-interval-mixin>;

define sealed method primary-object-interval
    (window :: <basic-window>)
 => (interval :: false-or(type-union(<interval>, <string>)))
  #f
end method primary-object-interval;

define sealed method primary-object-interval-setter
    (interval :: false-or(type-union(<interval>, <string>)), window :: <basic-window>)
 => (interval :: false-or(type-union(<interval>, <string>)))
  #f
end method primary-object-interval-setter;


define open abstract class <environment-deuce-pane>
    (<deuce-pane>)
end class <environment-deuce-pane>;

define method initialize (window :: <environment-deuce-pane>, #key)
  next-method();
  // Don't go through 'set-default-font-size', because the sheet
  // is not grafted to the port yet, nor is there a medium...
  let font = editor-policy($environment-editor).default-font;
  // Update all of the window's fonts
  window-default-font(window)
    := make-font(font-family(font), font-name(font), font-weight(font),
		 font-slant(font), font-size(font));
  window-default-bold-font(window)
    := make-font(font-family(font), font-name(font), #"bold",
		 font-slant(font), font-size(font));
  window-default-italic-font(window)
    := make-font(font-family(font), font-name(font), font-weight(font),
		 #"italic", font-size(font));
end method initialize;

define open class <environment-deuce-gadget>
    (<environment-deuce-pane>,
     <deuce-gadget>)
  keyword editor: = $environment-editor;
end class <environment-deuce-gadget>;

define sealed class <environment-editor-pane>
    (<primary-object-interval-mixin>,
     <environment-deuce-pane>)
end class <environment-editor-pane>;


/// Environment editor frames

define frame <environment-editor> 
    (<basic-editor-frame>,
     <frame-undo-mixin>,
     <frame-refresh-mixin>,
     <frame-cascading-window-mixin>,
     <basic-environment-tool>)
  slot %project :: false-or(<project-object>) = #f;
  constant slot %lines   :: false-or(<integer>) = #f,
    init-keyword: lines:;
  constant slot %columns :: false-or(<integer>) = #f,
    init-keyword: columns:;
  constant slot %mode-box :: false-or(<collection-gadget>) = #f;
  // An alist of (transaction-id . section) for use by c-sh-C
  slot %transaction-ids :: <stretchy-object-vector> = make(<stretchy-vector>);
  pane %window (frame)
    make(<environment-editor-pane>,
	 frame: frame,
	 lines:   frame.%lines,
	 columns: frame.%columns);
  layout (frame)
    scrolling (scroll-bars: #"both")
      frame.%window
    end;
  tool-bar (frame)
    make-environment-tool-bar(frame);
  status-bar (frame)
    make-deuce-status-bar(frame);
  command-table (frame)
    *editor-command-table*;
  keyword frame-class-name:, init-value: #"editor";
  keyword editor: = $environment-editor;
  // Note that these get overwritten by saved settings
  keyword lines:   = 50;
  keyword columns: = 80;
  keyword icon: = $editor-window-small-icon;
end frame <environment-editor>;

define cascading-window-settings 
  editor-window :: <environment-editor> = "Editor Window";


/// Frame start-up and initialization

// Note: If you change this method, change the reinitialize-frame method below.
define method initialize 
    (frame :: <environment-editor>,
     #key buffer :: false-or(<basic-buffer>),
          buffer-pathname: pathname :: false-or(<string>),
          line, index = 0,
	  new-file? :: <boolean> = #f,
          deuce-frame) => ()
  ignore(deuce-frame);
  next-method();
  frame-input-focus(frame) := frame.%window;
  frame-window(frame)      := frame.%window;
  command-enabled?(frame-edit-search-options, frame) := #t;
  let sccs = current-source-control-system();
  when (sccs)
    add-command-table-menu-item
      (*editor-command-table*, 
       sccs-label(sccs), <command-table>, *editor-source-control-command-table*,
       after: "Window",            //---*** SHOULD BE: after: "Application",
       error?: #f);
    disable-unimplemeted-sccs-commands(frame, sccs)
  end;
  do-reinitialize-frame(frame,
			buffer: buffer, buffer-pathname: pathname,
			line: line, index: index,
			new-file?: new-file?)
end method initialize;

// Doing the file-loading work here means that on the first opening of
// the frame, the frame won't appear until the file is loaded.  It then
// makes sense in terms of code reuse to do it in reinitialization too.
define method do-reinitialize-frame
    (frame :: <environment-editor>,
     #key buffer :: false-or(<basic-buffer>),
          buffer-pathname: pathname :: false-or(<string>),
          line, index = 0,
          new-file? :: <boolean> = #f) => ()
  //---*** There should maybe be some locking in here, in case re-use
  //---*** clashes with operations done on the frame's own thread.
  // 'new-file?' is intended to be mutually-exclusive with 'buffer' and 'buffer-pathname'.
  dynamic-bind (*editor-frame* = frame)
    let initial-buffer
      = buffer
	| when (pathname) try-to-load-from-pathname(frame, pathname) end
	| when (new-file?) make-empty-buffer(<non-file-buffer>) end
	| get-initial-buffer(frame);
    if (frame-mapped?(frame))
      try-to-select-buffer(frame, initial-buffer,
			   line: line, index: index);
    else
      // We'll call 'try-to-select-buffer' in the <frame-created-event> handler.
      frame-buffer(frame) := initial-buffer;
    end;
    //--- Remember the most recently opened files
    let locator = buffer-locator(initial-buffer);
    when (locator) most-recent-file() := locator end;
  end
end method do-reinitialize-frame;

define method frame-top-level
    (frame :: <environment-editor>) => (#rest values)
  dynamic-bind (*editor-frame* = frame)
    let buffer = get-initial-buffer(frame);
    dynamic-bind (*buffer* = buffer)
      select-buffer(frame-window(frame), buffer);
      let top-sheet = top-level-sheet(frame);
      while (#t)
	let event = read-event(top-sheet);
	block ()
	  handle-event(event-handler(event-client(event)), event);
	exception (e :: <command-error>)
	  when (command-error-format-string(e))
	    apply(deuce/display-error-message,
		  command-error-window(e),
		  command-error-format-string(e), command-error-format-arguments(e))
	  end;
	  #f
	end
      end
    end
  end
end method frame-top-level;

define method handle-event
    (frame :: <environment-editor>, event :: <frame-created-event>) => ()
  next-method();
  // Set up thread variables and initial buffer
  let window :: false-or(<basic-window>) = frame-window(frame);
  let buffer :: false-or(<basic-buffer>) = frame-buffer(frame);
  *editor-frame* := frame;
  *buffer*       := buffer;
  try-to-select-buffer(frame, buffer);
  // Do initial notifications, other than those which will be done
  // during buffer-selection
  //--- Should I do undo/redo here?
  let policy = editor-policy(frame-editor(frame));
  window-note-policy-changed(frame-window(frame), policy, #f);
end method handle-event;

define method handle-event
    (frame :: <environment-editor>, event :: <frame-focus-in-event>) => ()
  next-method();		// off to <search-target-frame-mixin>...
  frame-input-focus(frame) := frame.%window
end method handle-event;

define method frame-target-pane
    (frame :: <environment-editor>)
 => (pane :: <sheet>)
  frame.%window
end method frame-target-pane;


/// Frame shutdown

define method exit-editor (frame :: <environment-editor>) => ()
  exit-frame(frame)
end method exit-editor;

// This function returns #t iff the _frame_ is the only instance of
// _class_ in existence.
//---*** Something like this would probably belong better in Environment-Framework.
//---*** I would use 'find-matching-frames', but it will find only _reusable_ frames.
define function only-frame?
    (frame :: <frame>, class :: subclass(<frame>))
 => (only-frame? :: <boolean>)
  // Oh! for dependent types in parameter lists!
  assert(instance?(frame, class),
         "'only-frame?' must only be used on frames of the supplied class");
  let frames-of-class :: <integer> = 0;
  block (exit)
    do-frames(method (_frame :: <frame>)
                when (instance?(_frame, class))
                  frames-of-class := frames-of-class + 1;
                end;
                when (frames-of-class > 1)
                  exit();
                end;
	      end method,
              port: port(frame));
  end;
  (frames-of-class == 1)
end function only-frame?;

define method frame-can-exit?
    (frame :: <environment-editor>)
 => (exit? :: <boolean>)
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-buffer> = frame-buffer(frame);
  let editor = frame-editor(frame);
  let policy = editor-policy(editor);
  local method maybe-save-then-maybe-kill-buffer (window, buffer)
	  // Don't let 'kill-buffer' exit this frame, because we're already
	  // in the 'frame-exit' code!
	  let failed? = maybe-save-buffer(window, buffer);
	  unless (failed?)
	    // Kill the buffer 
	    kill-buffer(buffer, frame: frame, no-exit-frame: frame);
	    #t
	  end
	end method;
  // Bind *editor-frame* in case this is called on a different thread
  dynamic-bind (*editor-frame* = frame)
    case
      only-frame?(frame, <environment-editor>) =>
	// If we're the only editor frame left...
	let all-buffers
	  = choose(method (b) ~buffer-anonymous?(b) end, editor-buffers(editor));
        if (size(all-buffers) = 1 & all-buffers[0] == buffer)
	  // ...then if there is exactly one buffer left, use the simple dialog
	  maybe-save-then-maybe-kill-buffer(window, buffer)
	else
	  // ...then if there are multiple buffers left, use the "save all" dialog
	  let buffers = save-buffers-dialog(window, exit-label: "&Close");
	  select (buffers)
	    #f        => #t;
	    #"cancel" => #f;
	    otherwise =>
	      do-save-all-files(frame, buffers, curry(deuce/display-message, window));
	      // With the 'fixed-frame-buffer?' policy, we have to kill the buffer
	      // associated with this frame, no matter what, so that it doesn't come
	      // back and haunt us later
	      when (fixed-frame-buffer?(policy))
		kill-buffer(buffer, frame: frame, no-exit-frame: frame)
	      end;
	      #t
	  end
	end;
      otherwise =>
	// If we're not the only editor frame left...
	if (fixed-frame-buffer?(policy))
	  // ...then if the 'fixed-frame-buffer?' policy is in operation, offer
	  // to save just this buffer, and kill it before closing the window
	  maybe-save-then-maybe-kill-buffer(window, buffer)
	else
	  // ...then if we're just another Emacs-style frame, just close it
	  #t
	end;
    end
  end
end method frame-can-exit?;

define method handle-event
    (frame :: <environment-editor>, event :: <frame-exit-event>) => ()
  when (event-destroy-frame?(event))
    // If this frame is going away, stop tracking it
    //--- Deuce should probably provide some function that does this...
    let frames = editor-frames(frame-editor(frame));
    remove!(frames, frame);
  end;
  next-method();
end method handle-event;


/// Selecting buffers and loading files into buffers

// In an editor pane (i.e., a pane in the Editor), obey the selection policy
define method select-buffer-in-appropriate-window
    (window :: <environment-editor-pane>, buffer :: <buffer>, #key line, index = 0) => ()
  let frame  = window-frame(window);
  if (fixed-frame-buffer?(editor-policy(frame-editor(frame))))
    // Windows-like policy -- each buffer gets its own window
    find-deuce-frame(buffer: buffer, line: line, index: index)
  else
    // Emacs-like policy -- a window can view multiple buffers
    select-buffer(window, buffer);
    when (line)
      move-point!(line, index: index, window: window)
    end;
    queue-redisplay(window, $display-all)
  end
end method select-buffer-in-appropriate-window;

// In Deuce gadgets contained in other kinds of frames, always try to select
// a proper Editor frame
define method select-buffer-in-appropriate-window
    (window :: <environment-deuce-gadget>, buffer :: <buffer>, #key line, index = 0) => ()
  find-deuce-frame(buffer: buffer, line: line, index: index)
end method select-buffer-in-appropriate-window;

// By default, no can do on this command in Deuce gadgets
define method choose-buffer (frame :: <environment-deuce-gadget>) => ()
  let window :: <basic-window> = frame-window(frame);
  deuce/display-error-message(window, "")
end method choose-buffer;

// Ditto
define method switch-buffers (frame :: <environment-deuce-gadget>) => ()
  let window :: <basic-window> = frame-window(frame);
  deuce/display-error-message(window, "")
end method switch-buffers;

// Ditto
define method new-buffer (frame :: <environment-deuce-gadget>) => ()
  let window :: <basic-window> = frame-window(frame);
  deuce/display-error-message(window, "")
end method new-buffer;


define method try-to-select-buffer
    (frame :: <environment-editor>, buffer :: <basic-buffer>, #key line, index = 0) => ()
  let window :: false-or(<basic-window>) = frame-window(frame);
  when (window)
    case
      buffer ~== window-buffer(window) =>
	select-buffer(window, buffer);
	when (line)
	  move-point!(line, index: index, window: window)
	end;
	queue-redisplay(window, $display-all);
	redisplay-window(window);
      line =>
	move-point!(line, index: index, window: window);
	queue-redisplay(window, $display-point, centering: 0);
	redisplay-window(window);
      otherwise =>
	#f;
    end
  end
end method try-to-select-buffer;

define method try-to-load-from-pathname
    (frame :: <environment-editor>, pathname :: <string>)
 => (buffer :: false-or(<basic-buffer>))
  if (frame-showing-pathname?(frame, pathname))
    frame-buffer(frame)
  else
    let editor = frame-editor(frame);
    let buffer :: false-or(<basic-buffer>)
      = find-buffer-from-pathname(editor, pathname)
        | do-find-file(editor, pathname, direction: #"input");
    when (buffer)
      frame-last-command-type(frame) := #"file";
      buffer
    end
  end
end method try-to-load-from-pathname;

define method frame-showing-pathname?
    (frame :: <environment-editor>, pathname :: <string>)
 => (showing? :: <boolean>)
  let buffer = frame-buffer(frame);
  when (buffer & file-buffer?(buffer))
    buffer-locator(buffer) = as(<file-locator>, pathname)
  end
end method frame-showing-pathname?;

define method get-initial-buffer
    (frame :: <environment-editor>)
 => (buffer :: <basic-buffer>)
  let editor = frame-editor(frame);
  frame-buffer(frame)
  | element(editor-buffers(editor), 0, default: #f)
  | make-initial-buffer(editor: editor)
end method get-initial-buffer;


/// Editor implementations of 'environment-tools' generic functions

//--- Ideally Deuce would use locators itself for buffer-pathname...
define method buffer-pathname-as-string
    (buffer :: <buffer>) => (pathname :: false-or(<string>))
  let pathname = buffer-pathname(buffer);
  pathname & as(<string>, pathname)
end method buffer-pathname-as-string;

//--- Ideally Deuce would use locators itself for buffer-pathname...
define method buffer-locator
    (buffer :: <buffer>) => (locator :: false-or(<file-locator>))
  let pathname = buffer-pathname(buffer);
  pathname & as(<file-locator>, pathname)
end method buffer-locator;

define method buffer-title
    (buffer :: <buffer>, #key show-path? :: <boolean> = #t)
 => (title :: <string>)
  when (file-buffer?(buffer))
    let locator = buffer-locator(buffer);
    when (locator)
      let directory = show-path? & locator-directory(locator);
      if (directory)
	concatenate(locator-name(locator),
		    " (", as(<string>, directory), ")")
      else
	locator-name(locator)
      end
    end
  end
  | buffer-name(buffer)
end method buffer-title;

define method generate-frame-title
    (frame :: <environment-editor>) => (title :: <string>)
  let editor = frame-editor(frame);
  let buffer = frame-buffer(frame);
  let policy = editor-policy(editor);
  let show-path? = show-path-in-title?(policy);
  let name = buffer & buffer-title(buffer, show-path?: show-path?);
  let current-project = frame-current-project(frame);
  let current-project-name
    = current-project
        & environment-object-display-name(current-project, current-project, #f);
  concatenate(if (name)
                name
              else
                // This case probably won't happen, since there currently can't
                // be an editor window without a file, but let's handle this
                // gracefully anyway.
                "No Name"
              end,
              if (current-project-name)
		concatenate(" - Editing ", current-project-name)
	      else
		""
	      end,
              " - ", release-product-name())
end method generate-frame-title;

define method make-environment-tool-bar-buttons
    (frame :: <environment-editor>) => (buttons :: <vector>)
  let framem = frame-manager(frame);
  with-frame-manager (framem)
    let left-buttons  = make-editor-tool-bar-left-buttons(frame);
    let right-buttons = make-editor-tool-bar-right-buttons(frame);
    let mode-box      = make-editor-tool-bar-mode-box(frame);
    let buttons :: <stretchy-object-vector> = make(<stretchy-vector>);
    when (left-buttons & ~empty?(left-buttons))
      add!(buttons, make(<row-layout>, children: left-buttons, spacing: 0))
    end;
    concatenate!(buttons, next-method());
    when (right-buttons & ~empty?(right-buttons))
      add!(buttons, make(<row-layout>, children: right-buttons, spacing: 0))
    end;
    when (mode-box)
      add!(buttons, mode-box)
    end;
    buttons
  end
end method make-environment-tool-bar-buttons;

define method make-editor-tool-bar-left-buttons
    (frame :: <environment-editor>) => (buttons :: <vector>)
  vector(make(<button>,
	      label: $new-text-file-bitmap,
	      documentation: $new-text-file-title,
	      command: editor-new-file,
	      activate-callback:
		method (sheet)
		  ignore(sheet);
		  editor-new-file();
		end),
	 make(<button>,
	      label: $open-bitmap,
	      documentation: $open-file-title,
	      command: frame-open-file,
	      activate-callback:
		method (sheet)
		  frame-open-file(sheet-frame(sheet));
		end),
	 make(<button>,
	      label: $save-bitmap,
	      documentation: $save-file-title,
	      command: frame-save-file,
	      activate-callback:
		method (sheet)
		  frame-save-file(sheet-frame(sheet));
		end))
end method make-editor-tool-bar-left-buttons;

define method make-editor-tool-bar-right-buttons
    (frame :: <environment-editor>) => (buttons :: <vector>)
  make-clone-tool-bar-buttons(frame);
/* --- hughg, 1998/02/11: not used in release 1.0.
  vector(make(<button>,
	      label: "|<-",
	      command: unindent-rigidly,
	      activate-callback:
		method (sheet)
		  execute-command-in-frame(sheet-frame(sheet), unindent-rigidly)
		end),
	 make(<button>,
	      label: "->|",
	      command: indent-rigidly,
	      activate-callback:
		method (sheet)
		  execute-command-in-frame(sheet-frame(sheet), indent-rigidly)
		end))
*/
end method make-editor-tool-bar-right-buttons;

define method make-editor-tool-bar-mode-box
    (frame :: <environment-editor>) => (mode-box :: false-or(<collection-gadget>))
  //---*** There must be a better way to get the set of useful modes!
  let major-modes
    = sort(vector(find-mode(<fundamental-mode>),
		  find-mode(<text-mode>),
		  find-mode(<dylanworks-mode>)),
	   test: method (m1, m2)
		   mode-name(m1) < mode-name(m2)
		 end method);
  let initial-mode
    = if (frame-buffer(frame))
	buffer-major-mode(frame-buffer(frame))
      else
	find-mode(<fundamental-mode>)
      end;
  #f	/*---*** Not for Rel 1.0, since it doesn't work well
  let mode-box
    = make(<option-box>,
           label: "Mode",
           items: major-modes,
	   label-key: mode-name,
           value: initial-mode,
           documentation: "Selects a new major mode for the current buffer.",
           min-width: 100, max-width: 150,
           value-changed-callback:
	     method (box)
	       let frame    = sheet-frame(box);
	       let buffer   = frame-buffer(frame);
	       let window   = frame-window(frame);
	       let old-mode = buffer & buffer-major-mode(buffer);
	       let new-mode = gadget-value(box);
	       when (buffer & new-mode ~== old-mode)
		 exit-mode(buffer, old-mode);
		 enter-mode(buffer, new-mode);
		 sectionize-buffer(buffer);
		 queue-redisplay(window, $display-all);
		 redisplay-window(window)
	       end
	     end method);
  frame.%mode-box := mode-box;
  mode-box */
end method make-editor-tool-bar-mode-box;


/// Frame-reuse protocol extensions

define method editor-frame-showing-buffer?
    (frame :: <environment-editor>,
     #key buffer :: false-or(<basic-buffer>),
          buffer-pathname: pathname :: false-or(<string>))
 => (showing-buffer? :: <boolean>)
  (buffer & frame-buffer(frame) == buffer)
  | (pathname & frame-showing-pathname?(frame, pathname))
end method editor-frame-showing-buffer?;

define method reuse-matching-frame?
    (portd :: <port-designator>,
     frame :: <environment-editor>,
     class :: subclass(<environment-editor>),
     #rest initargs,
     #key buffer :: false-or(<basic-buffer>),
          buffer-pathname: pathname :: false-or(<string>))
 => (reuse? :: <boolean>)
  next-method()
  & if (fixed-frame-buffer?(editor-policy(frame-editor(frame))))
      // If we're in SDI mode, we can only reuse the frame if there's
      // a buffer requested and it's already showing that buffer.
      (buffer | pathname)
      & editor-frame-showing-buffer?
	  (frame, buffer: buffer, buffer-pathname: pathname)
    else
      // In non-SDI mode, we can reuse the frame by default.
      #t
    end
end method reuse-matching-frame?;

define method find-matching-frames
    (portd :: <port-designator>,
     class :: subclass(<environment-editor>),
     #rest initargs,
     #key buffer :: false-or(<basic-buffer>),
          buffer-pathname: pathname :: false-or(<string>),
          editor = $environment-editor,
          deuce-frame :: false-or(<basic-editor-frame>))
 => (frames :: <sequence>)
  let frames = next-method();
  local method matching-frame? (frame) 
	  frame-editor(frame) == editor
	  & (editor-frame-showing-buffer?
	       (frame, buffer: buffer, buffer-pathname: pathname))
	end;
  if (deuce-frame
      & (frame-editor(deuce-frame) == editor)
      & member?(deuce-frame, frames))
    vector(deuce-frame)
  elseif (buffer | pathname)
    let best-frames = choose(matching-frame?, frames);
    if (empty?(best-frames))
      frames
    else
      best-frames
    end
  else
    frames
  end
end method find-matching-frames;

// Note: If you change this method, change the initialize method above.
define method reinitialize-frame 
    (frame :: <environment-editor>,
     #rest initargs,
     #key buffer :: false-or(<basic-buffer>),
          buffer-pathname: pathname :: false-or(<string>),
          new-file? :: <boolean> = #f,
          deuce-frame) => ()
  ignore(deuce-frame);
  next-method();
  apply(do-reinitialize-frame, frame,
        buffer: buffer, buffer-pathname: pathname, new-file?: new-file?, #[]);
end method reinitialize-frame;



/// Frame<->project relationships

define method frame-note-project-updated
    (frame :: <environment-editor>) => ()
  let project = frame-current-project(frame);
  if (project ~== frame.%project)
    frame-note-project-changed(frame, project)
  end
end method frame-note-project-updated;

define method frame-note-project-changed
    (frame :: <environment-editor>, project :: false-or(<project-object>)) => ()
  dylanworks-mode-commands-enabled?(frame) := project ~== #f;
  frame-title(frame) := generate-frame-title(frame);
  let application = project & project-application(project);
  let state = application & application-state(application);
  frame-note-application-state-changed(frame, state);
  frame.%project := project
end method frame-note-project-changed;

define method frame-note-interaction-returned
    (frame :: <environment-editor>, thread :: <thread-object>, id :: <object>) => ()
  let entry = find-value(frame.%transaction-ids, method (e) head(e) == id end);
  when (entry)
    let id      = head(entry);
    let section = tail(entry);
    let project = frame-current-project(frame);
    let record  = transaction-id-source-record(project, id);
    frame.%transaction-ids := remove!(frame.%transaction-ids, entry);
    // Record the fact that there is an interactive definition "shadowing"
    // the definition in the [file] buffer
    put-property!(line-properties(section-start-line(section)), #"interactive-record", record);
    // Obsolete any breakpoints that might have existed in the old definition
    let window = frame-window(frame);
    let buffer = section-home-buffer(section, editor: $environment-editor);
    let table  = buffer & buffer-breakpoints-table(buffer, project);
    when (table)
      do-lines
	(method (line, start-index, end-index, last-line?)
	   ignore(start-index, end-index, last-line?);
	   let info = element(table, line, default: #f);
	   when (info)
	     let bkp = info.info-breakpoint;
	     breakpoint-message?(bkp)
	       := format-to-string("Hit breakpoint in out-of-date version of %s",
				   section-definition-name(section) | "this function");
	     remove-key!(table, line);
	     queue-redisplay(window, $display-line, line: line, index: 0, centering: #f)
	   end
	 end method,
	 section);
      redisplay-window(window)
    end;
    // Ensure the section and record are properly tracked
    gethash($interactive-source-sections, record)  := section;
    gethash($interactive-source-records,  section) := record;
    let message
      = format-to-string("%s successfully downloaded",
			 section-definition-name(section));
    frame-status-message(frame) := message
  end
end method frame-note-interaction-returned;

define method frame-note-interactive-compilation-warnings
    (frame :: <environment-editor>, thread :: <thread-object>, id :: <object>,
     warnings :: <sequence>) => ()
  let entry = find-value(frame.%transaction-ids, method (e) head(e) == id end);
  when (entry)
    let section = tail(entry);
    let project = frame-current-project(frame);
    frame.%transaction-ids := remove!(frame.%transaction-ids, entry);
    let message 
      = format-to-string("%s compiled: %s",
			 section-definition-name(section),
			 compilation-warning-count-message(project, warnings: warnings));
    frame-status-message(frame) := message
  end
end method frame-note-interactive-compilation-warnings;


/// Implementation of environment-manager functionality

// This is added to the generic function in environment-manager, so
// it can't be a 'define function'.
define sideways method find-deuce-frame
    (#rest initargs, #key, #all-keys) => ()
  apply(find-environment-frame,
	default-port(), <environment-editor>,
	initargs)
end method find-deuce-frame;

define sideways method find-and-call-in-deuce-frame
    (function :: <function>, #rest initargs, #key, #all-keys) => ()
  apply(call-in-environment-frame,
	function, default-port(), <environment-editor>,
	initargs);
end method find-and-call-in-deuce-frame;
    

// Display the buffer name in the title bar
define method deuce/display-buffer-name
    (window :: <environment-editor-pane>, buffer :: false-or(<basic-buffer>)) => ()
  let frame = sheet-frame(window);
  when (frame)
    frame-title(frame) := generate-frame-title(frame)
  end
end method deuce/display-buffer-name;


/// Notifications

/*
define method window-note-buffer-changed
    (window :: <environment-editor-pane>,
     buffer :: <basic-buffer>, modified? :: <boolean>) => ()
  next-method();		// this will display the buffer name, etc
end method window-note-buffer-changed;
*/


// Yes, this _is_ meant to specialise on <environment-deuce-pane>.
define method window-note-buffer-read-only
    (window :: <environment-deuce-pane>,
     buffer :: <basic-buffer>, read-only? :: <boolean>) => ()
  next-method();		// this will display the modified state
  let frame = sheet-frame(window);
  when (frame)
    // Set the command enabling to fit the current selection...
    window-note-selection-changed(window, window-mark(window));
    // ...then override it if the buffer is read-only.
    when (read-only? == #t)
      command-enabled?(<frame-cut-command>,    frame) := #f;
      command-enabled?(<frame-copy-command>,   frame) := #f;
      command-enabled?(<frame-paste-command>,  frame) := #f;
      command-enabled?(<frame-delete-command>, frame) := #f
    end
  end
end method window-note-buffer-read-only;


define method window-note-buffer-selected
    (window :: <environment-editor-pane>, buffer :: <basic-buffer>) => () 
  // The next-method will update modification and read-only status fields
  // and the frame title.
  next-method();
  let frame = sheet-frame(window);
  when (frame)
    command-enabled?(frame-revert-file, frame)  := #t;
    command-enabled?(frame-close-file, frame)   := #t;
    command-enabled?(frame-save-file, frame)    := #t;
    command-enabled?(frame-save-file-as, frame) := #t;
    update-optimization-commands(window, buffer);
    when (frame.%mode-box)
      gadget-value(frame.%mode-box) := buffer-major-mode(buffer)
    end;
    frame-note-project-updated(frame)
  end
end method window-note-buffer-selected;

define method window-note-buffer-selected
    (window :: <environment-editor-pane>, buffer == #f) => ()
  // The next-method will update modification and read-only status fields
  // and the frame title.
  next-method();
  let frame = sheet-frame(window);
  when (frame)
    command-enabled?(frame-revert-file, frame)  := #f;
    command-enabled?(frame-close-file, frame)   := #f;
    command-enabled?(frame-save-file, frame)    := #f;
    command-enabled?(frame-save-file-as, frame) := #f;
    when (frame.%mode-box)
      gadget-value(frame.%mode-box) := find-mode(<fundamental-mode>)
    end;
    frame-note-project-updated(frame)
  end
end method window-note-buffer-selected;


// Yes, this _is_ meant to specialise on <environment-deuce-pane>.
define method window-note-selection-changed
    (window :: <environment-deuce-pane>, mark :: false-or(<basic-bp>)) => ()
  next-method();
  let frame = sheet-frame(window);
  when (frame)
    note-frame-selection-updated(frame)
  end
end method window-note-selection-changed;


define method window-note-search-string
    (window :: <environment-editor-pane>, string :: false-or(<string>)) => ()
  next-method();
  let frame = sheet-frame(window);
  when (frame)
    note-frame-searching-updated(frame)
  end
end method window-note-search-string;


define method window-note-undo/redo
    (window :: <environment-editor-pane>,
     undo? :: <boolean>, redo? :: <boolean>)
 => ()
  next-method();
  let frame = sheet-frame(window);
  when (frame & frame-input-focus(frame) == window)
    command-enabled?(<frame-undo-command>, frame) := undo?;
    command-enabled?(<frame-redo-command>, frame) := redo?
  end
end method window-note-undo/redo;


/// File printing

define method frame-hardcopy-document
    (frame :: <environment-editor>) => ()
  let buffer   = frame-buffer(frame);
  let pathname = buffer & buffer-default-pathname(buffer);
  pathname & frame-hardcopy-object(frame, as(<file-locator>, pathname))
end method frame-hardcopy-document;


/// Policy persistence

define settings <dylan-editor-settings> (<open-dylan-user-settings>)
  key-name "Editor";
  slot default-x :: <integer>;
  slot default-y :: <integer>;
  slot default-width  :: <integer>;
  slot default-height :: <integer>;
end settings <dylan-editor-settings>;

define settings <editor-dialog-settings> (<dylan-editor-settings>)
  key-name "Dialogs";
  slot buffer-box-width  :: <integer> = $buffer-box-width;
  slot buffer-box-height :: <integer> = $buffer-box-height;
  slot choose-string-field-width :: <integer> = $choose-string-field-width;
  slot search-field-width     :: <integer> = $search-string-field-width;
end settings <editor-dialog-settings>;

define settings <editor-input-settings> (<dylan-editor-settings>)
  key-name "Input";
  slot command-set :: <symbol> = #"windows";
  slot alt-key :: <boolean> = #f;
  slot initial-click :: <boolean> = #f;
end settings <editor-input-settings>;

define settings <editor-editing-settings> (<dylan-editor-settings>)
  key-name "Editing";
  slot kill-to-clipboard :: <boolean> = #t;
  slot replace-selection :: <boolean> = #t;
  slot unselected-copy :: <symbol> = #"copy-line";
  slot add-newline :: <boolean> = #f;
  slot undo-past-save :: <boolean> = #f;
  slot confirm-kill :: <boolean> = #f;
  slot new-file-buffer :: <boolean> = #f;
end settings <editor-editing-settings>;

define constant $default-font-point-size :: <integer> = 8;

define settings <editor-display-settings> (<dylan-editor-settings>)
  key-name "Display";
  slot mark-policy :: <symbol> = #"end-of-line";
  slot scrolling-moves :: <boolean> = #f;
  slot default-font-family :: <symbol> = #"fix";
  slot default-font-name :: <string> = "";
  slot default-font-weight :: <symbol> = #"normal";
  slot default-font-slant :: <symbol> = #"normal";
  slot default-font-point-size :: <integer> = $default-font-point-size;
  slot tab-size        :: <integer> = 8;
  slot fixed-frame     :: <boolean> = #t;
  slot show-path       :: <boolean> = #f;
  slot show-separators :: <boolean> = #f;
end settings <editor-display-settings>;

define settings <editor-search-settings> (<dylan-editor-settings>)
  key-name "Search";
  slot wrap-searches :: <boolean> = #t;
  slot use-isearch   :: <boolean> = #t;
  slot mark-search   :: <boolean> = #t;
end settings <editor-search-settings>;

define variable *editor-dialog-settings*  = make(<editor-dialog-settings>);
define variable *editor-input-settings*   = make(<editor-input-settings>);
define variable *editor-editing-settings* = make(<editor-editing-settings>);
define variable *editor-display-settings* = make(<editor-display-settings>);
define variable *editor-search-settings*  = make(<editor-search-settings>);

define function editor-settings-font () => (font :: <font>)
  let name = *editor-display-settings*.default-font-name;
  let _size = *editor-display-settings*.default-font-point-size;
  make-font(*editor-display-settings*.default-font-family,
	    if (empty?(name)) #f else name end,
	    *editor-display-settings*.default-font-weight,
	    *editor-display-settings*.default-font-slant,
	    if (zero?(_size)) $default-font-point-size else _size end)
end function editor-settings-font;

define function load-policy-settings (policy :: <editor-policy>) => ()
  $buffer-box-width  := *editor-dialog-settings*.buffer-box-width;
  $buffer-box-height := *editor-dialog-settings*.buffer-box-height;
  $choose-string-field-width := *editor-dialog-settings*.choose-string-field-width;
  $search-string-field-width   := *editor-dialog-settings*.search-field-width;
  policy.command-set-policy := *editor-input-settings*.command-set;
  policy.alt-key-is-meta?   := *editor-input-settings*.alt-key;
  policy.initial-click-moves-point? := *editor-input-settings*.initial-click;
  policy.clipboard-policy := *editor-editing-settings*.kill-to-clipboard;
  policy.typing-replaces-selection? := *editor-editing-settings*.replace-selection;
  policy.unselected-copy-policy  := *editor-editing-settings*.unselected-copy;
  policy.next-line-adds-newline? := *editor-editing-settings*.add-newline;
  policy.undo-past-save-policy   := *editor-editing-settings*.undo-past-save;
  policy.confirm-kill-buffer?    := *editor-editing-settings*.confirm-kill;
  policy.new-file-buffer? := *editor-editing-settings*.new-file-buffer;
  policy.marking-policy   := *editor-display-settings*.mark-policy;
  policy.scrolling-moves-point? := *editor-display-settings*.scrolling-moves;
  policy.default-font := editor-settings-font();
  policy.tab-stop-size     := *editor-display-settings*.tab-size;
  policy.fixed-frame-buffer? := *editor-display-settings*.fixed-frame;
  policy.show-section-separators? := *editor-display-settings*.show-separators;
  policy.show-path-in-title? := *editor-display-settings*.show-path;
  policy.wrap-searches? := *editor-search-settings*.wrap-searches;
  policy.use-isearch?   := *editor-search-settings*.use-isearch;
end function load-policy-settings;

define function save-policy-settings (policy :: <editor-policy>) => ()
  // Only save things that changed, in case saving unchanged settings is slow
  when (*editor-dialog-settings*.buffer-box-width ~= $buffer-box-width)
    *editor-dialog-settings*.buffer-box-width := $buffer-box-width
  end;
  when (*editor-dialog-settings*.buffer-box-height ~= $buffer-box-height)
    *editor-dialog-settings*.buffer-box-height := $buffer-box-height
  end;
  when (*editor-dialog-settings*.choose-string-field-width ~= $choose-string-field-width)
    *editor-dialog-settings*.choose-string-field-width := $choose-string-field-width
  end;
  when (*editor-dialog-settings*.search-field-width ~= $search-string-field-width)
    *editor-dialog-settings*.search-field-width := $search-string-field-width
  end;
  when (*editor-input-settings*.command-set ~= policy.command-set-policy)
    *editor-input-settings*.command-set := policy.command-set-policy
  end;
  when (*editor-input-settings*.alt-key ~= policy.alt-key-is-meta?)
    *editor-input-settings*.alt-key := policy.alt-key-is-meta?
  end;
  when (*editor-input-settings*.initial-click ~= policy.initial-click-moves-point?)
    *editor-input-settings*.initial-click := policy.initial-click-moves-point?
  end;
  when (*editor-editing-settings*.kill-to-clipboard ~= policy.clipboard-policy)
    *editor-editing-settings*.kill-to-clipboard := policy.clipboard-policy
  end;
  when (*editor-editing-settings*.replace-selection ~= policy.typing-replaces-selection?)
    *editor-editing-settings*.replace-selection := policy.typing-replaces-selection?
  end;
  when (*editor-editing-settings*.unselected-copy ~= policy.unselected-copy-policy)
    *editor-editing-settings*.unselected-copy := policy.unselected-copy-policy
  end;
  when (*editor-editing-settings*.add-newline ~= policy.next-line-adds-newline?)
    *editor-editing-settings*.add-newline := policy.next-line-adds-newline?
  end;
  when (*editor-editing-settings*.undo-past-save ~= policy.undo-past-save-policy)
    *editor-editing-settings*.undo-past-save := policy.undo-past-save-policy
  end;
  when (*editor-editing-settings*.confirm-kill ~= policy.confirm-kill-buffer?)
    *editor-editing-settings*.confirm-kill := policy.confirm-kill-buffer?
  end;
  when (*editor-editing-settings*.new-file-buffer ~= policy.new-file-buffer?)
    *editor-editing-settings*.new-file-buffer := policy.new-file-buffer?
  end;
  when (*editor-display-settings*.mark-policy ~= policy.marking-policy)
    *editor-display-settings*.mark-policy := policy.marking-policy
  end;
  when (*editor-display-settings*.scrolling-moves ~= policy.scrolling-moves-point?)
    *editor-display-settings*.scrolling-moves := policy.scrolling-moves-point?
  end;
  let policy-font = policy.default-font;
  when (editor-settings-font() ~= policy-font)
    let (family, name, weight, slant, _size)
      = values(policy-font.font-family, policy-font.font-name,
	       policy-font.font-weight, policy-font.font-slant,
	       policy-font.font-point-size);
    *editor-display-settings*.default-font-family
      := if (instance?(family, <symbol>)) family else #"fix" end;
    *editor-display-settings*.default-font-name
      := if (instance?(name, <string>)) name else "" end;
    *editor-display-settings*.default-font-weight
      := if (instance?(weight, <symbol>)) weight else #"normal" end;
    *editor-display-settings*.default-font-slant
      := if (instance?(slant, <symbol>)) slant else #"normal" end;
    *editor-display-settings*.default-font-point-size := _size | 0;
  end;
  when (*editor-display-settings*.tab-size ~= policy.tab-stop-size)
    *editor-display-settings*.tab-size := policy.tab-stop-size
  end;
  when (*editor-display-settings*.fixed-frame ~= policy.fixed-frame-buffer?)
    *editor-display-settings*.fixed-frame := policy.fixed-frame-buffer?
  end;
  when (*editor-display-settings*.show-separators ~= policy.show-section-separators?)
    *editor-display-settings*.show-separators := policy.show-section-separators?
  end;
  when (*editor-display-settings*.show-path ~= policy.show-path-in-title?)
    *editor-display-settings*.show-path := policy.show-path-in-title?
  end;
  when (*editor-search-settings*.wrap-searches ~= policy.wrap-searches?)
    *editor-search-settings*.wrap-searches := policy.wrap-searches?
  end;
  when (*editor-search-settings*.use-isearch ~= policy.use-isearch?)
    *editor-search-settings*.use-isearch := policy.use-isearch?
  end;
end function save-policy-settings;


define method window-note-policy-changed
    (window :: <environment-editor-pane>,
     new-policy :: <editor-policy>, old-policy :: false-or(<editor-policy>)) => ()
  ignore(old-policy);
  next-method();
  let frame = sheet-frame(window);
  when (frame)
    let allow-new-windows? = ~fixed-frame-buffer?(new-policy);
    command-enabled?(editor-frame-switch-buffers, frame) := allow-new-windows?;
    command-enabled?(frame-close-file, frame)    := allow-new-windows?;
    command-enabled?(clone-tool, frame)          := allow-new-windows?;
    command-enabled?(clone-and-link-tool, frame) := allow-new-windows?;
    // One of the policies wants Copy to be enabled even if there
    // is no selection, so pretend that the selection has changed
    // in order to do this enabling
    note-frame-selection-updated(frame)
  end;
  save-policy-settings(new-policy)
end method window-note-policy-changed;

/*
//---*** Who should set the geometry of newly created editors?
//---*** This should record the geometry of the _first_ editor...
define method handle-event
    (frame :: <environment-editor>, event :: <frame-exited-event>) => ()
  let (x, y) = frame-position(frame);
  *editor-settings*.default-x := x;
  *editor-settings*.default-y := y;
  let (width, height) = frame-position(frame);
  *editor-settings*.default-width  := width;
  *editor-settings*.default-height := height;
  next-method()
end method handle-event;
*/


/// Bootstrapping

define variable $environment-editor :: <deuce-editor> = make(<deuce-editor>);

define inline function default-editor-frame
    () => (frame :: false-or(<editor-state-mixin>))
  *editor-frame*
  | begin
      let frames = editor-frames($environment-editor);
      ~empty?(frames) & frames[0]
    end
end function default-editor-frame;
