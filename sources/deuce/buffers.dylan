Module:       deuce-internals
Synopsis:     The Deuce editor
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Buffers

define protocol <<buffer>> (<<interval>>)
  getter buffer-name
    (buffer :: <buffer>) => (name :: <string>);
  setter buffer-name-setter
    (name :: <string>, buffer :: <buffer>) => (name :: <string>);
  getter buffer-pathname
    (buffer :: <buffer>) => (pathname :: false-or(<pathname>));
  setter buffer-pathname-setter
    (pathname :: false-or(<pathname>), buffer :: <buffer>) => (pathname :: false-or(<pathname>));
  getter buffer-default-pathname
    (buffer :: <buffer>) => (pathname :: <pathname>);
  getter buffer-source-container
    (buffer :: <buffer>) => (container :: false-or(<source-container>));
  setter buffer-source-container-setter
    (container :: false-or(<source-container>), buffer :: <buffer>)
 => (container :: false-or(<source-container>));
  getter buffer-start-node
    (buffer :: <buffer>) => (node :: false-or(<node>));
  setter buffer-start-node-setter
    (node :: false-or(<node>), buffer :: <buffer>) => (node :: false-or(<node>));
  getter buffer-end-node
    (buffer :: <buffer>) => (node :: false-or(<node>));
  setter buffer-end-node-setter
    (node :: false-or(<node>), buffer :: <buffer>) => (node :: false-or(<node>));
  getter buffer-lock
    (buffer :: <buffer>) => (lock :: false-or(<recursive-lock>));
  getter buffer-read-only?
    (buffer :: <buffer>) => (read-only? :: <boolean>);
  setter buffer-read-only?-setter
    (read-only? :: <boolean>, buffer :: <buffer>) => (read-only? :: <boolean>);
  getter file-buffer?
    (buffer :: <buffer>) => (file? :: <boolean>);
  getter saves-like-file-buffer?
    (buffer :: <buffer>) => (saves? :: <boolean>);
  getter special-purpose-buffer?
    (buffer :: <buffer>) => (special-purpose? :: <boolean>);
  getter buffer-anonymous?		// could also be called 'buffer-invisible?'
    (buffer :: <buffer>) => (anonymous? :: <boolean>);
  getter buffer-section-separator-style
    (buffer :: <buffer>) => (style :: <section-separator-style>);
  getter buffer-major-mode
    (buffer :: <buffer>) => (mode :: <major-mode>);
  setter buffer-major-mode-setter
    (mode :: <major-mode>, buffer :: <buffer>) => (mode :: <major-mode>);
  getter buffer-minor-modes
    (buffer :: <buffer>) => (modes :: <stretchy-sequence>);
  setter buffer-minor-modes-setter
    (modes :: <stretchy-sequence>, buffer :: <buffer>) => (modes :: <stretchy-sequence>);
  getter buffer-undo-history
    (buffer :: <buffer>, #key section :: false-or(<section>))
 => (history :: false-or(<undo-history>), buffer :: false-or(<buffer>));
  function add-node!
    (buffer :: <buffer>, node :: <node>, #key after) => ();
  function remove-node!
    (buffer :: <buffer>, node :: <node>) => ();
  getter buffer-modification-tick
    (buffer :: <buffer>) => (tick :: <integer>);
  setter buffer-modification-tick-setter
    (tick :: <integer>, buffer :: <buffer>) => (tick :: <integer>);
  getter buffer-save-tick
    (buffer :: <buffer>) => (tick :: <integer>);
  setter buffer-save-tick-setter
    (tick :: <integer>, buffer :: <buffer>) => (tick :: <integer>);
  getter buffer-modified?
    (buffer :: <buffer>) => (modified? :: <boolean>);
  setter buffer-modified?-setter
    (modified? :: <boolean>, buffer :: <buffer>) => (modified? :: <boolean>);
  getter buffer-properties
    (buffer :: <buffer>) => (properties :: <sequence>);
  setter buffer-properties-setter
    (properties :: <sequence>, buffer :: <buffer>) => (properties :: <sequence>);
  getter buffer-contents-properties
    (buffer :: <buffer>) => (properties :: <sequence>);
  setter buffer-contents-properties-setter
    (properties :: <sequence>, buffer :: <buffer>) => (properties :: <sequence>);
  getter buffer-associated-buffers
    (buffer :: <buffer>) => (buffers :: <sequence>);
  setter buffer-associated-buffers-setter
    (buffers :: <sequence>, buffer :: <buffer>) => (buffers :: <sequence>);
  function note-buffer-changed
    (buffer :: <buffer>) => ();
  getter buffer-has-hard-sections?
    (buffer :: <buffer>) => (hard-sections? :: <boolean>);
  getter buffer-contains-section?
    (buffer :: <buffer>, section :: <section>) => (contains? :: <boolean>);
  getter buffer-initial-point
    (buffer :: <buffer>, #key point :: false-or(<bp>))   => (bp   :: false-or(<bp>));
  getter buffer-initial-mark
    (buffer :: <buffer>, #key mark  :: false-or(<bp>))   => (bp   :: false-or(<bp>));
  getter buffer-initial-line
    (buffer :: <buffer>, #key line  :: false-or(<line>)) => (line :: false-or(<line>));
  // Higher level stuff
  function sectionize-buffer
    (buffer :: <buffer>) => (sectionized? :: <boolean>);
  function revert-buffer
    (buffer :: <buffer>, #key buffer-filler :: false-or(<function>), major-mode)
 => (reverted? :: <boolean>);
  function save-buffer
    (buffer :: <buffer>, #key frame, editor)
 => (pathname :: false-or(<pathname>), condition);	// #f => failed to save
  function save-buffer-as
    (container-class, buffer :: <buffer>, pathname :: <pathname>,
     #key frame, editor, format, if-exists)
 => (pathname :: false-or(<pathname>), condition);	// #f => failed to save
  function kill-buffer
    (buffer :: <buffer>, #key frame, editor, no-exit-frame) => ();
  function gc-buffer
    (buffer :: <buffer>) => ();
  // Navigation
  function line-next-in-buffer
    (line :: <line>, buffer :: false-or(<buffer>), #key skip-test)
 => (next :: false-or(<line>));
  function line-previous-in-buffer
    (line :: <line>, buffer :: false-or(<buffer>), #key skip-test)
 => (prev :: false-or(<line>));
end protocol <<buffer>>;

// A buffer is used to group some set of data, and can be displayed in
// zero or more windows.  It contains a linked list of nodes which get
// created by some sort of generating function.
define open abstract primary class <basic-buffer> (<buffer>)
  sealed slot buffer-name :: <byte-string> = "",
    setter: %buffer-name-setter,
    init-keyword: name:;
  sealed slot buffer-start-node :: false-or(<basic-node>) = #f,
    init-keyword: start-node:;
  sealed slot buffer-end-node :: false-or(<basic-node>) = #f,
    init-keyword: end-node:;
  sealed slot buffer-read-only? :: <boolean> = #f,
    setter: %read-only?-setter,
    init-keyword: read-only?:;
  // Buffer modes
  sealed slot buffer-major-mode :: <major-mode>,
    required-init-keyword: major-mode:;
  sealed slot buffer-minor-modes :: <object-deque> = make(<deque>),
    init-keyword: minor-modes:;
  sealed slot buffer-major-mode-undo-list :: <list> = #();
  // The buffer is considered changed when the modification tick is greater
  // than the save tick.  They are both initialized to the same value when
  // the buffer is reverted.
  sealed slot buffer-modification-tick :: <integer> = *tick*;
  sealed slot buffer-save-tick         :: <integer> = *tick*;
  sealed slot buffer-properties :: <list> = #(),
    init-keyword: properties:;
  sealed slot buffer-contents-properties :: <list> = #();
  // All of the buffers associated with this buffer, e.g., a file buffer
  // might have a list of all the composite buffers built from its sections
  sealed slot buffer-associated-buffers :: <list> = #();
  sealed constant slot buffer-lock :: false-or(<recursive-lock>) = #f,
    init-keyword: lock:;
end class <basic-buffer>;

define method initialize
    (buffer :: <basic-buffer>, #key editor = frame-editor(*editor-frame*)) => ()
  next-method();
  let buffers = editor-buffers(editor);
  editor-buffers(editor) := add!(buffers, buffer);
end method initialize;


// The currently selected buffer in "this" editor frame
// This is always kept in sync with 'frame-buffer(*editor-frame*)'
// The type is 'false-or' for bootstrapping reasons...
define thread variable *buffer* :: false-or(<basic-buffer>) = #f;


// The default pathname to use for new file and save file dialogs
// This function exists because it's meant to return a default that
// won't cause these dialogs to explode, as would otherwise happen
// for hairy composite buffer names
define method buffer-default-pathname
    (buffer :: <basic-buffer>) => (pathname :: <pathname>)
  buffer-pathname(buffer) | buffer-name(buffer)
end method buffer-default-pathname;

define method buffer-name-setter
    (name :: <byte-string>, buffer :: <basic-buffer>)
 => (name :: <byte-string>)
  %buffer-name(buffer) := name;
  display-buffer-name-everywhere(buffer);
  name
end method buffer-name-setter;

define method buffer-modified?
    (buffer :: <basic-buffer>) => (modified? :: <boolean>)
  buffer-save-tick(buffer) < buffer-modification-tick(buffer)
end method buffer-modified?;

define method buffer-modified?-setter
    (modified? :: <boolean>, buffer :: <basic-buffer>) => (modified? :: <boolean>)
  unless (modified?)
    //---*** How do we propagate "unmodifications" to the source container?
    buffer-save-tick(buffer) := buffer-modification-tick(buffer);
    note-buffer-changed-everywhere(buffer, #f)
  end;
  modified?
end method buffer-modified?-setter;

define method note-buffer-changed
    (buffer :: <basic-buffer>) => ()
  // Avoid work if the buffer is already marked modified
  unless (buffer-modified?(buffer))
    buffer-modification-tick(buffer) := tick();
    note-buffer-changed-everywhere(buffer, #t)
  end
end method note-buffer-changed;

define method buffer-read-only?-setter
    (read-only? :: <boolean>, buffer :: <basic-buffer>) => (read-only? :: <boolean>)
  unless (buffer-read-only?(buffer) == read-only?)
    buffer.%read-only? := read-only?;
    note-buffer-read-only-everywhere(buffer, read-only?)
  end;
  read-only?
end method buffer-read-only?-setter;


// Notify every window that a buffer has changed its modification state
define method note-buffer-changed-everywhere
    (buffer :: <basic-buffer>, modified? :: <boolean>) => ()
  do-associated-windows (window :: <basic-window> = *editor-frame*)
    when (window-buffer(window) == buffer)
      window-note-buffer-changed(window, buffer, modified?)
    end
  end
end method note-buffer-changed-everywhere;

// Notify every window that a buffer has changed its read-only state
define method note-buffer-read-only-everywhere
    (buffer :: <basic-buffer>, read-only? :: <boolean>) => ()
  do-associated-windows (window :: <basic-window> = *editor-frame*)
    when (window-buffer(window) == buffer)
      window-note-buffer-read-only(window, buffer, read-only?)
    end
  end
end method note-buffer-read-only-everywhere;

// Display the name of a buffer in every window showing it
define sealed method display-buffer-name-everywhere
    (buffer :: <basic-buffer>) => ()
  do-associated-windows (window :: <basic-window> = *editor-frame*)
    when (window-buffer(window) == buffer)
      display-buffer-name(window, buffer)
    end
  end
end method display-buffer-name-everywhere;


define method buffer-section-separator-style
    (buffer :: <basic-buffer>) => (style :: <section-separator-style>)
  #"requested"
end method buffer-section-separator-style;


define sealed method interval-start-bp
    (buffer :: <basic-buffer>) => (bp :: false-or(<basic-bp>))
  let node = buffer-start-node(buffer);
  node & interval-start-bp(node)
end method interval-start-bp;

define sealed method interval-end-bp
    (buffer :: <basic-buffer>) => (bp :: false-or(<basic-bp>))
  let node = buffer-end-node(buffer);
  node & interval-end-bp(node)
end method interval-end-bp;


define method buffer-initial-point
    (buffer :: <basic-buffer>, #key point :: false-or(<basic-bp>) = #f)
 => (bp :: false-or(<basic-bp>))
  point | interval-start-bp(buffer)
end method buffer-initial-point;

define method buffer-initial-mark
    (buffer :: <basic-buffer>, #key mark  :: false-or(<basic-bp>) = #f)
 => (bp :: false-or(<basic-bp>))
  mark
end method buffer-initial-mark;

define method buffer-initial-line
    (buffer :: <basic-buffer>, #key line  :: false-or(<basic-line>) = #f)
 => (line :: false-or(<basic-line>))
  line | bp-line(buffer-initial-point(buffer))
end method buffer-initial-line;


define method revert-buffer
    (buffer :: <basic-buffer>,
     #key buffer-filler :: false-or(<function>) = #f, major-mode)
 => (reverted? :: <boolean>)
  ignore(buffer-filler, major-mode);
  error("There is no default method for 'revert-buffer'")
end method revert-buffer;

define sealed method sectionize-buffer
    (buffer :: <basic-buffer>) => (sectionized? :: <boolean>)
  // If the buffer has hard sections, those sections are definitive,
  // so don't go sectionizing it
  unless (buffer-has-hard-sections?(buffer))
    dynamic-bind (*buffer* = buffer)
      do-sectionize-buffer(buffer-major-mode(buffer), buffer)
    end
  end
end method sectionize-buffer;

define sealed method resectionize-changed-sections
    (buffer :: <basic-buffer>) => (sectionized? :: <boolean>)
  let sectionized? = #f;
  for (node = buffer-start-node(buffer) then node-next(node),
       until: ~node)
    let section = node-section(node);
    when (section)
      sectionized? := resectionize-section(section) | sectionized?
    end
  end;
  sectionized?
end method resectionize-changed-sections;

define method buffer-contains-section?
    (buffer :: <basic-buffer>, section :: <basic-section>)
 => (contains? :: <boolean>)
  block (return)
    for (node = buffer-start-node(buffer) then node-next(node),
	 until: ~node)
      when (node-section(node) == section)
	return(#t)
      end
    end;
    #f
  end
end method buffer-contains-section?;


// It's meaningless to save anything but file buffers...
define method save-buffer
    (buffer :: <basic-buffer>,
     #key frame = *editor-frame*, editor)
 => (pathname :: false-or(<pathname>), condition)
  ignore(frame, editor);
  values(#f, #f)
end method save-buffer;

// ... but "Save As" works on just about anything
define method save-buffer-as
    (container-class == <flat-file-source-container>,
     buffer :: <basic-buffer>, pathname :: <pathname>,
     #key frame = *editor-frame*, editor,
	  format, if-exists = #"signal")
 => (pathname :: false-or(<pathname>), condition)
  ignore(editor, format);
  dynamic-bind (*buffer* = buffer)
    let window = frame-window(frame);
    let pathname :: false-or(<pathname>) = pathname; // so we can set it to #f below
    let condition = #f;
    for (n :: <integer> from 0 below 2)
      block ()
	with-open-file (stream = pathname, direction: #"output", if-exists: if-exists)
	  local method dump (line :: <line>, si, ei, last?)
		  ignore(si, ei, last?);
		  //--- Uncomment this if we decide to GC lines on save
		  // gc-line(line);
		  dump-line(line, stream)
		end method;
	  do-lines(dump, buffer);
	  n := 2				// terminate the loop
	end
      exception (<file-exists-error>)
        let save?
	  = (n = 0 & window)
	    & yes-or-no-dialog(window,
			       "%s already exists.\nDo you want to overwrite it?",
			       as(<string>, pathname | "The file"));
        case
	  save? =>
	    if-exists := #"new-version";	// try again, but this time overwrite
	  n = 0 =>
	    n := 2;				// give up now
	    pathname := #f;
	  otherwise =>
	    warning-dialog(window, "Couldn't save %s.\nThe file already exists.",
			   as(<string>, pathname | "the file"));
	    pathname := #f;
	end;
      exception (c :: <condition>)
        warning-dialog(window, "Couldn't save %s.\n%s",
		       as(<string>, pathname | "the file"), condition-to-string(c));
	n := 2;
	pathname  := #f;
	condition := c;
      end
    end;
    when (pathname & file-buffer?(buffer))
      buffer-pathname(buffer) := pathname;
      container-modification-date(buffer-source-container(buffer))
	:= get-file-property(pathname, #"modification-date", default: current-date())
    end;
    values(pathname, condition)
  end
end method save-buffer-as;

// In the one-frame-per-buffer policy, we want to exit when we kill the
// buffer.  'exit-editor?' is necessary to prevent infinite recursion
// the buffer gets killed via File->Exit as opposed to File->Close.
define method kill-buffer
    (buffer :: <basic-buffer>,
     #key frame = *editor-frame*, editor,
	  no-exit-frame :: false-or(<editor-state-mixin>) = #f) => ()
  let editor = editor | frame-editor(frame);
  // Call 'exit-mode' to undo any side-effects, and revert to fundamental
  // mode in case we fail to kill the buffer
  exit-mode(buffer, buffer-major-mode(buffer));  
  enter-mode(buffer, find-mode(<fundamental-mode>));
  // Clean up all the pointers to the nodes in this buffer
  for (node = buffer-start-node(buffer) then node-next(node),
       until: ~node)
    let section = node-section(node);
    when (section)
      //--- Should this clean up 'line-bps' of all the lines in the section?
      section-nodes(section) := remove!(section-nodes(section), node);
    end
  end;
  // Remove any BP's that point into this buffer
  for (bp keyed-by register in $register-point-table)
    when (~simple-bp?(bp) & bp-buffer(bp) == buffer)
      kill-bp!(bp);
      remove-key!($register-point-table, register)
    end
  end;
  // Remove all other references to this buffer
  let buffers = remove!(editor-buffers(editor), buffer);
  do-associated-windows (window :: <basic-window> = frame)
    let selected-buffers
      = window-selected-buffers(window);
    let entry
      = find-value(selected-buffers, method (s) selection-buffer(s) == buffer end);
    when (entry)
      remove!(selected-buffers, entry)
    end;
    // If this window's buffer is the one we're killing, we must do something
    when (buffer == window-buffer(window))
      let policy = editor-policy(editor);
      if (fixed-frame-buffer?(policy))
	// One-frame-per-buffer, so exit this window now, unless it will be
	// exited on our return
	let frame = window-frame(window);
	unless (frame == no-exit-frame)
	  exit-editor(frame)
	end;
      else
	// If this window isn't going away, we need a new buffer for it
	let new-buffer
	  = if (empty?(selected-buffers))
	      // If it's the last buffer from those ever shown in this
	      // window, try a non-anonymous buffer from any old window.
	      // If there are no other buffers at all, create a new one.
	      let new-buffer
		= find-value(buffers, method (b) ~buffer-anonymous?(b) end);
	      new-buffer | make-initial-buffer(editor: editor)
	    else
	      // Otherwise, choose buffer most recently shown in this window
	      selection-buffer(selected-buffers[0])
	    end;
	select-buffer(window, new-buffer);
	queue-redisplay(window, $display-all)
      end
    end
  end
end method kill-buffer;

define method gc-buffer
    (buffer :: <basic-buffer>) => ()
  //--- Can we arrange to GC only the nodes that have been modified?
  do-lines(gc-line, buffer)
end method gc-buffer;


/// Non-file buffers

// Mixin for buffers that contain random textual/graphical data,
// but are not associated with a source container
define open abstract class <non-file-buffer-mixin> (<buffer>)
  constant slot buffer-anonymous? :: <boolean> = #f,
    init-keyword: anonymous?:;
  sealed constant slot %undo-history :: <undo-history> = make(<undo-history>);
end class <non-file-buffer-mixin>;

define method buffer-modified?
    (buffer :: <non-file-buffer-mixin>) => (modified? :: <boolean>)
  // If there's any data in the buffer, claim that it's modified,
  // irrespective of the state of the buffer ticks
  interval-start-bp(buffer) ~= interval-end-bp(buffer)
end method buffer-modified?;

define method buffer-undo-history
    (buffer :: <non-file-buffer-mixin>, #key section :: false-or(<section>))
 => (history :: false-or(<undo-history>), buffer :: false-or(<buffer>))
  ignore(section);
  values(buffer.%undo-history, buffer)
end method buffer-undo-history;

define method buffer-has-hard-sections?
    (buffer :: <non-file-buffer-mixin>) => (hard-sections? :: <boolean>)
  #f
end method buffer-has-hard-sections?;

define method revert-buffer
    (buffer :: <non-file-buffer-mixin>,
     #key buffer-filler :: false-or(<function>) = #f, major-mode)
 => (reverted? :: <boolean>)
  ignore(buffer-filler, major-mode);
  #f
end method revert-buffer;


define sealed class <non-file-buffer>
    (<non-file-buffer-mixin>, <basic-buffer>)
end class <non-file-buffer>;

define sealed domain make (singleton(<non-file-buffer>));
define sealed domain initialize (<non-file-buffer>);


define sealed class <simple-display-buffer>
    (<non-file-buffer-mixin>, <basic-buffer>)
end class <simple-display-buffer>;

define method buffer-modified?
    (buffer :: <simple-display-buffer>) => (modified? :: <boolean>)
  #f
end method buffer-modified?;

define sealed domain make (singleton(<simple-display-buffer>));
define sealed domain initialize (<simple-display-buffer>);


define variable *untitled-buffer-count* :: <integer> = 0;

define method make-empty-buffer
    (buffer-class :: subclass(<buffer>),
     #rest buffer-initargs,
     #key buffer, name,
	  major-mode    = find-mode(<fundamental-mode>),
          section-class = <section>,
          node-class    = <section-node>,
	  editor        = frame-editor(*editor-frame*),
     #all-keys)
 => (buffer :: <buffer>)
  ignore(editor);
  unless (name)
    inc!(*untitled-buffer-count*);
    name := format-to-string("Untitled %d", *untitled-buffer-count*)
  end;
  with-keywords-removed (buffer-initargs = buffer-initargs,
			 #[name:, major-mode:, section-class:, node-class:])
    let buffer = buffer
		 | apply(make, buffer-class,
			 name: name,
			 major-mode: major-mode,
			 buffer-initargs);
    let node = make-empty-section-node(buffer,
				       section-class: section-class,
				       node-class:    node-class);
    node-buffer(node)         := buffer;
    buffer-start-node(buffer) := node;
    buffer-end-node(buffer)   := node;
    buffer
  end
end method make-empty-buffer;

define function make-initial-buffer
    (#key editor = frame-editor(*editor-frame*))
 => (buffer :: <buffer>)
  let name   = "Initial buffer";
  let buffer = find-buffer(editor, name)
	       | make-empty-buffer(<non-file-buffer>, name: name, editor: editor);
  buffer
end function make-initial-buffer;
    

/// File buffers

// Mixin for buffers that contain textual/graphical data gotten by
// reading the contents of some source container.  The name is a bit
// inaccurate, but hey, I'm a dinosaur.
define open abstract class <file-buffer-mixin> (<buffer>)
  sealed slot buffer-source-container :: false-or(<source-container>) = #f,
    init-keyword: container:;
end class <file-buffer-mixin>;

define method initialize
    (buffer :: <file-buffer-mixin>, #key name) => ()
  next-method();
  unless (name)
    // Create a reasonable buffer name from the source container
    let container = buffer-source-container(buffer);
    when (container)
      buffer-name(buffer)
	:= pathname->buffer-name(container-pathname(container))
    end
  end
end method initialize;

define function pathname->buffer-name
    (pathname :: <pathname>) => (name :: <byte-string>)
  let locator = as(<file-locator>, pathname);
  let directory = locator-directory(locator);
  let name = locator-name(locator);
  if (directory)
    concatenate(name, " (", as(<string>, directory), ")")
  else
    name
  end
end function pathname->buffer-name;
  
// Buffers are non-file buffers by default
define method file-buffer?
    (buffer :: <buffer>) => (file-buffer? :: singleton(#f))
  #f
end method file-buffer?;

define sealed inline method file-buffer?
    (buffer :: <file-buffer-mixin>) => (file-buffer? :: singleton(#t))
  #t
end method file-buffer?;

// By default, if it's a file buffer, it saves out like a file buffer
define method saves-like-file-buffer?
    (buffer :: <buffer>) => (saves? :: <boolean>)
  file-buffer?(buffer)
end method saves-like-file-buffer?;


define method buffer-anonymous?
    (buffer :: <file-buffer-mixin>) => (anonymous? :: <boolean>)
  #f
end method buffer-anonymous?;


// The pathname of a file buffer is its container's pathname,
// if it has a container
define sealed method buffer-pathname
    (buffer :: <file-buffer-mixin>) => (pathname :: false-or(<pathname>))
  let container = buffer-source-container(buffer);
  container & container-pathname(container)
end method buffer-pathname;

define sealed method buffer-pathname-setter
    (pathname :: <pathname>, buffer :: <file-buffer-mixin>) => (pathname :: <pathname>)
  let container = buffer-source-container(buffer);
  when (container)
    container-pathname(container) := pathname
  end;
  buffer-name(buffer) := pathname->buffer-name(pathname);
  // Compare the current mode with the one for the new pathname
  let old-mode = buffer-major-mode(buffer);
  let new-mode = find-mode-from-pathname(pathname);
  unless (old-mode == new-mode)
    // If they're different, update the buffer and any windows showing it
    //--- Maybe this could share code with 'revert-buffer'?
    exit-mode(buffer, old-mode);
    enter-mode(buffer, new-mode);
    do-associated-windows (window :: <basic-window> = *editor-frame*)
      when (window-buffer(window) == buffer)
	window-note-mode-entered(window, new-mode);
	// Changing modes might change the display arbitrarily, so refresh
	queue-redisplay(window, $display-all)
      end
    end
  end;
  pathname
end method buffer-pathname-setter;

// Default methods just return #f...
define method buffer-pathname
    (buffer :: <buffer>) => (pathname :: singleton(#f))
  #f
end method buffer-pathname;

define method buffer-source-container
    (buffer :: <buffer>) => (container :: singleton(#f))
  #f
end method buffer-source-container;


// File buffers use the lock for the source container
define sealed inline method buffer-lock
    (buffer :: <file-buffer-mixin>) => (lock :: false-or(<recursive-lock>))
  let container = buffer-source-container(buffer);
  container & container-lock(container)
end method buffer-lock;


define sealed method buffer-undo-history
    (buffer :: <file-buffer-mixin>, #key section :: false-or(<section>))
 => (history :: false-or(<undo-history>), buffer :: false-or(<buffer>))
  ignore(section);
  let container = buffer-source-container(buffer);
  values(container & container-undo-history(container), buffer)
end method buffer-undo-history;

// A file buffer has hard sections if its source container does
define method buffer-has-hard-sections?
    (buffer :: <file-buffer-mixin>) => (hard-sections? :: <boolean>)
  let container = buffer-source-container(buffer);
  container & container-has-hard-sections?(container)
end method buffer-has-hard-sections?;

// We can do this much faster for file buffers...
define sealed method buffer-contains-section?
    (buffer :: <file-buffer-mixin>, section :: <basic-section>)
 => (contains? :: <boolean>)
  section-container(section) == buffer-source-container(buffer)
end method buffer-contains-section?;


define sealed method note-buffer-changed
    (buffer :: <file-buffer-mixin>) => ()
  // Don't do any work if the buffer is already modified...
  unless (buffer-modified?(buffer))
    when (*editor-frame*)
      let window = frame-window(*editor-frame*);
      // If the container changed on disk the first time we increment
      // the modification tick, offer to revert the buffer
      let reverted? = revert-buffer-if-necessary(buffer, window: window);
      when (reverted?)
	// Redisplay, then back to the command loop if we reverted
	redisplay-window(window);
	signal(make(<command-error>, window: window))
      end
    end;
    // The method on <basic-buffer> will increment the modification tick
    // and notify all the windows...
    next-method()
  end
end method note-buffer-changed;

define method revert-buffer-if-necessary
    (buffer :: <basic-buffer>, #key window) => (reverted? :: <boolean>)
  ignore(window);
  #f
end method revert-buffer-if-necessary;

// Offer to revert the buffer iff it has changed on disk
define method revert-buffer-if-necessary
    (buffer :: <file-buffer-mixin>, #key window) => (reverted? :: <boolean>)
  let window    = window | frame-window(*editor-frame*);
  let container = buffer-source-container(buffer);
  let pathname  = container-pathname(container);
  if (pathname & file-exists?(pathname))
    let cdate
      = container-modification-date(container);
    let fdate
      = get-file-property(pathname, #"modification-date", default: current-date());
    when (cdate & cdate ~= fdate)
      when (yes-or-no-dialog(window,
			     "%s has been modified on disk.\nDo you want to re-read it?",
			     as(<string>, pathname)))
	let line = bp->line-index(point());
	when (revert-buffer(buffer))
	  restore-previous-position(buffer, window, line);
	  queue-redisplay(window, $display-all)
	end;
	#t
      end
    end
  else
    warning-dialog(window, "%s no longer exists on disk.",
		   as(<string>, pathname));
    #f
  end
end method revert-buffer-if-necessary;


define method revert-buffer
    (buffer :: <file-buffer-mixin>,
     #key buffer-filler :: false-or(<function>) = fill-file-buffer, major-mode)
 => (reverted? :: <boolean>)
  // Exit the old mode and ensure we are in fundamental mode
  let old-mode = buffer-major-mode(buffer);
  exit-mode(buffer, old-mode);
  enter-mode(buffer, find-mode(<fundamental-mode>));
  // Reset the timestamps on the buffer
  let tick = tick();
  buffer-modification-tick(buffer) := tick;
  buffer-save-tick(buffer) := tick;
  buffer-contents-properties(buffer) := #();
  // Reset the buffer's (i.e., the container's) undo history
  let history = buffer-undo-history(buffer);
  when (history)
    reset-undo-history(history)
  end;
  // Now go read the contents of the buffer
  when (buffer-filler)
    buffer-filler(buffer)
  end;
  let container = buffer-source-container(buffer);
  buffer-read-only?(buffer) := container-read-only?(container);
  // Determine the major mode of the buffer, and go sectionize it
  let mode = major-mode | find-mode-from-pathname(container-pathname(container));
  enter-mode(buffer, mode);
  do-associated-windows (window :: <basic-window> = *editor-frame*)
    when (window-buffer(window) == buffer)
      unless (mode == old-mode)
	window-note-mode-entered(window, mode)
      end;
      window-note-buffer-selected(window, buffer)
    end
  end;
  do-sectionize-buffer(mode, buffer);
  #t
end method revert-buffer;

define method fill-file-buffer
    (buffer :: <file-buffer-mixin>) => ()
  read-container-contents(buffer-source-container(buffer), buffer)
end method fill-file-buffer;

// Save the buffer to its home source container, first renaming the
// existing file to a backup pathname
define method save-buffer
    (buffer :: <file-buffer-mixin>,
     #key frame = *editor-frame*, editor)
 => (pathname :: false-or(<pathname>), condition)
  ignore(editor);
  let container = buffer-source-container(buffer);
  let pathname  = container-pathname(container);
  let namestring :: <string> = as(<string>, pathname);
  let cdate
    = container-modification-date(container);
  let fdate
    = get-file-property(pathname, #"modification-date", default: current-date());
  // Throw back to the command loop if the user declines to save.
  // (We mention the full pathname instead of the basic name in case this
  // happens in the middle of "save-all".)
  when (cdate & cdate ~= fdate)
    let window = frame-window(frame);
    unless (yes-or-no-dialog(window,
			     "%s has been modified on disk.\nDo you want to save it anyway?",
			     as(<string>, pathname)))
      signal(make(<command-error>, window: window))
    end
  end;
  // Try to rename the original file to the backup pathname,
  // ignoring any file system error we might encounter
  let backup :: false-or(<string>) = backup-file-namestring(pathname);
  when (backup)
    block ()
      rename-file(namestring, backup, if-exists: #"replace")
    exception (<file-system-error>)
      #f
    end
  end;
  // Finally, write out the contents of the buffer
  let (pathname, condition)
    = save-buffer-as(source-container-class(pathname), buffer, pathname,
		     frame: frame,
		     if-exists: #"new-version");
  when (pathname)
    buffer-modified?(buffer) := #f
  end;
  values(pathname, condition)
end method save-buffer;

//---*** Locators should provide a portable way to generate backup pathnames
define function backup-file-namestring
    (pathname :: <pathname>) => (backup-name :: false-or(<string>))
  concatenate(as(<string>, pathname), "~")
end function backup-file-namestring;

define method kill-buffer
    (buffer :: <file-buffer-mixin>,
     #key frame = *editor-frame*, editor, no-exit-frame) => ()
  ignore(frame, editor, no-exit-frame);
  next-method();
  let container = buffer-source-container(buffer);
  when (container)
    container-buffers(container)
      := remove!(container-buffers(container), buffer)
  end
end method kill-buffer;


define sealed class <file-buffer>
    (<file-buffer-mixin>, <basic-buffer>)
end class <file-buffer>;

define sealed domain make (singleton(<file-buffer>));
define sealed domain initialize (<file-buffer>);


/// Special purpose buffers

// Mixin for buffers that contain "special purpose" data, e.g.,
// a "List Callers", "Methods", or "Shell" buffer
define open abstract class <special-purpose-buffer-mixin> (<buffer>)
  sealed slot buffer-modified? :: <boolean> = #f;
  sealed slot buffer-anonymous? :: <boolean> = #f,
    init-keyword: anonymous?:;
  sealed constant slot %undo-history :: false-or(<undo-history>) = #f,
    init-keyword: undo-history:;
end class <special-purpose-buffer-mixin>;

// By default, buffers are not special-purpose
define method special-purpose-buffer?
    (buffer :: <buffer>) => (special-purpose? :: singleton(#f))
  #f
end method special-purpose-buffer?;

define sealed inline method special-purpose-buffer?
    (buffer :: <special-purpose-buffer-mixin>) => (special-purpose? :: singleton(#t))
  #t
end method special-purpose-buffer?;


define method buffer-undo-history
    (buffer :: <special-purpose-buffer-mixin>, #key section :: false-or(<section>))
 => (history :: false-or(<undo-history>), buffer :: false-or(<buffer>))
  let history = buffer.%undo-history;
  if (history)
    values(history, buffer)
  else
    // This is where things get a bit wierd!  We return the history
    // for the container and its home buffer as well.  This is so that
    // 'undo!' and 'redo!' can establish the right binding for *buffer*,
    // so that temporary BPs in change records don't end up getting an
    // unexpected value for 'bp-buffer'.  Yow!
    let container = section   & buffer-has-hard-sections?(buffer) & section-container(section);
    let history   = container & container-undo-history(container);
    let buffer    = history   & section-home-buffer(section);
    values(history, buffer)
  end
end method buffer-undo-history;

// By default, special purpose buffers have hard sections
define method buffer-has-hard-sections?
    (buffer :: <special-purpose-buffer-mixin>) => (hard-sections? :: <boolean>)
  #t
end method buffer-has-hard-sections?;


define method note-buffer-changed
    (buffer :: <special-purpose-buffer-mixin>) => ()
  buffer-modified?(buffer) := #t;
  next-method()
end method note-buffer-changed;


define open abstract class <basic-special-purpose-buffer>
    (<special-purpose-buffer-mixin>, <basic-buffer>)
end class <basic-special-purpose-buffer>;


/// Buffer coercions

define method as-file-buffer
    (class :: subclass(<file-buffer-mixin>),
     buffer :: <file-buffer-mixin>, pathname :: <pathname>, editor :: <editor>)
 => (file-buffer :: <file-buffer-mixin>)
  buffer
end method as-file-buffer;

define method as-file-buffer
    (class :: subclass(<file-buffer-mixin>),
     buffer :: <non-file-buffer-mixin>, pathname :: <pathname>, editor :: <editor>)
 => (file-buffer :: <file-buffer-mixin>)
  let container :: <source-container>
    = find-source-container(editor, source-container-class(pathname), pathname);
  let new-buffer :: <file-buffer>
    = make-empty-buffer(<file-buffer>,
			name:       pathname->buffer-name(pathname),
			major-mode: find-mode(<fundamental-mode>),
			container:  container,
			editor:     editor);
  local method move-lines (new-buffer :: <basic-buffer>)
	  // There's one node and one section in the new buffer --
	  // fill it in from the old buffer
	  let new-node    = buffer-start-node(new-buffer);
	  let new-section = node-section(buffer-start-node(new-buffer));
	  // First move the lines from the old buffer
	  let first-line :: <basic-line> = bp-line(interval-start-bp(buffer));
	  let last-line  :: <basic-line> = bp-line(interval-end-bp(buffer));
	  let line :: false-or(<basic-line>) = first-line;
	  let next :: false-or(<basic-line>) = #f;
	  while (line)
	    next := line-next-in-buffer(line, buffer);
	    line-section(line) := new-section;
	    line := next
	  end;
	  // Then fix up the new buffer's node and section
	  section-start-line(new-section) := first-line;
	  section-end-line(new-section)   := last-line;
	  interval-start-bp(new-node)
	    := make(<bp>,
		    line: first-line, index: 0, buffer: new-buffer);
	  interval-end-bp(new-node)
	    := make(<bp>,
		    line: last-line, index: line-length(last-line), buffer: new-buffer,
		    moving?: #t)
	end method;
  revert-buffer(new-buffer, buffer-filler: move-lines);
  new-buffer
end method as-file-buffer;


/// Navigation within buffers

define method do-lines
    (function :: <function>, buffer :: <basic-buffer>,
     #key from-end? = #f, skip-test = line-for-display-only?) => ()
  let (start-node, end-node, step :: <function>)
    = if (from-end?)
	values(buffer-end-node(buffer), buffer-start-node(buffer), node-previous)
      else
	values(buffer-start-node(buffer), buffer-end-node(buffer), node-next)
      end;
  block (break)
    for (node = start-node then step(node))
      when (node)
	do-lines(function, node, from-end?: from-end?, skip-test: skip-test)
      end;
      when (~node | node == end-node)
	break()
      end
    end
  end
end method do-lines;

define method count-lines
    (buffer :: <basic-buffer>,
     #key skip-test = line-for-display-only?, cache-result? = #f)
 => (nlines :: <integer>)
  let start-node = buffer-start-node(buffer);
  let end-node   = buffer-end-node(buffer);
  let n :: <integer> = 0;
  block (break)
    for (node = start-node then node-next(node))
      inc!(n, count-lines(node,
			  skip-test: skip-test, cache-result?: cache-result?));
      when (node == end-node)
	break()
      end
    end
  end;
  n
end method count-lines;

define method as
    (class :: subclass(<string>), buffer :: <basic-buffer>)
 => (string :: <byte-string>)
  let bp1 = interval-start-bp(buffer);
  let bp2 = interval-end-bp(buffer);
  if (bp1 & bp2)
    as(<byte-string>, make-interval(bp1, bp2, in-order?: #t))
  else
    ""
  end
end method as;


/// Line navigation within buffers

// 'line-next' returns #f for the last line in a section, so you need to
// use 'line-next-in-buffer' to navigate through the lines of a buffer.
// If a skip-test is supplied, any lines satisfying that test are skipped.
define method line-next-in-buffer
    (line :: <basic-line>, buffer :: false-or(<basic-buffer>),
     #key skip-test = line-for-display-only?)
 => (next :: false-or(<basic-line>))
  let next-line
    = line-next(line)
      | when (buffer)
	  let node = line-node(line, buffer: buffer);
	  let next-node = node & node-next(node);
	  next-node & bp-line(interval-start-bp(next-node))
	end;
  if (next-line & skip-test & skip-test(next-line))
    line-next-in-buffer(next-line, buffer, skip-test: skip-test)
  else
    next-line
  end
end method line-next-in-buffer;

// Same deal as 'line-next-in-buffer'
define method line-previous-in-buffer
    (line :: <basic-line>, buffer :: false-or(<basic-buffer>),
     #key skip-test = line-for-display-only?)
 => (previous :: false-or(<basic-line>))
  let prev-line
    = line-previous(line)
      | when (buffer)
	  let node = line-node(line, buffer: buffer);
	  let prev-node = node & node-previous(node);
	  prev-node & bp-line(interval-end-bp(prev-node))
	end;
  if (prev-line & skip-test & skip-test(prev-line))
    line-previous-in-buffer(prev-line, buffer, skip-test: skip-test)
  else
    prev-line
  end
end method line-previous-in-buffer;


// Returns #t iff line1 is before line2 in the buffer
define method line-less?
    (buffer :: <basic-buffer>,
     line1 :: <basic-line>, line2 :: <basic-line>)
 => (less? :: <boolean>)
  block (return)
    let section1 :: false-or(<basic-section>) = line-section(line1);
    let section2 :: false-or(<basic-section>) = line-section(line2);
    case
      section1 == section2 =>
	// This catches the case where both sections are #f
	//--- Note that this might be a bad idea!
	for (line = line1 then line-next(line),
	     until: ~line)
	  when (line == line2)
	    return(#t)
	  end
	end;
	#f;
      section1 & section2 =>
	let section1 :: <basic-section> = section1;	// force tighter type...
	let section2 :: <basic-section> = section2;	// force tighter type...
	// line1 precedes line2 if its section precedes line2's
	// section in the buffer
	section-less?(buffer, section1, section2);
      otherwise =>
	assert(section1 & section2,
	       "The lines %= and %= are in completely unrelated sections in 'line-less?'",
	       line1, line2);
    end
  end
end method line-less?;

// Returns #t iff section1 is before section2 in the buffer
define method section-less?
    (buffer :: <basic-buffer>,
     section1 :: <basic-section>, section2 :: <basic-section>)
 => (less? :: <boolean>)
  block (return)
    for (node = section-node(section1, buffer: buffer) then node-next(node),
	 until: ~node)
      when (node-section(node) == section2)
	return(#t)
      end
    end;
    #f
  end
end method section-less?;


// Given a section and a buffer, return the section's node within the buffer
// Returns #f if the section is not in the buffer
define method section-node
    (section :: <basic-section>, #key buffer = *buffer*)
 => (node :: false-or(<basic-node>))
  block (return)
    for (node :: <basic-node> in section-nodes(section))
      when (node-buffer(node) == buffer)
	return(node)
      end
    end;
    #f
  end
end method section-node;

define method line-node
    (line :: <basic-line>, #key buffer = *buffer*)
 => (node :: false-or(<basic-node>))
  let section = line-section(line);
  section & section-node(section, buffer: buffer)
end method line-node;

define method bp-node
    (bp :: <basic-bp>) => (node :: false-or(<basic-node>))
  line-node(bp-line(bp), buffer: bp-buffer(bp))
end method bp-node;


/// Adding and removing nodes to buffers

define sealed method add-node!
    (buffer :: <basic-buffer>, node :: <basic-node>, 
     #key after :: type-union(<basic-node>, one-of(#f, #"start", #"end")) = #"end") => ()
  assert(~node-buffer(node),
	 "The node %= is already in the buffer %=", node, node-buffer(node));
  let section = node-section(node);
  when (section)
    for (n :: <basic-node> in section-nodes(section))
      assert(n == node | node-buffer(n) ~== buffer,
	     "The section for node %= is already in the buffer %=", node, node-buffer(node))
    end
  end;
  let (next, prev)
    = select (after)
	#f, #"start" =>
	  values(buffer-start-node(buffer), #f);
	#"end" =>
	  values(#f, buffer-end-node(buffer));
	otherwise =>
	  assert(node-buffer(after) == buffer,
		 "The 'after' node %= is not in the buffer %=", after, buffer);
	  values(node-next(after), after);
      end;
  node-buffer(node)   := buffer;
  node-next(node)     := next;
  node-previous(node) := prev;
  if (next)
    node-previous(next) := node
  else
    buffer-end-node(buffer) := node
  end;
  if (prev)
    node-next(prev) := node
  else
    buffer-start-node(buffer) := node
  end;
  update-buffer-line-count(buffer, node)
end method add-node!;

define sealed method remove-node!
    (buffer :: <basic-buffer>, node :: <basic-node>) => ()
  assert(node-buffer(node) == buffer,
	 "The node %= is not in the buffer %=", node, buffer);
  let (next, prev)
    = values(node-next(node), node-previous(node));
  if (next)
    node-previous(next) := prev
  else
    buffer-end-node(buffer) := prev
  end;
  if (prev)
    node-next(prev) := next
  else
    buffer-start-node(buffer) := next
  end;
  node-buffer(node)   := #f;
  node-next(node)     := #f;
  node-previous(node) := #f;
  update-buffer-line-count(buffer, node)
end method remove-node!;

define sealed method update-buffer-line-count
    (buffer :: <basic-buffer>, node :: <basic-node>) => ()
  // Update every window which has a buffer containing this node
  do-associated-windows (window :: <basic-window> = *editor-frame*)
    when (window-buffer(window) = buffer)
      // We cache section line counts, so throwing info away won't really be slow
      window-total-lines(window) := #f
    end
  end
end method update-buffer-line-count;


/// Mode syntax tables

define sealed inline method word-syntax-table
    (buffer :: <basic-buffer>) => (table :: <syntax-table>)
  word-syntax-table(buffer-major-mode(buffer))
end method word-syntax-table;

define inline function word-syntax
    (char :: <byte-character>) => (syntax :: <integer>)
  character-syntax(char, word-syntax-table(buffer-major-mode(*buffer*)))
end function word-syntax;

define sealed inline method atom-syntax-table
    (buffer :: <basic-buffer>) => (table :: <syntax-table>)
  atom-syntax-table(buffer-major-mode(buffer))
end method atom-syntax-table;

define inline function atom-syntax
    (char :: <byte-character>) => (syntax :: <integer>)
  character-syntax(char, atom-syntax-table(buffer-major-mode(*buffer*)))
end function atom-syntax;

define sealed inline method list-syntax-table
    (buffer :: <basic-buffer>) => (table :: <syntax-table>)
  list-syntax-table(buffer-major-mode(buffer))
end method list-syntax-table;

define inline function list-syntax
    (char :: <byte-character>) => (syntax :: <integer>)
  character-syntax(char, list-syntax-table(buffer-major-mode(*buffer*)))
end function list-syntax;
