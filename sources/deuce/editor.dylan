Module:       deuce-internals
Synopsis:     The Deuce editor
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Editors

define protocol <<editor>> ()
  getter editor-frames
    (editor :: <editor>) => (frames :: <stretchy-sequence>);
  setter editor-frames-setter
    (frames :: <stretchy-sequence>, editor :: <editor>)
 => (frames :: <stretchy-sequence>);
  getter editor-windows
    (editor :: <editor>) => (windows :: <stretchy-sequence>);
  setter editor-windows-setter
    (windows :: <stretchy-sequence>, editor :: <editor>)
 => (windows :: <stretchy-sequence>);
  getter editor-buffers
    (editor :: <editor>) => (buffers :: <stretchy-sequence>);
  setter editor-buffers-setter
    (buffers :: <stretchy-sequence>, editor :: <editor>)
 => (buffers :: <stretchy-sequence>);
  getter editor-source-containers
    (editor :: <editor>) => (containers :: <stretchy-collection>);
  setter editor-source-containers-setter
    (containers :: <stretchy-collection>, editor :: <editor>)
 => (containers :: <stretchy-collection>);
  getter editor-kill-history
    (editor :: <editor>) => (history :: <kill-history>);
  getter editor-lock
    (editor :: <editor>) => (lock :: <recursive-lock>);
end protocol <<editor>>;

// An <editor> is a class that maintains the global state for a set of
// editor frames.  For example, all the buffers managed by an editor have
// their redisplay tracked in all of the windows managed by the same editor.
// Deuce back-ends are expected to supply a concrete implementation class
// that is a subclass of <basic-editor>.
define open abstract class <basic-editor> (<editor>)
  // Stretchy vectors are thread-safe, so we don't need to lock them
  sealed slot editor-frames  :: <stretchy-object-vector> = make(<stretchy-vector>),
    init-keyword: frames:;
  sealed slot editor-windows :: <stretchy-object-vector> = make(<stretchy-vector>),
    init-keyword: windows:;
  sealed slot editor-buffers :: <stretchy-object-vector> = make(<stretchy-vector>),
    init-keyword: buffers:;
  //--- Should this be a case-insensitive string table?
  sealed slot editor-source-containers :: <string-table> = make(<string-table>),
    init-keyword: containers:;
  sealed slot editor-kill-history :: <kill-history> = make(<kill-history>),
    init-keyword: kill-history:;
  sealed slot editor-policy :: <editor-policy> = copy-policy($default-editor-policy),
    init-keyword: policy:;
  sealed constant slot editor-lock :: <recursive-lock> = make(<recursive-lock>);
  // Cached tables for Boyer-Moore searching, shared by all frames
  sealed slot editor-search-string  :: false-or(<byte-string>) = #f,
    setter: %search-string-setter;
  sealed slot editor-replace-string  :: false-or(<byte-string>) = #f;
  sealed slot editor-reverse-search?        :: <boolean> = #f;
  sealed slot editor-case-sensitive-search? :: <boolean> = #f;
  sealed slot editor-whole-word-search?     :: <boolean> = #f;
  sealed slot editor-skip-table         :: false-or(<vector>) = #f;
  sealed slot editor-reoccurrence-table :: false-or(<vector>) = #f;
end class <basic-editor>;

define sealed class <simple-editor> (<basic-editor>)
end class <simple-editor>;

define sealed domain make (singleton(<simple-editor>));
define sealed domain initialize (<simple-editor>);

define sealed method editor-search-string-setter
    (search-string :: false-or(<byte-string>), editor :: <basic-editor>)
 => (search-string :: false-or(<byte-string>))
  unless (search-string = editor-search-string(editor))
    // Invalidate the Boyer-Moore search caches, but don't compute them
    // until we need them
    editor.%search-string := search-string;
    editor-skip-table(editor) := #f;
    editor-reoccurrence-table(editor) := #f;
    for (frame :: <editor-state-mixin> in editor-frames(editor))
      frame-search-string-found?(frame) := #f
    end
  end
end method editor-search-string-setter;


define method find-buffer
    (editor :: <basic-editor>, name :: <byte-string>)
 => (buffer :: false-or(<basic-buffer>))
  find-buffer(editor, method (b) buffer-name(b) = name end)
end method find-buffer;

define method find-buffer
    (editor :: <basic-editor>, test-function :: <function>)
  find-value(editor-buffers(editor), test-function)
end method find-buffer;

define method find-buffer-from-pathname
    (editor :: <basic-editor>, pathname :: <pathname>)
 => (buffer :: false-or(<basic-buffer>))
  let locator = as(<file-locator>, pathname);
  find-buffer(editor, rcurry(buffer-source-location-equals, locator))
end method find-buffer-from-pathname;

define method buffer-source-location-equals
    (buffer :: <buffer>, locator :: <file-locator>)
  #f
end method buffer-source-location-equals;

define method buffer-source-location-equals
    (buffer :: <file-buffer-mixin>, locator :: <file-locator>)
  let buffer-pathname = container-pathname(buffer-source-container(buffer));
  locator = as(<file-locator>, buffer-pathname)
end method buffer-source-location-equals;


/// Editor frames

define constant <command-type>
    = one-of(#"cancel", #"number",
	     #"motion", #"line-motion", #"mark", #"scroll", #"display",
	     #"insert", #"delete", #"kill", #"yank", #"yank-no-motion",
	     #"undo", #"redo", #"file", #"compile", #"shell", #"macro",
	     #"complete", #"dynamic-complete", #"browse",
	     #"mail", #"version-control", #f);

define constant <argument-state>
    = one-of(#"digits", #"sign", #"universal", #"universal-digits", #"universal-sign", #f);

define constant <incremental-search-direction>
    = one-of(#"forward", #"backward", #f);

// Backend classes can add methods to this to be notified of a
// change in the search state.  The value is the buffer in which the
// search string was found.
define open generic frame-search-string-found?-setter
    (found? :: false-or(<buffer>), frame :: <editor-frame>)
 => (found? :: false-or(<buffer>));

// This class is separated out from <basic-editor-frame> because
// we sometimes want embedded panes to maintain all this stuff themselves
define open abstract class <editor-state-mixin> (<editor-frame>)
  // Point back to the editor the owns this frame
  sealed slot frame-editor :: <basic-editor> = $null-editor,
    init-keyword: editor:;
  // The current buffer and window
  sealed slot frame-buffer :: false-or(<basic-buffer>) = #f,
    init-keyword: buffer:;
  sealed slot frame-window :: false-or(<basic-window>) = #f,
    init-keyword: window:;
  // The current command table, installed by the buffer's major mode
  sealed slot frame-command-set :: <command-set> = $standard-command-set,
    setter: %command-set-setter,
    init-keyword: command-set:;
  // State for reading of prefixed commands, e.g., c-X c-F
  sealed slot frame-command-state :: <command-table>
    = standard-command-table($standard-command-set);
  sealed slot frame-command = #f;
  sealed slot frame-command-character = #f;
  sealed slot frame-command-modifiers :: <integer> = 0;
  sealed slot frame-last-command = #f;
  sealed slot frame-last-command-type :: <command-type> = #f;
  // Command hooks
  sealed slot frame-before-command-hooks = #();
  sealed slot frame-after-command-hooks  = #();
  // Accumulated argument state for commands
  sealed slot frame-numeric-arg :: <integer> = 1;
  sealed slot frame-numeric-arg-state :: <argument-state> = #f;
  // Current keyboard macro, if any
  sealed slot frame-keyboard-macro :: false-or(<keyboard-macro>) = #f;
  // Incremental search state
  sealed slot frame-isearch-trail       :: <list> = #();
  sealed slot frame-isearch-direction   :: <incremental-search-direction> = #f;
  sealed slot frame-isearch-move-mark?  :: <boolean> = #f;
  sealed slot frame-isearch-last-string :: false-or(<byte-string>) = #f;
  // Search&Replace state
  slot frame-search-string-found? :: false-or(<buffer>) = #f;	// _not_ sealed
  // Dynamic completion state
  sealed slot frame-dynamic-completion-state = #f;
end class <editor-state-mixin>;

define method initialize
    (frame :: <editor-state-mixin>, #key) => ()
  next-method();
  let frames = editor-frames(frame-editor(frame));
  add!(frames, frame);
  frame-command-state(frame) := standard-command-table(frame-command-set(frame))
end method initialize;

define sealed method frame-command-set-setter
    (command-set :: <command-set>, frame :: <editor-state-mixin>)
 => (command-set :: <command-set>)
  frame.%command-set := command-set;
  frame-command-state(frame) := standard-command-table(command-set);
  command-set
end method frame-command-set-setter;


// An editor frame is a single editor with a current buffer, displaying in
// a particular window.  The current buffer is one of the buffers managed
// by the owning <editor>.
// Deuce back-ends are expected to supply a concrete implementation class
// that is a subclass of <basic-editor-frame>.
define open abstract class <basic-editor-frame>
    (<editor-state-mixin>)
end class <basic-editor-frame>;

define sealed class <simple-editor-frame> (<basic-editor-frame>)
end class <simple-editor-frame>;

define sealed domain make (singleton(<simple-editor-frame>));
define sealed domain initialize (<simple-editor-frame>);


// NB: this may queue an exit event, and therefore not necessarily exit immediately
define open generic exit-editor (frame :: <editor-frame>) => ();

// Default method is a no-op
// In particular, embedded Deuce gadgets want this no-op behavior
define method exit-editor (frame :: <editor-frame>) => ()
  #f
end method exit-editor;


// The editor frame for "this" command loop
// The type is 'false-or' for bootstrapping reasons...
define thread variable *editor-frame* :: false-or(<editor-state-mixin>) = #f;
