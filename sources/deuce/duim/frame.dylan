Module:       duim-deuce-internals
Synopsis:     DUIM back-end for Deuce
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Command enabling and disabling

define sealed method deuce/command-enabled?
    (window :: <deuce-pane>, command :: <function>)
 => (enabled? :: <boolean>)
  let frame = sheet-frame(window);
  when (frame)
    command-enabled?(command, frame)
  end
end method deuce/command-enabled?;

define sealed method deuce/command-enabled?-setter
    (enabled? :: <boolean>, window :: <deuce-pane>, command :: <function>)
 => (enabled? :: <boolean>)
  let frame = sheet-frame(window);
  when (frame)
    command-enabled?(command, frame) := enabled?
  end
end method deuce/command-enabled?-setter;


/// Status bar

// Indices into the status bar
// define constant $message-label-index :: <integer> = 0;
define constant $mode-label-index       :: <integer> = 1;
define constant $modified?-label-index  :: <integer> = 2;
define constant $read-only?-label-index :: <integer> = 3;

// Documentation/ToolTips for status bar fields
define constant $doc-message-label    :: <byte-string>	= "Message area";
define constant $doc-mode-label       :: <byte-string>	= "Major mode";
define constant $doc-modified?-label  :: <byte-string>	= "Buffer modified?";
define constant $doc-read-only?-label :: <byte-string>	= "Buffer read-only?";

// Content strings for status bar fields
define constant $modified?-label-Mod   :: <byte-string>	= "Mod";
define constant $modified?-label-unMod :: <byte-string>	= "   ";
define constant $read-only?-label-RW   :: <byte-string>	= "R/W";
define constant $read-only?-label-RO   :: <byte-string>	= "R-O";

define method make-deuce-status-bar
    (frame :: <basic-editor-frame>) => (status-bar :: <status-bar>)
  let framem = frame-manager(frame);
  with-frame-manager (framem)
    let message-area
      = make(<label>, 
	     label: "",
	     width: 100, min-width: 100, max-width: $fill,
	     documentation: $doc-message-label);
    make(<status-bar>,
	 label-pane: message-area,
	 children:
	   vector(message-area,
		  make(<label>,		// major mode field
		       label: "",
		       space-requirement: make(<space-requirement>,
					       label: "Fundamental", min-width: 100),
		       documentation: $doc-mode-label),
		  make(<label>,		// modified field
		       label: $modified?-label-unMod,
		       space-requirement: make(<space-requirement>,
					       label: $modified?-label-Mod, min-width: 30),
		       documentation: $doc-modified?-label),
		  make(<label>,		// read-only field
		       label: $read-only?-label-RW,
		       space-requirement: make(<space-requirement>,
					       label: $read-only?-label-RW, min-width: 30),
		       documentation: $doc-read-only?-label)))
  end
end method make-deuce-status-bar;

define method deuce-frame-show-mode
    (frame :: <editor-state-mixin>, mode :: <major-mode>) => ()
  let string = mode-name(mode);
  deuce-frame-status-bar-change-label(frame, $mode-label-index, string)
end method deuce-frame-show-mode;

define method deuce-frame-show-modified?
  (frame :: <editor-state-mixin>, modified? :: <boolean>) => ()
  let string = if (modified?) $modified?-label-Mod else $modified?-label-unMod end;
  deuce-frame-status-bar-change-label(frame, $modified?-label-index, string)
end method deuce-frame-show-modified?;

define method deuce-frame-show-read-only?
    (frame :: <editor-state-mixin>, read-only? :: <boolean>) => ()
  let string = if (read-only?) $read-only?-label-RO else $read-only?-label-RW end;
  deuce-frame-status-bar-change-label(frame, $read-only?-label-index, string)
end method deuce-frame-show-read-only?;

// This is here for Deuce gadgets, which have no status bars
define method deuce-frame-status-bar-change-label
    (frame :: <editor-state-mixin>, position :: <integer>, string :: <string>) => ()
  #f
end method deuce-frame-status-bar-change-label;

define method deuce-frame-status-bar-change-label
    (frame :: <basic-editor-frame>, position :: <integer>, string :: <string>) => ()
  let status-bar = frame-status-bar(frame);
  when (status-bar)
    let label = element(sheet-children(status-bar), position, default: #f);
    when (label)
      gadget-label(label) := string
    end
  end;
  string
end method deuce-frame-status-bar-change-label;


/// Notifications

define method window-note-mode-entered
    (window :: <deuce-pane>, mode :: <major-mode>) => ()
  next-method();
  deuce-frame-show-mode(window-frame(window), mode)
end method window-note-mode-entered;

define method window-note-mode-entered
    (window :: <deuce-pane>, mode :: <fundamental-mode>) => ()
  next-method();
  // Squeeze out a little more speed by not using 'deuce/command-enabled?'
  let frame = sheet-frame(window);
  when (frame)
    command-enabled?(evaluate-definition, frame)   := #f;
    command-enabled?(evaluate-region, frame)	   := #f;
    command-enabled?(evaluate-buffer, frame)	   := #f;
    command-enabled?(macroexpand-region, frame)	   := #f;
    command-enabled?(parse-project, frame)	   := #f;
    command-enabled?(compile-project, frame)	   := #f;
    command-enabled?(clean-compile-project, frame) := #f;
    command-enabled?(build-project, frame)	   := #f;
    command-enabled?(clean-build-project, frame)   := #f;
    command-enabled?(link-project, frame)	   := #f;
    command-enabled?(compile-file, frame)	   := #f;
    command-enabled?(load-file, frame)		   := #f;
    command-enabled?(describe-object, frame)	   := #f;
    command-enabled?(browse-object, frame)	   := #f;
    command-enabled?(browse-class, frame)	   := #f;
    command-enabled?(browse-function, frame)	   := #f;
    command-enabled?(show-arglist, frame)	   := #f;
    command-enabled?(show-documentation, frame)	   := #f
  end
end method window-note-mode-entered;

define method window-note-mode-entered
    (window :: <deuce-pane>, mode :: <dylan-mode>) => ()
  next-method();
  // Squeeze out a little more speed by not using 'deuce/command-enabled?'
  let frame = sheet-frame(window);
  when (frame)
    command-enabled?(evaluate-definition, frame)   := #t;
    command-enabled?(evaluate-region, frame)	   := #t;
    command-enabled?(evaluate-buffer, frame)	   := #t;
    command-enabled?(macroexpand-region, frame)	   := #t;
    command-enabled?(parse-project, frame)	   := #t;
    command-enabled?(compile-project, frame)	   := #t;
    command-enabled?(clean-compile-project, frame) := #t;
    command-enabled?(build-project, frame)	   := #t;
    command-enabled?(clean-build-project, frame)   := #t;
    command-enabled?(link-project, frame)	   := #t;
    command-enabled?(compile-file, frame)	   := #t;
    command-enabled?(load-file, frame)		   := #t;
    command-enabled?(describe-object, frame)	   := #t;
    command-enabled?(browse-object, frame)	   := #t;
    command-enabled?(browse-class, frame)	   := #t;
    command-enabled?(browse-function, frame)	   := #t;
    command-enabled?(show-arglist, frame)	   := #t;
    command-enabled?(show-documentation, frame)	   := #t
  end
end method window-note-mode-entered;


define method window-note-buffer-changed
    (window :: <deuce-pane>, buffer :: <basic-buffer>, modified? :: <boolean>) => ()
  next-method();
  deuce-frame-show-modified?(window-frame(window), modified?);
  let frame = sheet-frame(window);	// ~= window-frame for <deuce-gadget>
  when (frame)
    command-enabled?(save-file,    frame) := #t;
    command-enabled?(save-file-as, frame) := #t
  end
end method window-note-buffer-changed;


define method window-note-buffer-read-only
    (window :: <deuce-pane>, buffer :: <basic-buffer>, read-only? :: <boolean>) => ()
  next-method();
  deuce-frame-show-read-only?(window-frame(window), read-only?);
  let frame = sheet-frame(window);	// ~= window-frame for <deuce-gadget>
  when (frame)
    command-enabled?(save-file,  frame) := ~read-only?;
    command-enabled?(cut-region, frame) := ~read-only?;
    command-enabled?(paste, frame)      := ~read-only?
  end
end method window-note-buffer-read-only;


define method window-note-buffer-selected
    (window :: <deuce-pane>, buffer :: <basic-buffer>) => ()
  next-method();
  // A place-holder in case we need to do anything else...
  // The default method already calls 'window-note-buffer-changed',
  // then 'window-note-buffer-read-only'
end method window-note-buffer-selected;

define method window-note-buffer-selected
    (window :: <deuce-pane>, buffer == #f) => ()
  next-method();
  deuce-frame-show-modified?(window-frame(window), #f);
  deuce-frame-show-read-only?(window-frame(window), #t);
  let frame = sheet-frame(window);	// ~= window-frame for <deuce-gadget>
  when (frame)
    command-enabled?(save-file, frame)    := #f;
    command-enabled?(save-file-as, frame) := #f;
    command-enabled?(cut-region, frame)   := #f;
    command-enabled?(paste, frame)        := #f
  end
end method window-note-buffer-selected;


define method window-note-selection-changed
    (window :: <deuce-pane>, mark :: false-or(<basic-bp>)) => ()
  next-method();
  // Note that sheet-frame ~= window-frame for <deuce-gadget>
  let frame  = sheet-frame(window);
  when (frame)
    let editor     = frame-editor(window-frame(window));
    let policy     = editor-policy(editor);
    let mark?      = (mark & #t);
    let copy-line? = (unselected-copy-policy(policy) == #"copy-line");
    command-enabled?(cut-region, frame)  := mark?;
    command-enabled?(copy-region, frame) := mark? | copy-line?
  end
end method window-note-selection-changed;


define method window-note-search-string
    (window :: <deuce-pane>, string :: false-or(<string>)) => ()
  next-method();
  let frame = sheet-frame(window);
  when (frame)
    let editor = frame-editor(window-frame(window));
    let policy = editor-policy(editor);
    let emacs? = (command-set-policy(policy) == #"emacs");
    command-enabled?(find-next-string, frame)     := emacs? | (string & #t);
    command-enabled?(find-previous-string, frame) := emacs? | (string & #t)
  end
end method window-note-search-string;


define method window-note-undo/redo
    (window :: <deuce-pane>, undo? :: <boolean>, redo? :: <boolean>) => ()
  next-method();
  let frame = sheet-frame(window);
  when (frame)
    command-enabled?(deuce/undo-command, frame) := undo?;
    command-enabled?(deuce/redo-command, frame) := redo?
  end
end method window-note-undo/redo;

define method window-note-policy-changed
    (window :: <deuce-pane>,
     new-policy :: <editor-policy>, old-policy :: false-or(<editor-policy>)) => ()
  next-method();
  // Note that sheet-frame ~= window-frame for <deuce-gadget>
  let frame = sheet-frame(window);
  when (frame)
    // Update how we handle the Alt key on Windows
    frame-alt-key-is-meta?(frame) := alt-key-is-meta?(new-policy);
    // If we are using the Emacs command set, we want to turn off all
    // accelerators, otherwise just decache them
    let command-set = command-set-policy(new-policy);
    when (~old-policy | command-set ~== command-set-policy(old-policy))
      let accelerators = if (command-set == #"emacs") #[] else #f end;
      frame-accelerators(frame) := accelerators
    end;
  end
end method window-note-policy-changed;
