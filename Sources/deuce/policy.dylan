Module:       deuce-internals
Synopsis:     The Deuce editor
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Editor policy

define constant <command-set-policy>	 = one-of(#"emacs", #"windows");
define constant <unselected-copy-policy> = one-of(#"copy-line", #"nothing");
define constant <marking-policy>	 = one-of(#"end-of-line", #"right-margin");

// A class to contain editor policy
// NB: the initial values reflect Emacs policies for no particular reason
define sealed class <editor-policy> (<object>)
  // Input policies
  // Selects the default set of command table bindings
  sealed slot command-set-policy :: <command-set-policy> = #"emacs",
    init-keyword: command-set-policy:;
  sealed slot alt-key-is-meta? :: <boolean> = #t,
    init-keyword: alt-key-is-meta?:;
  sealed slot initial-click-moves-point? :: <boolean> = #f,
    init-keyword: initial-click-moves-point?:;

  // Editing policies
  // When #t, kill commands put the killed interval onto the clipboard
  sealed slot clipboard-policy :: <boolean> = #t,
    init-keyword: clipboard-policy:;
  // When #t, typing kills and replaces the current selection; otherwise,
  // typing simply clears the selection and inserts normally
  sealed slot typing-replaces-selection? :: <boolean> = #f,
    init-keyword: typing-replaces-selection?:;
  // When #"copy-line", 'copy-region' copies the whole current line if there
  // is no selected region; otherwise 'copy-region' does nothing
  sealed slot unselected-copy-policy :: <unselected-copy-policy> = #"copy-line",
    init-keyword: unselected-copy-policy:;
  // Does 'next-line' at the end of a buffer add a new line?
  sealed slot next-line-adds-newline? :: <boolean> = #t,
    init-keyword: next-line-adds-newline?:;
  // When #t, undo can "go past" the last time the file was saved;
  // when #f, the undo history is discarded when the file is saved
  sealed slot undo-past-save-policy :: <boolean> = #f,
    init-keyword: undo-past-save-policy:;
  // When #t, ask for yes/no confirmation before killing a buffer, even
  // if the buffer is unmodified (i.e., already saved)
  sealed slot confirm-kill-buffer? :: <boolean> = #t,
    init-keyword: confirm-kill-buffer?:;
  // When #t, this means that 'new-file' goes through a file dialog like Emacs;
  // when #f, it creates a new non-file buffer that will require 'save-file-as'
  sealed slot new-file-buffer? :: <boolean> = #t,
    init-keyword: new-file-buffer?:;

  // Display policies
  // When #"end-of-line", region marking is done to the end of the line;
  // when #"right-margin", region marking extends all the way to the right margin
  sealed slot marking-policy :: <marking-policy> = #"end-of-line",
    init-keyword: marking-policy:;
  // When #t, the point moves during scrolling to ensure that it is alway
  // visible on the screen; when #f, scrolling doesn't move the point
  sealed slot scrolling-moves-point? :: <boolean> = #t,
    init-keyword: scrolling-moves-point?:;
  // When #t, this means frames and buffers are in a 1-to-1 relationship;
  // when #f, it's like Emacs -- one frame can switch between buffers
  sealed slot fixed-frame-buffer? :: <boolean> = #f,
    init-keyword: fixed-frame-buffer?:;
  // When #t, backends should show section separators for buffers that create
  // them. When #f, all section separators are hidden.
  sealed slot show-section-separators? :: <boolean> = #t,
    init-keyword: show-section-separators?:;
  // When #t, backends should show the buffer's path as well as its base
  // filename in their window titles.  When #f, just the base filename.
  sealed slot show-path-in-title? :: <boolean> = #t,
    init-keyword: show-path-in-title?:;
  // Default font and font size
  sealed slot default-font = $default-font,
    init-keyword: default-font:;
  sealed slot default-font-size = #"normal",
    init-keyword: default-font-size:;
  // Size of tab stops
  sealed slot tab-stop-size :: limited(<integer>, min: 1, max: 10) = 8,
    init-keyword: tab-stop-size:;

  // Search policies
  sealed slot wrap-searches? :: <boolean> = #t,
    init-keyword: wrap-searches?:;
  sealed slot use-isearch? :: <boolean> = #t,
    init-keyword: use-isearch?:;
end class <editor-policy>;

define method copy-policy
    (policy :: <editor-policy>) => (new-policy :: <editor-policy>)
  make(<editor-policy>,
       command-set-policy:	   command-set-policy(policy),
       alt-key-is-meta?:	   alt-key-is-meta?(policy),
       initial-click-moves-point?: initial-click-moves-point?(policy),
       clipboard-policy:	   clipboard-policy(policy),
       typing-replaces-selection?: typing-replaces-selection?(policy),
       unselected-copy-policy:	   unselected-copy-policy(policy),
       next-line-adds-newline?:    next-line-adds-newline?(policy),
       undo-past-save-policy:	   undo-past-save-policy(policy),
       confirm-kill-buffer?:	   confirm-kill-buffer?(policy),
       new-file-buffer?:	   new-file-buffer?(policy),
       marking-policy:		   marking-policy(policy),
       scrolling-moves-point?:	   scrolling-moves-point?(policy),
       default-font:		   default-font(policy),
       default-font-size:	   default-font-size(policy),
       tab-stop-size:		   tab-stop-size(policy),
       fixed-frame-buffer?:	   fixed-frame-buffer?(policy),
       show-section-separators?:   show-section-separators?(policy),
       show-path-in-title?:	   show-path-in-title?(policy),
       wrap-searches?:		   wrap-searches?(policy),
       use-isearch?:		   use-isearch?(policy))
end method copy-policy;


/// Command table policy

// Called to install a new command table after the policy has been updated
define method install-command-set
    (editor :: <editor>, command-set :: <command-set-policy>) => ()
  let command-set :: <command-set> = gethash(*command-sets*, command-set);
  when (command-set)
    // Install the new command set into $standard-command-set,
    // then update major mode command sets.  Note that this assumes
    // that major modes have their own copies of command sets, rather
    // than using 'enter-mode' to "bind" the command set.
    copy-command-set-into!(command-set, $standard-command-set);
    for (mode :: <major-mode> in *major-modes*)
      //--- Reset to a known state in the best way we can for now
      copy-command-set-into!($standard-command-set, mode-command-set(mode));
      initialize-major-mode(mode)
    end
  end
end method install-command-set;


/// Standard policies

define constant $emacs-editor-policy
    = make(<editor-policy>,
	   command-set-policy:		#"emacs",
	   alt-key-is-meta?:		#t,
	   initial-click-moves-point?:	#f,
	   clipboard-policy:		#t,
	   typing-replaces-selection?:	#f,
	   unselected-copy-policy:	#"copy-line",
	   next-line-adds-newline?:	#t,
	   undo-past-save-policy:	#f,
	   confirm-kill-buffer?:	#t,
	   new-file-buffer?:		#t,
	   marking-policy:		#"right-margin",
	   scrolling-moves-point?:	#t,
	   default-font:		$default-font,
	   default-font-size:		#"normal",
	   tab-stop-size:		8,
	   fixed-frame-buffer?:		#f,
	   show-section-separators?:    #t,
	   show-path-in-title?:		#t,
	   wrap-searches?:		#t,
	   use-isearch?:		#t);

define constant $windows-editor-policy
    = make(<editor-policy>,
	   command-set-policy:		#"windows",
	   alt-key-is-meta?:		#f,
	   initial-click-moves-point?:	#f,
	   clipboard-policy:		#t,
	   typing-replaces-selection?:	#t,
	   unselected-copy-policy:	#"copy-line",
	   next-line-adds-newline?:	#f,
	   undo-past-save-policy:	#f,
	   confirm-kill-buffer?:	#f,
	   new-file-buffer?:		#f,
	   marking-policy:		#"end-of-line",
	   scrolling-moves-point?:	#f,
	   default-font:		$default-font,
	   default-font-size:		#"normal",
	   tab-stop-size:		8,
	   fixed-frame-buffer?:		#t,
	   show-section-separators?:    #f,
	   show-path-in-title?:		#f,
	   wrap-searches?:		#t,
	   use-isearch?:		#f);

// The default behavior for DylanWorks is to use the Windows policy (yech!)
define constant $default-editor-policy = $windows-editor-policy;
