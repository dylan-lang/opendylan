Module:       deuce-internals
Synopsis:     The Deuce editor
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Registry of command sets

// Each entry in the table is a command set
define variable *command-sets* :: <object-table> = make(<table>);


/// Emacs bindings

define constant $emacs-command-set :: <standard-command-set>
    = make(<command-set>,
	   name: #"emacs",
	   standard-command-table:  make(<command-table>, name: #"emacs"),
	   control-X-command-table: make(<command-table>, name: #"emacs-control-X"),
	   control-C-command-table: make(<command-table>, name: #"emacs-control-C"),
	   escape-command-table:    make(<command-table>, name: #"emacs-escape"));

define function initialize-emacs-command-set () => ()
  let nothing = 0;
  let shift   = $shift-key;
  let control = $control-key;
  let meta    = $meta-key;
  let control+meta  = logior($control-key, $meta-key);
  let control+shift = logior($control-key, $shift-key);
  // First fill in the main command table
  let command-set   = $emacs-command-set;
  let command-table = standard-command-table(command-set);
  // ASCII characters are self-inserting
  for (code from 32 to 126)
    add-command!(command-table, as(<byte-character>, code), self-insert)
  end;
  // Latin-1 characters are self-inserting, too
  for (code from 128 to 255)
    add-command!(command-table, as(<byte-character>, code), self-insert)
  end;
  let gesture :: <simple-object-vector> = vector(#f, #f);
  for (code from 48 below 58)		// i.e., '0' to '9'
    gesture[0] := as(<byte-character>, code);
    for (modifier-state from 1 below 8)	// all combinations of control/meta/super
      gesture[1] := modifier-state;
      add-command!(command-table, gesture, numeric-argument)
    end
  end;
  add-commands!(command-table,
		vector('q', control,      quoted-insert),
		vector('u', control,      universal-argument),
		vector('-', control,      numeric-negative),
		vector('-', meta,         numeric-negative),
		vector('-', control+meta, numeric-negative),
		vector('g', control,      cancel-command),
		vector('f', control,      forward-character),
		vector('f', meta,         forward-word),
		vector('f', control+meta, forward-list),
		vector('b', control,      backward-character),
		vector('b', meta,         backward-word),
		vector('b', control+meta, backward-list),
		vector('u', control+meta, up-list),
		vector('d', control+meta, down-list),
		vector('a', control,      start-of-line), 
		vector('e', control,      end-of-line),
		vector('n', control,      next-line), 
		vector('p', control,      previous-line),
		vector('g', meta,         goto-character),
		vector('G', meta,         goto-line),
		vector('<', meta,         start-of-buffer),
		vector('>', meta,         end-of-buffer),
		vector(#"right", nothing,       forward-character-ext),
		vector(#"right", shift,         forward-character-ext),
		vector(#"right", control,       forward-word-ext),
		vector(#"right", control+shift, forward-word-ext),
		vector(#"left",  nothing,       backward-character-ext),
		vector(#"left",  shift,         backward-character-ext),
		vector(#"left",  control,       backward-word-ext),
		vector(#"left",  control+shift, backward-word-ext),
		vector(#"down",  nothing, next-line-ext), 
		vector(#"down",  shift,   next-line-ext), 
		vector(#"up",    nothing, previous-line-ext),
		vector(#"up",    shift,   previous-line-ext),
		vector('a', control+meta, start-of-section),
		vector('e', control+meta, end-of-section),
		vector(#"home",  nothing, start-of-line-ext),
		vector(#"home",  shift,   start-of-line-ext),
		vector(#"end",   nothing, end-of-line-ext),
		vector(#"end",   shift,   end-of-line-ext),
		vector(#"home",  control, start-of-buffer), 
		vector(#"end",   control, end-of-buffer),
		vector(#"prior", meta,    start-of-section), 
		vector(#"next",  meta,    end-of-section),
		vector(#"delete", nothing,      delete-character),
		vector(#"delete", control,      delete-word),
		vector(#"delete", meta,         delete-word),
		vector(#"delete", control+meta, delete-list),
		vector(#"backspace", nothing,      rubout-character),
		vector(#"backspace", control,      rubout-word),
		vector(#"backspace", meta,         rubout-word),
		vector(#"backspace", control+meta, rubout-list),
		vector('d', control,      delete-character),
		vector('d', meta,         delete-word),
		vector('k', control+meta, delete-list),
		vector('i',        meta,    insert-tab),
		vector(#"tab",     nothing, insert-tab),
		vector(#"return",  nothing, insert-newline),
		vector(#"newline", nothing, indent-newline),
		vector('j',        control, indent-newline),
		vector('o', control,      open-line),
		vector('o', control+meta, split-line),
		vector('k', control,      kill-line),
		vector('^', meta,         join-lines),
		vector('t', control,      transpose-characters),
		vector('t', meta,         transpose-words),
		vector('t', control+meta, transpose-lists),
		vector('u', meta,         upcase-word),
		vector('l', meta,         downcase-word),
		vector('c', meta,         capitalize-word),
		vector('j', meta,         change-region-font),
		vector('w', control,      cut-region),
		vector('w', meta,         copy-region),
		vector('Y', control,      paste),
		vector('y', control,      yank),
		vector('y', meta,         yank-next),
		vector('s', control,      incremental-search-forward),
		vector('r', control,      incremental-search-backward),
		vector('%', control,      replace-string),
		vector('%', meta,         query-replace-string),
		vector(#"f3", nothing,    find-string),
		vector(#"f3", shift,      find-next-string),
		vector(#"f3", control,    query-replace-string),
		vector('/', meta,         dynamic-complete-name),
		vector('v', control,      scroll-forward),
		vector('v', meta,         scroll-backward),
		vector(#"next",  nothing, scroll-forward-ext),
		vector(#"next",  shift,   scroll-forward-ext),
		vector(#"prior", nothing, scroll-backward-ext),
		vector(#"prior", shift,   scroll-backward-ext),
		vector(#"next",  control, end-of-page),
		vector(#"prior", control, start-of-page),
		vector('l', control,      force-recenter),
		vector(#"f5", control,    force-recenter),
		vector(#"f5", nothing,    force-redisplay),
		vector('U', control,       undo-command),
		vector('_', control,       undo-command),
		vector('R', control,       redo-command),
		vector('_', meta,          redo-command),
		vector('\\', meta,        delete-whitespace),
		vector('~', meta,         mark-unmodified),
		vector('l', control+meta, switch-buffers),
		vector(' ', control,      set-mark),
		vector(' ', control+meta, exchange-point-pdl),
		vector('<', control,      mark-to-beginning),
		vector('>', control,      mark-to-end),
		vector('@', control+meta, mark-next-expression),
		vector(#"insert", nothing, toggle-overwrite),
		vector(#"help", nothing,  editor-key-bindings),
		vector('h', control,      editor-help));
  // Indirect to the control-X, control-C, and escape command tables
  add-command!(command-table, vector('x', control),
	       control-X-command-table(command-set));
  add-command!(command-table, vector('c', control),
	       control-C-command-table(command-set));
  add-command!(command-table, vector(#"escape", nothing),
	       escape-command-table(command-set));
  // Fill in the Emacs control-X command table
  let command-table = control-X-command-table(command-set);
  add-commands!(command-table,
		vector('g', control, cancel-command),
		vector('f', control, find-file),
		vector('n', control, new-file),
		vector('i', nothing, insert-file),
		vector('s', control, save-file),
		vector('s', nothing, save-all-files),
		vector('w', control, save-file-as),
		vector('k', nothing, close-file),
		vector('t', control, transpose-lines),
		vector('u', control, upcase-region),
		vector('l', control, downcase-region),
		vector('i',    control, indent-rigidly),
		vector(#"tab", nothing, indent-rigidly),
		vector('q', control, toggle-read-only),
		vector('x', control, swap-point-and-mark),
		vector('h', nothing, mark-buffer),
		vector(']', nothing, end-of-page),
		vector('[', nothing, start-of-page),
		vector('=', nothing, show-position),
		vector('b', nothing, choose-buffer),
		vector('b', control, choose-buffer),
		vector('(', nothing, start-keyboard-macro),
		vector(')', nothing, finish-keyboard-macro),
		vector('e', nothing, execute-keyboard-macro),
		vector('/', nothing, save-point-in-register),
		vector('j', nothing, restore-point-from-register),
		vector('x', nothing, save-string-in-register),
		vector('g', nothing, insert-string-from-register),
		vector('m', nothing, send-mail));
  // Fill in the Emacs control-C command table
  let command-table = control-C-command-table(command-set);
  add-commands!(command-table,
		vector('c', nothing, vc-claim),
		vector('o', nothing, vc-check-out),
		vector('i', nothing, vc-check-in),
		vector('a', nothing, vc-abandon),
		vector('m', nothing, vc-merge),
		vector('=', nothing, vc-diff),
		vector('r', nothing, vc-report),
		vector('+', nothing, vc-add),
		vector('-', nothing, vc-remove));
  // Fill in the Emacs escape command table
  let command-table = escape-command-table(command-set);
  let gesture = vector(#f, #f);
  for (code from 48 below 58)		// i.e., '0' to '9'
    gesture[0] := as(<byte-character>, code);
    for (modifier-state in vector(nothing, control))
      gesture[1] := modifier-state;
      add-command!(command-table, gesture, numeric-argument)
    end
  end;
  add-commands!(command-table,
		vector('-', nothing, numeric-negative),
		vector('-', control, numeric-negative),
		vector('g', control, cancel-command),
		vector('f', nothing, forward-word),
		vector('f', control, forward-list),
		vector('b', nothing, backward-word),
		vector('b', control, backward-list),
		vector('u', control, up-list),
		vector('d', control, down-list),
		vector('g', nothing, goto-character),
		vector('G', nothing, goto-line),
		vector('<', nothing, start-of-buffer),
		vector('>', nothing, end-of-buffer),
		vector('a', control, start-of-section),
		vector('e', control, end-of-section),
		vector(#"home", nothing, start-of-section), 
		vector(#"end",  nothing, end-of-section),
		vector(#"delete", nothing, delete-word),
		vector(#"delete", control, delete-list),
		vector(#"backspace", nothing, rubout-word),
		vector(#"backspace", control, rubout-list),
		vector('d', nothing, delete-word),
		vector('k', control, delete-list),
		vector('o', control, split-line),
		vector('^', nothing, join-lines),
		vector('t', nothing, transpose-words),
		vector('t', control, transpose-lists),
		vector('u', nothing, upcase-word),
		vector('l', nothing, downcase-word),
		vector('c', nothing, capitalize-word),
		vector('j', nothing, change-region-font),
		vector('w', nothing, copy-region),
		vector('y', nothing, yank-next),
		vector('%', nothing, query-replace-string),
		vector('/', nothing, dynamic-complete-name),
		vector('v', nothing, scroll-backward),
		vector('_', nothing, redo-command),
		vector('\\', nothing, delete-whitespace),
		vector('~', nothing, mark-unmodified),
		vector('l', control, switch-buffers),
		vector(' ', control, exchange-point-pdl),
		vector('@', control, mark-next-expression));
  gethash(*command-sets*, #"emacs") := $emacs-command-set
end function initialize-emacs-command-set;

initialize-emacs-command-set();


/// Windows bindings

define constant $windows-command-set :: <standard-command-set>
    = make(<command-set>,
	   name: #"windows",
	   standard-command-table:  make(<command-table>, name: #"windows"),
	   control-X-command-table: make(<command-table>, name: #"windows-control-X"),
	   control-C-command-table: make(<command-table>, name: #"windows-control-C"),
	   escape-command-table:    make(<command-table>, name: #"windows-escape"));

define function initialize-windows-command-set () => ()
  let nothing = 0;
  let shift   = $shift-key;
  let control = $control-key;
  let meta    = $meta-key;
  let control+meta  = logior($control-key, $meta-key);
  let control+shift = logior($control-key, $shift-key);
  // First fill in the main command table
  let command-set   = $windows-command-set;
  let command-table = standard-command-table(command-set);
  // ASCII characters are self-inserting
  for (code from 32 below 127)
    add-command!(command-table, as(<byte-character>, code), self-insert)
  end;
  // Latin-1 characters are self-inserting, too
  for (code from 128 below 256)
    add-command!(command-table, as(<byte-character>, code), self-insert)
  end;
  let gesture = vector(#f, $control-key);
  for (code from 48 below 58)		// i.e., '0' to '9'
    gesture[0] := as(<byte-character>, code);
    add-command!(command-table, gesture, numeric-argument)
  end;
  add-commands!(command-table,
		vector('q', control,       quoted-insert),
		vector('*', control,       universal-argument),
		vector('-', control,       numeric-negative),
		vector(#"escape", nothing, cancel-command),
		vector(#"right", nothing,       forward-character-ext),
		vector(#"right", shift,         forward-character-ext),
		vector(#"right", control,       forward-word-ext),
		vector(#"right", control+shift, forward-word-ext),
		vector(#"left",  nothing,       backward-character-ext),
		vector(#"left",  shift,         backward-character-ext),
		vector(#"left",  control,       backward-word-ext),
		vector(#"left",  control+shift, backward-word-ext),
		vector(')',      control, forward-list),
		vector('(',      control, backward-list),
		vector('+',      control, up-list),
		vector('_',      control, down-list),
		vector(#"down",  nothing, next-line-ext),
		vector(#"down",  shift,   next-line-ext),
		vector(#"up",    nothing, previous-line-ext),
		vector(#"up",    shift,   previous-line-ext),
		vector(#"home",  nothing, start-of-line-ext),
		vector(#"home",  shift,   start-of-line-ext),
		vector(#"end",   nothing, end-of-line-ext),
		vector(#"end",   shift,   end-of-line-ext),
		vector(#"home",  control, start-of-buffer), 
		vector(#"end",   control, end-of-buffer),
		vector(#"prior", meta,    start-of-section), 
		vector(#"next",  meta,    end-of-section),
		vector('g',      control, goto-line),
		vector(#"delete", nothing,    delete-character),
		vector(#"delete", control,    delete-word),
		vector(#"backspace", nothing, rubout-character),
		vector(#"backspace", control, rubout-word),
		vector('i',        control, insert-tab),
		vector(#"tab",     nothing, insert-tab),
		vector(#"return",  nothing, insert-newline),
		vector(#"newline", nothing, indent-newline),
		vector('t', control,      transpose-characters),
		vector('r', control,      transpose-words),
		vector('u', control,      upcase-region),
		vector('l', control,      downcase-region),
		vector('k', control,      capitalize-word),
		vector('x', control,      cut-region),
		vector('c', control,      copy-region),
		vector('v', control,      paste),
		vector(#"f3", nothing,    find-string),
		vector(#"f3", shift,      find-next-string),
		vector(#"f3", control,    query-replace-string),
		vector('f', control,      find-next-string),
		vector('F', control,      find-previous-string),
		vector(#"next",  nothing, scroll-forward-ext),
		vector(#"next",  shift,   scroll-forward-ext),
		vector(#"prior", nothing, scroll-backward-ext),
		vector(#"prior", shift,   scroll-backward-ext),
		vector(#"next",  control, end-of-page),
		vector(#"prior", control, start-of-page),
		vector(#"f5", control,    force-recenter),
		vector(#"f5", nothing,    force-redisplay),
		vector('a', control,      mark-buffer),
		vector(#"home", control+shift, mark-to-beginning),
		vector(#"end",  control+shift, mark-to-end),
		vector('z', control,      undo-command),
		vector('y', control,      redo-command),
		vector('o', control,      find-file),
		vector('n', control,      new-file),
		vector('s', control,      save-file),
		vector('=', control,      show-position),
		vector(#"f10", meta,      switch-buffers),
		vector(#"f10", control,   choose-buffer),
		vector(#"insert", nothing, toggle-overwrite),
		vector(#"help", nothing,  editor-key-bindings));
  gethash(*command-sets*, #"windows") := $windows-command-set
end function initialize-windows-command-set;

initialize-windows-command-set();


/// The standard command set

// The standard command set, to be filled in from one of concrete sets
define constant $standard-command-set :: <standard-command-set>
    = make(<command-set>,
	   standard-command-table:  make(<command-table>),
	   control-X-command-table: make(<command-table>),
	   control-C-command-table: make(<command-table>),
	   escape-command-table:    make(<command-table>));

begin
  select (command-set-policy($default-editor-policy))
    #"emacs"   =>
      copy-command-set-into!($emacs-command-set,   $standard-command-set);
    #"windows" =>
      copy-command-set-into!($windows-command-set, $standard-command-set);
  end
end;
