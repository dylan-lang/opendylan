Module:       deuce-internals
Synopsis:     The Deuce editor
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Keyboard macros

define sealed class <keyboard-macro> (<object>)
  sealed slot keyboard-macro-closed? :: <boolean> = #f;
  sealed slot keyboard-macro-items   :: <stretchy-object-vector> = make(<stretchy-vector>);
end class <keyboard-macro>;

define sealed class <keyboard-macro-item> (<object>)
  sealed constant slot %command,
    required-init-keyword: command:;
  sealed constant slot %character,
    required-init-keyword: character:;
  sealed constant slot %modifiers :: <integer>,
    required-init-keyword: modifiers:;
  sealed constant slot %numeric-arg :: <integer>,
    required-init-keyword: numeric-arg:;
  sealed constant slot %numeric-arg-state :: <argument-state>,
    required-init-keyword: numeric-arg-state:;
end class <keyboard-macro-item>;


define command start-keyboard-macro (frame)
    "Start defining a keyboard macro."
  let window :: <basic-window> = frame-window(frame);
  let kbdmac = frame-keyboard-macro(frame);
  if (kbdmac & ~keyboard-macro-closed?(kbdmac))
    command-error("You are already defining a keyboard macro")
  else
    frame-keyboard-macro(frame) := make(<keyboard-macro>);
    display-message(window, "Defining keyboard macro...")
  end;
  frame-last-command-type(frame) := #"macro"
end command start-keyboard-macro;

define command finish-keyboard-macro (frame)
    "Finish defining a keyboard macro."
  let window :: <basic-window> = frame-window(frame);
  let kbdmac = frame-keyboard-macro(frame);
  if (~kbdmac | keyboard-macro-closed?(kbdmac))
    command-error("You are not defining a keyboard macro")
  else
    keyboard-macro-closed?(kbdmac) := #t;
    display-message(window, "Keyboard macro defined")
  end;
  frame-last-command-type(frame) := #"macro"
end command finish-keyboard-macro;

define command execute-keyboard-macro (frame)
    "Call the last keyboard macro you defined.\n"
    "With a numeric argument, calls the macro that many times."
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-buffer> = window-buffer(window);
  let kbdmac = frame-keyboard-macro(frame);
  if (~kbdmac | ~keyboard-macro-closed?(kbdmac))
    command-error("There is no keyboard macro currently defined")
  else
    block ()
      let n = frame-numeric-arg(frame);
      for (i :: <integer> from 0 below n)
	do-execute-keyboard-macro(frame, kbdmac)
      end
    exception (e :: <command-error>)
      when (command-error-format-string(e))
	apply(display-error-message,
	      command-error-window(e),
	      command-error-format-string(e), command-error-format-arguments(e))
      end;
      #f
    end;
    let section = line-section(bp-line(window-point(window)));
    let history = buffer & buffer-undo-history(buffer, section: section);
    when (history)
      let (n-undo, n-redo) = undo-history-state(history);
      window-note-undo/redo(window, n-undo ~= 0, n-redo ~= 0)
    end
  end
end command execute-keyboard-macro;

// NB: Be sure to look at 'execute-command' if you change this!
define method do-execute-keyboard-macro
    (frame :: <editor-state-mixin>, kbdmac :: <keyboard-macro>) => ()
  for (item :: <keyboard-macro-item> in keyboard-macro-items(kbdmac))
    frame-command(frame)           := item.%command;
    frame-command-character(frame) := item.%character;
    frame-command-modifiers(frame) := item.%modifiers;
    frame-numeric-arg(frame)       := item.%numeric-arg;
    frame-numeric-arg-state(frame) := item.%numeric-arg-state;
    let command = item.%command;
    command(frame);
    frame-last-command(frame)  := command;
    frame-command-state(frame) := standard-command-table(frame-command-set(frame));
    unless (frame-last-command-type(frame) == #"number")
      frame-numeric-arg(frame) := 1;
      frame-numeric-arg-state(frame) := #f
    end;
    unless (frame-last-command-type(frame) == #"line-motion")
      let window :: <basic-window> = frame-window(frame);
      let buffer :: false-or(<basic-buffer>) = window-buffer(window);
      let mode = buffer & buffer-major-mode(buffer);
      let bp   = window-point(window);
      window-goal-x-position(window)
	:= if (mode) index->position(bp-line(bp), mode, window, bp-index(bp))
	   else 0 end
    end
  end
end method do-execute-keyboard-macro;
