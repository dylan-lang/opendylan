Module:       deuce-internals
Synopsis:     The Deuce editor
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Major modes

define protocol <<mode>> ()
  getter mode-name
    (mode :: <mode>) => (name :: <string>);
  // Any side effects done by 'enter-mode' must get undone by 'exit-mode'!
  function enter-mode
    (buffer :: <buffer>, mode :: <mode>) => ();
  function exit-mode
    (buffer :: <buffer>, mode :: <mode>) => ();
end protocol <<mode>>;

define protocol <<major-mode>> (<<mode>>)
  function initialize-major-mode
    (mode :: <major-mode>, #key command-set) => ();
  getter mode-command-set
    (mode :: <major-mode>) => (command-set :: <command-set>);
  getter mode-initial-minor-modes
    (mode :: <major-mode>) => (modes :: <sequence>);
  getter word-syntax-table
    (mode :: type-union(<major-mode>, <buffer>)) => (table :: <syntax-table>);
  getter atom-syntax-table
    (mode :: type-union(<major-mode>, <buffer>)) => (table :: <syntax-table>);
  getter list-syntax-table
    (mode :: type-union(<major-mode>, <buffer>)) => (table :: <syntax-table>);
  getter source-file-type
    (mode :: <major-mode>) => (file-type);
  getter binary-file-type
    (mode :: <major-mode>) => (file-type);
  // Command execution is a property of the mode so that major and minor
  // modes can put a "wrapper" around command execution
  function execute-command
    (mode :: <major-mode>, frame :: <editor-frame>, command :: <function>) => ();
  // Returns a sequence of item sets for the Mouse-Right menu for a buffer
  // Each item set is a sequence of items, and will be separated from the
  // other item sets by a divider line
  function buffer-command-menu-items
    (mode :: <major-mode>, buffer :: <buffer>) => (menu-items-sequence :: <sequence>);
end protocol <<major-mode>>;

// The major mode of a buffer controls the behavior of Deuce on the
// contents of that buffer, for example, by installing different command
// sets and implementing language-specific behavior.
define open abstract primary class <major-mode> (<mode>)
  slot mode-command-set :: <command-set>   = $standard-command-set,
    init-keyword: command-set:;
  slot word-syntax-table :: <syntax-table> = $default-word-syntax,
    init-keyword: word-syntax:;
  slot atom-syntax-table :: <syntax-table> = $default-atom-syntax,
    init-keyword: atom-syntax:;
  slot list-syntax-table :: <syntax-table> = $default-list-syntax,
    init-keyword: list-syntax:;
end class <major-mode>;

define sealed inline method major-mode?
    (mode :: <major-mode>) => (major-mode? :: <boolean>)
  #t
end method major-mode?;

define method initialize
    (mode :: <major-mode>, #key) => ()
  next-method();
  initialize-major-mode(mode)
end method initialize;

define method initialize-major-mode
    (mode :: <major-mode>, #key command-set) => ()
  ignore(command-set);
  #f
end method initialize-major-mode;


define variable *major-modes* :: <stretchy-object-vector> = make(<stretchy-vector>);

// Major modes are interned
define method find-mode
    (class :: subclass(<major-mode>)) => (mode :: <major-mode>)
  block (return)
    for (mode :: <major-mode> in *major-modes*)
      when (object-class(mode) == class)
        return(mode)
      end
    end;
    let mode = make(class);
    add!(*major-modes*, mode);
    mode
  end
end method find-mode;

define variable *keyword->major-mode* :: <object-table> = make(<table>);

define function find-mode-from-keyword
    (keyword :: <symbol>) => (mode :: <major-mode>)
  find-mode(gethash(*keyword->major-mode*, keyword,
                    default: <fundamental-mode>))
end function find-mode-from-keyword;

define variable *file-type->major-mode* :: <object-table> = make(<table>);

define function find-mode-from-file-type
    (file-type :: false-or(type-union(<symbol>, <string>)))
 => (mode :: <major-mode>)
  find-mode(gethash(*file-type->major-mode*, file-type & as(<symbol>, file-type),
                    default: <fundamental-mode>))
end function find-mode-from-file-type;

define function find-mode-from-pathname
    (pathname :: <pathname>) => (mode :: <major-mode>)
  let locator   = as(<file-locator>, pathname);
  let extension = locator-extension(locator);
  find-mode-from-file-type(extension)
end function find-mode-from-pathname;

// Set 'buffer-major-mode' to enter the given major mode, and enter
// any initial minor modes
define method enter-mode
    (buffer :: <buffer>, mode :: <major-mode>) => ()
  buffer-major-mode(buffer) := mode;
  // Enter the initial set of minor modes, too
  for (minor-mode :: <minor-mode> in mode-initial-minor-modes(mode))
    enter-mode(buffer, minor-mode)
  end
end method enter-mode;

// Undo any 'enter-mode' side-effects for the major mode, then exit
// the minor modes as well
// Note that we don't reset 'buffer-major-mode' to an "initial state",
// but instead rely on the next call to 'enter-mode' to do this
define method exit-mode
    (buffer :: <buffer>, mode :: <major-mode>) => ()
  until (empty?(buffer-major-mode-undo-list(buffer)))
    let undo = pop!(buffer-major-mode-undo-list(buffer));
    apply(undo[0], undo[1])
  end;
  // Now exit the minor modes
  until (empty?(buffer-minor-modes(buffer)))
    let minor-mode :: <minor-mode> = buffer-minor-modes(buffer)[0];
    exit-mode(buffer, minor-mode)
  end
end method exit-mode;

define method mode-initial-minor-modes
    (mode :: <major-mode>) => (modes :: <vector>)
  #[]
end method mode-initial-minor-modes;


// Execution of all commands goes through 'execute-command'
// Note that *editor-frame* and *buffer* must be correctly bound
// NB: Be sure to look at 'do-execute-keyboard-macro' if you change this!
//--- This is where we execute "command hooks" for "electric font" mode, etc
define method execute-command
    (mode :: <major-mode>, frame :: <editor-frame>, command :: <function>) => ()
  let window :: <basic-window> = frame-window(frame);
  let buffer :: false-or(<basic-buffer>) = window-buffer(window);
  // A higher level should have already filtered out disabled commands,
  // but it's better to be safe than sorry...
  when (command-enabled?(window, command))
    let section = line-section(bp-line(window-point(window)));
    let history = buffer & buffer-undo-history(buffer, section: section);
    let (n-undo, n-redo) = history & undo-history-state(history);
    display-message(window, "");
    frame-command(frame) := command;
    command(frame);
    frame-last-command(frame) := command;
    when (history)
      let (nu, nr) = undo-history-state(history);
      when (nu ~= n-undo | nr ~= n-redo)
        window-note-undo/redo(window, nu ~= 0, nr ~= 0)
      end
    end;
    // If we executed a "real" command, reset the numeric arg state,
    // but first record into the open keyboard macro, if any
    unless (frame-last-command-type(frame) == #"number")
      unless (frame-last-command-type(frame) == #"macro")
        let kbdmac = frame-keyboard-macro(frame);
        when (kbdmac & ~keyboard-macro-closed?(kbdmac))
          let item = make(<keyboard-macro-item>,
                          command:           command,
                          character:         frame-command-character(frame),
                          modifiers:         frame-command-modifiers(frame),
                          numeric-arg:       frame-numeric-arg(frame),
                          numeric-arg-state: frame-numeric-arg-state(frame));
          add!(keyboard-macro-items(kbdmac), item)
        end
      end;
      frame-numeric-arg(frame) := 1;
      frame-numeric-arg-state(frame) := #f
    end;
    // Once we've executed a command, reset the command reader state
    frame-command-state(frame) := standard-command-table(frame-command-set(frame));
    // Reset the goal X position unless we just did 'next-/previous-line'.
    // Note that the command may have changed the window or the major mode,
    // so we first recompute them from the frame.
    unless (frame-last-command-type(frame) == #"line-motion")
      let window :: <basic-window> = frame-window(frame);
      let buffer :: false-or(<basic-buffer>) = window-buffer(window);
      let mode = buffer & buffer-major-mode(buffer);
      let bp   = window-point(window);
      window-goal-x-position(window)
        := if (mode) index->position(bp-line(bp), mode, window, bp-index(bp))
           else 0 end
    end;
    // Now do redisplay
    redisplay-window(window)
  end
end method execute-command;

define sealed method execute-command-in-frame
    (frame :: <editor-frame>, command :: <function>, #key handle-errors? = #t) => ()
  let window :: <basic-window> = frame-window(frame);
  let buffer :: false-or(<basic-buffer>) = window-buffer(window);
  let mode = buffer & buffer-major-mode(buffer);
  if (mode)
    dynamic-bind (*editor-frame* = frame,
                  *buffer*       = buffer)
      if (handle-errors?)
        block ()
          execute-command(mode, frame, command)
        exception (e :: <command-error>)
          when (command-error-format-string(e))
            apply(display-error-message,
                  command-error-window(e),
                  command-error-format-string(e), command-error-format-arguments(e))
          end;
          #f
        end
      else
        execute-command(mode, frame, command)
      end
    end
  else
    when (window)
      display-error-message(window, "Can't determine major mode")
    end
  end
end method execute-command-in-frame;


define method source-file-type
    (mode :: <major-mode>) => (file-type)
  #f
end method source-file-type;

define method binary-file-type
    (mode :: <major-mode>) => (file-type)
  #f
end method binary-file-type;


define method buffer-command-menu-items
    (mode :: <major-mode>, buffer :: <buffer>)
 => (menu-items-vectors :: <vector>)
  vector(vector(vector("Revert Buffer", revert-file),
                vector("Close Buffer",  close-file),
                vector("Save Buffer",   save-file)))
end method buffer-command-menu-items;


/// Minor modes

define open abstract primary class <minor-mode> (<mode>)
end class <minor-mode>;

define sealed inline method major-mode?
    (mode :: <minor-mode>) => (major-mode? :: <boolean>)
  #f
end method major-mode?;


define variable *minor-modes* :: <stretchy-object-vector> = make(<stretchy-vector>);

define method find-mode
    (class :: subclass(<minor-mode>)) => (mode :: <minor-mode>)
  block (return)
    for (mode :: <minor-mode> in *minor-modes*)
      when (object-class(mode) == class)
        return(mode)
      end
    end;
    let mode = make(class);
    add!(*minor-modes*, mode);
    mode
  end
end method find-mode;


define method enter-mode
    (buffer :: <buffer>, mode :: <minor-mode>) => ()
  push-last(buffer-minor-modes(buffer), mode)
end method enter-mode;

define method exit-mode
    (buffer :: <buffer>, mode :: <minor-mode>) => ()
  remove!(buffer-minor-modes(buffer), mode)
end method exit-mode;


/// Overwrite mode

define sealed class <overwrite-mode> (<minor-mode>)
end class <overwrite-mode>;

define method mode-name
    (mode :: <overwrite-mode>) => (name :: <byte-string>)
  "Overwrite"
end method mode-name;
