Module:       deuce-internals
Synopsis:     The Deuce editor
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// The Deuce commands

/// Note that since most commands cons up simple BPs to hold their
/// intermediate results, there is an implicit assumption that *buffer*
/// is bound to the appropriate buffer.  Beware!

/// Note that command methods assume that *editor-frame* is bound to the
/// same value as the 'frame' argument.  Beware!

//---*** These commands really need to worry about locking...

// This just does 'define method' of the named function on <editor-state-mixin>,
// and installs the help for the command
define macro command-definer
  { define ?modifiers:* command ?:name (?frame:name, ?opt-args:*)
	?documentation:expression
      ?:body
    end }
    => { define open generic ?name (?frame :: <editor-state-mixin>, ?opt-args) => ();
	 define ?modifiers method ?name (?frame :: <editor-state-mixin>, ?opt-args) => ()
	   ?body;
	 end method ?name;
	 initialize-command-help(?name, ?documentation); }
  { define ?modifiers:* command ?:name (?frame:name :: ?type:name, ?opt-args:*)
	?documentation:expression
      ?:body
    end }
    => { define open generic ?name (?frame :: <editor-state-mixin>, ?opt-args) => ();
	 define ?modifiers method ?name (?frame :: ?type, ?opt-args) => ()
	   ?body;
	 end method ?name;
	 initialize-command-help(?name, ?documentation); }
end macro command-definer;

/*
//--- This is nicer, but it doesn't work in the emulator
define macro command-definer
  { define ?modifiers:* command ?:name (?frame:name ?opt-type, ?opt-args:*)
	?documentation:expression
      ?:body
    end }
    => { define open generic ?name (?frame :: <editor-state-mixin>, ?opt-args) => ();
	 define ?modifiers method ?name (?frame ?opt-type, ?opt-args) => ()
	   ?body;
	 end method ?name;
	 initialize-command-help(?name, ?documentation); }
 opt-type:
  { :: ?type:expression } => { :: ?type }
  { } => { :: <editor-state-mixin> }
end macro command-definer;
*/


define variable $brief-command-help :: <object-table> = make(<table>);
define variable $long-command-help  :: <object-table> = make(<table>);

define function initialize-command-help
    (command :: <function>, documentation :: <string>) => ()
  let nl = position(documentation, '\n');
  if (nl)
    // If there's a newline in the documentation string, use the first
    // part as the brief documentation and use the entire string as the
    // long documentation
    $brief-command-help[command] := copy-sequence(documentation, end: nl);
    $long-command-help[command]  := documentation
  else
    // Otherwise, the string serves as both brief and long documentation
    $brief-command-help[command] := documentation
  end
end function initialize-command-help;


/// Command errors

define sealed class <simple-command-error> (<command-error>)
end class <simple-command-error>;

define sealed inline method make
    (class == <command-error>, #rest initargs, #key, #all-keys)
 => (error :: <simple-command-error>)
  apply(make, <simple-command-error>, initargs)
end method make;

define sealed domain make (singleton(<simple-command-error>));
define sealed domain initialize (<simple-command-error>);

// Reset command reading state and signal a command error
define function command-error
    (format-string :: <string>, #rest format-args)
  let frame :: <editor-state-mixin> = *editor-frame*;
  let window = frame-window(frame);
  frame-last-command-type(frame) := #f;
  frame-numeric-arg(frame)       := 1;
  frame-numeric-arg-state(frame) := #f;
  abort-keyboard-macro-definition(frame);
  error(make(<command-error>,
	     window: window,
	     format-string:    concatenate-as(<string>, "Error: ", format-string),
	     format-arguments: copy-sequence(format-args)))
end function command-error;


/// Read-only errors

define sealed class <read-only-command-error> (<command-error>)
end class <read-only-command-error>;

define sealed domain make (singleton(<read-only-command-error>));
define sealed domain initialize (<read-only-command-error>);

define function read-only-command-error
    (format-string :: <string>, #rest format-args)
  let frame :: <editor-state-mixin> = *editor-frame*;
  let window = frame-window(frame);
  frame-last-command-type(frame) := #f;
  frame-numeric-arg(frame)       := 1;
  frame-numeric-arg-state(frame) := #f;
  abort-keyboard-macro-definition(frame);
  error(make(<read-only-command-error>,
	     window: window,
	     format-string:    concatenate-as(<string>, "Error: ", format-string),
	     format-arguments: copy-sequence(format-args)))
end function read-only-command-error;

define sealed method check-read-only
    (bp :: <basic-bp>) => ()
  check-read-only-line(bp-line(bp))
end method check-read-only;

define sealed method check-read-only
    (line :: <basic-line>) => ()
  check-read-only-line(line)
end method check-read-only;

define sealed inline method check-read-only-line
    (line :: <basic-line>) => ()
  let buffer = *buffer*;
  let home   = if (composite-buffer?(buffer)) section-home-buffer(line-section(line))
	       else buffer end;
  when (buffer-read-only?(buffer)
	| (home & buffer-read-only?(home))
	| line-read-only?(line))
    read-only-command-error("Can't modify a read-only region")
  end
end method check-read-only-line;

define sealed method check-read-only
    (interval :: <basic-interval>) => ()
  let buffer     = interval-buffer(interval);
  let composite? = composite-buffer?(buffer);
  when (buffer-read-only?(buffer))
    read-only-command-error("Can't modify a read-only region")
  end;
  local method read-only? (line :: <basic-line>, si, ei, last?)
	  ignore(si, ei, last?);
	  let home = if (composite?) section-home-buffer(line-section(line))
		     else buffer end;
	  when ((home & buffer-read-only?(home))
		| line-read-only?(line))
	    read-only-command-error("Can't modify a read-only region")
	  end
	end method;
  do-lines(read-only?, interval)
end method check-read-only;


/// Character insertion

define command self-insert (frame)
    "Insert the character you just typed.\n"
    "With a numeric argument, inserts that many copies of the character."
  let char :: <byte-character> = frame-command-character(frame);
  let state = frame-numeric-arg-state(frame);
  if (member?(state, #[#"universal", #"universal-digits", #"universal-sign"])
      & frame-last-command-type(frame) == #"number"
      & (digit-char?(char) | char == '-'))
    // The user typed something like control-U 1 0, treat it as a numeric arg
    let window :: <basic-window> = frame-window(frame);
    if (char == '-')
      let n     :: <integer> = if (state == #"universal") 1 else frame-numeric-arg(frame) end;
      frame-numeric-arg(frame)       := -abs(n);
      frame-numeric-arg-state(frame) := #"universal-sign"
    else
      let n     :: <integer> = if (state == #"universal-digits") frame-numeric-arg(frame) else 0 end;
      let sign  :: <integer> = if (n < 0 | state == #"universal-sign") -1 else 1 end;
      let digit :: <integer> = as(<integer>, char) - as(<integer>, '0');
      frame-numeric-arg(frame)       := n * 10 + digit * sign;
      frame-numeric-arg-state(frame) := #"universal-digits"
    end;
    display-message(window, "%d-", frame-numeric-arg(frame))
  else
    insert-character(frame, char)
  end
end command self-insert;

define command quoted-insert (frame)
    "Insert the non-standard character you just typed.\n"
    "With a numeric argument, inserts the character with that code."
  let window :: <basic-window> = frame-window(frame);
  if (frame-numeric-arg-state(frame) == #"digits")
    let char = as(<byte-character>, logand(frame-numeric-arg(frame), #o377));
    frame-numeric-arg(frame) := 1;
    insert-character(frame, char)
  else
    display-message(window, "Insert character: ");
    let char = read-character(window);
    insert-character(frame, char)
  end
end command quoted-insert;

define command insert-tab (frame)
    "Insert a tab at the current position.\n"
    "With a numeric argument, inserts that many tab characters."
  let char :: <byte-character> = '\t';
  insert-character(frame, char)
end command insert-tab;
  
define method insert-character
    (frame :: <editor-state-mixin>, char) => ()
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-buffer> = frame-buffer(frame);
  let n :: <integer> = frame-numeric-arg(frame);
  if (n <= 0)
    command-error("Can't insert a negative number of characters")
  else
    let bp  = point();
    let bp2 = typing-replaces-selection?(editor-policy(frame-editor(frame))) & mark();
    check-read-only(bp);
    let thing = if (n > 1) make(<byte-string>, size: n, fill: char)
		else char end;
    if (bp2)
      with-change-recording (buffer, <replace-change-record>,
			     start-bp: bp, end-bp: bp2, moving?: #t)
	let interval = make-interval(bp, bp2);
	check-read-only(interval);
	queue-region-redisplay(window, bp, bp2);
	bp := kill!(interval)
	      | command-error("Can't delete across hard sections");
	clear-mark!();
	insert-moving!(bp, thing)
      end
    else
      with-change-recording (buffer, <insert-change-record>, start-bp: bp)
	clear-mark!();
	insert-moving!(bp, thing)
      end
    end;
    move-point!(bp);
    if (char == '\n')
      queue-redisplay(window, $display-text, centering: n)
    else
      queue-redisplay(window, $display-line,
		      line: bp-line(bp), index: bp-index(bp) - n)
    end
  end;
  frame-last-command-type(frame) := #"insert"
end method insert-character;


define command toggle-overwrite (frame)
    "Toggle overwrite mode for this buffer."
  let buffer :: <basic-buffer> = frame-buffer(frame);
  let overwrite-mode = find-mode(<overwrite-mode>);
  if (member?(overwrite-mode, buffer-minor-modes(buffer)))
    exit-mode(buffer, overwrite-mode)
  else
    enter-mode(buffer, overwrite-mode)
  end
end command toggle-overwrite;


/// Numeric arguments

define command numeric-argument (frame)
    "Accumulate another digit into the numeric argument."
  let window :: <basic-window> = frame-window(frame);
  let state = frame-numeric-arg-state(frame);
  let n     :: <integer> = if (state & state ~== #"sign") frame-numeric-arg(frame) else 0 end;
  let sign  :: <integer> = if (n < 0 | state == #"sign") -1 else 1 end;
  let char  :: <byte-character> = frame-command-character(frame);
  let digit :: <integer> = as(<integer>, char) - as(<integer>, '0');
  frame-numeric-arg(frame)       := n * 10 + digit * sign;
  frame-numeric-arg-state(frame) := #"digits";
  display-message(window, "%d-", frame-numeric-arg(frame));
  frame-last-command-type(frame) := #"number"
end command numeric-argument;

define command numeric-negative (frame)
    "Make the accumulated numeric argument be negative."
  let window :: <basic-window> = frame-window(frame);
  let n :: <integer> = frame-numeric-arg(frame);
  frame-numeric-arg(frame) := -abs(n);
  frame-numeric-arg-state(frame) := #"sign";
  display-message(window, "%d-", frame-numeric-arg(frame));
  frame-last-command-type(frame) := #"number";
end command numeric-negative;

define command universal-argument (frame)
    "Multiply the current numeric argument by 4."
  let window :: <basic-window> = frame-window(frame);
  let n :: <integer> = frame-numeric-arg(frame);
  frame-numeric-arg(frame)       := n * 4;
  frame-numeric-arg-state(frame) := #"universal";
  display-message(window, "%d-", frame-numeric-arg(frame));
  frame-last-command-type(frame) := #"number"
end command universal-argument;


define command cancel-command (frame)
    "Cancel the current command."
  let window :: <basic-window> = frame-window(frame);
  frame-numeric-arg(frame)       := 1;
  frame-numeric-arg-state(frame) := #f;
  let aborted? = abort-keyboard-macro-definition(frame);
  if (frame-command-state(frame) == standard-command-table(frame-command-set(frame)))
    clear-mark!();
    queue-redisplay(window, $display-region)
  else
    frame-command-state(frame) := standard-command-table(frame-command-set(frame))
  end;
  if (aborted?)
    display-error-message(window, "Cancel macro definition")
  else
    display-error-message(window, "Cancel")
  end;
  frame-last-command-type(frame) := #"cancel"
end command cancel-command;


/// Help

define variable $editor-help-cache = vector(#f, #f, #f);

define command editor-help (frame)
    "Show help for all the current key bindings."
  let window :: <basic-window> = frame-window(frame);
  local method get-command
	    (window :: <basic-window>, comtab :: <command-table>, message :: <byte-string>)
	 => (command)
	  block (return)
	    while (#t)
	      display-message(window, message);
	      let (keysym, char, modifiers) = read-gesture(window);
	      let gesture = vector(char | keysym, modifiers);
	      let command = find-command(comtab, gesture);
	      case
		instance?(command, <command-table>) =>
		  // It's a command prefix (c-X), so change the state
		  comtab := command;
		command =>
		  // We've gotten to a command, all done
		  return(command);
		otherwise =>
		  return(#f);
	      end
	    end
	  end
	end method;
  display-message(window, "Help: ");
  let char = read-character(window);
  select (char)
    '?' =>
      display-message(window,
		      "Help: Use 'b' to see all bindings, 'k' to see what a key is bound to.");
    'b', 'B' =>
      display-message(window,
		      "Help: All current key bindings.");
      editor-key-bindings(frame);
    'c', 'C' =>
      let comtab  = standard-command-table(frame-command-set(frame));
      let command = get-command(window, comtab, "Brief help for key: ");
      if (command)
	let documentation
	  = gethash($brief-command-help, command);
	when (documentation)
	  display-message(window, "%s", documentation)
	end
      else
	display-error-message(window, "No binding for that key")
      end;
    'k', 'K' =>
      let comtab  = standard-command-table(frame-command-set(frame));
      let command = get-command(window, comtab, "Help for key: ");
      if (command)
	let documentation
	  = gethash($long-command-help, command) | gethash($brief-command-help, command);
	when (documentation)
	  display-message(window, "%s", documentation)
	end
      else
	display-error-message(window, "No binding for that key")
      end;
    otherwise =>
      error(make(<command-error>, window: window));
  end;
  frame-last-command-type(frame) := #"display"
end command editor-help;

define command editor-key-bindings (frame)
    "Show help for all the current key bindings."
  let window :: <basic-window> = frame-window(frame);
  let name   = "Key bindings";
  let editor = frame-editor(frame);
  let buffer = find-buffer(editor, name)
	       | make-empty-buffer(<simple-display-buffer>,
				   name:       name,
				   major-mode: find-mode(<text-mode>),
				   read-only?: #t,
				   editor:     editor);
  let command-set = frame-command-set(frame);
  let set-name    = command-set-name(command-set);
  unless (get-property(buffer-properties(buffer), #"command-set") == command-set
	  & get-property(buffer-properties(buffer), #"command-set-name") == set-name)
    put-property!(buffer-properties(buffer), #"command-set", command-set);
    put-property!(buffer-properties(buffer), #"command-set-name", set-name);
    let lines   = compute-key-binding-documentation(command-set);
    let section = make(<section>,
		       start-line: #f, end-line: #f);
    let first-line :: false-or(<basic-line>) = #f;
    let last-line  :: false-or(<basic-line>) = #f;
    for (line in lines)
      let line = make(<text-line>,
		      contents: line,
		      section: section);
      unless (first-line)
	first-line := line
      end;
      line-previous(line) := last-line;
      when (last-line)
	line-next(last-line) := line
      end;
      last-line := line
    end;
    if (first-line)
      // Be flashy and use italics...
      let contents = "Binding\t\tCommand";
      let changes  = vector(make(<style-change>,
				 index: 0,
				 font:  window-default-italic-font(window)));
      let line     = make(<rich-text-line>,
			  contents: contents,
			  length:   size(contents),
			  section:  section,
			  style-changes: changes);
      line-next(line) := first-line;
      line-previous(first-line) := line;
      first-line := line
    else
      first-line := make(<text-line>,
			 contents: "No help available",
			 section: section);
      last-line  := first-line
    end;
    section-start-line(section) := first-line;
    section-end-line(section)   := last-line;
    let node = make-section-node(buffer, section);
    node-buffer(node)         := buffer;
    section-nodes(section)    := list(node);
    buffer-start-node(buffer) := node;
    buffer-end-node(buffer)   := node
  end;
  select-buffer-in-appropriate-window(window, buffer);
  frame-last-command-type(frame) := #"display"
end command editor-key-bindings;

define method compute-key-binding-documentation
    (command-set :: <command-set>) => (commands :: <stretchy-object-vector>)
  let set-name    = command-set-name(command-set);
  unless ($editor-help-cache[0] == command-set
	  & $editor-help-cache[1] == set-name)
    let standard-commands  = standard-command-table(command-set);
    let control-X-commands = control-X-command-table(command-set);
    let control-C-commands = control-C-command-table(command-set);
    let escape-commands    = escape-command-table(command-set);
    local method find-binding
	      (comtab :: <command-table>, command) => (binding)
	    block (return)
	      for (bucket in comtab.%key-table,
		   code :: <integer> from 0)
		let bits = position(bucket, command);
		when (bits)
		  let char :: <byte-character> = as(<byte-character>, code);
		  when (upper-case?(char))
		    bits := logior(bits, $shift-key)
		  end;
		  return(vector(char, bits))
		end
	      end;
	      for (bucket keyed-by keysym in comtab.%keysym-table)
		let bits = position(bucket, command);
		when (bits)
		  return(vector(keysym, bits))
		end
	      end;
	      #f
	    end
	  end method;
    let commands :: <stretchy-object-vector> = make(<stretchy-vector>);
    local method print-bindings
	      (comtab :: false-or(<command-table>), #key prefix) => ()
	    when (comtab)
	      for (bucket in comtab.%key-table,
		   code :: <integer> from 0)
		for (command in bucket,
		     bits :: <integer> from 0)
		  let keysym = as(<byte-character>, code);
		  let bits :: <integer>
		    = if (upper-case?(keysym)) logior(bits, $shift-key) else bits end;
		  print-binding(prefix, bits, if (keysym = ' ') "Space" else as-uppercase(keysym) end, command)
		end
	      end;
	      for (bucket keyed-by keysym in comtab.%keysym-table)
		for (command in bucket,
		     bits :: <integer> from 0)
		  let keysym
		    = select (keysym)
			#"prior"  => "PageUp";
			#"next"   => "PageDn";
			otherwise => string-capitalize(as(<string>, keysym));
		      end;
		  print-binding(prefix, bits, keysym, command)
		end
	      end
	    end
	  end method,
          method print-binding
	      (prefix, bits :: <integer>, keysym, command) => ()
	    when (command
		  & command ~== self-insert
		  & command ~== numeric-argument)
	      let documentation = gethash($brief-command-help, command);
	      when (documentation)
		let binding
		  = if (prefix)
		      format-to-string("%s %s%s", prefix, $modifier-key-names[bits], keysym)
		    else
		      format-to-string("%s%s", $modifier-key-names[bits], keysym)
		    end;
		if (size(binding) < 8)
		  add!(commands, format-to-string("%s\t\t%s", binding, documentation))
		else
		  add!(commands, format-to-string("%s\t%s", binding, documentation))
		end
	      end
	    end
	  end method;
    when (find-binding(standard-commands, numeric-argument))
      add!(commands, "c-<digit>\tNumeric argument");
      add!(commands, "m-<digit>\tNumeric argument")
    end;
    print-bindings(standard-commands);
    print-bindings(control-X-commands, prefix: "c-X");
    print-bindings(control-C-commands, prefix: "c-C");
    print-bindings(escape-commands,    prefix: "Esc");
    $editor-help-cache[0] := command-set;
    $editor-help-cache[1] := set-name;
    $editor-help-cache[2] := commands
  end;
  let commands = $editor-help-cache[2];
  commands
end method compute-key-binding-documentation;


/// Scrolling and redisplay

define command scroll-forward (frame)
    "Scroll forward a page, or N lines.\n"
    "With a numeric argument, scroll by lines; otherwise scroll a page."
  let n :: <integer> = frame-numeric-arg(frame);  
  unless (n = 0)
    let state = frame-numeric-arg-state(frame);
    let direction = if (n < 0) #"backward" else #"forward" end;
    scroll-forward-or-backward(frame, direction, abs(n), state)
  end
end command scroll-forward;

define command scroll-backward (frame)
    "Scroll backward a page, or N lines.\n"
    "With a numeric argument, scroll by lines; otherwise scroll a page."
  let n :: <integer> = frame-numeric-arg(frame);  
  unless (n = 0)
    let state = frame-numeric-arg-state(frame);
    let direction = if (n < 0) #"forward" else #"backward" end;
    scroll-forward-or-backward(frame, direction, abs(n), state)
  end
end command scroll-backward;

define command scroll-forward-ext (frame)
    "Scroll forward a page, or N lines, extending the selection if the Shift key is held down.\n"
    "With a numeric argument, scroll by lines; otherwise scroll a page."
  let n :: <integer> = frame-numeric-arg(frame);  
  unless (n = 0)
    let state = frame-numeric-arg-state(frame);
    let direction = if (n < 0) #"backward" else #"forward" end;
    set-or-clear-mark(frame);
    scroll-forward-or-backward(frame, direction, abs(n), state)
  end
end command scroll-forward-ext;

define command scroll-backward-ext (frame)
    "Scroll backward a page, or N lines, extending the selection if the Shift key is held down.\n"
    "With a numeric argument, scroll by lines; otherwise scroll a page."
  let n :: <integer> = frame-numeric-arg(frame);  
  unless (n = 0)
    let state = frame-numeric-arg-state(frame);
    let direction = if (n < 0) #"forward" else #"backward" end;
    set-or-clear-mark(frame);
    scroll-forward-or-backward(frame, direction, abs(n), state)
  end
end command scroll-backward-ext;

define method scroll-forward-or-backward
    (frame :: <editor-state-mixin>, direction, n :: <integer>, state) => ()
  let window   :: <basic-window> = frame-window(frame);
  let buffer   :: <basic-buffer> = frame-buffer(frame);
  let dlines   :: <simple-object-vector> = window-display-lines(window);
  let n-lines  :: <integer> = window-n-display-lines(window);
  let degree   :: <redisplay-degree> = $display-all;
  let line     :: <basic-line> = bp-line(point());
  let fraction :: false-or(<real>) = #f;
  block (break)
    let (dline, hint) = find-display-line(window, line);
    let index = dline & (hint - 1);
    case
      state == #f | state == #"sign" =>
	// Scrolling forward/backward one page
	select (direction)
	  #"forward" =>
	    if (line == bp-line(interval-end-bp(buffer)) & index & index = 0)
	      break()
	    else
	      let dline :: <display-line> = dlines[n-lines - 1];
	      let line  :: <basic-line>   = display-line-line(dline);
	      move-point!(line, index: 0);
	      recenter-window(window, line, #"top");
	      fraction := 0.0;		// caret goes to the top of the window
	    end;
	  #"backward" =>
	    if (line == bp-line(interval-start-bp(buffer)))
	      break()
	    else
	      let dline :: <display-line> = dlines[0];
	      let line  :: <basic-line>   = display-line-line(dline);
	      move-point!(line, index: 0);
	      recenter-window(window, line, #"bottom");
	      fraction := 1.0;		// caret goes to the bottom of the window
	    end;
	end;
      otherwise =>
	// Otherwise scroll forward/backward n lines
	select (direction)
	  #"forward" =>
	    case
	      line == bp-line(interval-end-bp(buffer)) & index & index = 0 =>
	        break();
	      n < n-lines =>
		// If we're only scrolling a little, we can do this faster
		scroll-n-lines(window, n, move-point?: #t);
		break();
	      otherwise =>
		scroll-n-lines-slowly(window, n, move-point?: #t);
		break();
	    end;
	  #"backward" =>
	    case
	      line == bp-line(interval-start-bp(buffer)) =>
		break();
	      n < n-lines =>
		scroll-n-lines(window, -n, move-point?: #t);
		break();
	      otherwise =>
		scroll-n-lines-slowly(window, -n, move-point?: #t);
		break();
	    end;
	end;
    end;
    window-centering-fraction(window) := fraction;
    queue-redisplay(window, degree)
  end;
  frame-last-command-type(frame) := #"scroll"
end method scroll-forward-or-backward;


define command start-of-page (frame)
    "Put the point at the starting line on the page."
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-buffer> = window-buffer(window);
  block (break)
    local method find-page-break (line :: <basic-line>, si, ei, last?)
	    ignore(last?);
	    let index = position(line-contents(line), '\f', start: si, end: ei, from-end?: #t);
	    when (index)
	      move-point!(line, index: index);
	      break()
	    end
	  end method;
    let interval = make-interval(interval-start-bp(buffer), point());
    do-lines(find-page-break, interval, from-end?: #t);
    move-point!(interval-start-bp(buffer))
  end;
  queue-redisplay(window, $display-point, centering: -1);
  frame-last-command-type(frame) := #"motion"
end command start-of-page;

define command end-of-page (frame)
    "Put the point at the ending line on the page."
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-buffer> = window-buffer(window);
  block (break)
    local method find-page-break (line :: <basic-line>, si, ei, last?)
	    ignore(last?);
	    let index = position(line-contents(line), '\f', start: si, end: ei, from-end?: #f);
	    when (index)
	      move-point!(line, index: index + 1);
	      break()
	    end
	  end method;
    let interval = make-interval(point(), interval-end-bp(buffer));
    do-lines(find-page-break, interval, from-end?: #f);
    move-point!(interval-end-bp(buffer))
  end;
  queue-redisplay(window, $display-point, centering: 1);
  frame-last-command-type(frame) := #"motion"
end command end-of-page;


define command force-recenter (frame)
    "Recenter the point in this window.\n"
    "With no numeric argument, put the current line at the center of the window.\n"
    "With a positive numeric argument, put the current line near the top.\n"
    "With a negative numeric argument, put the current line near the bottom."
  let window :: <basic-window> = frame-window(frame);
  let n :: <integer> = frame-numeric-arg(frame);
  let state = frame-numeric-arg-state(frame);
  let line = bp-line(point());
  case
    state =>
      // Put this line at the n'th line on the screen
      recenter-window(window, line, n);
    otherwise =>
      // Put this line at the center of the screen
      recenter-window(window, line, #"center");
  end;
  set-scroll-position(window, 0, #f);
  do-force-redisplay(frame)
end command force-recenter;

define command force-redisplay (frame)
    "Forcibly redisplay the window."
  do-force-redisplay(frame)
end command force-redisplay;

// Split this out from 'force-redisplay' so that 'force-recenter'
// doesn't inadvertently reload colorization information
//--- It's a modularity kludge that we do it this way, but what the heck
define method do-force-redisplay
    (frame :: <editor-state-mixin>) => ()
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-buffer> = window-buffer(window);
  // Re-sectionize any changed sections
  resectionize-changed-sections(buffer);
  // Clear the whole window so all the bits get repainted
  let (width, height) = window-size(window);
  clear-area(window, 0, 0, width, height);
  queue-redisplay(window, $display-all);
  frame-last-command-type(frame) := #"display"
end method do-force-redisplay;


define command show-position (frame)
    "Display information about the current position"
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-buffer> = frame-buffer(frame);
  let interval
    = if (mark()) make-interval(point(), mark())
      else buffer end;
  let in-region
    = if (mark()) "in region " else "" end;
  let n-lines = count-lines(interval);
  let line-no = count-lines(make-interval(interval-start-bp(interval), point()));
  let percent = floor(100 * as(<single-float>, line-no) / as(<single-float>, n-lines));
  let index   = bp-index(point()) + 1;
  let char    = bp-character(point());
  let code    = as(<integer>, char);
  if (code < #o40 | code > #o176)
    display-message(window, "Line %d of %d %s(%d%%), column %d, char code #o%o #x%x",
		    line-no, n-lines, in-region, percent, index, code, code);
  else
    display-message(window, "Line %d of %d %s(%d%%), column %d, char '%c' (#o%o #x%x)",
		    line-no, n-lines, in-region, percent, index, char, code, code);
  end;
  frame-last-command-type(frame) := #"display"
end command show-position;


/// Simple forward and backward motion

define command forward-character (frame)
    "Move forward N characters."
  let n :: <integer> = frame-numeric-arg(frame);
  forward-or-backward-thing(frame, move-over-characters, n)
end command forward-character;

define command backward-character (frame)
    "Move backward N characters."
  let n :: <integer> = frame-numeric-arg(frame);
  forward-or-backward-thing(frame, move-over-characters, -n)
end command backward-character;


// The Windows-like version of 'forward-character'
define command forward-character-ext (frame)
    "Move forward N characters, extending the selection if the Shift key is held down."
  let n :: <integer> = frame-numeric-arg(frame);
  set-or-clear-mark(frame);
  forward-or-backward-thing(frame, move-over-characters, n)
end command forward-character-ext;

define command backward-character-ext (frame)
    "Move backward N characters, extending the selection if the Shift key is held down."
  let n :: <integer> = frame-numeric-arg(frame);
  set-or-clear-mark(frame);
  forward-or-backward-thing(frame, move-over-characters, -n)
end command backward-character-ext;

define method set-or-clear-mark (frame :: <editor-state-mixin>) => ()
  let state = frame-command-modifiers(frame);
  if (mark())
    // If there is already a mark and the motion command is not "shifted",
    // then we clear the mark
    when (logand(state, $shift-key) = 0)
      clear-mark!()
    end
  else
    // If there is not already a mark and the motion command is "shifted",
    // then we set the mark
    unless (logand(state, $shift-key) = 0)
      move-mark!(point())
    end
  end
end method set-or-clear-mark;


define command forward-word (frame)
    "Move forward N words."
  let n :: <integer> = frame-numeric-arg(frame);
  forward-or-backward-thing(frame, move-over-words, n)
end command forward-word;

define command backward-word (frame)
    "Move backward N words."
  let n :: <integer> = frame-numeric-arg(frame);
  forward-or-backward-thing(frame, move-over-words, -n)
end command backward-word;


// The Windows-like version of 'forward-word'
define command forward-word-ext (frame)
    "Move forward N words, extending the selection if the Shift key is held down."
  let n :: <integer> = frame-numeric-arg(frame);
  set-or-clear-mark(frame);
  forward-or-backward-thing(frame, move-over-words, n)
end command forward-word-ext;

define command backward-word-ext (frame)
    "Move backward N words, extending the selection if the Shift key is held down."
  let n :: <integer> = frame-numeric-arg(frame);
  set-or-clear-mark(frame);
  forward-or-backward-thing(frame, move-over-words, -n)
end command backward-word-ext;


define command forward-list (frame)
    "Move forward over N balanced lists -- (), [], {}."
  let n :: <integer> = frame-numeric-arg(frame);
  let bp   = point();
  let node = bp-node(bp) | bp-buffer(bp);
  local method move (bp :: <basic-bp>, n :: <integer>, #key fixup? = #t)
		 => (bp :: false-or(<basic-bp>))
	  move-over-lists(bp, n, fixup?: fixup?, interval: node)
	end method;
  forward-or-backward-thing(frame, move, n, message: "Unbalanced parentheses")
end command forward-list;

define command backward-list (frame)
    "Move backward over N balanced lists -- (), [], {}."
  let n :: <integer> = frame-numeric-arg(frame);
  let bp   = point();
  let node = bp-node(bp) | bp-buffer(bp);
  local method move (bp :: <basic-bp>, n :: <integer>, #key fixup? = #t)
		 => (bp :: false-or(<basic-bp>))
	  move-over-lists(bp, n, fixup?: fixup?, interval: node)
	end method;
  forward-or-backward-thing(frame, move, -n, message: "Unbalanced parentheses")
end command backward-list;


define command forward-expression (frame)
    "Move forward over N language expressions."
  let n :: <integer> = frame-numeric-arg(frame);
  let bp   = point();
  let node = bp-node(bp) | bp-buffer(bp);
  local method move (bp :: <basic-bp>, n :: <integer>, #key fixup? = #t)
		 => (bp :: false-or(<basic-bp>))
	  move-over-expressions(bp, n, fixup?: fixup?, interval: node)
	end method;
  forward-or-backward-thing(frame, move, n, message: "Unbalanced parentheses")
end command forward-expression;

define command backward-expression (frame)
    "Move backward over N language expressions."
  let n :: <integer> = frame-numeric-arg(frame);
  let bp   = point();
  let node = bp-node(bp) | bp-buffer(bp);
  local method move (bp :: <basic-bp>, n :: <integer>, #key fixup? = #t)
		 => (bp :: false-or(<basic-bp>))
	  move-over-expressions(bp, n, fixup?: fixup?, interval: node)
	end method;
  forward-or-backward-thing(frame, move, -n, message: "Unbalanced parentheses")
end command backward-expression;


define method forward-or-backward-thing
    (frame :: <editor-state-mixin>, function :: <function>, n :: <integer>,
     #key message) => ()
  let window :: <basic-window> = frame-window(frame);
  let bp :: false-or(<basic-bp>) = function(point(), n, fixup?: abs(n) > 1);
  if (bp)
    move-point!(bp);
    queue-redisplay(window, $display-point, centering: n);
    frame-last-command-type(frame) := #"motion"
  else
    command-error(message | "Can't move any further")
  end
end method forward-or-backward-thing;


/// Simple "up" and "down" motion

define command next-line (frame)
    "Move to the N'th next line."
  let n :: <integer> = frame-numeric-arg(frame);
  next-or-previous-line(frame, n)
end command next-line;

define command previous-line (frame)
    "Move to the N'th previous line."
  let n :: <integer> = frame-numeric-arg(frame);
  next-or-previous-line(frame, -n)
end command previous-line;

// The Windows-like version of 'next-line'
define command next-line-ext (frame)
    "Move to the N'th next line, extending the selection if the Shift key is held down."
  let n :: <integer> = frame-numeric-arg(frame);
  set-or-clear-mark(frame);
  next-or-previous-line(frame, n)
end command next-line-ext;

define command previous-line-ext (frame)
    "Move to the N'th previous line, extending the selection if the Shift key is held down."
  let n :: <integer> = frame-numeric-arg(frame);
  set-or-clear-mark(frame);
  next-or-previous-line(frame, -n)
end command previous-line-ext;

define method next-or-previous-line
    (frame :: <editor-state-mixin>, n :: <integer>, #key preferred-x = #f) => ()
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-buffer> = frame-buffer(frame);
  let mode = buffer-major-mode(buffer);
  let bp  = point();
  let nbp = move-over-lines(bp, n, fixup?: abs(n) > 1);
  if (~nbp)
    // Moved past the end of the buffer
    if (n < 0 | ~next-line-adds-newline?(editor-policy(frame-editor(frame))))
      command-error("Can't move any further");
    else
      // Moved one line past the end -- insert a new line
      let ebp = interval-end-bp(buffer);
      check-read-only(ebp);
      with-change-recording (buffer, <paste-change-record>, start-bp: ebp)
        move-point!(insert-moving!(ebp, '\n'))
      end;
      queue-redisplay(window, $display-text, centering: n)
    end
  else
    let line   = bp-line(nbp);
    let goal-x = preferred-x
		 | window-goal-x-position(window)
		 | index->position(bp-line(bp), mode, window, bp-index(bp));
    let index  = position->index(line, mode, window, goal-x);
    move-point!(line, index: index);
    queue-redisplay(window, $display-point, centering: n)
  end;
  frame-last-command-type(frame) := #"line-motion"
end method next-or-previous-line;


define command start-of-line (frame)
    "Move to the start of the current line."
  let window :: <basic-window> = frame-window(frame);
  let line = bp-line(point());
  move-point!(line, index: 0);
  queue-redisplay(window, $display-point, centering: -1);
  frame-last-command-type(frame) := #"motion"
end command start-of-line;

define command end-of-line (frame)
    "Move to the end of the current line."
  let window :: <basic-window> = frame-window(frame);
  let line = bp-line(point());
  move-point!(line, index: line-length(line));
  queue-redisplay(window, $display-point, centering: 1);
  frame-last-command-type(frame) := #"motion"
end command end-of-line;


// The Windows-like version of 'start-of-line'
define command start-of-line-ext (frame)
    "Move to the start of the current line, extending the selection if the Shift key is held down."
  let window :: <basic-window> = frame-window(frame);
  let bp   = point();
  let line = bp-line(bp);
  set-or-clear-mark(frame);
  if (bp-index(bp) ~= 0)
    move-point!(line, index: 0)
  else
    // If already at the beginning of the line, move over the whitespace
    let bp = forward-over!(line-start(line), #[' ', '\t']);
    move-point!(bp)
  end;
  queue-redisplay(window, $display-point, centering: -1);
  frame-last-command-type(frame) := #"motion"
end command start-of-line-ext;

define command end-of-line-ext (frame)
    "Move to the end of the current line, extending the selection if the Shift key is held down."
  let window :: <basic-window> = frame-window(frame);
  let line = bp-line(point());
  set-or-clear-mark(frame);
  move-point!(line, index: line-length(line));
  queue-redisplay(window, $display-point, centering: 1);
  frame-last-command-type(frame) := #"motion"
end command end-of-line-ext;


define command start-of-buffer (frame)
    "Move to the start of the buffer."
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-buffer> = frame-buffer(frame);
  push-point-pdl!(window, point());
  move-point!(interval-start-bp(buffer));
  queue-redisplay(window, $display-point, centering: -1);
  frame-last-command-type(frame) := #"motion"
end command start-of-buffer;

define command end-of-buffer (frame)
    "Move to the end of the buffer."
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-buffer> = frame-buffer(frame);
  push-point-pdl!(window, point());
  move-point!(interval-end-bp(buffer));
  queue-redisplay(window, $display-point, centering: 1);
  frame-last-command-type(frame) := #"motion"
end command end-of-buffer;


define command start-of-section (frame)
    "Move to the start of the section."
  let window :: <basic-window> = frame-window(frame);
  let bp = point();
  push-point-pdl!(window, bp);
  let line = section-start-line(line-section(bp-line(bp)));
  when (bp-line(bp) == line)
    // Already at the beginning of this section, move to the previous one
    let node = line-node(line);
    let prev = node & node-previous(node);
    when (prev)
      line := section-start-line(node-section(prev))
     end
  end;
  move-point!(line, index: 0);
  queue-redisplay(window, $display-point, centering: -1);
  frame-last-command-type(frame) := #"motion"
end command start-of-section;

define command end-of-section (frame)
    "Move to the end of the section."
  let window :: <basic-window> = frame-window(frame);
  let bp = point();
  push-point-pdl!(window, bp);
  let line = section-end-line(line-section(bp-line(bp)));
  when (bp-line(bp) == line)
    // Already at the end of this section, move to the next one
    let node = line-node(line);
    let next = node & node-next(node);
    when (next)
      line := section-end-line(node-section(next))
     end
  end;
  move-point!(line, index: 0);
  queue-redisplay(window, $display-point, centering: 1);
  frame-last-command-type(frame) := #"motion"
end command end-of-section;


/// Complex motion

define command down-list (frame)
    "Move down into the next nested list -- (), [], {}."
  let window :: <basic-window> = frame-window(frame);
  let n :: <integer> = frame-numeric-arg(frame);
  let bp   = point();
  let node = bp-node(bp) | bp-buffer(bp);
  move-point!(move-up-or-down-lists(bp, n, fixup?: abs(n) > 1, interval: node)
              | command-error("Can't move any further down in the nesting"));
  queue-redisplay(window, $display-point, centering: n);
  frame-last-command-type(frame) := #"motion"
end command down-list;

define command up-list (frame)
    "Move up out of this nested list -- (), [], {}."
  let window :: <basic-window> = frame-window(frame);
  let n :: <integer> = frame-numeric-arg(frame);
  let bp   = point();
  let node = bp-node(bp) | bp-buffer(bp);
  move-point!(move-up-or-down-lists(bp, -n, fixup?: abs(n) > 1, interval: node)
              | command-error("Can't move any further up in the nesting"));
  queue-redisplay(window, $display-point, centering: -n);
  frame-last-command-type(frame) := #"motion"
end command up-list;


define command down-expression (frame)
    "Move down into the next nested language expression."
  let window :: <basic-window> = frame-window(frame);
  let n :: <integer> = frame-numeric-arg(frame);
  let bp   = point();
  let node = bp-node(bp) | bp-buffer(bp);
  move-point!(move-up-or-down-expressions(bp, n, fixup?: abs(n) > 1, interval: node)
              | command-error("Can't move any further"));
  queue-redisplay(window, $display-point, centering: n);
  frame-last-command-type(frame) := #"motion"
end command down-expression;

define command up-expression (frame)
    "Move up out of this nested language expression."
  let window :: <basic-window> = frame-window(frame);
  let n :: <integer> = frame-numeric-arg(frame);
  let bp   = point();
  let node = bp-node(bp) | bp-buffer(bp);
  move-point!(move-up-or-down-expressions(bp, -n, fixup?: abs(n) > 1, interval: node)
              | command-error("Can't move any further"));
  queue-redisplay(window, $display-point, centering: -n);
  frame-last-command-type(frame) := #"motion"
end command up-expression;


/// Direct navigation

define command goto-character (frame)
    "Go to the N'th character in the buffer."
  goto-thing(frame, #"character");
end command goto-character;

define command goto-line (frame)
    "Go to the N'th line in the buffer."
  goto-thing(frame, #"line");
end command goto-line;

define method goto-thing
    (frame :: <editor-state-mixin>, what :: false-or(<goto-target-type>)) => ()
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-buffer> = frame-buffer(frame);
  let n :: false-or(<integer>) = #f;
  if (frame-numeric-arg-state(frame))
    // Use the numeric arg...
    n := frame-numeric-arg(frame)
  else
    // ...or pop up a dialog if no arg was set (usual on Windows)
    let (_n, _what) = goto-position-dialog(window, what | #"line");
    n    := _n;
    what := _what
  end;
  when (n & (n > 0))
    let bp
      = select (what)
	  #"line"      => line-index->bp(buffer, n - 1);
	  #"character" => char-index->bp(buffer, n - 1);
	end;
    when (bp)
      queue-redisplay(window, $display-point, centering: 0);
      unless (bp-line(bp) = bp-line(point()))
	push-point-pdl!(window, point())
      end;
      move-point!(bp)
    end
  end;
  frame-last-command-type(frame) := #"motion"
end method goto-thing;


/// Deletion

define command delete-character (frame)
    "Delete the next N characters, or the current selection."
  let n :: <integer> = frame-numeric-arg(frame);
  delete-or-rubout-char(frame, n)
end command delete-character;

define command rubout-character (frame)
    "Delete the previous N characters, or the current selection."
  let n :: <integer> = frame-numeric-arg(frame);
  delete-or-rubout-char(frame, -n)
end command rubout-character;

define method delete-or-rubout-char
    (frame :: <editor-state-mixin>, n :: <integer>) => ()
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-buffer> = frame-buffer(frame);
  if (typing-replaces-selection?(editor-policy(frame-editor(frame))) & mark())
    let bp1 = point();
    let bp2 = mark();
    let interval = make-interval(bp1, bp2);
    check-read-only(interval);
    clear-mark!();
    queue-region-redisplay(window, bp1, bp2);
    with-change-recording (buffer, <kill-change-record>, interval: interval)
      move-point!(kill!(interval,
			merge?: frame-last-command-type(frame) == #"kill")
		  | command-error("Can't delete across hard sections"))
    end;
    frame-last-command-type(frame) := #"delete"
  else
    let bp1 = point();
    let bp2 = move-over-characters(bp1, n);
    let interval = make-interval(bp1, bp2);
    check-read-only(interval);
    clear-mark!();
    queue-region-redisplay(window, bp1, bp2);
    // Regions longer than a single character and diagram lines go onto
    // the kill ring, otherwise let undo take care of it
    let kill? = (diagram-line?(bp-line(bp1)) | abs(n) > 1);
    let class = if (kill?) <kill-change-record> else <delete-change-record> end;
    with-change-recording (buffer, class, interval: interval)
      if (kill?)
	move-point!(kill!(interval,
			  merge?:   frame-last-command-type(frame) == #"kill",
			  reverse?: n < 0)
		    | command-error("Can't delete across hard sections"));
	frame-last-command-type(frame) := #"kill"
      else
	move-point!(delete!(interval)
		    | command-error("Can't delete across hard sections"));
	frame-last-command-type(frame) := #"delete"
      end
    end
  end
end method delete-or-rubout-char;


define command delete-word (frame)
    "Delete the next N words."
  let n :: <integer> = frame-numeric-arg(frame);
  kill-region(frame, n, move-over-words)
end command delete-word;

define command rubout-word (frame)
    "Delete the previous N words."
  let n :: <integer> = frame-numeric-arg(frame);
  kill-region(frame, -n, move-over-words)
end command rubout-word;


define command delete-list (frame)
    "Delete the next N lists -- (), [], {}."
  let n :: <integer> = frame-numeric-arg(frame);
  kill-region(frame, n, move-over-lists)
end command delete-list;

define command rubout-list (frame)
    "Delete the previous N lists -- (), [], {}."
  let n :: <integer> = frame-numeric-arg(frame);
  kill-region(frame, -n, move-over-lists)
end command rubout-list;


define command delete-expression (frame)
    "Delete the next N language expressions."
  let n :: <integer> = frame-numeric-arg(frame);
  kill-region(frame, n, move-over-expressions)
end command delete-expression;

define command rubout-expression (frame)
    "Delete the previous N language expressions."
  let n :: <integer> = frame-numeric-arg(frame);
  kill-region(frame, -n, move-over-expressions)
end command rubout-expression;


define method kill-region
    (frame :: <editor-state-mixin>, n :: <integer>, function :: <function>) => ()
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-buffer> = frame-buffer(frame);
  let bp1 = point();
  let bp2 = function(bp1, n);
  let interval = make-interval(bp1, bp2);
  check-read-only(interval);
  clear-mark!();
  queue-region-redisplay(window, bp1, bp2, centering: n);
  with-change-recording (buffer, <kill-change-record>, interval: interval)
    move-point!(kill!(interval,
		      merge?:   frame-last-command-type(frame) == #"kill",
		      reverse?: n < 0)
		| command-error("Can't delete across hard sections"))
  end;
  frame-last-command-type(frame) := #"kill"
end method kill-region;


/// Cut and paste

define command cut-region (frame)
    "Cut the current selection and add to the clipboard and the kill ring."
  cut-or-delete-region(frame, clipboard?: #t)
end command cut-region;

define command delete-region (frame)
    "Delete the current selection and add it to the kill ring but not the clipboard."
  if (mark())
    cut-or-delete-region(frame, clipboard?: #f)
  else
    delete-character(frame)		//--- kludge for Delete accelerator on Windows
  end
end command delete-region;

define method cut-or-delete-region
    (frame :: <editor-state-mixin>, #key clipboard? = $unsupplied) => ()
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-buffer> = frame-buffer(frame);
  let bp1 = point();
  let bp2 = mark();
  if (bp2)
    let interval = make-interval(bp1, bp2);
    check-read-only(interval);
    clear-mark!();
    queue-region-redisplay(window, bp1, bp2);
    with-change-recording (buffer, <kill-change-record>, interval: interval)
      move-point!(kill!(interval,
			merge?: frame-last-command-type(frame) == #"kill",
			clipboard?: clipboard?)
		  | command-error("Can't delete across hard sections"))
    end;
    frame-last-command-type(frame) := #"kill"
  else
    command-error("There is no selected region")
  end
end method cut-or-delete-region;

define command copy-region (frame)
    "Copy the current selection to the clipboard and the kill ring."
  let window :: <basic-window> = frame-window(frame);
  let bp1 = point();
  let bp2 = mark();
  let policy = editor-policy(frame-editor(frame));
  when (~bp2 & unselected-copy-policy(policy) == #"copy-line")
    let nbp = move-over-lines(bp1, 1, fixup?: #f);
    bp1 := line-start(bp-line(bp1));
    bp2 := if (nbp) line-start(bp-line(nbp)) else line-end(bp-line(bp1)) end
  end;
  if (bp2)
    unless (typing-replaces-selection?(policy))		// i.e., when Emacs-like
      clear-mark!()
    end;
    queue-redisplay(window, $display-region);
    let interval = make-interval(bp1, bp2);
    add-to-clipboard(window, interval);
    let kill-ring = editor-kill-history(frame-editor(frame));
    history-push(kill-ring, copy-interval(interval));
    frame-last-command-type(frame) := #"kill"
  else
    command-error("There is no selected region")
  end
end command copy-region;

define command paste (frame)
    "Paste the most recent clipboard item at the current position."
  let window :: <basic-window> = frame-window(frame);
  let elt      = get-from-clipboard(window, <byte-string>);
  let temp?    = window-temporary-mark?(window);
  let replace? = mark() & typing-replaces-selection?(editor-policy(frame-editor(frame)));
  if (elt)
    when (temp? & temp? ~== #t)
      // If it's a temporary region indicated by a BP, move the
      // point to that BP.  See 'with-temporary-region' for details
      move-point!(temp?)
    end;
    insert-yanked-element(frame, elt, replace?: replace? & ~temp?)
  else
    command-error("There is nothing on the clipboard")
  end
end command paste;

define command yank (frame)
    "Paste the most recent clipboard or kill ring item at the current position.\n"
    "With a numeric argument N, insert the N'th most recently killed selection."
  let window :: <basic-window> = frame-window(frame);
  let state = frame-numeric-arg-state(frame);
  let n :: <integer> = frame-numeric-arg(frame);
  unless (n = 0)
    let kill-ring = editor-kill-history(frame-editor(frame));
    initialize-yank-state(kill-ring);
    let index = (state & state ~== #"universal") & n;
    let elt   = yank-from-kill-ring(kill-ring, window, index: index);
    if (elt)
      insert-yanked-element(frame, elt)
    else
      command-error("There is nothing on the kill ring")
    end
  end
end command yank;

define command yank-next (frame)
    "Paste the next kill ring item at the current position, rotating the kill ring.\n"
    "With a numeric argument N, insert the N'th most recently killed selection."
  if (frame-last-command-type(frame) == #"yank"
      | frame-last-command-type(frame) == #"yank-no-motion")
    let window :: <basic-window> = frame-window(frame);
    let state = frame-numeric-arg-state(frame);
    let n :: <integer> = frame-numeric-arg(frame);
    unless (n = 0)
      let kill-ring = editor-kill-history(frame-editor(frame));
      let index = (state & state ~== #"universal") & n;
      let elt   = yank-from-kill-ring(kill-ring, window, index: index);
      when (elt)
	insert-yanked-element(frame, elt, replace?: #t)
      end
    end
  else
    command-error("The last command was not a yank")
  end
end command yank-next;

define method insert-yanked-element
    (frame :: <editor-state-mixin>, elt, #key replace? = #f) => ()
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-buffer> = frame-buffer(frame);
  let bp1    = copy-bp(point());
  let bp2    = replace? & (mark() | window-last-mark(window));
  let nlines = 0;
  // Don't move the point on commands like c-U c-Y...
  let move?  = ~(frame-numeric-arg-state(frame) == #"universal"
		 | frame-last-command-type(frame) == #"yank-no-motion");
  clear-mark!();
  with-change-recording (buffer, <paste-change-record>, start-bp: bp1, end-bp: bp2)
    when (bp2)
      let interval = make-interval(bp1, bp2);
      check-read-only(interval);
      dec!(nlines, count-lines(interval, skip-test: #f) - 1);
      bp1 := delete!(interval)
    end;
    check-read-only(bp1);
    // We don't set the mark directly, because that will cause the yanked
    // stuff to get highlighted, which is both ugly and slows down redisplay
    inc!(nlines, count-lines(elt, skip-test: #f) - 1);
    let bp2 = insert!(bp1, elt);
    if (move?)
      window-last-mark(window) := copy-bp(bp1);
      move-point!(bp2)
    else
      window-last-mark(window) := copy-bp(bp2);
    end;
    // Try to use $display-blt if it will do us some good
    let line  = bp-line(bp1);
    let (dline, hint) = find-display-line(window, line);
    let index = dline & (hint - 1);
    case
      nlines <= 0 =>
	// If we're not adding any new line, there's no need to recenter
	queue-region-redisplay(window, bp1, bp2, centering: #f);
      index + nlines >= window-n-display-lines(window) =>
	// The end of the display is off the screen, so we need to recenter
	queue-region-redisplay(window, bp1, bp2, centering: 1);
      otherwise =>
	// All the new lines will be on-screen, so we can use bitblt
        queue-redisplay(window, $display-blt, line: line, index: nlines);
    end
  end;
  frame-last-command-type(frame) := (if (move?) #"yank" else #"yank-no-motion" end);
end method insert-yanked-element;


/// Line hacking

define command insert-newline (frame)
    "Insert a newline at the current position."
  do-insert-newline(frame);
  frame-last-command-type(frame) := #"insert"
end command insert-newline;
  
define command indent-newline (frame)
    "Insert a newline at the current position and indent the new line."
  do-insert-newline(frame);
  indent-line(frame);
  frame-last-command-type(frame) := #"insert"
end command indent-newline;
  
define method do-insert-newline
    (frame :: <editor-state-mixin>) => (bp :: <basic-bp>)
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-buffer> = frame-buffer(frame);
  let bp  = point();
  let bp2 = typing-replaces-selection?(editor-policy(frame-editor(frame))) & mark();
  check-read-only(bp);
  if (bp2)
    with-change-recording (buffer, <replace-change-record>,
			   start-bp: bp, end-bp: bp2, moving?: #t)
      let interval = make-interval(bp, bp2);
      check-read-only(interval);
      queue-region-redisplay(window, bp, bp2);
      bp := kill!(interval)
	    | command-error("Can't delete across hard sections");
      clear-mark!();
      move-point!(insert-moving!(bp, '\n'))
    end
  else
    clear-mark!();
    let line = bp-line(bp);
    with-change-recording (buffer, <paste-change-record>, start-bp: bp)
      move-point!(insert-moving!(bp, '\n'))
    end;
    // Center the new line near the bottom of the window, avoiding bitblt
    // if it's a new final line in the buffer
    let last? = (bp-line(bp) == bp-line(interval-end-bp(buffer)));
    if (last?)
      queue-redisplay(window, $display-text, centering: 1)
    else
      queue-redisplay(window, $display-blt, line: line, index: 1, centering: 1)
    end
  end;
  bp
end method do-insert-newline;


define command open-line (frame)
    "Open up a new line at the current position."
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-buffer> = frame-buffer(frame);
  let bp   = copy-bp(point());
  let line = bp-line(bp);
  check-read-only(bp);
  clear-mark!();
  with-change-recording (buffer, <paste-change-record>, start-bp: bp)
    insert!(bp, '\n');
  end;
  queue-redisplay(window, $display-blt, line: line, index: 1);
  frame-last-command-type(frame) := #"insert"
end command open-line;

define command split-line (frame)
    "Split the line at the current position."
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-buffer> = frame-buffer(frame);
  let mode = buffer-major-mode(buffer);
  let bp   = copy-bp(point());
  let line = bp-line(bp);
  check-read-only(bp);
  clear-mark!();
  with-change-recording (buffer, <paste-change-record>, start-bp: bp)
    let space-width = string-size(window, " ");
    let indentation = index->position(line, mode, window, bp-index(bp)) - line-margin(line, mode, window);
    let n-spaces = floor/(indentation, space-width);
    let spaces = make(<byte-string>, size: n-spaces, fill: ' ');
    insert-moving!(bp, '\n');
    insert-moving!(bp, spaces)
  end;
  queue-redisplay(window, $display-blt, line: line, index: 1);
  frame-last-command-type(frame) := #"insert"
end command split-line;


define command kill-line (frame)
    "Kill the current line starting from the current position.\n"
    "With a numeric argument, kill that many lines."
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-buffer> = frame-buffer(frame);
  let bp1 = point();
  let bp2 = #f;
  let line  = bp-line(bp1);
  let index = bp-index(bp1);
  let reverse? = #f;
  clear-mark!();
  case
    frame-numeric-arg-state(frame) =>
      // Numeric arg means to kill that many lines
      let n :: <integer> = frame-numeric-arg(frame);
      unless (n = 0)
	bp2 := move-over-lines(bp1, n);
	reverse? := (n < 0)
      end;
    line-empty?(line, index: index) =>
      // If the line has nothing but whitespace past the current position,
      // merge the next line to the end of this one
      let next = line-next-in-buffer(line, buffer);
      if (next)
	bp2 := line-start(next)
      else
	command-error("Nothing to delete")
      end;
    otherwise =>
      // Otherwise just truncate this line from the current point
      bp2 := line-end(line);
  end;
  when (bp2)
    let interval = make-interval(bp1, bp2);
    check-read-only(interval);
    //--- This should somehow use $display-blt as its redisplay degree
    queue-region-redisplay(window, bp1, bp2);
    with-change-recording (buffer, <kill-change-record>, interval: interval)
      move-point!(kill!(interval,
			merge?:   frame-last-command-type(frame) == #"kill",
			reverse?: reverse?)
		  | command-error("Can't delete across hard sections"))
    end
  end;
  frame-last-command-type(frame) := #"kill"
end command kill-line;

define command join-lines (frame)
    "Join this line to the previous line."
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-buffer> = frame-buffer(frame);
  let bp = point();
  let this-bp = if (line-empty?(bp-line(bp))) bp
		else forward-over!(line-start(bp-line(bp)), #[' ', '\t']) end;
  let prev-bp = move-over-lines(this-bp, -1);
  let this-line = bp-line(this-bp);
  let prev-line = bp-line(prev-bp);
  unless (this-line == prev-line)
    let bp1 = line-end(prev-line);
    let bp2 = this-bp;
    let interval = make-interval(bp1, bp2);
    check-read-only(interval);
    queue-redisplay(window, $display-text);
    with-change-recording (buffer, <replace-change-record>, interval: interval)
      let bp = delete!(interval);
      if (  word-syntax(bp-character(bp)) == $word-alphabetic
          & word-syntax(bp-character-before(bp)) == $word-alphabetic)
        move-point!(insert-moving!(bp, ' '))
      else
        move-point!(bp)
      end
    end
  end;
  frame-last-command-type(frame) := #"delete"
end command join-lines;


/// Tranposition commands

define command transpose-characters (frame)
    "Transpose the two characters around the current position."
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-buffer> = frame-buffer(frame);
  let this = copy-bp(point());
  when (end-of-line?(this))
    this := decrement-bp!(this, fixup?: #f)
  end;
  let n :: <integer> = if (start-of-line?(this)) -2 else -1 end;
  let prev = this & move-over-characters(this, n, fixup?: #f);
  let next = this & move-over-characters(this, 1, fixup?: #f);
  when (prev & next)
    let ch1 = bp-character(this);
    let ch2 = bp-character(prev);
    unless (ch1 = '\n' | ch2 = '\n')
      check-read-only(this);
      check-read-only(prev);
      with-change-recording (buffer, <replace-change-record>,
			     start-bp: prev, end-bp: next)
	bp-character(this) := ch2;
	bp-character(prev) := ch1;
	note-line-changed(bp-line(this));
	note-line-changed(bp-line(prev))
      end;
      queue-region-redisplay(window, prev, this);
      move-point!(increment-bp!(this))
    end
  end;
  frame-last-command-type(frame) := #"insert"
end command transpose-characters;


define command transpose-words (frame)
    "Transpose the two words around the current position."
  transpose-regions(frame, move-over-words)
end command transpose-words;

define command transpose-lists (frame)
    "Transpose the two lists around the current position."
  transpose-regions(frame, move-over-lists)
end command transpose-lists;

define command transpose-expressions (frame)
    "Transpose the two language expressions around the current position."
  transpose-regions(frame, move-over-expressions)
end command transpose-expressions;

define command transpose-lines (frame)
    "Transpose the current line with the previous one."
  let buffer :: <basic-buffer> = frame-buffer(frame);
  let line = bp-line(point());
  let prev = line-previous-in-buffer(line, buffer);
  if (prev)
    do-transpose-regions
      (frame, line-start(prev), line-end(prev), line-start(line), line-end(line))
  else
    command-error("There is no previous line")
  end
end command transpose-lines;

define method transpose-regions
    (frame :: <editor-state-mixin>, function :: <function>) => ()
  let bp     = point();
  let end2   = function(bp, 1, fixup?: #f);
  let start2 = end2 & function(end2, -1, fixup?: #f);
  let start1 = start2 & function(start2, -1, fixup?: #f);
  let end1   = start1 & function(start1, 1, fixup?: #f);
  when (end1 & start1 ~= start2)
    do-transpose-regions(frame, start1, end1, start2, end2)
  end
end method transpose-regions;

define method do-transpose-regions
    (frame :: <editor-state-mixin>,
     start1 :: <basic-bp>, end1 :: <basic-bp>,
     start2 :: <basic-bp>, end2 :: <basic-bp>) => ()
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-buffer> = frame-buffer(frame);
  let interval1 = make-interval(start1, end1, in-order?: #t);
  let interval2 = make-interval(end1, start2, in-order?: #t);
  let interval3 = make-interval(start2, end2, in-order?: #t);
  check-read-only(interval1);
  check-read-only(interval2);
  let s1 = as(<byte-string>, interval1);
  let s2 = as(<byte-string>, interval2);
  let s3 = as(<byte-string>, interval3);
  queue-region-redisplay(window, start1, end2);
  with-change-recording (buffer, <replace-change-record>,
			 start-bp: start1, end-bp: end2)
    if (s2 = "\n")
      // Be a bit more efficient when twiddling around a newline
      delete!(interval1);
      delete!(interval3);
      insert!(start1, s3);
      insert-moving!(start2, s1);
      move-point!(start2)
    else
      delete!(make-interval(start1, end2, in-order?: #t));
      insert-moving!(start1, s3);
      insert-moving!(start1, s2);
      insert-moving!(start1, s1);
      move-point!(start1)
    end
  end;
  frame-last-command-type(frame) := #"insert"
end method do-transpose-regions;


/// Case-changing commands

define command upcase-word (frame)
    "Make the next N words be all uppercase."
  let n :: <integer> = frame-numeric-arg(frame);
  change-word-case(frame, n, as-uppercase)
end command upcase-word;

define command downcase-word (frame)
    "Make the next N words be all lowercase."
  let n :: <integer> = frame-numeric-arg(frame);
  change-word-case(frame, n, as-lowercase)
end command downcase-word;

define method change-word-case
    (frame :: <editor-state-mixin>, n :: <integer>, function :: <function>) => ()
  let bp1 = copy-bp(point());
  let bp2 = move-over-words(bp1, n);
  change-region-case(frame, bp1, bp2, function, n < 0)
end method change-word-case;


define command upcase-region (frame)
    "Uppercase the current selection."
  let bp1 = copy-bp(point());
  let bp2 = mark();
  if (bp2)
    change-region-case(frame, bp1, bp2, as-uppercase, bp-less?(bp2, bp1))
  else
    upcase-word(frame)		// upcase the next word if there's no region
  end
end command upcase-region;

define command downcase-region (frame)
    "Lowercase the current selection."
  let bp1 = copy-bp(point());
  let bp2 = mark();
  if (bp2)
    change-region-case(frame, bp1, bp2, as-lowercase, bp-less?(bp2, bp1))
  else
    downcase-word(frame)	// downcase the next word if there's no region
  end
end command downcase-region;

define method change-region-case
    (frame :: <editor-state-mixin>, bp1 :: <basic-bp>, bp2 :: <basic-bp>,
     function :: <function>, reverse? :: <boolean>) => ()
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-buffer> = frame-buffer(frame);
  let interval = make-interval(bp1, bp2); 
  check-read-only(interval);
  local method change-case (line :: <line>, si :: <integer>, ei :: <integer>, last?)
	  ignore(last?);
	  let line = note-line-changed(line);
	  let contents = line-contents(line);
	  for (i :: <integer> from si below ei)
	    let old = contents[i];
	    let new = function(old);
	    contents[i] := new;
	    // Start redisplay at the first changed character
	    unless (old == new)
	      move-bp!(bp1, line, i)
            end
	  end
	end method;
  clear-mark!();
  queue-region-redisplay(window, bp1, bp2, centering: #f);
  with-change-recording (buffer, <replace-change-record>, interval: interval)
    do-lines(change-case, interval, skip-test: diagram-line?)
  end;
  move-point!(interval-end-bp(interval));
  frame-last-command-type(frame) := #"insert"
end method change-region-case;


define command capitalize-word (frame)
    "Capitalize the next N words."
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-buffer> = frame-buffer(frame);
  let n :: <integer> = frame-numeric-arg(frame);
  let bp = copy-bp(point());
  when (n < 0)
    bp := move-over-words(bp, n, fixup?: #t);
    n  := abs(n)
  end;
  when (n > 0)
    for (i :: <integer> from 0 below n,
	 while: bp)
      let sbp = bp;
      let ebp = move-over-words(sbp, 1, fixup?: #f);
      when (ebp)
        let interval = make-interval(sbp, ebp);
	check-read-only(interval);
	clear-mark!();
	queue-region-redisplay(window, sbp, ebp, centering: 1);
	with-change-recording (buffer, <replace-change-record>, interval: interval)
	  do-lines(method (line :: <line>, si, ei, last?)
		     ignore(si, ei, last?);
		     note-line-changed(line)
		   end method, interval, skip-test: diagram-line?);
	  let state = #f;
	  until (sbp = ebp)
	    let ch :: <byte-character> = bp-character(sbp);
	    if (state)
	      if (alpha-char?(ch))
		bp-character(sbp) := as-lowercase(ch)
	      else
		state := #f
	      end
	    else
	      when (alpha-char?(ch))
		bp-character(sbp) := as-uppercase(ch);
		state := #t
	      end
	    end;
	    increment-bp!(sbp)
	  end
	end;
	move-point!(ebp)
      end;
      bp := ebp
    end
  end;
end command capitalize-word;


/// Font-changing commands

define command change-region-font (frame)
    "Change the font of the current selection."
  let window :: <basic-window> = frame-window(frame);
  let n :: <integer> = frame-numeric-arg(frame);
  let (bp1, bp2)
    = if (mark())
	values(point(), mark())
      else
	let bp1 = copy-bp(point());
	let bp2 = move-over-words(bp1, n);
	values(bp1, bp2)
      end;
  display-message(window, "Change font: ");
  let char = read-character(window);
  select (char)
    'b', 'B' =>
      display-message(window, "Change font: weight to bold");
      change-font(frame, bp1, bp2, weight: #"bold");
    'n', 'N' =>
      display-message(window, "Change font: weight to normal");
      change-font(frame, bp1, bp2, weight: #"normal");
    'i', 'I' =>
      display-message(window, "Change font: slant to italic");
      change-font(frame, bp1, bp2, slant: #"italic");
    'r', 'R' =>
      display-message(window, "Change font: slant to roman");
      change-font(frame, bp1, bp2, slant: #"roman");
    '+' =>
      display-message(window, "Change font: larger size");
      change-font(frame, bp1, bp2, size: #"larger");
    '-' =>
      display-message(window, "Change font: smaller size");
      change-font(frame, bp1, bp2, size: #"smaller");
    'f', 'F' =>
      display-message(window, "Change font: face: ");
      let char = read-character(window);
      let face = select (char)
		   's', 'S' => #"serif";	// 's' for Swiss or Serif
		   'n', 'N' => #"sans-serif";	// 'n' for saNs-serif
		   'h', 'H' => #"sans-serif";	// 'h' for Helvetica
		   'f', 'F' => #"fixed";	// 'f' for Fixed
		   'c', 'C' => #"fixed";	// 'c' for Courier
		   '?' =>
		     display-message(window,
				     "'s' for serif, 'n' for sans-serif, 'f' for fixed.");
		     #f;
		   otherwise =>
		     error(make(<command-error>, window: window));
		 end;
      when (face)
	display-message(window, "Change font: face to %s",
			if (face == #"serif") "serif"
			elseif (face == #"sans-serif") "sans-serif"
			else "fix" end);
	change-font(frame, bp1, bp2, face: face)
      end;
    '?' =>
      display-message(window,
		      "'b' or 'n' for weight, 'i' or 'r' for slant, "
		      "'+' or '-' for size, 'f' for face.");
    otherwise =>
      error(make(<command-error>, window: window));
  end
end command change-region-font;

// We define this as a standalone command for use by tool bars
define command change-region-weight (frame)
    "Change the weight of the current selection to be bold (or back to normal)."
  let (bp1, bp2)
    = if (mark()) values(point(), mark()) else atom-under-bp(point()) end;
  let weight
    = if (frame-numeric-arg-state(frame)) #"normal" else #"bold" end;
  change-font(frame, bp1, bp2, weight: weight)
end command change-region-weight;

// We define this as a standalone command for use by tool bars
define command change-region-slant (frame)
    "Change the slant of the current selection to be italic (or back to roman)."
  let (bp1, bp2)
    = if (mark()) values(point(), mark()) else atom-under-bp(point()) end;
  let slant
    = if (frame-numeric-arg-state(frame)) #"roman" else #"italic" end;
  change-font(frame, bp1, bp2, slant: slant)
end command change-region-slant;

define method change-font
    (frame :: <editor-state-mixin>, bp1 :: <basic-bp>, bp2 :: <basic-bp>,
     #key face = #f, weight = #f, slant = #f, size = #f) => ()
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-buffer> = frame-buffer(frame);
  let reverse? = bp-less?(bp2, bp1);
  let interval = make-interval(bp1, bp2, in-order?: ~reverse?); 
  check-read-only(interval);
  local method set-font (line :: <line>, si :: <integer>, ei :: <integer>, last?)
	  ignore(last?);
	  let line = note-line-changed(line);
	  //---*** Do it
          //---*** Requires "fattening" and "thinning" lines as needed,
          //---*** but how do we maintain line identity in the current design?
	end method;
  clear-mark!();
  queue-region-redisplay(window, bp1, bp2, centering: #f);
  with-change-recording (buffer, <replace-change-record>, interval: interval)
    do-lines(set-font, interval, skip-test: diagram-line?)
  end;
  move-point!(interval-end-bp(interval));
  frame-last-command-type(frame) := #"insert"
end method change-font;


/// Searching and replacing

define command find-string (frame)
    "Display the string search dialog."
  let window :: <basic-window> = frame-window(frame);
  let editor :: <basic-editor> = frame-editor(frame);
  // Display the modeless string search dialog, which updates the
  // frame state as the user messes around with it
  string-search-dialog(window,
		       string:   editor-search-string(editor),
		       reverse?: editor-reverse-search?(editor),
		       case-sensitive?: editor-case-sensitive-search?(editor),
		       whole-word?:     editor-whole-word-search?(editor))
end command find-string;

define command find-next-string (frame)
    "Find the next occurrence of the search string."
  let editor :: <basic-editor> = frame-editor(frame);
  if (editor-search-string(editor) & ~empty?(editor-search-string(editor)))
    find-next-or-previous-string(frame, reverse?: #f)
  else
    editor-reverse-search?(editor) := #f;
    find-string(frame)
  end
end command find-next-string;

define command find-previous-string (frame)
    "Find the previous occurrence of the search string."
  let editor :: <basic-editor> = frame-editor(frame);
  if (editor-search-string(editor) & ~empty?(editor-search-string(editor)))
    find-next-or-previous-string(frame, reverse?: #t)
  else
    editor-reverse-search?(editor) := #t;
    find-string(frame)
  end
end command find-previous-string;

define method find-next-or-previous-string
    (frame :: <editor-state-mixin>,
     #key reverse? = $unsupplied) => ()
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-buffer> = frame-buffer(frame);
  let editor :: <basic-editor> = frame-editor(frame);
  let string   = editor-search-string(editor);
  let reverse? = if (supplied?(reverse?)) reverse? else editor-reverse-search?(editor) end;
  let policy       = editor-policy(editor);
  let wrap-search? = wrap-searches?(policy);
  let whole-word?  = editor-whole-word-search?(editor);
  let syntax-table = whole-word? & list-syntax-table(buffer-major-mode(buffer));
  let case-sensitive? = editor-case-sensitive-search?(editor);
  when (string)
    clear-mark!(window: window, redisplay?: #t);
    let test = if (case-sensitive?) \= else char-equal? end;
    unless (editor-skip-table(editor) & editor-reoccurrence-table(editor))
      let (skip-table, reoccurrence-table)
	= compute-boyer-tables(string,
			       skip-table: editor-skip-table(editor),
			       reoccurrence-table: editor-reoccurrence-table(editor),
			       test: test);
      editor-skip-table(editor) := skip-table;
      editor-reoccurrence-table(editor) := reoccurrence-table
    end;
    let start-bp = point();
    let state :: <boolean> = #t;
    display-message(window, "Searching for: %s", string);
    while (state)
      let bp = search(point(), string,
		      test: test, reverse?: reverse?,
		      syntax-table: syntax-table,
		      skip-table:   editor-skip-table(editor),
		      reoccurrence-table: editor-reoccurrence-table(editor));
      if (bp)
	frame-search-string-found?(frame) := buffer;
	let length = size(string);
	let (pbp, mbp)
	  = if (reverse?) values(bp, move-over-characters(bp, length))
	    else values(move-over-characters(bp, length), bp) end;
	move-point!(pbp, window: window);
	when (mbp)
	  move-mark!(mbp, window: window)
	end;
	queue-redisplay(window, $display-point, centering: 0);
	state := #f		// force the search to end
      else
	frame-search-string-found?(frame) := #f;
	if (wrap-search?)
	  display-message(window, "Wrapping search for: %s", string);
	  if (reverse?)
	    move-point!(interval-end-bp(buffer), window: window);
	  else
	    move-point!(interval-start-bp(buffer), window: window);
	  end;
	  state := wrap-search?;
	  wrap-search? := #f	// give up next time around
	else
	  move-point!(start-bp, window: window);
	  command-error("Search failed for: %s", string)
	end
      end;
      frame-last-command-type(frame) := #"motion"
    end
  end
end method find-next-or-previous-string;


define command replace-string (frame)
    "Display the string replace dialog."
  let window :: <basic-window> = frame-window(frame);
  let editor :: <basic-editor> = frame-editor(frame);
  string-replace-dialog(window,
		        string:   editor-search-string(editor),
		        replace:  editor-replace-string(editor),
			reverse?: editor-reverse-search?(editor),
			case-sensitive?: editor-case-sensitive-search?(editor),
			whole-word?:     editor-whole-word-search?(editor))
end command replace-string;

define command query-replace-string (frame)
    "Display the string replace dialog."
  let window :: <basic-window> = frame-window(frame);
  let editor :: <basic-editor> = frame-editor(frame);
  string-replace-dialog(window,
		        string:   editor-search-string(editor),
		        replace:  editor-replace-string(editor),
			reverse?: editor-reverse-search?(editor),
			case-sensitive?: editor-case-sensitive-search?(editor),
			whole-word?:     editor-whole-word-search?(editor))
end command query-replace-string;

define method replace-next-or-previous-string
    (frame :: <editor-state-mixin>,
     #key reverse? = $unsupplied, replace-all? :: <boolean> = #f) => ()
  local method compare-strings
	    (char-test :: <function>, s1 :: <byte-string>, s2 :: <byte-string>)
	 => (equal? :: <boolean>)
	  // Automatically not equal if they're not the same size.
	  let equal? = (size(s1) = size(s2));
          for (c1 in s1, c2 in s2, until: ~equal?)
	    equal? := char-test(c1, c2)
	  end;
	  equal?
        end method;
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-buffer> = frame-buffer(frame);
  let editor :: <basic-editor> = frame-editor(frame);
  let string   = editor-search-string(editor);
  let replace  = editor-replace-string(editor);
  let reverse? = if (supplied?(reverse?)) reverse? else editor-reverse-search?(editor) end;
  let whole-word?  = editor-whole-word-search?(editor);
  let syntax-table = whole-word? & list-syntax-table(buffer-major-mode(buffer));
  let case-sensitive? = editor-case-sensitive-search?(editor);
  let n-replaced :: <integer> = 0;
  when (string)
    let test = if (case-sensitive?) \= else char-equal? end;
    unless (editor-skip-table(editor) & editor-reoccurrence-table(editor))
      let (skip-table, reoccurrence-table)
	= compute-boyer-tables(string,
			       skip-table: editor-skip-table(editor),
			       reoccurrence-table: editor-reoccurrence-table(editor),
			       test: test);
      editor-skip-table(editor) := skip-table;
      editor-reoccurrence-table(editor) := reoccurrence-table
    end;
    let bp = point();
    let found? = frame-search-string-found?(frame);
    frame-search-string-found?(frame) := #f;	// string is now unfound...
    while (bp)
      let ebp = window-mark(window);
      let interval = ebp & make-interval(bp, ebp);
      // If the search string has already been found and there's a selected
      // region matching it, use that; otherwise, go searching
      unless (found? == buffer & interval
	      & compare-strings(test, as(<string>, interval), string))
	// String was found but is not selected now, so we'll have to search
	bp := search(bp, string,
		     test: test, reverse?: reverse?, 
		     syntax-table: syntax-table,
		     skip-table:   editor-skip-table(editor),
		     reoccurrence-table: editor-reoccurrence-table(editor));
	if (bp)
	  ebp := move-over-characters(bp, size(string));
	  interval := make-interval(bp, ebp)
	else
	  interval := #f
	end
      end;
      if (interval)
	check-read-only(interval);
	queue-region-redisplay(window, bp, ebp, centering: #f);
	inc!(n-replaced);
	if (replace)
	  with-change-recording (buffer, <replace-change-record>,
				 interval: interval, moving?: #t)
	    let dbp = delete!(interval);
	    let nbp = insert!(interval-start-bp(interval), replace);
	    bp := if (reverse?) dbp else nbp end
	  end
        else
	  with-change-recording (buffer, <kill-change-record>, interval: interval)
	    let dbp = delete!(interval);
	    bp := dbp
	  end
	end;
        move-point!(bp)
      else
        if (replace-all?)
	  display-message(window, "Replaced %d items", n-replaced)
	else
	  //--- What about search wrapping?
	  command-error("Search failed for: %s", string)
	end
      end;
      // Stop after one replacement if we're not replacing everything
      unless (replace-all?)
	bp := #f
      end;
    end
  end
end method replace-next-or-previous-string;


/// Show matching/non-matching lines, sort lines, etc...

define command hack-matching-lines (frame)
    "Create a new buffer showing all the lines that match (or don't match) a given string."
  let window :: <basic-window> = frame-window(frame);
  let (string, operation) = hack-matching-lines-dialog(window);
  when (string)
    select (operation)
      #"show-matching" =>
        show-matching-lines(frame, string: string);
      #"show-non-matching" =>
        show-non-matching-lines(frame, string: string);
    end
  end
end command hack-matching-lines;

define function do-hack-matching-lines 
    (frame :: <editor-state-mixin>, title :: <byte-string>,
     string :: <byte-string>, filter :: <function>) => ()
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-buffer> = frame-buffer(frame);
  let editor = frame-editor(frame);
  let name   = format-to-string(title, string);
  let result-buffer
    = find-buffer(editor, name)
      | make-empty-buffer(<non-file-buffer>,
                          name: name,
                          major-mode: find-mode(<text-mode>),
                          editor: editor);
  let node    = buffer-start-node(result-buffer);
  let section = node-section(node);
  let first-line :: false-or(<basic-line>) = #f;
  let last-line  :: false-or(<basic-line>) = #f;
  let interval
    = if (mark()) make-interval(point(), mark())
      else make-interval(point(), interval-end-bp(buffer)) end;
  clear-mark!();
  //--- Provide some way to get back to the original line...
  local method hack-line (line :: <basic-line>, si, ei, last?)
          ignore(last?);
          when (filter(string, line, si, ei))
            let line = copy-line(line, start: si, end: ei);
            line-section(line) := section;
            unless (first-line)
              first-line := line
            end;
            line-previous(line) := last-line;
            when (last-line)
              line-next(last-line) := line
            end;
            last-line := line
          end
        end method;
  do-lines(hack-line, interval);
  section-start-line(section) := first-line;
  section-end-line(section)   := last-line;
  move-bp!(interval-start-bp(node), first-line, 0);
  move-bp!(interval-end-bp(node), last-line, line-length(last-line));
  select-buffer-in-appropriate-window(window, result-buffer)
end function do-hack-matching-lines;

define command show-matching-lines
    (frame, #key string :: false-or(<byte-string>) = #f)
    "Create a new buffer showing all the lines that match a given string."
  let window :: <basic-window> = frame-window(frame);
  let string = string | choose-string-dialog(window,
                                             title: "Show Matching Lines");
  when (string)
    local method filter-line
              (string :: <byte-string>,
               line :: <basic-line>, si :: <integer>, ei :: <integer>)
           => (keep? :: <boolean>)
            string-search(string, line-contents(line), start: si, end: ei) ~== #f
          end method;
    do-hack-matching-lines(frame, "Lines matching \"%s\"", string, filter-line)
  end;
  frame-last-command-type(frame) := #"display"
end command show-matching-lines;

define command show-non-matching-lines
    (frame, #key string :: false-or(<byte-string>) = #f)
    "Create a new buffer showing all the lines that don't match a given string."
  let window :: <basic-window> = frame-window(frame);
  let string = string | choose-string-dialog(window,
                                             title: "Show Non-matching Lines");
  when (string)
    local method filter-line
              (string :: <byte-string>,
               line :: <basic-line>, si :: <integer>, ei :: <integer>)
           => (keep? :: <boolean>)
            string-search(string, line-contents(line), start: si, end: ei) == #f
          end method;
    do-hack-matching-lines(frame, "Lines not matching \"%s\"", string, filter-line)
  end;
  frame-last-command-type(frame) := #"display"
end command show-non-matching-lines;


/// Point and mark

define command set-mark (frame)
    "Set mark at the current position, or jump to a saved mark.\n"
    "With no numeric argument, push the old mark onto the mark ring and set the new mark.\n"
    "With c-U argument, pop the saved mark and set the point to it.\n"
    "With c-U c-U argument, pop the saved mark and discard it.\n"
  let window :: <basic-window> = frame-window(frame);
  let n :: <integer> = frame-numeric-arg(frame);
  let state  = frame-numeric-arg-state(frame);
  let degree = $display-point;
  case
    state == #f =>			// c-Space
      push-point-pdl!(window, point());
      move-mark!(point());
    state == #"universal" & n = 4 =>	// c-U c-Space
      clear-mark!();
      let bp = pop-point-pdl!(window);
      if (bp)
	display-message(window, "Saved cursor position restored");
	move-point!(bp)
      else
	command-error("The point stack is empty")
      end;
    state == #"universal" & n = 16 =>	// c-U c-U c-Space
      clear-mark!();
      let bp = pop-point-pdl!(window);
      if (bp)
	display-message(window, "Saved cursor position discarded");
	degree := $display-region;
      else
	command-error("The point stack is empty")
      end;
    otherwise =>
      #f;
  end;
  queue-redisplay(window, degree);
  frame-last-command-type(frame) := #"motion"
end command set-mark;

define command exchange-point-pdl (frame)
    "Swap the point with the top of the point stack."
  let window :: <basic-window> = frame-window(frame);
  let bp = pop-point-pdl!(window);
  when (bp)
    push-point-pdl!(window, point(), display-message?: #f);
    move-point!(bp);
    queue-redisplay(window, $display-point)
  end;
  frame-last-command-type(frame) := #"motion"
end command exchange-point-pdl;

define command swap-point-and-mark (frame)
    "Swap the point and mark."
  let window :: <basic-window> = frame-window(frame);
  if (mark())
    swap-point-and-mark!(window);
    queue-redisplay(window, $display-point, centering: 0)
  else
    // If there's no current mark, but there is a "last mark",
    // swap the point and the "last mark"
    let bp1 = copy-bp(point());
    let bp2 = window-last-mark(window);
    when (bp2 & bp1 ~= bp2)
      move-point!(bp2);
      move-mark!(bp1);
      queue-redisplay(window, $display-point, centering: 0)
    end
  end;
  frame-last-command-type(frame) := #"motion"
end command swap-point-and-mark;


define command mark-buffer (frame)
    "Select the entire buffer."
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-buffer> = frame-buffer(frame);
  move-point!(interval-start-bp(buffer));
  move-mark!(interval-end-bp(buffer));
  queue-redisplay(window, $display-point, centering: -1);
  frame-last-command-type(frame) := #"mark"
end command mark-buffer;

define command mark-to-beginning (frame)
    "Select from the current position to the beginning of the buffer."
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-buffer> = frame-buffer(frame);
  move-mark!(interval-start-bp(buffer));
  queue-redisplay(window, $display-point);
  frame-last-command-type(frame) := #"mark"
end command mark-to-beginning;

define command mark-to-end (frame)
    "Select from the current position to the end of the buffer."
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-buffer> = frame-buffer(frame);
  move-mark!(interval-end-bp(buffer));
  queue-redisplay(window, $display-point);
  frame-last-command-type(frame) := #"mark"
end command mark-to-end;


define command mark-next-word (frame)
    "Select the next N words."
  let window :: <basic-window> = frame-window(frame);
  let n :: <integer> = frame-numeric-arg(frame);
  let sbp = point();
  let ebp = move-over-words(sbp, n);
  move-mark!(ebp);
  queue-redisplay(window, $display-point);
  frame-last-command-type(frame) := #"mark"
end command mark-next-word;

define command mark-word (frame)
    "Select the whole word around the point."
  let window :: <basic-window> = frame-window(frame);
  let bp   = point();
  let node = bp-node(bp) | bp-buffer(bp);
  let sbp = if (word-syntax(bp-character-before(bp)) == $word-delimiter)
              forward-over(bp, #[' ', '\t', '\f'], interval: node)
            else
	      move-over-words(bp, -1, interval: node)
            end;
  let ebp = move-over-words(sbp, 1, interval: node);
  move-mark!(sbp);
  move-point!(ebp);
  queue-redisplay(window, $display-point);
  frame-last-command-type(frame) := #"mark"
end command mark-word;


define command mark-next-atom (frame)
    "Select the next N atoms."
  let window :: <basic-window> = frame-window(frame);
  let n :: <integer> = frame-numeric-arg(frame);
  let sbp = point();
  let ebp = move-over-atoms(sbp, n);
  move-mark!(ebp);
  queue-redisplay(window, $display-point);
  frame-last-command-type(frame) := #"mark"
end command mark-next-atom;

define command mark-atom (frame)
    "Select the whole atom around the point."
  let window :: <basic-window> = frame-window(frame);
  select-atom-under-bp(window, point());
  frame-last-command-type(frame) := #"mark"
end command mark-atom;


define command mark-next-expression (frame)
    "Select the next N language expressions."
  let window :: <basic-window> = frame-window(frame);
  let n :: <integer> = frame-numeric-arg(frame);
  let sbp = point();
  let ebp = move-over-expressions(sbp, n);
  move-mark!(ebp);
  queue-redisplay(window, $display-point);
  frame-last-command-type(frame) := #"mark"
end command mark-next-expression;


/// Registers

define constant $register-point-table :: <object-table> = make(<table>);

define command save-point-in-register (frame)
    "Store the current point in the given register."
  let window :: <basic-window> = frame-window(frame); 
  let register = get-register-name(window, "Save point in register: ");
  let bp = point();
  let bp = make(<bp>,
		line: bp-line(bp), index: bp-index(bp),
		buffer: bp-buffer(bp),
		moving?: #t);
  gethash($register-point-table, register) := bp;
  frame-last-command-type(frame) := #"motion"
end command save-point-in-register;

define command restore-point-from-register (frame)
    "Restore the current point from the given register."
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-buffer> = frame-buffer(frame);
  let register = get-register-name(window, "Restore point from register: ");
  let bp = gethash($register-point-table, register);
  if (bp & member?(bp-buffer(bp), editor-buffers(frame-editor(frame))))
    unless (bp-buffer(bp) == buffer)
      select-buffer-in-appropriate-window(window, bp-buffer(bp),
					  line: bp-line(bp), index: bp-index(bp));
      queue-redisplay(window, $display-all)
    end;
    move-point!(bp);
    queue-redisplay(window, $display-point);
    frame-last-command-type(frame) := #"motion"
  else
    command-error("The register '%c' does not point to a known location", register)
  end
end command restore-point-from-register;


define constant $register-string-table :: <object-table> = make(<table>);

define command save-string-in-register (frame)
    "Copy the current region into the given register.\n"
    "With a numeric argument, appends to the register."
  let window :: <basic-window> = frame-window(frame);
  let register = get-register-name(window, "Save region in register: ");
  let bp1 = point();
  let bp2 = mark();
  let old-string = gethash($register-string-table, register);
  let new-string = if (bp2) as(<string>, make-interval(bp1, bp2)) else "" end;
  gethash($register-string-table, register)
    := if (frame-numeric-arg-state(frame) & old-string)
         concatenate-as(<string>, old-string, new-string)
       else
         new-string
       end;
  frame-last-command-type(frame) := #"kill"
end command save-string-in-register;

define command insert-string-from-register (frame)
    "Insert the contents of the given register at the current point."
  let window :: <basic-window> = frame-window(frame);
  let register = get-register-name(window, "Insert string from register: ");
  let string = gethash($register-string-table, register);
  if (string)
    insert-yanked-element(frame, string, replace?: #f);
    frame-last-command-type(frame) := #"insert"
  else
    command-error("The register '%c' does not contain anything", register)
  end
end command insert-string-from-register;

//---*** This won't interact properly with keyboard macro recording!
define method get-register-name
    (window :: <basic-window>, prompt :: <string>) => (register)
  display-message(window, prompt);
  let register = read-character(window);
  if (graphic-char?(register))
    display-message(window, concatenate(prompt, "%c"), register);
    register
  else
    command-error("The register must be named by a printable character")
  end
end method get-register-name;


/// Undo and redo

define command undo-command (frame)
    "Undo the previously executed command.\n"
    "With a numeric argument, undoes that many previous commands."
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-buffer> = window-buffer(window);
  let section = line-section(bp-line(window-point(window)));
  let (history, home-buffer)
    = buffer & buffer-undo-history(buffer, section: section);
  when (history)
    let (n-undo, n-redo) = undo-history-state(history);
    ignore(n-redo);
    if (n-undo > 0)
      let n :: <integer> = min(frame-numeric-arg(frame), n-undo);
      for (i :: <integer> from 0 below n)
	undo!(window, home-buffer, history)
      end
    else
      command-error("Nothing to undo")
    end
  end;
  frame-last-command-type(frame) := #"undo"
end command undo-command;

define command undo-all-commands (frame)
    "Undo all of the previously executed commands."
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-buffer> = window-buffer(window);
  let section = line-section(bp-line(window-point(window)));
  let (history, home-buffer)
    = buffer & buffer-undo-history(buffer, section: section);
  when (history)
    undo-all!(window, home-buffer, history)
  end;
  frame-last-command-type(frame) := #"undo"
end command undo-all-commands;

define command redo-command (frame)
    "Redo the previously undone command.\n"
    "With a numeric argument, redoes that many previously undone commands."
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-buffer> = window-buffer(window);
  let section = line-section(bp-line(window-point(window)));
  let (history, home-buffer)
    = buffer & buffer-undo-history(buffer, section: section);
  when (history)
    let (n-undo, n-redo) = undo-history-state(history);
    ignore(n-undo);
    if (n-redo > 0)
      let n :: <integer> = min(frame-numeric-arg(frame), n-redo);
      for (i :: <integer> from 0 below n)
	redo!(window, home-buffer, history)
      end
    else
      command-error("Nothing to redo")
    end
  end;
  frame-last-command-type(frame) := #"redo"
end command redo-command;

define command redo-all-commands (frame)
    "Redo all of the previously undone commands."
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-buffer> = window-buffer(window);
  let section = line-section(bp-line(window-point(window)));
  let (history, home-buffer)
    = buffer & buffer-undo-history(buffer, section: section);
  when (history)
    redo-all!(window, home-buffer, history)
  end;
  frame-last-command-type(frame) := #"redo"
end command redo-all-commands;


/// Indentation

define command indent-line (frame)
    "Indent the current line."
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-buffer> = frame-buffer(frame);
  let bp   = point();
  let line = bp-line(bp);
  check-read-only(bp);
  queue-redisplay(window, $display-line,
		  line: line, index: 0, centering: #f);
  let bp1 = line-start(line);
  let bp2 = forward-over(bp1, #[' ', '\t']);
  let at-start? = (forward-over(bp, #[' ', '\t']) = bp2);
  with-change-recording (buffer, <replace-change-record>,
			 start-bp: bp1, end-bp: bp2, moving?: #t)
    let (ebp, nchars)
      = do-indent-line(buffer-major-mode(buffer), line);
    case
      ~ebp      =>
	command-error("Can't indent this line");
      at-start? =>
	move-point!(ebp);	// don't move the point if it was not at the start
      otherwise =>
	move-point!(line, index: bp-index(bp) + nchars);
    end
  end;
  frame-last-command-type(frame) := #"insert"
end command indent-line;

define command indent-expression (frame)
    "Indent the entire next language expression."
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-buffer> = frame-buffer(frame);
  let bp1 = point();
  let bp2 = move-over-expressions(bp1, 1);
  let interval = make-interval(bp1, bp2);
  check-read-only(interval);
  clear-mark!();
  queue-region-redisplay(window, bp1, bp2, centering: #f);
  with-change-recording (buffer, <indentation-change-record>, interval: interval)
    do-indent-region(buffer-major-mode(buffer), interval)
  end;
  frame-last-command-type(frame) := #"insert"
end command indent-expression;

define command indent-region (frame)
    "Indent all the lines in the current selection."
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-buffer> = frame-buffer(frame);
  let bp1 = point();
  let bp2 = mark();
  if (bp2)
    let interval = make-interval(bp1, bp2);
    check-read-only(interval);
    clear-mark!();
    queue-region-redisplay(window, bp1, bp2, centering: #f);
    with-change-recording (buffer, <indentation-change-record>, interval: interval)
      do-indent-region(buffer-major-mode(buffer), interval)
    end
  else
    indent-line(frame)		// no region, just indent the line
  end;
  frame-last-command-type(frame) := #"insert"
end command indent-region;


define command indent-rigidly (frame)
    "Indent all the lines in the current selection by the given amount.\n"
    "With a negative argument, \"outdents\" the lines in the selection."
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-buffer> = frame-buffer(frame);
  let n :: <integer> = frame-numeric-arg(frame);
  unless (n = 0)
    let bp1 = point();
    let bp2 = mark();
    if (bp2)
      let interval = make-interval(bp1, bp2);
      check-read-only(interval);
      clear-mark!();
      queue-region-redisplay(window, bp1, bp2, centering: #f);
      if (n > 0)
	let spaces = make(<byte-string>, size: n, fill: ' ');
	local method indent (line :: <basic-line>, si, ei, last?)
		ignore(last?);
		// Indent the line only if the start index is zero and
		// the end index is not zero
		when (si = 0 & ei ~= 0)
		  insert-into-line(line, 0, spaces)
		end
	      end method indent;
        with-change-recording (buffer, <indentation-change-record>, interval: interval)
	  do-lines(indent, interval, skip-test: diagram-line?)
	end
      else
	let n :: <integer> = -n;
	let interval = make-interval(bp1, bp2);
 	local method unindent (line :: <basic-line>, si, ei, last?)
		ignore(last?);
		// Indent the line only if the start index is zero and
		// the end index is not zero
		when (si = 0 & ei ~= 0)
		ignore(si, ei);
		  let contents = line-contents(line);
		  for (i :: <integer> from 0 below min(line-length(line), n),
		       while: whitespace-char?(contents[i]))
		  finally
		    when (i > 0)
		      move-bp!(interval-start-bp(interval), line, 0);
		      move-bp!(interval-end-bp(interval),   line, i);
		      delete!(interval);
		    end
		  end
		end
	      end method unindent;
        with-change-recording (buffer, <indentation-change-record>, interval: interval)
	  do-lines(unindent, interval, skip-test: diagram-line?)
	end
      end
    else
      command-error("There is no selected region")
    end
  end;
  frame-last-command-type(frame) := #"insert"
end command indent-rigidly;

define command unindent-rigidly (frame)
    "Unindent all the lines in the current selection by the given amount."
  frame-numeric-arg(frame) := -frame-numeric-arg(frame);
  indent-rigidly(frame)
end command unindent-rigidly;


define variable *comment-start-column* :: <integer> = 40;

define command insert-comment (frame)
    "Insert a comment in the current line."
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-buffer> = frame-buffer(frame);
  let mode   = buffer-major-mode(buffer);
  let line   = bp-line(point());
  let column = if (line-empty?(line)) #f else *comment-start-column* end;
  let bp     = if (column) line-end(line) else line-start(line) end;
  with-change-recording (buffer, <paste-change-record>, start-bp: bp)
    clear-mark!();
    let bp = do-insert-comment(mode, line, column: column);
    when (bp)
      move-point!(bp);
      queue-redisplay(window, $display-line, line: line, index: 0)
    end
  end;
  frame-last-command-type(frame) := #"insert"
end command insert-comment;

define command comment-region (frame)
    "Comment out the current selection.\n"
    "With a numeric argument, \"uncomments\" the selection."
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-buffer> = frame-buffer(frame);
  let mode   = buffer-major-mode(buffer);
  let bp1 = point();
  let bp2 = mark();
  if (bp2)
    let interval = make-interval(bp1, bp2);
    check-read-only(interval);
    clear-mark!();
    queue-region-redisplay(window, bp1, bp2, centering: #f);
    with-change-recording (buffer, <replace-change-record>, interval: interval)
      do-comment-region(mode, interval, comment?: ~frame-numeric-arg-state(frame))
    end
  else
    command-error("There is no selected region")
  end;
  frame-last-command-type(frame) := #"insert"
end command comment-region;


define command delete-whitespace (frame)
    "Delete all whitespace around the point."
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-buffer> = frame-buffer(frame);
  let bp = point();
  let bp1 = backward-over(bp, #[' ', '\t']);
  let bp2 = forward-over(bp,  #[' ', '\t']);
  unless (bp1 = bp2)
    let interval = make-interval(bp1, bp2, in-order?: #t);
    check-read-only(interval);
    clear-mark!();
    queue-region-redisplay(window, bp1, bp2, centering: #f);
    with-change-recording (buffer, <kill-change-record>, interval: interval)
      move-point!(delete!(interval)
		  | command-error("Can't delete across hard sections"))
    end
  end;
  frame-last-command-type(frame) := #"delete"
end command delete-whitespace;


/// File and buffer commands

define command find-file (frame)
    "Create a buffer with this file in it.\n"
    "With a numeric argument, the buffer is put in \"fundamental\" mode."
  let window :: <basic-window> = frame-window(frame);
  let buffer = frame-buffer(frame);	// might get called before there's a buffer
  let home
    = if (composite-buffer?(buffer)) section-home-buffer(line-section(bp-line(point())))
      else buffer end;
  let default  = home & buffer-default-pathname(home);
  let type     = home & source-file-type(buffer-major-mode(home));
  let pathname = open-file-dialog(window, default: default, default-type: type);
  when (pathname)
    with-busy-cursor (window)
      let editor = frame-editor(frame);
      let buffer = find-buffer-from-pathname(editor, pathname);
      if (buffer)
	revert-buffer-if-necessary(buffer, window: window)
      else
	let mode = frame-numeric-arg-state(frame) & find-mode(<fundamental-mode>);
	buffer  := do-find-file(editor, pathname, direction: #"input", major-mode: mode)
      end;
      if (buffer)
	select-buffer-in-appropriate-window(window, buffer)
      else
	command-error("Couldn't find file %s", as(<string>, pathname))
      end
    end
  end;
  frame-last-command-type(frame) := #"file"
end command find-file;

define command new-file (frame)
    "Create an empty file buffer.\n"
    "With a numeric argument, the buffer is put in \"fundamental\" mode."
  let window :: <basic-window> = frame-window(frame);
  let buffer = frame-buffer(frame);	// might get called before there's a buffer
  let home
    = if (composite-buffer?(buffer)) section-home-buffer(line-section(bp-line(point())))
      else buffer end;
  let editor = frame-editor(frame);
  if (new-file-buffer?(editor-policy(editor)))
    let default  = home & buffer-default-pathname(home);
    let type     = home & source-file-type(buffer-major-mode(home));
    let pathname = new-file-dialog(window, default: default, default-type: type);
    when (pathname)
      with-busy-cursor (window)
	let mode   = frame-numeric-arg-state(frame) & find-mode(<fundamental-mode>);
	let buffer = find-buffer-from-pathname(editor, pathname)
		     | do-find-file(editor, pathname, direction: #"output", major-mode: mode);
	if (buffer)
	  when (~mode & interval-start-bp(buffer) = interval-end-bp(buffer))
	    // If the buffer is empty, at least enter the proper major mode
	    let mode = find-mode-from-pathname(pathname);
	    enter-mode(buffer, mode)
	  end;
	  select-buffer-in-appropriate-window(window, buffer)
	else
	  command-error("Couldn't open file %s", as(<string>, pathname))
	end
      end
    end
  else
    let buffer = make-empty-buffer(<non-file-buffer>, editor: editor);
    select-buffer-in-appropriate-window(window, buffer)
  end;
  frame-last-command-type(frame) := #"file"
end command new-file;

define method do-find-file
    (editor :: <basic-editor>, pathname :: <pathname>,
     #key direction, major-mode)
 => (buffer :: false-or(<basic-buffer>))
  let container :: <source-container>
    = find-source-container(editor, source-container-class(pathname), pathname);
  let buffer :: false-or(<file-buffer>)		// so we can set it to #f below...
    = make-empty-buffer(<file-buffer>,
			name:       pathname->buffer-name(pathname),
			major-mode: major-mode | find-mode(<text-mode>),
			container:  container,
			editor:     editor);
  block ()
    // Always try to revert the buffer, even for output files
    revert-buffer(buffer, major-mode: major-mode)
  exception (<file-does-not-exist-error>)
    when (direction == #"input")
      remove!(editor-buffers(editor), buffer);
      buffer := #f
    end
  end;
  buffer
end method do-find-file;


define command insert-file (frame)
    "Insert the contents of a file into the current buffer."
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-buffer> = frame-buffer(frame);
  let home
    = if (composite-buffer?(buffer)) section-home-buffer(line-section(bp-line(point())))
      else buffer end;
  let default  = home & buffer-default-pathname(home);
  let type     = home & source-file-type(buffer-major-mode(home));
  let pathname = open-file-dialog(window, default: default, default-type: type);
  when (pathname)
    with-busy-cursor (window)
      // Read the contents of the file into a section
      let section = make(<section>,
			 container: #f,
			 start-line: #f, end-line: #f);
      let stream  = make(<file-stream>,
			 locator: pathname, direction: #"input");
      block ()
	read-section-contents-from-stream(section, stream)
      cleanup
	when (stream & stream-open?(stream))
	  close(stream)
	end
      end;
      // Splice all the lines into the current buffer
      let bp = point();
      let interval = make-interval(line-start(section-start-line(section)),
				   line-end(section-end-line(section)),
				   in-order?: #t);
      with-change-recording (buffer, <paste-change-record>, start-bp: bp)
	clear-mark!();
	insert!(bp, interval)
      end;
      when (sectionize-buffer(buffer))
	queue-redisplay(window, $display-all)
      end
    end
  end;
  frame-last-command-type(frame) := #"file"
end command insert-file;


define command save-file (frame)
    "Save this buffer into the file it came from."
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-buffer> = frame-buffer(frame);
  if (saves-like-file-buffer?(buffer))
    if (buffer-modified?(buffer))
      with-busy-cursor (window)
	let (pathname, condition) = save-buffer(buffer, frame: frame);
	ignore(condition);
	if (pathname)
	  display-message(window, "Saved to %s", as(<string>, pathname));
	  note-buffer-changed-everywhere(buffer, #f);
	  frame-last-command-type(frame) := #"file";
	  when (resectionize-changed-sections(buffer))
	    queue-redisplay(window, $display-all)	// might want to redraw sections...
	  end
	else
	  command-error("Couldn't save file %s",
			as(<string>, buffer-default-pathname(buffer)));
	end
      end
    else
      display-message(window, "No changes need to be saved for %s",
		      as(<string>, buffer-default-pathname(buffer)))
    end
  else
    save-file-as(frame)
  end
end command save-file;

define command save-file-as (frame)
    "Save this buffer into a new file."
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-buffer> = frame-buffer(frame);
  do-save-file-as(frame, buffer);
  when (resectionize-changed-sections(buffer))
    queue-redisplay(window, $display-all)	// might want to redraw sections...
  end
end command save-file-as;

// This is split out so that frames can do "Save As" on buffers other
// than their own frame's buffer, for use in 'save-all-files'
define method do-save-file-as
    (frame :: <editor-state-mixin>, buffer :: <basic-buffer>)
 => (pathname :: false-or(<pathname>))
  let window :: <basic-window> = frame-window(frame);
  let home
    = if (composite-buffer?(buffer)) section-home-buffer(line-section(bp-line(point())))
      else buffer end;
  let default  = buffer-default-pathname(home);
  let type     = source-file-type(buffer-major-mode(home));
  let pathname = new-file-dialog(window, default: default, default-type: type);
  let result   = #f;
  when (pathname)
    with-busy-cursor (window)
      result := save-buffer-as(source-container-class(pathname), buffer, pathname,
			       frame: frame)
    end;
    if (result)
      when (~file-buffer?(buffer) & ~special-purpose-buffer?(buffer))
	// If we saved out a simple non-file buffer, "upgrade" it
	// to be a file buffer by creating a new (saved) file buffer
	// and killing the old buffer
	let new-buffer
	  = as-file-buffer(<file-buffer>, buffer, result, frame-editor(frame));
	// Kill the old buffer gently and select the new one
	let editor = frame-editor(frame);
	remove!(editor-buffers(editor), buffer);
	for (window :: <basic-window> in editor-windows(editor))
	  let buffers = window-selected-buffers(window);
	  let entry
	    = find-value(buffers, method (s) selection-buffer(s) == buffer end);
	  when (entry)
	    remove!(buffers, entry)
	  end;
	  when (window-buffer(window) == buffer)
	    select-buffer(window, new-buffer)
	  end
	end;
        queue-redisplay(window, $display-all);
	buffer := new-buffer
      end;
      note-buffer-changed-everywhere(buffer, #f);
      display-buffer-name-everywhere(buffer);
      display-message(window, "Saved to %s", as(<string>, result))
    else
      command-error("Couldn't save file %s", as(<string>, pathname))
    end
  end;
  frame-last-command-type(frame) := #"file";
  result
end method do-save-file-as;

define command save-all-files
  (frame, #key reason :: false-or(<string>), buffers :: false-or(<sequence>))
    "Save all buffers, even if they're not modified or not file-buffers."
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-buffer> = frame-buffer(frame);
  //--- It's not standard on Windows to prompt during Save All, but
  //--- until we are smarter about only saving new or modified buffers,
  //--- that's what we do to avoid saving unmodified buffers
  let (buffers, no-buffers?)
    = save-buffers-dialog(window, reason: reason, buffers: buffers);
  if (no-buffers?)
    display-message(window, "No documents need to be saved")
  else
    select (buffers)
      #f        => #f;
      #"cancel" => #f;
      otherwise =>
	do-save-all-files(frame, buffers, curry(display-message, window));
    end
  end;
  //--- This should 'display-buffer-name' for all for all editor windows
  //--- Fix D-environment-editor-backends-deuce!deuce-backend.dylan, too
  display-buffer-name(window, buffer);
  frame-last-command-type(frame) := #"file"
end command save-all-files;

define method do-save-all-files
    (frame :: <editor-state-mixin>, buffers :: <sequence>,
     display-message-function :: <function>) => ()
  // We dynamic-bind *editor-frame* for 'save-buffer', in case we're being
  // called from a thread where it's not bound (e.g., when DW is saving all
  // files as the last project window is closing).  We don't need to bind
  // *buffer*, since 'save-buffer' does that itself.
  dynamic-bind (*editor-frame* = frame)
    for (buffer :: <basic-buffer> in buffers)
      try-to-save-buffer(frame, buffer, display-message-function)
    end
  end
end method do-save-all-files;

define method try-to-save-buffer
    (frame :: <editor-state-mixin>, buffer :: <basic-buffer>,
     display-message-function :: <function>)
 => (pathname :: false-or(<pathname>))
  let window :: <basic-window> = frame-window(frame);
  let name = as(<string>, buffer-pathname(buffer) | buffer-name(buffer));
  let pathname
    = block ()
	if (file-buffer?(buffer))
	  with-busy-cursor (window)
	    let pathname = save-buffer(buffer, frame: frame);
	    when (pathname)
	      note-buffer-changed-everywhere(buffer, #f)
	    end;
	    pathname
	  end
	else
	  do-save-file-as(frame, buffer)
	end;
      exception(<command-error>)
	command-error("Couldn't save file %s", as(<string>, name));
	#f
      end;
  when (pathname)
    display-message-function("Saved %s", as(<string>, name))
  end;
  pathname
end method try-to-save-buffer;


// A bit of a misnomer -- it actually reverts the contents of the _buffer_
define command revert-file (frame)
    "Revert the contents of this buffer to its initial state."
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-buffer> = frame-buffer(frame);
  let container = buffer-source-container(buffer);
  let pathname  = container & container-pathname(container);
  if (file-buffer?(buffer) & pathname & ~file-exists?(pathname))
    // If the file is gone, don't revert
    warning-dialog(window, "%s no longer exists on disk; revert cancelled.",
		   as(<string>, pathname))
  else
    when (~buffer-modified?(buffer)
	  | yes-or-no-dialog(window, "The document is modified; really revert it?"))
      let line = bp->line-index(point());
      with-busy-cursor (window)
	when (revert-buffer(buffer))
	  restore-previous-position(buffer, window, line);
	  queue-redisplay(window, $display-all)
	end
      end
    end
  end;
  frame-last-command-type(frame) := #"file"
end command revert-file;

// Try to move to where we were before the buffer was reverted
define method restore-previous-position
    (buffer :: <basic-buffer>, window :: <basic-window>, line :: false-or(<integer>)) => ()
  let (tbp :: <basic-bp>, centering)
    = if (line)
	let bp = line-index->bp(buffer, line);
	if (bp) values(bp, #"center")
	else values(interval-start-bp(buffer), #"top") end
      else
	values(interval-start-bp(buffer), #"top")
      end;
  let bp = make(<bp>, line: bp-line(tbp), index: bp-index(tbp), buffer: buffer);
  window-point(window) := as(<permanent-bp>, bp);
  window-mark(window)  := #f;
  recenter-window(window, bp-line(bp), centering);
  window-goal-x-position(window) := #f;
  window-total-lines(window) := #f;
  queue-redisplay(window, $display-all);
  note-buffer-changed-everywhere(buffer, #f)
end method restore-previous-position;

// Another misnomer -- it's really just the interface to 'kill-buffer'
define command close-file (frame)
    "Close the buffer, offering to save its underlying file if it is modified."
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-buffer> = frame-buffer(frame);
  // Only confirm when the command came from the keyboard and the
  // user has got the confirmation policy turned on
  let buffers
    = if (confirm-kill-buffer?(editor-policy(frame-editor(frame)))
	  & frame-command-character(frame))	// kludge-o-rama
	choose-buffers-dialog(window,
			      title: "Documents to Close",
			      ok-label: "&Close", all-label: "Close &All",
			      buffer: buffer)
      else
	vector(buffer)
      end;
  when (buffers)
    let killed? = #f;
    block (break)
      for (b in buffers)
	unless (b == buffer)		// kill the current buffer last
	  let result = maybe-save-buffer(window, b);
	  select (result)
	    #f => 
	      // Note that this might exit in the one-frame-per-buffer policy...
	      kill-buffer(b, frame: frame);
	      killed? := #t;
	    #t        => #f;
	    #"cancel" => break();
	  end
	end
      end;
      when (member?(buffer, buffers))
	let result = maybe-save-buffer(window, buffer);
	select (result)
	  #f => 
	    kill-buffer(buffer, frame: frame);
	    killed? := #t;
	  #t        => #f;
	  #"cancel" => break();
	end
      end
    end block;
    when (killed?)
      queue-redisplay(window, $display-all);
      display-buffer-name(window, window-buffer(window))
    end
  end;
  frame-last-command-type(frame) := #"file"
end command close-file;

define command print-file (frame)
    "Print the contents of the buffer."
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-buffer> = frame-buffer(frame);
  unless (maybe-save-buffer(window, buffer))
    //---*** Print the file
    frame-last-command-type(frame) := #"file"
  end;
end command print-file;

// Returns #f if the "Save file" succeeded, #t if the save operation failed,
// or #"cancel" if the "Save file" query was cancelled by the user
define method maybe-save-buffer
    (window :: <basic-window>, buffer :: <basic-buffer>)
 => (failed? :: type-union(<boolean>, singleton(#"cancel")))
  // Obviously if the buffer's not modified, we don't need save it.
  // Less obviously, if it's a special-purpose buffer, the modification
  // state is irrelevant -- we never offer to save it
  if (~buffer-modified?(buffer)
      | special-purpose-buffer?(buffer))
    #f				// succeeded, after a fashion
  else
    let result = yes-no-or-cancel-dialog(window, "Do you want to save changes to %s?",
					 buffer-name(buffer));
    select (result)
      #t        =>		// "Yes"
	let pathname
	  = try-to-save-buffer(window-frame(window), buffer, curry(display-message, window));
	~pathname;		// pathname => succeeded, no pathname => failed
      #f        => #f;		// "No"     => succeeded, after a fashion
      otherwise => #"cancel";	// "Cancel" => failed
    end
  end
end method maybe-save-buffer;


// One more misnomer -- it sectionizes the contents of the _buffer_
define command sectionize-file (frame)
    "Sectionize the contents of this buffer."
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-buffer> = frame-buffer(frame);
  when (sectionize-buffer(buffer))
    queue-redisplay(window, $display-all)
  end;
  frame-last-command-type(frame) := #"file"
end command sectionize-file;


define command mark-unmodified (frame)
    "Mark the current buffer as being unmodified."
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-buffer> = frame-buffer(frame);
  buffer-modified?(buffer) := #f;
  display-message(window, "Buffer marked unmodified");
  frame-last-command-type(frame) := #"file"
end command mark-unmodified;

define command toggle-read-only (frame)
    "Toggle the read-only status of a region or the entire buffer."
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-buffer> = frame-buffer(frame);
  if (mark())
    let interval = make-interval(point(), mark());
    let read-only? = interval-read-only?(interval);
    interval-read-only?(interval) := ~read-only?;
    display-message(window,
		    if (read-only?) "Region now writable" else "Region now read-only" end)
  else
    let read-only? = buffer-read-only?(buffer);
    buffer-read-only?(buffer) := ~read-only?;
    display-message(window,
		    if (read-only?) "Buffer now writable" else "Buffer now read-only" end)
  end;
  frame-last-command-type(frame) := #"file"
end command toggle-read-only;


define command choose-buffer (frame)
    "Select a buffer to be shown in the current window.\n"
    "With a numeric argument, creates a new non-file buffer."
  if (frame-numeric-arg-state(frame))
    new-buffer(frame)
  else
    let window :: <basic-window> = frame-window(frame);
    let buffer :: false-or(<basic-buffer>)
      = choose-buffer-dialog(window,
			     title: "Select Document",
			     buffer: previously-selected-buffer(window, 1));
    when (buffer)
      select-buffer-in-appropriate-window(window, buffer)
    end;
    frame-last-command-type(frame) := #"motion";
    // Do the redisplay right here so that we can tell it not to move
    // the point.  Otherwise, Deuce will always recenter around the new
    // point because we haven't yet computed the dlines for the newly
    // selected buffer.  And that would break "poor man's source compare",
    // which I would find very depressing...
    redisplay-window(window, move-point?: #f, move-viewport?: #t)
  end
end command choose-buffer;

define command switch-buffers (frame)
    "Switch to the N'th buffer in the set of recently selected buffers."
  let window :: <basic-window> = frame-window(frame);
  // N gives the number of buffers through which to rotate
  let n :: <integer> = if (frame-numeric-arg-state(frame)) frame-numeric-arg(frame) else 2 end;
  case
    n = 0 =>
      let buffers = map(selection-buffer, window-selected-buffers(window));
      let buffer :: false-or(<basic-buffer>)
	= choose-buffer-dialog(window,
			       title: "Select Document",
			       buffers: buffers,
			       buffer:  previously-selected-buffer(window, 1));
      when (buffer)
	select-buffer-in-appropriate-window(window, buffer)
      end;
    n > 1 =>
      let buffer = previously-selected-buffer(window, n - 1);
      if (buffer)
	when (buffer ~= window-buffer(window))
	  select-buffer-in-appropriate-window(window, buffer)
	end
      else
	command-error("This window hasn't shown that many documents")
      end;
    otherwise =>
      #f;
  end;
  frame-last-command-type(frame) := #"motion";
  // See comments in 'choose-buffer' above...
  redisplay-window(window, move-point?: #f, move-viewport?: #t)
end command switch-buffers;

define method previously-selected-buffer
    (window :: <basic-window>, n :: <integer>) => (buffer :: false-or(<basic-buffer>))
  let buffers = window-selected-buffers(window);
  n < size(buffers)
  & selection-buffer(buffers[n])
end method previously-selected-buffer;

define command new-buffer (frame)
    "Create a new non-file buffer to be shown in the current window."
  let window :: <basic-window> = frame-window(frame);
  let name = new-buffer-dialog(window);
  when (name)
    let editor = frame-editor(frame);
    let buffer = make-empty-buffer(<non-file-buffer>,
				   name: ~empty?(name) & name,
				   major-mode: find-mode(<text-mode>),
				   editor: editor);
    select-buffer-in-appropriate-window(window, buffer)
  end;
  frame-last-command-type(frame) := #"motion"
end command new-buffer;


/// Configuration

define command choose-configuration (frame)
    "Set up various configuration parameters"
  let window :: <basic-window> = frame-window(frame);
  let editor = frame-editor(frame);
  let old-policy = editor-policy(editor);
  let new-policy = configuration-dialog(window);
  when (new-policy)
    editor-policy(editor) := new-policy;
    when (command-set-policy(new-policy) ~== command-set-policy(old-policy))
      install-command-set(editor, command-set-policy(new-policy))
    end;
    for (window :: <basic-window> in editor-windows(editor))
      window-note-policy-changed(window, new-policy, old-policy);
      redisplay-window(window);
    end
  end
end command choose-configuration;


/// Language (major mode) based commands

define command start-of-definition (frame)
    "Move to the beginning of the current language definition."
  let window :: <basic-window> = frame-window(frame);
  let bp = point();
  push-point-pdl!(window, bp);
  let line = section-defining-line(line-section(bp-line(bp)));
  when (bp-line(bp) == line)
    // Already at the beginning of this definition, move to the previous one
    let node = line-node(line);
    let prev = node & node-previous(node);
    when (prev)
      line := section-defining-line(node-section(prev))
     end
  end;
  move-point!(line, index: 0);
  queue-redisplay(window, $display-point, centering: -1);
  frame-last-command-type(frame) := #"motion"
end command start-of-definition;

define command end-of-definition (frame)
    "Move to the end of the current language definition."
  let window :: <basic-window> = frame-window(frame);
  let bp = point();
  push-point-pdl!(window, bp);
  let line = section-end-line(line-section(bp-line(bp)));
  when (bp-line(bp) == line)
    // Already at the end of this definition, move to the next one
    let node = line-node(line);
    let next = node & node-next(node);
    when (next)
      line := section-end-line(node-section(next))
     end    
  end;
  move-point!(line, index: 0);
  queue-redisplay(window, $display-point, centering: 1);
  frame-last-command-type(frame) := #"motion"
end command end-of-definition;


define command edit-definition (frame)
    "Edit the named definition."
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-buffer> = frame-buffer(frame);
  let interval = selected-language-object();
  let name
    = unless (mark())
	let name = edit-definition-dialog(window, as(<string>, interval));
	name | error(make(<command-error>, window: window))
      end;
  unless (do-edit-definition(buffer-major-mode(buffer), interval, window, name: name))
    command-error("Couldn't find the definition for %s", as(<string>, interval))
  end;
  frame-last-command-type(frame) := #"motion"
end command edit-definition;


define command complete-name (frame)
    "Perform completion of the name at the current point."
  do-completion(frame, do-complete-name);
  frame-last-command-type(frame) := #"complete"
end command complete-name;

define command dynamic-complete-name (frame)
    "Expand the word at the current point \"dynamically\"."
  let state = reset-dynamic-completion-state(frame);
  do-completion(frame, do-complete-dynamically, completion-state: state);
  frame-last-command-type(frame) := #"dynamic-complete"
end command dynamic-complete-name;

define method do-completion
    (frame :: <editor-state-mixin>,
     completer :: <function>, #rest completer-args) => ()
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-buffer> = frame-buffer(frame);
  let (sbp, ebp) = atom-under-bp(point());
  let interval = make-interval(sbp, ebp, in-order?: #t);
  check-read-only(interval);
  let (completion, ambiguous?)
    = apply(completer, buffer-major-mode(buffer), interval, window,
	    menu?: frame-numeric-arg-state(frame) ~== #f, completer-args);
  case
    ~completion =>
      command-error("No completion for \"%s\"", as(<string>, interval));
    completion == #t =>
      #f;
    string-equal?(completion, as(<string>, interval)) =>
      move-point!(ebp);
    otherwise =>
      queue-redisplay(window, $display-line,
		      line: bp-line(sbp), index: bp-index(sbp), centering: #f);
      with-change-recording (buffer, <replace-change-record>,
			     interval: interval, moving?: #t)
	delete!(interval);
	let ebp = insert-moving!(sbp, completion);
	move-point!(ebp)
      end;
  end;
  when (completion & ambiguous?)
    display-message(window, "The completion for \"%s\" was ambiguous",
		    as(<string>, interval))
  end
end method do-completion;


define command show-value (frame)
    "Show the value of the selected language object."
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-buffer> = frame-buffer(frame);
  let interval = selected-language-object();
  //---*** Do this via major mode
  frame-last-command-type(frame) := #"browse"
end command show-value;

define command describe-object (frame) 
    "Describe the value of the selected language object."
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-buffer> = frame-buffer(frame);
  let interval = selected-language-object();
  let name
    = unless (mark())
	let name = edit-definition-dialog(window, as(<string>, interval),
					  title: "Describe Object");
	name | error(make(<command-error>, window: window))
      end;
  unless (do-describe-object(buffer-major-mode(buffer), interval, window, name: name))
    command-error("Couldn't describe %s", as(<string>, interval))
  end;
  frame-last-command-type(frame) := #"browse"
end command describe-object;

define command browse-object (frame) 
    "Browse the value of the selected language object."
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-buffer> = frame-buffer(frame);
  let interval = selected-language-object();
  let name
    = unless (mark())
	let name = edit-definition-dialog(window, as(<string>, interval),
					  title: "Browse Object");
	name | error(make(<command-error>, window: window))
      end;
  unless (do-browse-object(buffer-major-mode(buffer), interval, window, name: name))
    command-error("Couldn't browse %s", as(<string>, interval))
  end;
  frame-last-command-type(frame) := #"browse"
end command browse-object;

define command browse-class (frame)
    "Browse the class of the selected language object."
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-buffer> = frame-buffer(frame);
  let interval = selected-language-object();
  let name
    = unless (mark())
	let name = edit-definition-dialog(window, as(<string>, interval),
					  title: "Browse Class");
	name | error(make(<command-error>, window: window))
      end;
  unless (do-browse-class(buffer-major-mode(buffer), interval, window, name: name))
    command-error("Couldn't browse the class of %s", as(<string>, interval))
  end;
  frame-last-command-type(frame) := #"browse"
end command browse-class;

define command browse-function (frame)
    "Browse the generic function of the selected language object."
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-buffer> = frame-buffer(frame);
  let interval = selected-language-object();
  let name
    = unless (mark())
	let name = edit-definition-dialog(window, as(<string>, interval),
					  title: "Browse Function");
	name | error(make(<command-error>, window: window))
      end;
  unless (do-browse-function(buffer-major-mode(buffer), interval, window, name: name))
    command-error("Couldn't browse the function %s", as(<string>, interval))
  end;
  frame-last-command-type(frame) := #"browse"
end command browse-function;

define function selected-language-object
    () => (interval :: <basic-interval>)
  let (sbp, ebp)
    = if (mark()) order-bps(point(), mark()) else atom-under-bp(point()) end;
  make-interval(sbp, ebp, in-order?: #t)
end function selected-language-object;


define command show-arglist (frame)
    "Show the parameter list of the selected language object."
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-buffer> = frame-buffer(frame);
  let interval
    = if (mark()) make-interval(point(), mark())
      else relevant-function-interval(point()) end;
  let name
    = unless (mark())
	let name = edit-definition-dialog(window, as(<string>, interval),
					  title: "Show Parameters");
	name | error(make(<command-error>, window: window))
      end;
  unless (do-show-arglist(buffer-major-mode(buffer), interval, window, name: name))
    command-error("Couldn't show the parameters for %s", as(<string>, interval))
  end;
  frame-last-command-type(frame) := #"browse"
end command show-arglist;

define command show-documentation (frame)
    "Show the on-line documentation for the selected language object."
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-buffer> = frame-buffer(frame);
  let interval = selected-language-object();
  let name
    = unless (mark())
	let name = edit-definition-dialog(window, as(<string>, interval),
					  title: "Show Documentation");
	name | error(make(<command-error>, window: window))
      end;
  unless (do-show-documentation(buffer-major-mode(buffer), interval, window, name: name))
    command-error("Couldn't show the documentation for %s", as(<string>, interval))
  end;
  frame-last-command-type(frame) := #"browse"
end command show-documentation;


define command evaluate-definition (frame)
    "Interactively compile the current definition."
  let interval = definition-interval(point());
  compile-to-core(frame, interval, "definition");
end command evaluate-definition;

define command evaluate-region (frame)
    "Interactively compile the selection, or the current definition if there's no selection.\n"
    "With a numeric argument, compiles the entire buffer."
  if (frame-numeric-arg-state(frame))
    evaluate-buffer(frame)
  else
    let (interval, what)
      = if (mark())
	  values(make-interval(point(), mark()), "region")
	else
	  values(definition-interval(point()), "definition")
	end;
    compile-to-core(frame, interval, what)
  end
end command evaluate-region;

define command evaluate-buffer (frame)
    "Interactively compile the entire buffer." 
  let buffer = frame-buffer(frame);
  let interval = make-interval(interval-start-bp(buffer), interval-end-bp(buffer));
  compile-to-core(frame, interval, "buffer");
end command evaluate-buffer;

define method compile-to-core
    (frame :: <editor-state-mixin>, interval, what) => ()
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-buffer> = frame-buffer(frame);
  display-message(window, "Compiling %s... ", what);
  do-compile-to-core(buffer-major-mode(buffer), interval);
  display-message(window, "Compiling %s... compiled", what);
  frame-last-command-type(frame) := #"compile"
end method compile-to-core;


define command macroexpand-region (frame)
    "Macroexpand the selection, or the current definition if there's no selection.\n"
    "With a numeric argument, inserts the expansion into a \"results\" buffer."
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-buffer> = frame-buffer(frame);
  let interval = if (mark()) make-interval(point(), mark())
		 else definition-interval(point()) end;
  let stream   = make(<string-stream>, direction: #"output");
  do-macroexpand(buffer-major-mode(buffer), interval, stream);
  let expansion = stream-contents(stream);
  if (frame-numeric-arg-state(frame))
    //---*** Display the expansion somewhere
    #f
  else
    with-change-recording (buffer, <paste-change-record>, start-bp: interval-end-bp(interval))
      let bp1 = interval-end-bp(interval);
      let bp2 = insert!(bp1, expansion);
      move-point!(bp1);
      move-mark!(bp2)
    end;
    queue-redisplay(window, $display-all)
  end;
  frame-last-command-type(frame) := #"compile"
end command macroexpand-region;


define command compile-file (frame)
    "Compile the indicated file."
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-buffer> = frame-buffer(frame);
  let home
    = if (composite-buffer?(buffer)) section-home-buffer(line-section(bp-line(point())))
      else buffer end;
  let default  = home & buffer-default-pathname(home);
  let type     = home & source-file-type(buffer-major-mode(home));
  let pathname = open-file-dialog(window, default: default, default-type: type);
  when (pathname)
    let mode = find-mode-from-pathname(pathname);
    when (compilation-supported?(mode))
      display-message(window, "Compiling file... ");
      do-compile-file(mode, pathname);
      display-message(window, "Compiling file... compiled")
    end
  end;
  frame-last-command-type(frame) := #"compile"
end command compile-file;

define command load-file (frame)
    "Load the indicated file."
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-buffer> = frame-buffer(frame);
  let home
    = if (composite-buffer?(buffer)) section-home-buffer(line-section(bp-line(point())))
      else buffer end;
  let default  = home & buffer-default-pathname(home);
  let type     = home & binary-file-type(buffer-major-mode(home));
  let pathname = open-file-dialog(window, default: default, default-type: type);
  when (pathname)
    let mode = find-mode-from-pathname(pathname);
    when (compilation-supported?(mode))
      display-message(window, "Loading file... ");
      do-load-file(mode, pathname);
      display-message(window, "Loading file... loaded")
    end
  end;
  frame-last-command-type(frame) := #"compile"
end command load-file;


define command parse-project (frame)
    "Partially compile the project associated with this buffer."
  build-buffer-project(frame, #"project-parse", "Partially compiling", "compiled")
end command parse-project;

define command compile-project (frame)
    "Compile the project associated with this buffer."
  build-buffer-project(frame, #"project-compile", "Compiling", "compiled")
end command compile-project;

define command clean-compile-project (frame)
    "Cleanly recompile the project associated with this buffer."
  build-buffer-project(frame, #"project-clean-compile", "Cleanly compiling", "compiled")
end command clean-compile-project;

define command link-project (frame)
    "Link the project associated with this buffer."
  build-buffer-project(frame, #"project-link", "Linking", "linked")
end command link-project;

define command build-project (frame)
    "Build (compile then link) the project associated with this buffer."
  build-buffer-project(frame, #"project-build", "Building", "built")
end command build-project;

define command clean-build-project (frame)
    "Cleanly build (clean compile then link) the project associated with this buffer."
  build-buffer-project(frame, #"project-clean-build", "Cleanly building", "built")
end command clean-build-project;

define method build-buffer-project
    (frame :: <editor-state-mixin>, scope :: <symbol>, what, what-done) => ()
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-buffer> = frame-buffer(frame);
  let mode = buffer-major-mode(buffer);
  when (compilation-supported?(mode))
    // display-message(window, "%s project... ", what);
    do-build-project(buffer-major-mode(buffer), buffer, scope);
    // display-message(window, "%s project... %s", what, what-done)
  end;
  frame-last-command-type(frame) := #"compile"
end method build-buffer-project;


/// Browsing commands

define command edit-class-subclasses (frame)
    "Create a browsing buffer containing the subclasses of a class."
  make-definition-browser(frame, #"class-subclasses",
			  buffer-class: <subclasses-browsing-buffer>)
end command edit-class-subclasses;

define command edit-class-superclasses (frame)
    "Create a browsing buffer containing the superclasses of a class."
  make-definition-browser(frame, #"class-superclasses",
			  buffer-class: <superclasses-browsing-buffer>)
end command edit-class-superclasses;

define command edit-class-methods (frame)
    "Create a browsing buffer containing the methods of a class."
  make-definition-browser(frame, #"class-methods",
			  buffer-class: <class-methods-browsing-buffer>)
end command edit-class-methods;

define command edit-generic-function-methods (frame)
    "Create a browsing buffer containing the methods of a generic function."
  make-definition-browser(frame, #"generic-function-methods",
			  buffer-class: <generic-function-methods-browsing-buffer>)
end command edit-generic-function-methods;

define command edit-callers (frame)
    "Create a browsing buffer containing the callers of a function."
  make-definition-browser(frame, #"function-callers",
			  buffer-class: <callers-browsing-buffer>)
end command edit-callers;

define command edit-callees (frame)
    "Create a browsing buffer containing the callees of a function."
  make-definition-browser(frame, #"function-callees",
			  buffer-class: <callees-browsing-buffer>)
end command edit-callees;

define method make-definition-browser
    (frame :: <editor-state-mixin>, what :: <symbol>, #key buffer-class)
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-buffer> = frame-buffer(frame);
  let mode   :: <major-mode>   = buffer-major-mode(buffer);
  let editor   = frame-editor(frame);
  let interval = selected-language-object();
  let (definition, name-key, generator, major-mode, node-class)
    = definition-browser-parameters(mode, interval, what);
  when (definition)
    let buffer = make-empty-buffer(buffer-class,
				   name:       as(<string>, what),
				   definition: definition,
				   name-key:   name-key,
				   generator:  generator,
				   major-mode: major-mode,
				   node-class: node-class,
				   editor:     editor);
    with-busy-cursor (window)
      revert-buffer(buffer)
    end;
    select-buffer-in-appropriate-window(window, buffer)
  end;
  frame-last-command-type(frame) := #"file"
end method make-definition-browser;


/// Debugging redisplay

define command redisplay-toggle-bitblt (frame)
    "Toggle the state of bitblt scrolling."
  let window :: <basic-window> = frame-window(frame);
  $scroll-with-bitblt? := ~$scroll-with-bitblt?;
  display-message(window, if ($scroll-with-bitblt?)
			    "Bitblt scrolling enabled"
			  else
			    "Bitblt scrolling disabled"
			  end)  
end command redisplay-toggle-bitblt;

define command redisplay-toggle-debugging (frame)
    "Toggle the state of display debugging."
  let window :: <basic-window> = frame-window(frame);
  $debug-redisplay? := ~$debug-redisplay?;
  $debug-scrolling? :=  $debug-redisplay?;
  display-message(window, if ($debug-redisplay?)
			    "Redisplay debugging enabled"
			  else
			    "Redisplay debugging disabled"
			  end)  
end command redisplay-toggle-debugging;
