Module:       commands-internals
Synopsis:     Commands protocols and basic classes
Author:       Scott McKay, Hugh Greene
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// String-based commands

define open abstract class <string-command> (<command>)
end class <string-command>;

define open generic command-pattern-string
    (command :: <string-command>) => (pattern :: <string>);
define open generic command-argument-names
    (command :: <string-command>) => (argument-names :: <object-table>);
define open generic string-for-command
    (command :: <string-command>) => (string :: <string>);
define open generic string-for-argument
    (server, command :: <command>, name :: <symbol>, value) => (string :: <string>);

define open abstract primary class <basic-string-command>
    (<basic-command>, <string-command>)
  sealed constant slot command-pattern-string :: <string>,
    required-init-keyword: pattern-string:;
end class <basic-string-command>;


define macro string-command-definer
  { define ?modifiers:* string-command ?class:name (?superclasses:*) ?slots:* end }
    => { define constant "$" ## ?class ## "-names" :: <object-table> = make(<table>);
	 define ?modifiers string-command-class ?class (?superclasses)
	   sealed class slot command-argument-names :: <object-table>
	     = "$" ## ?class ## "-names",
	     setter: #f;
	   ?slots
	  end;
	 define string-command-slots ?class ?slots end; }
end macro string-command-definer;

define macro string-command-class-definer
  { define ?modifiers:* string-command-class ?class:name (?superclasses:*) ?slots:* end }
    => { define ?modifiers class ?class (?superclasses) ?slots end }
 slots:
  { } => { }
  { ?slot:*; ... } => { ?slot ... }
 slot:
  { ?modifiers:* named-argument ?arg-name:name is ?slot-name:name ?stuff:* }
    => { ?modifiers slot ?slot-name ?stuff; }
  { ?other:* } => { ?other; }
end macro string-command-class-definer;

define macro string-command-slots-definer
  { define string-command-slots ?class:name end }
    => { }
  { define string-command-slots ?class:name
      ?modifiers:* named-argument ?arg-name:name is ?slot-name:name ?stuff:*; ?more-slots:*
    end }
    => { "$" ## ?class ## "-names"[?#"arg-name"] := ?slot-name;
         define string-command-slots ?class ?more-slots end; }
  { define string-command-slots ?class:name
      ?other-slot:*; ?more-slots:*
    end }
    => { define string-command-slots ?class ?more-slots end; }
end macro string-command-slots-definer;


/// Argument substitution

define method string-for-command
    (command :: <basic-string-command>) => (string :: <string>)
  let server  = command-server(command);
  let pattern = command-pattern-string(command);
  let length :: <integer> = size(pattern);
  let result :: <stretchy-object-vector> = make(<stretchy-vector>);
  let i :: <integer> = 0;
  while (i < length)
    let char :: <character> = pattern[i];
    case
      char = '$' & i < length - 2 & pattern[i + 1] = '(' =>
	let open   = i + 1;
	let close  = find-char(pattern, ')', start: open, end: length);
	let name   = close & copy-sequence(pattern, start: open + 1, end: close);
	let name   = name & as(<symbol>, name);
	let getter = element(command-argument-names(command), name, default: #f);
	let object = getter & getter(command);
	let string = string-for-argument(server, command, name, object);
	if (getter)
	  for (j :: <integer> from 0 below size(string))
	    add!(result, string[j])
	  end;
	  i := close + 1
	else
	  for (j :: <integer> from i to (close | length - 1))
	    add!(result, pattern[j])
	  end;
	  i := (close | length - 1) + 1
	end;
      char = '$' & i < length - 2 & pattern[i + 1] = '$' =>
	add!(result, pattern[i]);
	i := i + 2;
      otherwise =>
	add!(result, pattern[i]);
	i := i + 1;
    end
  end;
  as(<string>, result)
end method string-for-command;

// We get the server and the name into the action so that the value can be
// printed in special ways, e.g., some server might want booleans to be printed
// as "yes" and "no" for some arguments
define method string-for-argument
    (server, command :: <command>, name :: <symbol>, value)
 => (string :: <string>)
  object-to-string(value)
end method string-for-argument;

define sealed method find-char
    (string :: <string>, char, #key start: _start = 0, end: _end = size(string))
 => (index :: false-or(<integer>))
  block (return)
    for (index :: <integer> from _start below _end)
      when (string[index] = char)
	return(index)
      end
    end
  end
end method find-char;


/// Value printing

define open generic object-to-string (object) => (string :: <string>);

// Default just uses 'format-to-string'
define method object-to-string
    (object :: <object>) => (string :: <string>)
  format-to-string("%=", object)
end method object-to-string;

define sealed method object-to-string
    (string :: <string>) => (string :: <string>)
  string
end method object-to-string;

define sealed method object-to-string
    (char :: <character>) => (string :: <string>)
  make(<string>, size: 1, fill: char)
end method object-to-string;

define sealed method object-to-string
    (symbol :: <symbol>) => (string :: <string>)
  as(<string>, symbol)
end method object-to-string;

define sealed method object-to-string
    (integer :: <integer>) => (string :: <string>)
  integer-to-string(integer)
end method object-to-string;

define sealed method object-to-string
    (float :: <float>) => (string :: <string>)
  float-to-string(float)
end method object-to-string;

define method object-to-string
    (sequence :: <sequence>) => (string :: <string>)
  select (size(sequence))
    0 => "";
    1 => object-to-string(sequence[0]);
    otherwise =>
      reduce1(method (s1, s2) concatenate(s1, ", ", s2) end method,
	      map(object-to-string, sequence))
  end
end method object-to-string;


/// Sample usage

/*
define open abstract primary string-command <editor-command> (<basic-string-command>)
  named-argument pathname is %pathname     :: <string>,
    required-init-keyword: pathname:;
  named-argument start-line is %start-line :: <integer> = 0,
    init-keyword: start-line:;
  named-argument start-col  is %start-col  :: <integer> = 0,
    init-keyword: start-col:;
  named-argument end-line   is %end-line   :: <integer> = 0,
    init-keyword: end-line:;
  named-argument end-col    is %end-col    :: <integer> = 0,
    init-keyword: end-col:;
end string-command <editor-command>;


define sealed class <lisp-new-file-command> (<editor-command>)
  keyword pattern-string: = "(new-file \"$(pathname)\");";
end class <lisp-new-file-command>;

define sealed class <lisp-open-file-command> (<editor-command>)
  keyword pattern-string: = "(open-file \"$(pathname)\")"
			    "(go-to \"$(pathname)\" $(start-line) $(start-col))"
end class <lisp-open-file-command>;

define sealed class <lisp-close-file-command> (<editor-command>)
  keyword pattern-string: = "(close-file \"$(pathname)\");";
end class <lisp-close-file-command>;


define sealed class <dde-new-file-command> (<editor-command>)
  keyword pattern-string: = "FileNew($(pathname));";
end class <dde-new-file-command>;

define sealed class <dde-open-file-command> (<editor-command>)
  keyword pattern-string: = "FileOpen($(pathname));"
			    "GoTo($(pathname), $(start-line), $(start-col));"
end class <dde-open-file-command>;

define sealed class <dde-close-file-command> (<editor-command>)
  keyword pattern-string: = "FileClose($(pathname));";
end class <dde-close-file-command>;
*/
