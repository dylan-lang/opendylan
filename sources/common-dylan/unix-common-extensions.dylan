Module:       common-dylan-internals
Author:       Gary Palter
Synopsis:     Common extensions to Dylan
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define function format-out (format-string :: <string>, #rest format-arguments) => ()
  let string :: <string> = apply(format-to-string, format-string, format-arguments);
  write-console(string);
end function format-out;

define inline function write-console (string :: <string>, #key end: _end) => ()
  let string-size :: <integer> = _end | size(string);
  %call-c-function ("write")
      (fd :: <raw-c-signed-int>, buffer :: <raw-byte-string>, size :: <raw-c-unsigned-long>)
   => (count :: <raw-c-signed-int>)
    (integer-as-raw(1), primitive-string-as-raw(string), integer-as-raw(string-size))
  end;
  //---*** NOTE: Should we do something here if we can't do the I/O???
  %call-c-function ("fsync") (fd :: <raw-c-signed-int>) => (result :: <raw-c-signed-int>)
    (integer-as-raw(1))
  end;
end function write-console;


define thread variable *time-buffer* :: <byte-string>
  = make(<byte-string>, size: ash($machine-word-size, -3), fill: '\0');

define function default-random-seed () => (seed :: <integer>)
  %call-c-function ("time")
      (time :: <raw-c-pointer>) => (time :: <raw-c-signed-long>)
    (primitive-cast-raw-as-pointer(primitive-string-as-raw(*time-buffer*)))
  end;
  logior(as(<integer>, *time-buffer*[0]),
	 ash(as(<integer>, *time-buffer*[1]), 8),
	 ash(as(<integer>, *time-buffer*[2]), 16))
    + as(<integer>, *time-buffer*[3])
end function default-random-seed;


/// Application information

define variable *application-name* :: false-or(<byte-string>) = #f;
define variable *application-filename* :: false-or(<byte-string>) = #f;
define variable *application-arguments* :: <simple-object-vector> = #[];

///---*** NOTE: The following works on Linux and, presumably, any other UNIX
///---***       variant that implements the /proc filesystem.
define inline-only function ensure-application-name-filename-and-arguments () => ()
  unless (*application-name*)
    let pid
      = raw-as-integer(%call-c-function ("getpid") () => (pid :: <raw-c-signed-int>) () end);
    //
    let cmdline-path
      = concatenate("/proc/", integer-to-string(pid), "/cmdline");
    let cmdline-fd = -1;
    block ()
      cmdline-fd 
	:= raw-as-integer(%call-c-function ("open")
			      (path :: <raw-byte-string>, flags :: <raw-c-signed-int>,
			       mode :: <raw-c-signed-int>)
			   => (fd :: <raw-c-signed-int>)
			    (primitive-string-as-raw(cmdline-path),
			     integer-as-raw(0),
			     integer-as-raw(0))
			  end);
      if (cmdline-fd > 0)
	let cmdline :: <byte-string> = "";
	let count :: <integer> = 1;
	while (count > 0)
	  let buffer = make(<byte-string>, size: 8192, fill: '\0');
	  count
	    := raw-as-integer(%call-c-function ("read")
				  (fd :: <raw-c-signed-int>, buffer :: <raw-byte-string>,
				   size :: <raw-c-unsigned-long>)
			       => (count :: <raw-c-signed-int>)
				(integer-as-raw(cmdline-fd),
				 primitive-string-as-raw(buffer),
				 integer-as-raw(8192))
			      end);
	  if (count > 0)
	    cmdline := concatenate(cmdline, copy-sequence(buffer, end: count));
	  end;
	end;
	let tokens = make(<stretchy-vector>);
	let _start :: <integer> = 0;
	let _end :: <integer> = size(cmdline);
	let _skip :: <integer> = 0;
	while (_start < _end)
	  let _next :: <integer> = position(cmdline, '\0', test: \=, skip: _skip) | _end;
	  add!(tokens, copy-sequence(cmdline, start: _start, end: _next));
	  _start := _next + 1;
	  _skip := _skip + 1;
	end;
	*application-name* := tokens[0];
	*application-arguments* := apply(vector, copy-sequence(tokens, start: 1));
      end;
    cleanup
      if (cmdline-fd > 0)
	%call-c-function ("close")
	    (fd :: <raw-c-signed-int>) => (ok? :: <raw-c-signed-int>)
	  (integer-as-raw(cmdline-fd))
	end
      end
    end;
    //
    let exe-path
      = concatenate("/proc/", integer-to-string(pid), "/exe");
    let buffer = make(<byte-string>, size: 8192, fill: '\0');
    let count
      = raw-as-integer(%call-c-function ("readlink")
			   (path :: <raw-byte-string>, buffer :: <raw-byte-string>,
			    bufsize :: <raw-c-unsigned-long>)
			=> (count :: <raw-c-signed-int>)
			 (primitive-string-as-raw(exe-path),
			  primitive-string-as-raw(buffer),
			  integer-as-raw(8192))
		       end);
    unless (count = -1)
      *application-filename* := copy-sequence(buffer, end: count)
    end
  end
end function ensure-application-name-filename-and-arguments;

define function application-name () => (name :: <byte-string>)
  ensure-application-name-filename-and-arguments();
  *application-name*
end function application-name;

define function application-filename () => (filename :: false-or(<byte-string>))
  ensure-application-name-filename-and-arguments();
  *application-filename*
end function application-filename;

define function application-arguments () => (arguments :: <simple-object-vector>)
  ensure-application-name-filename-and-arguments();
  *application-arguments*
end function application-arguments;


///---*** These inline-only functions really want to be local to
///---*** tokenize-command-line but our compiler doesn't yet
///---*** inline local functions which are called more than once

define inline-only function whitespace? (c :: <character>) => (whitespace? :: <boolean>)
  c = ' ' | c = '\t' | c = '\n'
end function whitespace?;

define inline-only function skip-whitespace
    (string :: <byte-string>, _start :: <integer>, _end :: <integer>)
 => (_new-start :: <integer>)
  while (_start < _end & whitespace?(string[_start]))
    _start := _start + 1
  end;
  _start
end function skip-whitespace;

define function tokenize-command-line (line :: <byte-string>)
 => (command :: <byte-string>, #rest arguments :: <byte-string>)
  let tokens = #();
  let _start :: <integer> = 0;
  let _end :: <integer> = size(line);
  let token = make(<stretchy-vector>);
  local method next-token () => (token :: false-or(<byte-string>))
	  _start := skip-whitespace(line, _start, _end);
	  if (_start < _end)
	    let escaped? :: <boolean> = #f;
	    let quoted? :: false-or(<character>) = #f;
	    let done? :: <boolean> = #f;
	    token.size := 0;
	    while (_start < _end & ~done?)
	      let c :: <character> = line[_start];
	      case
		escaped? =>
		  add!(token, c);
		  escaped? := #f;
		quoted? & whitespace?(c) =>
		  add!(token, c);
		quoted? = c =>
		  quoted? := #f;
		c = '\\' =>
		  escaped? := #t;
		c = '"' | c = '\'' =>
		  quoted? := c;
		whitespace?(c) =>
		  done? := #t;
		otherwise =>
		  add!(token, c);
	      end;
	      _start := _start + 1
	    end;
	    concatenate-as(<byte-string>, token)
	  else
	    #f
	  end
	end method next-token;
  while (_start < _end)
    let token = next-token();
    if (token)
      tokens := add!(tokens, token)
    end
  end;
  apply(values, reverse!(tokens))
end function tokenize-command-line;
