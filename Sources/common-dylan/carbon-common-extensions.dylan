Module:       common-dylan-internals
Author:       Gary Palter
Synopsis:     Common extensions to Dylan
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

///---*** NOTE: This implementation only works with the C back-end which is the only
///---*** back-end available at this time for MacOS.  (The dependency is the
///---*** use of the psuedo_stdout external variable...)
define function format-out (format-string :: <string>, #rest format-arguments) => ()
  let string :: <string> = apply(format-to-string, format-string, format-arguments);
  write-console(string)
end function format-out;

define inline function write-console (string :: <string>, #key end: _end) => ()
  let string-size :: <integer> = _end | size(string);
  let terminal :: <raw-c-pointer>
    = primitive-c-pointer-at(%c-variable-pointer("pseudo_stdout", #f), 
			     integer-as-raw(0),
			     integer-as-raw(0));
  %call-c-function ("fwrite")
      (data :: <raw-c-pointer>, datumSize :: <raw-c-unsigned-long>,
       nDatums :: <raw-c-unsigned-long>, fd :: <raw-c-pointer>)
   => (nDatumsWritten :: <raw-c-unsigned-long>)
    (primitive-string-as-raw(string), integer-as-raw(1),
     integer-as-raw(string-size), terminal)
  end;
  //---*** NOTE: Should we do something here if we can't do the I/O???
  %call-c-function ("fflush") (fd :: <raw-c-pointer>) => (success? :: <raw-c-signed-int>)
    (terminal)
  end;
end function write-console;

define function default-random-seed () => (seed :: <integer>)
  let seed-buffer :: <byte-string>
    = make(<byte-string>, size: raw-as-integer(primitive-word-size()), fill: '\0');
  %call-c-function ("GetDateTime", c-modifiers: "pascal")
      (secs :: <raw-c-pointer>) => (void :: <raw-c-void>)
    (primitive-cast-raw-as-pointer(primitive-string-as-raw(seed-buffer)))
  end;
  raw-as-integer
    (primitive-c-unsigned-long-at
       (primitive-cast-raw-as-pointer(primitive-string-as-raw(seed-buffer)),
	integer-as-raw(0), integer-as-raw(0)))
end function default-random-seed;

/// See MacTypes.h for the definitions of Str31 and Str255 ...

define constant $STR31-SIZE  =  32;
define constant $STR255-SIZE = 256;

/// See Processes.h for the definition of ProcessSerialNumber and ProcessInfoRec ...

define constant $kCurrentProcess =  2;
define constant $PSN-SIZE        =  8;
define constant $PROCINFO-SIZE   = 60;

/// See Files.h for the definitions of FSSpec and CInfoPBRec ...

define constant $fsRtDirID       =   2;
define constant $FSSPEC-SIZE     =  70;
define constant $CINFOPBREC-SIZE = 104;

define variable *current-process-info* :: false-or(<byte-string>) = #f;

define variable *current-process-name* :: <byte-string>
  = make(<byte-string>, size: $STR31-SIZE, fill: '\0');

define variable *current-process-fsspec* :: <byte-string>
  = make(<byte-string>, size: $FSSPEC-SIZE, fill: '\0');

define function ensure-current-process-info () => ()
  unless (*current-process-info*)
    let info :: <byte-string> = make(<byte-string>, size: $PROCINFO-SIZE, fill: '\0');
    let psn :: <byte-string> = make(<byte-string>, size: $PSN-SIZE, fill: '\0');
    primitive-c-unsigned-long-at
        (primitive-cast-raw-as-pointer(primitive-string-as-raw(info)),
	 integer-as-raw(0), integer-as-raw(0))
      := integer-as-raw($PROCINFO-SIZE);
    primitive-c-pointer-at
        (primitive-cast-raw-as-pointer(primitive-string-as-raw(info)),
	 integer-as-raw(0), integer-as-raw(4))
      := primitive-cast-raw-as-pointer(primitive-string-as-raw(*current-process-name*));
    primitive-c-pointer-at
        (primitive-cast-raw-as-pointer(primitive-string-as-raw(info)),
	 integer-as-raw(0), integer-as-raw(56))
      := primitive-cast-raw-as-pointer(primitive-string-as-raw(*current-process-fsspec*));
    primitive-c-unsigned-long-at
        (primitive-cast-raw-as-pointer(primitive-string-as-raw(psn)),
	 integer-as-raw(0), integer-as-raw(4))
      := integer-as-raw($kCurrentProcess);
    let status
      = raw-as-integer
         (%call-c-function ("GetProcessInformation", c-modifiers: "pascal")
	      (PSN :: <raw-c-pointer>, info :: <raw-c-pointer>)
	   => (status :: <raw-c-signed-short>)
	    (primitive-cast-raw-as-pointer(primitive-string-as-raw(psn)),
	     primitive-cast-raw-as-pointer(primitive-string-as-raw(info)))
	  end);
    unless (zero?(status))
      *current-process-name*[0] := as(<byte-character>, 4);
      *current-process-name*[1] := *current-process-name*[2] 
	:= *current-process-name*[3] := *current-process-name*[4] := '?';
      fill!(*current-process-fsspec*, '\0')
    end;
    *current-process-info* := info
  end
end function ensure-current-process-info;

define function application-name () => (name :: <byte-string>)
  ensure-current-process-info();
  copy-sequence(*current-process-name*,
		start: 1, 
		end: as(<integer>, *current-process-name*[0]) + 1)
end function application-name;

/// NOTE: Under the MacOS Carbon interface, applications don't have arguments 
/// in the classic sense as there's no command line.  The only way we could
/// provide "arguments" would be to handle the OpenDocuments AppleEvent and
/// retrieve the list of files from it.  However, that would require an event
/// loop and should clearly be left to the application itself.
///---*** (NOTE: We should provide a mechanism for this, perhaps in DUIM!)
define function application-arguments () => (arguments :: <simple-object-vector>)
  #[]
end function application-arguments;

define function application-filename () => (filename :: false-or(<byte-string>))
  ensure-current-process-info();
  let vrefnum 
    = raw-as-integer
        (primitive-c-signed-short-at
	   (primitive-cast-raw-as-pointer(primitive-string-as-raw(*current-process-fsspec*)),
	    integer-as-raw(0), integer-as-raw(0)));
  let dirid
    = primitive-wrap-machine-word
        (primitive-c-signed-long-at
	   (primitive-cast-raw-as-pointer(primitive-string-as-raw(*current-process-fsspec*)),
	    integer-as-raw(0), integer-as-raw(2)));
  let filename
    = copy-sequence(*current-process-fsspec*,
		    start: 7,
		    end: as(<integer>, *current-process-fsspec*[6]) + 7);
  if (zero?(vrefnum) & zero?(dirid) & empty?(filename))
    #f
  else
    let cinfopbrec :: <byte-string> = make(<byte-string>, size: $CINFOPBREC-SIZE, fill: '\0');
    let dirname :: <byte-string> = make(<byte-string>, size: $STR255-SIZE, fill: '\0');
    let path = make(<stretchy-vector>);
    primitive-c-pointer-at
        (primitive-cast-raw-as-pointer(primitive-string-as-raw(cinfopbrec)),
         integer-as-raw(0), integer-as-raw(18))
      := primitive-cast-raw-as-pointer(primitive-string-as-raw(dirname));
    primitive-c-signed-short-at
        (primitive-cast-raw-as-pointer(primitive-string-as-raw(cinfopbrec)),
         integer-as-raw(0), integer-as-raw(22))
      := integer-as-raw(vrefnum);
    primitive-c-signed-long-at
        (primitive-cast-raw-as-pointer(primitive-string-as-raw(cinfopbrec)),
         integer-as-raw(0), integer-as-raw(100))
      := primitive-unwrap-machine-word(dirid);
    primitive-c-signed-short-at
        (primitive-cast-raw-as-pointer(primitive-string-as-raw(cinfopbrec)),
         integer-as-raw(0), integer-as-raw(28))
      := integer-as-raw(-1);
    let done :: <boolean> = #f;
    while (~done)
      primitive-c-signed-long-at
	  (primitive-cast-raw-as-pointer(primitive-string-as-raw(cinfopbrec)),
           integer-as-raw(0), integer-as-raw(48))
	:= primitive-c-signed-long-at
  	     (primitive-cast-raw-as-pointer(primitive-string-as-raw(cinfopbrec)),
	      integer-as-raw(0), integer-as-raw(100));
      %call-c-function ("PBGetCatInfoSync", c-modifiers: "pascal")
  	  (paramBlock :: <raw-c-pointer>) => (status :: <raw-c-signed-short>)
	(primitive-cast-raw-as-pointer(primitive-string-as-raw(cinfopbrec)))
      end;
      add!(path, copy-sequence(dirname, start: 1, end: as(<integer>, dirname[0]) + 1));
      done := $fsRtDirID
	      = raw-as-integer
	          (primitive-c-signed-long-at
		     (primitive-cast-raw-as-pointer(primitive-string-as-raw(cinfopbrec)),
		      integer-as-raw(0), integer-as-raw(48)));
    end;
    concatenate(reduce(rcurry(concatenate, ":"), "", reverse!(path)), filename)
  end
end function application-filename;

///---*** These inline-only functions really want to be local to
///---*** tokenize-command-string but our compiler doesn't yet
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

define function tokenize-command-string (line :: <byte-string>)
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
end function tokenize-command-string;
