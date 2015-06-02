Module:       common-dylan-internals
Author:       Gary Palter
Synopsis:     Functional Objects extensions to Dylan
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// From WINBASE.H
define constant $STD_OUTPUT_HANDLE = -11;

define variable *console* :: false-or(<machine-word>) = #f;

/// Large enough to hold a Win32 DWORD ...
define thread variable *actual-count-buffer* :: <byte-string>
  = make(<byte-string>, size: ash($machine-word-size, -3));

define function ensure-console () => ()
  local method call-succeeded? (result :: <machine-word>) => (success :: <boolean>)
          primitive-machine-word-not-equal?
            (primitive-unwrap-machine-word(result),
             integer-as-raw(-1))
          & primitive-machine-word-not-equal?
              (primitive-unwrap-machine-word(result),
               integer-as-raw(0))
        end method;
  local method get-handle () => (handle :: <machine-word>)
          primitive-wrap-machine-word
            (primitive-cast-pointer-as-raw
               (%call-c-function ("GetStdHandle", c-modifiers: "__stdcall")
                    (nStdHandle :: <raw-c-unsigned-long>)
                 => (handle :: <raw-c-pointer>)
                  (integer-as-raw($STD_OUTPUT_HANDLE))
                end))
        end method;
  unless (*console*)
    let handle = get-handle();
    if (call-succeeded?(handle))
      *console* := handle
    elseif (primitive-raw-as-boolean
              (%call-c-function ("AllocConsole", c-modifiers: "__stdcall")
                   () => (success? :: <raw-c-signed-int>)
                 ()
               end))
      let handle = get-handle();
      *console* := call-succeeded?(handle) & handle
    end
    //---*** NOTE: Should we do something here if we can't get the console???
  end
end function ensure-console;

define function format-out (format-string :: <string>, #rest format-arguments) => ()
  let string :: <string> = apply(format-to-string, format-string, format-arguments);
  write-console(string);
end function format-out;

define inline function write-console
    (string :: <string>,
     #key end: _end,
          stream :: one-of(#"standard-output", #"standard-error") = #"standard-output")
 => ()
  let string-end :: <integer> = _end | size(string);
  ensure-console();
  when (*console*)
    %call-c-function ("WriteFile", c-modifiers: "__stdcall")
        (handle :: <raw-c-pointer>, buffer-ptr :: <raw-c-pointer>,
         count :: <raw-c-unsigned-long>, actual-count :: <raw-c-pointer>,
         lpOverlapped :: <raw-c-pointer>)
     => (success? :: <raw-c-signed-int>)
      (primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(*console*)),
       primitive-string-as-raw(string),
       integer-as-raw(string-end),
       primitive-string-as-raw(*actual-count-buffer*),
       primitive-cast-raw-as-pointer(integer-as-raw(0)))
    end
    //---*** NOTE: Should we do something here if we can't do the I/O???
    // No need to flush the console; in fact, we can't FlushFileBuffers on a console...
  end
end function write-console;


/// Large enough to hold a Win32 FILETIME ...
define thread variable *filetime-buffer* :: <byte-string>
  = make(<byte-string>, size: 2 * ash($machine-word-size, -3));

define function default-random-seed () => (seed :: <integer>)
  %call-c-function ("GetSystemTimeAsFileTime", c-modifiers: "__stdcall")
      (lpSystemTimeAsFileTime :: <raw-c-pointer>) => ()
    (primitive-string-as-raw(*filetime-buffer*))
  end;
  logior(as(<integer>, *filetime-buffer*[0]),
         ash(as(<integer>, *filetime-buffer*[1]), 8),
         ash(as(<integer>, *filetime-buffer*[2]), 16))
  + logior(as(<integer>, *filetime-buffer*[3]),
           ash(as(<integer>, *filetime-buffer*[4]), 8),
           ash(as(<integer>, *filetime-buffer*[5]), 16))
  + logior(as(<integer>, *filetime-buffer*[6]),
           ash(as(<integer>, *filetime-buffer*[7]), 8))
end function default-random-seed;


/// Application information

define variable *application-name* :: false-or(<byte-string>) = #f;
define variable *application-filename* :: false-or(<byte-string>) = #f;
define variable *application-arguments* :: <simple-object-vector> = #[];

define inline-only function ensure-application-name-filename-and-arguments () => ()
  unless (*application-name*)
    // The documentation for GetCommandLine claims it never fails ...
    let command-line = primitive-raw-as-string
                         (%call-c-function ("GetCommandLineA", c-modifiers: "__stdcall")
                              () => (line :: <raw-byte-string>)
                            ()
                          end);
    let (name, #rest arguments) = tokenize-command-line(command-line);
    *application-name* := name;
    *application-arguments* := apply(vector, arguments);
    //
    let path-buffer-size :: <integer> = 1024;
    let path-buffer :: <byte-string>
      = make(<byte-string>, size: path-buffer-size, fill: '\0');
    let path-size :: <integer>
      = raw-as-integer(%call-c-function ("GetModuleFileNameA", c-modifiers: "__stdcall")
                           (hModule :: <raw-c-pointer>,
                            lpFilename :: <raw-byte-string>,
                            nSize :: <raw-c-unsigned-long>)
                        => (value-size :: <raw-c-unsigned-long>)
                         (primitive-cast-raw-as-pointer(integer-as-raw(0)),
                          primitive-string-as-raw(path-buffer),
                          integer-as-raw(path-buffer-size))
                       end);
    if (path-size > path-buffer-size)
      // The documentation for GetModuleFileName doesn't state whether it returns
      // the actual size even if it won't fit in the buffer.  Let's hope it does ...
      let path-buffer-size :: <integer> = path-size + 1;
      path-buffer := make(<byte-string>, size: path-buffer-size, fill: '\0');
      path-size :=
        raw-as-integer(%call-c-function ("GetModuleFileNameA", c-modifiers: "__stdcall")
                           (hModule :: <raw-c-pointer>,
                            lpFilename :: <raw-byte-string>,
                            nSize :: <raw-c-unsigned-long>)
                        => (value-size :: <raw-c-unsigned-long>)
                         (primitive-cast-raw-as-pointer(integer-as-raw(0)),
                          primitive-string-as-raw(path-buffer),
                          integer-as-raw(path-buffer-size))
                       end)
    end;
    if (path-size > 0)
      *application-filename* := copy-sequence(path-buffer, end: path-size)
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

define inline-only function add-escapes
    (token :: <stretchy-vector>, n-escapes :: <integer>) => ()
  while (n-escapes > 0)
    add!(token, '\\');
    n-escapes := n-escapes - 1
  end
end function add-escapes;

/// Parses a command line into its constituent command name and arguments
/// according to the rules given in Microsoft's Visual C++ C reference
/// in the section "Parsing C Command-Line Arguments".  Basically, the
/// rules are simple except for the treatment of the escape character (\)
/// which is also the pathname delimiter character.
define function tokenize-command-line (line :: <byte-string>)
 => (command :: <byte-string>, #rest arguments :: <byte-string>)
  let tokens = #();
  let _start :: <integer> = 0;
  let _end :: <integer> = size(line);
  let token :: <stretchy-vector> = make(<stretchy-vector>);
  local method next-token () => (token :: false-or(<byte-string>))
          _start := skip-whitespace(line, _start, _end);
          if (_start < _end)
            let escaped? :: false-or(<integer>) = #f;
            let quoted? :: <boolean> = #f;
            let done? :: <boolean> = #f;
            token.size := 0;
            while (_start < _end & ~done?)
              let c :: <character> = line[_start];
              case
                escaped? & c = '\\' =>
                  escaped? := escaped? + 1;
                escaped? & c = '"' =>
                  if (even?(escaped?))
                    add-escapes(token, ash(escaped?, -1));
                    escaped? := #f;
                    _start := _start - 1;
                  else
                    add-escapes(token, ash(escaped? - 1, -1));
                    escaped? := #f;
                    add!(token, c);
                  end;
                escaped? =>
                  let n-escapes :: <integer> = escaped?;
                  add-escapes(token, n-escapes);
                  if (whitespace?(c) & ~quoted?)
                    done? := #t
                  else
                    add!(token, c)
                  end;
                  escaped? := #f;
                quoted? & whitespace?(c) =>
                  add!(token, c);
                c = '\\' =>
                  escaped? := 1;
                c = '"' =>
                  quoted? := ~quoted?;
                whitespace?(c) =>
                  done? := #t;
                otherwise =>
                  add!(token, c);
              end;
              _start := _start + 1
            end;
            if (escaped?)
              let n-escapes :: <integer> = escaped?;
              add-escapes(token, n-escapes)
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
