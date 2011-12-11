Module:       common-dylan-internals
Author:       Gary Palter
Synopsis:     Common extensions to Dylan
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
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

// Darwin sysctl() constants
define constant $CTL_KERN = 1;
define constant $KERN_PROCARGS2 = 49;

// Wrap Darwin's sysctl() for the read-only case
// This allocates an appropriately sized data buffer for you
define function darwin-sysctl
  (mib :: <vector>) => (ret :: false-or(<byte-string>))
  let rmib = make(<byte-string>, size: size(mib), fill: '\0');
  let wsize = raw-as-integer(primitive-word-size());
  let rosize = make(<byte-string>, size: wsize, fill: '\0');

  // create the real mib vector
  for (i from 0 below size(mib))
    primitive-c-signed-int-at
      (primitive-cast-raw-as-pointer(primitive-string-as-raw(rmib)),
        integer-as-raw(0), integer-as-raw(i * wsize)) := integer-as-raw(mib[i])
  end for;

  // get the size of the available data
  when (raw-as-integer(%call-c-function ("sysctl")
    (mib :: <raw-byte-string>, cnt :: <raw-c-unsigned-int>,
     out :: <raw-byte-string> , osize :: <raw-byte-string>,
     in :: <raw-byte-string>, isize :: <raw-byte-string>)
    => (ret :: <raw-c-signed-int>)
    (primitive-string-as-raw(rmib), integer-as-raw(size(rmib)),
     primitive-unwrap-machine-word($machine-word-zero),
     primitive-string-as-raw(rosize),
     primitive-unwrap-machine-word($machine-word-zero),
     primitive-unwrap-machine-word($machine-word-zero)) end) >= 0)

    let osize = raw-as-integer(primitive-c-unsigned-long-at
      (primitive-cast-raw-as-pointer(primitive-string-as-raw(rosize)),
       integer-as-raw(0), integer-as-raw(0))) + 1;
    let out = make(<byte-string>, size: osize, fill: '\0');

    primitive-c-unsigned-long-at(primitive-cast-raw-as-pointer
      (primitive-string-as-raw(rosize)), integer-as-raw(0), integer-as-raw(0))
      := integer-as-raw(osize);

    // do the actual sysctl
    when(raw-as-integer(%call-c-function ("sysctl")
      (mib :: <raw-byte-string>, cnt :: <raw-c-unsigned-int>,
       out :: <raw-byte-string>, osize :: <raw-byte-string>,
       in :: <raw-byte-string>, isize :: <raw-byte-string>)
      => (ret :: <raw-c-signed-int>)
      (primitive-string-as-raw(rmib), integer-as-raw(size(mib)),
       primitive-string-as-raw(out), primitive-string-as-raw(rosize),
       primitive-unwrap-machine-word($machine-word-zero),
       primitive-unwrap-machine-word($machine-word-zero)) end) >= 0)
      out
    end when;
  end when;
end function;

/// This code uses one sysctl() to get the process arguments, and another
/// to get the process's filename. It only works on OS X > 10.3.
/// The data format returned by KERN_PROCARGS2 is:
/// [int32] <--- argc
/// [string] <--- cmd name
/// [NUL]* <--- 1-3 padding NUL's, to align next string
/// [string] <--- cmd name (again)
/// [NUL]* <--- more padding NUL's
/// [string] <--- first argument
/// [NUL]* <--- more padding
/// ... for each argument, each environment variable
define inline-only function ensure-application-name-filename-and-arguments () => ()
  unless (*application-name*)
    let pid
      = raw-as-integer(%call-c-function ("getpid") () => (pid :: <raw-c-signed-int>) () end);
    let cmdline = darwin-sysctl(vector($CTL_KERN, $KERN_PROCARGS2, pid));
    when (cmdline)
      let argc =
        raw-as-integer(primitive-c-signed-int-at
          (primitive-cast-raw-as-pointer(primitive-string-as-raw(cmdline)),
           integer-as-raw(0), integer-as-raw(0)));
      // tokenize the returned buffer
      let tokens = make(<stretchy-vector>);
      let _start = 4;
      let _end :: <integer> = size(cmdline);

      while ((_start < _end) & (size(tokens) < argc + 1))
        // skip null padding
        while ((_start < _end) & (cmdline[_start] = '\0'))
          _start := _start + 1;
        end;

        let token = make(<byte-string>);
        while ((_start < _end) & cmdline[_start] ~= '\0')
          token := add(token, cmdline[_start]);
          _start := _start + 1;
        end;

        if (~ empty?(token))
          add!(tokens, token);
        end;
      end while;
      *application-name* := tokens[0];
      *application-filename* := binary-location();
      *application-arguments*
        := apply(vector, copy-sequence(tokens, start: 2, end: argc + 1));
    end when;
  end unless;
end function;

define function binary-location
    () => (location :: false-or(<string>))
  let bufsiz :: <integer> = 128;
  let size = primitive-wrap-machine-word
    (primitive-cast-pointer-as-raw
       (%call-c-function ("GC_malloc")
          (nbytes :: <raw-c-unsigned-long>) => (p :: <raw-c-pointer>)
          (integer-as-raw(4))
       end));
  let err = -1;
  block (return)
    while (err == -1)
      let buffer = make(<byte-string>, size: bufsiz, fill: '\0');
      primitive-c-unsigned-int-at-setter(integer-as-raw(bufsiz), size,
                                         integer-as-raw(0), integer-as-raw(0));
      if (raw-as-integer
            (%call-c-function ("_NSGetExecutablePath")
               (buf :: <raw-byte-string>, siz :: <raw-c-pointer> /* uint32_t */)
               => (result :: <raw-c-signed-int>)
               (primitive-string-as-raw(buffer), size)
            end) == 0)
        let real-size = raw-as-integer(primitive-c-unsigned-int-at
                                         (size,
                                          integer-as-raw(0),
                                          integer-as-raw(0)));
        return(copy-sequence(buffer, end: real-size))
      else
        bufsiz := raw-as-integer(primitive-c-unsigned-int-at
                                   (size, integer-as-raw(0), integer-as-raw(0)));
      end
    end;
    #f
  cleanup
    %call-c-function ("GC_free") (p :: <raw-c-pointer>) => (void :: <raw-c-void>)
      (primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(size)))
    end
  end
end function;

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
