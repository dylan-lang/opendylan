Module:       common-dylan-internals
Author:       Gary Palter
Synopsis:     Common extensions to Dylan
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Darwin sysctl() constants
define constant $CTL_KERN = 1;
define constant $KERN_PROCARGS2 = 49;

// Wrap Darwin's sysctl() for the read-only case
// This allocates an appropriately sized data buffer for you
define function darwin-sysctl
  (mib :: <vector>) => (ret :: false-or(<byte-string>))
  let wsize = raw-as-integer(primitive-word-size());
  let rmib = make(<byte-string>, size: size(mib) * wsize, fill: '\0');
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
define inline-only function get-application-commandline
    () => (name :: <string>, arguments :: <simple-object-vector>)
  let pid
    = raw-as-integer(%call-c-function ("getpid") () => (pid :: <raw-c-signed-int>) () end);
  let cmdline = darwin-sysctl(vector($CTL_KERN, $KERN_PROCARGS2, pid));
  if (cmdline)
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
    *application-filename* := tokens[0];
    let name = make(<byte-string>);
    for (x in tokens[1])
      if (x == '/')
        name := make(<byte-string>);
      else
        name := add(name, x);
      end;
    end;
    values(name, apply(vector, copy-sequence(tokens, start: 2, end: argc + 1)))
  else
    values("", vector())
  end
end function;

define function get-application-filename
    () => (location :: false-or(<string>))
  let bufsiz :: <integer> = 128;
  let size = primitive-wrap-machine-word
    (primitive-cast-pointer-as-raw
       (%call-c-function ("malloc")
          (nbytes :: <raw-c-unsigned-long>) => (p :: <raw-c-pointer>)
          (integer-as-raw(4))
       end));
  block (return)
    while (#t)
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
        // Buffer too small, try again with the right size.
        bufsiz := raw-as-integer(primitive-c-unsigned-int-at
                                   (size, integer-as-raw(0), integer-as-raw(0)));
      end
    end;
    #f
  cleanup
    %call-c-function ("free")
      (p :: <raw-c-pointer>) => (void :: <raw-c-void>)
        (primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(size)))
    end
  end
end function;

