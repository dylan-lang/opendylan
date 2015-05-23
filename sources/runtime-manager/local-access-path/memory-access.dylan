module:     access-path-implementation
synopsis:   Implementation of functions for memory access
author:     Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// DO-REGISTERS

define method register-vector-on-connection
    (conn :: <local-access-connection>) => (vec :: <vector>)

  local method nub-register-descriptor
          (cat :: <symbol>, r :: <integer>)
            => (descriptor :: <unassigned-remote-register>)
          let name-length :: <integer> =
          nub-get-register-name-length (conn.connection-process, r);
          let enum-code :: <integer> =
            nub-get-register-enumeration-code(conn.connection-process, r);
          let register-name = make (<byte-string>, size: name-length);
          nub-get-register-name 
            (conn.connection-process, r, name-length, register-name);
          let reg = make (<unassigned-remote-register>,
                          descriptor: r,
                          name: register-name,
                          code: enum-code,
                          category: cat);

          reg;
        end method;

  let (first-general, last-general)
    = nub-general-registers (conn.connection-process);

  let (first-special, last-special)
    = nub-special-registers (conn.connection-process);

  let (first-register, last-register)
    = nub-all-registers (conn.connection-process);

  let register-vector =
    make (<vector>, size: (last-register - first-register + 1));

  for (i from first-general to last-general)
    register-vector[i - 1] := 
      nub-register-descriptor (#"general", i);
  end for;

  for (i from first-special to last-special)
    register-vector[i - 1] := 
      nub-register-descriptor (#"special", i);
  end for;

  register-vector;
end method;


///// READ-VALUE

define method read-value-from-register 
    (conn :: <local-access-connection>, register :: <active-remote-register>,
     #key frame-index = #f)
      => (val :: <remote-value>)

  let (value, error) = 
    if (frame-index)
      nub-read-value-from-process-register-in-stack-frame
         (conn.connection-process, register.register-thread.nub-descriptor,
          register.nub-descriptor, frame-index);
    else
      nub-read-value-from-process-register
	(conn.connection-process,
	 register.register-thread.nub-descriptor,
	 register.nub-descriptor);
    end if;

  if (error ~= $access-ok)
    signal (make (<remote-access-violation-error>));
  end if;
  value;
end method;

define method read-value-from-memory 
    (conn :: <local-access-connection>, location :: <remote-value>)
      => (val :: <remote-value>)

  let (value, error) =
    nub-read-value-from-process-memory (conn.connection-process, location);

  if (error ~= $access-ok)
    signal (make (<remote-access-violation-error>));
  end if;
  value;
end method;


///// WRITE-VALUE

define method write-value-to-register 
    (conn :: <local-access-connection>,
     register :: <active-remote-register>,
     value :: <remote-value>) => ()

  let err = 
    nub-write-value-to-process-register
    (conn.connection-process,
     register.register-thread.nub-descriptor,
     register.nub-descriptor,
     value);
  if (err ~= $access-ok)
    signal (make (<remote-access-violation-error>));
  end if;
end method;

define method write-value-to-memory 
    (conn :: <local-access-connection>, address :: <remote-value>,
     value :: <remote-value>) => ()

  let err =
    nub-write-value-to-process-memory (conn.connection-process, address, value);

  if (err ~= $access-ok)
     signal (make (<remote-access-violation-error>));
  end if
end method;


///// READ-SINGLE-FLOAT

define method read-single-float-from-register 
    (conn :: <local-access-connection>,
     register :: <active-remote-register>)
       => (val :: <single-float>)
  let (value, error) = 
    nub-read-single-float-from-process-register
      (conn.connection-process, register.register-thread.nub-descriptor, register.nub-descriptor);
  if (error ~= $access-ok)
    signal (make (<remote-access-violation-error>));
  end if;
  value;
end method;

define method read-single-float-from-memory 
    (conn :: <local-access-connection>, location :: <remote-value>)
       => (val :: <single-float>)
  let (value, error) =
    nub-read-single-float-from-process-memory (conn.connection-process, location);
  if (error ~= $access-ok)
    signal (make (<remote-access-violation-error>));
  end if;
  value;
end method;


///// WRITE-SINGLE-FLOAT

define method write-single-float-to-register 
    (conn :: <local-access-connection>, 
     register :: <active-remote-register>,
     value :: <single-float>) 
       => ()
  let error = 
    nub-write-single-float-to-process-register 
      (conn.connection-process, register.register-thread.nub-descriptor, register.nub-descriptor,
       value);
  if (error ~= $access-ok)
    signal (make (<remote-access-violation-error>));
  end if
end method;

define method write-single-float-to-memory 
    (conn :: <local-access-connection>, address :: <remote-value>,
     value :: <single-float>) 
       => ()
  let error =
    nub-write-single-float-to-process-memory (conn.connection-process, address, value);
  if (error ~= $access-ok)
    signal (make (<remote-access-violation-error>));
  end if
end method;


///// READ-DOUBLE-FLOAT

define method read-double-float-from-register 
    (conn :: <local-access-connection>,
     register :: <active-remote-register>)
       => (val :: <double-float>)
  let (value, error) = 
    nub-read-double-float-from-process-register 
      (conn.connection-process, register.register-thread.nub-descriptor, register.nub-descriptor);
  if (error ~= $access-ok)
    signal (make (<remote-access-violation-error>));
  end if;
  value;
end method;

define method read-double-float-from-memory 
    (conn :: <local-access-connection>, location :: <remote-value>)
       => (val :: <double-float>)
  let (value, error) =
    nub-read-double-float-from-process-memory (conn.connection-process, location);
  if (error ~= $access-ok)
    signal (make (<remote-access-violation-error>));
  end if;
  value;
end method;


///// WRITE-DOUBLE-FLOAT

define method write-double-float-to-register 
    (conn :: <local-access-connection>, 
     register :: <active-remote-register>,
     value :: <double-float>) 
       => ()
  let error = 
    nub-write-double-float-to-process-register 
      (conn.connection-process, register.register-thread.nub-descriptor, register.nub-descriptor,
       value);
  if (error ~= $access-ok)
    signal (make (<remote-access-violation-error>));
  end if
end method;

define method write-double-float-to-memory 
    (conn :: <local-access-connection>, address :: <remote-value>,
     value :: <double-float>) 
       => ()
  let error =
    nub-write-double-float-to-process-memory (conn.connection-process, address, value);
  if (error ~= $access-ok)
    signal (make (<remote-access-violation-error>));
  end if
end method;


///// READ-BYTE-STRING

define method read-byte-string-from-memory 
   (conn :: <local-access-connection>, address :: <remote-value>,
    length :: <integer>)
      => (val :: <byte-string>)

  let string-destination = make (<byte-string>, size: length);

  let error =
    nub-read-byte-string-from-process-memory (conn.connection-process, address, length,
                                      string-destination);

  if (error ~= $access-ok)
    signal (make (<remote-access-violation-error>));
  end if;
  string-destination
end method;


///// WRITE-BYTE-STRING

define method write-byte-string-to-memory
    (conn :: <local-access-connection>, address :: <remote-value>,
     string-source :: <byte-string>, ending-index :: <integer>) => ()
  let error =
    nub-write-byte-string-to-process-memory
      (conn.connection-process, address, ending-index + 1, string-source);
  if (error ~== $access-ok)
    signal(make(<remote-access-violation-error>));
  end if;
end method;


///// CALCULATE-STACK-ADDRESS
//    Returns the address of a position on the stack of the application's
//    thread. Offset 0 is the top of the stack. Offset 1 is the position
//    1 remote-value below the top of the stack, etc...

define method calculate-stack-address-on-connection
    (conn :: <local-access-connection>, thread :: <remote-thread>, 
     offset :: <integer>)
       => (addr :: <remote-value>)
  nub-calculate-stack-address
    (conn.connection-process, thread.nub-descriptor, offset);
end method;


///// REMOTE-VIRTUAL-PAGE-SIZE
//    Returns the size of a memory page on the remote machine. This is
//    given as an integer, measured in remote-value-sized units.

define method virtual-page-size-on-connection
    (conn :: <local-access-connection>) => (page-size :: <integer>)
  nub-virtual-page-size(conn.connection-process)
end method;


///// REMOTE-VALUE-BYTE-SIZE
//    Returns the size, in bytes, of a <remote-value> in the runtime.

define method remote-value-byte-size-on-connection
    (conn :: <local-access-connection>) => (value-size :: <integer>)
  nub-remote-value-byte-size(conn.connection-process)
end method;


///// PAGE-READ-PERMISSION?
//    Queries whether the given address lies within a read-protected page.

define method page-read-permission-on-connection?
    (conn :: <local-access-connection>, address :: <remote-value>)
       => (answer :: <boolean>)
  let nub-answer = nub-page-read-permission(conn.connection-process, address);
  if (nub-answer == 0)
    #f
  else
    #t
  end if
end method;


///// PAGE-WRITE-PERMISSION?
//    Queries whether the given address lies within a write-protected page.

define method page-write-permission-on-connection?
    (conn :: <local-access-connection>, address :: <remote-value>)
       => (answer :: <boolean>)
  let nub-answer = nub-page-write-permission(conn.connection-process, address);
  if (nub-answer == 0)
    #f
  else
    #t
  end if
end method;


///// PAGE-RELATIVE-ADDRESS
//    Turns an address into an integer-enumerated memory page ID, and an
//    offset into the page.

define method page-relative-address-on-connection
    (conn :: <local-access-connection>, addr :: <remote-value>)
       => (id :: <integer>, offset :: <integer>)
  let (pagenum, offset) =
     nub-page-relative-address(conn.connection-process, addr);
  values(pagenum, offset);
end method;


///// PERFORM-COFF-RELOCATION
//    Alters the contents of an address 'ra' according to COFF-file relocation
//    semantics. This is used by the interactive downloader.

define method perform-coff-relocation-on-connection
    (conn :: <local-access-connection>, 
     ra :: <remote-value>, da :: <remote-value>,
     #key relative? = #f)
       => (worked? :: <boolean>)
  let success =
    if (relative?)
      nub-perform-relative-relocation(conn.connection-process, ra, da)
    else
      nub-perform-absolute-relocation(conn.connection-process, ra, da)
    end if;
  // Turn the integer success code into a boolean.
  if (success == 1)
    #t
  else
    #f
  end if;
end method;
