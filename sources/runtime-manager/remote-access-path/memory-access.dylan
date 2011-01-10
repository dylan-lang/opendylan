module:     remote-access-path
synopsis:   Implementation of functions for memory access
author:     Paul Howard, Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define method register-vector-on-connection
    (conn :: <remote-access-connection>) => (vec :: <vector>)

  local method nub-register-descriptor
          (cat :: <symbol>, r :: <integer>)
            => (descriptor :: <unassigned-remote-register>)
          let enum-code :: <integer> =
            Rtmgr/RemoteNub/get-register-enumeration-code(conn.nub, r);
          let register-name =
	    Rtmgr/RemoteNub/get-register-name(conn.nub, r);
          let reg = make (<unassigned-remote-register>,
                          descriptor: r,
                          name: register-name,
                          code: enum-code,
                          category: cat);

          reg;
        end method;

  let (first-general, last-general)
    = Rtmgr/RemoteNub/general-registers (conn.nub);
  let (first-special, last-special)
    = Rtmgr/RemoteNub/special-registers (conn.nub);
  let (first-register, last-register)
    = Rtmgr/RemoteNub/all-registers (conn.nub);
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
    (conn :: <remote-access-connection>, register :: <active-remote-register>,
     #key frame-index = #f)
      => (val :: <remote-value>)

  let (value :: <RTARGET-ADDRESS>, error) = 
    if (frame-index)
      Rtmgr/RemoteNub/read-value-from-process-register-in-stack-frame
         (conn.nub, register.register-thread.rnub-descriptor,
          register.nub-descriptor, frame-index);
    else
      Rtmgr/RemoteNub/read-value-from-process-register
	(conn.nub,
	 register.register-thread.rnub-descriptor,
	 register.nub-descriptor);
    end if;

  if (error ~= $access-ok)
    signal (make (<remote-access-violation-error>));
  end if;
  as-remote-value(value);
end method;

define method read-value-from-memory 
    (conn :: <remote-access-connection>, location :: <remote-value>)
      => (val :: <remote-value>)

  let (value :: <RTARGET-ADDRESS>, error) =
    Rtmgr/RemoteNub/read-value-from-process-memory (conn.nub, as-integer(location));

  if (error ~= $access-ok)
    signal (make (<remote-access-violation-error>));
  end if;
  as-remote-value(value);
end method;

///// WRITE-VALUE

define method write-value-to-register 
    (conn :: <remote-access-connection>,
     register :: <active-remote-register>,
     value :: <remote-value>) => ()
  let err = 
    Rtmgr/RemoteNub/write-value-to-process-register
    (conn.nub,
     register.register-thread.rnub-descriptor,
     register.nub-descriptor,
     as-integer(value));
  if (err ~= $access-ok)
    signal (make (<remote-access-violation-error>));
  end if;
end method;

define method write-value-to-memory 
    (conn :: <remote-access-connection>, address :: <remote-value>,
     value :: <remote-value>) => ()
  let err =
    Rtmgr/RemoteNub/write-value-to-process-memory
    (conn.nub, as-integer(address), as-integer(value));
  if (err ~= $access-ok)
     signal (make (<remote-access-violation-error>));
  end if
end method;

///// READ-SINGLE-FLOAT

define method read-single-float-from-register 
    (conn :: <remote-access-connection>,
     register :: <active-remote-register>)
       => (val :: <single-float>)
  let (value, error) = 
    Rtmgr/RemoteNub/read-single-float-from-process-register 
      (conn.nub, register.register-thread.rnub-descriptor, register.nub-descriptor);
  if (error ~= $access-ok)
    signal (make (<remote-access-violation-error>));
  end if;
  value;
end method;

define method read-single-float-from-memory 
    (conn :: <remote-access-connection>, location :: <remote-value>)
       => (val :: <single-float>)
  let (value, error) =
    Rtmgr/RemoteNub/read-single-float-from-process-memory
    (conn.nub, as-integer(location));
  if (error ~= $access-ok)
    signal (make (<remote-access-violation-error>));
  end if;
  value;
end method;


///// WRITE-SINGLE-FLOAT

define method write-single-float-to-register 
    (conn :: <remote-access-connection>, 
     register :: <active-remote-register>,
     value :: <single-float>) 
       => ()
  let error = 
    Rtmgr/RemoteNub/write-single-float-to-process-register 
      (conn.nub, register.register-thread.rnub-descriptor, register.nub-descriptor,
       value);
  if (error ~= $access-ok)
    signal (make (<remote-access-violation-error>));
  end if
end method;

define method write-single-float-to-memory 
    (conn :: <remote-access-connection>, address :: <remote-value>,
     value :: <single-float>) 
       => ()
  let error =
    Rtmgr/RemoteNub/write-single-float-to-process-memory
    (conn.nub, as-integer(address), value);
  if (error ~= $access-ok)
    signal (make (<remote-access-violation-error>));
  end if
end method;

///// READ-DOUBLE-FLOAT

define method read-double-float-from-register 
    (conn :: <remote-access-connection>,
     register :: <active-remote-register>)
       => (val :: <double-float>)
  let (value, error) = 
    Rtmgr/RemoteNub/read-double-float-from-process-register 
      (conn.nub, register.register-thread.rnub-descriptor, register.nub-descriptor);
  if (error ~= $access-ok)
    signal (make (<remote-access-violation-error>));
  end if;
  value;
end method;

define method read-double-float-from-memory 
    (conn :: <remote-access-connection>, location :: <remote-value>)
       => (val :: <double-float>)
  let (value, error) =
    Rtmgr/RemoteNub/read-double-float-from-process-memory
    (conn.nub, as-integer(location));
  if (error ~= $access-ok)
    signal (make (<remote-access-violation-error>));
  end if;
  value;
end method;

///// WRITE-DOUBLE-FLOAT

define method write-double-float-to-register 
    (conn :: <remote-access-connection>, 
     register :: <active-remote-register>,
     value :: <double-float>) 
       => ()
  let error = 
    Rtmgr/RemoteNub/write-double-float-to-process-register 
      (conn.nub, register.register-thread.rnub-descriptor, register.nub-descriptor,
       value);
  if (error ~= $access-ok)
    signal (make (<remote-access-violation-error>));
  end if
end method;

define method write-double-float-to-memory 
    (conn :: <remote-access-connection>, address :: <remote-value>,
     value :: <double-float>) 
       => ()
  let error =
    Rtmgr/RemoteNub/write-double-float-to-process-memory
    (conn.nub, as-integer(address), value);
  if (error ~= $access-ok)
    signal (make (<remote-access-violation-error>));
  end if
end method;

///// READ-BYTE-STRING

define method read-byte-string-from-memory 
   (conn :: <remote-access-connection>, address :: <remote-value>,
    length :: <integer>)
      => (val :: <byte-string>)

  let (string-destination :: <string>, error) =
    Rtmgr/RemoteNub/read-byte-string-from-process-memory
    (conn.nub, as-integer(address), length);

  if (error ~= $access-ok)
    signal (make (<remote-access-violation-error>));
  end if;
  string-destination
end method;

///// WRITE-BYTE-STRING

define method write-byte-string-to-memory
    (conn :: <remote-access-connection>, address :: <remote-value>,
     string-source :: <byte-string>, ending-index :: <integer>) => ()
  let error =
    Rtmgr/RemoteNub/write-byte-string-to-process-memory
      (conn.nub, as-integer(address), ending-index + 1, string-source);
  if (error ~== $access-ok)
    signal(make(<remote-access-violation-error>));
  end if;
end method;

///// CALCULATE-STACK-ADDRESS

define method calculate-stack-address-on-connection
    (conn :: <remote-access-connection>, thread :: <remote-thread>, 
     offset :: <integer>)
       => (addr :: <remote-value>)
  as-remote-value
  (Rtmgr/RemoteNub/calculate-stack-address
     (conn.nub, thread.rnub-descriptor, offset));
end method;


///// REMOTE-VIRTUAL-PAGE-SIZE

define method virtual-page-size-on-connection
    (conn :: <remote-access-connection>) => (page-size :: <integer>)
  Rtmgr/RemoteNub/virtual-page-size(conn.nub)
end method;


///// REMOTE-VALUE-BYTE-SIZE

define method remote-value-byte-size-on-connection
    (conn :: <remote-access-connection>) => (value-size :: <integer>)
  Rtmgr/RemoteNub/remote-value-byte-size(conn.nub)
end method;


///// PAGE-READ-PERMISSION?

define method page-read-permission-on-connection?
    (conn :: <remote-access-connection>, address :: <remote-value>)
       => (answer :: <boolean>)
  let nub-answer =
    Rtmgr/RemoteNub/page-read-permission(conn.nub, as-integer(address));
  if (nub-answer == 0)
    #f
  else
    #t
  end if
end method;


///// PAGE-WRITE-PERMISSION?

define method page-write-permission-on-connection?
    (conn :: <remote-access-connection>, address :: <remote-value>)
       => (answer :: <boolean>)
  let nub-answer =
    Rtmgr/RemoteNub/page-write-permission(conn.nub, as-integer(address));
  if (nub-answer == 0)
    #f
  else
    #t
  end if
end method;


///// PAGE-RELATIVE-ADDRESS

define method page-relative-address-on-connection
    (conn :: <remote-access-connection>, addr :: <remote-value>)
       => (id :: <integer>, offset :: <integer>)
  let (pagenum, offset) =
     Rtmgr/RemoteNub/page-relative-address(conn.nub, as-integer(addr));
  values(pagenum, offset);
end method;


///// PERFORM-COFF-RELOCATION

define method perform-coff-relocation-on-connection
    (conn :: <remote-access-connection>, 
     ra :: <remote-value>, da :: <remote-value>,
     #key relative? = #f)
       => (worked? :: <boolean>)
  let ra = as-integer(ra);
  let da = as-integer(da);
  let success =
    if (relative?)
      Rtmgr/RemoteNub/perform-relative-relocation(conn.nub, ra, da)
    else
      Rtmgr/RemoteNub/perform-absolute-relocation(conn.nub, ra, da)
    end if;
  // Turn the integer success code into a boolean.
  if (success == 1)
    #t
  else
    #f
  end if;
end method;
