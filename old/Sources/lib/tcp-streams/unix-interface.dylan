Module:      tcp-streams
Synopsis:    An interface to file-related unix system.
Author:      Eliot Miranda and Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// LOW LEVEL FFI

define method unix-open
    (host :: <byte-string>, port :: <integer>) => (fd :: <integer>)
  raw-as-integer
    (%call-c-function ("connect_to_numbered_server") 
         (host :: <raw-byte-string>, port :: <raw-c-unsigned-int>)
            => (fd :: <raw-c-unsigned-int>)
       (primitive-string-as-raw(host), integer-as-raw(port)) end)
end method unix-open;

define method unix-open
    (host :: <byte-string>, port :: <byte-string>) => (fd :: <integer>)
  raw-as-integer
    (%call-c-function ("connect_to_named_server") 
         (host :: <raw-byte-string>, port :: <raw-byte-string>)
            => (fd :: <raw-c-unsigned-int>)
       (primitive-string-as-raw(host), primitive-string-as-raw(port)) end)
end method unix-open;

define method unix-close
    (fd :: <integer>) => (result :: <integer>);
  raw-as-integer
    (%call-c-function ("close") (fd :: <raw-c-unsigned-int>)
       => (result :: <raw-c-signed-int>)
       (integer-as-raw(fd)) end)
end method unix-close;

define method unix-read
    (fd :: <integer>, data, offset :: <integer>, count :: <integer>)
 => (result :: <integer>);
  raw-as-integer
    (%call-c-function ("read")
         (fd :: <raw-c-unsigned-int>, address :: <raw-pointer>, 
          size :: <raw-c-unsigned-long>)
      => (result :: <raw-c-signed-int>)
       (integer-as-raw(fd), 
	primitive-cast-raw-as-pointer
	  (primitive-machine-word-add
	     (primitive-cast-pointer-as-raw(primitive-vector-as-raw(data)), 
	      primitive-cast-pointer-as-raw(integer-as-raw(offset)))), 
	integer-as-raw(count))
     end)
end method unix-read;

define method unix-write
    (fd :: <integer>, data, offset :: <integer>, count :: <integer>)
 => (result :: <integer>);
  raw-as-integer
    (%call-c-function ("write")
         (fd :: <raw-c-unsigned-int>, address :: <raw-pointer>, 
          size :: <raw-c-unsigned-long>)
      => (result :: <raw-c-signed-int>)
       (integer-as-raw(fd), 
	primitive-cast-raw-as-pointer
	  (primitive-machine-word-add
	     (primitive-cast-pointer-as-raw(primitive-vector-as-raw(data)), 
	      primitive-cast-pointer-as-raw(integer-as-raw(offset)))), 
	integer-as-raw(count))
     end)
end method unix-write;

define method get-unix-error
    (errno :: <integer>) => (message :: <string>);
  primitive-raw-as-string
    (%call-c-function ("primitive_errstr") 
         (errno :: <raw-c-signed-int>) => (result :: <raw-byte-string>)
       (integer-as-raw(errno))
     end)
end method get-unix-error;

define method unix-errno-value () => (errno :: <integer>);
  raw-as-integer
    (%call-c-function ("primitive_errno") 
        () => (result :: <raw-c-signed-int>)
      () end)
end method unix-errno-value;

// standard unix error definitions
define constant $e_access = 13;

define method unix-error (syscall :: <string>, #key errno = #f) => ();
  let message :: <string> 
   = get-unix-error(if (~errno) unix-errno-value() else errno end);
  error("%s %s", syscall, message);
end method unix-error;
