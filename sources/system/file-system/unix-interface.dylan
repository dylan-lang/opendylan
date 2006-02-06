Module:       system-internals
Synopsis:     An interface to file-related unix system.
Author:       Eliot Miranda, Scott McKay, Marc Ferguson
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define macro with-interrupt-repeat
  { with-interrupt-repeat ?:body end }
    =>
  { iterate loop()
      let result = ?body;
      if(result < 0 & unix-errno-value() == $EINTR)
        loop()
      else
        result
      end if;
    end iterate }
end macro;

/// LOW LEVEL FFI

define function unix-open
    (path :: <byte-string>, mode :: <integer>, create-flags :: <integer>) => (fd :: <integer>)
  with-interrupt-repeat
    raw-as-integer
      (%call-c-function ("open")
           (path :: <raw-byte-string>, oflag :: <raw-c-unsigned-int>, 
            mode :: <raw-c-unsigned-int>)
        => (fd :: <raw-c-unsigned-int>)
         (primitive-string-as-raw(path), 
          integer-as-raw(mode), 
          integer-as-raw(create-flags))
       end)
  end
end function unix-open;

define function unix-close (fd :: <integer>) => (result :: <integer>)
  with-interrupt-repeat
    raw-as-integer
      (%call-c-function ("close") (fd :: <raw-c-unsigned-int>)
            => (result :: <raw-c-signed-int>)
         (integer-as-raw(fd)) end)
  end
end function unix-close;

define function unix-read
    (fd :: <integer>, data :: <buffer>, offset :: <integer>, count :: <integer>) => (result :: <integer>)
  with-interrupt-repeat
    raw-as-integer
      (%call-c-function ("read")
           (fd :: <raw-c-unsigned-int>, address :: <raw-pointer>, 
            size :: <raw-c-unsigned-long>)
        => (result :: <raw-c-signed-int>)
         (integer-as-raw(fd), 
	  primitive-cast-raw-as-pointer
	    (primitive-machine-word-add
	       (primitive-cast-pointer-as-raw
		  (primitive-repeated-slot-as-raw(data, primitive-repeated-slot-offset(data))), 
                primitive-cast-pointer-as-raw(integer-as-raw(offset)))), 
          integer-as-raw(count))
       end)
  end
end function unix-read;

define function unix-write
    (fd :: <integer>, data, offset :: <integer>, count :: <integer>) => (result :: <integer>)
  with-interrupt-repeat
    raw-as-integer
      (%call-c-function ("write")
           (fd :: <raw-c-unsigned-int>, address :: <raw-pointer>, 
            size :: <raw-c-unsigned-long>)
        => (result :: <raw-c-signed-int>)
         (integer-as-raw(fd), 
	  primitive-cast-raw-as-pointer
	    (primitive-machine-word-add
	       (primitive-cast-pointer-as-raw
		  (primitive-repeated-slot-as-raw(data, primitive-repeated-slot-offset(data))), 
	        primitive-cast-pointer-as-raw(integer-as-raw(offset)))), 
	  integer-as-raw(count))
       end)
  end
end function unix-write;

define function unix-lseek
    (fd :: <integer>, position :: <integer>, mode :: <integer>) => (position :: <integer>)
  with-interrupt-repeat
    raw-as-integer
      (%call-c-function ("lseek")
           (fd :: <raw-c-unsigned-int>, position :: <raw-c-unsigned-long>, 
            mode :: <raw-c-unsigned-int>) 
        => (result :: <raw-c-signed-int>)
         (integer-as-raw(fd), 
	  integer-as-raw(position), integer-as-raw(mode))
       end)
  end
end function unix-lseek;

define function get-unix-error (errno :: <integer>) => (message :: <string>)
  let message :: <byte-string>
    = primitive-raw-as-string
       (%call-c-function ("strerror") 
	    (errno :: <raw-c-signed-int>) => (message :: <raw-byte-string>)
	  (integer-as-raw(errno))
	end);
  // Make a copy to avoid it being overwritten ...
  copy-sequence(message)
end function get-unix-error;

define function unix-errno-value () => (errno :: <integer>)
  raw-as-integer
    (primitive-c-signed-int-at
      (%call-c-function ("__errno_location") () => (errnop :: <raw-pointer>) () end,
       integer-as-raw(0), integer-as-raw(0)))
end function unix-errno-value;


/// HIGHER LEVEL INTERFACE

/// This value is overkill, actually ...
define constant $stat-size = 128 * raw-as-integer(primitive-word-size());

define thread variable *stat-buffer* = make(<byte-vector>, size: $stat-size, fill: '\0');

define function unix-file-exists? (path :: <byte-string>) => (exists? :: <boolean>)
  ~primitive-raw-as-boolean
    (%call-c-function ("stat")
       (path :: <raw-byte-string>, statbuf :: <raw-pointer>)
      => (result :: <raw-c-signed-int>)
       (primitive-string-as-raw(path),
	primitive-cast-raw-as-pointer(primitive-string-as-raw(*stat-buffer*)))
     end)
end function unix-file-exists?;

define function unix-delete-file (path :: <byte-string>) => (ok :: <boolean>)
  with-interrupt-repeat
    raw-as-integer(%call-c-function ("unlink")
		       (path :: <raw-byte-string>) => (result :: <raw-c-signed-int>)
		     (primitive-string-as-raw(path))
		   end)
  end = 0;
end function unix-delete-file;

// POSIX lseek whence definitions:

define constant $seek_set = 0;
// define constant $seek_cur = 1;
define constant $seek_end = 2;

// Definitions for open mode arg.

define constant $o_rdonly = 0;
define constant $o_wronly = 1;
define constant $o_rdwr   = 2;

// define constant $o_append = 8;

// The following are very OS specific :(

define constant $o_creat
  = select ($os-name)
      #"linux"              =>   64;
      #"Solaris2", #"IRIX5" =>  256;
      #"SunOS4", #"OSF3"    =>  512;
      #"freebsd"            => #x200;
    end;

define constant $o_trunc
  = select ($os-name)
      #"Solaris2", #"IRIX5",  #"linux" =>  512;
      #"SunOS4", #"OSF3"               => 1024;
      #"freebsd"                       => #x400;
    end;

define constant $o_sync
  = select ($os-name)
      #"Solaris2", #"IRIX5" =>    16;
      #"linux"              =>  4096;
      #"SunOS4"             =>  8192;
      #"OSF3"               => 16384;
      #"freebsd"            => #x80;
    end;

// standard unix error definitions
define constant $e_access = 13;

define function unix-error (syscall :: <string>, #key errno = #f) => ()
  let message :: <string> 
   = get-unix-error
       (if (~errno) unix-errno-value() else errno end);
  error("%s %s", syscall, message);
end function unix-error;
