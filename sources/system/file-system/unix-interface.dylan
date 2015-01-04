Module:       system-internals
Synopsis:     An interface to file-related unix system.
Author:       Eliot Miranda, Scott McKay, Marc Ferguson
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define macro with-interrupt-repeat
  { with-interrupt-repeat ?:body end }
    =>
  { iterate loop()
      let result = ?body;
      if(result < 0 & unix-errno() == $EINTR)
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

define function unix-lseek
    (fd :: <integer>, position :: <integer>, mode :: <integer>) => (position :: <integer>)
  raw-as-integer
    (%call-c-function ("io_lseek")
       (fd :: <raw-c-signed-int>, position :: <raw-c-signed-long>,
        mode :: <raw-c-signed-int>)
       => (result :: <raw-c-signed-long>)
       (integer-as-raw(fd), integer-as-raw(position), integer-as-raw(mode))
     end)
end function unix-lseek;

define function unix-raw-read
    (fd :: <integer>, address :: <machine-word>, count :: <integer>)
 => (result :: <integer>)
  with-interrupt-repeat
    raw-as-integer
      (%call-c-function ("read")
           (fd :: <raw-c-unsigned-int>, address :: <raw-pointer>,
            size :: <raw-c-unsigned-long>)
        => (result :: <raw-c-signed-int>)
         (integer-as-raw(fd),
          primitive-cast-raw-as-pointer
            (primitive-unwrap-machine-word(address)),
          integer-as-raw(count))
       end)
  end
end function unix-raw-read;

/// HIGHER LEVEL INTERFACE

define thread variable *stat-buffer* = make(<byte-vector>, size: $stat-size, fill: as(<byte>, '\0'));

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

//define constant $seek_set = 0;
//define constant $seek_cur = 1;
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
      #"freebsd", #"darwin" => #x200;
    end;

define constant $o_trunc
  = select ($os-name)
      #"linux"              =>   512;
      #"freebsd", #"darwin" => #x400;
    end;

define constant $o_sync
  = select ($os-name)
      #"linux"              =>  4096;
      #"freebsd", #"darwin" => #x80;
    end;

// standard unix error definitions
define constant $e_access = 13;
