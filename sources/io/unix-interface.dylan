Module:       io-internals
Synopsis:     An interface to file-related unix system.
Author:       Eliot Miranda, Scott McKay, Marc Ferguson
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $EINTR   =  4;

define function unix-errno () => (errno :: <integer>)
  raw-as-integer
    (%call-c-function ("io_errno") () => (result :: <raw-c-signed-int>)() end)
end function;

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
                integer-as-raw(offset))),
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
                integer-as-raw(offset))),
          integer-as-raw(count))
       end)
  end
end function unix-write;

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

define function unix-fsync (fd :: <integer>) => (result :: <integer>)
  with-interrupt-repeat
    raw-as-integer
      (%call-c-function ("fsync") (fd :: <raw-c-unsigned-int>)
            => (result :: <raw-c-signed-int>)
         (integer-as-raw(fd)) end)
  end
end function unix-fsync;

define function unix-fd-positionable?
    (fd :: <integer>)
 => (positionable? :: <boolean>);
  let info :: <integer>
    = raw-as-integer(%call-c-function ("io_fd_positionable")
                       (fd :: <raw-c-unsigned-int>) => (result :: <raw-c-signed-int>)
                       (integer-as-raw(fd))
                     end);
  ~zero?(info)
end function unix-fd-positionable?;

define function unix-isatty
    (fd :: <integer>)
 => (result :: <boolean>)
  let res :: <integer>
    = raw-as-integer(%call-c-function ("isatty")
                       (fd :: <raw-c-unsigned-int>) => (result :: <raw-c-signed-int>)
                       (integer-as-raw(fd))
                     end);
  (res == 1)
end function unix-isatty;

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


/// HIGHER LEVEL INTERFACE

define function unix-error
    (syscall :: <string>, #key errno = #f)
 => (does-not-return :: <bottom>);
  let message :: <string> = get-unix-error (errno | unix-errno());
  error("%s: %s", syscall, message);
end function unix-error;

// POSIX lseek whence definitions:

define constant $seek_set = 0;
define constant $seek_cur = 1;
define constant $seek_end = 2;
