Module:       io-internals
Synopsis:     *standard-input*, *standard-output*, *standard-error*
Author:       Gary Palter and Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define function carbon-read
    (fd :: <integer>, data :: <buffer>, offset :: <integer>, count :: <integer>)
 => (nread :: <integer>)
  let nread :: <integer>
    = raw-as-integer
        (%call-c-function ("read")
	     (fd :: <raw-c-unsigned-int>, address :: <raw-pointer>, 
	      size :: <raw-c-unsigned-long>)
	  => (result :: <raw-c-signed-int>)
	   (integer-as-raw(fd),
	    primitive-cast-raw-as-pointer
	      (primitive-machine-word-add
		 (primitive-cast-pointer-as-raw
		    (primitive-repeated-slot-as-raw(data,
						    primitive-repeated-slot-offset(data))),
		  integer-as-raw(offset))),
	    integer-as-raw(count))
	 end);
  if (nread > 0)
    // Need to convert '\r' to '\n' before passing the data back to our caller.
    // Otherwise, the Streams library will "peek" for the '\n' on seeing the '\r' forcing
    // the user to hit RETURN (or ENTER) twice for each line typed ...
    //---*** NOTE: We should fix Streams to peek in a "proper" fashion...
    let cr = as(<byte>, '\r');
    let nl = as(<byte>, '\n');
    for (i :: <integer> from offset below offset + nread)
      if (data[i] = cr) data[i] := nl end
    end
  end;
  nread
end function carbon-read;

define function carbon-write
    (fd :: <integer>, data :: <buffer>, offset :: <integer>, count :: <integer>)
 => (nwritten :: <integer>)
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
end function carbon-write;

// standard carbon error definitions
define function get-carbon-error (errno :: <integer>) => (message :: <string>)
  primitive-raw-as-string
    (%call-c-function ("primitive_errstr") (errno :: <raw-c-signed-int>) 
         => (result :: <raw-byte-string>)
       (integer-as-raw(errno))
     end)
end function get-carbon-error;

define function carbon-errno-value () => (errno :: <integer>)
  raw-as-integer
    (%call-c-function ("primitive_errno") () => (result :: <raw-c-signed-int>)
       () end)
end function carbon-errno-value;

define function carbon-error (syscall :: <string>, #key errno = #f) => ()
  let message :: <string> 
   = get-carbon-error
       (if (~errno) carbon-errno-value() else errno end);
  error("%s %s", syscall, message);
end function carbon-error;

/// Here's the actual console implementation ...

define class <console-stream-accessor> (<external-stream-accessor>)
end class <console-stream-accessor>;

define class <console-stream> (<double-buffered-stream>,
                               <byte-char-element-stream>,
                               <external-stream>)
  inherited slot stream-element-type = <byte-character>;
  constant slot file-descriptor :: <integer>, required-init-keyword: file-descriptor:;
  slot accessor :: false-or(<console-stream-accessor>) = #f,
    init-keyword: accessor:;  // inherited from <external-stream>
end class <console-stream>;

define method initialize (stream :: <console-stream>, #rest initargs, #key) => ()
  next-method();
  stream.accessor := make(<console-stream-accessor>);
  if (stream-direction(stream) = #"input")
    stream-input-buffer(stream) := make(<buffer>, size: 128)
  else
    stream-output-buffer(stream) := make(<buffer>, size: 1024);
    // Force first attempt to get an output buffer to find the console
    stream-output-buffer(stream).buffer-next := stream-output-buffer(stream).buffer-size
  end
end method initialize;

define variable *standard-input* 
  = make(<console-stream>, file-descriptor: 0, direction: #"input");

define variable *standard-output*
  = make(<console-stream>, file-descriptor: 1, direction: #"output");

define variable *standard-error*
  = make(<console-stream>, file-descriptor: 2, direction: #"output");

define function flush-stdout () => ()
  let ob = stream-output-buffer(*standard-output*);
  if (ob & ~(ob.buffer-start = ob.buffer-end))
    force-output(*standard-output*)
  end
end function flush-stdout;

// Doesn't really close the stream cause you can't really close
// console streams for carbon.  Just flushes standard output.  This
// is only expected to be called on application exit.
define method close
    (stream :: <console-stream>,
     #key abort? = #f) => ()
  unless (abort?)
    if (stream.stream-direction == #"output") 
      let ob = stream-output-buffer(stream);
      if (ob & ~(ob.buffer-start = ob.buffer-end))
	force-output(stream);
      end if;
    end if;
  end;
  //  DON'T CALL NEXT-METHOD()!! Lower level close methods shouldn't
  //  be called on console streams.
end method close;

/// Input protocol ...

define method stream-input-available?
    (stream :: <console-stream>) => (input-available? :: <boolean>)
  stream-direction(stream) = #"input"
  & (stream-input-buffer(stream).buffer-next < stream-input-buffer(stream).buffer-end
     | do-input-available-at-source?(stream))
end method stream-input-available?;

define method do-input-available-at-source?
    (stream :: <console-stream>) => (input-available? :: <boolean>)
  //---*** NOTE: This may not be the best choice but let's try it ...
  primitive-raw-as-boolean
    (%call-c-function ("kbhit")
	 () => (keypressed? :: <raw-c-signed-int>)
       ()
     end)
end method do-input-available-at-source?;

define method read-element
    (stream :: <console-stream>, #key on-end-of-stream)
 => (element :: <object>)
  flush-stdout();
  next-method()
end method read-element;

define method peek
    (stream :: <console-stream>, #key on-end-of-stream)
 => (element :: <object>)
  flush-stdout();
  next-method()
end method peek;

define method read
    (stream :: <console-stream>, n :: <integer>, #key on-end-of-stream)
 => (elements-or-eof :: <object>)
  flush-stdout();
  next-method()
end method read;

define method read-into!
    (stream :: <console-stream>, 
       n :: <integer>, sequence :: <mutable-sequence>,
     #key start = 0, on-end-of-stream)
 => (count-or-eof :: <object>)
  flush-stdout();
  next-method()
end method read-into!;

define method read-line
    (stream :: <console-stream>, #key on-end-of-stream)
 => (string-or-eof :: <object>, newline? :: <boolean>)
  flush-stdout();
  next-method()
end method read-line;

define method accessor-read-into!
    (accessor :: <console-stream-accessor>, stream :: <console-stream>,
     offset :: <buffer-index>, count :: <buffer-index>, #key buffer)
 => (nread :: <integer>)
  ignore(accessor);
  let bufv = as(<vector>, buffer | stream-input-buffer(stream));
  // N.B. No checking for sufficient length, e.g.
  // if (offset + count > buffer.size) error "Argh!!" end;
  let nread = carbon-read(stream.file-descriptor, bufv, offset, count);
  if (nread < 0)
    carbon-error("read");
  end if;
  nread
end method accessor-read-into!;


/// Output protocol ...

define method write-element
    (stream :: <console-stream>, element :: <character>) => ()
  next-method();
  if (element == '\n' | element == '\r')
    force-output(stream)
  end
end method write-element;

define method write-line
    (stream :: <console-stream>, elements :: <string>,
     #key start: _start = 0, end: _end = #f) => ()
  next-method();
  force-output(stream);
end method write-line;

define method new-line
    (stream :: <console-stream>) => ()
  next-method();
  force-output(stream)
end method new-line;

define method accessor-write-from
    (accessor :: <console-stream-accessor>, stream :: <console-stream>,
     offset :: <buffer-index>, count :: <buffer-index>, #key buffer,
     return-fresh-buffer? = #f)
 => (number-of-bytes-written :: <integer>, new-buffer :: <buffer>)
  ignore(accessor);
  let buffer = buffer | stream-output-buffer(stream);
  let bufv = as(<vector>, buffer);
  // N.B. No checking for sufficient length, e.g.
  // if (offset + count > buffer.size) error "Argh!!" end;
  let nwritten = carbon-write(stream.file-descriptor, bufv, offset, count);
  if (nwritten < 0)
    carbon-error("write")
  end if;
  values(nwritten, buffer)
end method accessor-write-from;

define method accessor-force-output
    (accessor :: <console-stream-accessor>,
     stream :: <console-stream>)
 => ()
  ignore(accessor);
end method accessor-force-output;

define method do-force-output-buffers
    (stream :: <console-stream>) => ()
  next-method();
  let sb :: <buffer> = stream-output-buffer(stream);
  sb.buffer-next := 0;
  sb.buffer-end := 0;
  values()
end method do-force-output-buffers;

define method accessor-newline-sequence
    (accessor :: <console-stream-accessor>)
 => (newline-sequence :: <sequence>);
  "\r"
end method accessor-newline-sequence;

define method accessor-close
    (accessor :: <console-stream-accessor>,
     #key abort? = #f, wait? = #t)
 => (closed? :: <boolean>)
  #f
end method;
