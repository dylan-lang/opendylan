Module:     socket-internals
Author:     Nosa Omo
Synopsis:   Streams for berkeley sockets
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

///  Classes
define class <socket-stream> (<external-stream>, <double-buffered-stream>)
  inherited slot stream-direction = #"input-output";
  slot input-buffer-handshake :: <function> = dummy-buffer-handshake,
    init-keyword: input-buffer-handshake:;
  slot output-buffer-handshake :: <function> = dummy-buffer-handshake,
    init-keyword: output-buffer-handshake:;
end class <socket-stream>;

define function dummy-buffer-handshake (#key arg)
  #f
end function dummy-buffer-handshake;

define class <general-socket-stream>
    (<socket-stream>,
     <general-typed-stream>)
  inherited slot stream-element-type = <character>;
end class <general-socket-stream>;

define class <byte-char-socket-stream>
    (<socket-stream>,
     <byte-char-element-stream>)
  inherited slot stream-element-type = <byte-character>;
end class <byte-char-socket-stream>;

define class <byte-socket-stream>
    (<socket-stream>,
     <byte-element-stream>)
  inherited slot stream-element-type = <byte>;
end class <byte-socket-stream>;


///  Creating Streams

define method initialize 
    (stream :: <socket-stream>, #rest initargs, #key) => ()
  next-method();
  unless (stream.accessor)
    stream.accessor := apply(new-accessor, #"socket", initargs)
  end;
  let direction = stream.stream-direction;
  if (direction ~= #"output")
    stream-input-buffer(stream) := make(<buffer>, size: 1024) // 8000
  end;
  if (direction ~= #"input")
    stream-output-buffer(stream) := make(<buffer>, size: 1024)
  end
end method initialize;

define method make (class == <socket-stream>, #rest initargs,
		    #key element-type = <byte-character>)
 => (stream :: <socket-stream>)
  let type
    = select (element-type)
        <byte> => <byte-socket-stream>;
        <byte-character> => <byte-char-socket-stream>;
        otherwise => <general-socket-stream>;
      end;
  if (type == class)
    next-method()
  else
    apply(make, type, initargs)
  end
end method make;

define function output-buffer-dirty?
    (stream :: <socket-stream>) => (dirty? :: <boolean>)
  let ob = stream-output-buffer(stream);
  ob & ~(ob.buffer-start = ob.buffer-next)
end function output-buffer-dirty?;

define method read-element
    (stream :: <socket-stream>, #key on-end-of-stream)
 => (element :: <object>)
  if (output-buffer-dirty?(stream))
    force-output(stream)
  end;
//  next-method(stream, on-end-of-stream: #f)
  next-method(stream)
end method read-element;

define method peek
    (stream :: <socket-stream>, #key on-end-of-stream)
 => (element :: <object>)
  if (output-buffer-dirty?(stream))
    force-output(stream)
  end;
//  next-method(stream, on-end-of-stream: #f)
  next-method(stream)
end method peek;

define method read
    (stream :: <socket-stream>, n :: <integer>, #key on-end-of-stream)
 => (elements :: <sequence>)
  if (output-buffer-dirty?(stream))
    force-output(stream)
  end;
//  next-method(stream, n, on-end-of-stream: #f)
  next-method(stream,n)
end method read;

define method read-into!
    (stream :: <socket-stream>, n :: <integer>, sequence :: <mutable-sequence>,
     #key start = 0, on-end-of-stream)
 => (elements :: <sequence>)
  if (output-buffer-dirty?(stream))
    force-output(stream)
  end;
//  next-method(stream, n, start: start, on-end-of-stream: #f)
  next-method(stream, n, start: start)
end method read-into!;

// Note that 'read-line' handles \n, \r, and \r\n...
define method read-line
    (stream :: <socket-stream>, #key on-end-of-stream)
 => (elements :: <sequence>, newline? :: <boolean>)
  if (output-buffer-dirty?(stream))
    force-output(stream)
  end;
//  next-method(stream, on-end-of-stream: #f)
  next-method(stream)
end method read-line;

define method do-next-input-buffer
    (stream :: <socket-stream>, #key wait? = #t, bytes = 1)
 => (buffer :: false-or(<buffer>))
  // If the input buffer has any data in it, do an input handshake
  if (stream-input-buffer(stream).buffer-next ~= 0)
     stream.input-buffer-handshake()
  end;
  next-method()
end method do-next-input-buffer;


///  Writable Stream Protocol

define method do-force-output-buffers
    (stream :: <socket-stream>) => ()
  let reinitialise-buffer 
    = method ()
        let sb = stream-output-buffer(stream);
        byte-vector-fill(sb, 0);
        sb.buffer-next := sb.buffer-start;
        sb.buffer-end := sb.size
      end method;
  // If the output buffer has any data in it, do an output handshake
  if (stream-output-buffer(stream).buffer-next ~= 0)
    let write-count = stream-output-buffer(stream).buffer-next;
    next-method();
    reinitialise-buffer();
    stream.output-buffer-handshake(arg: write-count);
    reinitialise-buffer()
  end
end method do-force-output-buffers;


///  Stream Testing Protocol

define method stream-at-end?
    (stream :: <socket-stream>) => (at-end? :: <boolean>)
  stream-direction(stream) ~= #"output"
  //---*** Is the setting of the EOF flag in the back-end correct?
  & stream.accessor.socket-connection-eof?
end method stream-at-end?;

define method stream-input-available?
    (stream :: <socket-stream>) => (available? :: <boolean>)
  stream-direction(stream) ~= #"output"
  //---*** This isn't so great...
  & #t
end method stream-input-available?;