Module:       sockets-internals
Author:       Toby
Synopsis:     Client sockets -- protocol independent classes
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

ignorable(force-output-before-read?-setter);

define constant <socket-designator> = type-union(<socket>, <socket-accessor>);

define abstract class 
    <socket-accessor> (<external-stream-accessor>)
end class;

define open abstract free class 
    <socket> (<abstract-socket>, <external-stream>) 
  inherited slot stream-direction = #"input-output"; // no one-way sockets
  slot accessor :: false-or(<socket-accessor>) = #f,
    init-keyword: accessor:; 
  slot force-output-before-read? :: <boolean>, 
    init-keyword: force-output-before-read?:, init-value: #t;
end class;

define method make 
    (class == <socket>, #rest initargs,
     #key protocol :: type-union(<string>, <symbol>) = #"TCP")
 => (stream :: <socket>)
  apply(make, client-class-for-protocol(as(<symbol>, protocol)), initargs)
end method make;

define open generic client-class-for-protocol (protocol :: <symbol>)
  => (class :: subclass(<socket>));

define method client-class-for-protocol (protocol :: <symbol>)
  => (class :: subclass(<socket>))
  error("unrecognized socket protocol: %s", protocol);
end method;

define open generic remote-host (the-socket :: <socket-designator>)
 => (host-address :: false-or(<internet-address>));

define open generic remote-port (the-socket :: <socket-designator>)
 => (host-address :: false-or(<integer>));

define method socket-descriptor (the-socket :: <socket-designator>)
 => (the-descriptor :: false-or(<accessor-socket-descriptor>));
  let acc = the-socket.accessor;
  acc & acc.socket-descriptor
end method;

define method remote-host
    (the-socket :: <socket>)
 => (the-host :: false-or(<internet-address>));
  the-socket.accessor.remote-host
end method;

define method remote-port
    (the-socket :: <socket>)
 => (the-port :: false-or(<integer>));
  the-socket.accessor.remote-port
end method;

define method close (the-socket :: <socket>, #rest keys, #key) => ()
  let manager = current-socket-manager();
  with-lock (socket-manager-lock(manager))
    if (socket-open?(the-socket))
      // Call next-method first so that socket-open? will still be true
      // for the next methods.  Make the value for already-unregistered?
      // explicit so that it won't be forgotten when we need this method
      // to do something else useful.
      apply(next-method, the-socket, already-unregistered?: #f, keys);
    end if;
  end with-lock;
end method;

//  Is there a reason to allow unbuffered sends or receives?  Sends
//  probably, think about fragmentation.

define open abstract free class 
    <buffered-socket> (<socket>, <double-buffered-stream>) end class;

// This should really be in double buffered streams.  Just need to
// bring console streams up to snuff 
define function output-buffer-dirty?
    (stream :: <buffered-socket>) => (dirty? :: <boolean>)
  // Shouldn't really need to test for existence of output buffer.
  // Well maybe with the next round of changes it will be necessary.
  let ob = stream-output-buffer(stream);
  ob & ob.buffer-dirty? // ~(ob.buffer-start = ob.buffer-next)
end function output-buffer-dirty?;


/// These methods seem to hit multi-threaded code where one thread
/// is trying to write requests, while another thread is trying to
/// blocking waiting for replies. If the reader also tries to write
/// via FORCE-OUTPUT as below then there is a race condition between
/// the reader and writer. However, FORCE-OUTPUTs cannot be removed
/// from the writer because only it knows when its finished. -- Jason

// All of the read methods share the following rule.  Pending output
// is sent before read is actually called.
define method read-element
    (stream :: <buffered-socket>, #key on-end-of-stream)
 => (element :: <object>)
  if (stream.force-output-before-read? & output-buffer-dirty?(stream))
    force-output(stream)
  end;
  next-method(stream)
end method read-element;

define method peek
    (stream :: <buffered-socket>, #key on-end-of-stream)
 => (element :: <object>)
  if (stream.force-output-before-read? & output-buffer-dirty?(stream))
    force-output(stream)
  end;
  next-method(stream)
end method peek;

define method read
    (stream :: <buffered-socket>, n :: <integer>, #key on-end-of-stream)
 => (elements-or-eof :: <object>)
  if (stream.force-output-before-read? & output-buffer-dirty?(stream))
    force-output(stream)
  end;
  next-method()
end method read;

define method read-into!
    (stream :: <buffered-socket>, n :: <integer>, 
     sequence :: <mutable-sequence>, #key start = 0, on-end-of-stream)
 => (count-or-eof :: <object>)
  if (stream.force-output-before-read? & output-buffer-dirty?(stream))
    force-output(stream)
  end;
  next-method()
end method read-into!;

// Note that 'read-line' handles \n, \r, and \r\n...
define method read-line
    (stream :: <buffered-socket>, #key on-end-of-stream)
 => (line-or-eof-object :: <object>, newline? :: <boolean>)
  if (stream.force-output-before-read? & output-buffer-dirty?(stream))
    force-output(stream)
  end;
  let (line-or-eof-object, newline?) = next-method();
  values(line-or-eof-object, newline?)
end method read-line;


///  Writable Stream Protocol

///  Stream Testing Protocol

define method stream-at-end?
    (stream :: <buffered-socket>) => (at-end? :: <boolean>)
  // Testing stream.accessor.connection-closed?  isn't enough.
  // The stream isn't at end if there is still unread stuff in the
  // input buffer.  If the input buffer is currently empty you don't
  // know if the stream is at end until you try to fill the buffer.
  // You can't try to fill the buffer however if the connection is
  // already closed so you must test that first.
  if ((~ stream.accessor.connection-closed?)
	& ((stream-direction(stream) == #"input")
	     | (stream-direction(stream) == #"input-output")))
    with-input-buffer (the-input-buffer = stream)
	(the-input-buffer = #f)
    end with-input-buffer
  else
    #f
  end if
end method stream-at-end?;

define method stream-input-available?
    (stream :: <buffered-socket>) => (available? :: <boolean>)
  if ((~ stream.accessor.connection-closed?)
        & ((stream-direction(stream) == #"input")
             | (stream-direction(stream) == #"input-output")))
    let buffer = stream.stream-input-buffer;
    if (buffer & buffer.buffer-next < buffer.buffer-end)
      #t
    else
      accessor-input-available?(stream.accessor.socket-descriptor)
    end if;
  else
    #f
  end
end method stream-input-available?;

define method do-force-output-buffers
    (stream :: <buffered-socket>) => ()
  next-method();
  let sb :: <buffer> = stream-output-buffer(stream);
  sb.buffer-next := 0;
  sb.buffer-end := 0;
end method do-force-output-buffers;

define macro with-socket
  { with-socket (?socket-var:name, #rest ?keys:expression)
      ?body:body
    end }
  => { begin
	 let _socket = #f;
	 block ()
	   _socket := make(<socket>, ?keys);
	   let ?socket-var :: <socket> = _socket;
	   ?body
	 cleanup
	   if (_socket & socket-open?(_socket)) close(_socket) end;
	 end
       end }
  { with-socket (?socket-var:name \:: ?socket-class:expression,
			#rest ?keys:expression)
      ?body:body
    end }
  => { begin
	 let _socket = #f;
	 block ()
	   _socket := make(?socket-class, ?keys);
	   let ?socket-var :: ?socket-class = _socket;
	   ?body
	 cleanup
	   if (_socket & socket-open?(_socket)) close(_socket) end;
	 end
       end }
end macro with-socket;

