Module:       sockets-internals
Author:       Jason Trenouth
Synopsis:     UDP sockets
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// EXTERNALS

define abstract class <UDP-socket> (<platform-socket>)
end class;

define class <byte-UDP-socket> (<UDP-socket>, <byte-element-stream>, <sealed-object>)
end class;

define class <byte-char-UDP-socket> (<UDP-socket>, <byte-char-element-stream>, <sealed-object>)
end class;

define class <general-UDP-socket> (<UDP-socket>, <general-typed-stream>, <sealed-object>)
end class;

define primary class <UDP-server-socket> (<platform-server-socket>)
end class;

define method server-class-for-protocol (protocol == #"UDP")
  => (class == <UDP-server-socket>)
  <UDP-server-socket>
end method;

define method client-class-for-protocol (protocol == #"UDP")
  => (class == <UDP-socket>)
  <UDP-socket>
end method;

define method client-class-for-server (server-socket :: <UDP-server-socket>)
  => (class == <UDP-socket>)
  <UDP-socket>
end method;

define method client-class-for-element-type 
    (class == <UDP-socket>, element-type :: <type>)
  => (class == <general-UDP-socket>)
  <general-UDP-socket>
end method;

define method client-class-for-element-type 
    (class == <UDP-socket>, element-type == <byte>)
  => (class == <byte-UDP-socket>)
  <byte-UDP-socket>
end method;

define method client-class-for-element-type 
    (class == <UDP-socket>, element-type == <byte-character>)
  => (class == <byte-char-UDP-socket>)
  <byte-char-UDP-socket>
end method;

define method make (class == <UDP-socket>, #rest initargs,
		    #key element-type = <byte-character>,
		    direction: requested-direction = #"input-output")
 => (stream :: <UDP-socket>)
  apply(make, client-class-for-element-type(class, element-type),
        direction: requested-direction,
        initargs)
end method make;

/// Setup serverside reply socket as wrapper around server socket descriptor
define method accept 
    (server-socket :: <UDP-server-socket>, #rest args, #key element-type = #f, #all-keys)
 => (connected-socket :: <UDP-socket>);
  let socket = next-method();
  let accessor = socket.accessor;
  accessor.local-host := server-socket.local-host;
  accessor.local-port := server-socket.local-port;
  accessor.reply-socket? := #t;
  socket
end method;

define method type-for-socket (socket :: <UDP-socket>)
 => (type == #"UDP")
  #"UDP"
end method;

