Module:       sockets-internals
Author:       Toby
Synopsis:     TCP sockets
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// CLIENTSIDE

define abstract primary class <platform-socket> (<buffered-socket>)
end class;

define abstract primary class <TCP-socket> (<platform-socket>)
end class;

define method initialize 
    (stream :: <platform-socket>, #rest initargs,
     #key 
       service :: false-or(type-union(<string>, <symbol>)), 
     host: requested-host :: 
       false-or(type-union(<internet-address>, <string>)) = #f, 
     port: requested-port :: false-or(<integer>) = #f,
     descriptor :: false-or(<accessor-socket-descriptor>) = #f,
     buffer-size: requested-buffer-size  :: false-or(<integer>) = #f,
     direction: requested-direction = #"input-output")
 => ()
  apply(next-method, stream, direction: requested-direction, initargs);
  // Descriptor is really only for internal use, in order to create 
  // sockets from socket descriptors returned by accept.
  unless(descriptor | service | requested-port)
    error("either port: or service: keyword to make <TCP-socket> is required");
  end unless;
  // port/service  doesn't work 
  let the-remote-port =
    if (requested-port)
      requested-port // ignore service if also specified
    elseif (service)
      accessor-get-port-for-service(as(<string>, service), "tcp")
    else #f	
    end if;
  // host
  let remote-host =
    select (requested-host by instance?)
      <internet-address> => requested-host;
      <string> => make(<internet-address>, name: requested-host);
      <boolean> => 
	unless (descriptor)
	  error("host: keyword to make <TCP-socket> is required");
	end unless;
    end select;
  unless (stream.accessor)
    stream.accessor := 
      apply(new-accessor,
	    type-for-socket(stream),
	    remote-host: remote-host,
	    remote-port: the-remote-port,
	    descriptor: descriptor, 
	    initargs);
  end unless;
  // Initializing the buffers has to be done here since we don't know
  // the accessor-preferred-buffer-size until we have an accessor.
  // Actually better might be to have this code in the initialize
  // method for <double-buffered-streams> and merely mess with the
  // buffer-size keyword but then the next method call has to be
  // elsewhere.  Might not work?  Investigate.
  let direction = stream.stream-direction;
  let size-for-buffers :: <integer> =
    if (requested-buffer-size) 
      requested-buffer-size
    else
      accessor-preferred-buffer-size(stream.accessor)
    end if;
  if ((direction == #"input") | (direction == #"input-output"))
    stream-input-buffer(stream) := make(<buffer>, size: size-for-buffers) 
  end;
  if ((direction == #"output") | (direction == #"input-output"))
    stream-output-buffer(stream) := make(<buffer>, size: size-for-buffers)
  end
end method initialize;

define method type-for-socket (socket :: <TCP-socket>)
 => (type == #"TCP")
  #"TCP"
end method;

define method make (class == <TCP-socket>, #rest initargs,
		    #key element-type = <byte-character>,
		    direction: requested-direction = #"input-output")
 => (stream :: <TCP-socket>)
  apply(make, client-class-for-element-type(class, element-type),
        direction: requested-direction,
        initargs)
end method make;

define generic client-class-for-element-type
    (class :: subclass(<socket>), element-type :: <type>)
 => (class :: subclass(<socket>));

define method client-class-for-element-type
    (class == <TCP-socket>, element-type == <byte>) => (class == <byte-TCP-socket>)
  <byte-TCP-socket>
end method;

define method client-class-for-element-type
    (class == <TCP-socket>, element-type == <byte-character>) => (class == <byte-char-TCP-socket>)
  <byte-char-TCP-socket>
end method;

define method client-class-for-element-type
    (class == <TCP-socket>, element-type :: <type>) => (class == <general-TCP-socket>)
  <general-TCP-socket>
end method;


define class <general-TCP-socket>
    (<TCP-socket>,
     <general-typed-stream>,
     <sealed-object>)
  inherited slot stream-element-type = <character>;
end class <general-TCP-socket>;

define class <byte-char-TCP-socket>
    (<TCP-socket>,
     <byte-char-element-stream>,
     <sealed-object>)
  inherited slot stream-element-type = <byte-character>;
end class <byte-char-TCP-socket>;

define class <byte-TCP-socket>
    (<TCP-socket>,
     <byte-element-stream>,
     <sealed-object>)
  inherited slot stream-element-type = <byte>;
end class <byte-TCP-socket>;

define method client-class-for-protocol (protocol == #"TCP")
  => (class == <TCP-socket>)
  <TCP-socket>
end method;

/// SERVERSIDE

define primary class
    <platform-server-socket> (<server-socket>, <sealed-object>)
  slot default-element-type :: <type>, init-keyword: element-type:,
    init-value: <byte-character>;
end class;

define primary class
    <TCP-server-socket> (<platform-server-socket>)
end class;

define method server-class-for-protocol (protocol == #"TCP")
 => (class == <TCP-server-socket>)
  <TCP-server-socket>
end method;

define inline method socket-code (socket :: <TCP-server-socket>)
  $SOCK-STREAM
end method;

define method initialize 
    (new-server-socket :: <platform-server-socket>, #rest initargs,
     #key service :: false-or(type-union(<string>, <symbol>)), 
     host: requested-host :: 
       false-or(type-union(<internet-address>, <string>)) = #f, 
     port: requested-port :: false-or(<integer>) = #f) => ()
  next-method();
  new-server-socket.socket-descriptor := accessor-new-socket-descriptor(socket-code(new-server-socket));
  if (service) 
    error("service keyword to make <server-socket> not supported yet");
  end if;
  // host
  let host-to-bind =
    select (requested-host by instance?)
      <internet-address> => requested-host;
      <string> => make(<internet-address>, name: requested-host);
      <boolean> => #"wildcard";
    end select;
  // port
  let port-to-bind =
    if (requested-port) 
      requested-port 
    elseif (service)
      accessor-get-port-for-service(as(<string>, service), "tcp")
    else #"wildcard" 
    end if;
    //
  // LispWorks sockets sets these options before calling bind. Investigate
  // what this is about later.
//  Y 4Jun94 put this into work, solve some problems.
//   if (*use_so_reuseaddr*)
//     if (0 > setsockopt(fd, *sockopt_sol_socket*, *sockopt_so_reuseaddr*,
// 		       pointer-cast(<c-char*>, on), size-of-on))
//       close-socket(fd);
//       return-from-create-tcp-socket-for-service(#f, -3);
//     end if;
//   end if;
//   if (*sockopt_so_dontlinger*)
//     if (0 > setsockopt(fd, *sockopt_sol_socket*,
// 		       *sockopt_so_dontlinger*,  
// 		       pointer-cast(<c-char*>, on), size-of-on))
//       close-socket(fd);
//       return-from-create-tcp-socket-for-service(#f, -4);
//     end if;
//   else
//     with-stack-structure(ls :: <LPLINGER>)
//       ls.l-onoff-value := 0;
//      if (0 > setsockopt(fd, *sockopt_sol_socket*, *sockopt_so_linger*,
// 			 pointer-cast(<c-char*>, ls), size-of(<LINGER>)))
//         close-socket(fd);
//         return-from-create-tcp-socket-for-service(#f, -5);
//       end if;
//     end with-stack-structure;
//   end if;
  // bind
  accessor-bind(new-server-socket,
		host-to-bind, port-to-bind);
  let (bound-host :: false-or(<ipv4-address>), bound-port :: false-or(<integer>)) =
    if ((host-to-bind == #"wildcard") | (port-to-bind == #"wildcard"))
      accessor-local-address-and-port(new-server-socket.socket-descriptor);
    end if;
  new-server-socket.local-host :=
    if (host-to-bind == #"wildcard") bound-host else host-to-bind end;
  new-server-socket.local-port :=
    if (port-to-bind == #"wildcard") bound-port else port-to-bind end;
//   if (*sockopt_tcp_nodelay*)
//     with-stack-structure(mi :: <C-int*>)
//       setsockopt(fd, *sockopt_ipproto_tcp*, *sockopt_tcp_nodelay*, 
// 		 pointer-cast(<c-char*>, mi), size-of(<C-int>));
//     end with-stack-structure;
//   end if;

  // listen, sometime propagate backlog keyword to initialize method
  accessor-listen(new-server-socket);
  // make the socket non-blocking, I think this may be unnecessary
  // since the event-select call in wait-for-socket-io is a stronger
  // way to make it non-blocking.
//   with-stack-structure(on :: <c-unsigned-long*>)
//     pointer-value(on) := 1;
//     ioctlsocket(fd, *ioctlsocket-fionbio*, on);
//   end with-stack-structure;
end method;

define method client-class-for-server (server-socket :: <TCP-server-socket>)
  => (class == <TCP-socket>)
  <TCP-socket>
end method;


