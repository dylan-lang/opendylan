Module:       sockets-internals
Author:       Toby
Synopsis:     Protocol independent unix socket accessor layer
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Utilities:

define macro with-cleared-stack-structure
  { with-cleared-stack-structure
        (?:name :: ?:expression, ?options:*)
      ?:body
    end }
    => { with-stack-structure (?name :: ?expression, ?options)
           clear-memory!(?name, size-of(referenced-type(?expression)));
           ?body 
         end }
end macro;

define constant <LPLINGER> = <linger*>;
define constant <LPHOSTENT> = <hostent*>;
define constant <LPSERVENT> = <servent*>;
define constant <LPSOCKADDR> = <sockaddr*>;
define constant <LPSOCKADDR-IN> = <sockaddr-in*>;
define constant $SOCKET-ERROR = -1;
define constant $INVALID-SOCKET = -1;

// --

define macro interruptible-system-call
  { interruptible-system-call(?:expression) }
    =>
  { iterate loop()
      let result = ?expression;
      if(result == $SOCKET-ERROR & unix-errno() == $EINTR)
        loop()
      else
        result
      end if;
    end iterate }
end macro;

define open class <unix-socket-accessor> (<socket-accessor>)
  slot remote-host :: false-or(<ipv4-address>), init-value: #f;
  slot remote-port :: false-or(<integer>), init-value: #f;
  slot local-host :: false-or(<ipv4-address>), init-value: #f;
  slot local-port :: false-or(<integer>), init-value: #f;
  slot socket-descriptor :: false-or(<accessor-socket-descriptor>);
  slot connected? :: <boolean>, init-value: #f;
  slot connection-closed? :: <boolean>, init-value: #f;
end class;

// errors
define class <socket-accessor-condition> (<socket-condition>, <sealed-object>)
end class;

define class <socket-accessor-error> (<socket-error>, <sealed-object>)
end class;

define class <unix-socket-error> (<socket-accessor-error>)
  // slot numeric-error-code :: <integer>, init-keyword: error-code:;
  // slot symbolic-error-code :: <string>;
  slot explanation :: <string>, init-value: "";
  slot calling-function :: <string>, init-keyword: calling-function:;
end class;

/*
define method initialize (the-condition :: <unix-socket-error>,
			  #key error-code, calling-function) => ()
  ignore(calling-function);
  next-method();
  the-condition.WSA-symbolic-error-code :=
    element($wsa-error-codes, error-code, 
	    default: "unknown-wsa-error");
end method;
*/

define function unix-socket-error
    (calling-function :: <string>, 
     #key format-string = "socket error %=", format-arguments = #[],
     high-level-error = #f, host-name = #f, host-address = #f, 
     host-port = #f, error-code: input-error-code = #f)
  if (instance?(host-name, <string>))
    // Get rid of annoying <c-string>s in the condition slots
    host-name := as(<byte-string>, host-name);
  end if;
  let error-code = 
    if (input-error-code) input-error-code else unix-errno() end;
  let unix-condition-object =
    make(<unix-socket-error>, calling-function: calling-function,
	 error-code: error-code, format-string: format-string,
	 format-arguments: concatenate(vector(error-code),
                                       format-arguments));
  if (high-level-error)
    high-level-error.socket-condition-details :=
      unix-condition-object;
  else
    // TODO: Decode documented unix errors
    select (error-code by ==)
      otherwise =>
	high-level-error := unix-condition-object;
    end select;
  end if;
  //  now actually signal.
  if (instance?(high-level-error, <recoverable-socket-condition>))
    signal(high-level-error);
  else
    error(high-level-error);
  end if;
end function;

// startup and shutdown

define class <stub-manager> (<socket-manager>, <sealed-object>)
  slot socket-manager-started? :: <boolean> = #f;
  slot socket-manager-closing-thread :: false-or(<thread>) = #f; // thread that closed socket-manager
  constant slot socket-manager-lock :: <recursive-lock> = make(<recursive-lock>);
				// lock to control opening and closing
				// of sockets so that corrupt and race
				// conditions are prevented during
				// socket manager shutdown
end class;

install-socket-manager(make(<stub-manager>));

define function accessor-startup (manager :: <socket-manager>) => (result :: <boolean>)
  unless (socket-manager-started?(manager))
    socket-manager-started?(manager) := #t;
    register-application-exit-function(stop-sockets);
  end unless;
  socket-manager-started?(manager)
end function;

// Temporarily disable automatic start-up until maybe we fix dylan
// initialization so maybe it isn't tied to dll initialization.
// begin
//   accessor-startup();
// end;
// 

define function accessor-cleanup (manager :: <socket-manager>) => ()
  with-lock(socket-manager-lock(manager))
    if (socket-manager-started?(manager))
      socket-manager-started?(manager) := #f;
      socket-manager-closing-thread(manager) := current-thread();
    elseif (socket-manager-closing-thread(manager))
      error("Internal error: accessor-cleanup called when sockets already "
	      "closed by thread:%s\n",
	    socket-manager-closing-thread(manager).thread-name | "unnamed thread");
    else
      error("Internal error: accessor-cleanup called but sockets were never"
	      " initialized");    
    end if;
  end with-lock;
end function;
 
// conversions and DNS calls
define method accessor-htonl (host-order-number :: <machine-word>)
 => (network-order-number :: <machine-word>);
  unix-htonl(host-order-number)
end method;

define method accessor-htonl (host-order-number :: <abstract-integer>)
 => (network-order-number :: <machine-word>);
  unix-htonl(as(<machine-word>, host-order-number))
end method;

define constant accessor-ntohl = unix-ntohl;
define constant accessor-htons = unix-htons;
define constant accessor-ntohs = unix-ntohs;
 
//  assume machine word in host order
define method accessor-ipv4-address-to-presentation 
    (input-ip-address :: <ipv4-numeric-address>) => (presentation :: <string>)
  let ip-address :: <machine-word> = input-ip-address.host-order; // I think
  format-to-string("%D.%D.%D.%D",
	 as(<integer>, %logand(#xFF, u%shift-right(ip-address, 24))),
         as(<integer>, %logand(#xFF, u%shift-right(ip-address, 16))),
         as(<integer>, %logand(#xFF, u%shift-right(ip-address, 8))),
         as(<integer>, %logand(#xFF, ip-address)));
end method;

define method accessor-ipv4-presentation-to-address
    (address-string :: <C-char*>) => 
    (result :: <ipv4-network-order-address>)
  // Filter out screw case
  if (address-string = "255.255.255.255")
    make(<ipv4-network-order-address>, 
	  // probably don't need htonl
	 address: accessor-htonl(as(<machine-word>, #xFFFFFFFF)))
  else 
    let machine-address :: <machine-word> = inet-addr(address-string);
    if (machine-address == $INADDR-NONE) 
      error(make(<invalid-address>,
		 format-string: "badly formed ip address string %s",
		 format-arguments: vector(address-string),
		 bad-address: address-string));
    else 
      make(<ipv4-network-order-address>, 
	   address: machine-address)
    end if
  end if
end method;

define method accessor-ipv4-presentation-to-address
    (ip-address-string :: <byte-string>) => 
    (result :: <ipv4-network-order-address>)
  // Filter out screw case
  if (ip-address-string = "255.255.255.255")
    make(<ipv4-network-order-address>, 
	  // probably don't need htonl
	 address: accessor-htonl(as(<machine-word>,#xFFFFFFFF)))
  else 
    let machine-address :: <machine-word> =
      with-C-string(address-as-c-string = ip-address-string) 
        inet-addr(address-as-c-string)
       end with-C-string;
    if (machine-address == $INADDR-NONE) 
      error(make(<invalid-address>,
		 format-string: "badly formed ip address string %s",
		 format-arguments: vector(ip-address-string),
		 bad-address: ip-address-string))
    else 
      make(<ipv4-network-order-address>, 
	   address: machine-address)
    end if
  end if
end method;

// unix-gethostbyname errors:
// 
// TODO: Fill in and trap

define constant $resolver-lock = make(<recursive-lock>);

define method get-host-entry 
    (the-name :: <C-string>) => (host-entry :: <LPHOSTENT>)
  let host-entry :: <LPHOSTENT> = unix-gethostbyname(the-name);
  if (null-pointer?(host-entry))
    let error-code :: <integer> = unix-errno();
    select (error-code by \==)
      /*
        $WSAHOST-NOT-FOUND => 
          // Maybe it's a presentation address.  Try to covert the
          // presentation address to a number.
          let address-as-number :: <machine-word> =
          if (the-name = "255.255.255.255")
            accessor-htonl(#xFFFFFFFF) // probably don't need htonl
          else 
            let result = inet-addr(the-name);
            if (result == $INADDR-NONE) 
              unix-socket-error("get-host-entry",
                                error-code: error-code,
                                format-string: 
                                  "Couldn't translate %s as a host name",
                                format-arguments:
                                  vector(as(<byte-string>,the-name)),
                                host-name: the-name);
            end if;
            result
          end if;
        with-stack-structure(hostnum-pointer :: <C-raw-unsigned-long*>)
          pointer-value(hostnum-pointer) := address-as-number;
          let result :: <LPHOSTENT> =
            unix-gethostbyaddr(pointer-cast(<C-char*>, hostnum-pointer),
                               size-of(<C-raw-unsigned-long>),
                               $AF-INET);
          if (null-pointer?(result))
            unix-socket-error("get-host-entry", 
                              error-code: error-code,
                              format-string: 
                                "Couldn't translate %s as a host name",
                              format-arguments: 
                                vector(as(<byte-string>, the-name)),
                              host-name: the-name);
          else
            host-entry := result;
          end if;
        end with-stack-structure;
      */
      otherwise =>
        unix-socket-error("get-host-entry", error-code: error-code,
                          format-string: 
                            "Error translating %s as a host name",
                          format-arguments: 
                            vector(as(<byte-string>, the-name)),
                          host-name: the-name);
    end select;
  end if;
  host-entry
end method;

// Helper functions for copying out parts of hostent structs returned
// by winsock functions.

define function copy-aliases (array-pointer :: <C-char**>)
 => (result :: <vector>)
 copy-aliases-recursive (array-pointer, 0)
end function;

// Aliases are a null terminated vector of pointers to null terminated
// strings.  The strings must be copied since the kernel will reuse
// the space.  This function recurs until it reaches the null in the
// outer vector, counting as it goes.  It allocates the <vector> for
// the results when it reaches the null and knows how big to make it.
// It fills in the result on the way out.

define function copy-aliases-recursive 
    (array-pointer :: <C-char**>, offset :: <integer>)
 => (result :: <vector>)
  let string-pointer :: <C-char*> = array-pointer[offset];
  if (null-pointer?(string-pointer))
    make(<simple-object-vector>, size: offset)
  else 
    let result = copy-aliases-recursive(array-pointer, offset + 1);
    result[offset] := as(<byte-string>, 
			 pointer-cast(<C-string>, string-pointer));
    result
  end if
end function;

// Only know about ipv4 addresses for now

define function copy-addresses (host-entry :: <LPHOSTENT>)
 => (result :: <vector>)
  unless ((host-entry.h-addrtype-value == $AF-INET) 
	    & (host-entry.h-length-value == 4))
    signal(make(<socket-accessor-error>, format-string:
		  "Unexpected address family or size for host address,"
		  "expecting 4 byte AF-INET address"));
  end unless;
 copy-addresses-recursive(pointer-cast(<in-addr**>,
				       host-entry.h-addr-list-value),
			  0)
end function;

// Same sort of deal as copy-aliases-recursive
define function copy-addresses-recursive 
    (array-pointer :: <in-addr**>, offset :: <integer>)
 => (result :: <vector>)
  let in-addr-pointer :: <in-addr*> = array-pointer[offset];
  if (null-pointer?(in-addr-pointer))
    make(<simple-object-vector>, size: offset)
  else 
    let result = 
      copy-addresses-recursive(array-pointer, offset + 1);
    result[offset] := 
      make(<ipv4-network-order-address>,
	   address: pointer-value(in-addr-pointer));
    result
  end if
end function;

define method accessor-get-host-by-name
    (new-address :: <ipv4-address>, input-name :: <string>) => ()
  //  ISSUE: this isn't thread safe.  Need to lock so that other
  //  threads don't smash the single hostent strut before we can copy
  //  the fields out of it.
  with-lock($resolver-lock)
    let host-entry :: <LPHOSTENT>
      = select (input-name by instance?)
          <C-char*> =>
            get-host-entry(input-name);
          <byte-string> =>
            with-C-string(input-name-as-C-string = input-name)
              get-host-entry(input-name-as-C-string);    
            end with-c-string;
        end select;
    // now fill in the fields of the <ipv4-address>. Everything must be
    // copied out of the
    new-address.%host-name := as(<byte-string>, 
                                 pointer-cast(<C-string>, 
                                              host-entry.h-name-value));
    new-address.%aliases := copy-aliases(host-entry.h-aliases-value);
    new-address.%addresses := copy-addresses(host-entry);
  end
end method;

// unix-gethostbyaddr error codes:
// 
// Return Values
// 
// If no error occurs, gethostbyaddr returns a pointer to the HOSTENT
// structure. Otherwise, it returns a NULL pointer, and a specific
// error code can be retrieved by calling WSAGetLastError.  
// 
// Error Codes
// 
// WSANOTINITIALISED A successful WSAStartup must occur before using
//   this function.
// WSAENETDOWN The network subsystem has failed.
// WSAHOST_NOT_FOUND Authoritative Answer Host not found.
// WSATRY_AGAIN Non-Authoritative Host not found, or server failed.
// WSANO_RECOVERY Nonrecoverable error occurred.
// WSANO_DATA Valid name, no data record of requested type.
// WSAEINPROGRESS A blocking Windows Sockets 1.1 call is in progress,
//   or the service provider is still processing a callback function.
// WSAEAFNOSUPPORT The type specified is not supported by the Windows
//   Sockets implementation.
// WSAEFAULT The addr parameter is not a valid part of the user
//   address space, or the len parameter is too small.
// WSAEINTRA blocking Windows Socket 1.1 call was canceled through
//   WSACancelBlockingCall. 

define function accessor-get-host-by-address
    (new-address :: <ipv4-address>) => ();
  with-lock($resolver-lock)
    let host-entry :: <LPHOSTENT>
      // Could maybe use a with-initialized-pointer macro here
      = with-stack-structure(hostnum-pointer :: <C-raw-unsigned-long*>)
          pointer-value(hostnum-pointer) := 
            new-address.numeric-host-address.network-order;
          let gethostbyaddr-result :: <LPHOSTENT> =
            unix-gethostbyaddr(pointer-cast(<C-char*>, hostnum-pointer),
                               size-of(<C-raw-unsigned-long>),
                               $AF-INET);
          if (null-pointer?(gethostbyaddr-result))
            let error-code :: <integer> = unix-errno();
            unix-socket-error("unix-gethostbyaddr", 
                              error-code: error-code,
                              format-string: 
                                "Couldn't translate %s as a host address",
                              format-arguments: 
                                vector(new-address.host-address),
                              host-address: new-address);
          end if;
          gethostbyaddr-result
        end with-stack-structure;
    // now fill in the fields of the <ipv4-address>. Everything must be
    // copied out of the
    new-address.%host-name := as(<byte-string>, 
                                 pointer-cast(<C-string>, 
                                              host-entry.h-name-value));
    new-address.%aliases := copy-aliases(host-entry.h-aliases-value);
    new-address.%addresses := copy-addresses(host-entry);
  end
end function;


define method accessor-get-port-for-service
    (service :: <c-string>, proto :: <c-string>) => (result :: <integer>)
  with-lock($resolver-lock)
    let sp :: <LPSERVENT> = 
      unix-getservbyname(service, proto);
    if (null-pointer?(sp))
      let service-error-code = unix-errno();
      let high-level-error =
        select (service-error-code by \==)
          // TODO: High level error messages
          /*
            $WSAHOST-NOT-FOUND, $WSANO-RECOVERY, $WSANO-DATA =>
            make(<service-not-found>,
                 format-string: "Service: %s not found for protocol: %s",
                 format-arguments: vector(service, proto),
                 service: as(<byte-string>, service),
                 protocol: as(<byte-string>, proto));
          */
          otherwise => #f;
        end select;
      unix-socket-error("unix-getservbyname", error-code: service-error-code,
                        high-level-error: high-level-error);
    else
      accessor-ntohs(sp.s-port-value);
    end if
  end
end method;

define method accessor-get-port-for-service
    (service :: <c-string>, proto :: <byte-string>) => 
    (result :: <integer>)
  with-c-string (proto-as-c-string = proto) 
    accessor-get-port-for-service(service, proto-as-c-string);
  end
end method;

define method accessor-get-port-for-service
    (service :: <byte-string>, proto :: <c-string>) => 
    (result :: <integer>)
  with-c-string (service-as-c-string = service)
    accessor-get-port-for-service(service-as-c-string, proto);
  end
end method;

define method accessor-get-port-for-service
    (service :: <byte-string>, proto :: <byte-string>) => 
    (result :: <integer>)
  with-c-string (service-as-c-string = service)
    with-c-string (proto-as-c-string = proto) 
      accessor-get-port-for-service
	(service-as-c-string, proto-as-c-string);
    end
  end;
end method;

// Results for getpeername
// 
// If no error occurs, getpeername returns zero. Otherwise, a value of
// SOCKET_ERROR is returned, and a specific error code can be
// retrieved by calling WSAGetLastError. 
// 
// Error Codes
// WSANOTINITIALISED A successful WSAStartup must occur before using
//   this function. 
// WSAENETDOWN The network subsystem has failed.
// WSAEFAULT The name or the namelen parameter is not a valid part of
//   the user address space, or the namelen parameter is too small. 
// WSAEINPROGRESS A blocking Windows Sockets 1.1 call is in progress,
//   or the service provider is still processing a callback function.
// WSAENOTCONN The socket is not connected.
// WSAENOTSOCK The descriptor is not a socket.

define function accessor-remote-address-and-port (the-descriptor :: <accessor-socket-descriptor>)
  => (connected? :: <boolean>,
      the-remote-address :: false-or(<ipv4-address>), 
      the-remote-port :: false-or(<integer>));
  let connected? = #t; // we will find out for sure
  let the-remote-address :: false-or(<ipv4-address>) = #f;
  let the-remote-port :: false-or(<integer>) = #f;
  with-cleared-stack-structure (inaddr :: <LPSOCKADDR-IN>)
    let addr = pointer-cast(<LPSOCKADDR>, inaddr);
    with-stack-structure (size-pointer :: <C-int*>)
      pointer-value(size-pointer) := size-of(<SOCKADDR-IN>);
      let getpeername-result = 
        unix-getpeername(the-descriptor, addr, size-pointer);
      if (getpeername-result == $SOCKET-ERROR)
	let error-code :: <integer> = unix-errno();
	select (error-code by \==)
          /* TODO: High level error conditions
	  $WSAENOTCONN => connected? := #f;
          */
	  otherwise =>
	    unix-socket-error("unix-getpeername", 
			       error-code: error-code);
	end select;
      else
	// Probably shouldn't ignore the result value for size-pointer and
	// the family value returned.
	the-remote-address := 
	  make(<ipv4-address>, 
	       address: make(<ipv4-network-order-address>,
			     address: inaddr.sin-addr-value));
	the-remote-port := accessor-ntohs(inaddr.sin-port-value);
      end if;  
    end with-stack-structure;
  end with-cleared-stack-structure;
  values(connected?, the-remote-address, the-remote-port)
end function;

define function accessor-local-address-and-port 
     (the-descriptor :: <accessor-socket-descriptor>)
  => (the-local-address :: <ipv4-address>, the-local-port :: <integer>)
  with-cleared-stack-structure (inaddr :: <LPSOCKADDR-IN>)
    let addr = pointer-cast(<LPSOCKADDR>, inaddr);
    with-stack-structure (size-pointer :: <C-int*>)
      pointer-value(size-pointer) := size-of(<SOCKADDR-IN>);
      let getsockname-result = 
        unix-getsockname(the-descriptor, addr, size-pointer);
      if (getsockname-result == $SOCKET-ERROR)
	unix-socket-error("unix-getsockname");
      end if;  
    end with-stack-structure;
    // Probably shouldn't ignore the result value for size-pointer and
    // the family value returned.
    let the-local-address = 
      make(<ipv4-address>, 
	   address: make(<ipv4-network-order-address>,
			 address: inaddr.sin-addr-value));
    let the-local-port = accessor-ntohs(inaddr.sin-port-value);
    values(the-local-address, the-local-port)
  end with-cleared-stack-structure
end function;


define function accessor-local-host-name()
  => (local-host-name :: <string>);
  let local-host-name = "";
  let name-buffer-size = 256; // wag for the size
  let name-buffer :: <byte-vector> =
    make(<byte-vector>, size: name-buffer-size, fill: as(<integer>, '\0')); 
  with-C-string(name-buffer-as-C-string = name-buffer)
    let gethostname-result = 
      unix-gethostname(name-buffer-as-C-string, name-buffer-size);
    if (gethostname-result == $SOCKET-ERROR)
      unix-socket-error("unix-gethostname");
    end if;      
    local-host-name := as(<byte-string>, name-buffer-as-C-string);
  end with-c-string;
  local-host-name
end function;

define constant <accessor-socket-descriptor> 
  = type-union(<C-void*>, <machine-word>, <integer>);

// Return Values
// 
// If no error occurs, bind returns zero. Otherwise, it returns
// SOCKET_ERROR, and a specific error code can be retrieved by calling
// WSAGetLastError.
// 
// Error Codes
// 
// WSANOTINITIALISED A successful WSAStartup must occur before using
//   this function.
// WSAENETDOWN The network subsystem has failed.
// WSAEADDRINUSE A process on the machine is already bound to the same
//   fully-qualified address and the socket has not been marked to
//   allow address re-use with SO_REUSEADDR. For example, IP address
//   and port are bound in the af_inet case) . (See the SO_REUSEADDR
//   socket option under setsockopt.)
// WSAEADDRNOTAVAIL The specified address is not a valid address for
//   this machine
// WSAEFAULT The name or the namelen parameter is not a
//   valid part of the user address space, the namelen parameter is
//   too small, the name parameter contains incorrect address format
//   for the associated address family, or the first two bytes of the
//   memory block specified by name does not match the address family
//   associated with the socket descriptor s.
// WSAEINPROGRESS A blocking Windows Sockets 1.1 call is in progress,
//   or the service provider is still processing a callback function.
// WSAEINVAL The socket is already bound to an address.Countries
// WSAENOBUFS Not enough buffers available, too many connections.
// WSAENOTSOCK The descriptor is not a socket.

define function accessor-bind 
    (the-socket :: <abstract-socket>,
     local-host-address :: 
       type-union(<ipv4-address>, singleton(#"wildcard")), 
     local-port-number :: 
       type-union(<integer>, singleton(#"wildcard")))
 => ();
  with-stack-structure (inaddr :: <LPSOCKADDR-IN>)
    if (local-host-address == #"wildcard")
      inaddr.sin-family-value := $AF-INET;
      inaddr.sin-addr-value := accessor-htonl($INADDR-ANY); 
    else
      inaddr.sin-family-value := local-host-address.address-family;
      inaddr.sin-addr-value := 
        local-host-address.numeric-host-address.network-order;
    end if;
    if (local-port-number == #"wildcard")
      inaddr.sin-port-value := accessor-htons(0);
    else 
      inaddr.sin-port-value := accessor-htons(local-port-number);
    end if;
    let addr = pointer-cast(<LPSOCKADDR>, inaddr);
    let bind-result = 
      unix-bind(the-socket.socket-descriptor, addr, size-of(<SOCKADDR-IN>));
    if (bind-result = $SOCKET-ERROR)
      close-socket(the-socket);
      unix-socket-error("unix-bind");
    end if;
  end with-stack-structure;
end function;

// Return Values
// 
// If no error occurs, listen returns zero. Otherwise, a value of
// SOCKET_ERROR is returned, and a specific error code can be
// retrieved by calling WSAGetLastError.
// 
// Error Codes
// 
// WSANOTINITIALISED A successful WSAStartup must occur before using
//   this function.
// WSAENETDOWN The network subsystem has failed.
// WSAEADDRINUSE The socket's local address is already in use and the
//   socket was not marked to allow address reuse with
//   SO_REUSEADDR. This error usually occurs during execution of the
//   bind function, but could be delayed until this function if the
//   bind was to a partially wild-card address (involving ADDR_ANY)
//   and if a specific address needs to be "committed" at the time of
//   this function.
// WSAEINPROGRESS A blocking Windows Sockets 1.1 call is in progress,
//   or the service provider is still processing a callback function.
// WSAEINVAL The socket has not been bound with bind.
// WSAEISCONN The socket is already connected.
// WSAEMFILE No more socket descriptors are available.
// WSAENOBUFS No buffer space is available.
// WSAENOTSOCK The descriptor is not a socket. 
// WSAEOPNOTSUPP The referenced socket is not of a type that supports
//   the listen operation.

define method accessor-listen 
    (the-socket :: <TCP-server-socket>, #key backlog :: <integer> = 5)
 => ()
  // Backlog of five is the maximum value allowed by Windows sockets.
  // Actually it's overkill since winsock silently reduces it to 2.
  // The backlog governs how many accept requests will be queued
  // before requests are turned down.  Backlog of 2 seems pretty
  // stingy.
  let listen-result = unix-listen(the-socket.socket-descriptor, backlog);
  if (listen-result = $SOCKET-ERROR)
    close-socket(the-socket);
    unix-socket-error("unix-listen");
  end if;
end method;


define method accessor-shutdown
    (the-descriptor :: <accessor-socket-descriptor>) => ();
  let shutdown-result = shutdown(the-descriptor, $SHUT-WR); // TODO: Why?
  if (shutdown-result = $SOCKET-ERROR)
    unix-socket-error("unix-shutdown");
  end if;
end method;    

define method accessor-input-available?
    (the-descriptor :: <accessor-socket-descriptor>) => (input? :: <boolean>);
  with-stack-structure(pollfds :: <pollfd*>)
    pollfds.pollfd-fd := the-descriptor;
    pollfds.pollfd-events := $POLLIN;
    pollfds.pollfd-revents := 0;
    poll(pollfds, 1, 0)
  end > 0
end method accessor-input-available?;

define method accessor-close-socket
    (the-descriptor :: <accessor-socket-descriptor>) => ();
  let manager = current-socket-manager();
  if (socket-manager-started?(manager))
    let close-result = unix-closesocket(the-descriptor);
    if (close-result == $SOCKET-ERROR)
      let error-code :: <integer> = unix-errno();
      select (error-code by \==)
	otherwise =>
	  signal(make(<unix-socket-error>, 
		      calling-function: "unix-closesocket",
		      error-code: error-code));
      end select;
    end if;
  elseif (socket-manager-closing-thread(manager))
    error(make(<socket-accessor-closed-error>,
	       calling-function: "accessor-close-socket",
	       calling-thread: current-thread(),
	       accessor-started?-value: socket-manager-started?(manager),
	       thread-that-closed-accessor: socket-manager-closing-thread(manager),
	       format-string:
		 "Internal Error: "
		 "accessor-close-socket was called from thread: %s after "
		 "winsock2 was apparently closed by thread: %s",
	       format-arguments:
		 vector(current-thread().thread-name | "unknown thread",
			socket-manager-closing-thread(manager).thread-name | "unknown thread")));
  else
    error(make(<socket-accessor-closed-error>,
	       calling-function: "accessor-close-socket",
	       calling-thread: current-thread(),
	       accessor-started?-value: socket-manager-started?(manager),
	       thread-that-closed-accessor: socket-manager-closing-thread(manager),
	       format-string:
		 "Internal Error: "
		 "accessor-close-socket was called from thread: %s but "
		 "winsock2 was apparently never initialized",
	       format-arguments:
		 vector(current-thread().thread-name | "unknown thread")));
  end if;
end method;
