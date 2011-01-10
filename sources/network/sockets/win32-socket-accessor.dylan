Module:       sockets-internals
Author:       Toby
Synopsis:     Protocol independent winsock2 accessor layer
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <win32-socket-accessor> (<socket-accessor>)
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

define class <win32-socket-error> (<socket-accessor-error>)
//  slot WSA-symbolic-error-code :: <string>;
  slot explanation :: <string>, init-value: "";
  slot calling-function :: <string>, init-keyword: calling-function:;
end class;

/*
define method initialize (the-condition :: <win32-socket-error>,
			  #key error-code, calling-function) => ()
  ignore(calling-function);
  next-method();
  the-condition.WSA-symbolic-error-code :=
    element($wsa-error-codes, error-code, 
	    default: "unknown-wsa-error");
end method;
*/
 
// Most winsock2 functions return  $SOCKET-ERROR on error.  This isn't
// true for the startup function though.
define function win32-socket-error
    (calling-function :: <string>, 
     #key format-string = "socket error", format-arguments = #[],
     high-level-error = #f, host-name = #f, host-address = #f, 
     host-port = #f, error-code: input-error-code = #f)
  if (instance?(host-name, <string>))
    // Get rid of annoying <c-string>s in the condition slots
    host-name := as(<byte-string>, host-name);
  end if;
  let error-code = 
    if (input-error-code) input-error-code else WSAGetLastError() end;
  let win32-condition-object =
    make(<win32-socket-error>, calling-function: calling-function,
	 error-code: error-code, format-string: format-string,
	 format-arguments: format-arguments);
  if (high-level-error)
    high-level-error.socket-condition-details := win32-condition-object;
  else
    //     high-level-error :=
    select (error-code by ==)
      $WSAVERNOTSUPPORTED, $WSAEINPROGRESS, $WSAEFAULT,
      $WSAEAFNOSUPPORT, $WSAENOTSOCK, 
      $WSAEINVAL, $WSAEISCONN,
      $WSAEOPNOTSUPP, $WSAEWOULDBLOCK, $WSAEACCES,
      $WSAEMSGSIZE =>
	high-level-error := 
	  make(<internal-socket-error>, details: win32-condition-object,
	       format-string: format-string, 
	       format-arguments: format-arguments);
      $WSANOTINITIALISED =>
	high-level-error := 
	  make(<sockets-not-initialized>, details: win32-condition-object,
	       format-string: 
		 "Sockets library not initialized.  Call start-sockets"
		 " before calling this function.");
      $WSAEINTR =>
	high-level-error := 
	  make(<blocking-call-interrupted>, details: win32-condition-object,
	       format-string: format-string, 
	       format-arguments: format-arguments);
      $WSAEPROCLIM, $WSAENOBUFS, $WSAEMFILE =>
	high-level-error := 
	  make(<out-of-resources>, details: win32-condition-object,
	       format-string: format-string, 
	       format-arguments: format-arguments);
      $WSASYSNOTREADY, $WSAENETDOWN =>
	high-level-error := 
	  make(<network-not-responding>, details: win32-condition-object,
	       format-string: format-string, 
	       format-arguments: format-arguments);
      $WSAHOST-NOT-FOUND, $WSANO-RECOVERY, $WSANO-DATA =>
	high-level-error := 
	  make(<host-not-found>, details: win32-condition-object,
	       format-string: format-string,
	       format-arguments: format-arguments, 
	       host-name: host-name, host-address: host-address);
      $WSATRY-AGAIN =>
	high-level-error :=
	  make(<server-not-responding>, details: win32-condition-object,
	       format-string: format-string, 
	       format-arguments: format-arguments, host-name: host-name,
	       host-address: host-address);
      $WSAEHOSTUNREACH, $WSAECONNREFUSED, $WSAEADDRNOTAVAIL =>
	// $WSAETIMEDOUT is possible for this also but that has to
	// be distinguished by context.
	high-level-error := 
	  make(<connection-failed>, details: win32-condition-object,
	       format-string: format-string, 
	       format-arguments: format-arguments, host-port: host-port,
	       host-address: host-address);
      $WSAENOTCONN, $WSAENETRESET, $WSAESHUTDOWN, $WSAECONNABORTED,
      $WSAETIMEDOUT,$WSAECONNRESET =>
	high-level-error := 
	  make(<connection-closed>, details: win32-condition-object,
	       format-string: format-string, 
	       format-arguments: format-arguments, host-port: host-port,
	       host-address: host-address);
      $WSAEADDRINUSE =>
	high-level-error := make(<address-in-use>,
                                 details: win32-condition-object,
                                 format-string: "address in use",
                                 host-address: host-address,
                                 host-port: host-port);
      otherwise =>
	high-level-error := win32-condition-object;
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

define class <winsock-manager> (<socket-manager>, <sealed-object>)
  slot socket-manager-started? :: <boolean> = #f;
  slot socket-manager-closing-thread :: false-or(<thread>) = #f; // thread that closed socket-manager
  constant slot socket-manager-lock :: <recursive-lock> = make(<recursive-lock>);
				// lock to control opening and closing
				// of sockets so that corrupt and race
				// conditions are prevented during
				// socket manager shutdown
end class;

install-socket-manager(make(<winsock-manager>));

// WSAstartup errors:
// 
// WSASYSNOTREADY Indicates that the underlying network subsystem is not
//   ready for network communication.
// WSAVERNOTSUPPORTED The version of Windows Sockets support requested
//   isn't provided by this particular Windows Sockets
//   implementation. 
// WSAEINPROGRESS A blocking Windows Sockets 1.1 operation is in progress.
// WSAEPROCLIM Limit on the number of tasks supported by the Windows
//   Sockets implementation has been reached. 
// WSAEFAULT The lpWSAData is not a valid pointer.

// Require winsock version 2.0 or better
define constant $min-winsock-version = logior(2, ash(0, 8)); // makeword(2,0)
define function accessor-startup (manager :: <socket-manager>) => (result :: <boolean>)
  unless (socket-manager-started?(manager))
    with-stack-structure (wsadata :: <LPWSADATA>)
      let error-code = WSAstartup($min-winsock-version, wsadata);
      let result =
	if (zero?(error-code))
	  register-application-exit-function(stop-sockets);
	  //  MJS 21Aug97: call WSAGetLastError to force the  symbol to be
	  //  resolved since it shares the error code with that set by
	  //  GetProcAddress etc. 
	  WSAGetLastError();
	  // Should really verify that wsadata really does say that 2.0 is
	  // supported.
	  socket-manager-started?(manager) := #t;
	  #t;
	else
	  error(make(<win32-socket-error>, error-code: error-code,
		     calling-function: "WSAstartup",
		     format-string: 
		       "Error  initializing winsock2 DLL for use."
		       "No other socket calls can succeed."));
	  #f
	end if;
      result
    end with-stack-structure;
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
      let error-code = WSAcleanup();
      if (error-code == $SOCKET-ERROR)
	win32-socket-error("WSAcleanup")     
      end if;
      socket-manager-started?(manager) := #f;
      socket-manager-closing-thread(manager) := current-thread();
    elseif (socket-manager-closing-thread(manager))
      error("Internal error: accessor-cleanup called when winsock2 already "
	      "closed by thread:%s\n",
	    socket-manager-closing-thread(manager).thread-name | "unnamed thread");
    else
      error("Internal error: accessor-cleanup called but winsock2 was never"
	      " initialized");    
    end if;
  end with-lock;
end function;
 
// conversions and DNS calls
define method accessor-htonl (host-order-number :: <machine-word>)
 => (network-order-number :: <machine-word>);
  win32-htonl(host-order-number)
end method;

define method accessor-htonl (host-order-number :: <abstract-integer>)
 => (network-order-number :: <machine-word>);
  win32-htonl(as(<machine-word>, host-order-number))
end method;

define constant accessor-ntohl = win32-ntohl;
define constant accessor-htons = win32-htons;
define constant accessor-ntohs = win32-ntohs;
 
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

// win32-gethostbyname errors:
// 
// WSANOTINITIALISED A successful WSAStartup must occur before using
//   this function. 
// WSAENETDOWN The network subsystem has failed.
// WSAHOST_NOT_FOUND Authoritative Answer Host not found.
// WSATRY_AGAIN Non-Authoritative Host not found, or server failure.
// WSANO_RECOVERY Nonrecoverable error occurred.
// WSANO_DATA Valid name, no data record of requested type.
// WSAEINPROGRESS A blocking Windows Sockets 1.1 call is in progress,
//   or the service provider is still processing a callback function. 
// WSAEFAULT The name parameter is not a valid part of the user
//   address space. 
// WSAEINTR A blocking Windows Socket 1.1 call was canceled through
//   WSACancelBlockingCall. 

define method get-host-entry 
    (the-name :: <C-string>) => (host-entry :: <LPHOSTENT>)
  let host-entry :: <LPHOSTENT> = win32-gethostbyname(the-name);
  if (null-pointer?(host-entry))
    let error-code :: <integer> = WSAGetLastError();
    select (error-code by \==)
      $WSAHOST-NOT-FOUND => 
	// Maybe it's a presentation address.  Try to covert the
	// presentation address to a number.
	let address-as-number :: <machine-word> =
	  if (the-name = "255.255.255.255")
	    accessor-htonl(#xFFFFFFFF) // probably don't need htonl
	  else 
	    let result = inet-addr(the-name);
	    if (result == $INADDR-NONE) 
	       win32-socket-error("get-host-entry",
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
	  win32-gethostbyaddr(pointer-cast(<C-char*>, hostnum-pointer),
			      size-of(<C-raw-unsigned-long>),
			      $AF-INET);
	  if (null-pointer?(result))
	    win32-socket-error("get-host-entry", 
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
      otherwise =>
	win32-socket-error("get-host-entry", error-code: error-code,
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
end method;

// win32-gethostbyaddr error codes:
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
  //  ISSUE: this isn't thread safe.  Need to lock so that other
  //  threads don't smash the single hostent strut before we can copy
  //  the fields out of it.
  let host-entry :: <LPHOSTENT>
    // Could maybe use a with-initialized-pointer macro here
    = with-stack-structure(hostnum-pointer :: <C-raw-unsigned-long*>)
        pointer-value(hostnum-pointer) := 
          new-address.numeric-host-address.network-order;
	let gethostbyaddr-result :: <LPHOSTENT> =
	  win32-gethostbyaddr(pointer-cast(<C-char*>, hostnum-pointer),
			      size-of(<C-raw-unsigned-long>),
			      $AF-INET);
	if (null-pointer?(gethostbyaddr-result))
	  let error-code :: <integer> = WSAGetLastError();
	  win32-socket-error("win32-gethostbyaddr", 
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
end function;


define method accessor-get-port-for-service
    (service :: <c-string>, proto :: <c-string>) => (result :: <integer>)
  let sp :: <LPSERVENT> = 
  win32-getservbyname(service, proto);
  if (null-pointer?(sp))
    let service-error-code = WSAGetLastError();
    let high-level-error =
      select (service-error-code by \==)
	$WSAHOST-NOT-FOUND, $WSANO-RECOVERY, $WSANO-DATA =>
	  make(<service-not-found>,
	       format-string: "Service: %s not found for protocol: %s",
	       format-arguments: vector(service, proto),
	       service: as(<byte-string>, service),
	       protocol: as(<byte-string>, proto));
	otherwise => #f;
      end select;
    win32-socket-error("win32-getservbyname", error-code: service-error-code,
		       high-level-error: high-level-error);
  else
    accessor-ntohs(sp.s-port-value);
  end if
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

define function accessor-remote-address-and-port (the-descriptor :: <machine-word>)
  => (connected? :: <boolean>,
      the-remote-address :: false-or(<ipv4-address>), 
      the-remote-port :: false-or(<integer>));
  let connected? = #t; // we will find out for sure
  let the-remote-address :: false-or(<ipv4-address>) = #f;
  let the-remote-port :: false-or(<integer>) = #f;
  with-stack-structure (inaddr :: <LPSOCKADDR-IN>)
    // 0 out all the fields
    inaddr.sin-family-value := 0;
    inaddr.sin-addr-value := as(<machine-word>, 0);
    inaddr.sin-port-value := 0;
    let addr = pointer-cast(<LPSOCKADDR>, inaddr);
    with-stack-structure (size-pointer :: <C-int*>)
      pointer-value(size-pointer) := size-of(<SOCKADDR-IN>);
      let getpeername-result = 
        win32-getpeername(the-descriptor, addr, size-pointer);
      if (getpeername-result == $SOCKET-ERROR)
	let error-code :: <integer> = WSAGetLastError();
	select (error-code by \==)
	  $WSAENOTCONN => connected? := #f;
	  otherwise =>
	    win32-socket-error("win32-getpeername", 
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
  end with-stack-structure;
  values(connected?, the-remote-address, the-remote-port)
end function;

define function accessor-local-address-and-port (the-descriptor :: <machine-word>)
  => (the-local-address :: <ipv4-address>, the-local-port :: <integer>)
  with-stack-structure (inaddr :: <LPSOCKADDR-IN>)
    // 0 out all the fields
    inaddr.sin-family-value := 0;
    inaddr.sin-addr-value := as(<machine-word>, 0);
    inaddr.sin-port-value := 0;
    let addr = pointer-cast(<LPSOCKADDR>, inaddr);
    with-stack-structure (size-pointer :: <C-int*>)
      pointer-value(size-pointer) := size-of(<SOCKADDR-IN>);
      let getsockname-result = 
        win32-getsockname(the-descriptor, addr, size-pointer);
      if (getsockname-result == $SOCKET-ERROR)
	win32-socket-error("win32-getsockname");
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
  end with-stack-structure
end function;


define function accessor-local-host-name()
  => (local-host-name :: <string>);
  let local-host-name = "";
  let name-buffer-size = 256; // wag for the size
  let name-buffer :: <byte-vector> =
    make(<byte-vector>, size: name-buffer-size, fill: as(<integer>, '\0')); 
  with-C-string(name-buffer-as-C-string = name-buffer)
    let gethostname-result = 
      win32-gethostname(name-buffer-as-C-string, name-buffer-size);
    if (gethostname-result == $SOCKET-ERROR)
      win32-socket-error("win32-gethostname");
    end if;      
    local-host-name := as(<byte-string>, name-buffer-as-C-string);
  end with-c-string;
  local-host-name
end function;

define constant <accessor-socket-descriptor> = <machine-word>;

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
      win32-bind(the-socket.socket-descriptor, addr, size-of(<SOCKADDR-IN>));
    if (bind-result = $SOCKET-ERROR)
      close-socket(the-socket);
      win32-socket-error("win32-bind");
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
  let listen-result = win32-listen(the-socket.socket-descriptor, backlog);
  if (listen-result = $SOCKET-ERROR)
    close-socket(the-socket);
    win32-socket-error("win32-listen");
  end if;
end method;


define method accessor-shutdown
    (the-descriptor :: <accessor-socket-descriptor>) => ();
  let shutdown-result = shutdown(the-descriptor, $SD-SEND);
  if (shutdown-result = $SOCKET-ERROR)
    win32-socket-error("win32-shutdown");
  end if;
end method;    

define method accessor-input-available?
    (the-descriptor :: <accessor-socket-descriptor>) => (input? :: <boolean>);
/* unimplemented for Win32
  with-stack-structure(pollfds :: <pollfd*>)
    pollfds.pollfd-fd := the-descriptor;
    pollfds.pollfd-events := $POLLIN;
    pollfds.pollfd-revents := 0;
    poll(pollfds, 1, 0)
  end > 0 */
  #t
end method accessor-input-available?;

define method accessor-close-socket
    (the-descriptor :: <accessor-socket-descriptor>) => ();
  let manager = current-socket-manager();
  if (socket-manager-started?(manager))
    let close-result = win32-closesocket(the-descriptor);
    if (close-result == $SOCKET-ERROR)
      let error-code :: <integer> = WSAGetLastError();
      select (error-code by \==)
	$WSANOTINITIALISED =>
	  error(make(<socket-accessor-closed-error>,
		     calling-function: "accessor-close-socket",
		     calling-thread: current-thread(),
		     accessor-started?-value: socket-manager-started?(manager),
		     thread-that-closed-accessor: socket-manager-closing-thread(manager),
		     format-string:
		       "Internal Error: "
		       "WSANOTINITIALISED was returned from win32-closesocket"
		       " which was called from accessor-close-socket."));
	otherwise =>
	  signal(make(<win32-socket-error>, 
		      calling-function: "win32-closesocket",
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
