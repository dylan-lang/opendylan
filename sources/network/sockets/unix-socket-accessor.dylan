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
  slot explanation :: <string>, init-value: "";
  slot calling-function :: <string>, init-keyword: calling-function:;
end class;

define function unix-socket-error
    (calling-function :: <string>,
     #key format-string = "socket error", format-arguments = #[],
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
         format-arguments: format-arguments);
  if (high-level-error)
    high-level-error.socket-condition-details :=
      unix-condition-object;
  else
    // TODO: These are copied as far as possible from Winsock
    select (error-code by ==)
      $EINPROGRESS, $EFAULT,
      $EAFNOSUPPORT, $ENOTSOCK,
      $EINVAL, $EISCONN,
      $EOPNOTSUPP, $EWOULDBLOCK, $EACCES,
      $EMSGSIZE =>
        high-level-error :=
          make(<internal-socket-error>, details: unix-condition-object,
               format-string: format-string,
               format-arguments: format-arguments);
      $EINTR =>
        high-level-error :=
          make(<blocking-call-interrupted>, details: unix-condition-object,
               format-string: format-string,
               format-arguments: format-arguments);
      $ENOBUFS, $EMFILE =>
        high-level-error :=
          make(<out-of-resources>, details: unix-condition-object,
               format-string: format-string,
               format-arguments: format-arguments);
      $ENETDOWN =>
        high-level-error :=
          make(<network-not-responding>, details: unix-condition-object,
               format-string: format-string,
               format-arguments: format-arguments);
/*
      $HOST-NOT-FOUND, $NO-RECOVERY, $NO-DATA =>
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
*/
      $EHOSTUNREACH, $ECONNREFUSED, $EADDRNOTAVAIL =>
        high-level-error :=
          make(<connection-failed>, details: unix-condition-object,
               format-string: format-string,
               format-arguments: format-arguments, host-port: host-port,
               host-address: host-address);
      $ENOTCONN, $ENETRESET, $ESHUTDOWN, $ECONNABORTED,
      $ETIMEDOUT,$ECONNRESET =>
        high-level-error :=
          make(<connection-closed>, details: unix-condition-object,
               format-string: format-string,
               format-arguments: format-arguments, host-port: host-port,
               host-address: host-address);
      $EADDRINUSE =>
        high-level-error := make(<address-in-use>,
                                 details: unix-condition-object,
                                 format-string: "address in use",
                                 host-address: host-address,
                                 host-port: host-port);
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

/* There is another set of errors returned by netdb functions
 * gethostbyname(), gethostbyaddr() et al */
define constant $HOST-NOT-FOUND    = 1; /* Authoritative Answer Host not found */
define constant $TRY-AGAIN         = 2; /* Non-Authoritative Host not found, or SERVERFAIL */
define constant $NO-RECOVERY	   = 3; /* Non recoverable errors, FORMERR, REFUSED, NOTIMP */
define constant $NO-DATA           = 4; /* Valid name, no data record of requested type */

define function unix-socket-herror(calling-function :: <string>,
                                   #key format-string = "netdb error",
                                        format-arguments = #[],
                                   host-name = #f,
                                   host-address = #f,
                                   host-port = #f)
  let error-code = h-errno();
  let high-level-error =
    select (error-code by ==)
      $HOST-NOT-FOUND, $NO-RECOVERY, $NO-DATA =>
        make(<host-not-found>,
             format-string: format-string,
             format-arguments: format-arguments,
             host-name: host-name, host-address: host-address);
      $TRY-AGAIN =>
        make(<server-not-responding>,
             format-string: format-string,
             format-arguments: format-arguments, host-name: host-name,
             host-address: host-address);
      otherwise => #f
    end select;
  unix-socket-error(calling-function, format-string: format-string,
                    format-arguments: format-arguments,
                    high-level-error: high-level-error,
                    host-name: host-name,
                    host-port: host-port,
                    error-code: error-code);
end function unix-socket-herror;

// startup and shutdown

define class <stub-manager> (<socket-manager>, <sealed-object>)
  slot socket-manager-started? :: <boolean> = #f;
  slot socket-manager-closing-thread :: false-or(<thread>) = #f; // thread that closed socket-manager
end class;

install-socket-manager(make(<stub-manager>));

define function accessor-startup (manager :: <socket-manager>) => (result :: <boolean>)
  unless (socket-manager-started?(manager))
    socket-manager-started?(manager) := #t;
    register-application-exit-function(stop-sockets);
  end unless;
  socket-manager-started?(manager)
end function;

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
  (ip-address-string :: <byte-string>) =>
  (result :: <ipv4-network-order-address>)
  with-stack-structure(inp :: <in-addr*>)
    if (zero?(inet-aton(ip-address-string, inp)))
      error(make(<invalid-address>,
                 format-string: "badly formed ip address string %s",
                 format-arguments: vector(ip-address-string),
                 bad-address: ip-address-string))
    else
      let addr = as(<machine-word>, pointer-value(inp));
      make(<ipv4-network-order-address>, address: addr)
    end if;
  end;
end method;

define constant $resolver-lock = make(<recursive-lock>);

define method get-host-entry
    (the-name :: <C-string>) => (host-entry :: <LPHOSTENT>)
  let host-entry :: <LPHOSTENT> = unix-gethostbyname(the-name);
  if (null-pointer?(host-entry))
    unix-socket-herror("get-host-entry",
                      format-string:
                        "Error translating %s as a host name",
                      format-arguments:
                        vector(as(<byte-string>, the-name)),
                      host-name: the-name);
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
    // copied out of the hostent struct
    new-address.%host-name := as(<byte-string>,
                                 pointer-cast(<C-string>,
                                              host-entry.h-name-value));
    new-address.%aliases := copy-aliases(host-entry.h-aliases-value);
    new-address.%addresses := copy-addresses(host-entry);
  end
end method;

define function accessor-get-host-by-address
    (new-address :: <ipv4-address>) => ();
  with-lock($resolver-lock)
    let host-entry :: <LPHOSTENT>
      // Could maybe use a with-initialized-pointer macro here
      = with-stack-structure(hostnum-pointer :: <C-raw-unsigned-int*>)
          pointer-value(hostnum-pointer) :=
            new-address.numeric-host-address.network-order;
          let gethostbyaddr-result :: <LPHOSTENT> =
            unix-gethostbyaddr(pointer-cast(<C-char*>, hostnum-pointer),
                               size-of(<C-raw-unsigned-int>),
                               $AF-INET);
          if (null-pointer?(gethostbyaddr-result))
            unix-socket-herror("unix-gethostbyaddr",
                               format-string:
                                 "Couldn't translate %s as a host address",
                               format-arguments:
                                 vector(new-address.host-address),
                               host-address: new-address);
          end if;
          gethostbyaddr-result
        end with-stack-structure;
    // now fill in the fields of the <ipv4-address>. Everything must be
    // copied out of the hostent structure
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
      /* Linux/BSD docs don't mention a specific errno */
      let service = as(<byte-string>, service);
      let proto = as(<byte-string>, proto);
      let service-error-code = unix-errno();
      let high-level-error =
        make(<service-not-found>,
             format-string: "Service: %s not found for protocol: %s",
             format-arguments: vector(service, proto),
             service: service,
             protocol: proto);
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
// [EBADF]      The argument socket is not a valid descriptor.
// [EFAULT]     The address parameter points to memory not in a valid
//              part of the process address space.
// [EINVAL]     socket has been shut down.
// [ENOBUFS]    Insufficient resources were available in the system to
//              perform the operation.
// [ENOTCONN]   Either the socket is not connected or it has not had
//              the peer pre-specified.
// [ENOTSOCK]   The argument socket refers to something other than a
//              socket (e.g., a file).
// [EOPNOTSUPP] getpeername() is not supported for the protocol in use
//              by socket.
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
          /* TODO: High level error conditions */
          $ENOTCONN => connected? := #f;
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

// On Linux this is 64. Allow extra space here.
define constant $HOST-NAME-SIZE :: <integer> = 256;

define function accessor-local-host-name()
  => (local-host-name :: <string>);
  let local-host-name = "";
  let name-buffer :: <byte-vector> =
    make(<byte-vector>, size: $HOST-NAME-SIZE, fill: as(<integer>, '\0'));
  with-C-string(name-buffer-as-C-string = name-buffer)
    let gethostname-result =
      unix-gethostname(name-buffer-as-C-string, $HOST-NAME-SIZE);
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
// errno.
//
// Error Codes
// [EACCES]        The requested address is protected, and the current
//                 user has inadequate permission to access it.
// [EADDRINUSE]    The specified address is already in use.
// [EADDRNOTAVAIL] The specified address is not available from the local
//                  machine.
// [EAFNOSUPPORT]  address is not valid for the address family of socket.
// [EBADF]         socket is not a valid file descriptor.
// [EDESTADDRREQ]  socket is a null pointer.
// [EFAULT]        The address parameter is not in a valid part of the
//                 user address space.
// [EINVAL]        socket is already bound to an address and the protocol
//                 does not support binding to a new address.  Alterna-
//                 tively, socket may have been shut down.
// [ENOTSOCK]      socket does not refer to a socket.

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
// retrieved by calling errno.
//
// Error Codes
// [EACCES]       The current process has insufficient privileges.
// [EBADF]        The argument socket is not a valid file descriptor.
// [EDESTADDRREQ] The socket is not bound to a local address and the
//                protocol does not support listening on an unbound
//                socket.
// [EINVAL]       socket is already connected.
// [ENOTSOCK]     The argument socket does not reference a socket.
// [EOPNOTSUPP]   The socket is not of a type that supports the opera-
//                tion listen
//
define method accessor-listen
    (the-socket :: <TCP-server-socket>, backlog :: <integer>)
 => ()
  // The backlog governs how many accept requests will be queued
  // before requests are turned down.
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
      unix-socket-error("close-socket",
                        error-code: error-code);
    end if;
  else
    error(make(<socket-accessor-closed-error>,
               calling-function: "accessor-close-socket",
               calling-thread: current-thread(),
               accessor-started?-value: socket-manager-started?(manager),
               thread-that-closed-accessor: socket-manager-closing-thread(manager),
               format-string: "Internal Error in socket-manager ",
               format-arguments: #[]));
  end if;
end method;
