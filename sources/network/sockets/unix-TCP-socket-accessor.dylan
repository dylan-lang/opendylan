Module:       sockets-internals
Author:       Toby
Synopsis:     TCP specific winsock2 accessor layer
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// General TCP stuff and TCP server socket stuff

define function accessor-new-socket-descriptor
    (code)
 => (the-descriptor :: <accessor-socket-descriptor>)
  let the-descriptor = unix-socket($AF-INET, code, 0);
  if (the-descriptor = $INVALID-SOCKET)
    unix-socket-error("unix-socket");
  end if;
  with-stack-structure (int :: <C-int*>)
    pointer-value(int) := 1;
    let setsockopt-result =
      setsockopt(the-descriptor, $SOL-SOCKET, $SO-REUSEADDR,
                 int, size-of(<C-int*>));
    if (setsockopt-result < 0)
      unix-socket-error("setsockopt");
    end;
  end;
  the-descriptor
end function;

define inline method socket-code
    (socket :: <unix-TCP-accessor>)
  $SOCK-STREAM
end method;

define method accessor-accept
    (server-socket :: <platform-server-socket>)
 => (connected-socket-descriptor :: <accessor-socket-descriptor>)
  with-cleared-stack-structure (inaddr :: <LPSOCKADDR-IN>)
    let addr = pointer-cast(<LPSOCKADDR>, inaddr);
    with-stack-structure (size-pointer :: <C-int*>)
      pointer-value(size-pointer) := size-of(<SOCKADDR-IN>);
      let connected-socket-descriptor =
        interruptible-system-call(unix-accept
                                    (server-socket.socket-descriptor,
                                     addr, size-pointer));
      if (connected-socket-descriptor = $INVALID-SOCKET)
        unix-socket-error("unix-accept");
      end if;
      connected-socket-descriptor
    end with-stack-structure
  end with-cleared-stack-structure
end method;

// TCP stream accessor protocol

define class <unix-tcp-accessor> (<unix-socket-accessor>)
end class <unix-tcp-accessor>;

define sideways method platform-accessor-class
    (type == #"TCP", locator)
 => (class :: singleton(<unix-tcp-accessor>))
  ignore(locator);
  <unix-tcp-accessor>
end method platform-accessor-class;

define method accessor-close
    (accessor :: <unix-socket-accessor>,
     #key abort? = #f, wait? = #t)
 => (closed? :: <boolean>);
  let the-descriptor :: <accessor-socket-descriptor>
    = accessor.socket-descriptor;
  if (the-descriptor & (abort? | accessor.connection-closed?))
    accessor-close-socket(the-descriptor);
    accessor.socket-descriptor := #f;
  else
    //  worry about various kinds of graceful exits/linger options
    //  Windows documentation since:
    // For example, to initiate a graceful disconnect:
    // 1.Call WSAAsyncSelect to register for FD_CLOSE notification.
    // 2.Call shutdown with how=SD_SEND.
    // 3.When FD_CLOSE received, call recv until zero returned, or
    //   SOCKET_ERROR.
    // 4.Call closesocket.
    //  for now graceless close
    accessor-close-socket(the-descriptor);
    accessor.socket-descriptor := #f;
  end if;
  #t
end method accessor-close;

define method accessor-open?
    (accessor :: <unix-socket-accessor>) => (open? :: <boolean>)
  ((~ accessor.connection-closed?) & (accessor.socket-descriptor ~= #f))
end method accessor-open?;

define variable *linger* :: <integer> = 1; //seconds

define method accessor-open
    (accessor :: <unix-socket-accessor>, locator,
     #key remote-host: input-remote-host :: false-or(<ipv4-address>),
     remote-port: input-remote-port :: false-or(<integer>),
     descriptor:
       input-descriptor :: false-or(<accessor-socket-descriptor>) = #f,
     no-delay? :: <boolean>,
     // These next keys are meaningless for sockets but required keys
     // for the generic defined in external-stream.dylan (sigh)
     direction, if-exists, if-does-not-exist,
     #all-keys) => ();
  ignore(direction, if-exists, if-does-not-exist);
  if (input-descriptor)
    //  This is probably a connected socket returned by accept.
    accessor.socket-descriptor := input-descriptor;
    let (the-local-address :: false-or(<ipv4-address>),
         the-local-port :: false-or(<integer>)) =
      accessor-local-address-and-port(accessor.socket-descriptor);
    accessor.local-host := the-local-address;
    accessor.local-port := the-local-port;
    let (is-connected? :: <boolean>,
         the-remote-host :: false-or(<ipv4-address>),
         the-remote-port :: false-or(<integer>)) =
      accessor-remote-address-and-port(accessor.socket-descriptor);
    if (is-connected?)
      accessor.connected? := is-connected?;
      accessor.remote-host := the-remote-host;
      accessor.remote-port := the-remote-port;
    end if;
  else
      // This is a client socket. Connect it.
    with-stack-structure (inaddr :: <LPSOCKADDR-IN>)
      inaddr.sin-family-value := input-remote-host.address-family;
      inaddr.sin-addr-value :=
        input-remote-host.numeric-host-address.network-order;
      inaddr.sin-port-value := accessor-htons(input-remote-port);
      accessor.socket-descriptor :=
        accessor-new-socket-descriptor(socket-code(accessor));
      block ()
        if (*linger* & instance?(accessor, <unix-tcp-accessor>))
          with-stack-structure (linger :: <LPLINGER>)
            linger.l-onoff-value := 1;
            linger.l-linger-value := *linger*;
            let result = setsockopt(accessor.socket-descriptor,
                                    $SOL-SOCKET,
                                    $SO-LINGER,
                                    pointer-cast(<c-char*>, linger),
                                    size-of(<LINGER>));
            if (result = $SOCKET-ERROR)
              unix-socket-error("setsockopt");
            end if;
          end with-stack-structure;
        end if;

// I don't know what this setsockopt call is for.  The code from
// LispWorks sockets looks really confused.  It tests the
// variable *sockopt_tcp_nodelay* which is initialized to 1 =
// $TCP-NODELAY.  Then if true (which appears to be always it calls
// setsockopt with the parameter, mi pointing to some apparently
// garbage location in memory.  This pointer is supposed to point to a
// boolean value (C style, 0 is false any other integer value is true)
// whether to switch the option on or not.  Sounds random whether the
// option gets set or not. Furthermore the option supposedly "disables
// Nagle algorithm for send coalescing" whatever the hell that means.

// enable this code, make no-delay? an init keyword and set the value
// of mi to 1; Needed TCP connection without Nagle.
//     -- hannes, 13. July 2008
         if (no-delay?)
           with-stack-structure(mi :: <C-int*>)
            pointer-value(mi) := 1;
            let setsockopt-result =
              setsockopt(accessor.socket-descriptor,
                         $IPPROTO-TCP,
                         $TCP-NODELAY,
                         pointer-cast(<c-char*>, mi),
                         size-of(<C-int>));
            if (setsockopt-result < 0)
              unix-socket-error("setsockopt");
            end;
           end with-stack-structure;
        end if;

        let addr = pointer-cast(<LPSOCKADDR>, inaddr);
        let connect-result =
          unix-connect(accessor.socket-descriptor,
                       addr, size-of(<SOCKADDR-IN>));
        if (connect-result == $SOCKET-ERROR)
          let connect-error-code = unix-errno();
          let high-level-error =
            // TODO: High level error conditions
            // if (connect-error-code == $WSAETIMEDOUT)
            if (#f)
              make(<connection-failed>,
                   host-address: input-remote-host,
                   host-port: input-remote-port);
            else #f
            end if;
          unix-socket-error("unix-connect", error-code: connect-error-code,
                            high-level-error: high-level-error,
                            host-address: input-remote-host,
                            host-port: input-remote-port);
        end if;
        let (the-local-address :: false-or(<ipv4-address>),
             the-local-port :: false-or(<integer>)) =
          accessor-local-address-and-port(accessor.socket-descriptor);
        accessor.local-host := the-local-address;
        accessor.local-port := the-local-port;
        accessor.connected? := #t;
        accessor.remote-host := input-remote-host;
        accessor.remote-port := input-remote-port;
      cleanup
        if ((~ accessor.connected?) & accessor.socket-descriptor)
          let close-result =  unix-closesocket(accessor.socket-descriptor);
          if (close-result == $SOCKET-ERROR)
            unix-socket-error("unix-closesocket");
          end if;
          accessor.socket-descriptor := #f
        end if;
      end block;
    end with-stack-structure;
  end if;
  accessor.connected?
end method accessor-open;

define constant $preferred-buffer-size = 1024;

define method accessor-preferred-buffer-size
    (accessor :: <unix-socket-accessor>)
 => (preferred-buffer-size :: <integer>)
  $preferred-buffer-size
end method accessor-preferred-buffer-size;

define method accessor-read-into!
    (accessor :: <unix-socket-accessor>, stream :: <platform-socket>,
     offset :: <buffer-index>, count :: <buffer-index>, #key buffer)
 => (nread :: <integer>)
  let the-buffer :: <buffer> = buffer | stream-input-buffer(stream);
  let the-descriptor = accessor.socket-descriptor;
  if (accessor.connection-closed? | (~ the-descriptor))
    error(make(<socket-closed>, socket: stream))
  else
    let nread =
      interruptible-system-call
        (unix-recv(the-descriptor, the-buffer, offset, count));
    if (nread == $SOCKET-ERROR)
      unix-socket-error("unix-recv", host-address: stream.remote-host,
                        host-port: stream.remote-port);
    elseif (nread == 0) // Check for EOF (nread == 0)
      accessor.connection-closed? := #t;
    end if;
    nread
  end
end method;

// Function for adding the base address of a <buffer>'s repeated slots
// to an offset and returning the result as a <machine-word>.  This is
// necessary for passing <buffer> contents across the FFI.

define function buffer-offset
    (the-buffer :: <buffer>, data-offset :: <integer>)
 => (result-offset :: <machine-word>)
  u%+(data-offset,
      primitive-wrap-machine-word
        (primitive-repeated-slot-as-raw
           (the-buffer, primitive-repeated-slot-offset(the-buffer))))
end function;

// There is an interesting non-blocking version of recv in  the
// LispWorks sockets stuff called stream--stream-read-buffer.

// Here is the return value information for unix-recv from msdn
//
// Return Values
//
// If no error occurs, recv returns the number of bytes received. If
// the connection has been gracefully closed, the return value is
// zero. Otherwise, a value of SOCKET_ERROR is returned, and a
// specific error code can be retrieved by calling WSAGetLastError.
//
// Error Codes
//
// WSANOTINITIALISED A successful WSAStartup must occur before using
//   this function.WSAENETDOWNThe network subsystem has failed.
// WSAEFAULT The buf parameter is not completely contained in a valid
//   part of the user address space.
// WSAENOTCONN The socket is not connected.
// WSAEINTR The (blocking) call was canceled through WSACancelBlockingCall.
// WSAEINPROGRESS A blocking Windows Sockets 1.1 call is in progress,
//   or the service provider is still processing a callback function.
// WSAENETRESET The connection has been broken due to the keep-alive
//   activity detecting a failure while the operation was in progress.
// WSAENOTSOCK The descriptor is not a socket.
// WSAEOPNOTSUPP MSG_OOB
//   was specified, but the socket is not stream-style such as type
//   SOCK_STREAM, out-of-band data is not supported in the
//   communication domain associated with this socket, or the socket
//   is unidirectional and supports only send operations.
// WSAESHUTDOWN The socket has been shut down; it is not possible to
//   recv on a socket after shutdown has been invoked with how set to
//   SD_RECEIVE or SD_BOTH.
// WSAEWOULDBLOCK The socket is marked as nonblocking and the receive
//   operation would block.WSAEMSGSIZEThe message was too large to fit
//   into the specified buffer and was truncated.
// WSAEINVAL The socket has not been bound with bind, or an unknown
//   flag was specified, or MSG_OOB was specified for a socket with
//   SO_OOBINLINE enabled or (for byte stream sockets only) len was
//   zero or negative.
// WSAECONNABORTED The virtual circuit was terminated due to a
//   time-out or other failure. The application should close the
//   socket as it is no longer usable.
// WSAETIMEDOUT The connection has been dropped because of a network
//   failure or because the peer system failed to respond.
// WSAECONNRESET The virtual circuit was reset by the remote side
//   executing a "hard" or "abortive" close. The application should
//   close the socket as it is no longer usable. On a UDP datagram
//   socket this error would indicate that a previous send operation
//   resulted in an ICMP "Port Unreachable" message.

define function unix-recv
    (descriptor :: <accessor-socket-descriptor>, the-buffer :: <buffer>,
     offset :: <integer>, count :: <integer> )
 => (nread :: <integer>)
  //  recv returns:
  //   >0 when that many bytes were read
  //    0 when the peer is closed
  //   -1 ($SOCKET-ERROR) for error or no bytes available
  unix-recv-buffer(descriptor, buffer-offset(the-buffer, offset),
                   count, 0)
end function;

define method accessor-write-from
    (accessor :: <unix-socket-accessor>, stream :: <platform-socket>,
     offset :: <buffer-index>, count :: <buffer-index>, #key buffer,
     return-fresh-buffer?)
 => (nwritten :: <integer>, new-buffer :: <buffer>)
  ignore(return-fresh-buffer?);
  let buffer :: <buffer> = buffer | stream-output-buffer(stream);
  let the-descriptor = accessor.socket-descriptor;
  if (accessor.connection-closed? | (~ the-descriptor))
    error("Stream closed") // ---*** FIX THIS
  else
    let remaining = count;
    while (remaining > 0)
      let nwritten =
        interruptible-system-call
          (unix-send(accessor.socket-descriptor, buffer,
                     offset + count - remaining, remaining));
      if (nwritten == $SOCKET-ERROR)
        unix-socket-error("unix-send", host-address: stream.remote-host,
                          host-port: stream.remote-port)
      end if;
      remaining := remaining - nwritten
    end while;
  end if;
  values(count, buffer)
end method accessor-write-from;

// There is an interesting non-blocking version of send in  the
// LispWorks sockets stuff called stream--stream-write-buffer.
// Unfortunately the version there doesn't allow for the possibility
// that fewer than the requested number of bytes were written.

// Here is the return information from msdn:
// If no error occurs, send returns the total number of bytes sent,
// which can be less than the number indicated by len for nonblocking
// sockets. Otherwise, a value of SOCKET_ERROR is returned, and a
// specific error code can be retrieved by calling WSAGetLastError.
//
// Error Codes
//
// WSANOTINITIALISED A successful WSAStartup must occur before using
//   this function.
// WSAENETDOWN The network subsystem has failed.
// WSAEACCES The requested address is a broadcast address, but the
//   appropriate flag was not set. Call setsockopt with the SO_BROADCAST
//   parameter to allow the use of the broadcast address.
// WSAEINTR A blocking Windows Sockets 1.1 call was canceled through
//   WSACancelBlockingCall.
// WSAEINPROGRESS A blocking Windows Sockets 1.1 call is in progress,
//   or the service provider is still processing a callback function.
// WSAEFAULT The buf parameter is not completely contained in a valid
//   part of the user address space.
// WSAENETRESET The connection has been broken due to the "keep-alive"
//   activity detecting a failure while the operation was in progress.
// WSAENOBUFS No buffer space is available.
// WSAENOTCONN The socket is not connected.
// WSAENOTSOCK The descriptor is not a socket.
// WSAEOPNOTSUPPMSG_OOB was specified, but the socket is not
//   stream-style such as type SOCK_STREAM, out-of-band data is not
//   supported in the communication domain associated with this
//   socket, or the socket is unidirectional and supports only receive
//   operations.
// WSAESHUTDOWN The socket has been shut down; it is not possible to
//   send on a socket after shutdown has been invoked with how set to
//   SD_SEND or SD_BOTH.
// WSAEWOULDBLOCK The socket is marked as nonblocking and the
//   requested operation would block.
// WSAEMSGSIZE The socket is message oriented, and the message is
//   larger than the maximum supported by the underlying transport.
// WSAEHOSTUNREACH The remote host cannot be reached from this host at
//   this time.
// WSAEINVAL The socket has not been bound with bind, or an unknown
//   flag was specified, or MSG_OOB was specified for a socket with
//   SO_OOBINLINE enabled.
// WSAECONNABORTED The virtual circuit was terminated due to a
//   time-out or other failure. The application should close the
//   socket as it is no longer usable.
// WSAECONNRESET The virtual circuit was reset by the remote side
//   executing a "hard" or "abortive" close. For UPD sockets, the
//   remote host was unable to deliver a previously sent UDP datagram
//   and responded with a "Port Unreachable" ICMP packet. The
//   application should close the socket as it is no longer usable.
// WSAETIMEDOUT The connection has been dropped, because of a network
//   failure or because the system on the other end went down without
//   notice.

define function unix-send
    (descriptor :: <accessor-socket-descriptor>, the-buffer :: <buffer>,
     offset :: <integer>, count :: <integer> )
 => (nwritten :: <integer>)
  unix-send-buffer(descriptor, buffer-offset(the-buffer, offset), count, 0)
end function;

define method accessor-newline-sequence
    (accessor :: <unix-socket-accessor>)
 => (string :: <string>)
  "\n"
end method accessor-newline-sequence;
