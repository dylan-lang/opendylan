Module:       sockets-internals
Author:       Jason Trenouth
Synopsis:     UDP sockets
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define class <win32-UDP-accessor> (<win32-socket-accessor>) 
  slot reply-socket? :: <boolean> = #f, init-keyword: reply?:;
end class;

define sideways method platform-accessor-class (type == #"UDP", locator)
 => (class == <win32-UDP-accessor>)
  <win32-UDP-accessor>
end method;

define inline method socket-code
    (accessor :: <win32-UDP-accessor>)
  $SOCK-DGRAM
end method;

define inline method socket-code
    (socket :: <UDP-server-socket>)
  $SOCK-DGRAM
end method;

define method accessor-listen 
    (the-socket :: <UDP-server-socket>, #key backlog :: <integer> = 5)
 => ()
 // NB do nothing for UDP sockets
end method;

/// Serverside reply-socket is just wrapper around server-socket descriptor
/// No need to block using win32-accept. (In fact win32-accept is meaningless
/// on UDP sockets).
define method accessor-accept
    (server-socket :: <UDP-server-socket>)
 => (connected-socket-descriptor :: <accessor-socket-descriptor>)
  socket-descriptor(server-socket)
end method;

/// Don't actually close reply-sockets on serverside since that would close
/// the server socket itself (they share the same descriptor).
define method accessor-close
    (accessor :: <win32-udp-accessor>,
     #key abort? = #f, wait? = #t)
 => (closed? :: <boolean>);
  if (reply-socket?(accessor))
    // do nothing
  else
    next-method()
  end if;
end method;

/// Same method as Win32/TCP method but uses RecvFrom instead of Recv.
/// This method records the remote address associated with the message.
define method accessor-read-into!
    (accessor :: <win32-udp-accessor>, stream :: <udp-socket>,
     offset :: <buffer-index>, count :: <buffer-index>, #key buffer)
 => (nread :: <integer>)
  if (reply-socket?(accessor))
    let the-buffer :: <buffer> = buffer | stream-input-buffer(stream);
    let the-descriptor = accessor.socket-descriptor;
    if (accessor.connection-closed? | (~ the-descriptor))
      error(make(<socket-closed>, socket: stream)) 
    else
      with-stack-structure (inaddr :: <LPSOCKADDR-IN>)
      // 0 out all the fields
        inaddr.sin-family-value := 0;
        inaddr.sin-addr-value := as(<machine-word>, 0);
        inaddr.sin-port-value := 0;
        let addr = pointer-cast(<LPSOCKADDR>, inaddr);
        with-stack-structure (size-pointer :: <C-int*>)
          pointer-value(size-pointer) := size-of(<SOCKADDR-IN>);
          let nread = win32-recv-buffer-from(the-descriptor,
                                             buffer-offset(the-buffer, offset),
                                             count,
                                             0,
                                             addr,
                                             size-pointer);
          if (nread == $SOCKET-ERROR) 
            win32-socket-error("win32-recv", host-address: stream.remote-host,
                               host-port: stream.remote-port);
          elseif ( nread == 0) // Check for EOF (nread == 0)
            accessor.connection-closed? := #t;
          end if;
          // NB store addr info into accessor object for user
          accessor.remote-host := make(<ipv4-address>, 
                                       address: make(<ipv4-network-order-address>,
                                                     address: inaddr.sin-addr-value));
          accessor.remote-port := accessor-ntohs(inaddr.sin-port-value);
          // return nread
          nread
        end with-stack-structure;
      end with-stack-structure;
    end if;
  else
    next-method();
  end if;
end method;

/// Same as Win32/TCP method but calls SendTo instead of Send.
/// Sets up remote destination based on details recorded in socket
/// by accessor-read-into!
define method accessor-write-from
    (accessor :: <win32-udp-accessor>, stream :: <udp-socket>,
     offset :: <buffer-index>, count :: <buffer-index>, #key buffer,
     return-fresh-buffer?)
 => (nwritten :: <integer>, new-buffer :: <buffer>)
  ignore(return-fresh-buffer?);
  if (reply-socket?(accessor))
    let buffer :: <buffer> = buffer | stream-output-buffer(stream);
    let the-descriptor = accessor.socket-descriptor;
    if (accessor.connection-closed? | (~ the-descriptor))
      error("Stream closed") // ---*** FIX THIS
    else
      with-stack-structure (inaddr :: <LPSOCKADDR-IN>)
        let the-remote-host = remote-host(accessor);
        inaddr.sin-family-value := the-remote-host.address-family;
        inaddr.sin-addr-value := the-remote-host.numeric-host-address.network-order;
        inaddr.sin-port-value := accessor-htons(remote-port(accessor));
        let remaining = count;
        let addr = pointer-cast(<LPSOCKADDR>, inaddr);
        while (remaining > 0)
          let nwritten = 
          win32-send-buffer-to(accessor.socket-descriptor,
                               buffer-offset(buffer, offset + count - remaining),
                               remaining,
                               0,
                               addr,
                               size-of(<SOCKADDR-IN>));
          if (nwritten == $SOCKET-ERROR)
            win32-socket-error("win32-send", host-address: stream.remote-host,
                               host-port: stream.remote-port)
          end if;
          remaining := remaining - nwritten
        end while;
      end with-stack-structure;
    end if;
    values(count, buffer)
  else
    next-method();
  end if;
end method accessor-write-from;
