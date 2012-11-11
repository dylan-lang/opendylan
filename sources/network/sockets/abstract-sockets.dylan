Module:       sockets-internals
Author:       Toby
Synopsis:     Abstract sockets--stuff common to client and server sockets
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <sealed-object> (<object>)
end class;
define sealed domain make (subclass(<sealed-object>));
define sealed domain initialize (<sealed-object>);

// Gotta be free -- because stream classes are primary.  Servers can be
// primary because they aren't streams.

define open abstract free class
    <abstract-socket> (<closable-object>) end class;

define open generic local-host
    (the-socket :: type-union(<abstract-socket>, <socket-accessor>))
 => (host-address :: false-or(<internet-address>));

define open generic local-port
    (the-socket :: type-union(<abstract-socket>, <socket-accessor>))
 => (host-address :: false-or(<integer>));

// maybe to restrictive return type -- object instead?
define open generic socket-descriptor
    (the-socket :: type-union(<abstract-socket>, <socket-accessor>))
 => (handle-or-fd :: false-or(<accessor-socket-descriptor>));

//  Should think about meanings for the keys used by streams close
// #key abort? :: <boolean>, wait? :: <boolean>, synchronize? ::
// <boolean>.  Also ought to think about meaning of
// accessor-wait-for-completion for <socket>s.

define abstract class <socket-manager> (<object>)
  constant slot socket-manager-sockets :: <table> = make(<table>);
  constant slot socket-manager-threads :: <stretchy-vector> = make(<stretchy-vector>);
  constant slot socket-manager-server-threads :: <stretchy-vector> = make(<stretchy-vector>);
end class;

define variable *current-socket-manager* :: false-or(<socket-manager>) = #f;

define function current-socket-manager ()
 => (manager :: <socket-manager>)
  *current-socket-manager*
end function;

define function install-socket-manager (manager :: <socket-manager>)
 => ()
  *current-socket-manager* := manager
end function;

define function stop-sockets () => ()
  let manager = current-socket-manager();
  close-sockets(manager);
end function;

define method close-sockets (manager :: <socket-manager>) => ()
  shutdown-all-sockets(manager);
  wait-for-socket-threads(manager);
  close-all-sockets(manager);
  wait-for-socket-threads(manager, server?: #t);
  // Shutdown sockets now after all open sockets are closed.
  accessor-cleanup(manager);
end method;

define macro with-socket-thread
  { with-socket-thread (?keys:*) ?body:body end }
   =>
  { invoke-with-socket-thread(method () ?body end method, ?keys) }
end macro;

define method invoke-with-socket-thread (function :: <function>, #key server? :: <boolean> = #f) => ()
  let manager = current-socket-manager();
  let thread = current-thread();
  register-socket-manager-thread(manager, thread, server?: server?);
  function();
  // don't unregister because otherwise join-thread can see corrupted
  // sequence and gets confused (we'd have to lock the modification)
end method;

define method wait-for-socket-threads
    (manager :: <socket-manager>, #key server? :: <boolean> = #f) => ()
  let sockets = if (server?)
                  socket-manager-server-threads(manager)
                else
                  socket-manager-threads(manager)
                end if;
  do(join-thread, sockets);
end method;

define method register-socket-thread (#key thread = current-thread(), server? :: <boolean> = #f)
  let manager = current-socket-manager();
  register-socket-manager-thread(manager, thread, server?: server?);
end method;

define method unregister-socket-thread (#key thread = current-thread(), server? :: <boolean> = #f)
  let manager = current-socket-manager();
  unregister-socket-manager-thread(manager, thread, server?: server?);
end method;

define method register-socket-manager-thread
    (manager :: <socket-manager>, thread :: <thread>, #key server? :: <boolean> = #f)
 => ()
  if (server?)
    add-new!(socket-manager-server-threads(manager), thread);
  else
    add-new!(socket-manager-threads(manager), thread);
  end if;
end method;

define method unregister-socket-manager-thread
    (manager :: <socket-manager>, thread :: <thread>, #key server? :: <boolean> = #f)
 => ()
  if (server?)
    remove!(socket-manager-server-threads(manager), thread);
  else
    remove!(socket-manager-threads(manager), thread);
  end if;
end method;

define method close-all-sockets (manager :: <socket-manager>) => ()
  block (exit-loop)
    for (socket in socket-manager-sockets(manager))
      block (exit-step)
        close(socket);
      exception (recoverable-condition :: <recoverable-socket-condition>)
        close(socket, abort?: #t);
        exit-step();
      exception (unrecoverable-condition :: <socket-error>)
        exit-loop();
      end block;
    end for;
  end block;
end method;

define method shutdown-all-sockets (manager :: <socket-manager>) => ()
  block (exit-loop)
    for (socket in socket-manager-sockets(manager))
      block (exit-step)
        shutdown-socket(socket);
      exception (recoverable-condition :: <recoverable-socket-condition>)
        exit-step();
      exception (unrecoverable-condition :: <socket-error>)
        exit-loop();
      end block;
    end for;
  end block;
end method;

define method shutdown-socket (socket :: <abstract-socket>)
  let the-descriptor = socket.socket-descriptor;
  if (the-descriptor)
    accessor-shutdown(the-descriptor);
  end if;
end method;

define function start-sockets () => ()
  let manager = current-socket-manager();
  open-sockets(manager);
end function;

define method open-sockets (manager :: <socket-manager>) => ()
  accessor-startup(manager);
  $loopback-address := make(<ipv4-address>, address: "127.0.0.1");
  $local-host-name := accessor-local-host-name();
  $local-host := make(<ipv4-address>, name: $local-host-name);
end method;

define open method initialize
    (the-socket :: <abstract-socket>, #rest keys,
     #key already-registered? = #f) => ()
  apply(next-method, the-socket, already-registered?: #t, keys);
  // Use the socket as its own key.
  unless (already-registered?)
    let manager = current-socket-manager();
    socket-manager-sockets(manager)[the-socket] := the-socket;
  end unless;
end method;

define method close
    (the-socket :: <abstract-socket>,
     #rest keys,
     #key abort? = #f, wait? = #t, synchronize? = #f,
     already-unregistered? = #f) => ()
  if (socket-open?(the-socket))
    unless (already-unregistered?)
      let manager = current-socket-manager();
      remove-key!(socket-manager-sockets(manager), the-socket);
    end unless;
    apply(next-method, the-socket, already-unregistered?: #t, keys);
  end if;
end method close;

//  deprecrated
define open generic close-socket
    (the-socket :: <abstract-socket>, #rest keys, #key) => ();

define method close-socket (socket :: <abstract-socket>, #rest keys, #key)
 => ()
   apply(close, socket, keys);
end method;

define open generic socket-open?
    (the-socket :: <abstract-socket>) => (open? :: <boolean>);

// I rather hate this method of testing for open sockets/streams but
// its done that way in streams so best to be consistent for now.  I'd
// like to fix it in both libraries, probably by using a new direction
// keyword for streams like #"closed".  But that won't work for
// <server-sockets> to so what to do?  What to do?

define method  socket-open?
    (the-socket :: <abstract-socket>) => (open? :: <boolean>);
  (the-socket.socket-descriptor ~= #f)
end method;

