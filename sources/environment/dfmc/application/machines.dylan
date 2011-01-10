Module:    dfmc-application
Synopsis:  The implementation of the <MACHINE> class, and related protocols.
Author:    Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

///// <CONNECTED-MACHINE>
//    The concrete implementation of <MACHINE>.

define sealed abstract class <connected-machine> (<machine>)
  slot machine-debug-connection :: <debugger-connection>;
  slot connection-open? :: <boolean> = #t;
  constant slot process-mappings :: <object-table> = make(<object-table>);
end class;

define sealed class <locally-connected-machine> (<connected-machine>)
end class;

define sealed class <remotely-connected-machine> (<connected-machine>)
  constant slot machine-password :: <string>,
    required-init-keyword: password:;
end class;


///// MAKE (Dylan)
//    Ensures that the <MACHINE> class is instantiable by clients.

define sideways method make 
    (c == <machine>, 
     #rest args, #key network-address = #f, #all-keys) => (m)
  if (network-address)
    apply(make, <remotely-connected-machine>, args)
  else
    apply(make, <locally-connected-machine>, args)
  end if;
end method;


///// INITIALIZE (Dylan)

define method initialize (m :: <remotely-connected-machine>, #key) => ()
  next-method();
  block ()
    m.machine-debug-connection := 
      make(<remote-debugger-connection>,
           network-address: m.machine-network-address,
           password: m.machine-password);
  exception (<open-debugger-connection-failure>)
    m.connection-open? := #f;
    error(make(<remote-connection-failed-error>,
               network-address: m.machine-network-address))
  exception (<open-debugger-connection-password-mismatch>)
    m.connection-open? := #f;
    error(make(<remote-connection-password-mismatch-error>,
               password: m.machine-password))
  end block;
end method;

define method initialize (m :: <locally-connected-machine>, #key) => ()
  next-method();
  m.machine-debug-connection := host-machine()
end method;


///// MACHINE-CONNECTION-OPEN? (Environment Protocols)
//    The back-end's implementation.

define method machine-connection-open?
    (m :: <connected-machine>) => (well? :: <boolean>)
  m.connection-open?
end method;


///// CLOSE-CONNECTION-TO-MACHINE (Environment Protocols)
//    Releases all resources associated with the connection, and ensures
//    that the connection cannot be used again.

define method close-connection-to-machine (m :: <connected-machine>) => ()
  next-method();
  m.connection-open? := #f;
end method;

define method close-connection-to-machine (m :: <remotely-connected-machine>)
  => ()
  next-method();
  close-remote-debugger-connection(m.machine-debug-connection)
end method;


///// MACHINE-HOSTNAME (Environment Protocols)

define method machine-hostname (m :: <connected-machine>) => (h :: <string>)
  m.machine-debug-connection.connection-hostname
end method;
