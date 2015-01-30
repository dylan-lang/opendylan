module:     environment-protocols
synopsis:   Abstract descriptions of machines that can be connected to in
            order to do remote debugging.
author:     Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// <MACHINE>
//    Describes a connection to a machine, on which target applications can
//    be executed and debugged.

//    This class is instantiable, with NETWORK-ADDRESS being the single
//    required init keyword. Clients should always supply a <STRING> as
//    the value for this keyword, although the slot is permitted to
//    hold the value #f for implementational reasons.

//    Callers of MAKE on this class must be prepared to handle errors
//    of the following classes:
//         <REMOTE-CONNECTION-FAILED-ERROR>


define open abstract class <machine> (<object>)

  constant slot %network-address :: false-or(<string>),
    required-init-keyword: network-address:;

end class;


///// MACHINE-NETWORK-ADDRESS
//    Returns the network address of a <MACHINE>.
//    Just returns the string "Local" for the environment's host machine,
//    since its network address can't be determined by the implementation
//    at the present time. (It also isn't necessary to know it, as far
//    as functionality is concerned).

define function machine-network-address (m :: <machine>) => (a :: <string>)
  m.%network-address | "Local"
end function;

define variable *environment-open-connections* = make(<stretchy-vector>);

define method initialize (m :: <machine>, #key) => ()
  next-method();
  add!(*environment-open-connections*, m);
end method;


///// MACHINE-HOSTNAME
//    Returns the hostname of a <MACHINE>.

define open generic machine-hostname (m :: <machine>) => (h :: <string>);

define method machine-hostname (m :: <machine>) => (h :: <string>)
  "Unknown"
end method;


///// MACHINE-CONNECTION-OPEN?
//    Returns #t if the connection to the <MACHINE> is known to be open,
//    and #f otherwise.
//    The back-end supplies the only applicable method.

define open generic machine-connection-open? (m :: <machine>)
  => (well? :: <boolean>);


///// ENVIRONMENT-HOST-MACHINE
//    Returns the instance of <MACHINE> that corresponds to the machine
//    on which the environment is actually running.

define variable $local-machine = #f;

define function environment-host-machine () => (m :: <machine>)
  unless ($local-machine)
    $local-machine := make(<machine>, network-address: #f)
  end unless;
  $local-machine
end function;


///// DO-MACHINE-CONNECTIONS
//    Iterates a user-supplied function over all open <machine>s currently
//    known to the environment.

//    The function 'f' should have the signature (<MACHINE>) => ().
//    The keyword argument INCLUDE-LOCAL? is a boolean flag that determines
//    whether the environment's host machine is included in the
//    iteration.

define function do-machine-connections
   (itr :: <function>, #key include-local? = #t)
  if (include-local?)
    itr(environment-host-machine())
  end if;
  for (connection in *environment-open-connections*)
    unless (connection == environment-host-machine())
      if (connection.machine-connection-open?)
        itr(connection)
      end if
    end unless
  end for
end function;


///// CLOSE-CONNECTION-TO-MACHINE
//    Frees up all implementational resources associated with the connection
//    to a specific <machine>, and makes it impossible to subsequently
//    use the same <machine> for further operations of any kind.

//    Clients of this function must be prepared to handle the following
//    error classes:
//       <ATTEMPTED-TO-CLOSE-LOCAL-CONNECTION>
//       <REMOTE-CONNECTION-CLOSED-ERROR>

define open generic close-connection-to-machine (m :: <machine>) => ();

// The default method performs front-end housekeeping. The implementation
// will arrange to call this method first.

define method close-connection-to-machine (m :: <machine>) => ()
  if (m == environment-host-machine())
    error(make(<attempted-to-close-local-connection>))
  elseif (~m.machine-connection-open?)
    error(make(<remote-connection-closed-error>,
               failed-connection: m))
  end if
end method;


///// <REMOTE-DEBUG-CONNECTION-ERROR>
//    The abstract parent class of all errors that can be signalled
//    during remote debugging operations.

define abstract class <remote-debug-connection-error> (<error>)
end class;


///// <REMOTE-CONNECTION-CLOSED-ERROR>
//    Signalled when an attempt is made to access a <MACHINE> after its
//    connection has been closed. A connection can only become closed
//    by using the function CLOSE-CONNECTION-TO-MACHINE.

define class <remote-connection-closed-error>
                (<remote-debug-connection-error>)

  constant slot failed-connection :: <machine>,
    required-init-keyword: failed-connection:;

end class;


///// <REMOTE-CONNECTION-FAILED-ERROR>
//    Signalled when a client calls MAKE on <MACHINE> if the underlying
//    implementation cannot obtain a connection to the specified network
//    address.

define class <remote-connection-failed-error>
                (<remote-debug-connection-error>)

  constant slot failed-network-address :: <string>,
    required-init-keyword: network-address:;

end class;


///// <REMOTE-CONNECTION-PASSWORD-MISMATCH-ERROR>
//    Signalled when a client calls MAKE on <MACHINE> if the supplied
//    password does not match the one stored on the remote server.

define class <remote-connection-password-mismatch-error>
                 (<remote-debug-connection-error>)

  constant slot failed-password :: <string>,
    required-init-keyword: password:;

end class;


///// <ATTEMPTED-TO-CLOSE-LOCAL-CONNECTION>
//    Only signalled if a client attempts to call CLOSE-CONNECTION-TO-
//    MACHINE with the <MACHINE> corresponding to the value of
//    ENVIRONMENT-HOST-MACHINE().

define class <attempted-to-close-local-connection>
                (<remote-debug-connection-error>)
end class;
