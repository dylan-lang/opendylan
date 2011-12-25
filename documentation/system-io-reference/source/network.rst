*******************
The Network Library
*******************

Overview
--------

This chapter covers the Network library. The Network library provides
Internet address protocols and TCP/IP server and client sockets. It
exports a single module, called Sockets.

Utilities
---------

This section covers the *start-sockets* function, which all libraries
using the Network library must call before *any* other call to the
Network library API. It also covers the *with-socket-thread* macro which
registers the current thread as a thread that will call a socket
function that blocks.
                                                                                                                                                                                                                                                                                                        

start-sockets
~~~~~~~~~~~~~

Function
''''''''

start-sockets () => ()
                      

Applications must call this function before using *any* other function
or variable from the Network library.

This function is necessary because the Win32 API library Winsock2, which
the Network library calls to get native socket functionality, requires
applications to call an initialization function before calling any
Winsock2 API functions. The call to *start-sockets* calls this
underlying Winsock2 function.

Note that you must not call *start-sockets* from a top-level form in any
DLL project. The combination of this, and the restriction that you must
call *start-sockets* before calling anything else, implies that no
Network library function or variable can be called (directly or
indirectly) from a top-level form in any DLL project. Instead, the DLL
project should define a start function that calls *start-sockets*
(directly or indirectly) or re-export *start-sockets* so that their
clients can arrange to have it called from a top-level form in an
appropriate EXE project.

Applications using the Network library must arrange for *start-sockets*
to be called (directly or indirectly) before *any* other sockets API
functions. A good place to do this is at the beginning of your start
function (usually the *main* method). For example:

define method main () => ();
                            

start-sockets();
                

let the-server = make(<TCP-server-socket>, port: 7);
                                                    

...
   

end;
    

begin
     

main();
       

end;
    

New start functions that call *start-sockets* and that are defined for
DLL projects that use the Network library will inherit all of the
restrictions described above for *start-sockets*.

Calling a Network library function before calling *start-sockets*
results in a ``<sockets-not-initialized>`` error. Calling *start-sockets*
from a top-level form in a DLL project will result in unpredictable
failures—probably access violations during initialization.

with-socket-thread
~~~~~~~~~~~~~~~~~~

Statement macro
'''''''''''''''

with-socket-thread (#key *server?*)
                                    

*body*
      

end;
    

Registers the current thread as a blocking socket thread, that is, a
thread which will call a socket function that blocks, such as
*read-element* or *accept*.

The reason for the registration is that Network library shutdown can
then synchronize with these threads. The early part of the shutdown
sequence should cause the threads to unblock with an
``<end-of-stream-error>`` so that they can do whatever application cleanup
is necessary. Once these threads have exited, the rest of the shutdown
sequence can be executed.

A server socket thread (blocking on *accept* rather than *read-element*
) notices that the shutdown sequence is underway slightly later, with a
``<blocking-call-interrupted>`` condition.

Internet addresses
------------------

This section covers Internet address protocols.
                                               

Basic Internet address protocol
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This section covers the class ``<internet-address>`` and related generic
functions and constants.
                                                                                               

<internet-address>
~~~~~~~~~~~~~~~~~~

Open abstract primary instantiable class
''''''''''''''''''''''''''''''''''''''''

Superclasses: ``<object>``

The class of objects representing Internet addresses used as endpoints
for peer-to-peer socket connections.

To construct an ``<internet-address>`` object you must supply either the
*name:* or *address:* keyword. For example:

make (<internet-address>, name: "www.whatever.com")
                                                   

or

make (<internet-address>, address: "9.74.122.0")
                                                

*make* on ``<internet-address>`` returns an instance of ``<ipv4-address>``.

Keywords:

-  *name:* An instance of ``<string>`` representing a symbolic internet
   address.
-  *address:* An instance of ``<string>`` representing a presentation
   (dotted) form Internet address or an instance of ``<numeric-address>``
   (see below).

host-name
~~~~~~~~~

Open generic function
'''''''''''''''''''''

host-name *internet-address* => *name*
                                      

Returns an instance of ``<string>`` containing a symbolic host name. The
*internet-address* argument must be an instance of ``<internet-address>``
.

Usually the name returned is the canonical host name. Note, however,
that the implementation is conservative about making DNS calls. Suppose
that the ``<internet-address>`` instance was created with the *name:*
keyword and no other information. If the application has not made any
other requests that would require a DNS call, such as to *host-address*
or *aliases* (see below), the name that this function returns will be
the one specified with the *name:* keyword, regardless of whether that
is the canonical name or not.

host-address
~~~~~~~~~~~~

Open generic function
'''''''''''''''''''''

host-address *internet-address* => *address*
                                            

Returns an instance of ``<string>`` containing the presentation form of
the host address. In the case of multi-homed hosts this will usually be
the same as:

multi-homed-internet-address.all-addresses.first.host-address
                                                             

In the case of an Internet address created using the *address:* keyword
it will be either the keyword value or
*all-addresses.first.host-address*.

numeric-host-address
~~~~~~~~~~~~~~~~~~~~

Open generic function
'''''''''''''''''''''

numeric-host-address *internet-address* => *numeric-address*
                                                            

Returns the host address as a ``<numeric-address>``.

all-addresses
~~~~~~~~~~~~~

Open generic function
'''''''''''''''''''''

all-addresses *internet-address* => *sequence*
                                              

Returns an instance of ``<sequence>`` whose elements are
``<internet-address>`` objects containing all known addresses for the
host.

aliases
~~~~~~~

Open generic function
'''''''''''''''''''''

aliases *internet-address* => *sequence*
                                        

Returns an instance of ``<sequence>`` whose elements are instances of
``<string>`` representing alternative names for the host.

$loopback-address
~~~~~~~~~~~~~~~~~

Constant
''''''''

An instance of ``<internet-address>`` representing the loopback address:
*"127.0.0.1"*.

$local-host
~~~~~~~~~~~

Constant
''''''''

An instance of ``<internet-address>`` representing the host on which the
application using sockets is correctly running.

Note that this value is not necessarily the same as would be created by
the expression

make (<internet-address>, name: "localhost")
                                            

The address assigned to the symbolic name *localhost* is dependent on
the configuration of DNS. In some cases this may be configured to be the
loopback address rather than a real address for the local host.

The <IPV6-ADDRESS> class
^^^^^^^^^^^^^^^^^^^^^^^^

This name is reserved for future development.
                                             

The <NUMERIC-ADDRESS> class
^^^^^^^^^^^^^^^^^^^^^^^^^^^

This section describes numeric Internet representation and associated
protocols.
                                                                                

<numeric-address>
~~~~~~~~~~~~~~~~~

Sealed abstract primary class
'''''''''''''''''''''''''''''

Superclasses: ``<object>``

The class of objects representing the numeric form of an Internet
addresses.

Currently only ipv4 (32-bit) addresses are supported. Ipv6 addresses
will be added when they are supported by Winsock2. In general
``<numeric-address>`` objects are accessed using the functions
*host-order* or *network-order*, depending on the context in which they
are employed.

network-order
~~~~~~~~~~~~~

Sealed generic function
'''''''''''''''''''''''

network-order *address* => *network-order-address*
                                                  

Returns the value of the numeric address in network order. The argument
is a general instance of ``<numeric-address>``. The class of the object
returned depends upon the particular subclass of the argument; the
*network-order* method for ``<ipv4-numeric-address>`` returns an instance
of ``<machine-word>``.

*Network order* is big-endian byte order.

host-order
~~~~~~~~~~

Sealed generic function
'''''''''''''''''''''''

host-order *address* => *host-order-address*
                                            

Like *network-order* but returns the value in host order.

*Host order* is either big-endian byte order on a big-endian host
machine and little-endian on a little-endian host machine.

IPV4 addresses
^^^^^^^^^^^^^^

<ipv4-numeric-address>
~~~~~~~~~~~~~~~~~~~~~~

Open abstract primary instantiable class
''''''''''''''''''''''''''''''''''''''''

Superclasses: ``<numeric-address>``

The single slot of this class contains a 32-bit value representing a
ipv4 address. This slot is accessed by the generic functions
*network-order* and *host-order* described above.
``<ipv4-numeric-address>`` has two concrete subclasses
``<ipv4-network-order-address>`` and ``<ipv4-host-order-address>``. Make
``<ipv4-numeric-address>`` returns one or the other of these depending
upon the value of the *order:* keyword.

Keywords:

-  *value:* An instance of ``<machine-word>``. Required.
-  *order:* One of *#"network-order"* or *#"host-order"*. Required.

host-order
~~~~~~~~~~

G.f. method
'''''''''''

host-order *ip4-numeric-address* => *machine-word*
                                                  

Returns the numeric address in host order as an instance of
``<machine-word>``. The argument is an instance of
``<ip4-numeric-address>``.

network-order
~~~~~~~~~~~~~

G.f. method
'''''''''''

network-order *ipv4-numeric-address* => *machine-word*
                                                      

Returns the numeric address in network order as an instance of
``<machine-word>``. The argument is an instance of
``<ip4-numeric-address>``.

as
~~

G.f. method
'''''''''''

as *string* *ipv4-numeric-address* => *string*
                                              

Returns the presentation (dotted string) form of an instance of
``<ip4-numeric-address>``.

<ipv4-network-order-address>
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Sealed concrete class
'''''''''''''''''''''

Superclasses: ``<ipv4-numeric-address>``

Concrete subclass for network-order numeric addresses.

make(<ipv4-network-order-address>)
                                  

is equivalent to

make(<ipv4-numeric-address>, order: network-order)
                                                  

<ipv4-host-order-address>
~~~~~~~~~~~~~~~~~~~~~~~~~

Sealed concrete class
'''''''''''''''''''''

Superclasses: ``<ipv4-numeric-address>``

Concrete subclass for host order numeric addresses.

Sockets
-------

This section describes socket classes and protocols.
                                                    

The <ABSTRACT-SOCKET> class
^^^^^^^^^^^^^^^^^^^^^^^^^^^

<abstract-socket>
~~~~~~~~~~~~~~~~~

Open abstract uninstantiable free class
'''''''''''''''''''''''''''''''''''''''

Superclasses: ``<object>``

The common superclass of all socket objects including ``<socket>`` (IP
client socket), ``<server-socket>`` and ``<socket-accessor>``.

-  Keywords:

*socket-descriptor:*
                    

-  A Windows handle or UNIX fd (file descriptor) for the socket. In
   general users of the sockets API should not need to use this keyword.
   Only implementors of new socket classes should be interested.

Each subclass of ``<abstract-socket>`` must provide methods for *close*
and for the following generic functions:
                                                                                                              

local-port
~~~~~~~~~~

Open generic function
'''''''''''''''''''''

local-port *socket* => *port-number*
                                    

Returns the local port number for an instance of ``<socket>``,
``<datagram-socket>`` or ``<server-socket>``. The return value is an
instance of ``<integer>``.

socket-descriptor
~~~~~~~~~~~~~~~~~

Open generic function
'''''''''''''''''''''

socket-descriptor *socket* => descriptor
                                        

Returns the descriptor (handle or fd) for the socket. The argument is an
instance of ``<abstract-socket>`` and the return value an instance of
``<accessor-socket-descriptor>``.

local-host
~~~~~~~~~~

Open generic function
'''''''''''''''''''''

local-host *socket* => *host-address*
                                     

Returns the address of the local host. The argument is an instance of
``<abstract-socket>`` and the return value an instance of
``<internet-address>``.

The <SERVER-SOCKET> class
^^^^^^^^^^^^^^^^^^^^^^^^^

<server-socket>
~~~~~~~~~~~~~~~

Open abstract primary instantiable class
''''''''''''''''''''''''''''''''''''''''

Superclasses: ``<abstract-socket>``

Server-sockets listen on a specified port for connection requests which
come in over the network. Either the *port:* or *service:* keyword must
be supplied.

-  Keywords:
-  *service:* An instance of ``<string>`` containing an abstract name for
   a service with a “well-known” port, such as *"ftp"* or *"daytime"*.
   Valid names depend on the configuration of the DNS. Required unless
   *port:* is supplied.
-  *port:* An instance of ``<integer>`` identifying the port on which the
   ``<server-socket>`` should listen for connection requests. Required
   unless *service:* is supplied.
-  *protocol:* An instance of ``<string>`` naming the protocol. Currently
   *"tcp"* is the only supported protocol. You can create instances of
   protocol-specific subclasses as an alternative to using the
   *protocol:* keyword. For example, *make(<server-socket>, protocol:
   "tcp",* …*)* is equivalent to *make(<TCP-server-socket>,* …*)*.
-  *make* on *(<server-socket>)* returns an instance of
   ``<tcp-server-socket>`` by default.

accept
~~~~~~

Open generic function
'''''''''''''''''''''

accept *server-socket* #rest *args* #key => *result*
                                                    

Blocks until a connect request is received, then it returns a connected
instance of ``<socket>``. The particular subclass of ``<socket>`` returned
depends on the actual class of the argument, which must be a general
instance of ``<server-socket>``. Calling accept on ``<tcp-server-socket>``
returns a connected ``<tcp-socket>``. The keyword arguments are passed to
the creation of the ``<socket>`` instance. For UDP sockets *accept*
returns immediately with an instance of ``<udp-socket>``. No blocking
happens for UDP sockets because they are connectionless. After reading
from a UDP socket returned from *accept* the socket can be interrogated
for the location of the sender using *remote-host* and *remote-port*.

with-server-socket
~~~~~~~~~~~~~~~~~~

Macro
'''''

with-server-socket (*server-var* [:: *server-class* ], *keywords*)
                                                                   

*body*
      

end;
    

Creates an instance of ``<server-socket>``, using the (optional)
*server-class* argument and keyword arguments to make the
``<server-socket>``, and binds it to the local variable named by
*server-var*. The *body* is evaluated in the context of the binding and
the ``<server-socket>`` is closed after the body is executed.

start-server
~~~~~~~~~~~~

Macro
'''''

start-server ([*server-var* = ]*socket-server-instance*,
                                                         

*socket-var* [, *keywords* ])
                             

*body*
      

end;
    

Enters an infinite *while(#t)* *accept* loop on the server socket. Each
time accept succeeds the ``<socket>`` returned from accept is bound to
*socket-var* and the *body* is evaluated in the context of the binding.
When *body* exits, *accept* is called again producing a new binding for
*socket-var*. The optional keywords are passed to the call to *accept*
.

The <TCP-SERVER-SOCKET> class
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

<tcp-server-socket>
~~~~~~~~~~~~~~~~~~~

Class
'''''

Superclass: ``<server-socket>``

The class of TCP server sockets. A server socket is an object which
listens for requests for connections from the network. When accept is
called on the server socket and a request for connection is detected,
accept returns a connected ``<socket>``.

Keywords:

-  *element-type:* Establishes a new default for the *element-type* of
   ``<TCP-socket>`` instances returned by calling *accept* with this
   server socket as the argument to *accept*. This default
   *element-type* may be overridden for any particular call to *accept*
   by using the *element-type:* keyword to *accept*. If no
   *element-type:* is specified when the server socket is created,
   ``<byte-character>`` is used as the default *element-type*.

accept
~~~~~~

G.f. method
'''''''''''

accept *server-socket* #rest *args* #key *element-type* =>
*connected-socket*
                                                                             

This method on *accept* takes an instance of type ``<tcp-server-socket>``
and returns a connected instance of ``<tcp-socket>``. The *element-type:*
keyword controls the element type of the ``<tcp-socket>`` (stream)
returned from *accept*. If the keyword is not supplied, the default
value used is *#f*. The other keyword arguments are passed directly to
the creation of the ``<tcp-socket>`` instance.

The <SOCKET> class
^^^^^^^^^^^^^^^^^^

<socket>
~~~~~~~~

Open abstract free instantiable class
'''''''''''''''''''''''''''''''''''''

Superclasses: ``<abstract-socket>``, ``<external-stream>``

The class of general client sockets. All client sockets are streams.

Keywords:

-  *direction:* Specifies the direction of the stream. It must be one of
   *#"input"*, *#"output"*, and *"#input-output"*. This keyword is an
   inherited streams class keyword. See the Streams library
   documentation in the *System and I/O* library reference for a full
   description.
-  *element-type:* An instance of ``<class>``. Useful values are
   ``<byte-character>`` and ``<byte>``. This keyword is an inherited
   streams class keyword. See the Streams library documentation in the
   *System and I/O* library reference for a full description.

The <BUFFERED-SOCKET> class
^^^^^^^^^^^^^^^^^^^^^^^^^^^

<buffered-socket>
~~~~~~~~~~~~~~~~~

Class
'''''

Superclasses: ``<socket>``, ``<double-buffered-stream>``

Socket streams whose elements are bytes or characters. These inherit
buffering protocols and the implications of *read*, *write*,
*read-element*, *write-element*, *force-output* and suchlike methods
from ``<double-buffered-stream>``.

Keywords:

*force-output-before-read?:*
                            

-  An instance of ``<boolean>``. Defaults value: *#t*. The methods which
   implement the stream reading protocols (*read*, *read-line*,
   *read-element* and so on) for instances of ``<socket>`` call
   *force-output* by default before blocking. This is to ensure that any
   pending output has been sent to the peer before the socket blocks
   waiting to read data sent by the peer. This corresponds to the
   expected, usual behavior of single-threaded client sockets and avoids
   deadlock in usual cases. Multi-threaded applications, particularly
   applications where one thread is reading and another thread is
   writing to the same socket, may wish to inhibit the default
   *force-output*. If the socket is created with
   *force-output-before-read?:* as *#f*, *force-output* will not be
   called before the read functions block.

The <TCP-SOCKET> class
^^^^^^^^^^^^^^^^^^^^^^

The class of TCP client sockets.
                                

<tcp-socket>
~~~~~~~~~~~~

Class
'''''

Superclasses: ``<buffered-socket>``

The class of TCP client sockets.

-  Keywords:

Of the keywords below, *host:* and one of either *service:* or *port:*
are required.

-  *host:* An instance of ``<internet-address>`` or ``<string>``. The
   remote host to connect to. The ``<string>`` may be either a host name
   or a presentation-form Internet address. Required.
-  *service:* An instance of ``<string>``. A ``<string>`` containing an
   abstract name for a service with a “well-known“ port, such as *"ftp"*
   or *"daytime"*. Valid names depend on the configuration of the DNS.
   Required unless *port:* is supplied.
-  *protocol:* An instance of ``<string>`` naming the protocol. Currently
   *#"tcp"* and *#"udp"* are the only supported protocols. You can
   create instances of protocol-specific subclasses as an alternative to
   using the *protocol:* keyword. For example *make(<socket>, protocol:
   #"tcp", …)* is equivalent to *make(<TCP-socket>, …)*. *make* on
   ``<socket>`` returns an instance of ``<tcp-socket>`` by default.
-  *port:* An instance of ``<integer>`` representing the remote port to
   connect to. Required unless *service:* is supplied.
-  *element-type:* An instance of ``<class>``. Useful values for
   ``<tcp-streams>`` are ``<byte-character>`` and ``<byte>``. This keyword is
   an inherited streams class keyword. See :doc:`streams` for a full description.

remote-port
~~~~~~~~~~~

Open generic function
'''''''''''''''''''''

remote-port *socket* => *port-number*
                                     

Returns the remote port number for a ``<socket>``. The value returned is
an instance of ``<integer>``.

remote-host
~~~~~~~~~~~

Open generic function
'''''''''''''''''''''

remote-host *socket* => *remote-host-address*
                                             

Returns the remote host for a ``<socket>``. The value returned is an
instance of ``<internet-address>``.

The <UDP-SOCKET> class
^^^^^^^^^^^^^^^^^^^^^^

The class of UDP client sockets.
                                

<udp-socket>
~~~~~~~~~~~~

Class
'''''

Superclasses: ``<buffered-socket>``

The class of UDP client sockets.

Keywords:

Of the keywords below, *host:* and one of either *service:* or *port:*
are required.

-  *host:* An instance of ``<internet-address>`` or ``<string>``. The
   remote host to connect to. The ``<string>`` may be either a host name
   or a *presentation-form* Internet address. Required.
-  *service:* An instance of ``<string>``. A ``<string>`` containing an
   abstract name for a service with a “well-known port”, such as *"ftp"*
   or *"daytime"*. Valid names depend on the configuration of the DNS.
   Required unless *port:* is supplied.
-  *protocol:* An instance of ``<string>`` naming the protocol. Currently
   *#"tcp"* and *#"udp"* are the only supported protocols. You can
   create instances of protocol-specific subclasses as an alternative to
   using the *protocol:* keyword. For example *make(<socket>, protocol:
   #"udp", …)* is equivalent to *make(<UDP-socket>, …)*. *make* on
   ``<socket>`` returns an instance of ``<tcp-socket>`` by default.
-  *port:* An instance of ``<integer>`` representing the remote port to
   connect to. Required unless *service:* is supplied.
-  *element-type:* An instance of ``<class>``. Useful values for
   ``<udp-socket>`` s are ``<byte-character>`` and ``<byte>``. This keyword
   is an inherited streams class keyword. See :doc:`streams` for a full
   description.

The <UDP-SERVER-SOCKET> class
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The class of UDP server sockets.
                                

<udp-server-socket>
~~~~~~~~~~~~~~~~~~~

Class
'''''

Superclass: ``<server-socket>``

The class of UDP server sockets. A server socket is an object that
listens for requests from the network. When *accept* is called on the
UDP server socket, *accept* returns a ``<udp-socket>``.

Keywords:

-  *element-type:* Establishes a new default for the element-type of
   ``<UDP-socket>`` instances returned by calling *accept* with this
   server socket as the argument to *accept*. This default element-type
   may be overridden for any particular call to *accept* by using the
   *element-type:* keyword to *accept*. If no *element-type:* is
   specified when the server socket is created, ``<byte-character>`` is
   used as the default element-type.

Socket conditions
-----------------

This section lists the socket condition classes in the Network library.
                                                                       

<socket-condition>
^^^^^^^^^^^^^^^^^^

All socket conditions are general instances of ``<socket-condition>``.
Some are recoverable and others are not.
                                                                                                              

<socket-condition>
~~~~~~~~~~~~~~~~~~

Condition
'''''''''

Superclasses: ``<simple-condition>``

The class of socket conditions. It inherits the *format-string:* and
*format-arguments:* keywords from ``<simple-condition>``.

Slots:

-  *socket-condition-details*
-  Most socket conditions originate in error return codes from Open
   Dylan’s Winsock2 library, an FFI interface to the native socket
   library Winsock2.
-  The *socket-condition-details* slot provides information about the
   low-level failure which was the source for the condition. In most
   cases this slot will hold an instance of
   ``<socket-accessor-condition>``, below.
-  When creating general instances of ``<socket-condition>``, you can use
   the *details:* keyword to set the value for this slot.

<socket-error>
^^^^^^^^^^^^^^

The class ``<socket-error>`` is the superclass of all unrecoverable socket
conditions.
                                                                                    

<socket-error>
~~~~~~~~~~~~~~

Condition
'''''''''

Superclasses: ``<socket-condition>``

The class of socket conditions from which no recovery is possible.

<internal-socket-error>
^^^^^^^^^^^^^^^^^^^^^^^

The class ``<internal-socket-error>`` is the class of unexpected socket
errors.
                                                                             

<internal-socket-error>
~~~~~~~~~~~~~~~~~~~~~~~

Condition
'''''''''

Superclasses: ``<socket-error>``

The class of unexpected errors from Open Dylan’s Winsock2 library,
an FFI interface to the native socket library Winsock2.

Inspect the contents of the *socket-condition-details* slot for more
information.

<recoverable-socket-condition>
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The ``<recoverable-socket-condition>`` class is the general class of
socket conditions for which an application may be able to take some
remedial action.
                                                                                                                                                       

<recoverable-socket-condition>
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Condition
'''''''''

Superclasses: ``<socket-condition>``

The general class of socket conditions for which an application may be
able to take some remedial action.

For instance, a web browser receiving such conditions as
``<connection-refused>`` or ``<host-not-found>`` (see below) would normally
expect to report those conditions to the user and continue with some
other connection request from the user, while a server receiving a
``<connection-closed>`` condition from a connected ``<socket>`` would
probably close the ``<socket>`` and continue to handle other requests for
connections.

<network-not-responding>
^^^^^^^^^^^^^^^^^^^^^^^^

<network-not-responding>
~~~~~~~~~~~~~~~~~~~~~~~~

Condition
'''''''''

Superclasses: ``<recoverable-socket-condition>``

The network — probably a local network — is down. Try again later.

<invalid-address>
^^^^^^^^^^^^^^^^^

<invalid-address>
~~~~~~~~~~~~~~~~~

Condition
'''''''''

Superclasses: ``<recoverable-socket-condition>``

A badly formed address string has been passed to a function trying to
make an ``<internet-address>``.

<host-not-found>
^^^^^^^^^^^^^^^^

<host-not-found>
~~~~~~~~~~~~~~~~

Condition
'''''''''

Superclasses: ``<recoverable-socket-condition>``

The Domain Name Server (DNS) cannot resolve the named host or internet
address. Try again with a different (correct) name or address.

<server-not-responding>
^^^^^^^^^^^^^^^^^^^^^^^

<server-not-responding>
~~~~~~~~~~~~~~~~~~~~~~~

Condition
'''''''''

Superclasses: ``<recoverable-socket-condition>``

The Domain Name Server (DNS) did not respond or returned an ambiguous
result. Try again.

<host-unreachable>
^^^^^^^^^^^^^^^^^^

<host-unreachable>
~~~~~~~~~~~~~~~~~~

Condition
'''''''''

Superclasses: ``<recoverable-socket-condition>``

The remote host cannot be reached from this host at this time.

<socket-closed>
^^^^^^^^^^^^^^^

<socket-closed>
~~~~~~~~~~~~~~~

Condition
'''''''''

Superclasses: ``<recoverable-socket-condition>``

The socket or server socket has been closed.

Most operations on closed instances of ``<TCP-socket>`` return instances
of ``<stream-closed-error>`` (from the Streams library) rather than
instances of ``<socket-closed>``.

<connection-failed>
^^^^^^^^^^^^^^^^^^^

<connection-failed>
~~~~~~~~~~~~~~~~~~~

Condition
'''''''''

Superclasses: ``<recoverable-socket-condition>``

The attempt to connect to the remote host was not successful. Connection
failed for one of the following reasons: because the connect request
timed out or because it was refused, or because the remote host could
not be reached.

<connection-closed>
^^^^^^^^^^^^^^^^^^^

<connection-closed>
~~~~~~~~~~~~~~~~~~~

Condition
'''''''''

Superclasses: ``<recoverable-socket-condition>``

The connection to the remote host has been broken. The socket should be
closed. To try again, open a new socket.

<address-in-use>
^^^^^^^^^^^^^^^^

<address-in-use>
~~~~~~~~~~~~~~~~

Condition
'''''''''

Superclasses: ``<recoverable-socket-condition>``

A process on the machine is already bound to the same fully qualified
address. This condition probably occurred because you were trying to use
a port with an active server already installed, or a process crashed
without closing a socket.

<blocking-call-interrupted>
^^^^^^^^^^^^^^^^^^^^^^^^^^^

<blocking-call-interrupted>
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Condition
'''''''''

Superclasses: ``<recoverable-socket-condition>``

A blocking socket call, like *read*, *write* or *accept*, was
interrupted.

<out-of-resources>
^^^^^^^^^^^^^^^^^^

<out-of-resources>
~~~~~~~~~~~~~~~~~~

Condition
'''''''''

Superclasses: ``<recoverable-socket-condition>``

The implementation-dependent limit on the number of open sockets has
been reached. You must close some sockets before you can open any more.
The limits for Windows NT (non-server machines) and Windows 95 are
particularly small.

<socket-accessor-error>
^^^^^^^^^^^^^^^^^^^^^^^

<socket-accessor-error>
~~~~~~~~~~~~~~~~~~~~~~~

Condition
'''''''''

Superclasses: ``<socket-error>``

An implementation-specific error from the C-FFI interface to the native
socket library. Usually instances of this class these appear in the
*socket-condition-details* slot of another ``<socket-condition>``.

<win32-socket-error>
^^^^^^^^^^^^^^^^^^^^

<win32-socket-error>
~~~~~~~~~~~~~~~~~~~~

Condition
'''''''''

Superclasses: ``<socket-accessor-error>``

A Win32-specific error from the Winsock2 library, a C-FFI interface to
the native socket library Winsock2. A function in the FFI library has
returned an error return code.

Slots:

*WSA-numeric-error-code*
                        

-  Contains the numeric error code that was returned. An instance of
   ``<integer>``.

*WSA-symbolic-error-code*
                         

-  Contains an instance of ``<string>`` giving the symbolic
   (human-readable) form of the error code. For example, the string
   might be *"wsanotsock"*.
-  *explanation* An explanation if any of the error. An instance of
   ``<string>``.

*calling-function*
                  

-  The name of Winsock2 FFI interface function which returned the error
   code. An instance of ``<string>``.


