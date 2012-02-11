*******************
The Network Library
*******************

.. current-library:: network
.. current-module:: sockets

Overview
--------

This chapter covers the Network library. The Network library provides
Internet address protocols and TCP/IP server and client sockets. It
exports a single module, called Sockets.

Utilities
---------

This section covers the :func:`start-sockets` function, which all
libraries using the Network library must call before *any* other call to
the Network library API. It also covers the :macro:`with-socket-thread`
macro which registers the current thread as a thread that will call a
socket function that blocks.

.. function:: start-sockets

   :signature: start-sockets () => ()

   :description:

     Applications must call this function before using *any* other
     function or variable from the Network library.

     This function is necessary because the Win32 API library Winsock2,
     which the Network library calls to get native socket functionality,
     requires applications to call an initialization function before
     calling any Winsock2 API functions. The call to ``start-sockets``
     calls this underlying Winsock2 function.

     Note that you must not call ``start-sockets`` from a top-level form
     in any DLL project. The combination of this, and the restriction
     that you must call ``start-sockets`` before calling anything else,
     implies that no Network library function or variable can be called
     (directly or indirectly) from a top-level form in any DLL project.
     Instead, the DLL project should define a start function that calls
     ``start-sockets`` (directly or indirectly) or re-export
     ``start-sockets`` so that their clients can arrange to have it
     called from a top-level form in an appropriate EXE project.

     Applications using the Network library must arrange for
     ``start-sockets`` to be called (directly or indirectly) before
     *any* other sockets API functions. A good place to do this is at
     the beginning of your start function (usually the ``main`` method).
     For example:

     .. code-block:: dylan

       define method main () => ();
         start-sockets();
         let the-server = make(<TCP-server-socket>, port: 7);
         ...
       end;

       begin
         main();
       end;

     New start functions that call ``start-sockets`` and that are
     defined for DLL projects that use the Network library will inherit
     all of the restrictions described above for ``start-sockets``.

     Calling a Network library function before calling ``start-sockets``
     results in a :class:`<sockets-not-initialized>` error. Calling
     ``start-sockets`` from a top-level form in a DLL project will
     result in unpredictable failures—probably access violations during
     initialization.

.. macro:: with-socket-thread
   :statement:

   :macrocall:
     .. code-block:: dylan

       with-socket-thread (#key *server?*)
         *body*
       end;

   :description:

     Registers the current thread as a blocking socket thread, that is,
     a thread which will call a socket function that blocks, such as
     :gf:`read-element` or :gf:`accept`.

     The reason for the registration is that Network library shutdown
     can then synchronize with these threads. The early part of the
     shutdown sequence should cause the threads to unblock with an
     :class:`<end-of-stream-error>` so that they can do whatever
     application cleanup is necessary. Once these threads have exited,
     the rest of the shutdown sequence can be executed.

     A server socket thread (blocking on :gf:`accept` rather than
     :gf:`read-element`) notices that the shutdown sequence is underway
     slightly later, with a :class:`<blocking-call-interrupted>`
     condition.

Internet addresses
------------------

This section covers Internet address protocols.

Basic Internet address protocol
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This section covers the class :class:`<internet-address>` and related
generic functions and constants.

.. class:: <internet-address>
   :open:
   :abstract:
   :primary:
   :instantiable:

   :superclasses: <object>

   :keyword name: An instance of ``<string>`` representing a symbolic
     internet address.
   :keyword address: An instance of ``<string>`` representing a
     presentation (dotted) form Internet address or an instance of
     :class:`<numeric-address>` (see below).

   :description:

     The class of objects representing Internet addresses used as
     endpoints for peer-to-peer socket connections.

     To construct an ``<internet-address>`` object you must supply
     either the ``name:`` or ``address:`` keyword. For example:

     .. code-block:: dylan

       make (<internet-address>, name: "www.whatever.com")

     or

     .. code-block:: dylan

       make (<internet-address>, address: "9.74.122.0")

     :drm:`make` on ``<internet-address>`` returns an instance of
     :class:`<ipv4-address>`.

.. generic-function:: host-name
   :open:

   :signature: host-name *internet-address* => *name*

   :description:

     Returns an instance of ``<string>`` containing a symbolic host
     name. The *internet-address* argument must be an instance of
     :class:`<internet-address>`.

     Usually the name returned is the canonical host name. Note,
     however, that the implementation is conservative about making DNS
     calls. Suppose that the :class:`<internet-address>` instance was
     created with the ``name:`` keyword and no other information. If the
     application has not made any other requests that would require a
     DNS call, such as to :gf:`host-address` or :gf:`aliases`, the name
     that this function returns will be the one specified with the
     ``name:`` keyword, regardless of whether that is the canonical name
     or not.

.. generic-function:: host-address
   :open:

   :signature: host-address *internet-address* => *address*

   :description:

     Returns an instance of ``<string>`` containing the presentation form of
     the host address. In the case of multi-homed hosts this will usually be
     the same as:

     .. code-block:: dylan

       multi-homed-internet-address.all-addresses.first.host-address

     In the case of an Internet address created using the ``address:`` keyword
     it will be either the keyword value or
     ``all-addresses.first.host-address``.

.. generic-function:: numeric-host-address
   :open:

   Returns the host address as a :class:`<numeric-address>`.

   :signature: numeric-host-address *internet-address* => *numeric-address*

.. generic-function:: all-addresses
   :open:

   :signature: all-addresses *internet-address* => *sequence*

   :description:

     Returns an instance of ``<sequence>`` whose elements are
     :class:`<internet-address>` objects containing all known addresses
     for the host.

.. generic-function:: aliases
   :open:

   :signature: aliases *internet-address* => *sequence*

   :description:

     Returns an instance of ``<sequence>`` whose elements are instances
     of ``<string>`` representing alternative names for the host.

.. constant:: $loopback-address

   :type: <internet-address>

   :description:

     An instance of :class:`<internet-address>` representing the
     loopback address: "127.0.0.1".

.. constant:: $local-host

   :type: <internet-address>

   :description:

     An instance of :class:`<internet-address>` representing the host on
     which the application using sockets is correctly running.

     Note that this value is not necessarily the same as would be
     created by the expression

     .. code-block:: dylan

       make (<internet-address>, name: "localhost")

     The address assigned to the symbolic name *localhost* is dependent
     on the configuration of DNS. In some cases this may be configured
     to be the loopback address rather than a real address for the local
     host.

The <IPV6-ADDRESS> class
^^^^^^^^^^^^^^^^^^^^^^^^

This name is reserved for future development.

The <NUMERIC-ADDRESS> class
^^^^^^^^^^^^^^^^^^^^^^^^^^^

This section describes numeric Internet representation and associated
protocols.

.. class:: <numeric-address>
   :sealed:
   :abstract:
   :primary:

   :superclasses: <object>

   :description:

     The class of objects representing the numeric form of an Internet
     addresses.

     Currently only ipv4 (32-bit) addresses are supported. Ipv6
     addresses will be added when they are supported by Winsock2. In
     general ``<numeric-address>`` objects are accessed using the
     functions :gf:`host-order` or :gf:`network-order`, depending on the
     context in which they are employed.

.. generic-function:: network-order
   :sealed:

   :signature: network-order *address* => *network-order-address*

   :description:

     Returns the value of the numeric address in network order. The argument
     is a general instance of :class:`<numeric-address>`. The class of the object
     returned depends upon the particular subclass of the argument; the
     ``network-order`` method for :class:`<ipv4-numeric-address>` returns an instance
     of :class:`<machine-word>`.

     *Network order* is big-endian byte order.

.. generic-function:: host-order
   :sealed:

   :signature: host-order *address* => *host-order-address*

   :description:

     Like :gf:`network-order` but returns the value in host order.

     *Host order* is either big-endian byte order on a big-endian host
     machine and little-endian on a little-endian host machine.

IPV4 addresses
^^^^^^^^^^^^^^

.. class:: <ipv4-numeric-address>
   :open:
   :abstract:
   :primary:
   :instantiable:

   :superclasses: :class:`<numeric-address>`

   :keyword value: An instance of :class:`<machine-word>`. Required.
   :keyword order: One of ``#"network-order"`` or ``#"host-order"``. Required.

   :description:

     The single slot of this class contains a 32-bit value representing
     a ipv4 address. This slot is accessed by the generic functions
     :gf:`network-order` and :gf:`host-order` described above.
     ``<ipv4-numeric-address>`` has two concrete subclasses
     :class:`<ipv4-network-order-address>` and
     :class:`<ipv4-host-order-address>`. Make ``<ipv4-numeric-address>``
     returns one or the other of these depending upon the value of the
     ``order:`` keyword.

.. method:: host-order
   :specializer: <ipv4-numeric-address>

   :signature: host-order *ip4-numeric-address* => *machine-word*

   :description:

     Returns the numeric address in host order as an instance of
     :class:`<machine-word>`. The argument is an instance of
     :class:`<ip4-numeric-address>`.

.. method:: network-order
   :specializer: <ipv4-numeric-address>

   :signature: network-order *ipv4-numeric-address* => *machine-word*

   :description:

     Returns the numeric address in network order as an instance of
     :class:`<machine-word>`. The argument is an instance of
     :class:`<ip4-numeric-address>`.

.. method:: as
   :specializer: <string>, <ipv4-numeric-address>

   Returns the presentation (dotted string) form of an instance of
   :class:`<ip4-numeric-address>`.

   :signature: as *string* *ipv4-numeric-address* => *string*

.. class:: <ipv4-network-order-address>
   :sealed:
   :concrete:

   Concrete subclass for network-order numeric addresses.

   :superclasses: :class:`<ipv4-numeric-address>`

   :description:

     .. code-block:: dylan

       make(<ipv4-network-order-address>)

     is equivalent to

     .. code-block:: dylan

       make(<ipv4-numeric-address>, order: network-order)

.. class:: <ipv4-host-order-address>
   :sealed:
   :concrete:

   Concrete subclass for host order numeric addresses.

   :superclasses: :class:`<ipv4-numeric-address>`

Sockets
-------

This section describes socket classes and protocols.

The <ABSTRACT-SOCKET> class
^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. class:: <abstract-socket>
   :open:
   :abstract:
   :uninstantiable:
   :free:

   :superclasses: <object>

   :keyword socket-descriptor: A Windows handle or UNIX fd (file
     descriptor) for the socket. In general users of the sockets API
     should not need to use this keyword. Only implementors of new socket
     classes should be interested.

   :description:

   The common superclass of all socket objects including
   :class:`<socket>` (IP client socket), :class:`<server-socket>` and
   :class:`<socket-accessor>`.

Each subclass of :class:`<abstract-socket>` must provide methods for :gf:`close`
and for the following generic functions:

.. generic-function:: local-port
   :open:

   Returns the local port number.

   :signature: local-port *socket* => *port-number*

   :parameter socket: An instance of :class:`<socket>`,
     :class:`<datagram-socket>` or :class:`<server-socket>`.
   :value port-number: An instance of ``<integer>``.

.. generic-function:: socket-descriptor
   :open:

   Returns the descriptor (handle or fd) for the socket.

   :signature: socket-descriptor *socket* => descriptor

   :parameter socket: An instance of :class:`<abstract-socket>`.
   :value descriptor: An instance of :class:`<accessor-socket-descriptor>`.

.. generic-function:: local-host
   :open:

   Returns the address of the local host.

   :signature: local-host *socket* => *host-address*

   :parameter socket: An instance of :class:`<abstract-socket>`.
   :value host-address: An instance of :class:`<internet-address>`.

The <SERVER-SOCKET> class
^^^^^^^^^^^^^^^^^^^^^^^^^

.. class:: <server-socket>
   :open:
   :abstract:
   :primary:
   :instantiable:

   :superclasses: :class:`<abstract-socket>`

   :keyword service: An instance of ``<string>`` containing an abstract
     name for a service with a “well-known” port, such as ``"ftp"`` or
     ``"daytime"``. Valid names depend on the configuration of the DNS.
     Required unless ``port:`` is supplied.
   :keyword port: An instance of ``<integer>`` identifying the port on
     which the ``<server-socket>`` should listen for connection requests.
     Required unless ``service:`` is supplied.
   :keyword protocol: An instance of ``<string>`` naming the protocol.
     Currently ``"tcp"`` is the only supported protocol. You can create
     instances of protocol-specific subclasses as an alternative to using
     the ``protocol:`` keyword. For example, ``make(<server-socket>,
     protocol: "tcp", …)`` is equivalent to ``make(<tcp-server-socket>,
     …)``.

   :description:

     Server-sockets listen on a specified port for connection requests
     which come in over the network. Either the ``port:`` or
     ``service:`` keyword must be supplied.

     :drm:`make` on ``(<server-socket>)`` returns an instance of
     :class:`<tcp-server-socket>` by default.

.. generic-function:: accept
   :open:

   :signature: accept *server-socket* #rest *args* #key => *result*

   :description:

     Blocks until a connect request is received, then it returns a
     connected instance of :class:`<socket>`. The particular subclass of
     :class:`<socket>` returned depends on the actual class of the
     argument, which must be a general instance of
     :class:`<server-socket>`. Calling accept on
     :class:`<tcp-server-socket>` returns a connected
     :class:`<tcp-socket>`. The keyword arguments are passed to the
     creation of the :class:`<socket>` instance. For UDP sockets
     *accept* returns immediately with an instance of
     :class:`<udp-socket>`. No blocking happens for UDP sockets because
     they are connectionless. After reading from a UDP socket returned
     from ``accept`` the socket can be interrogated for the location of
     the sender using :gf:`remote-host` and :gf:`remote-port`.

.. macro:: with-server-socket

   :macrocall:
     .. code-block:: dylan

       with-server-socket (*server-var* [:: *server-class* ], *keywords*)
         *body*
       end;

   :description:

     Creates an instance of :class:`<server-socket>`, using the
     (optional) *server-class* argument and keyword arguments to make
     the :class:`<server-socket>`, and binds it to the local variable
     named by *server-var*. The *body* is evaluated in the context of
     the binding and the ``<server-socket>`` is closed after the body is
     executed.

.. macro:: start-server

   :macrocall:
     .. code-block:: dylan

       start-server ([*server-var* = ]*socket-server-instance*,
           *socket-var* [, *keywords* ])
         *body*
       end;

   :description:

     Enters an infinite ``while(#t) accept`` loop on the server socket.
     Each time accept succeeds the :class:`<socket>` returned from
     accept is bound to *socket-var* and the *body* is evaluated in the
     context of the binding. When *body* exits, :gf:`accept` is called
     again producing a new binding for *socket-var*. The optional
     keywords are passed to the call to :gf:`accept`.

The <TCP-SERVER-SOCKET> class
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. class:: <tcp-server-socket>

   :superclass: <server-socket>

   :keyword element-type: Establishes a new default for the *element-type* of
      :class:`<TCP-socket>` instances returned by calling :gf:`accept` with this
      server socket as the argument to :gf:`accept`. This default
      *element-type* may be overridden for any particular call to :gf:`accept`
      by using the ``element-type:`` keyword to :gf:`accept`. If no
      ``element-type:`` is specified when the server socket is created,
      :class:`<byte-character>` is used as the default *element-type*.

   :description:

     The class of TCP server sockets. A server socket is an object which
     listens for requests for connections from the network. When accept is
     called on the server socket and a request for connection is detected,
     accept returns a connected :class:`<socket>`.

.. method:: accept
   :specializer: <tcp-server-socket>

   :signature: accept *server-socket* #rest *args* #key *element-type* => *connected-socket*

   :parameter server-socket: An instance of :class:`<tcp-server-socket>`.
   :parameter #key element-type: Controls the element type of the
     :class:`<tcp-socket>` (stream) returned. If not supplied, defaults
     to ``#f``.
   :value connected-socket: A connected instance of :class:`<tcp-socket>`.

   :description:

     The other keyword arguments are passed directly to the creation of
     the :class:`<tcp-socket>` instance.

The <SOCKET> class
^^^^^^^^^^^^^^^^^^

.. class:: <socket>
   :open:
   :abstract:
   :free:
   :instantiable:

   The class of general client sockets. All client sockets are streams.

   :superclasses: :class:`<abstract-socket>`, :class:`<external-stream>`

   :keyword direction: Specifies the direction of the stream. It must be
     one of ``#"input"``, ``#"output"``, and ``"#input-output"``. This
     keyword is an inherited streams class keyword. See the Streams
     library documentation in the *System and I/O* library reference for a
     full description.
   :keyword element-type: An instance of ``<class>``. Useful values are
     :class:`<byte-character>` and ``<byte>``. This keyword is an
     inherited streams class keyword. See the Streams library
     documentation in the *System and I/O* library reference for a full
     description.

The <BUFFERED-SOCKET> class
^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. class:: <buffered-socket>

   :superclasses: :class:`<socket>`, :class:`<double-buffered-stream>`


   :keyword force-output-before-read?: An instance of ``<boolean>``.
     Defaults value: ``#t``. The methods which implement the stream
     reading protocols (:gf:`read`, :gf:`read-line`, :gf:`read-element`
     and so on) for instances of :class:`<socket>` call :gf:`force-output`
     by default before blocking. This is to ensure that any pending output
     has been sent to the peer before the socket blocks waiting to read
     data sent by the peer. This corresponds to the expected, usual
     behavior of single-threaded client sockets and avoids deadlock in
     usual cases. Multi-threaded applications, particularly applications
     where one thread is reading and another thread is writing to the same
     socket, may wish to inhibit the default :gf:`force-output`. If the
     socket is created with ``force-output-before-read?:`` as ``#f``,
     :gf:`force-output` will not be called before the read functions
     block.

   :description:

     Socket streams whose elements are bytes or characters. These
     inherit buffering protocols and the implications of :gf:`read`,
     :gf:`write`, :gf:`read-element`, :gf:`write-element`,
     :gf:`force-output` and suchlike methods from
     :class:`<double-buffered-stream>`.

The <TCP-SOCKET> class
^^^^^^^^^^^^^^^^^^^^^^

The class of TCP client sockets.

.. class:: <tcp-socket>

   The class of TCP client sockets.

   :superclasses: :class:`<buffered-socket>`

   :keyword host: An instance of :class:`<internet-address>` or
     ``<string>``. The remote host to connect to. The ``<string>`` may be
     either a host name or a presentation-form Internet address. Required.
   :keyword service: An instance of ``<string>``. A ``<string>``
     containing an abstract name for a service with a “well-known“ port,
     such as ``"ftp"`` or ``"daytime"``. Valid names depend on the
     configuration of the DNS. Required unless ``port:`` is supplied.
   :keyword protocol: An instance of ``<string>`` naming the protocol.
     Currently ``#"tcp"`` and ``#"udp"`` are the only supported protocols.
     You can create instances of protocol-specific subclasses as an
     alternative to using the ``protocol:`` keyword. For example
     ``make(<socket>, protocol: #"tcp", …)`` is equivalent to
     ``make(<TCP-socket>, …)``. :drm:`make` on :class:`<socket>` returns
     an instance of ``<tcp-socket>`` by default.
   :keyword port: An instance of ``<integer>`` representing the remote
     port to connect to. Required unless ``service:`` is supplied.
   :keyword element-type: An instance of ``<class>``. Useful values for
     :class:`<tcp-streams>` are :class:`<byte-character>`` and ``<byte>``.
     This keyword is an inherited streams class keyword. See
     :doc:`../io/streams` for a full description.

.. generic-function:: remote-port
   :open:

   Returns the remote port number for a :class:`<socket>`.

   :signature: remote-port *socket* => *port-number*

   :parameter socket: An instance of :class:`<socket>`.
   :value port-number: An instance of ``<integer>``.

.. generic-function:: remote-host
   :open:

   Returns the remote host for a :class:`<socket>`.

   :signature: remote-host *socket* => *remote-host-address*

   :parameter socket: An instance of :class:`<socket>`.
   :value remote-host-address: An instance of :class:`<internet-address>`.

The <UDP-SOCKET> class
^^^^^^^^^^^^^^^^^^^^^^

The class of UDP client sockets.

.. class:: <udp-socket>

   The class of UDP client sockets.

   :superclasses: :class:`<buffered-socket>`

   :keyword host: An instance of :class:`<internet-address>` or
     ``<string>``. The remote host to connect to. The ``<string>`` may be
     either a host name or a *presentation-form* Internet address.
     Required.
   :keyword service: An instance of ``<string>``. A ``<string>``
     containing an abstract name for a service with a “well-known port”,
     such as ``"ftp"`` or ``"daytime"``. Valid names depend on the
     configuration of the DNS. Required unless ``port:`` is supplied.
   :keyword protocol: An instance of ``<string>`` naming the protocol.
     Currently ``#"tcp"`` and ``#"udp"`` are the only supported protocols.
     You can create instances of protocol-specific subclasses as an
     alternative to using the ``protocol:`` keyword. For example
     ``make(<socket>, protocol: "udp", …)`` is equivalent to
     ``make(<UDP-socket>, …)``. :drm:`make` on :class:`<socket>` returns
     an instance of :class:`<tcp-socket>` by default.
   :keyword port: An instance of ``<integer>`` representing the remote
     port to connect to. Required unless ``service:`` is supplied.
   :keyword element-type: An instance of ``<class>``. Useful values for
     ``<udp-socket>`` s are ``<byte-character>`` and ``<byte>``. This
     keyword is an inherited streams class keyword. See :doc:`../io/streams` for
     a full description.

   :description:

     Of the keywords, ``host:`` and one of either ``service:`` or
     ``port:`` are required.

The <UDP-SERVER-SOCKET> class
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The class of UDP server sockets.

.. class:: <udp-server-socket>

   :superclass: <server-socket>

   :keyword element-type: Establishes a new default for the element-type of
      :class:`<UDP-socket>` instances returned by calling :gf:`accept` with this
      server socket as the argument to :gf:`accept`. This default element-type
      may be overridden for any particular call to :gf:`accept` by using the
      ``element-type:`` keyword to :gf:`accept`. If no ``element-type:`` is
      specified when the server socket is created, :class:`<byte-character>` is
      used as the default element-type.

   :description:

     The class of UDP server sockets. A server socket is an object that
     listens for requests from the network. When :gf:`accept` is called
     on the UDP server socket, :gf:`accept` returns a
     :class:`<udp-socket>`.

Socket conditions
-----------------

This section lists the socket condition classes in the Network library.

.. class:: <socket-condition>

   All socket conditions are general instances of ``<socket-condition>``.
   Some are recoverable and others are not.

   :superclasses: :class:`<simple-condition>`

   :description:

     The class of socket conditions. It inherits the ``format-string:``
     and ``format-arguments:`` keywords from
     :class:`<simple-condition>`.

     Slots:

     *socket-condition-details*

     -  Most socket conditions originate in error return codes from Open
        Dylan’s Winsock2 library, an FFI interface to the native socket
        library Winsock2.
     -  The *socket-condition-details* slot provides information about the
        low-level failure which was the source for the condition. In most
        cases this slot will hold an instance of
        ``<socket-accessor-condition>``, below.
     -  When creating general instances of ``<socket-condition>``, you can use
        the *details:* keyword to set the value for this slot.

.. class:: <socket-error>

   The class ``<socket-error>`` is the superclass of all unrecoverable socket
   conditions.

   :superclasses: :class:`<socket-condition>`

   The class of socket conditions from which no recovery is possible.

.. class:: <internal-socket-error>

   The class ``<internal-socket-error>`` is the class of unexpected
   socket errors.

   :superclasses: :class:`<socket-error>`

   :description:

     The class of unexpected errors from Open Dylan’s Winsock2 library,
     an FFI interface to the native socket library Winsock2.

     Inspect the contents of the ``socket-condition-details`` slot for
     more information.

.. class:: <recoverable-socket-condition>

   The ``<recoverable-socket-condition>`` class is the general class of
   socket conditions for which an application may be able to take some
   remedial action.

   :superclasses: :class:`<socket-condition>`

   :description:

     The general class of socket conditions for which an application may be
     able to take some remedial action.

     For instance, a web browser receiving such conditions as
     :class:`<connection-refused>` or :class:`<host-not-found>` would
     normally expect to report those conditions to the user and continue
     with some other connection request from the user, while a server
     receiving a :class:`<connection-closed>` condition from a connected
     :class:`<socket>` would probably close the :class:`<socket>` and
     continue to handle other requests for connections.

.. class:: <network-not-responding>

   The network — probably a local network — is down. Try again later.

   :superclasses: :class:`<recoverable-socket-condition>`

.. class:: <invalid-address>

   A badly formed address string has been passed to a function trying to
   make an `<internet-address>`.

   :superclasses: :class:`<recoverable-socket-condition>`

.. class:: <host-not-found>

   The Domain Name Server (DNS) cannot resolve the named host or
   internet address. Try again with a different (correct) name or
   address.

   :superclasses: :class:`<recoverable-socket-condition>`

.. class:: <server-not-responding>

   The Domain Name Server (DNS) did not respond or returned an ambiguous
   result. Try again.

   :superclasses: :class:`<recoverable-socket-condition>`

.. class:: <host-unreachable>

   The remote host cannot be reached from this host at this time.

   :superclasses: :class:`<recoverable-socket-condition>`

.. class:: <socket-closed>

   :superclasses: :class:`<recoverable-socket-condition>`

   :description:

     The socket or server socket has been closed.

     Most operations on closed instances of :class:`<tcp-socket>``
     return instances of :class:`<stream-closed-error>` (from the
     Streams library) rather than instances of :class:`<socket-closed>`.

.. class:: <connection-failed>

   :superclasses: :class:`<recoverable-socket-condition>`

   :description:

     The attempt to connect to the remote host was not successful.
     Connection failed for one of the following reasons: because the
     connect request timed out or because it was refused, or because the
     remote host could not be reached.

.. class:: <connection-closed>

   :superclasses: :class:`<recoverable-socket-condition>`

   :description:

     The connection to the remote host has been broken. The socket
     should be closed. To try again, open a new socket.

.. class:: <address-in-use>

   :superclasses: :class:`<recoverable-socket-condition>`

   :description:

     A process on the machine is already bound to the same fully
     qualified address. This condition probably occurred because you
     were trying to use a port with an active server already installed,
     or a process crashed without closing a socket.

.. class:: <blocking-call-interrupted>

   A blocking socket call, like :gf:`read`, :gf:`write` or :gf:`accept`,
   was interrupted.

   :superclasses: :class:`<recoverable-socket-condition>`

.. class:: <out-of-resources>

   :superclasses: :class:`<recoverable-socket-condition>`

   :description:

     The implementation-dependent limit on the number of open sockets
     has been reached. You must close some sockets before you can open
     any more. The limits for Windows NT (non-server machines) and
     Windows 95 are particularly small.

.. class:: <socket-accessor-error>

   :superclasses: :class:`<socket-error>`

   :description:

     An implementation-specific error from the C-FFI interface to the
     native socket library. Usually instances of this class these appear
     in the ``socket-condition-details`` slot of another
     :class:`<socket-condition>`.

.. class:: <win32-socket-error>

   :superclasses: :class:`<socket-accessor-error>`

   :description:

     A Win32-specific error from the Winsock2 library, a C-FFI interface to
     the native socket library Winsock2. A function in the FFI library has
     returned an error return code.

     Slots:

     *WSA-numeric-error-code*
       Contains the numeric error code that was returned. An instance of
       ``<integer>``.

     *WSA-symbolic-error-code*
       Contains an instance of ``<string>`` giving the symbolic
       (human-readable) form of the error code. For example, the string
       might be *"wsanotsock"*.

     *explanation*
       An explanation if any of the error. An instance of ``<string>``.

     *calling-function*
       The name of Winsock2 FFI interface function which returned the
       error code. An instance of ``<string>``.
