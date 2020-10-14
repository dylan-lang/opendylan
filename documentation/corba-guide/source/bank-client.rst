***************
The Bank Client
***************


The bank client
===============

In this chapter, we design and implement a CORBA client, using Dylan
and the Open Dylan ORB.

Our client presents a graphical user interface to a bank object and
its operations. We implement the user interface using DUIM, Open
Dylan’s graphical interface programming toolkit. Since the primary
motivation for this tutorial is to illustrate the use of CORBA, we
focus less on the design of the graphical interface, and more on the
method for interacting with CORBA objects.


The client’s perspective
========================

From the client’s perspective, the IDL definition of a bank’s
interface, together with some documentation, fully determines its
functionality. This means that in writing the client we need only rely
on the information in the documented IDL to be able to interact with a
bank object. Knowing the IDL description, we can implement the client
before our bank object implementation is available.

The Bank-Protocol library, which was produced by the IDL compiler,
Scepter, merely specifies the protocol for interacting with CORBA
objects satisfying the interfaces in the IDL file
:file:`bank.idl`. The client-side implementation of this protocol
resides the Bank-Stubs library. Any application that wants to act as a
client with respect to some CORBA object matching an interface in the
:file:`bank.idl` file should use the Bank-Stubs library.

The Bank-Stubs library defines the following concrete classes::

   BankingDemo/<account-reference>
   BankingDemo/<checkingAccount-reference>
   BankingDemo/<bank-reference>

These classes subclass the following abstract classes::

   BankingDemo/<account>
   BankingDemo/<checkingAccount>
   BankingDemo/<bank>

The class ``BankingDemo/<checkingAccount-reference>`` is defined to
inherit from ``BankingDemo/<account-reference>``, matching the
inheritance relationship in the IDL. Instances of these classes act as
proxies for CORBA objects running on the server.

The Bank-Stubs library also defines a concrete stub method,
specialized on the appropriate proxy class, for each protocol function
stemming from an IDL attribute or operation. When the client applies
the generic function to a particular target proxy, the stub method
communicates with the ORB to invoke the corresponding operation on the
actual target object in the server. If the request succeeds, the stub
method returns the result to the client. If the request fails, raising
a CORBA user or system exception, the stub method raises the
corresponding Dylan condition of the appropriate class. This condition
can then be handled by the client code using standard Dylan
constructs.

Requirements for implementing the bank client
=============================================

There are two parts to implementing the bank client:

-  Write the code to initialize the CORBA ORB, and obtain a reference
   to a bank server object.

-  Write the code for the client’s GUI.

We start with the GUI implementation.

Implementing the bank client’s GUI
====================================

.. note::
   This section assumes some basic knowledge of DUIM (Dylan User
   Interface Manager), Open Dylan’s window programming toolkit. See
   the manual Building Applications Using DUIM for details. However,
   you do not need to know about DUIM to follow the rest of the
   tutorial.

Since this demonstration principally concerns CORBA, and because we
would like to revamp the look-and-feel of the demonstration
occasionally, we no longer describe the GUI implementation in great
detail. Instead only a brief outline of the current design is given.

The bank client consists of one window that shows a table of retrieved
accounts. Each row in the table shows the name, the current balance,
and the overdraft limit (if applicable). CORBA operations are mapped
on to menu items whose callbacks make the necessary requests.

The bank client is implemented as a library:

.. code-block:: dylan

   define library bank-client
     use common-dylan;
     use dylan-orb;
     use bank-stubs;
     use duim;
     // ...
   end library bank-client;

that defines a single module:

.. code-block:: dylan

   define module bank-client
     use common-dylan;
     use dylan-orb;
     use bank-stubs;
     use duim;
     // ...
   end module bank-client;

(See :file:`library.dylan` and :file:`module.dylan` in the Bank-Client
project.)

Any application that wants to use the Open Dylan ORB should use the
Dylan-ORB system library and module, in addition to any
application-specific libraries. Because our application acts as a
client of CORBA objects satisfying interfaces defined in the
:file:`bank.idl` file, it also needs to uses the Bank-Stubs library and
module. It also needs to use the DUIM library and module to construct
the graphical user interface.

The focal point of the bank client GUI is the ``<bank-frame>`` class
defined by DUIM’s ``frame-definer`` macro. This maintains a set of
account references and organizes the tabular layout of their details.

Defining the callbacks attached to each menu item is
straightforward. Recall that in DUIM, the argument passed to a
callback is the gadget whose activation triggered that callback, and
that the DUIM function ``sheet-frame`` can be used to return the
enclosing frame of a gadget.

The source code for the client GUI is in file :file:`bank-client.dylan`.

Implementing CORBA initialization for the bank client
=====================================================

Having written the client GUI we are now ready to set up the client’s
CORBA environment. A client can only communicate with a CORBA object
if it possesses a reference to that object. This raises the question
of how the client obtains its initial object reference. The fact that
some IDL operation may return an object reference is of no help here:
without a reference to specify as its target, there is no way to
invoke this operation.

In more detail, before a client can enter the CORBA environment, it
must first:

-  Be initialized into the ORB.
-  Get a reference to the ORB pseudo-object for use in future ORB operations.
-  Get an initial reference to an actual object on the server.

CORBA provides a standard set of operations, specified in pseudo IDL
(PIDL), to initialize applications and obtain the appropriate object
references.

Operations providing access to the ORB reside in the ``CORBA``
module. (Like an IDL interface declaration, an IDL (or PIDL) module
declaration defines a new namespace for the body of declarations it
encloses. What it does not do is define a new type of CORBA object.)
Operations providing access to CORBA features such as Object Adapters,
the Interface Repository, the Naming Service, and other Object
Services reside in the ``ORB`` interface defined within the ``CORBA``
module.

To provide some flavor of PIDL, here is a fragment of the PIDL
specification of ``CORBA`` that we rely on in our implementation of
the bank client.

.. code-block:: idl

   module CORBA {
     interface Object {
       boolean is_a (in string logical_type_id);
       // ...
     };

     interface ORB {
       string object_to_string (in Object obj);
       Object string_to_object (in string str);
       // ...
     };

     // ...

     typedef string ORBid;

     typedef sequence <string> arg_list;

     ORB ORB_init (inout arg_list argv, in ORBid orb_identifier);
   };


The ``Object`` interface is implicitly inherited by all IDL
interfaces, much as every Dylan class inherits from the class
:drm:`<object>`. The ``is_a`` operation provides a test for inheritance —
the ``logical_type_id`` is a string representation of an interface
identifier. The operation returns true if the object is an instance of
that interface, including if that interface is an ancestor of the most
derived interface of that object.

The ``ORB`` operations ``object_to_string`` and ``string_to_object``
provide an invertible mapping from object references to their
representations as strings.

Notice that the CORBA operation ``ORB_init`` is defined outside the
scope of any interface, providing a means of bootstrapping into the
CORBA world. Calling ``ORB_init`` initializes the ORB, returning an
``ORB`` pseudo object that can be used as the target for further ORB
operations.

Like most other language bindings, the Dylan binding adopts the pseudo
objects approach, in which these CORBA and ORB operations are accessed
by applying the binding’s normal IDL mapping rules to the PIDL
specification.

In this tutorial, as in the Hello World example of Chapter 2, the
client can obtain the initial object reference from a shared file, in
which the server has published a reference to its implementation of
the bank object, encoded as a string. After starting up, the client
reads the file, decodes the string into an object reference (using the
ORB utility operation ``file_to_object``, which in turn uses
``string_to_object``), and then uses this reference as the target of
further operations.

Alternatively, this demonstration can also use a Name Service to
communicate the initial bank reference between the client and
server. A Name Service acts as an intermediary, allowing the server to
register a reference against name , and then allowing the client to
query for the associated reference. To use the Name Service, pass
``-location-service:naming-service`` on the command line of the client.

To change the command line arguments given to the program, choose the
:menuselection:`Project --> Settings...` dialog and switch to the
:guilabel:`Debug` tab page. By default the command line arguments for
the Bank demo are::

   -ORBname-service-file c:\temp\ns.ior -location-service:shared-file

which tells the ORB where the Name Service is, but that it should use
a shared file to pass the initial Bank reference.

Here is some the Dylan code that implements the initialization of the client:

.. code-block:: dylan

   define method initialize-client ()
     let orb = CORBA/ORB-init(make(CORBA/<arg-list>), 
                              "Open Dylan ORB");
     let ls = get-location-service();
     block ()
       let bank = lookup-bank(orb, ls);
       let bank-frame = make(<bank-frame>, bank: bank);
       start-frame(bank-frame);
     exception (lookup-bank-failure(orb, ls))
       notify-user("Cannot locate the Bank. Click OK to Exit.");
     end block;
   end method;

This method first initializes the Open Dylan ORB by calling the Dylan
generic function ``CORBA/ORB-init`` corresponding to the PIDL
``ORB_init`` operation. (Note that the IDL module name ``CORBA`` forms
a prefix of the Dylan operation name, and that IDL underscore “``_``”
maps to a Dylan dash “``-``”.) The first argument to this call
evaluates to an empty ``CORBA/<arglist>``. Passing an empty sequence
instructs the ``CORBA/ORB-init`` function to ignore this argument and
use the application’s command line arguments (if any) instead. The
value of the second argument, ``"Open Dylan ORB"``, merely identifies
the ORB to use. The call returns an object of class ``CORBA/<ORB>``.

The function ``get-location-service`` reads the command line to see
whether to look for a shared file or use a Name Service . It then
passes this information to the function ``lookup-bank``, which knows
how to get a bank reference using either method. For example, for the
shared file case ``lookup-bank`` does the following:

.. code-block:: dylan

   define method lookup-bank (orb :: corba/<orb>,
                              location-service == #"shared-file")
    => (bank :: bankingdemo/<bank>)
     as(BankingDemo/<bank>, corba/orb/file-to-object(orb, $bank-ior-file))
   end method;

The constant ``$bank-ior-file`` is the name of the shared file used to
pass the reference of the bank object from the server to the client.

Invoking ``CORBA/ORB/file-to-object`` on this ORB, passing the shared
file name, reconstitutes the IOR string contained in the file as an
unspecific object reference of class ``CORBA/<Object>``. Calling the
``as`` method on this object reference narrows (that is, coerces) it
to a more specific object reference of class
``BankingDemo/<bank>``. (The ``as`` method, which is generated by the
IDL compiler and defined in the Bank-Stubs library, employs an
implicit call to the object’s ``is_a`` operation to check that the
desired coercion is safe.)

Finally, the resulting object reference ``bank``, of class
``BankingDemo/<bank>``, is used to make and start a new bank frame,
displaying the initial GUI to the user.

The full implementation of the client initialization can be found in
the file :file:`init-client.dylan`.

In this tutorial, as in the Hello World example of Chapter 2, the
client obtains the initial object reference from a shared file, in
which the server has published a reference to its implementation of
the bank object, encoded as a string. After starting up, the client
reads the file, decodes the string into an object reference (using the
operation ``string_to_object``), and then uses this reference as the
target of further operations.

Here is the remaining Dylan code that completes the implementation of
the client:

.. code-block:: dylan

   define constant $bank-ior-file = "bank-demo.ior";

   define method file-as-string (ior-file :: <string>)
     with-open-file(stream = ior-file, direction: #"input")
       as(<string>, read-to-end(stream))
     end
   end method;

   begin
     let orb = CORBA/ORB-init(make(CORBA/<arg-list>),
                              "Open Dylan ORB");
     let bank =
       as(BankingDemo/<bank>,
          CORBA/ORB/string-to-object(orb,
                                     file-as-string($bank-ior-file)));
     let bank-frame = make(<bank-frame>, bank: bank);
     start-frame(bank-frame);
   end;

The constant ``$bank-ior-file`` is the name of the shared file used to
pass the reference of the bank object from the server to the client.

The method ``file-as-string`` reads a file’s contents into a string.

The top-level ``begin ... end`` statement first initializes the Open
Dylan ORB by calling the Dylan generic function ``CORBA/ORB-init``
corresponding to the PIDL ``ORB_init`` operation. (Notice that the IDL
module name ``CORBA`` forms a prefix of the Dylan operation name, and
that IDL underscore (``_``) maps to a Dylan dash (``-``).) The first
argument to this call evaluates to an empty
``CORBA/<arglist>``. Passing an empty sequence instructs the
``CORBA/ORB-init`` function to ignore this argument and use the
application’s command line arguments (if any) instead. The value of
the second argument, ``"Open Dylan ORB"``, merely identifies the ORB
to use. The call returns an object of class ``CORBA/<ORB>``.

Invoking ``CORBA/ORB/string-to-object`` on this ORB, passing the
string read from the shared file, reconstitutes the string as an
unspecific object reference of class ``CORBA/<Object>``, using the
ORB operation ``string_to_object``. Calling the ``as`` method on this
object reference narrows (that is, coerces) it to a more specific
object reference of class ``BankingDemo/<bank>``. (The ``as`` method,
which is generated by the IDL compiler and defined in the Bank-Stubs
library, employs an implicit call to the object’s ``is_a`` operation
to check that the desired coercion is safe.)

Finally, the resulting object reference ``bank``, of class
``BankingDemo/<bank>``, is used to make and start a new bank frame,
displaying the initial GUI to the user.

The implementation of the client is now complete.
