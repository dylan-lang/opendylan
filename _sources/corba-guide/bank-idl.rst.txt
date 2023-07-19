*************************
Writing and Compiling IDL
*************************

Writing IDL for a CORBA application
===================================

The first step in developing a CORBA application is to define the
interfaces to its distributed application objects. You define these
interfaces using OMG’s Interface Definition Language (IDL).

Essentially, the IDL specification of an interface lists the names and
types of operations that:

-  Any CORBA object, satisfying that interface, must support.
-  Any CORBA client application, targeting such an object, may request.

Our bank server application manages three types of CORBA object,
representing accounts, checking accounts, and banks. We declare the
interfaces to all three objects within the same CORBA module,
``BankingDemo``:

.. code-block:: idl

   module BankingDemo {
     interface account {
       // details follow
     };

     interface checkingAccount : account {
       // details follow
     };

     interface bank {
       // details follow
     };
   };

The following subsections describe the IDL declarations for each of
the three interfaces. You can find the complete IDL description for
the bank demo in the Open Dylan Examples folder, under
:file:`Examples\\corba\\bank\\bank\\bank.idl`.

IDL for the account interface
-----------------------------

This is the IDL definition of the interface to an ``account`` object.

.. code-block:: idl

   // in module BankingDemo
   interface account {
     readonly attribute string name;

     readonly attribute long balance;

     void credit (in unsigned long amount);

     exception refusal {string reason;};

     void debit (in long amount)
       raises (refusal);
   };

An ``account`` object’s ``name`` attribute is used to store the name
of the account holder. The state of an account is recorded in the
``balance`` attribute. To keep things simple, we use CORBA ``long``
values to represent the monetary amounts that we use to store account
balances and to modify them with credits and debits.

To prevent clients from directly altering the account’s ``name`` or
``balance``, these attributes are declared as ``readonly``
attributes. The operations ``credit`` and ``debit`` are provided to
allow updates to an account’s ``balance`` attribute.

The operation ``credit`` adds a non-negative amount to the current
account balance.

Next is an exception declaration:

.. code-block:: idl

   exception refusal {string reason;}; 

This declares a named exception, ``refusal``, that the ``debit``
operation uses to signal errors. The ``refusal`` exception is declared
to contain a ``reason`` field that documents the reason for failure
using a ``string``.

The operation ``debit`` subtracts a given amount from the current
account balance, as long as this does not make the account balance
negative. Qualifying ``debit`` by the phrase ``raises (refusal)``
declares that invoking this operation might raise the exception
``refusal``. This phrase is necessary because although a CORBA
operation may raise any CORBA system exception, its declaration must
specify any additional user-defined CORBA exceptions that it might
raise.

This completes the IDL declaration of the ``account`` interface.

IDL for the checkingAccount interface
-------------------------------------

Our application manages a second sort of bank account, called a
checking account . While an ordinary ``account`` must maintain a
positive balance, a ``checkingAccount`` may be overdrawn up to an
agreed limit. We use IDL’s notion of interface inheritance to capture
the intuition that a checking account is just a special form of
``account``:

.. code-block:: idl

   // in module BankingDemo
   interface checkingAccount : account {
     readonly attribute long limit;
   };

The declaration ``checkingAccount : account`` specifies that the
interface ``checkingAccount`` inherits all the operations and
attributes declared in the ``account`` interface. The body of the
definition states that a ``checkingAccount`` also supports the
additional ``limit`` attribute.

The fact that ``checkingAccount`` inherits some operations from
``account`` does not imply that the methods implementing those
operations need to be inherited too. We will exploit this flexibility
to provide a specialized ``debit`` method for ``checkingAccount``\ s.

IDL for the bank interface
--------------------------

We can now design the interface of a bank object. The intention is
that a bank associates customer names with accounts, with each name
identifying at most one account. A client is able to open accounts for
new customers and to retrieve both accounts and checking accounts for
existing customers from the persistent store. If the client attempts
to open a second account under the same name, the bank should refuse
the request by raising an exception. Similarly, if the client attempts
to retrieve an account for an unknown customer, the bank should reject
the request by raising an exception.

The IDL definition of the ``bank`` interface captures some of these
requirements:

.. code-block:: idl

   // in module BankingDemo
   interface bank {
     readonly attribute string name;

     exception duplicateAccount{};

     account openAccount (in string name)
       raises (duplicateAccount);

     checkingAccount openCheckingAccount(in string name,
                                         in long limit)
       raises (duplicateAccount);

     exception nonExistentAccount{};

     account retrieveAccount(in string name)
       raises (nonExistentAccount);

     void closeAccount (in account account);
   };

The name of a ``bank`` object is recorded in its ``name`` attribute.

The operation ``openAccount`` is declared to take a CORBA ``string``
and return an ``account``. Because ``account`` is defined as an
interface, and not a type, this means that the operation will return
a reference to an ``account`` object. This illustrates an important
distinction between ordinary values and objects in CORBA: while
members of basic and constructed types are passed by value, objects
are passed by reference.

The qualification ``raises (duplicateAccount)`` specifies that
``openAccount`` can raise the user-defined exception
``duplicateAccount``, instead of returning an account. (The exception
``duplicateAccount`` has no fields.)

The operation ``openCheckingAccount`` is similar to ``openAccount``,
but takes an additional argument, ``limit``, which represents the
account’s overdraft limit.

The operation ``retrieveAccount`` looks up the account (or checking
account), if any, associated with a customer ``name``, and returns an
object reference of interface ``account``. The operation can raise the
exception ``nonExistentAccount`` to indicate that there is no account
under the supplied name.

The last operation, ``closeAccount``, closes an ``account`` by
deleting it from the bank’s records.

Because ``checkingAccount`` inherits from ``account``, a
``checkingAccount`` object can be used wherever an ``account`` object
is expected, whether as the actual argument, or the result, of an
operation. For instance, we can use ``closeAccount`` to close
``checkingAccount`` objects as well as ``account`` objects, and we can
use ``retrieveAccount`` to fetch ``checkingAccount`` objects as well
as ``account`` objects.

Compiling IDL for a CORBA application
========================================

Open Dylan includes an IDL compiler, Scepter, that it uses to check
and compile IDL files into Dylan libraries. These libraries provide
essential infrastructure for CORBA-based applications in Dylan.

The libraries are built according to the specification in :doc:`idl-app`. That document is a draft specification for
a standard mapping from CORBA IDL to the Dylan language. Briefly, the
specification states that:

-  CORBA types are mapped to Dylan types and classes
-  CORBA interfaces are mapped to Dylan classes
-  CORBA interface inheritance is mapped to Dylan class inheritance
-  CORBA attributes are mapped to Dylan getter and setter functions
-  CORBA operations are mapped to Dylan generic functions
-  CORBA exceptions are mapped to Dylan conditions

IDL declarations are mapped to Dylan according to these rules. The
resulting libraries provide a Dylan protocol equivalent to the IDL.

Libraries created by compiling IDL
----------------------------------

The IDL compiler can produce *skeleton*, *stub*, and *protocol*
libraries from an IDL file. This, again, is as specified in
:doc:`idl-app`. The purpose of these libraries is to make writing
CORBA applications easier, by providing a pre-built interface to CORBA
operations.

The skeletons or *server skeletons* library contains code for use by a
CORBA server application, while the stubs or client stubs library
contains code for use by a CORBA client application. In both cases,
the code hides the details of CORBA communication from the
application, allowing you to invoke operations in other CORBA objects
without having to worry about where those objects are running. The
stubs and skeletons act as proxies for the real, remote operations.

The protocol library is a Dylan representation of the interface
described in the IDL file. The Dylan representation is mapped from IDL
according to the Dylan IDL binding, with open classes and open generic
functions representing IDL interfaces and operations. As we saw in
Chapter 2, “Quick Start Tutorial”, the protocol provides the basis for
implementing clients and servers. The skeletons and the stubs library
both use the protocol library and re-export the names from it.

.. note::
   Typically a server project uses the client stubs library in
   addition to the skeletons and protocol library. This allows the
   server to make callbacks to the client.

IDL files in Dylan projects
------------------------------

IDL files can be treated as part of Dylan projects, allowing the IDL
interface to be compiled at the same time as the project’s Dylan
sources. This is in fact the simplest way to manage a Dylan project
that uses CORBA facilities.

However, you do not include IDL files directly in the project. Rather,
each IDL file that a project depends on is represented in the project
by a corresponding Open Dylan Tool Specification (spec) file.

The spec file indicates the path to the IDL file, and states which of
the skeletons, stubs, and protocol libraries the project requires. The
Open Dylan development environment uses the spec file to invoke
the IDL compiler as part of the normal build process for the project.

The Bank-Client and Bank-Server projects each contain a spec file for
the :file:`bank.idl` file. The Bank-Client project’s spec file,
:file:`idl.spec`, requests that stubs and protocol libraries be
generated. The Bank-Server project’s spec file requests that
skeletons, stubs, and protocol libraries be generated. (This second
spec file is also called :file:`idl.spec` but is a different file to the
one in Bank-Client.)

After building both the bank client and the bank server, there will be
three new subfolders:

:file:`Examples\\corba\\bank\\bank\\stubs`
   Contains the project ``bank-stubs.hdp`` that defines the Bank-Stubs
   library. This library is used by the implementation of the bank
   client. Its project is automatically added to ``bank-client.hdp``
   as a subproject.

:file:`Examples\\corba\\bank\\bank\\skeletons`
   Contains the project ``bank-skeletons.hdp`` that defines the
   Bank-Skeletons library. This library is used by the implementation
   of the bank server. Its project is automatically added to
   ``bank-server.hdp`` as a subproject.

:file:`Examples\\corba\\bank\\bank\\protocol`
   Contains the project ``bank-protocol.hdp`` that defines the
   Bank-Protocol library. This library is shared by both the
   Bank-Skeletons and Bank-Stubs libraries, and added automatically to
   the projects of both.

Compilation steps
-----------------

If you have not built the Bank-Client and Bank-Server projects yet,
you should do so now.

#. Open the Bank-Client and Bank-Server projects from the CORBA
   section of the Examples dialog.

   You can bring up the Examples dialog by choosing
   :menuselection:`Help --> Open Example Project...`.

#. Choose :menuselection:`Project --> Build` in each project window.

   Notice how the stubs, skeletons, and protocol projects are
   generated and added automatically to the top-level client and
   server projects.

Mapping IDL to Dylan
====================

To get an impression of the mapping from IDL to Dylan, we can take a
look at the result of applying the mapping to the file
:file:`bank.idl`. The following Dylan definitions are taken from
:file:`bank-protocol.dylan`, part of the Bank-Protocol project
produced by compiling :file:`bank.idl` IDL:

.. code-block:: dylan

   define open abstract class BankingDemo/<account> (<object>)

   end class;

   define open generic BankingDemo/account/name
       (object :: BankingDemo/<account>)
    => (result :: CORBA/<string>);

   define open generic BankingDemo/account/balance
       (object :: BankingDemo/<account>)
    => (result :: CORBA/<long>);

   define open generic BankingDemo/account/credit
       (object :: BankingDemo/<account>,
        amount :: CORBA/<unsigned-long>)
    => ();

   define sealed class BankingDemo/account/<refusal>
       (CORBA/<user-exception>)
     slot BankingDemo/account/refusal/reason :: CORBA/<string>,
       required-init-keyword: reason:;
   end class;


   define open generic BankingDemo/account/debit
       (object :: BankingDemo/<account>, amount :: CORBA/<long>)
    => ();

   define open abstract class BankingDemo/<checkingAccount>
       (BankingDemo/<account>)
   end class;

   define open generic BankingDemo/checkingAccount/limit
       (object :: BankingDemo/<checkingAccount>)
    => (result :: CORBA/<long>);

   define open abstract class BankingDemo/<bank> (<object>)
   end class;

   define open generic BankingDemo/bank/name
       (object :: BankingDemo/<bank>)
    => (result :: CORBA/<string>);

   define sealed class BankingDemo/bank/<duplicateAccount>
       (CORBA/<user-exception>)
   end class;

   ...

To provide some more intuition for the mapping scheme, the following
subsections examine the Dylan counterparts of some of the more
representative IDL declarations from the file :file:`bank.idl`. See
:doc:`idl-app` for the full mapping description.

Mapping for interfaces 
-------------------------

The IDL interfaces ``account``, ``checkingAccount`` and ``bank`` map
to the Dylan abstract classes ``BankingDemo/<account>``,
``BankingDemo/<checkingAccount>`` and ``BankingDemo/<bank>`` .

Dylan does not support nested namespaces, so in Dylan, the IDL scope
identifier ``BankingDemo`` is prefixed to the name of each interface
defined within its scope. This is how we get the Dylan class
identifiers ``BankingDemo/<account>``,
``BankingDemo/<checkingAccount>`` and ``BankingDemo/<bank>``.

Notice how IDL interface inheritance (``checkingAccount : account``)
maps naturally onto Dylan class inheritance: the class
``BankingDemo/<checkingAccount>`` is defined as a subclass of
``BankingDemo/<account>``).

Mapping for basic types
-----------------------

The IDL types ``string``, ``long``, and ``unsigned long`` are mapped
to the Dylan classes ``CORBA/<string>``, ``CORBA/<long>`` and
``CORBA/<unsigned-long>``, which are simply aliases for the Dylan
classes :drm:`<string>`, :drm:`<integer>` and a positive subset of
:drm:`<integer>`.

Mapping for attributes
----------------------

The read-only ``balance`` attribute of an IDL ``account`` gives rise
to the Dylan generic function:

.. code-block:: dylan

   define open generic BankingDemo/account/balance
       (object :: BankingDemo/<account>)
    => (result :: CORBA/<long>);

If we had omitted the ``readonly`` keyword from the definition of the
``balance`` attribute, the mapping would have introduced an
additional generic ``-setter`` function:

.. code-block:: dylan

   define open generic BankingDemo/account/balance-setter
       (value :: CORBA/<long>, object :: BankingDemo/<account>)
    => (value :: CORBA/<long>);

Recall that, in the IDL source, the ``balance`` attribute is declared
within the definition, and thus the subordinate namespace, of the
``BankingDemo`` module and the ``account`` interface. Again, because
Dylan does not support nested namespaces, the IDL scope identifiers
``BankingDemo`` and ``account`` are simply prefixed to the name of the
attribute’s getter method, resulting in the Dylan function identifier
``BankingDemo/account/balance``.

.. note::
   More generally, the Dylan language IDL binding specifies that an
   IDL identifier is mapped to a Dylan identifier by appending
   together all the enclosing scope identifiers and the scoped
   identifier itself, separating the identifiers by forward slashes
   (``/``).




Mapping for operations
----------------------

The IDL operation ``credit`` is mapped to the Dylan generic function:

.. code-block:: dylan

   define open generic BankingDemo/account/credit
       (object :: BankingDemo/<account>,
        amount :: CORBA/<unsigned-long>)
    => ();


In IDL, the ``credit`` operation is defined within the ``account``
interface, declaring it to be an operation on ``account`` objects. The
Dylan language IDL binding adopts the convention that an operation’s
target object should be passed as the first argument of the
corresponding Dylan generic function. Thus the first parameter of the
generic function ``BankingDemo/account/credit`` is an object of class
``BankingDemo/<account>``.

The operation’s ``in`` and ``inout`` arguments become the remaining
parameters of the corresponding Dylan generic function. In this case,
the ``credit`` operation specifies a single ``in`` parameter, ``in
unsigned long amount``, that determines the second and only other
parameter, ``amount :: CORBA/<long>``, of the
``BankingDemo/account/credit`` generic function.

The operation's result type and any other parameters declared as
``out`` or ``inout`` become results of the corresponding Dylan
generic function. In this case, because the result type of ``credit``
is ``void``, and the operation has no ``out`` or ``inout``
parameters, ``BankingDemo/account/credit`` has an empty result list.

Mapping for exceptions
----------------------

The IDL exception ``refusal`` maps onto the Dylan class
``BankingDemo/account/<refusal>`` . Its member, ``string reason;``,
maps onto a slot ``BankingDemo/account/refusal/reason ::
CORBA/<string>``.

Note that ``BankingDemo/account/<refusal>`` is a subclass of
``CORBA/<user-exception>`` and, by inheritance, of Dylan
``<condition>``. This means that CORBA user exceptions can be raised
on the server, and handled in the client, using the standard Dylan
condition mechanism.
