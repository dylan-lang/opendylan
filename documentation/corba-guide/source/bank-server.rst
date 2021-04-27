***************
The Bank Server
***************

The server
==========

In this chapter, we design and implement a CORBA server, using Dylan
and the Open Dylan ORB.

Our server presents an object-oriented interface to a bank object and
its accounts. Because we want the bank’s account records to persist
beyond the lifetime of the server, we store the account records in a
Microsoft Access™ relational database. The server manipulates this
database using Open Dylan’s SQL-ODBC library.

Since the primary motivation for this tutorial is to illustrate the
use of CORBA, we will not spend too much time on the use of the
SQL-ODBC library. Instead, we concentrate on how to provide
implementations of CORBA objects, and how to make those implementation
available to the CORBA environment.

A very small amount of SQL knowledge is assumed in early sections of
this chapter.

The ODBC database
=================

The folder :file:`Examples\corba\bank\bank-server` contains a prepared
Microsoft Access database file :file:`bankDB.mdb` that the server uses
to record account details.

This database contains a single SQL table called ``Accounts``:

===== ======= =====
Name  Balance Limit
===== ======= =====
Jack  0       NULL
Jill  -100    200
===== ======= =====

The ``Accounts`` table has three columns: ``Name``, ``Balance``, and
``Limit``. Each record in the table represents an account.

- The ``Name`` column contains values of SQL type ``string`` that are
  used to uniquely identify account records.

- The ``Balance`` column contains values of SQL type ``long``,
  reflecting the current balance of each account.

- The ``Limit`` column contains either the distinguished SQL value
  NULL that indicates an absent value, or a value of SQL type
  ``long``, reflecting the overdraft limit of a checking account.

Note that both accounts and checking accounts are stored as records in
the same table. By convention, we interpret a record with a NULL
``Limit`` value an ordinary account. We interpret a record with a
non-NULL, and thus integral, ``Limit`` value as a checking account
with the given overdraft limit.

For instance, the ``Accounts`` table above contains two records, one
for Jack’s account and one for Jill’s checking account. (Recall that
checking accounts can have negative balances while ordinary accounts
cannot.)

Overview of the Open Dylan SQL-ODBC library
===========================================

The SQL-ODBC library is built on top of a generic SQL library. The SQL
library does not include the low-level code necessary to communicate
with any particular database management system (DBMS): it simply
provides a convenient high-level mechanism for integrating database
operations into Dylan applications.

The SQL library is designed to form the high-level part of
implementation libraries that contain lower-level code to support a
particular DBMS protocol, such as ODBC. The SQL-ODBC library is one
such implementation library.

The SQL library defines the following abstract classes:

- The abstract class ``<dbms>`` is used to identify a database
  management system.

  The SQL-ODBC library defines an instantiable subclass
  ``<odbc-dbms>`` of ``<user>`` for identifying the ODBC DBMS.

- The abstract class ``<user>`` identifies users to a DBMS.

  Exactly what a user means depends on the DBMS. The :drm:`make`
  method on ``<user>`` takes two keyword arguments, ``user:`` and ``password:``.
  The ``user:`` init-keyword takes an instance of :drm:`<string>`
  that should be a valid user name for the DBMS in question. The ``password:``
  init-keyword should be the password that accompanies the user name.

  The SQL-ODBC library defines the instantiable subclass ``<odbc-user>``
  of ``<user>`` for identifying a user to an ODBC DBMS.

- The abstract class ``<database>`` identifies a database to a DBMS.

  Exactly what a database is depends on the DBMS in question. The
  SQL-ODBC library defines the class ``<odbc-database>``, a subclass
  of ``<database>``, whose instances identify databases to an ODBC
  DBMS. In particular, the :drm:`make` method on ``<odbc-database>``
  accepts the ``datasource-name:`` keyword argument to specify the
  name of the ODBC datasource, as a :drm:`<string>`.

- The abstract class ``<connection>`` represents database connections.

  Instances of this class identify an execution context for executing
  SQL statements. The exact composition of a connection depends on the
  DBMS.

  The SQL-ODBC library defines the class ``<odbc-connection>``, a
  subclass of ``<connection>``. Instances of this class are created
  upon making a connection an ODBC database.

The ``<sql-statement>`` class represents SQL statements and their
state.  This class has the init-keywords ``text:``,
``input-indicator:``, and ``output-indicator:``.

The required keyword ``text:`` expects a :drm:`<string>` that contains
the text of an SQL statement.  Host variables can be included in the
statement by placing a question mark (``?``) at the point in the
string at which the value of the host variable should be
substituted. The optional keyword argument ``output-indicator:``
expects an instance of :drm:`<object>`. The output indicator is a
substitution value to be used whenever the column of a retrieved
record contains the SQL NULL value. The optional keyword
``input-indicator:`` expects an instance of :drm:`<object>`. The input
indicator is a marker value used to identify SQL NULL values in host
variables.

The SQL library defines two convenient macros that we use in this
tutorial: ``with-dbms`` and ``with-connection``. Here is the form of
a ``with-dbms`` call:

.. code-block:: dylan

  with-dbms (dbms)
    // body
  end with-dbms;

The ``with-dbms`` statement macro considers dbms, which must be a
general instance of class ``<dbms>``, to be the DBMS in use throughout
body. For example, if dbms is an instance of ``<odbc-dbms>`` and body
contains a call to ``connect``, then the call actually returns an
``<odbc-connection>``.

Here is the form of a ``with-connection`` call:

.. code-block:: dylan

   with-connection (connection)
     // body
   end with-connection;

The ``with-connection`` statement macro considers connection, which
must be an instance of class ``<connection>``, to be the default
database connection in use throughout body . For instance, each call
to ``execute`` an SQL statement within body uses connection by
default, so that the call’s ``connection:`` keyword argument need not
be supplied.

A call to the generic function ``connect`` returns a new connection of
class ``<connection>`` to the specified database database as the user
user . The connection can be closed by a call to ``disconnect``.

A call to the generic function ``execute(sql-statement, parameter:
vector)`` executes the SQL statement on the default connection. The
(optional) ``parameter:`` argument supplies a vector of values to be
substituted for any host variables appearing in the statement’s
text. The n th entry of this vector determines the value of the n th
host variable. Vector entries that equal the value of the statement’s
``input-indicator:`` keyword argument are sent as SQL NULL values.

If the SQL statement is a ``SELECT`` statement, then the result of
executing the statement (with ``execute``) is a value of class
``<result-set>``, which is itself a subclass of Dylan's built in
:drm:`<sequence>` class. Each element of a result set is a record and
each element of a record is a value. The various Dylan collection
protocols and functions work as you would expect on a result set. For
the purpose of this tutorial, it suffices to think of a result set as
a sequence of vectors.

Just to illustrate the use of the SQL-ODBC library without worrying
about the implementation of our CORBA server, here is a code fragment
that might be used to extract the entries in the ``Name`` and
``Balance`` columns of the :file:`bankDB.mdb` database:

.. code-block:: idl

   begin
     // choose the DBMS
     let odbc-dbms = make(<odbc-dbms>);
     with-dbms (odbc-dbms)
       // identify the database
       let database = make(<database>, datasource-name: "bankDB");

       // identify the user
       let user = make(<user>, user-name: "", password: "");

       // establish a connection for this database and user
       let connection = connect(database, user);
       with-connection (connection) // make it the default
         let query1 =               // construct the query...
           make(<sql-statement>,
                text: "select (Name, Balance) from Accounts");

          // ... and execute it on the default connection
          let result-set = execute(query);

          // extract the first record
          let first-record = result-set[0];

          // extract the first field of the first record.
          let first-name = result-set[0][0];
          let first-balance = result-set[0][1];
          let second-record = result-set[1];

          // ...
       end with-connection;
       disconnect(connection); // disconnect from the database
     end with-dbms;
   end;


Implementing CORBA objects in a server
======================================

A CORBA server has to provide an implementation object, called a
servant, for each of the proxy objects that might be manipulated by a
client. Our server needs to implement the initial ``bank`` servant,
and then create new servants for each of the account objects created
in response to ``openAccount``, ``openCheckingAccount`` and
``retrieveAccount`` requests. Each of these servants needs to be
registered in the CORBA environment and assigned an object reference,
so that the ORB can direct incoming requests to the appropriate
servant.

In CORBA, the primary means for an object implementation to access ORB
services such as object reference generation is via an object adapter.

Object adapters
---------------

An object adapter is responsible for the following functions:

- Generation and interpretation of object references

- Registration of servants

- Mapping object references to the corresponding servants

- IDL method invocations, mediated by skeleton methods

- Servant activation and deactivation

The Open Dylan ORB library provides an implementation of the Portable
Object Adapter (POA). This object adapter forms part of the CORBA
standard and, like the ORB, has an interface that is specified in
pseudo IDL (PIDL). The Open Dylan interface to the POA conforms
closely to the interface obtained by applying the Dylan mapping rules
to the POA’s PIDL specification.

A POA object manages the implementation of a collection of objects,
associating object references with specific servants. While the ORB is
an abstraction visible to both the client and server, POA objects are
visible only to the server. User-supplied object implementations are
registered with a POA and assigned object references. When a client
issues a request to perform an operation on such an object reference,
the ORB and POA cooperate to determine which servant the operation
should be invoked on, and to perform the invocation as an upcall
through a skeleton method.

The POA allows several ways of using servants although it does not
deal with the issue of starting the server process. Once started,
however, there can be a servant started and ended for a single method
call, a separate servant for each object, or a shared servant for all
instances of the object type. It allows for groups of objects to be
associated by means of being registered with different instances of
the POA object and allows implementations to specify their own
activation techniques. If the implementation is not active when an
invocation is performed, the POA will start one.

Unfortunately, the flexibility afforded by the POA means that its
interface is complex and somewhat difficult to use. The example in
this tutorial makes only elementary use of the POA.

Here is the PIDL specification of the facilities of the POA that are
used in this tutorial:

.. code-block:: idl

   module PortableServer {
     native Servant;

     interface POAManager {
       exception AdapterInactive{};
       void activate() raises (...);
       ...
     };

     interface POA {
       exception WrongAdapter {};
       readonly attribute POAManager the_POAManager;
       Object servant_to_reference(in Servant p_servant)
         raises (...);
       Servant reference_to_servant(in Object reference)
         raises (WrongAdapter, ...);
       ...
     };
   };

The POA-related interfaces are defined in a module separate from the
``CORBA`` module, called ``PortableServer`` . That module declares
several interfaces, of which only the ``POA`` and ``POAManager`` are
shown here.

The ``PortableServer`` module specifies the type ``Servant``. Values
of type ``Servant`` represent language-specific implementations of
CORBA interfaces. Since this type can only be determined by the
programming language in question, it is merely declared as a ``native`` type in the PIDL.

In the Dylan mapping, the ``Servant`` type maps to the abstract class
``PortableServer/<Servant>``. User-defined Dylan classes that are
meant to implement CORBA objects and be registered with a POA must
inherit from this abstract class.

Each ``POA`` object has an associated ``POAManager`` object. A POA
manager encapsulates the processing state of the POA it is associated
with. Using operations on the POA manager, an application can make
requests for a POA to be queued or discarded, and can have the POA
deactivated.

A POA manager has two main processing states, holding and active ,
that determine the capabilities of the associated POA and the handling
of ORB requests received by that POA. Both the POA manager and its
associated POA are initially in the holding state.

When a POA is in the holding state, it simply queues requests received
from the ORB without dispatching them to their implementation
objects. In the active state, the POA receives and processes requests.

Invoking the POA Manager’s ``activate`` operation causes it, and its
associated POA, to enter the active state.

A POA object provides two useful operations that map between object
references and servants: ``servant_to_reference`` and
``reference_to_servant``.

The ``servant_to_reference`` operation has two behaviors. If the given
servant is not already active in the POA, then the POA generates a new
object reference for that servant, records the association in the POA,
and returns the reference. If the servant is already active in the
POA, then the operation merely returns its associated object
reference.

The ``reference_to_servant`` operation returns the servant associated
with a given object reference in the POA. If the object reference was
not created by this POA, the operation raises the ``WrongAdapter``
exception.

The server’s perspective
------------------------

From the perspective of the server, the Bank-Protocol library
specifies the protocol that its servants must implement in order to
satisfy the interfaces in the IDL :file:`bank.idl`. A partial
implementation of this protocol resides in the Bank-Skeletons library
generated by the IDL compiler. This library should be used by any
application that wants to act as a server by providing an
implementation for a CORBA object matching an interface in the
:file:`bank.idl` file.

The Bank-Skeletons library defines an abstract servant class for each
of the protocol classes corresponding to an IDL interface. Each of
these classes inherits from the abstract class
``PortableServer/<Servant>``, allowing instances of these classes to
be registered with a POA.

A server provides an implementation of an abstract servant class by
defining a concrete subclass of that class, called an implementation
class, and defining methods, specialized on the implementation class,
for each of the protocol functions corresponding to an IDL attribute
or operation.

The Bank-Skeletons library defines a concrete skeleton method,
specialized on the appropriate abstract servant class, for each
protocol function stemming from an IDL attribute or operation. When
the POA receives a request from a client through the ORB it looks up
the servant targeted by that request, and invokes the corresponding
skeleton method on that servant. The skeleton method performs an
upcall to the method that implements the protocol function for the
implementation class of the servant. If the upcall succeeds, the
skeleton method sends the result to the client. If the method raises a
Dylan condition corresponding to a CORBA user or system exception, the
skeleton method sends the CORBA exception back to the client.

Requirements for implementing the bank server
=============================================

As there were for the bank client, there are three parts to
implementing the bank server:

- Write the code to initialize the CORBA ORB, set up the POA and POA
  manager, and get an initial object reference.

- Write the code for the CORBA objects that the server provides.

- Write the code for the server GUI.

We start by writing the CORBA object code. As noted in Section 6.4,
this entails writing concrete servant implementations.

The bank server GUI
-------------------

Since this demonstration principally concerns CORBA, and because we
would like to revamp the look-and-feel of the demonstration
occasionally, we do not describe the GUI implementation in great
detail. Instead, only a brief outline of the current design is given.

The bank server consists of one window that shows a table of raw
account data. Each row in the table shows the name, the current
balance, and the overdraft limit data.

There is also a log window for viewing incoming requests. The full
implementation of the server GUI can be found in the file
:file:`server-frame.dylan`.

The bank server library and module
----------------------------------

The bank server is implemented as a library:

.. code-block:: dylan

   define library bank-server
     use common-dylan;
     use dylan-orb;
     use bank-skeletons;
     use sql-odbc;
     use duim;
     // ...
   end library bank-server;

that defines a single module:

.. code-block:: dylan

   define module bank-server
     use common-dylan;
     use dylan-orb;
     use bank-skeletons;
     use sql-odbc;
     use duim;
     use threads;
     // ...
   end module bank-server;

Like the client, our server needs to use the Dylan-ORB system library
and module, in addition to its application specific libraries. Because
the server provides implementations (or servants) for CORBA objects
satisfying interfaces defined in the ``bank.idl`` file, it also needs
to use the Bank-Skeletons library and module.

Interoperating with ODBC requires the SQL-ODBC library and module.

Finally, our implementation of the server makes non-essential use of
the DUIM and Threads libraries and modules to present the user with a
dialog to shutdown the server. The full source code for the server is
in the :file:`bank-server.dylan` file.

Implementing the servant classes
--------------------------------

The Bank-Skeletons library defines three abstract servant classes::

   BankingDemo<account-servant>
   BankingDemo/<checkingAccount-servant>
   BankingDemo/<bank-servant>

These classes correspond to the IDL interfaces ``account``,
``checkingAccount``, and ``bank``.

The class ``BankingDemo/<checkingAccount-servant>`` is defined to
inherit from ``BankingDemo<account-servant>``, matching the
inheritance relationship declared in the IDL.

Each class inherits from the abstract class
``PortableServer/<Servant>``. This allows instances of the class to be
registered with a POA.

In our implementation of the bank server, these servant classes are
implemented by the following concrete subclasses::

   <bank-implementation>
   <account-implementation>
   <checkingAccount-implementation>

The ``<bank-implementation>`` class implements
``BankingDemo/<bank-servant>`` by representing a bank as a connection
to a database:

.. code-block:: dylan

   define class <bank-implementation> (BankingDemo/<bank-servant>)
     slot connection :: <connection>,
       required-init-keyword: connection:;
     constant slot poa :: PortableServer/<POA>,
       required-init-keyword: poa:;
     constant slot name :: CORBA/<string>,
       required-init-keyword: name:;
   end class <bank-implementation>;

The bank implementation class includes the slot ``poa`` to record the
POA in which the bank servant is active, so that servants representing
accounts at the bank can be registered in the same POA.

The ``<account-implementation>`` class implements ``BankingDemo/<account-servant>``:

.. code-block:: dylan

   define class <account-implementation>
       (BankingDemo/<account-servant>)
     constant slot bank :: <bank-implementation>,
       required-init-keyword: bank:;
     constant slot name :: CORBA/<string>,
       required-init-keyword: name:;
   end class <account-implementation>;

An instance of this class represents an account. The ``bank`` slot
provides a connection to the database that holds the account’s
record. The ``name`` slot identifies the record in the database.

Finally, the ``<checkingAccount-implementation>`` class implements
``BankingDemo/<checkingAccount-servant>`` simply by inheriting from
``<account-implementation>``:

.. code-block:: dylan

   define class <checkingAccount-implementation>
     (<account-implementation>,
      BankingDemo/<checkingAccount-servant>)
   end class <checkingAccount-implementation>;

Implementing the servant methods
--------------------------------

The next step in implementing the server is to define methods,
specialized on the implementation classes, for each of the protocol
functions corresponding to an IDL attribute or operation.

To support this, the abstract servant classes::

   BankingDemo/<account-servant>
   BankingDemo/<checkingAccount-servant>
   BankingDemo/<bank-servant>

are defined to inherit, respectively, from the abstract protocol
classes::

   BankingDemo/<account>
   BankingDemo/<checkingAccount>
   BankingDemo/<bank-servant>

As a result, implementing a protocol function boils down to defining a
concrete method for that function, where the method specializes on the
implementation class of its target object. Recall that the target
object of a protocol function is the first parameter to that function.

We can now present the implementations of the protocol functions. The
``BankingDemo/account/name`` method returns the value of the account’s
``name`` slot:

.. code-block:: dylan

   define method BankingDemo/account/name
       (account :: <account-implementation>)
    => (name :: CORBA/<string>)
     account.name
   end method BankingDemo/account/name;

The ``BankingDemo/account/balance`` method retrieves the balance field
from the corresponding record on the database by executing an SQL
``SELECT`` statement:

.. code-block:: dylan

   define method BankingDemo/account/balance
       (account :: <account-implementation>)
       => (balance :: CORBA/<long>)
     with-connection(account.bank.connection)
       let query = make(<sql-statement>,
                        text: "SELECT Balance FROM Accounts "
                              "WHERE Name = ?");
       let result-set = execute(query,
                                parameters: vector(account.name));
       as(CORBA/<long>, result-set[0][0]);
     end with-connection;
   end method BankingDemo/account/balance;

The ``BankingDemo/account/balance`` method increments the record’s
balance field by executing an SQL ``UPDATE`` statement:

.. code-block:: dylan

   define method BankingDemo/account/credit
       (account :: <account-implementation>,
        amount :: CORBA/<unsigned-long>)
       => ()
     with-connection(account.bank.connection)
       let amount = abs(amount);
       let query = make(<sql-statement>,
                        text: "UPDATE Accounts "
                              "SET Balance = Balance + ? "
                              "WHERE Name = ?");
       execute(query, parameters: vector(as(<integer>, amount),
               account.name));
     end with-connection;
   end method BankingDemo/account/credit;

The ``BankingDemo/account/debit`` method executes an SQL ``UPDATE``
statement that decrements the record’s balance field, provided the
balance exceeds the desired amount:

.. code-block:: dylan

   define method BankingDemo/account/debit
       (account :: <account-implementation>, amount :: CORBA/<long>)
    => ()
     with-connection(account.bank.connection)
       let amount = abs(amount);
       let query = make(<sql-statement>,
                        text: "UPDATE Accounts "
                              "SET Balance = Balance - ? "
                              "WHERE Name = ? AND Balance >= ?");
       execute(query,
               parameters: vector(as(<integer>, amount),
                                  account.name,
                                  as(<integer>, amount)));
     end with-connection;
   end method BankingDemo/account/debit;

The ``BankingDemo/checkingAccount/limit`` method is similar to the
``BankingDemo/account/balance`` method defined above:

.. code-block:: dylan

   define method BankingDemo/checkingAccount/limit
       (account :: <checkingAccount-implementation>)
       => (limit :: CORBA/<long>)
     with-connection(account.bank.connection)
       let query = make(<sql-statement>,
                        text: "select Limit from Accounts "
                              "where Name = ?");
       let result-set = execute(query,
                                parameters: vector(account.name));
       as(CORBA/<long>, result-set[0][0])
     end with-connection
   end method BankingDemo/checkingAccount/limit;

Because we defined ``<checkingAccount-implementation>`` to inherit
from ``<account-implementation>``, there is no need to re-implement
the ``BankingDemo/account/balance`` and ``BankingDemo/account/credit``
methods for this implementation class. However, we do want to define a
specialized ``BankingDemo/account/debit`` method, to reflect that a
checking account can be overdrawn up to its limit:

.. code-block:: dylan

   define method BankingDemo/account/debit
       (account :: <checkingAccount-implementation>,
        amount :: CORBA/<long>)
    => ()
     with-connection(account.bank.connection)
       let amount = abs(amount);
       let query = make(<sql-statement>,
                        text: "UPDATE Accounts "
                              "SET Balance = Balance - ? "
                              "WHERE Name = ? AND (Balance + Limit) >= ?");
       execute(query,
               parameters: vector(as(<integer>, amount),
                                  account.name, as(<integer>,
                                  amount)));
     end with-connection;
   end method BankingDemo/account/debit;

The ``BankingDemo/bank/name`` method returns the value of the bank’s ``name`` slot:


.. code-block:: dylan

   define method BankingDemo/bank/name
       (bank :: <bank-implementation>)
    => (name :: CORBA/<string>)
     bank.name
   end method BankingDemo/bank/name;

The ``BankingDemo/bank/openAccount`` method illustrates how CORBA user exceptions are raised:

.. code-block:: dylan

   define method BankingDemo/bank/openAccount
       (bank :: <bank-implementation>, name :: CORBA/<string>)
    => (account :: BankingDemo/<account>)
     if (existsAccount?(bank, name))
       error(make(BankingDemo/bank/<duplicateAccount>));
     else
       begin
         with-connection(bank.connection)
           let query = make(<sql-statement>,
                 text: "INSERT INTO Accounts(Name, Balance, Limit) "
                       "VALUES(?, ?, ?)",
                 input-indicator: #f);
           execute(query, parameters: vector(name, as(<integer>, 0),
                                      #f));
         end with-connection;
         let new-account = make(<account-implementation>,
                                bank: bank, name: name);
         as(BankingDemo/<account>,
            PortableServer/POA/servant-to-reference(bank.poa,
                                                    new-account));
       end;
     end if;
   end method BankingDemo/bank/openAccount;

If the test ``existsAccount?(bank, name)`` succeeds, the call to 

.. code-block:: dylan

   error (make(BankingDemo/bank/<duplicateAccount>));

raises a Dylan condition. (We omit the definition of
``existsAccount?``, which can be found in the source.) Recall that the
condition class ``BankingDemo/bank/<duplicateAccount>`` corresponds to
the IDL ``duplicateAccount`` exception. The POA that invoked this
method in response to a client’s request will catch the condition and
send the IDL ``duplicateAccount`` exception back to the client.

If there is no existing account for the supplied name, the
``BankingDemo/bank/openAccount`` method creates a new record in the
database by executing an SQL ``INSERT`` statement, initializing the
“Limit” field of this record with the SQL NULL value. (Recall that the
presence of the NULL value serves to distinguish ordinary accounts
from checking accounts on the database.)

Finally, the method makes a new servant of class
``<account-implementation>``, registers it with the bank’s POA with a
call to ``PortableServer/POA/servant-to-reference``, and narrows the
resulting object reference to the more specific class
``BankingDemo/<account>``, the class of object references to account
objects, as required by the signature of the protocol function.

The ``BankingDemo/bank/openCheckingAccount`` method is similar, except
that it initializes the ``Limit`` field of the new account record with
the desired overdraft limit, and registers a new servant of class
``<checkingAccount-implementation>``, returning an object reference of
class ``BankingDemo/<checkingAccount>``:


.. code-block:: dylan

   define method BankingDemo/bank/openCheckingAccount
       (bank :: <bank-implementation>, name :: CORBA/<string>,
        limit :: CORBA/<long>)
    => (checkingAccount :: BankingDemo/<checkingAccount>)
     if (existsAccount?(bank, name))
       error (make(BankingDemo/bank/<duplicateAccount>));
     else
       begin
         with-connection(bank.connection)
           let limit = abs(limit);
           let query =
             make(<sql-statement>,
                 text: "INSERT INTO Accounts(Name, Balance, Limit) "
                       "VALUES(?, ?, ?)",
                 input-indicator: #f);
           execute(query, parameters: vector(name, as(<integer>, 0),
                                             as(<integer>, limit)));
         end with-connection;
         let new-account = make(<checkingAccount-implementation>,
                                bank: bank, name: name);
         as(BankingDemo/<checkingAccount>,
         PortableServer/POA/servant-to-reference(bank.poa,
                                                 new-account));
        end;
     end if;
   end method BankingDemo/bank/openCheckingAccount;

The ``BankingDemo/bank/retrieveAccount`` method uses the ``name``
parameter to select the ``Limit`` field of an account record. If there
is no record with that name, indicated by the query returning an empty
result set, the method raises the CORBA user exception
``nonExistentAccount`` by signalling the corresponding Dylan error.

Otherwise, the method uses the value of the ``Limit`` field to
distinguish whether the account is an account or a current account,
creating a new servant of the appropriate class:

.. code-block:: dylan

   define method BankingDemo/bank/retrieveAccount
       (bank :: <bank-implementation>, name :: CORBA/<string>)
    => (account :: BankingDemo/<account>)
     with-connection(bank.connection)
       let query = make(<sql-statement>,
   		     text: "SELECT Limit FROM Accounts "
   		           "WHERE Name = ?",
   		     output-indicator: #f);
       let result-set = execute(query, parameters: vector(name),
   			     result-set-policy:
   			       $scrollable-result-set-policy);
       if (empty? (result-set))
         error (make(BankingDemo/bank/<nonExistentAccount>));
       elseif (result-set[0][0])
         as(BankingDemo/<checkingAccount>,
   	 PortableServer/POA/servant-to-reference
   	   (bank.poa, make(<checkingAccount-implementation>,
   			   bank: bank, name: name)));
       else
         as(BankingDemo/<account>,
   	 PortableServer/POA/servant-to-reference
   	   (bank.poa, make(<account-implementation>,
   			   bank: bank, name: name)));
       end if;
     end with-connection;
   end method BankingDemo/bank/retrieveAccount;

(Unlike the other queries in this example, this query is executed with
``result-set-policy: $scrollable-result-set-policy`` to ensure that
testing the emptiness of the result set does not invalidate its
records.)

Finally, the ``closeAccount`` removes the record of an account from
the database by executing an SQL ``delete`` statement:

.. code-block:: dylan

   define method BankingDemo/bank/closeAccount
       (bank :: <bank-implementation>,
        account-reference :: BankingDemo/<account>)
    => ()
     let account
       = Portableserver/POA/reference-to-servant(bank.poa,
   					      account-reference);
     with-connection(bank.connection)
       let query = make(<sql-statement>,
   		     text: "DELETE FROM Accounts "
   			   "WHERE Name = ?");
       execute(query, parameters: vector(account.name));
     end with-connection;
   end method BankingDemo/bank/closeAccount;

Note that we need to dereference the object reference ``account`` that
is passed in as the parameter of the ``BankingDemo/bank/closeAccount``
operation. We call the ``Portableserver/POA/reference-to-servant``
operation of the POA to do so. Here, we make implicit use of our
knowledge that, in our application, the server only encounters object
references registered with its local POA. This assumption is not true
in general.


Implementing CORBA initialization for the bank server
=====================================================

To complete the implementation of the bank server we need to write the
code that enters it into the CORBA environment. In detail, we need to:

- Initialize the server’s ORB
- Get a reference to the ORB pseudo-object for use in future ORB operations
- Get a reference to the POA pseudo-object for use in future POA operations
- Make a bank servant and register it with the POA
- Make the object reference of the bank servant available to the client
- Activate the POA to start processing incoming requests
- Prevent the process from exiting, providing availability

To do this, we need to make use of some additional operations
specified in the CORBA module:

.. code-block:: idl

   module CORBA {
     /// ...
     interface ORB {
       // ...
       typedef string ObjectId;
        exception InvalidName {};
        Object resolve_initial_references (in ObjectId identifier)
          raises (InvalidName);
       void run();
       void shutdown( in boolean wait_for_completion );
     }
   }

The CORBA standard specifies the ORB operation
``resolve_initial_references``. This operation provides a portable
method for applications to obtain initial references to a small set of
standard objects (objects other than the initial ORB). These objects
are identified by a mnemonic name, using a string knows as an
``ObjectId``. For instance, the ``ObjectID`` for an initial POA object
is ``"RootPOA"``. (References to a select few other objects, such as
the ``"Interface Repository"`` and ``"NamingService"``, can also be
obtained in this manner.)

The ORB operation ``resolve_initial_references`` returns the object
associated with an ``ObjectId``, raising the exception ``InvalidName``
for an unrecognized ``ObjectID``.

The ``run`` and ``shutdown`` operations are useful in multi-threaded
programs, such as servers, which, apart from the main thread, need to
run a separate request receiver thread for each POA.

(A single-threaded application, such as a pure ORB client, does not
generally need to use these operations.)

A thread that calls an ORB’s ``run`` operation simply waits until it
receives notification that the ORB has shut down.

Calling ``run`` in a server’s main thread can then be used to ensure
that the server process does not exit until the ORB has been
explicitly shut down.

Meanwhile, the ``shutdown`` operation instructs the ORB, and its
object adapters, to shut down.

If the ``wait_for_completion`` parameter is ``TRUE``, the operation
blocks until all pending ORB processing has completed, otherwise it
simply shuts down the ORB immediately.

.. code-block:: idl

   define method initialize-server ()
     let location-service = get-location-service();

     // get reference to ORB
     let orb = CORBA/ORB-init(make(CORBA/<arg-list>), "Open Dylan ORB");

     // get reference to root POA, initially in the holding state
     let RootPOA = CORBA/ORB/resolve-initial-references(orb, "RootPOA");

     with-dbms ($dbms)
        // connect to the database
        let database = make(<database>, datasource-name: $datasource-name);
        let user =  make(<user>, user-name: $user-name, password: $user-password);
        let connection = connect(database, user);

        // make the server frame, initialize and refresh it.
        let server-frame = make(<server-frame>, connection: connection);
        server-frame.refresh-check-button.gadget-value := #t;
        refresh(server-frame);

        //  make the bank servant
        let bank = make(<bank-implementation>, connection: connection,
                        poa: RootPOA, name: "Dylan Bank",
                        server-frame: server-frame);

        // get the servant's object reference from the poa
        let bank-reference = PortableServer/POA/servant-to-reference(bank.poa, bank);

        // activate the bank's POA using its POA manager.
        let POAManager = PortableServer/POA/the-POAManager(bank.poa);
        PortableServer/POAManager/activate(POAManager);

        // register the bank with the location service
        register-bank(orb, location-service, bank-reference);

        // create a separate thread to shut down the orb, unblocking the main thread.
        make(<thread>,
             function: method ()
                         start-frame(server-frame);
                         CORBA/ORB/shutdown(orb, #t);
                       end method);

        // block the main thread
        CORBA/ORB/run(orb);

        // remove from location service
        unregister-bank(orb, location-service, bank-reference);

        // close the bank's connection.
        disconnect(connection);
     end with-dbms;
   end method;

The ``initialize-server`` function first initializes the Open Dylan
ORB by calling the Dylan generic function ``CORBA/ORB-init``, just as
we initialized the ORB in the client. The call returns a
``CORBA/<ORB>`` pseudo object.

Invoking ``CORBA/ORB/resolve-initial-references`` on this ORB, passing
the ``ObjectID "RootPOA"``, returns a POA object of class
``PortableServer/<POA>``. This is the CORBA standard method for
obtaining the initial POA object. Note that RootPOA is initially in
the holding state.

Next, we connect to the database and use the connection to make a bank
servant. We register the servant with the POA, RootPOA, and publish
the resulting object reference, encoded as a string, according to the
location-service requested in the command line arguments. By default
this is via a shared file. However, if the following is specified on
the command line::

   -location-service:naming-service

then a Name Service is used instead. Use the ORB command line option
``-ORBname-service`` to specify the IOR of the Name Service. Be sure
to use the same command line options for the client and the server or
they will not find each other!

We then obtain the POA Manager for the POA using the POA operation
``PortableServer/POA/the-POAManager``. The call to
``PortableServer/POAManager/activate`` moves the POA out of the
holding state, into the active state, ready to receive and process
incoming requests.

To prevent the server from exiting before having the chance to process
any requests, we introduce a new thread. This thread waits until the
user responds to a DUIM dialog and then proceeds to shut down the ORB
with a CORBA standard call to ``CORBA/ORB/shutdown``. Meanwhile,
back in the main thread, the subsequent call to ``CORBA/ORB/run``
causes the main thread to block, waiting for notification that the ORB
has shut down.

Once the ORB has shut down, the main thread resumes, closes the
connection to the bank, and exits, terminating the server application.

The full implementation of the server initialization is in the file
``init-server.dylan``.

This completes the description of our implementation of the server.
