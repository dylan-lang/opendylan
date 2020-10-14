***************************
Setting up the Bank Example
***************************

About the bank example
======================

Chapters 4, 5, and 6 of this guide present a deeper CORBA development
example than the Hello World example we saw in chapter 2. The new
example is a simple implementation of a bank. The architecture of the
bank has three components:

- A database that provides persistent storage for the accounts managed
  by the bank.
- A CORBA server that represents the bank and provides an
  object-oriented interface to its accounts.
- A CORBA client that provides a graphical user interface to the bank.

This application is a typical example of a three-tier application
architecture comprising a database access layer, a business logic
layer, and a user interface layer.

Accounts are stored as records in a Microsoft Access™ relational
database. The database is manipulated by the server using the
Open Dylan SQL-ODBC library.

The server provides a single CORBA object that represents the
bank. This object manages a collection of CORBA objects that represent
customer accounts. The bank has operations for opening and closing
accounts, and for retrieving existing accounts from the database. In
turn, accounts support operations for querying and updating their
balance.

The client initially contacts the server by obtaining a reference to
the bank object from the Open Dylan ORB. It then presents the
user with a graphical interface to the bank.

In response to user requests, the interface invokes operations on the
bank, obtaining further references to accounts created on the
server. The client manages separate windows for the bank and each of
the accounts that are active in the server.

We will use the Open Dylan DUIM library to implement the client’s user
interface.


Where to find the example code
==============================

The bank example code is available in the Open Dylan Examples menu,
under the CORBA category. There is a Bank Client project and a Bank
Server project.

The same code is also available in the top-level Open Dylan
installation folder, under :file:`Examples\\corba\\bank`. This folder
has several subfolders.

- :file:`Examples\\corba\\bank\\bank` contains the file
  :file:`bank.idl`. This is the IDL file declaring the CORBA interface
  to the server.
- :file:`Examples\\corba\\bank\\bank-client` contains the implementation
  of the client as project :file:`bank-client.hdp`.
- :file:`Examples\\corba\\bank\\bank-server` contains the implementation
  of the server as project :file:`bank-server.hdp`. This folder also
  contains a ready made Microsoft Access database file
  :file:`bankDB.mdb`. The application uses this to record bank account
  details.

ODBC requirements
=================

In order to run the example, you need to have ODBC version 3.0 (or
higher) and an ODBC driver for Microsoft Access installed on the
machine hosting the server application. You do not need a copy of
Microsoft Access.

Both ODBC 3.x and the Microsoft Access driver are available free for
download from Microsoft Access Database Engine 2010 Redistributable
download page,
<https://www.microsoft.com/en-us/download/details.aspx?id=13255>.

This information may change in the future.

Registering the database with ODBC
==================================

So that the bank server can access the :file:`bank.mdb` database using
ODBC, we need to register the database as an ODBC data source. This
installation step also tells ODBC which driver to use when connecting
to the database.

To register the database in this way, ODBC drivers must be installed
on the machine that will host the server.

Registering the database on Windows 10
--------------------------------------

To register the :file:`bank.mdb` database with ODBC on Windows 10:

#. From the Windows :guilabel:`Start` menu, choose
   :menuselection:`Windows Administrative Tools --> ODBC Data Sources
   (32-bit)`

   The ODBC Data Source Administrator appears.

#. Select the :guilabel:`User DSN` tab.

#. Click :guilabel:`Add...`.

#. Select :guilabel:`Microsoft Access Driver`
   from the list of available drivers.

#. Click :guilabel:`Finish`.

   The ODBC Microsoft Access Setup dialog appears.

#. In the ODBC Microsoft Access Setup dialog, enter ``bankDB``
   in the Data Source Name field.

#. Click :guilabel:`Select...`.

   The Select Database dialog appears.

   We can now specify the database file name. The file is stored under
   the top-level Open Dylan installation folder, in the
   subfolder :file:`Examples\\corba\\bank\\bank-server`.

#. In the Select Database dialog, browse until you reach the folder above.

#. Select :file:`bankDB.mbd` from the list of available files, then
   click :guilabel:`OK` .

#. Click :guilabel:`OK` again to close the ODBC Microsoft Access Setup
   dialog.

#. Click :guilabel:`OK` to close the ODBC Data Source Administrator.

   We also need to ensure that the :file:`bank.mdb`
   file is writable. 

#. Right-click on the file in a Windows Explorer window.

#. Select :guilabel:`Properties` from the shortcut menu.

#. Clear the Read-only attribute if it is checked, and click
   :guilabel:`OK`.

The ODBC setup work for the demo is now complete. We can move on to
building and running the demo itself.

Building the Bank client and server
===================================

We can now build the client and server applications for the demo.

#. Start the Open Dylan environment.

   The client and server projects are available in the Examples
   dialog. Choose :menuselection:`Help --> Open Example Project...` from
   the main window.

#. In the Examples dialog, open the Bank-Client project.

#. In the Bank-Client project window, choose :menuselection:`Project
   --> Build`.

   The IDL compiler, Scepter, is invoked automatically during the
   build process. It compiles the file :file:`bank.idl` to generate
   the source code for the protocol and stubs libraries.

#. Bring up the Examples dialog again, and open the Bank-Server project.

#. In the Bank-Server project window, choose choose :menuselection:`Project
   --> Build`.

Running the server and client
=============================

We can now run the bank demo for the first time.

In the Bank-Server project window, choose :menuselection:`Application
--> Start` to run the server executable. The server is represented by
an administration window with a raw table of the database contents, a
log window for seeing requests, and a couple of menu items.

Once you have finished interacting with the bank, click the close
button in the top right-hand corner of this bank server window to
exit. Do not do this yet.

Once the server’s dialog has appeared, go to the Bank-Client project
window and choose :menuselection:`Application --> Start` to run the
executable for the client. A window presenting a GUI to the bank
should appear.You can now interact with the bank to create new
accounts, deposit amounts, and so on.

Once you have finished interacting with the bank, click the close
button in the top right-hand corner of the bank window to exit the
client application. Then do the same for the server window.
