****************************************
Running and Debugging CORBA Applications
****************************************

Debugging client/server applications in the IDE
===============================================

The following text is a quotation from the Open Dylan manual Getting
Started with Open Dylan. See Chapter 6 of that manual for general
information about running, debugging, and interacting with
applications.

If you have a client/server application, where both the client
application and server application are written in Dylan, you can debug
them in parallel.

Start by opening both projects in the environment. It is not possible
to run two instances of the environment, with one debugging the client
and the other debugging the server: if any libraries are shared
between the applications, both environment instances will attempt to
lock the compiler database files for those libraries. Since all
applications ultimately use the Dylan library, and most share other
libraries — not the least of which in this case being networking
libraries — using two Open Dylan processes is never a practical
debugging method.

This is not a disadvantage. By running both client and server in one
Open Dylan instance, you can be debugging in the client, and then when
the client invokes the server you can smoothly start debugging that
instead. This can be very useful for tracking down synchronization
bugs.

Once you have both projects open, you can start both applications
up. Note that by default the action of starting a project will switch
the active project, so the last project you start will be the active
one by default. You can change this behavior in the main window with
:menuselection:`Options --> Environment Options...` so that the active
project does not switch in this situation. See “The active project” on
page 111 [of Getting Started with Open Dylan] for more information.

If you need to rebuild a library shared between the client and server,
you need to stop both running applications, since Windows forbids
writing to a DLL that is currently in use.

Be careful when setting breakpoints if the client and server library
share source files. If you set a breakpoint when editing a shared
file, the breakpoint will be set in the editor’s active project. You
can change the active project using the popup in the main window.

Breakpoints set in other windows’ source pages (such as in the
browser) act on the project associated with the window. Note that this
makes it possible to set breakpoints in both the client and the server
so that the debugger correctly opens up on the appropriate project as
the breakpoints are reached. However, you cannot set the same
breakpoint in both projects at once. Instead you have to go into each
project and set the breakpoint separately.

Browsing for supported CORBA operations
=======================================

Until we have a CORBA reference manual for Open Dylan, you can work
out what CORBA PIDL operations are available to Dylan applications
using the ``dylan-orb`` library by using the browser in the
development environment.

#. In a CORBA project window, go to the :guilabel:`Libraries` page.

#. Double-click on the ``dylan-orb`` item.

   This browses the library ``dylan-orb``.

#. In the browser, go to the :guilabel:`Definitions` page.

#. Double-click on the ``module dylan-orb:dylan-orb`` item.

   This browses the ``dylan-orb`` library’s ``dylan-orb`` module.

#. Go to the :guilabel:`Names` page.

Having done this you can see the names in the module. Use the pop-up
list to show only the locally exported names — the names exported from
this module.

The locally exported names contain the PIDL translations into Dylan as
well as various Dylan protocols for using CORBA. These are the names
that the browser’s :guilabel:`Module` column shows were imported from
the ``corba-protocol`` module of the ``corba-protocol`` library.

ORB runtime
=============


Implicit activation
---------------------

Servants may be returned from operations and if the POA ID Assignment
policy is set to ``#"System-ID"``, then they are automatically
converted to object references. For instance, instead of

.. code-block:: dylan

   as(<account>, create-reference(poa, make(<demo-account>, ...)))


you can simply use:

.. code-block:: dylan

   make(<demo-account>, ...)

Port assignment
---------------

Socket port numbers are assigned dynamically, by default. If you wish
to use the original (late 1990s era) fixed port number of the Open
Dylan ORB, specify the following on the command line::

   -ORBport 3672

As of version 2.0, this port number is reserved for use by the Open
Dylan ORB’s Implementation Repository (which sadly does not
exist). However, you can still explicitly select a port number, for
example::

   -ORBport 9999

POA threading
-------------

The Open Dylan ORB has one request processing thread per POA, by
default. You can set the number of request processing threads when
creating a POA by using the thread policy, for example:

.. code-block:: dylan

   thread-policy: 4

This creates a pool of four worker threads that service requests
dispatched to that POA. Therefore, up to four requests could be
processed concurrently. In such a situation the underlying application
code that is called from the POA must be multi-thread safe.

ORB runtime switches
--------------------

The full set of command line arguments that the Dylan ORB supports is
listed below. When running the application under the control of Open
Dylan, these command-line options can be set using the
:guilabel:`Debug` page of the :menuselection:`Project --> Settings...`
dialog, in the :guilabel:`Arguments` field of the :guilabel:`Command
Line` section.

``-ORBtrace``
   Turns on debug messages inside the ORB. These are mainly internal
   debugging messages, but may help you to understand what is going on
   inside the ORB, or help you report problems to the Open Dylan
   developers.

``-ORBdebug``
   Suppresses handling of application implementation errors in server
   code. That is, instead of them being translated into CORBA
   exceptions for transmission to the client, they are left unhandled
   in the server so that they can be debugged.

``-ORBport`` number
    Sets default socket port for listening. The default port number
    registered with IANA for ``lispworks-orb`` is 3672.

``-ORBid`` name
   Sets name of ORB.

``-ORBno-co-location``
   Suppresses co-location optimization. That is, forces the ORB to
   always use sockets and IIOP marshalling even when it might have
   detected an in-process server.

The next command line options are concerned with the initial services offered by the ORB.

``-ORBname-service-file`` *filename*
   Sets *filename* containing IOR for name service. The string in the
   file is converted to an object reference and returned by

   .. code-block:: dylan

      CORBA/ORB/ResolveInitialServices("NameService")

   This option persists from session to session via the Windows Registry.

``-ORBname-service`` *ior*
   Sets *ior* for the name service (takes precedence over file-based
   alternative above). The string is converted to an object reference
   and returned by

   .. code-block:: dylan

      CORBA/ORB/ResolveInitialServices("NameService")

   This option persists from session to session via the Windows Registry.

``-ORBinterface-repository-file`` *filename*
   Sets *filename* containing IOR for interface repository. The string
   in the file is converted to an object reference and returned by

   .. code-block:: dylan

      CORBA/ORB/ResolveInitialServices("InterfaceRepository")

   This option persists from session to session via the Windows Registry.

``-ORBinterface-repository`` *ior*
   Sets *ior* for the interface repository (takes precedence over
   file-based alternative above). The string is converted to an object
   reference and returned by

   .. code-block:: dylan

      CORBA/ORB/ResolveInitialServices("InterfaceRepository")

   This option persists from session to session via the Windows Registry.

``-ORBsettings``
   Prints out a list of the configuration options to the standard output.
