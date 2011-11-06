****************
Remote Debugging
****************

Running a Dylan application on a remote machine
-----------------------------------------------

Open Dylan offers a facility for running, debugging, and interacting
with a Dylan application, DLL, or OLE control (“program” hereafter)
running on a remote machine—that is, a networked machine other than
the one running the Open Dylan IDE.

The ability to do these things on the remote machine is a simple
extension of the standard features described in `See Debugging and
Interactive Development <../debug.htm#18907>`_, which covers debugging
and interaction techniques. The techniques for running, debugging, and
interacting with a program are exactly the same as for the local
machine, but there are a few initial configuration issues to cover.

Installing the program and debugging server on the remote machine
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To do anything with a Dylan program on a remote machine, the program has
to be installed there. It may be there already—if, for instance, you are
working with another Dylan developer—but if it is not, you must install
the program on that machine.

To install the debugging server and your program on a remote machine,
perform these steps on the target machine:

Install the Open Dylan runtime system.
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The runtime installer executable, the name of which begins “*hdrun* …”,
resides in the *Extras* folder at the top-level of your Dylan CD-ROM.

Install the debugging server application.
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The debugging server installer executable, the name of which begins with
“*hddbg* …”, also resides in the *Extras* folder.

Copy the *bin* folder for your project onto the remote machine.
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Starting the debugging server
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Whenever you try to invoke the program on the remote machine, Open
Dylan expects to be able to talk to the debugging server application on
the remote machine. If this server is not running on the remote machine,
it is not possible to run the Dylan program there either.

The debugging server must be started manually on the remote machine
itself, so it is worth installing it there. (See
` <remotedbg.htm#61300>`_ for information about using the debugging
server installer.) Of course, if the remote machine already has the
Open Dylan installed on it, you do not need to install the debugging
server.

To start the debugging server choose *Start > Programs > Open Dylan
> Start Debug Server*. You can also invoke it with
*debugger-server.exe*, which is available in the *Bin* subfolder of the
Open Dylan installation folder.

If the debugging server starts up correctly, a Open Dylan Debugger
Server window opens. The window provides up-to-date information about
active local debugger processes and remote connections. The upper pane
of the window shows all locally running processes being debugged on
remote machines, and the lower pane shows the remote machines that are
connected to the server. A status bar at the bottom of the window
displays a summary.

You can set (and later change) a password for the debugging server by
using the *Change Password* button on the Debugger Server window. The
password can be anything. By default, no password is required.

To exit the Debugging Server, click the *Exit* button or simply close
the window.

At this point, you can return to the machine running the Open Dylan
IDE, where you will be ready to run Dylan programs remotely.

Starting an application remotely
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Running and debugging an application on a remote machine is identical in
almost every respect to using your local machine. You can use the three
menu items *Application > Start*, *Application > Debug* and
*Application > Interact* to launch the application in the normal way.
The only difference is that for remote startup you must specify a remote
machine in the project’s debug settings.

Select *Project > Settings…* and select the Debug page.
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In the “Remote machine” section, an option box displays the name of the
machine on which the project’s application is to be run. By default, the
selection will be the local machine. Before you can select a remote
machine, you must first establish a debugging connection.

Click the *Open New Connection…* button.
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In the dialog that appears, enter the network address of the machine to
which you wish to connect, and enter the password for the debugging
server (if one is set), and click *OK*.
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The address is whatever the Windows operating system needs to identify
the machine on the network. A computer hostname is likely to be
sufficient.

The password is the password established when you first started the
debugging server (see ` <remotedbg.htm#35532>`_).

If the connection is successful, the newly connected machine is added to
those listed in the “Remote machine” option box. You can now select this
machine.

If the connection does not succeed, you should ensure that you have
successfully started the debug server program on the remote machine.
(See ` <remotedbg.htm#35532>`_ above.)

Select the remote machine in the option box.
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Having selected a remote machine, you must also ensure that Open
Dylan can locate the program (EXE or DLL) on the filesystem of the
remote machine.

Specify the path to the program in the “Command line” section of the
Debug page.

The path should be fully qualified, including the name of the remote
machine. For example::

    \\\\spiral\\c\_drive\\apps\\reversi\\release\\reversi.exe

where *spiral* is a machine name, and *c\_drive* is the share name of
the drive containing the program *release* folder.

You are now ready to run and debug the application remotely.
*Application > Start* starts your application running on the remote
machine. All the usual debugging and interaction facilities will now be
available.

Details about the connection to a remote machine are stored and saved
with the project. Open Dylan tries to re-establish the remote
connection automatically when you next open and try to run the project.

Attaching to running processes
------------------------------

The three commands *Application > Start*, *Application > Debug* and
*Application > Interact* are all used to launch a program from within
Open Dylan. But if the program is running already, perhaps even
before you started up the Open Dylan environment, the environment
does not know about the process and therefore it has no visible presence
in the environment.

Open Dylan allows you to “attach” to such a running process, thereby
bringing it under Open Dylan’s control just as if it had been started
by the environment. It is very simple to do this:

Open the project whose application, DLL, or control is running.

Choose *Application > Attach…* from the project window, or choose *Tools
> Attach Debugger…* from the main window.

A dialog listing all available running processes appears.

Select the process to which you want to attach, and click *OK*.

After a few moments, the debugger attaches to the running process, and
all the normal debugging and interaction facilities become available,
just as with *Application > Start*.

*Note:* Open Dylan does not currently offer any facility for
detaching from a process. Once it has been attached to the Open
Dylan debugger, and all of your debugging work is finished, you will
need to close the program down using the *Application > Stop* command.

The process to which you attach need not be running on the local
machine. You can also attach to a process that is running on a remote
machine provided that the debugging server application (see
` <remotedbg.htm#35532>`_) is running on that machine. The process list
dialog has an option box that allows you to select the machine whose
process list you want to view. There is also an *Open New Connection…*
button for creating new connections to remote machines, which works in
the same way as described in ` <remotedbg.htm#34289>`_.


