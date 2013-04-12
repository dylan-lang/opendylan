*****************************
Delivering Dylan Applications
*****************************

Applications you have written using Open Dylan need access to
Open Dylan’s run-time libraries in order to run. This applies to
all applications, whether executables or DLLs. The run-time libraries
are normal Win32 DLLs, stored in the top-level installation folder
*Redistributable*.

When you run an application on a machine where Open Dylan is
installed, the libraries will be found in *Redistributable*, whether
you run the application from the command line or from within the
Open Dylan environment using commands such as **Application > Start**.

But to deliver your Dylan application to a customer or other third
party, you will need to include in your distribution the Open Dylan
run-time libraries that the application uses.

This chapter discusses two methods of delivering Dylan applications with
the necessary run-time libraries: using the environment to build a
Release folder, and using Open Dylan’s stand-alone run-time library
installer.

Building a release folder
=========================

To create a single folder containing everything necessary for your
application to run on a customer’s machine, use the **Project > Make
Release** command in the project window.

This command takes the compiled application files associated with the
project, and whichever Open Dylan run-time libraries (DLLs) are
necessary for the application to run, and copies them into the *Release*
subfolder of the project’s own folder.

You can then distribute the entire *Release* folder as a stand-alone
application. Read the Open Dylan license agreement for details of
the legal side of redistributing the Open Dylan run-time libraries.

Using the run-time library installer
====================================

An alternative to building a single release folder is to use Open
Dylan’s run-time library installer, a self-extracting executable that
installs all the Open Dylan run-time libraries (DLLs) in a central
location.

The default location is

::

    C:\Program Files\Common Files\Harlequin\System

The run-time library installer also sets the PATH environment variable
to include this folder.

Your distributable application should then consist of a copy of your
compiled application files from the project’s *Bin* folder, and the
run-time installer.

The run-time library installer is included on CD-ROM editions of
Open Dylan, and can also be downloaded from Harlequin’s World Wide
Web site. You can distribute the run-time installer to customers, or
allow them to download it themselves.

About the run-time library DLLs
===============================

The run-time library DLLs have 8.3 format names and include a version
number. This version number will be incremented for new releases of the
run-time libraries.
