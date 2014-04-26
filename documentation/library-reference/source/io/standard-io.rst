**********************
The standard-io Module
**********************

.. current-library:: io
.. current-module:: standard-io

Introduction
------------

This document describes the Standard-IO module, which requires the
Streams module. All interfaces described in this document are exported
from the Standard-IO module. The functionality provided by this module
mirrors some of the functionality provided by the *Java.io* package in
Java.

For convenience, the Standard-IO module, together with the Format
module, is repackaged by the Format-out module. See :doc:`format-out`
for details.

Handling standard input and output
----------------------------------

The Standard-IO module provides a Dylan interface to the standard I/O
facility of operating systems such as MS-DOS or UNIX.

The module consists of three variables, each of which is bound to a
stream:

- :var:`*standard-input*`
- :var:`*standard-output*`
- :var:`*standard-error*`

For console-based applications (i.e., applications that run in character
mode), the three streams just use the console window in which the
application was started.

For purely window-based applications, each variable is bound by default
to a stream that lazily creates a console window as soon as any input is
requested or output is performed. Only one window is created, and this
is shared between all three streams. Any subsequent input or output uses
the same window. The window that is created uses the standard
configuration settings set by the user. For example, the window is only
scrollable if all console windows are configured to be scrollable on the
machine running the application.

For more information about streams, please refer to :doc:`streams`.

The standard-io module
----------------------

This section contains a complete reference of all the interfaces that
are exported from the *standard-io* module.

.. variable:: *standard-input*

   The standard input stream.

   :type: :class:`<stream>`

   :value: The standard input stream for the platform on which the
     application is running.

   :description:

     This variable is bound to an input stream that reads data from the
     standard input location for the platform on which the application
     is running. It is equivalent to the Java stream
     *java.lang.System.in*.

     If the platform has a notion of standard streams, such as MS-DOS,
     this stream maps onto the platform-specific standard input stream.
     If the platform has no such convention, such as a platform that is
     primarily window-based, then a console window is created for this
     stream if necessary, in order to provide users with a place to
     provide input.

     If a console window has already been created as a result of writing
     to one of the other variables in the Standard-IO module, then the
     existing console window is used, and a new one is not created: a
     single console window is used for all variables in this module.

.. variable:: *standard-output*

   The standard output stream.

   :type: :class:`<stream>`

   :value: The standard output stream for the platform on which the
     application is running.

   :description:

     This variable is bound to an output stream that sends normal output
     to the standard output location for the platform on which the
     application is running. It is equivalent to the Java stream
     *java.lang.System.out*.

     If the platform has a notion of standard streams, such as MS-DOS,
     this stream maps onto the platform-specific standard output stream.
     If the platform has no such convention, such as a platform that is
     primarily window-based, a console window is created for this stream
     if necessary, just to capture output to it.

     If a console window has already been created as a result of writing
     to or reading from one of the other variables in the Standard-IO
     module, then the existing console window is used, and a new one is
     not created: a single console window is used for all variables in
     this module.

.. variable:: *standard-error*

   The standard error stream.

   :type: :class:`<stream>`

   :value: The standard error stream for the platform on which the
     application is running.

   :description:

     This variable is bound to an output stream that sends error
     messages to the standard error location for the platform on which
     the application is running. It is equivalent to the Java stream
     *java.lang.System.err*.

     If the platform has a notion of standard streams, such as MS-DOS,
     this stream maps onto the platform-specific standard error stream.
     If the platform has no such convention, such as a platform that is
     primarily window-based, a console window is created for this stream
     if necessary, just to capture output to it.

     If a console window has already been created as a result of writing
     to or reading from one of the other variables in the Standard-IO
     module, then the existing console window is used, and a new one is
     not created: a single console window is used for all variables in
     this module.
