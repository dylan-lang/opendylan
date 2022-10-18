***************************
The operating-system Module
***************************

.. current-library:: system
.. current-module:: operating-system

The operating-system module is part of the System library. It provides
an interface to some features of the host machine's operating system.

.. contents::

Manipulating environment information
------------------------------------

The operating-system module contains a number of interfaces for
examining and specifying information about the operating system
environment of the host machine. As well as providing constants that you
can use in your code, you can examine and set the value of any
environment variable in the system.

The following constants contain machine-specific information:

- :const:`$architecture-little-endian?`
- :const:`$machine-name`
- :const:`$os-name`
- :const:`$os-variant`
- :const:`$os-version`
- :const:`$platform-name`

These constants contain information about the hardware and software
resident on the host machine. The constants let you programmatically
check the current system conditions before executing a piece of code.

The following function also returns information about the machine:

- :func:`machine-concurrent-thread-count`

The following two functions let you manipulate the values of any
environment variables in the system.

- :func:`environment-variable`
- :func:`environment-variable-setter`

The following functions access information about the user logged on to
the current machine, where available.

- :func:`login-name`
- :func:`login-group`
- :func:`owner-name`
- :func:`owner-organization`

Running and Introspecting Applications
--------------------------------------

- :func:`run-application`
- :func:`current-process-id`
- :func:`parent-process-id`

The following functions are defined in the :doc:`common-dylan
<../common-dylan/index>` library and re-exported from the operating-system
module:

- :func:`application-arguments`
- :func:`application-filename`
- :func:`application-name`
- :func:`exit-application`
- :func:`register-application-exit-function`
- :func:`tokenize-command-line`

Working with shared libraries
-----------------------------

- :func:`load-library`

The operating-system module
---------------------------

This section contains a reference entry for each item exported from the
System library's operating-system module.

.. constant:: $architecture-little-endian?

   Constant specifying whether the processor architecture is little-endian.

   :type: <boolean>

   :description:

     This constant is a boolean value that is true if the processor
     architecture is little-endian and false if it is big-endian. (A
     processor is little-endian if the rightmost bit in a word is the
     least-significant bit.) For processors implementing the Intel x86
     architecture this value is ``#t``.

   :seealso:

     - :const:`$machine-name`
     - :const:`$os-name`
     - :const:`$os-variant`
     - :const:`$os-version`
     - :const:`$platform-name`

.. function:: current-process-id

   Returns the integer value for the current process ID.

   :signature: current-process-id => *pid*

   :value pid: An instance of :drm:`<integer>`.

   :description:

     Returns the integer value of the current process ID.

   :seealso:

     - :func:`current-thread-id`
     - :func:`parent-process-id`

.. function:: environment-variable

   Returns the value of a specified environment variable.

   :signature: environment-variable *name* => *value*

   :parameter name: An instance of :drm:`<byte-string>`.
   :value value: An instance of :drm:`<byte-string>`, or ``#f``.

   :description:

     Returns the value of the environment variable specified by *name*,
     or ``#f`` if there is no such environment variable.

   :seealso:

     - :func:`environment-variable-setter`

.. function:: environment-variable-setter

   Sets the value of an environment variable.

   :signature: environment-variable-setter *new-value* *name* => *new-value*

   :parameter new-value: An instance of :drm:`<byte-string>`, or ``#f``.
   :parameter name: An instance of :drm:`<byte-string>`.
   :value new-value: An instance of :drm:`<byte-string>`, or ``#f``.

   :description:

     Changes the value of the environment variable specified by *name*
     to *new-value*. If *new-value* is ``#f``, the environment variable
     is undefined. If the environment variable does not already exist,
     *environment-variable-setter* creates it.

     .. note:: Windows 95 places restrictions on the number of
       environment variables allowed, based on the total length of the
       names and values of the existing environment variables. The
       function *environment-variable-setter* only creates a new
       environment variable if it is possible within these restrictions.
       See the relevant Windows 95 documentation for more details.

   :seealso:

     - :func:`environment-variable`

.. function:: load-library

   Loads a shared library into the current process.

   :signature: load-library *name* => *module*

   :parameter name: An instance of :drm:`<string>`.
   :value module: An instance of :class:`<machine-word>`.

   :description:

     Loads the library specified by *name* into the current process. The
     library must be a shared library.

     If the library is a library written in Dylan, then when it loaded,
     constructor functions will run which set up the various methods and other
     Dylan objects within the shared library. Top level code within the library
     will be executed.

     Load-time failures, for example due to missing files or
     unsatisfied symbol dependencies, will cause an :drm:`<error>`
     condition to be signaled.

     .. note:: Dynamic loading of Dylan code is currently only
               supported on non-Windows platforms using the LLVM
               back-end, and on Windows using the HARP back-end.

.. function:: login-name

   Returns as an instance of :drm:`<string>` the name of the user logged on
   to the current machine, or ``#f`` if unavailable.

   :signature: login-name () => *name-or-false*

   :value name-or-false: An instance of ``false-or(<string>)``.

   :description:

     Returns as an instance of :drm:`<string>` the name of the user logged
     on to the current machine, or ``#f`` if unavailable.

   :seealso:

     - :func:`login-group`

.. function:: login-group

   :signature: login-group () => *group-or-false*

   :value group-or-false: An instance of ``false-or(<string>)``.

   :description:

     Returns as an instance of :drm:`<string>` the group (for example NT
     domain, or Windows Workgroup) of which the user logged on to the
     current machine is a member, or ``#f`` if the group is unavailable.

   :seealso:

     - :func:`login-name`

.. constant:: $machine-name

   Constant specifying the type of hardware installed in the host machine.

   :type: <symbol>
   :value: #"x86", #"x86-64", #"ppc"

   :description:

     This constant is a symbol that represents the type of hardware
     installed in the host machine.

   :seealso:

     - :const:`$architecture-little-endian?`
     - :const:`$os-name`
     - :const:`$os-variant`
     - :const:`$os-version`
     - :const:`$platform-name`

.. constant:: $os-name

   Constant specifying the operating system running on the host machine.

   :type: <symbol>
   :value: #"win32", #"linux", #"darwin", #"freebsd"

   :description:

     This constant is a symbol that represents the operating system
     running on the host machine.

   :seealso:

     - :const:`$architecture-little-endian?`
     - :const:`$machine-name`
     - :const:`$os-variant`
     - :const:`$os-version`
     - :const:`$platform-name`

.. constant:: $os-variant

   Constant specifying which variant of an operating system the current
   machine is running, where relevant.

   :type: <symbol>

   :description:

     This constant is a symbol value distinguishing between variants of
     the operating system identified by ``$os-name``, where relevant;
     otherwise it has the same value as ``$os-name``. On Windows, the
     possible values are ``#"win3.1"``, ``#"win95"``, ``#"win98"``, and
     ``#"winnt"``.

   :seealso:

     - :const:`$architecture-little-endian?`
     - :const:`$machine-name`
     - :const:`$os-name`
     - :const:`$os-version`
     - :const:`$platform-name`

.. constant:: $os-version

   Constant specifying which version of an operating system the current
   machine is running.

   :type: <string>

   :description:

     The constant *$os-version* is a string value that identifies the
     version of the operating system. For Windows NT, a typical value
     would be *"4.0.1381 Service Pack 3"*. For Windows 95, a typical
     value would be *"4.0.1212 B"*.

   :seealso:

     - :const:`$architecture-little-endian?`
     - :const:`$machine-name`
     - :const:`$os-name`
     - :const:`$os-variant`
     - :const:`$platform-name`

.. function:: owner-name

   Returns the name of the user who owns the current machine, if available.

   :signature: owner-name () => *name-or-false*

   :value name-or-false: An instance of ``false-or(<string>)``.

   :description:

     Returns as an instance of :drm:`<string>` the name of the user who
     owns the current machine (that is, the name entered when the
     machine was registered), or ``#f`` if the name is unavailable.

.. function:: owner-organization

   Returns the organization to which the user who owns the current
   machine belongs, if available.

   :signature: owner-organization () => *organization-or-false*

   :value organization-or-false: An instance of ``false-or(<string>)``.

   :description:

     Returns as an instance of :drm:`<string>` the organization to which
     the user who owns the current machine belongs, or ``#f`` if the
     name is unavailable.

.. function:: parent-process-id

   Returns the integer value for the parent process ID.

   :signature: parent-process-id => *pid*

   :value pid: An instance of :drm:`<integer>`.

   :description:

     Returns the integer value of the parent process ID.

     .. note:: This is not yet implemented on Windows.

   :seealso:

     - :func:`current-process-id`
     - :func:`current-thread-id`

.. constant:: $platform-name

   Constant specifying the operating system running on and the type of
   hardware installed in the host machine.

   :type: <symbol>
   :value: ``#"x86-win32"``, ``#"x86-linux"``, etc.

   :description:

     This constant is a symbol that represents the both the operating
     system running on, and the type of hardware installed in, the host
     machine. It is a combination of the :const:`$os-name` and
     :const:`$machine-name` constants.

   :example:

     ``#"x86-win32"``, ``#"x86_64-linux"``

   :seealso:

     - :const:`$machine-name`
     - :const:`$os-name`

.. function:: machine-concurrent-thread-count

   Return the number of concurrent execution threads available.

   :signature: machine-concurrent-thread-count => *count*

   :value count: An instance of :drm:`<integer>`.

   :description:

      Returns the number of execution threads currently
      available. This normally corresponds to the number of logical
      processor cores currently online, and may vary over the lifetime
      of the program.

.. function:: run-application

   Launches an application using the specified name and arguments.

   :signature: run-application *command* #key *minimize?* *activate?* *under-shell?* *inherit-console?* => *status*

   :parameter command: An instance of :drm:`<string>`.
   :parameter #key minimize?: An instance of :drm:`<boolean>`.
   :parameter #key activate?: An instance of :drm:`<boolean>`.
   :parameter #key under-shell?: An instance of :drm:`<boolean>`.
   :parameter #key inherit-console?: An instance of :drm:`<boolean>`.
   :value status: An instance of :drm:`<integer>`.

   :description:

     Launches an application using the name and arguments specified in
     command. Using this function is equivalent to typing the command in
     a MS-DOS window. The return value is the exit status returned by
     the application.

     If the *minimize?* keyword is ``#t``, the command's shell will
     appear minimized. It is ``#f`` by default.

     If the *activate?* keyword is ``#t``, the shell window becomes the
     active window. It is ``#t`` by default.

     If the *under-shell?* keyword is ``#t``, an MS-DOS shell is created
     to run the application; otherwise, the application is run directly.
     It is ``#f`` by default.

     If the *inherit-console?* keyword is ``#t``, the new application
     uses the same console window as the current application; otherwise,
     the new application is created with a separate console window. It
     is ``#t`` by default.

   :seealso:

     - :func:`exit-application`

