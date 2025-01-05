***************************
The operating-system Module
***************************

.. current-library:: system
.. current-module:: operating-system

The operating-system module is part of the System library. It provides
an interface to some features of the host machine's operating system.

Manipulating environment information
------------------------------------

The operating-system module contains a number of interfaces for
examining and specifying information about the operating system
environment of the host machine. As well as providing constants that you
can use in your code, you can examine and set the value of any
environment variable in the system.

The following constants contain machine-specific information:

- :const:`$architecture-little-endian?`
- :const:`$machine-architecture`
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
     architecture this value is :drm:`#t`.

   :seealso:

     - :const:`$machine-architecture`
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
   :value value: An instance of :drm:`<byte-string>`, or :drm:`#f`.

   :description:

     Returns the value of the environment variable specified by *name*,
     or :drm:`#f` if there is no such environment variable.

   :seealso:

     - :func:`environment-variable-setter`

.. function:: environment-variable-setter

   Sets the value of an environment variable.

   :signature: environment-variable-setter *new-value* *name* => *new-value*

   :parameter new-value: An instance of :drm:`<byte-string>`, or :drm:`#f`.
   :parameter name: An instance of :drm:`<byte-string>`.
   :value new-value: An instance of :drm:`<byte-string>`, or :drm:`#f`.

   :description:

     Changes the value of the environment variable specified by *name*
     to *new-value*. If *new-value* is :drm:`#f`, the environment variable
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
   to the current machine, or :drm:`#f` if unavailable.

   :signature: login-name () => *name-or-false*

   :value name-or-false: An instance of ``false-or(<string>)``.

   :description:

     Returns as an instance of :drm:`<string>` the name of the user logged
     on to the current machine, or :drm:`#f` if unavailable.

   :seealso:

     - :func:`login-group`

.. function:: login-group

   :signature: login-group () => *group-or-false*

   :value group-or-false: An instance of ``false-or(<string>)``.

   :description:

     Returns as an instance of :drm:`<string>` the group (for example NT
     domain, or Windows Workgroup) of which the user logged on to the
     current machine is a member, or :drm:`#f` if the group is unavailable.

   :seealso:

     - :func:`login-name`

.. constant:: $machine-architecture

   Constant specifying the type of hardware installed in the host machine.

   :type: :drm:`<symbol>`
   :value: One of ``#"aarch64"``, ``#"arm"``, ``#"riscv64"``  ``#"x86"``, ``#"x86_64"``

   :description:

     This constant represents the execution platform's instruction set
     architecture.

     Note that this not always the same as the architecture of the hardware
     installed in the host machine. For example, when running ``x86_64`` code
     on Apple Silicon the value is ``#"x86_64``, not ``#"aarch64"``.

   :seealso:

     - :const:`$architecture-little-endian?`
     - :const:`$os-name`
     - :const:`$os-variant`
     - :const:`$os-version`
     - :const:`$platform-name`

.. constant:: $machine-name

   .. deprecated:: 2025.1
      Use :const:`$machine-architecture` instead.

   This constant will be removed in a future release.

.. constant:: $os-name

   Constant specifying the operating system running on the host machine.

   :type: :drm:`<symbol>`
   :value: One of  ``#"darwin"``, ``#"freebsd"``, ``#"linux"``, ``#"netbsd"``, ``#"win32"``

   :description:

     This constant is a symbol that represents the operating system
     running on the host machine.

   :seealso:

     - :const:`$architecture-little-endian?`
     - :const:`$machine-architecture`
     - :const:`$os-variant`
     - :const:`$os-version`
     - :const:`$platform-name`

.. constant:: $os-variant

   Constant specifying which variant of an operating system the current
   machine is running, where relevant.

   :type: :drm:`<symbol>`

   :description:

     This constant is a symbol value distinguishing between variants of
     the operating system identified by ``$os-name``, where relevant;
     otherwise it has the same value as ``$os-name``. On Windows, the
     possible values are ``#"win3.1"``, ``#"win95"``, ``#"win98"``, and
     ``#"winnt"``.

   :seealso:

     - :const:`$architecture-little-endian?`
     - :const:`$machine-architecture`
     - :const:`$os-name`
     - :const:`$os-version`
     - :const:`$platform-name`

.. constant:: $os-version

   Constant specifying which version of an operating system the current
   machine is running.

   :type: :drm:`<string>`

   :description:

     The constant *$os-version* is a string value that identifies the
     version of the operating system. For Windows NT, a typical value
     would be *"4.0.1381 Service Pack 3"*. For Windows 95, a typical
     value would be *"4.0.1212 B"*.

   :seealso:

     - :const:`$architecture-little-endian?`
     - :const:`$machine-architecture`
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
     machine was registered), or :drm:`#f` if the name is unavailable.

.. function:: owner-organization

   Returns the organization to which the user who owns the current
   machine belongs, if available.

   :signature: owner-organization () => *organization-or-false*

   :value organization-or-false: An instance of ``false-or(<string>)``.

   :description:

     Returns as an instance of :drm:`<string>` the organization to which
     the user who owns the current machine belongs, or :drm:`#f` if the
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

   :type: :drm:`<symbol>`
   :value: ``#"x86-win32"``, ``#"x86_64-darwin"``, etc.

   :description:

     Represents both the architecture and the operating system running on the
     host machine. It is a symbol created from the concatenation of
     :const:`$machine-architecture`, ``"-"``, and :const:`$os-name`.  See those
     constants to determine the full list of possible values.

   :example:

     ``#"x86-win32"``, ``#"x86_64-linux"``

   :seealso:

     - :const:`$machine-architecture`
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

   Launches an application in a new process, using the specified name and
   arguments.

   :signature: run-application *command* #key *minimize?* *activate?*
      *under-shell?* *inherit-console?* *outputter* *asynchronous?*
      *environment* *working-directory* *input* *if-input-does-not-exist*
      *output* *if-output-exists* *error* *if-error-exists* *hide?*
      => *status* *signal* *process* #rest *streams*

   :parameter command: An instance of :drm:`<sequence>`. Either a string
      containing the entire command or a sequence of strings representing the
      command as parsed by the shell. Example: ``"/bin/ls -l"`` or
      ``#["/bin/ls", "-l"]``

      .. note:: On Windows this must be a :drm:`<string>`, never a sequence of
                strings.

   :parameter #key under-shell?: An instance of :drm:`<boolean>`. If true (the
      default), use a shell to invoke the *command*. On Unix systems this is
      equivalent to ``/bin/sh -c '...command...'``. On Windows the
      :envvar:`COMSPEC` environment variable specifies which command
      interpreter to use.

   :parameter #key inherit-console?: An instance of :drm:`<boolean>`. Whether
      to run in the same session and process group as the calling process and
      therefore retain the same controlling TTY. Essentially, whether or not to
      call ``setsid()``. If you want the subprocess to be a daemon process, pass
      :drm:`#f`. The default is :drm:`#t`.

   :parameter #key outputter: An instance of :drm:`<function>`. A function with
      signature ``(buffer :: <string>, #key end)`` which will receive all output
      (both stdout and stderr) from the command.

   :parameter #key asynchronous?: An instance of :drm:`<boolean>`. If true,
      return immediately after creating the process. Otherwise, block until the
      command completes or is terminated by signal.

   :parameter #key environment: :drm:`#f` or an instance of
      :drm:`<explicit-key-collection>`.  A table mapping environment variable
      names (strings) to values (also strings). These values *augment* the
      environment in the current process. (There is currently no way to specify
      via this API that *environment* should be the only environment variables
      set in the subprocess.)

   :parameter #key working-directory: :drm:`#f` or an instance of
      :class:`<pathname>`. If not #f, the working directory of the subprocess
      is set to this directory.

   :parameter #key input: An instance of :class:`<pathname>` or one of the
      following symbols:

      * ``#"inherit"``: Inherit standard input from the calling process. Write
        to :var:`*standard-input*` to send input to the subprocess.
      * ``#"null"``: Use a null stream as standard input.
      * ``#"stream"``: Create and return a stream connected to the subprocess's
        standard input.
      * A :class:`<pathname>`: Open the specified file for reading and connect
        it to the subprocess's standard input.

   :parameter #key if-input-does-not-exist: Either ``#"signal"`` or
      ``#"create"``. The default is ``#signal``.

      * ``#"signal"``: Signal a :class:`<file-does-not-exist-error>` if
        ``input`` is a pathname that names a non-existent file.
      * ``#"create"``: Create an empty input file and connect it to standard
        input of the subprocess.

   :parameter #key output: An instance of :class:`<pathname>` or one of the
      following symbols:

      * ``#"inherit"``: Inherit standard output from the calling process.
      * ``#"null"``: Send standard output to a null stream.
      * ``#"stream"``: Create and return a stream connected to the subprocess's
        standard output.
      * A :class:`<pathname>`: Open the specified file for writing and connect
        it to the subprocess's standard output.

   :parameter #key if-output-exists: As for the ``if-exists`` option when
      creating an output :class:`<file-stream>` except that :drm:`#f` is not
      allowed.

   :parameter #key error: Possible values are the same as for the ``output``
      parameter except that they apply to :var:`*standard-error*`.

   :parameter #key if-error-exists: As for the ``if-exists`` option when
      creating an output :class:`<file-stream>` except that :drm:`#f` is not
      allowed.

   :parameter #key activate?: An instance of :drm:`<boolean>`.  If the
      *activate?* argument is :drm:`#t`, the shell window becomes the active
      window. The default is :drm:`#t`. (**Ignored on Unix platforms.**)

   :parameter #key minimize?: An instance of :drm:`<boolean>`. If :drm:`#t`, the
      command's shell window will appear minimized. The default is
      :drm:`#f`. (**Ignored on Unix platforms.**)

   :parameter #key hide?: An instance of :drm:`<boolean>`.  If :drm:`#t`, the
      window associated with this process will be hidden. The default is
      :drm:`#f`. (**Ignored on Unix platforms.**)

   :value status: An instance of :drm:`<integer>`. The exit status returned by
      ``waitpid`` (Unix) or ``WaitForSingleObject`` (Windows).

   :value signal: :drm:`#f` or an instance of :drm:`<integer>`. If the process
      was terminated by a signal this value is the signal number.

   :value process: :drm:`#f` or an instance of :class:`<application-process>`.  If
      ``asynchronous?`` is true, :func:`run-application` returns immediately
      and this value identifies the running process. See
      :func:`wait-for-application-process`, which may be used to wait for this
      process to terminate.

   :value #rest streams: Instances of :class:`<stream>`. Up to three streams
      are returned, always in the order stdin, stdout, stderr.  For example, if
      the arguments were ``input: #"null", output: #"stream", error: #"stream"``
      then two streams are returned: output and error.

   :description:

     Launches an application in a new process, using the name and arguments
     specified in *command*.

     Perhaps the simplest example is to run a command synchronously, with all
     input/output inherited from the parent shell and only looking at the exit
     status return value:

     .. code-block:: dylan

        let exit-status = run-application("/bin/ls foo");
        if (~zero?(exit-status))
          error("/bin/ls failed with status %d", exit-status);
        end;

   :seealso:

     - :func:`exit-application`
     - :func:`wait-for-application-process`

.. function:: wait-for-application-process

   Waits for a process to terminate.

   :signature: wait-format-application-process *process* => *status* *signal*

   :parameter process: An instance of :class:`<application-process>`.

   :value status: An instance of :drm:`<integer>`. The exit status returned by
      ``waitpid`` (Unix) or ``WaitForSingleObject`` (Windows).

   :value signal: :drm:`#f` or an instance of :drm:`<integer>`. If the process
      was terminated by a signal this value is the signal number.

