******************************
Open Dylan Console Environment
******************************

.. 1  Hello World
   2  Using Source Registries
   3  A Few More Quick Tips
   4  Using dylan-compiler interactively
   5  An example of dylan-environment interactive functionality
   6  Dylan Interactor Mode for Emacs (DIME)

.. contents:: Contents
   :local:

.. index:: console environment, dylan-compiler

In Open Dylan, you can develop Dylan applications using the IDE (on
Windows) or command-line tools.  The compiler executable is called
``dylan-compiler``.  There is a helper application called
``make-dylan-app``, which can be used to generate some boilerplate for
a new project, and finally there's ``dswank`` which is a back-end for
interactive development in Emacs.  This chapter describes these
command-line tools.

Hello World
===========

You have just downloaded Open Dylan and installed it in
``/opt/opendylan-2012.1``.  So how do you write the canonical Hello
World app?  This example assumes bash is being used.  You may need
to adjust for your local shell.  ::

  $ export PATH=/opt/opendylan-2012.1/bin:$PATH
  $ make-dylan-app hello-world
  $ cd hello-world
  $ dylan-compiler -build hello-world.lid
  ...lots of output...
  $ _build/bin/hello-world
  Hello, world!

Ta da!  Now a quick review of the steps with a little bit of
explanation.

First you must set PATH so that ``make-dylan-app`` and
``dylan-compiler`` will be found.  ``./_build/bin`` is where
dylan-compiler puts the executables it builds.

``make-dylan-app`` creates a directory with the same name as the
application and three files:

    1. hello-world.lid -- This says what other files are part of the
       project.  The order in which the files are listed here determines
       the order in which the code in them is loaded.

    2. library.dylan contains simple library and module definitions.
       These can be extended as your project grows more complex.

    3. hello-world.dylan contains the main program.

The first time you build hello-world it builds all used libraries, all
the way down to the dylan library itself.  Subsequent compiles will only
need to recompile hello-world itself and will therefore be much faster.

``dylan-compiler`` has both a batch mode and an interactive mode.  The
``-build`` option says to build the project in batch mode.  When you
pass a .lid file to the compiler it builds the library described by
that file.  In the next section you'll see that it can also pass the
name of the project (without ".lid") and it will use "registries" to
find the project sources.

The compiler places its output in the ``_build`` directory in the
current working directory. This includes the libraries and executables
that it builds.  You can run the executable as noted above from this
location.


Using Source Registries
=======================

Passing the name of a .lid file to ``dylan-compiler`` works great when
you have a single library that only uses other libraries that are part
of Open Dylan, but what if you want to use a second library that you
wrote yourself?  How will ``dylan-compiler`` find the sources for that
library?  The answer is registries.  For each Dylan library that isn't
part of Open Dylan itself, you create a file in the registry that
points to the .lid file for the library.  Here's an example for
hello-world::

  $ mkdir -p src/registry/generic
  $ echo abstract://dylan/hello-world/hello-world.lid > src/registry/generic/hello-world
  $ export OPEN_DYLAN_USER_REGISTRIES=`pwd`/src/registry

What's going on here?  First of all, the registry mechanism makes it
possible to have platform specific libraries.  Anything
platform-independent can be put under the "generic" directory.  Other
supported platform names are amd64-freebsd, x86-linux, x86-win32, etc.
For a full list see `the Open Dylan registry
<https://github.com/dylan-lang/opendylan/tree/master/sources/registry>`_.

Platform-specific registry directories are searched before the
"generic" registry, so if you have a library that has a special case
for Windows, you could use two registry entries: one in the
"x86-win32" directory and one in the "generic" directory.

Now let's look at the actual content of our hello-world registry file::

  abstract://dylan/hello-world/hello-world.lid

What this is doing is locating a file *relative to the directory that
the registry itself is in*.  If the "registry" directory is
``/home/you/dylan/registry`` then this registry file says the
hello-world .lid file is in
``/home/you/dylan/hello-world/hello-world.lid``.  "abstract://dylan/"
is just boilerplate.

Once you've set the ``OPEN_DYLAN_USER_REGISTRIES`` environment variable
to point to our new registry, ``dylan-compiler`` can find the
hello-world library source no matter what directory you're currently
working in.  You only need to specify the library name::

  $ cd /tmp
  $ dylan-compiler -build hello-world

You can add more than one registry to ``OPEN_DYLAN_USER_REGISTRIES`` by
separating them with colons::

  $ export OPEN_DYLAN_USER_REGISTRIES=/my/registry:/their/registry


A Few More Quick Tips
=====================

1. Add ``-clean`` to the command line to do a clean build::

     dylan-compiler -build -clean /my/project.lid

2. Use ``dylan-compiler -help`` to see all the options.  Options that
   don't take an argument may be negated by adding "no".  e.g. -nologo

3. The ``-build`` option builds an executable unless you add this
   line to your .lid file::

     target-type: dll

You should now have enough information to start working on your Dylan
project.  The next few sections go into more detail on using
``dylan-compiler``, which also has an interactive mode that can make
the edit/build/debug cycle a bit faster.  Or if you're an Emacs user
you may prefer to jump directly to the section on the `Dylan
Interactor Mode for Emacs (DIME)`_.


Using dylan-compiler interactively
==================================

The interactive mode of ``dylan-compiler`` allows you to carry out
multiple development tasks over a period of time without having to
restart the console compiler each time.  To start the console
environment in interactive mode, enter ``dylan-compiler`` without any
arguments at a shell. For example::

    $ dylan-compiler
    Hacker Edition
    Version 2012.1
    Copyright (c) 1997-2004, Functional Objects, Inc.
    Portions Copyright (c) 2004-2012, Dylan Hackers
    Portions Copyright (c) 2001-2012, Ravenbrook Ltd.
    >

If you've used the Open Dylan IDE on Windows, note that using
``dylan-compiler`` interactively is similar to working in the IDE's
interactor.

You can find a list of command groups by entering the command
``help``. The command groups in the console compiler are:

+------------------+----------------------------+
| Command Group    | Description                |
+==================+============================+
| *BASIC*          | basic commands             |
+------------------+----------------------------+
| *BROWSING*       | browsing commands          |
+------------------+----------------------------+
| *BUILD*          | project building commands  |
+------------------+----------------------------+
| *INTERNAL*       | internal commands          |
+------------------+----------------------------+
| *LIBRARY-PACKS*  | library packs commands     |
+------------------+----------------------------+
| *PROJECT*        | project commands           |
+------------------+----------------------------+
| *PROPERTY*       | property handling commands |
+------------------+----------------------------+
| *REGISTRY*       | registry commands          |
+------------------+----------------------------+
| *REPORTS*        | report commands            |
+------------------+----------------------------+
| *SYSTEM*         | operating system commands  |
+------------------+----------------------------+

You can use ``help -group group-name`` to view the available commands
and properties of a specific group.  You can also use ``help
command-name`` to view the full documentation of a command. We can see
the kind of information available by looking at the help entry for the
``help`` command::

    > help help
    Usage: :HELP [options*] [command]

    If specified with no arguments, HELP shows a list of all commands
    with a one line description. Help can display command options by
    specifying the name of the command. Additionally, it can display
    group or property help by specifying the GROUP or PROPERTY option.

    Arguments:
      COMMAND - the command to describe

    Options:
      -GROUP group - the command group to describe
      -PROPERTY property - the property to describe

Therefore, to find out what commands exist within the *PROJECT* command
group, type::

    > help -group project
    
    PROJECT:
    
    Commands applying to projects.
    
    Commands:
      CLOSE   closes the specified project
      IMPORT  imports a LID file
      OPEN    opens the specified project
    
    Properties:
      PROJECT   Current project
      PROJECTS  Open projects
    
    For documentation on a group, use:    HELP -GROUP group.
    For documentation on a command, use:  HELP command
    For a complete list of commands, use: SHOW COMMANDS

Then, to examine the ``OPEN`` command, type::

    > help open
    Usage: OPEN file
    
    Opens the specified project.
    
    Arguments:

      FILE - the filename of the project

Properties can be displayed via the ``show`` command.  For example to
see the value of the "projects" property listed previously, use ``show
projects``.

To exit the console environment, use the command ``exit``.

.. index:: command line

An example of dylan-environment interactive functionality
=========================================================

.. index:: dylan-environment

**Note:** ``dylan-environment`` is currently only supported on
Windows.  Unix users may wish to skip this section.

The dylan-environment has a few more options and command groups, which
will be presented briefly here:

+----------------------------+---------------------------------------------+
| Options                    | Description                                 |
+============================+=============================================+
| *-ARGUMENTS* *arguments*   | Arguments for the project’s application     |
+----------------------------+---------------------------------------------+
| *-PLAY*                    | Open and debug the playground project       |
+----------------------------+---------------------------------------------+
| *-START*                   | Start the project’s application             |
+----------------------------+---------------------------------------------+
| *-DEBUG*                   | Debug the project’s application             |
+----------------------------+---------------------------------------------+
| *-PROFILE*                 | Profile the execution of the application    |
+----------------------------+---------------------------------------------+
| *-SHARE-CONSOLE*           | Share the console with the application      |
+----------------------------+---------------------------------------------+

+--------------------+----------------------------+
| Command Group      | Description                |
+====================+============================+
| *BREAKPOINTS*      | breakpoint commands        |
+--------------------+----------------------------+
| *DEBUGGING*        | debugging commands         |
+--------------------+----------------------------+
| *MEMORY*           | memory viewing commands    |
+--------------------+----------------------------+
| *REMOTE-DEBUGGING* | remote debugging commands  |
+--------------------+----------------------------+
| *STACK*            | stack commands             |
+--------------------+----------------------------+

The following example demonstrates the console environment’s interactive
functionality. In the example, the user starts dylan-environment in
interactive mode, opens the playground project, performs some
arithmetic, defines a method, and then traces it::

    # dylan-environment
    Hacker Edition
    Version 2012.1
    Copyright (c) 1997-2004, Functional Objects, Inc.
    Portions Copyright (c) 2004-2012, Dylan Hackers
    Portions Copyright (c) 2001-2012, Ravenbrook Ltd.

    > play
    Opened project gui-dylan-playground
    Starting: gui-dylan-playground
    ? 1 + 2;
      $0 = 3
    ? define method factorial (x) if (x < 2) 1 else x * factorial(x - 1) end end;
    ? factorial(5);
      $1 = 120
    ? :trace factorial
    ? :set messages verbose
    Messages: verbose
    ? factorial(6);
    0: factorial (<object>): (6)
      1: factorial (<object>): (5)
        2: factorial (<object>): (4)
          3: factorial (<object>): (3)
            4: factorial (<object>): (2)
              5: factorial (<object>): (1)
              5: factorial (<object>) => (2)
            4: factorial (<object>) => (6)
          3: factorial (<object>) => (24)
        2: factorial (<object>) => (120)
      1: factorial (<object>) => (720)
    0: factorial (<object>) => (#[720])
      $2 = 720
    ? :exit

The commands described in this appendix can also be used in the Command
Line window within the regular Open Dylan development environment.
Choose **File > Command Line...** from the main window and use commands at
the *?* prompt.


Dylan Interactor Mode for Emacs (DIME)
======================================

DIME and its back-end, dswank, create a link between the Dylan
compiler and emacs so that editor commands can leverage everything the
compiler knows about your source code.  It allows you to view cross
references, locate definitions, view argument lists, compile your
code, browse class hierarchies, and more.  This section will give a
brief introduction to using DIME.

The first thing you need to use DIME is the emacs Lisp code for
dylan-mode, which can be downloaded from `the dylan-mode GitHub
repository <https://github.com/dylan-lang/dylan-mode>`_.  If you don't
have ready access to git there is a link on that page to download as a
.zip file.

Next set up your .emacs file as follows.   Adjust the pathnames to
match your Open Dylan installation location and the directory where
you put dylan-mode.  ::

    (add-to-list 'load-path "/path/to/dylan-mode")
    (setq inferior-dylan-program "/opt/opendylan/bin/dswank")
    (require 'dime)
    (dime-setup '(dime-dylan dime-repl))
    (setenv "OPEN_DYLAN_USER_REGISTRIES" "/path/to/your/registry:...more...")

Setting ``OPEN_DYLAN_USER_REGISTRIES`` is important because that's how
DIME finds your projects.

For this tutorial let's use a "dime-test" project created with
``make-dylan-app``.  See the section `Hello World`_ to create the
project, and also make sure you have a registry entry for it.  See
`Using Source Registries`_ if you're not sure how to set that up.

**Start dime:**  ::

    $ export PATH=/opt/opendylan/bin:$PATH
    $ cd ...dir containing registry...
    $ echo abstract://dylan/dime-test/dime-test.lid > registry/generic/dime-test
    $ make-dylan-app dime-test
    $ cd dime-test
    $ emacs dime-test.dylan
    M-x dime <Enter>

You should now have a buffer called ``*dime-repl nil*`` that looks
like this::

    Welcome to dswank - the Hacker Edition Version 2012.1 SLIME interface
    opendylan> 

This is the Open Dylan compiler interactive shell.  You can issue
commands directly here if you like, but mostly you'll issue dime
commands from your Dylan source buffers.

**Change projects:** Switch back to the dime-test.dylan buffer and
type ``C-c M-p dime-test`` to tell DIME to switch to the dime-test
project.  If DIME doesn't let you enter "dime-test" as the project
name that means it couldn't find the registry entry.  Press <Tab> to
see a complete list of available projects.

**Compile:** To build the project, type ``C-c C-k``.  You should see
something like "Compilation finished: 3 warnings, 18 notes".  (The
reason there are so many warnings is because there are some warnings
in the dylan library itself.  This is a bug that should be fixed
eventually.)

**Edit definition:** There's not much code in dime-test.dylan except
for a ``main`` method.  Move the cursor onto the call to "format-out"
and type ``M-.``.  It should jump to the format-out definition in the
``io-internals`` module.

**Compiler warnings:** Switch back to the dime-test.dylan buffer and
make a change that causes a compiler warning, such as removing the
semicolon at the end of the ``format-out`` line.  Recompile with ``C-c
C-k`` and you should see something like "Compilation finished: 6
warnings, 18 notes".  You can jump to the first warning using the
standard for emacs: ``C-x ```.

**Argument lists:** Note that when you type an open parenthesis, or
comma, or space after a function name dime will display the **argument
list** and return values in the emacs minibuffer.  e.g., try typing
``+(``.

**Cross references:** To list cross references (e.g., who calls
function F?) move the cursor over the name you want to look up and
type ``C-c C-w C-c`` ('c' for call).  DIME will display a list of
callers in a ``*dime-xref*`` buffer.  ``C-M-.`` will take you to the
next caller.  Use it repeatedly to move to each caller definition in
turn.  Move the cursor to a particular caller in the ``*dime-xref*``
buffer and press <Enter> to jump to that caller.

That should be enough to give you the flavor of DIME.  Following is a
table of useful commands, and you can of course find many more using
the standard emacs tools such as ``C-h b`` and ``M-x apropos``.

    +-------------------+------------------------------------------+
    | Keyboard shortcut | Effect                                   |
    +===================+==========================================+
    |M-x dime           |start dime                                |
    +-------------------+------------------------------------------+
    | , change-project  | change project (in the repl buffer)      |
    +-------------------+------------------------------------------+
    | C-c M-p           | change project (in Dylan source buffers) |
    +-------------------+------------------------------------------+
    | M-.               | jump to definition                       |
    +-------------------+------------------------------------------+
    | M-,               | jump backwards                           |
    +-------------------+------------------------------------------+
    | C-c C-k           | compile project                          |
    +-------------------+------------------------------------------+
    | C-c C-w C-a       | who specializes? (or who defines?)       |
    +-------------------+------------------------------------------+
    | C-c C-w C-r       | who references?                          |
    +-------------------+------------------------------------------+
    | C-c C-w C-b       | who binds?                               |
    +-------------------+------------------------------------------+
    | C-c C-w C-c       | who calls?                               |
    +-------------------+------------------------------------------+
