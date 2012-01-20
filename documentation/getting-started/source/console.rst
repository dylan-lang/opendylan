******************************
Open Dylan Console Environment
******************************

.. index:: console environment, dylan-compiler

In Open Dylan, you can develop Dylan applications using the IDE or
command-line tools.  The compiler executable is called
``dylan-compiler``.  There is a helper application called
``make-dylan-app``, which can be used to generate some boilerplate for
a new project, and finally there's ``dswank`` which is a back-end for
interactive development in Emacs.  This appendix describes these
command-line tools.

Hello World
===========

You have just downloaded Open Dylan and installed it in
``/opt/opendylan-2012.1``.  So how do you write the canonical Hello
World app?  This example assumes bash is being used.  You may need
to adjust for your local shell.  ::

  $ export PATH=~/Open-Dylan/bin:/opt/opendylan-2012.1/bin:$PATH
  $ make-dylan-app hello-world
  $ cd hello-world
  $ dylan-compiler -build hello-world.lid
  ...lots of output...
  $ hello-world
  Hello, world!

Ta da!  Now a quick review of the steps with a little bit of
explanation.

First you must set PATH so that ``make-dylan-app`` and
``dylan-compiler`` will be found.  You must add ``~/Open-Dylan/bin``
to the PATH as well because this is where ``dylan-compiler`` puts the
executables it builds.

``make-dylan-app`` creates a directory with the same name as the
application and three files:

    1. hello-world.lid -- This says what other files are part of the
       project.  The order in which the files are listed here determines
       the order in which the code in them is loaded.

    2. hello-world-exports.dylan contains simple library and module
       definitions.  These can be extended as your project grows more
       complex.

    3. hello-world.dylan contains the main program logic.

``dylan-compiler`` has both a batch mode and an interactive mode.  The
``-build`` option says to build the project in batch mode.  When you
pass a .lid file to the compiler it builds the library described by
that file.  In the next section you'll see that it can also pass the
name of the project (without ".lid") and it will use "registries" to
find the project sources.


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


Using dylan-compiler in batch mode
==================================

To use the Open Dylan console compiler in batch mode, go to a shell,
and enter *dylan-compiler*, followed by command options and a list of
one or more projects to perform the commands upon (and, optionally, a
list of projects to compile). The basic form of this call is::

    dylan-compiler [*options* ] [*project* ]

The default behavior is to open the project. You can specify projects
with a pathname to either a project file (*.hdp* ) or a LID (*.lid* )
file.

Therefore::

    dylan-compiler c:\users\dylan\projects\*my-project*\*my-project*.lid

opens the project *my-project*. The console compiler prints an error
and exits if the given file is not a project file or a project name.

The following options are available for use with *dylan-compiler* :

+----------------------------+---------------------------------------------+
| Options                    | Description                                 |
+============================+=============================================+
| *-HELP*                    | Print help                                  |
+----------------------------+---------------------------------------------+
| *-BUILD-SCRIPT* file       | Use the provided (Jam) build script         |
+----------------------------+---------------------------------------------+
| *-TARGET* symbol           | Type of the executable: "dll" (shared       |
|                            | library) or "executable"                    |
+----------------------------+---------------------------------------------+
| *-ARCH* symbol             | Architecture ("i386" or "x86_64")           |
+----------------------------+---------------------------------------------+
| *-LOGO*                    | Print the copyright information             |
+----------------------------+---------------------------------------------+
| *-VERSION*                 | Print the version string                    |
+----------------------------+---------------------------------------------+
| *-SHORTVERION*             | Print  the short version string             |
+----------------------------+---------------------------------------------+
| *-DEBUGGER*                | Enter the debugger if the compiler crashes  |
+----------------------------+---------------------------------------------+
| *-ECHO-INPUT*              | Echoes all console-dylan input to the       |
|                            | console                                     |
+----------------------------+---------------------------------------------+
| *-IMPORT*                  | Import a *.LID* file and generate a *.HDP*  |
|                            | file                                        |
+----------------------------+---------------------------------------------+
| *-BUILD*                   | Build and link the project                  |
+----------------------------+---------------------------------------------+
| *-COMPILE*                 | Compile the project                         |
+----------------------------+---------------------------------------------+
| *-LINK*                    | Link the project                            |
+----------------------------+---------------------------------------------+
| *-CLEAN*                   | Force a clean build of the project          |
+----------------------------+---------------------------------------------+
| *-RELEASE*                 | Build a release for the project             |
+----------------------------+---------------------------------------------+
| *-SUBPROJECTS*             | Build subprojects as well if necessary      |
+----------------------------+---------------------------------------------+
| *-FORCE*                   | Force relink the executable                 |
+----------------------------+---------------------------------------------+
| *-PERSONAL-ROOT* directory | Personal root directory (build products go  |
|                            | here                                        |
+----------------------------+---------------------------------------------+
| *-SYSTEM-ROOT* directory   | System root directory                       |
+----------------------------+---------------------------------------------+
| *-INTERNAL-DEBUG* list     | List of targets to print debug messages (e. |
|                            | g. linker, project-manager)                 |
+----------------------------+---------------------------------------------+
| *-UNIFY*                   | Combine libraries into a single executable  |
+----------------------------+---------------------------------------------+
| *-PROFILE-COMMANDS*        | Profile the execution of each command       |
+----------------------------+---------------------------------------------+
| *-HARP*                    | Generate HARP output file                   |
+----------------------------+---------------------------------------------+
| *-ASSEMBLE*                | Generate assembly-language output file      |
+----------------------------+---------------------------------------------+
| *-DFM*                     | Generate DFM output file                    |
+----------------------------+---------------------------------------------+

Examples:

#. Compile and link a library as an executable (EXE) file, you can do
   this in two ways::

    dylan-compiler -build *my-executable*

    dylan-compiler -compile -link *my-executable*

Recompile a project from scratch and link it as an executable::

    dylan-compiler -build -clean c:/dylan/*my-project*.hdp

The options that do not take arguments are flags that can be turned on
and off. By default, only ``-logo`` and ``-subprojects`` are turned on. To
turn flags off, precede the option with “*-no* …”, for instance:
``-nologo`` and ``-nosubprojects``.


Using dylan-compiler interactively
==================================

The interactive mode of the console compiler allows you to carry out
multiple development tasks over a period of time without having to
restart the console compiler each time. To start the console
environment in interactive mode, double-click *dylan-compiler* in the
*bin* folder of your Dylan installation, or enter *dylan-compiler*
without any arguments at a shell. For example::

    # dylan-compiler
    Hacker Edition
    Version 2011.1
    Copyright (c) 1997-2004, Functional Objects, Inc.
    Portions Copyright (c) 2004-2011, Dylan Hackers
    Portions Copyright (c) 2001-2002, Ravenbrook Ltd.
    >

Working at the prompt within the Dylan console compiler is similar
to working in the interactor in the regular Open Dylan development
environment (in other words, in the interaction pane in the Debugger).

You can find a list of command groups by entering the command
``help`` at the command line. The command groups in the console
compiler are:

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

For full documentation on a command, use: ``HELP /COMMAND command``.

Then, to examine the ``OPEN`` command, type::

    > help open
    Usage: OPEN file
    
    Opens the specified project.
    
    Arguments:

      FILE - the filename of the project

To exit the console environment, use the command ``exit``.

.. index:: command line

An example of dylan-environment interactive functionality
=========================================================

.. index:: dylan-environment, dylan-environment-with-tools

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
    Version 2011.1
    Copyright (c) 1997-2004, Functional Objects, Inc.
    Portions Copyright (c) 2004-2011, Dylan Hackers
    Portions Copyright (c) 2001-2002, Ravenbrook Ltd.

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


