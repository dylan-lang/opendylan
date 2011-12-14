***********************************
Open Dylan Console Environment
***********************************

.. index:: console environment

In Open Dylan, you can develop Dylan applications using the
interactive development environment or the Open Dylan console
environment. This appendix describes the console binaries.

About the Dylan console compilers
=================================

.. index:: dylan-compiler, dylan-compiler-with-tools,
  dylan-environment, dylan-environment-with-tools

The Open Dylan console compiler is an executable application called
*dylan-compiler*. You can find it in the *bin* folder of your Open
Dylan installation. The console compiler is a command line alternative
for batch compilation.

The console environment is an executable called *dylan-environment*.
It is a command line alternative for performing any of the development
tasks you might perform in the regular Open Dylan environment. You can
use it as a batch compiler, or you can develop and debug applications
using the interactive mode interface.

Both console applications are available in two flavors, with and
without tools interface (including CORBA IDL (scepter) and OLE
(motley)). The binaries with the tools interface have *-with-tools*
appended to their name.

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


