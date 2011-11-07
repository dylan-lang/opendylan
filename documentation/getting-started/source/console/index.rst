***********************************
Open Dylan Console Environment
***********************************

In Open Dylan, you can develop Dylan applications using the
interactive development environment or the Open Dylan console
environment. This appendix describes the console environment, which is
called *console-dylan*.

About the Dylan console environment
===================================

The Open Dylan console environment is an executable application
called *console-dylan.exe*. You can find it in the *bin* folder of your
Open Dylan installation.

The console environment is a command line alternative for performing any
of the development tasks you might perform in the regular Open
Dylan environment. You can use it as a batch compiler, or you can
develop and debug applications using the interactive mode interface.

Using console-dylan in batch mode
=================================

To use the Open Dylan console environment in batch mode, go to an
MS-DOS prompt, and enter *console-dylan*, followed by command options
and a list of one or more projects to perform the commands upon (and,
optionally, a list of projects to compile). The basic form of this call
is:

console-dylan [*options* ] [*project* ]

The default behavior is to open the project. You can specify projects
with a pathname to either a project file (*.HDP* ) or LID (*.LID* )
file.

Therefore:

console-dylan c:\\users\\dylan\\projects\\*my-project* \\*my-project*
.hdp

opens the project *my-project*. The console environment prints an error
and exits if the project does not exist.

The following options are available for use with *console-dylan* :

Options

Description

*/ARGUMENTS* *arguments*

Arguments for the project’s application

*/BUILD*

Build the project

*/CLEAN*

Force a clean build of the project

*/COMPILE*

Compile the project

*/DEBUG*

Debug the project’s application

*/DEBUGGER*

Enter the debugger if the compiler crashes

*/ECHO-INPUT*

Echoes all console-dylan input to the console

*/FORCE*

Force relink the executable

*/HELP*

Show this help summary

*/IMPORT*

Import a *.LID* file to generate an *.HDP* file

*/LINK*

Link the project

*/LINKER* *linker* **

The linker to use

*/LOGO*

Displays the Open Dylan copyright banner

*/PLAY*

Open and debug the playground project

*/RELEASE*

Build a release for the project

*/SHARE-CONSOLE*

Share the console with the application

*/START*

Start the project’s application

*/SUBPROJECTS*

Build subprojects as well if necessary

*/TARGET* *target* **

The target

Examples:

#. Compile and link a library as an executable (EXE) file, you can do
   this in two ways::

    console-dylan /build *my-executable*

    console-dylan /compile /link *my-executable*

Compile a library and link it with the GNU linker::

    console-dylan /build /linker gnu *my-dll*

Recompile a project from scratch and link it as an executable::

    console-dylan /build /clean c:/dylan/*my-project*.hdp

The options that do not take arguments are flags that can be turned on
and off. By default, only */logo* and */subprojects* are turned on. To
turn flags off, precede the option with “*/no* …”, for instance:
*/nologo* and */nosubprojects*.

Using console-dylan interactively
=================================

The interactive mode of the console environment allows you to carry out
multiple development tasks over a period of time without having to
restart the console environment each time. To start the console
environment in interactive mode, double-click *console-dylan.exe* in the
*bin* folder of your Dylan installation, or enter *console-dylan*
without any arguments at an MS-DOS prompt. For example::

    MS-DOS> console-dylan
    Harlequin(R) Dylan(TM) …
    Version …
    Copyright (c) 1997-1999, Harlequin Group plc.
    All rights reserved.
    >

Working at the prompt within the Dylan console environment is equivalent
to working in the interactor in the regular Open Dylan development
environment (in other words, in the interaction pane in the Debugger).

Console environment commands must be preceded by a colon, such as
*:start*. You can find a list of command groups by entering the command
*:help* at the command line. The command groups in the console
environment are:

Command Group

Description

*BASIC*

basic commands

*BREAKPOINTS*

breakpoint commands

*BROWSING*

browsing commands

*BUILD*

project building commands

*DEBUGGING*

debugging commands

*MEMORY*

memory viewing commands

*PROJECT*

project commands

*PROPERTY*

property handling commands

*REPORTS*

report commands

*STACK*

stack commands

*SYSTEM*

operating system commands

You can also use *:help* *command-name* to view the full documentation
of a command. We can see the kind of information available by looking at
the help entry for the *:help* command:

> :help help

Usage: :HELP [options\*] [command]

If specified with no arguments, HELP shows a list of all commands with a
one line description. Help can also show documentation for a command
group, a command or a command property if the /group, /command or
/property options are specified.

Arguments:

COMMAND - the command to describe

Options:

GROUP - the command group to describe

PROPERTY - the property to describe

Therefore, to find out what commands exist within the *STACK* command
group, type:

> :help /group stack

STACK:

Commands to handle an application’s stack.

Commands:

:BACKTRACE displays the stack backtrace

:BOTTOM selects the bottom stack frame

:DOWN selects a frame further down the stack

:TOP selects the top stack frame

:UP selects a frame further up the stack

Properties:

FRAME Current stack frame

For full documentation on a command, use: HELP /COMMAND command.

Then, to examine the *:DOWN* command, type:

> :help down

Usage: :DOWN [count]

Selects a frame further down the stack.

Arguments:

COUNT - number of frames to move down

To exit the console environment, use the command *:exit*.

An example of console-dylan interactive functionality
=====================================================

The following example demonstrates the console environment’s interactive
functionality. In the example, the user starts console-dylan in
interactive mode, opens the playground project, performs some
arithmetic, defines a method, and then traces it:

MS-DOS> console-dylan

Harlequin(R) Dylan(TM) …

Version …

Copyright (c) 1997-1999, Harlequin Group plc.

All rights reserved.

> :play

Opened project dylan-playground

Starting: dylan-playground

> 1 + 2;

$0 = 3

> define method factorial (x) if (x < 2) 1 else x \* factorial(x - 1)
end end;

> factorial(5);

$1 = 120

> :trace factorial

> :set messages verbose

Messages: verbose

> factorial(6);

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

> :exit

d:\\users\\dylan\\builds>

The commands described in this appendix can also be used in the Command
Line window within the regular Open Dylan development environment.
Choose *File > Command Line…* from the main window and use commands at
the *?* prompt.


