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