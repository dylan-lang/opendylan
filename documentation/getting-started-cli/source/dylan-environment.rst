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
    Version 2013.1
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