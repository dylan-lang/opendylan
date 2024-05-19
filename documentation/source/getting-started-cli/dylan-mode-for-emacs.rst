Dylan Interactor Mode for Emacs (DIME)
======================================

DIME and its back-end, dswank, create a link between the Dylan
compiler and emacs so that editor commands can leverage everything the
compiler knows about your source code.  It allows you to view cross
references, locate definitions, view argument lists, compile your
code, browse class hierarchies, and more.  This section gives a
brief introduction to using DIME.

The first thing you need to use DIME is the emacs Lisp code for dylan-mode,
which can be downloaded from https://github.com/dylan-lang/dylan-emacs-support.

Next set up your :file:`.emacs` file as follows.  Adjust the pathnames to match
your Open Dylan installation location and the directory where you put the
dylan-emacs-support repository.

.. code-block:: emacs-lisp

   (add-to-list 'load-path "/path/to/dylan-mode")
   (require 'dime)
   (dime-setup '(dime-repl dime-note-tree))
   (setq dime-dylan-implementations
         '((opendylan ("/opt/opendylan/bin/dswank")
                      :env ("OPEN_DYLAN_USER_REGISTRIES=/tmp/dime-test/registry"))))

Setting :ref:`OPEN_DYLAN_USER_REGISTRIES <open-dylan-user-registries>` is important because
that's how DIME finds your projects. Above we set it to the dime-test registry
that is created in the example below.

For this tutorial let's use a "dime-test" project created with :program:`deft`.
Create the project with ::

   $ deft new application dime-test

which creates a library named "dime-test" and a corresponding executable
library and test suite, as well as downloading dependencies and creating
registry files.  See the `deft new application
<https://package.opendylan.org/deft/index.html#deft-new-application>`_
command for more.

**Start dime:**  ::

    $ export PATH=/opt/opendylan/bin:$PATH
    $ cd dime-test         # Created by deft new application, above.
    $ emacs dime-test-app.dylan
    M-x dime <Enter>

You should now have a buffer called ``*dime-repl nil*`` that looks
like this::

    Welcome to dswank - the Hacker Edition SLIME interface
    opendylan>

This is the Open Dylan compiler interactive shell.  You can issue
commands directly here if you like, but mostly you'll issue dime
commands from your Dylan source buffers.

**Change projects:** Switch back to the :file:`dime-test.dylan` buffer and type
``C-c M-p dime-test-app`` to tell DIME to switch to the dime-test-app project.
If DIME doesn't let you enter "dime-test-app" as the project name that means it
couldn't find the registry entry.  Make sure ``OPEN_DYLAN_USER_REGISTRIES``
(see above) is set correctly.

.. hint:: Press <Tab> to see a complete list of available projects and in the
          ``*dime-repl nil*`` buffer run the "show registries" command to see
          the active registries the order they're searched.

**Compile:** To build the project, type ``C-c C-k``.  You should see something
like "Compilation finished: 15 notes".  (The reason there are so many notes is
because there are some non-serious warnings in the dylan library itself.  This
is a bug that should be fixed eventually.)

**Edit definition:** There's not much code in :file:`dime-test-app.dylan`
except for a ``main`` function.  Move the cursor onto the call to "format-out"
and type ``M-.``.  It should jump to the format-out definition in the
``io-internals`` module.

**Compiler warnings:** Switch back to the :file:`dime-test-app.dylan` buffer
and make a change that causes a compiler warning, such as removing the
semicolon at the end of the ``format-out`` line.  Recompile with ``C-c C-k``
and you should see something like "Compilation finished: 3 warnings, 15 notes".
You can jump to the first warning using the standard for emacs: ``C-x ```.

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
    | M-,               | jump backwards (return from definition)  |
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
