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
``make-dylan-app``.  See the section :doc:`hello-world` to create the
project, and also make sure you have a registry entry for it.  See
:doc:`source-registries` if you're not sure how to set that up.

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

    Welcome to dswank - the Hacker Edition Version 2013.2 SLIME interface
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
