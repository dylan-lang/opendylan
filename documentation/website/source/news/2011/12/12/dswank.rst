:Author: Hannes Mehnert
:Date: 2011-12-12 23:30:00

Development inside of Emacs using DIME
======================================

We have implemented a Dylan backend for DIME using the Open Dylan
environment-protocols API to ease development of Dylan code on UNIX.
At present, cross references, locating definitions, retrieving
argument lists, compiling, warning reports and a class browser are
implemented.

.. figure:: dswank-screenshot.png
   :align: center

.. raw:: html

   <center><iframe width="480" height="360" src="http://www.youtube.com/embed/SIBlnDg0USQ?rel=0" frameborder="0" allowfullscreen></iframe></center>

When successfully installed, just run ``M-x dime`` to get a shell.
This is the same as the dylan-compiler shell, you can also use the
same commands.

When you program and hit ``(`` or ``,`` or ``(space)``, you get the
argument list of the method you are calling.

``M-x dime-dylan-browse-superclasses`` will show the superclass hierarchy
of the current class. ``M-x dime-dylan-browse-subclasses`` displays the
subclass hierarchy.

+-------------------+------------------------------------------+
| Keyboard shortcut | Effect                                   |
+===================+==========================================+
|M-x dime           |start dime                                |
+-------------------+------------------------------------------+
| , change-package  | select project (in the repl buffer)      |
+-------------------+------------------------------------------+
| M-.               | jump to definition                       |
+-------------------+------------------------------------------+
| M-,               | jump backwards                           |
+-------------------+------------------------------------------+
| C-c C-k           | compile project                          |
+-------------------+------------------------------------------+
| C-c C-w C-a       | who specializes?                         |
+-------------------+------------------------------------------+
| C-c C-w C-r       | who references?                          |
+-------------------+------------------------------------------+
| C-c C-w C-b       | who binds?                               |
+-------------------+------------------------------------------+
| C-c C-w C-c       | who calls?                               |
+-------------------+------------------------------------------+

Dswank uses the Open Dylan project registry, so make sure
``OPEN_DYLAN_USER_REGISTRIES`` is set properly before starting Emacs.

DIME is a fork of SLIME.

Installation
============

You need the following pieces:

   * `dylan-mode <https://github.com/dylan-lang/dylan-mode>`_, and extend your ``~/.emacs`` as documented in the README
   * dswank itself is shipped with the 2011.1 release
