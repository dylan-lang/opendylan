:Author: Hannes Mehnert
:Date: 2011-12-12 23:30:00

Development inside of Emacs using SLIME
=======================================

We have implemented a Dylan backend for SLIME using the Open Dylan
environment-protocols API to ease development of Dylan code on UNIX.
At present, cross references, locating definitions, retrieving
argument lists, compiling, warning reports and a class browser are
implemented.

.. figure:: dswank-screenshot.png
   :align: center

.. raw:: html

   <center><iframe width="480" height="360" src="http://www.youtube.com/embed/SIBlnDg0USQ?rel=0" frameborder="0" allowfullscreen></iframe></center>

When successfully installed, just run ``M-x slime`` to get a shell.
This is the same as the dylan-compiler shell, you can also use the
same commands.

When you program and hit ``(`` or ``,`` or ``(space)``, you get the
argument list of the method you are calling.

``M-x slime-dylan-browse-superclasses`` will show the superclass hierarchy
of the current class. ``M-x slime-dylan-browse-subclasses`` displays the
subclass hierarchy.

+-------------------+------------------------------------------+
| Keyboard shortcut | Effect                                   |
+===================+==========================================+
|M-x slime          |start slime                               |
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


Installation
============

You need the following pieces:

   * `SLIME <http://opendylan.org/~hannes/slime.tar.gz>`_ - since the CVS is a moving target, this is a safe version to use (February 2011 snapshot)
   * `dylan-mode <https://github.com/dylan-lang/dylan-mode>`_, and extend your ``~/.emacs`` as documented in README)
   * dswank itself is shipped with the 2011.1 release
