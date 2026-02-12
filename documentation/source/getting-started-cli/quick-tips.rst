A Few More Quick Tips
=====================

1. Add ``-clean`` to the command line to do a clean build::

     dylan-compiler -build -clean /my/project.lid

   However, note that there are `bugs
   <https://github.com/dylan-lang/opendylan/issues/1270>`_ related to the
   ``-clean`` flag so deleting the ``_build`` directory may be preferable.

2. Use ``dylan-compiler -help`` to see all the options.  Options that
   don't take an argument may be negated by adding "no".  e.g. -nologo

3. The ``-build`` option builds an executable unless you add this
   line to your .lid file::

     target-type: dll

4. If you miss having rich command line history and similar features,
   use ``rlwrap`` with ``dylan-compiler``.

5. If you're an Emacs user you may be interested in the `dylan-emacs-support
   <https://package.opendylan.org/dylan-emacs-support>`_ package, which provides Dylan
   editing support and DIME, an Emacs-based Dylan IDE.

You should now have enough information to start working on your Dylan
project.  The next few sections go into more detail on using
``dylan-compiler``, which also has an interactive mode that can make
the edit/build/debug cycle a bit faster.
