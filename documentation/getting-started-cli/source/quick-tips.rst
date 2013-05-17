A Few More Quick Tips
=====================

1. Add ``-clean`` to the command line to do a clean build::

     dylan-compiler -build -clean /my/project.lid

2. Use ``dylan-compiler -help`` to see all the options.  Options that
   don't take an argument may be negated by adding "no".  e.g. -nologo

3. The ``-build`` option builds an executable unless you add this
   line to your .lid file::

     target-type: dll

4. If you miss having rich command line history and similar features,
   use ``rlwrap`` with ``dylan-compiler``.

You should now have enough information to start working on your Dylan
project.  The next few sections go into more detail on using
``dylan-compiler``, which also has an interactive mode that can make
the edit/build/debug cycle a bit faster.  Or if you're an Emacs user
you may prefer to jump directly to the section on the
:doc:`dylan-mode-for-emacs`.
