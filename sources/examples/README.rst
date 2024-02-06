Open Dylan example projects
===========================

Each sub-directory contains a standalone Dylan project. To compile them, you should have
a `working Open Dylan <https://opendylan.org/download/index.html>`_ installation.

To verify that your installation is working correctly, check the compiler's version::

    $ dylan-compiler -version
    Version 2024.1

If a version string doesn't appear, :command:`dylan-compiler` may not be on
your ``$PATH``. For help, check the `installation instructions
<https://opendylan.org/download/index.html>`_ again or reach out on one of the
`community's communication channels
<https://opendylan.org/community/index.html>`_.


Compiling examples
------------------

To compile any of the examples, run :command:`dylan-compiler -build
foo.lid`. For example::

    $ dylan-compiler -build console/hello-world/hello-world.lid

Executable files, if any, will appear in ``./_build/bin/``.

Some projects have multiple ``.lid`` files. Feel free to point the compiler at
each of them.  Think of a LID/HDP file as a manifest.

The first time the project compiles, it will take a bit longer while the
project's dependencies are compiled first. Subsequent builds will be *much*
faster.


Where to start
--------------

Experiment with the examples in the "console" directory. Each of them generates
one or more command-line utilities that explore various features of the Dylan
language.

- :file:`console/hello-world` demonstrates defining methods, printing to the
  console and not needing a "``main()`` function" for Dylan programs.
- :file:`console/factorial` demonstrates integer operations and composing
  libraries. :file:`factorial-big.lid` and :file:`factorial-small.lid` use the
  same base code to produce multiple applications with different integer
  internals.
- :file:`console/quicksort` demonstrates collections and polymorphic/generic
  methods and iteration.
- :file:`console/towers-of-hanoi` demonstrates defining your own objects.
