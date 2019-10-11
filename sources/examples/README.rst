Open Dylan example projects
===========================

Each sub-directory contains a standalone Dylan project. To compile them, you should have
a `working Open Dylan <https://opendylan.org/download/index.html>`_ installation.

To verify that your installation is working correctly, try checking the compiler's version::

    $ dylan-compiler -version
    Version 2019.1

If a version string doesn't appear, ``dylan-compiler`` may not be on your ``$PATH``. For
help, check the `installation instructions <https://opendylan.org/download/index.html>`_ again
or reach out on one of the `community's communication channels <https://opendylan.org/community/index.html>`_.


Compiling examples
------------------

To compile any of the examples, run ``dylan-compiler`` with the ``-build`` option pointing at
a ``.lid`` or ``.hdp`` file::

    cd <project>
    dylan-compiler -build <project>.lid  # or <project>.hdp

Executable files, if any, will appear in ``_build/bin/``.

Some projects have multiple ``.lid``/``.hdp`` files. Feel free to point the compiler at each of them.
Think of a LID/HDP file as a manifest.
 
The first time that the project compiles, things will take a long time while the project's dependencies
are compiled first. Repeated builds will be _much_ faster.


Where to start
--------------

Experiment with the examples in the ``console`` directory. Each of them generates one or more CLI utilities
that explore various features of the Dylan language.

- ``console/hello-world`` demonstrates defining methods, printing to the console and not needing a "``main()`` function" for Dylan programs.
- ``console/factorial`` demonstrates integer operations and composing libraries. ``factorial-big.lid`` and `factorial-small.lid`` use the same base code to produce multiple applications with different integer internals.
- ``console/quicksort`` demonstrates collections and polymorphic/generic methods and iteration.
- ``console/towers-of-hanoi`` demonstrates defining your own objects.
