Hello World
===========

You have just downloaded Open Dylan and installed it in ``/opt/opendylan``.  So
how do you write the canonical Hello World app?  This example assumes the
``bash`` shell is used.  You may need to adjust for your local shell.  ::

  $ export PATH=/opt/opendylan/bin:$PATH
  $ make-dylan-app hello-world
  $ cd hello-world
  $ dylan-compiler -build hello-world.lid
  ...lots of output...
  $ _build/bin/hello-world
  Hello, world!

Ta da!  Now a quick review of the steps with a little bit of
explanation.

First you must set ``PATH`` so that ``make-dylan-app`` and
``dylan-compiler`` will be found.  ``./_build/bin`` is where
dylan-compiler puts the executables it builds.

.. note:: Some of these differ on Windows, so please be sure
   to read :doc:`windows` if you are on Windows.
   :class: alert alert-block alert-warning

``make-dylan-app`` creates a directory with the same name as the
project, and four files:

1. ``hello-world.lid`` -- This says what other files are part of the project.
   The order in which the files are listed here determines the order in which
   the code in them is executed.

2. ``library.dylan`` contains the library and module definitions.  These can be
   extended as your project grows more complex.

3. ``hello-world.dylan`` contains the main program. Note that the last
   top-level definition is a call to the main function, which may have any
   name; there is no predefined "main" function that is automatically called.

4. ``registry/<platform>/hello-world`` is a "registry file", which contains the
   path to the ``hello-world.lid`` file.

The first time you build ``hello-world`` it builds all used libraries, all the
way down to the ``dylan`` library itself.  Subsequent compiles only need to
recompile ``hello-world`` itself and are therefore much faster.

``dylan-compiler`` has a batch mode and an interactive mode.  The ``-build``
option says to build the project in batch mode.  When you pass a ".lid" file to
the compiler it builds the library described by that file.  In the next section
you'll see that you can also pass the name of the project (without ".lid") and
it will use "registries" to find the project sources.

The compiler places its output in the ``_build`` directory in the
current working directory. This includes the libraries and executables
that it builds.  You can run the executable as noted above from this
location.
