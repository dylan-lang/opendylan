Hello World
===========

You have just downloaded Open Dylan and installed it in
``/opt/opendylan-2013.1``.  So how do you write the canonical Hello
World app?  This example assumes bash is being used.  You may need
to adjust for your local shell.  ::

  $ export PATH=/opt/opendylan-2013.1/bin:$PATH
  $ make-dylan-app hello-world
  $ cd hello-world
  $ dylan-compiler -build hello-world.lid
  ...lots of output...
  $ _build/bin/hello-world
  Hello, world!

Ta da!  Now a quick review of the steps with a little bit of
explanation.

First you must set PATH so that ``make-dylan-app`` and
``dylan-compiler`` will be found.  ``./_build/bin`` is where
dylan-compiler puts the executables it builds.

.. note:: Some of these differ on Windows, so please be sure
   to read :doc:`windows` if you are on Windows.

``make-dylan-app`` creates a directory with the same name as the
application and three files:

1. hello-world.lid -- This says what other files are part of the
   project.  The order in which the files are listed here determines
   the order in which the code in them is loaded.

2. library.dylan contains simple library and module definitions.
   These can be extended as your project grows more complex.

3. hello-world.dylan contains the main program.

The first time you build hello-world it builds all used libraries, all
the way down to the dylan library itself.  Subsequent compiles will only
need to recompile hello-world itself and will therefore be much faster.

``dylan-compiler`` has both a batch mode and an interactive mode.  The
``-build`` option says to build the project in batch mode.  When you
pass a .lid file to the compiler it builds the library described by
that file.  In the next section you'll see that it can also pass the
name of the project (without ".lid") and it will use "registries" to
find the project sources.

The compiler places its output in the ``_build`` directory in the
current working directory. This includes the libraries and executables
that it builds.  You can run the executable as noted above from this
location.
