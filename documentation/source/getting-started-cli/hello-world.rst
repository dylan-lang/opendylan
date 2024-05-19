Hello World
===========

You have just downloaded Open Dylan and installed it in ``/opt/opendylan``.  So
how do you write the canonical Hello World app?  This example assumes the
``bash`` shell is used.  You may need to adjust for your local shell.  ::

  $ export PATH=/opt/opendylan/bin:$PATH
  $ deft new application --simple hello-world
  $ cd hello-world
  $ deft build --all
  ...lots of output...
  $ _build/bin/hello-world
  Hello, world!

Ta da!  Now a quick review of the steps with a little bit of explanation.

First you must set ``PATH`` so that the :program:`deft` and
:program:`dylan-compiler` executables will be found.  ``./_build/bin`` is where
:program:`dylan-compiler` puts the executables it builds.

.. note:: Some of these differ on Windows, so please be sure
   to read :doc:`windows` if you are on Windows.
   :class: alert alert-block alert-warning

``deft new application --simple hello-world`` creates a directory named
"hello-world", and several files. The ``--simple`` flag says to skip generating
a test suite library and instead of a separate ``hello-world`` library and
``hello-world-app`` executable library to just make one ``hello-world``
executable library.

1. :file:`hello-world.lid` lists the files in the project.  The order in which
   the files are listed here determines the order in which the code in them is
   loaded. The library definition file should always be first.

2. :file:`library.dylan` contains the library and module definitions.  These
   can be extended as your project grows in complexity. Note that the library
   and modules are defined in the pre-defined ``dylan-user`` module.

3. :file:`hello-world.dylan` contains the main program code. Note that this
   code is defined in the ``hello-world`` module of the ``hello-world``
   library.

4. The :file:`registry` directory is how :program:`dylan-compiler` locates each
   used library. :program:`deft` generates this directory for you. See
   :doc:`source-registries` for details on the registry format.

5. :file:`dylan-package.json` describes the new "hello-world" package. This is
   where you can specify dependencies, the package location, etc. See the `deft
   documentation <https://package.opendylan.org/deft/index.html>`_ for more on
   this. Note that the existence of this file turns the "hello-world" directory
   into a :program:`deft` workspace.

The first time you build your project all used libraries are built, all the
way down to the ``dylan`` library itself. Subsequent builds only need to
recompile ``hello-world`` itself and are therefore much faster.

The compiler places its output in the ``_build`` directory in the current
working directory. This includes the libraries and executables that it builds.
You can run the executable from this location, as noted above.
