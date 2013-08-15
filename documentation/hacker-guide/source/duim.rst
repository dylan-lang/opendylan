***********************************
DUIM - Dylan User Interface Manager
***********************************

We have a lot to learn about hacking on DUIM, so there isn't
much to document yet as we haven't learned yet.

GTK Back-end
============

The GTK+ back-end is usable on both Mac OS X and Linux. It is
based on GTK+ 3.x. On Mac OS X, it assumes that the Quartz
back-end is being used rather than X11.

The bindings for GTK+ are largely generated from gobject-introspection
meta-data.  Some portions are hand-edited however or entirely done by
hand, like the Cairo bindings.  Most of the special cases for hand-editing
are put into the binding generator (`gir-dylan`_), but not all.

Getting GTK+ 3.x with Quartz on Mac OS X
----------------------------------------

Building GTK+ 3.x with Quartz on Mac OS X must be done in a specific way.
This currently can *not* be done with Homebrew.

Follow the `directions`_ on the GTK+ wiki, but make sure to build
GTK+ 3.x rather than 2.x. To do this, you'll want to use the moduleset
``meta-gtk-osx-gtk3`` rather than ``meta-gtk-osx-core``.

Once you have a build, be sure to add the appropriate directories to
your path.

We have not yet integrated GTK+'s Mac OS X integration module, so
the menu bar will not (yet) be integrated with the system menu bar
when running a DUIM application.

.. note:: GTK+ will be built as a 64 bit binary. This means that
   your Dylan code will also have to be built as a 64 bit binary.
   Doing this requires a build of Open Dylan from the master branch.
   Once you have a current build of Open Dylan, set the
   ``OPEN_DYLAN_TARGET_PLATFORM`` environment variable to
   ``x86_64-darwin`` and your builds will be 64 bit.

.. _gir-dylan: https://github.com/dylan-foundry/gir-dylan
.. _directions: https://wiki.gnome.org/GTK+/OSX/Building
