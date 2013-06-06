********************************
Porting to a New Target Platform
********************************

.. warning:: This is a work in progress, as we work through doing a new port.
   :class: alert alert-block alert-warning

Naming Your Target
==================

A target name typically consists of the CPU type and the OS name, separated
by a hyphen.  Due to using the hyphen as a separator, the CPU type and OS name
should not include a hyphen.

Example target names are:

* ``arm-linux``
* ``ppc-darwin``
* ``x86-win32``
* ``x86_64-linux``

CPU types are typically:

* ``#"arm"``
* ``#"ppc"``
* ``#"x86"``
* ``#"x86_64"``

OS names are typically:

* ``#"linux"``
* ``#"darwin"``
* ``#"freebsd"``
* ``#"win32"``

.. warning:: Some existing targets are poorly named.  We may attempt to
   rename ``#"win32"`` to ``#"windows"`` in the future.
   :class: alert alert-block alert-warning

.. warning:: We do not yet have a strategy in place for targets which
   are not for a particular CPU and OS type. Examples of these would be
   emscripten, the JVM, and Google's Native Client.
   :class: alert alert-block alert-warning

New Registry
============

The registry is how we map library names to the actual project files that
should be built to provide that library.  This lets us provide per-platform
implementations of libraries as required.

The registry can be found within ``sources/registry``.

The easiest way to do this is to copy an existing directory and modify it
as needed. If you are doing a port to a Unix-like platform, this should be
pretty straightforward to copy from one of the Linux definitions.

System Library
==============

New LID and definitions
-----------------------

You will need to create a new ``.lid`` file to point to the per-platform
files for the new target.  You will also need a ``*-operating-system.dylan``
file to provide a couple of definitions.

There are plenty of examples of this within ``sources/system`` for the
currently supported target platforms.

Magic Numbers
-------------

The system library requires some magic numbers for accessing struct members
defined within the standard C library.

To do this, compile ``sources/system/dump-magic-numbers.c`` for your target
platform and execute it on the target.  This will generate a Dylan file that
you can add to the system library.

C Back-End
==========

If the new processor type is a 64 bit processor, you will
want to update the C back-end appropriately:

* Edit ``sources/dfmc/c-back-end/c-back-end.dylan``
* Update the definition of ``back-end-word-size``

You will also need to do a couple of edits to the C run-time:

* Edit ``sources/dfmc/c-run-time/run-time.*``, looking for
  usages of ``__x86_64__``

.. warning:: We should improve this code within the C run-time.
   :class: alert alert-block alert-info

Additionally, there are some mappings involving ``OPEN_DYLAN_PLATFORM_NAME``
in ``sources/dfmc/c-run-time/Makefile.in`` that will need to be updated.

LLVM Back-End
=============

The per-platform configurations for the LLVM back-end can be found
within ``sources/dfmc/llvm-back-end/llvm-targets.dylan``.

When adding a new CPU type, add a new abstract back-end for the CPU with
the appropriate data layout and word size.

When adding a new OS type, add a new abstract back-end for the OS.

After that, you can create the new target back-end that inherits from the
appropriate CPU and OS specific abstract back-ends.  You may need to
override the data layout if a different ABI is required.

.. warning:: Someone should document how to correctly determine the
   appropriate data layout to use.
   :class: alert alert-block alert-info

Be sure to invoke ``register-back-end`` correctly with your new back-end
class.

Additionally, there are some mappings involving ``OPEN_DYLAN_PLATFORM_NAME``
in ``sources/lib/run-time/Makefile.in`` that will need to be updated.

*This isn't completely written yet.*

Build Scripts
=============

Build scripts are partially documented within :doc:`build-system`.
The existing build scripts can be found within ``sources/jamfiles``.

You will want to copy an existing one and make whatever changes are
required. When targeting a Unix-like platform, much of the logic is
already shared within ``sources/jamfiles/posix-build.jam``.

Autoconf
========

The ``configure.ac`` script handles detecting a target platform and
setting some appropriate variables within the build system. There
is a large block that deals with checking the ``$target`` (set up
by ``AC_CANONICAL_TARGET``) and configuring things appropriately.

After updating ``configure.ac``, be sure to re-run ``autogen.sh``
to create an updated ``configure`` script before re-running
``configure``.

Performing a Cross-Build
========================

*Going to have to work this out and then document it.*
