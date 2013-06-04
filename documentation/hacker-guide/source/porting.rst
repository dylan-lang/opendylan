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

.. warning:: Some existing targets are poorly named. The CPU type
   ``#"amd64"`` should not be used, instead favoring ``#"x86_64"``.
   Similarly, we may attempt to rename ``#"win32"`` to ``#"windows"``
   in the future.
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

LLVM Back-End
=============

The per-platform configurations for the LLVM back-end can be found
within ``sources/dfmc/llvm-back-end/llvm-targets.dylan``.

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

Performing a Cross-Build
========================

*Going to have to work this out and then document it.*