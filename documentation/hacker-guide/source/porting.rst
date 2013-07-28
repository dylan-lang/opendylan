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

* Edit ``sources/lib/run-time/run-time.*``, looking for
  usages of ``__x86_64__``

.. warning:: We should improve this code within the C run-time.
   :class: alert alert-block alert-info

Additionally, there are some mappings involving ``OPEN_DYLAN_TARGET_PLATFORM``
in ``sources/lib/run-time/Makefile.in`` that will need to be updated.

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

Additionally, there are some mappings involving ``OPEN_DYLAN_TARGET_PLATFORM``
in ``sources/lib/run-time/Makefile.in`` that will need to be updated.

*This isn't completely written yet.*

Build Scripts
=============

Build scripts are partially documented within :doc:`build-system`.
The existing build scripts can be found within ``sources/jamfiles``.

You will want to copy an existing one and make whatever changes are
required. When targeting a Unix-like platform, much of the logic is
already shared within ``sources/jamfiles/posix-build.jam``.

You should also add your new build script to ``sources/jamfiles/Makefile.in``
so that it gets installed.

Autoconf
========

The ``configure.ac`` script handles detecting a target platform and
setting some appropriate variables within the build system. There
is a large block that deals with checking the ``$host`` (set up
by ``AC_CANONICAL_TARGET``) and configuring things appropriately.

After updating ``configure.ac``, be sure to re-run ``autogen.sh``
to create an updated ``configure`` script before re-running
``configure``.

Performing a Cross-Build
========================

In the examples below, we will use ``arm-linux`` as the example.

Preparing the Garbage Collector
-------------------------------

Currently, most ports of Open Dylan will probably be using the Boehm
garbage collector rather than MPS.  An easy way to get the required
files is to install the Boehm GC on the target platform and then
copy the include and library files back to the build machine being
used to perform cross-compilation.

If you intend to start with MPS, you will need to ensure that the MPS
has been ported to the target platform first. This is an undertaking
that is outside the scope of this document.

Building the Run-Time
---------------------

You can cross-compile the run-time by going to ``sources/lib/run-time``
and running ``make install``, however, you will need to pass some special
flags to ``make``:

``CC``
    This should point to your cross-compiler, along with any special
    compilation flags that are required, such as the include paths
    for the garbage collector, or target CPU flags.

``OPEN_DYLAN_TARGET_PLATFORM``
    This should be the name of the target platform for which you are
    cross-compiling.

``OPEN_DYLAN_USER_INSTALL``
    This points to the path to the installation of Open Dylan which
    you will be using to perform cross-compilation.

An example command line might look like::

    make CC="arm-linux-gnueabihf-gcc -I/path/to/gc/include" \
      OPEN_DYLAN_TARGET_PLATFORM=arm-linux \
      OPEN_DYLAN_USER_INSTALL=/opt/opendylan-current \
      clean install

Creating a Custom Build Script
------------------------------

When cross-compiling, it is best to set up a custom Jam build
script which can be passed to the ``dylan-compiler``.  This will
allow you to customize important parts of the build configuration.

An example custom script might look like::

    CC = /opt/arm-linux/tools/arm-bcm2708/gcc-linaro-arm-linux-gnueabihf-raspbian/bin/arm-linux-gnueabihf-gcc ;
    GC_CFLAGS = -I/opt/arm-linux/gc/include -DGC_USE_BOEHM -DGC_THREADS ;
    GC_LFLAGS = -L/opt/arm-linux/gc/lib -lgc ;

    include $(SYSTEM_ROOT)/lib/arm-linux-build.jam ;

This just overrides the default values for some variables and then
includes the system-provided build script for ``arm-linux``.

Cross-Building a Test Application
---------------------------------

Assuming that you've cross-compiled and installed a copy of the
run-time into the version of Open Dylan that you're using for
cross-compilation, this is an easy step::

    OPEN_DYLAN_TARGET_PLATFORM=arm-linux \
      dylan-compiler -build-script path/to/custom-build.jam \
        -build hello-world

This should create a build of the ``hello-world`` application
in the ``_build`` directory. This directory can be copied to the
target machine and executed. Hopefully it runs correctly. If not,
now is the time to start debugging.

Cross-Building the Dylan Compiler
---------------------------------

This is no different from building the ``hello-world`` application
except that now you are building ``dylan-compiler``::

    OPEN_DYLAN_TARGET_PLATFORM=arm-linux \
      dylan-compiler -build-script path/to/custom-build.jam \
        -build dylan-compiler

Good luck!
