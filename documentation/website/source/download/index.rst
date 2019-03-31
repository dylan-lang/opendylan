********************
Open Dylan Downloads
********************

Current Version
===============

The current version is 2019.1, released March 31, 2019:

.. table::
   :class: table-striped

   +---------------------------+---------------------------------------------+---------------+------------------+
   | Platform                  | File                                        | Size (bytes)  | Install Notes    |
   +===========================+=============================================+===============+==================+
   | Windows (32 bit)          | `opendylan-2019.1-win32.exe`_               | 100643764     | `Windows Notes`_ |
   +---------------------------+---------------------------------------------+---------------+------------------+
   | Mac OS X Lion+ (Intel)    | `opendylan-2019.1-x86_64-darwin.tar.bz2`_   |  55077039     | `Unix Notes`_    |
   +---------------------------+---------------------------------------------+---------------+------------------+
   | Linux (x86)               | `opendylan-2019.1-x86-linux.tar.bz2`_       |  48674460     | `Unix Notes`_    |
   +---------------------------+---------------------------------------------+---------------+------------------+
   | Linux (x86_64)            | `opendylan-2019.1-x86_64-linux.tar.bz2`_    |  66704263     | `Unix Notes`_    |
   +---------------------------+---------------------------------------------+---------------+------------------+
   | FreeBSD (x86)             | `opendylan-2019.1-x86-freebsd.tar.bz2`_     |  49634759     | `Unix Notes`_    |
   +---------------------------+---------------------------------------------+---------------+------------------+
   | FreeBSD (x86_64)          | `opendylan-2019.1-x86_64-freebsd.tar.bz2`_  |  64495718     | `Unix Notes`_    |
   +---------------------------+---------------------------------------------+---------------+------------------+

The release may also be downloaded from the GitHub `opendylan release page
<https://github.com/dylan-lang/opendylan/releases/tag/v2019.1.0>`_.

See the `release notes
<http://opendylan.org/documentation/release-notes/index.html>`_ for
information about what changed since the previous version.

The `source code <https://github.com/dylan-lang/opendylan/tree/v2019.1.0>`_ is
available under an open source license in the `"dylan-lang" organization on
GitHub`_.

Installation on Windows
-----------------------

.. warning:: The 32 bit version of Open Dylan IDE does NOT work on 64 bit Windows
   prior to Windows 8. There is no workaround at this time, unfortunately. We
   hope to resolve this in an upcoming release.
   :class: alert alert-warning

   However, the command line tools should work as the problem is with interaction
   between threads, GC and WoW64.

For installation, double-click on the installer, and follow instructions.  You
need to have either the PellesC linker or the linker of VC++ 6.0, 7.0 or the
current .NET platform SDK installed. `PellesC 8.00
<https://www.pellesc.de/index.php?page=download&lang=en&version=8.00>` is the
best option.  (Note that PellesC 9.00 does not work.)

Your environment variables must be set such that the external build
system (linker, resource compiler, etc.) can be found.  For example,
for Pelles C you should set these environment variables in the System
control panel (assuming installation in ``C:\Program
Files\PellesC``)::

  INCLUDE=C:\Program Files\PellesC\include;C:\Program Files\PellesC\include\win
  LIB=C:\Program Files\PellesC\lib;C:\Program Files\PellesC\lib\win
  PATH=C:\Program Files\PellesC\bin;...more...

You may instead start a Pelles C interactive shell and run
``C:\Program Files\Open Dylan\bin\win32-environment.exe``, but this
won't help if you want to run Open Dylan via the Start menu.

Installation on Mac OS X, FreeBSD, Linux
----------------------------------------

Note that these versions only have a command-line compiler and no IDE.

* The README file inside the tarball describes installation and basic
  usage. The easiest way is extracting the tarball in /opt.

* The Linux and FreeBSD platforms should have gcc installed, in order to allow
  linking.

* All three platforms must have the Boehm GC installed.  For example, ``apt-get
  install libgc-dev`` on Ubuntu.


Installation from source
------------------------

You can read more about installing Open Dylan from the source files
in the `README <https://github.com/dylan-lang/opendylan/blob/master/README.rst>`_.
That file contains the instructions on how to obtain the source code, the required
dependencies and how to build it.

Get Started!
------------

If you're new to Dylan, you'll want to check out these handy resources:

* `Introduction to Dylan <http://opendylan.org/documentation/intro-dylan/>`_:
   A tutorial written for those with solid programming
   experience in C++ or another object-oriented, static language. It
   provides a gentler introduction to Dylan than does the Dylan
   Reference Manual (DRM).
* `Getting Started with the Open Dylan Command Line Tools <http://opendylan.org/documentation/getting-started-cli/>`_:
   An introduction to the usage of the command line tools.
* `Dylan Programming Guide <http://opendylan.org/books/dpg/>`_:
   A book length Dylan tutorial.
* `Open Dylan Documentation <http://opendylan.org/documentation/>`_:
   All of the Open Dylan documentation.

Older Versions
==============

Old builds can be found by browsing the `download directories`_.



.. _opendylan-2019.1-win32.exe: https://opendylan.org/downloads/opendylan/2019.1/opendylan-2019.1-win32.exe
.. _opendylan-2019.1-x86_64-darwin.tar.bz2: https://opendylan.org/downloads/opendylan/2019.1/opendylan-2019.1-x86_64-darwin.tar.bz2
.. _opendylan-2019.1-x86-linux.tar.bz2: https://opendylan.org/downloads/opendylan/2019.1/opendylan-2019.1-x86-linux.tar.bz2
.. _opendylan-2019.1-x86_64-linux.tar.bz2: https://opendylan.org/downloads/opendylan/2019.1/opendylan-2019.1-x86_64-linux.tar.bz2
.. _opendylan-2019.1-x86-freebsd.tar.bz2: https://opendylan.org/downloads/opendylan/2019.1/opendylan-2019.1-x86-freebsd.tar.bz2
.. _opendylan-2019.1-x86_64-freebsd.tar.bz2: https://opendylan.org/downloads/opendylan/2019.1/opendylan-2019.1-x86_64-freebsd.tar.bz2
.. _Windows Notes: #installation-on-windows
.. _Unix Notes: #installation-on-mac-os-x-freebsd-linux
.. _download directories: http://opendylan.org/downloads/opendylan/
.. _"dylan-lang" organization on GitHub: https://github.com/dylan-lang/
