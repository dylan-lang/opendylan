********************
Open Dylan Downloads
********************

Current Version
===============

The current version is 2020.1, released October 10, 2020:

.. table::
   :class: table-striped

   +---------------------------+----------------------------------------------+---------------+
   | Platform                  | File                                         | Size (bytes)  |
   +===========================+==============================================+===============+
   | AArch64                   | `opendylan-2020.1pre-aarch64-linux.tar.bz2`_ | 111015768     |
   +---------------------------+----------------------------------------------+---------------+
   | FreeBSD (x86_64)          | `opendylan-2020.1-x86_64-freebsd.tar.bz2`_   |  64495718     |
   +---------------------------+----------------------------------------------+---------------+
   | Linux (x86_64)            | `opendylan-2020.1-x86_64-linux.tar.bz2`_     |  66704263     |
   +---------------------------+----------------------------------------------+---------------+
   | Mac OS X Lion+ (Intel)    | `opendylan-2020.1-x86_64-darwin.tar.bz2`_    |  55077039     |
   +---------------------------+----------------------------------------------+---------------+
   | Windows (32 bit)          | `opendylan-2020.1-win32.exe`_                | 100643764     |
   +---------------------------+----------------------------------------------+---------------+

.. raw:: html

   <br/>
   <br/>

(Previous builds can be found `on GitHub
<https://github.com/dylan-lang/opendylan/releases>`_ or in the `download
directories`_.)

See the `release notes
<http://opendylan.org/documentation/release-notes/index.html>`_ for
information about what changed since the previous version.

The `source code <https://github.com/dylan-lang/opendylan/tree/v2020.1.0>`_ is
available under an open source license in the `"dylan-lang" organization on
GitHub`_.


Installation on Unix
====================

Note that Unix versions only have a command-line compiler and no IDE.

Starting with the 2020.1 release all required dependencies (llvm, libgc,
libunwind) are included; just unpack the tarball and add the ``bin`` directory
to your ``PATH``::

  $ export PATH=/path/to/opendylan/bin:$PATH

You may wish to ``sudo apt install lldb-10``, for debugging.

For 2019.1 and earlier releases:

* All Unix platforms must have the Boehm GC and ``libunwind`` installed.
  For example, ``apt-get install libgc-dev libunwind-dev`` on Ubuntu.

* The README file inside the tarball describes installation and basic
  usage. The easiest way is extracting the tarball in /opt.

* The Linux and FreeBSD platforms should have gcc installed, in order to allow
  linking.

* On Arch Linux you may use the following recipe instead::

    git clone https://aur.archlinux.org/opendylan.git
    cd opendylan
    makepkg -si

* On macOS you may use this recipe instead::

    brew tap dylan-lang/dylan
    brew install opendylan       # or brew upgrade opendylan


Installation on Windows
=======================

For installation, double-click on the installer, and follow instructions.  You
need to have either the PellesC linker or the linker of VC++ 6.0, 7.0 or the
current .NET platform SDK installed. `PellesC 8.00
<https://www.pellesc.de/index.php?page=download&lang=en&version=8.00>`_ is the
best option.  (Note that PellesC 9.00 does not work.)

Your environment variables must be set such that the external build system
(linker, resource compiler, etc.) can be found.  For example, for Pelles C set
these environment variables in the System control panel (assuming installation
in ``C:\Program Files\PellesC``)::

  INCLUDE=C:\Program Files\PellesC\include;C:\Program Files\PellesC\include\win
  LIB=C:\Program Files\PellesC\lib;C:\Program Files\PellesC\lib\win
  PATH=C:\Program Files\PellesC\bin;...more...

You may instead start a Pelles C interactive shell and run
``C:\Program Files\Open Dylan\bin\win32-environment.exe``, but this
won't help if you want to run Open Dylan via the Start menu.


Get Started!
============

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



.. _opendylan-2020.1pre-aarch64-linux.tar.bz2: https://github.com/dylan-lang/opendylan/releases/download/v2020.1pre/opendylan-2020.1pre-aarch64-linux.tar.bz2
.. _opendylan-2020.1-win32.exe: https://github.com/dylan-lang/opendylan/releases/download/v2020.1pre/opendylan-2020.1pre-win32.exe
.. _opendylan-2020.1-x86_64-darwin.tar.bz2: https://github.com/dylan-lang/opendylan/releases/download/v2020.1pre/opendylan-2020.1pre-x86_64-darwin.tar.bz2
.. _opendylan-2020.1-x86_64-linux.tar.bz2: https://github.com/dylan-lang/opendylan/releases/download/v2020.1pre/opendylan-2020.1pre-x86_64-linux.tar.bz2
.. _opendylan-2020.1-x86_64-freebsd.tar.bz2: https://opendylan.org/downloads/opendylan/2020.1/opendylan-2020.1-x86_64-freebsd.tar.bz2
.. _download directories: http://opendylan.org/downloads/opendylan/
.. _"dylan-lang" organization on GitHub: https://github.com/dylan-lang/
