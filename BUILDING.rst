*******************
Building Open Dylan
*******************

This document describes how to build the Open Dylan compiler and IDE.

Clone the git repository::

  git clone --recursive git://github.com/dylan-lang/opendylan.git

.. note:: It does not work to download a ZIP file of the repository from github
   because it doesn't include git submodules.


UNIX
====

Dependencies
------------

Binary releases come packaged with all of the necessary
dependencies. The current binary release includes

- `LLVM/Clang
  <https://github.com/llvm/llvm-project/releases/tag/llvmorg-10.0.1
  version 10.0.1>`_ (subsetted to include only necessary components)
- `LLVM libunwind
  <https://clang.llvm.org/docs/Toolchain.html#unwind-library>`_
  (snapshot revision `22b615a9
  <https://github.com/llvm/llvm-project/tree/22b615a96593f13109a27cabfd1764ec4f558c7a>`_)
- `BDW GC 8.0.4 <https://github.com/ivmai/bdwgc/releases/tag/v8.0.4>`_

If you build from source, you may need to supply these dependencies as
described below.

All 64-bit platforms and macOS must use the Boehm Demers Weiser conservative GC
(or just "Boehm GC") with the LLVM or C back-end. The Memory Pool System (MPS)
is only integrated with the HARP back-end, which itself only works on 32-bit
x86 platforms.

* 64-bit systems and macOS (LLVM or C back-end) -> `boehm-gc
  <https://github.com/ivmai/bdwgc>`_, usually installed via a package (see
  below).
* 32-bit x86 Linux or FreeBSD (HARP back-end) -> `MPS 1.114
  <http://www.ravenbrook.com/project/mps/release/1.114.0/>`_

On macOS, you may find it easiest to install Homebrew and install
the following::

    brew install autoconf automake bdw-gc --universal

You will also need to install the command line build tools available from
Apple. If your installation of ``bdw-gc`` is not universal (doesn't contain
both i386 and x86_64 code), you will need to uninstall it and install again
with the ``--universal`` flag.

On Ubuntu, Debian, etc, you can install the necessary dependencies
with::

    apt-get install autoconf automake clang-10 gcc libgc-dev libunwind-dev

The ``libunwind`` library is an optional dependency on Linux and
FreeBSD. If available, it is used to display stack traces for
unhandled error conditions. (The ``libunwind`` API is built-in on
macOS.)

You may also want to install ``lldb-10`` for debugging if you are using the LLVM
back-end.

Building
--------

To build :program:`dylan-compiler` and several tools::

  export PATH=$(dirname $(which dylan-compiler)):$PATH
  ./autogen.sh
  ./configure --prefix=/opt/opendylan-current   # (but see note below)
  make
  sudo make install

The build process attempts to select the correct garbage collector
implementation based on your platform.

If you are on ``x86-linux`` or ``x86-freebsd`` you must add a flag to
``configure`` to point it at the MPS sources, using ``--with-mps``::

  ./configure --prefix=/opt/opendylan-current --with-mps=/path/to/mps

``/path/to/mps`` should point to the root directory of the MPS
distribution, for example ``--with-mps=/opt/mps-kit-1.114.0``.

On other platforms, the Boehm GC will be used. If you have installed
the Boehm GC via your operating system package manager, you may not
need to specify its location; it will be found automatically if it is
in ``/usr`` or ``/usr/local``. If you have installed the Boehm GC into
a non-standard location or the configure script cannot find it, you
can point it in the right direction by using ``--with-gc``::

  ./configure --prefix=/opt/opendylan-current --with-gc=/path/to/boehm

By default, this will build a fully bootstrapped compiler with the first
generation in :file:`Bootstrap.1/bin/dylan-compiler`, the second generation in
:file:`Bootstrap.2/bin/dylan-compiler`, and the third in
:file:`Bootstrap.3/bin/dylan-compiler`. The third generation will then be
installed as :file:`/opt/opendylan-current/bin/dylan-compiler`.

Running Tests
-------------

There is an extensive set of tests which can be run once the build is
complete::

  make check

This runs the tests for the core language implementation as well as for many
bundled libraries. You may also want to run the
``dfmc-environment-test-suite``, with ::

  make check-environment

Windows
=======

* Get `MPS 1.108
  <http://www.ravenbrook.com/project/mps/release/1.108.0/>`_. Be sure
  that you have the older 1.108 release and **not** the newer 1.114
  release.

* Make sure to have required tools installed:

  - Debugging tools for Windows
  - Microsoft Visual C++ 6.0 -- Note that newer versions do not work, nor does
    Pelles C.
  - Microsoft Platform SDK

* Open a shell (windows command processor) and set the environment
  variable SDK4MEMORY_POOL_SYSTEM to <where you unpacked MPS>.

* Please keep in mind that paths with whitespaces are not well supported.

* cd into :file:`build\\windows` and run::

    build-release.bat <target-dir> /sources <git-checkout>\sources /internal

This will do a 4-stage bootstrap.  In the end there will be a complete
IDE in <target-dir>.

* Building an installer:

  * Get NSIS from http://nsis.sf.net and the HTML help workshop (from
    Microsoft, to generate the chm).

  * Go to :file:`packages\\win32-nsis`, read :file:`Build.txt` and follow the
    instructions. Make sure you are using the same command shell as used for
    building Open Dylan (to retain environment variables).

