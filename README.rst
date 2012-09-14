Welcome to Open Dylan!
======================

Open Dylan is a compiler and a set of libraries for the Dylan
programming language.  If you're interested in working on the compiler
and core libraries then you've come to the right place.  If you want
to write your own Dylan libraries and *use* the compiler then you
should `download a binary release <http://opendylan.org/download/>`_
and then read the `Getting Started
<http://opendylan.org/documentation/getting-started/>`_ guide.

Open Dylan has two back-ends, HARP (which translates to native x86
code) and a C back-end.

The HARP back-end uses the Memory Pool System (MPS) from Ravenbrook,
Limited to do its memory management.  The MPS is available from
Ravenbrook at http://www.ravenbrook.com/project/mps/ and must be
downloaded and built separately. If you are using Windows, you
must download the `older 1.108 release
<http://www.ravenbrook.com/project/mps/release/1.108.0/>`_. On
other platforms, the `current 1.110 release
<http://www.ravenbrook.com/project/mps/release/1.110.0/>`_ is
required.

The C back-end uses Boehm-Demers-Weiser conservative C/C++ garbage
collector, available at https://github.com/ivmai/bdwgc

Open Dylan is written in Dylan, thus a Dylan compiler is needed to
bootstrap it. Binary releases are available from
http://opendylan.org/download/

Once installed, the following command-line will produce a binary in
~/Open-Dylan/bin/hello-world
::

  dylan-compiler -build hello-world


Compilation of the compiler itself
==================================

Clone the git repository::

  git clone git://github.com/dylan-lang/opendylan.git --recursive


Compilation on UNIX
===================

Please note that on 64 bit Linux we need a big stack, the default
stack is too small, please increase with ulimit -s before (safe is
to double its value)

Get MPS or boehm-gc, depending on your platform:

* Linux x86 or FreeBSD x86 (HARP) -> `MPS <http://www.ravenbrook.com/project/mps/release/1.110.0/>`_
* Mac OS X and all 64 bit (C) -> boehm-gc

::

  export PATH=/path/to/opendylan/bin:$PATH
  ./autogen.sh
  ./configure \
     --with-mps=/path/to/mps-kit \  # if using the HARP back-end
     --with-gc=/path/to/gc \ # if using the C back-end
     --prefix=/opt/opendylan-current
  make
  sudo make install

If you already have Open Dylan installed and are building with the C
back-end you can use the GC that came with the installation.  For
example, ``--with-gc=/opt/opendylan-2011.1/gc``.

The first generation will be in Bootstrap.2/bin/dylan-compiler ,
and a second generation will be in /opt/opendylan-current/bin/dylan-compiler


Compilation on Windows
=======================

* Get `MPS <http://www.ravenbrook.com/project/mps/release/1.108.0/>`_. Be
  sure that you have the older 1.108 release and NOT the newer 1.110
  release.

* Make sure to have required tools installed: namely Debugging tools for
  Windows, a C compiler (PellesC or VC6) and Microsoft Platform SDK.

* Open a shell (windows command processor), there set the environment
  variable SDK4MEMORY_POOL_SYSTEM to <where you unpacked MPS>.

* Please keep in mind that paths with whitespaces are not well supported.

* Go to admin\\builds and do a::

  build-release.bat <target-dir> /sources <git-checkout>\sources /internal

This will do a 4-stage bootstrap, in the end there will be a
complete IDE in <target-dir>.

* Building an installer:

* Get NSIS from http://nsis.sf.net and the HTML help workshop (from
  Microsoft, to generate the chm).

* Go to packages\\win32-nsis, read Build.txt and follow the
  instructions. Make sure you are using the same command shell as used
  for building Open Dylan (to retain environment variables).


Building the MPS
================

This is not required anymore since it is part of building the runtime.

