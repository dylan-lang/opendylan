********************
Open Dylan Downloads
********************

.. raw:: html

   <div class="row">
     <div class="span8">
       <h2>Binary Releases</h2>

.. warning:: The 32 bit version of Open Dylan IDE does NOT work on 64 bit Windows
   prior to Windows 8. There is no workaround at this time, unfortunately. We
   hope to resolve this in an upcoming release.
   :class: alert alert-warning

   However, the command line tools should work as the problem is with interaction
   between threads, GC and WoW64.


.. table::
   :class: table-striped

   +---------------------------+--------------------+--------------------+---------------+
   | Platform                  | Version            | Date Released      | Revision ID   |
   +===========================+====================+====================+===============+
   | `Windows (32 bit)`_       | 2013.1             | July 11, 2013      | `211c9f8752`_ |
   +---------------------------+--------------------+--------------------+---------------+
   | `Mac OS X Lion+ (Intel)`_ | 2013.1             | July 11, 2013      | `211c9f8752`_ |
   +---------------------------+--------------------+--------------------+---------------+
   | `Linux (x86)`_            | 2013.1             | July 11, 2013      | `211c9f8752`_ |
   +---------------------------+--------------------+--------------------+---------------+
   | `Linux (x86-64)`_         | 2013.1             | July 11, 2013      | `211c9f8752`_ |
   +---------------------------+--------------------+--------------------+---------------+
   | `FreeBSD (x86)`_          | 2013.1             | July 11, 2013      | `211c9f8752`_ |
   +---------------------------+--------------------+--------------------+---------------+

Old builds (including PowerPC Mac OS X and 64 bit FreeBSD) can be found by
browsing the `download directories`_.

.. raw:: html

     </div>
     <div class="span4">
       <div class="hero-unit">
         <h2>Source Code</h2>
         <p>All of our source code is available under an open source license in the <a href="https://github.com/dylan-lang/">"dylan-lang" organization on GitHub</a>.</p>
       </div>
     </div>
   </div>

-----------

Installation on Windows
-----------------------

For installation, double-click on the installer, and follow instructions.
You need to have either the PellesC linker or the linker of VC++ 6.0, 7.0
or the current .NET platform SDK installed. PellesC is the best option.

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

The README file inside the tarball describes installation and basic
usage. The easiest way is extracting the tarball in /opt. The
Linux platform should have gcc installed, in order to allow
linking. Note that these versions only have a command-line compiler
and no IDE.

64 bit Linux requires that the Boehm GC is installed
(For example, ``apt-get install libgc-dev`` on Ubuntu).

Installation from source
------------------------

You can read more about installing Open Dylan from the source files
in the `README <https://github.com/dylan-lang/opendylan/blob/master/README.rst>`_.  
This file contains the instructions on how to obtain the source code, the required
dependencies and how to build it.

.. _Windows (32 bit): http://opendylan.org/downloads/opendylan/2013.1/opendylan-2013.1-win32.exe
.. _Mac OS X Lion+ (Intel): http://opendylan.org/downloads/opendylan/2013.1/opendylan-2013.1-x86-darwin.tar.bz2
.. _Linux (x86): http://opendylan.org/downloads/opendylan/2013.1/opendylan-2013.1-x86-linux.tar.bz2
.. _Linux (x86-64): http://opendylan.org/downloads/opendylan/2013.1/opendylan-2013.1-x86_64-linux.tar.bz2
.. _FreeBSD (x86): http://opendylan.org/downloads/opendylan/2013.1/opendylan-2013.1-x86-freebsd.tar.bz2
.. _download directories: http://opendylan.org/downloads/opendylan/
.. _211c9f8752: https://github.com/dylan-lang/opendylan/tree/v2013.1
