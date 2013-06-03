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
   | `Windows (32 bit)`_       | 2012.1             | December 20, 2012  | `0ff69dce5d`_ |
   +---------------------------+--------------------+--------------------+---------------+
   | `Mac OS X Lion+ (Intel)`_ | 2012.1             | December 20, 2012  | `0ff69dce5d`_ |
   +---------------------------+--------------------+--------------------+---------------+
   | `Linux (x86)`_            | 2012.1             | December 20, 2012  | `0ff69dce5d`_ |
   +---------------------------+--------------------+--------------------+---------------+
   | `Linux (x86-64)`_         | 2012.1             | December 20, 2012  | `0ff69dce5d`_ |
   +---------------------------+--------------------+--------------------+---------------+
   | `FreeBSD (x86)`_          | 2012.1             | December 20, 2012  | `0ff69dce5d`_ |
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

.. _Windows (32 bit): http://opendylan.org/downloads/opendylan/2012.1/opendylan-2012.1-win32.exe
.. _Mac OS X Lion+ (Intel): http://opendylan.org/downloads/opendylan/2012.1/opendylan-2012.1-x86-darwin.tar.bz2
.. _Linux (x86): http://opendylan.org/downloads/opendylan/2012.1/opendylan-2012.1-x86-linux.tar.bz2
.. _Linux (x86-64): http://opendylan.org/downloads/opendylan/2012.1/opendylan-2012.1-x86_64-linux.tar.bz2
.. _FreeBSD (x86): http://opendylan.org/downloads/opendylan/2012.1/opendylan-2012.1-x86-freebsd.tar.bz2
.. _download directories: http://opendylan.org/downloads/opendylan/
.. _0ff69dce5d: https://github.com/dylan-lang/opendylan/tree/v2012.1
