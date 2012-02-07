********************
Open Dylan Downloads
********************

.. raw:: html

   <div class="row">
     <div class="span8">
       <h2>Binary Releases</h2>

.. warning:: The 32 bit version of Open Dylan does NOT work on 64 bit Windows.
   There is no workaround at this time, unfortunately. We hope to resolve this
   in an upcoming release.
   :class: alert alert-warning

.. table::
   :class: table-striped

   +-----------------------+--------------------+--------------------+---------------+
   | Platform              | Version            | Date Released      | Revision ID   |
   +=======================+====================+====================+===============+
   | `Windows (32 bit)`_   | 2011.1             | December 10, 2011  | `23f8ab5878`_ |
   +-----------------------+--------------------+--------------------+---------------+
   | `Mac OS X (Intel)`_   | 2011.1             | December 10, 2011  | `23f8ab5878`_ |
   +-----------------------+--------------------+--------------------+---------------+
   | `Mac OS X (PowerPC)`_ | 2011.1             | December 10, 2011  | `23f8ab5878`_ |
   +-----------------------+--------------------+--------------------+---------------+
   | `Linux (x86)`_        | 2011.1             | December 10, 2011  | `23f8ab5878`_ |
   +-----------------------+--------------------+--------------------+---------------+
   | `Linux (x86-64)`_     | 2011.1             | December 11, 2011  | `23f8ab5878`_ |
   +-----------------------+--------------------+--------------------+---------------+
   | `FreeBSD (x86)`_      | 2011.1             | December 10, 2011  | `23f8ab5878`_ |
   +-----------------------+--------------------+--------------------+---------------+
   | `FreeBSD (x86-64)`_   | 1.0beta5           | November 27, 2008  | ...           |
   +-----------------------+--------------------+--------------------+---------------+


Old builds can be found by browsing the `download directories`_.

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
or the current .NET platform SDK installed.

Installation on Mac OS X, FreeBSD, Linux
----------------------------------------

The README file inside the tarball describes installation and basic
usage. The easiest way is extracting the tarball in /opt. The
Linux platform should have binutils installed, in order to allow
linking. Note that these versions only have a command-line compiler
and no IDE.

.. _Windows (32 bit): http://opendylan.org/downloads/opendylan/2011.1/opendylan-2011.1-win32.exe
.. _Mac OS X (Intel): http://opendylan.org/downloads/opendylan/2011.1/opendylan-2011.1-x86-darwin.tar.bz2
.. _Mac OS X (PowerPC): http://opendylan.org/downloads/opendylan/2011.1/opendylan-2011.1-ppc-darwin.tar.bz2
.. _Linux (x86): http://opendylan.org/downloads/opendylan/2011.1/opendylan-2011.1-x86-linux.tar.bz2
.. _Linux (x86-64): http://opendylan.org/downloads/opendylan/2011.1/opendylan-2011.1-x86_64-linux.tar.bz2
.. _FreeBSD (x86): http://opendylan.org/downloads/opendylan/2011.1/opendylan-2011.1-x86-FreeBSD.tar.bz2
.. _FreeBSD (x86-64): http://opendylan.org/downloads/opendylan/old/1.0beta5/opendylan-1.0beta5-r11990-amd64-FreeBSD7.tar.bz2
.. _download directories: http://opendylan.org/downloads/opendylan/
.. _23f8ab5878: https://github.com/dylan-lang/opendylan/tree/v2011.1
