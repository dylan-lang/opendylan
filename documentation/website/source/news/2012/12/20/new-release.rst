:Author: Bruce Mitchener, Jr.
:Date: 2012-12-20 00:00:00

Open Dylan 2012.1 released
==========================

Dear Dylan Hacker,

It is a pleasure for us to finally announce a release of Open Dylan.

The last release with an announcement was 2011.1, in December, 2011.
For a variety of reasons, we didn't get to carry out our plan for
more frequent releases in 2012, but hope to have more frequent releases
in 2013.

While we have extensive `release notes <http://opendylan.org/documentation/release-notes/2012.1.html>`_,
there are some important changes that we feel are worth calling out
separately:

* We now use the C3 superclass linearization. See `DEP-0003 <http://opendylan.org/proposals/dep-0003.html>`_
  for more details.
* Multithreading now works in the C backend.
* Binaries and libraries built by ``dylan-compiler`` no longer have
  their directory location hard-coded within the executables.
* ``dylan-compiler`` places output into a local ``_build`` directory
  and can read from a local registry rather than relying on global
  directories.
* Many bugs have been fixed, especially involving the usage of floats.
* Linux and FreeBSD 32 bit builds have been updated to a new version
  of the Memory Pool System GC which is much faster and stable.
* A new ``strings`` library has been provided which replaces the old
  ``strings`` and ``string-extensions`` libraries with a more consistent
  and straightforward API. ``string-extensions`` has been removed.
* The ``command-line-parser`` library has been cleaned up and integrated
  with the Open Dylan distribution.
* We no longer provide PowerPC builds of Open Dylan on Mac OS X. Also,
  our Intel builds are for Mac OS X Lion and later. If you desire Snow
  Leopard support, please get in touch with us.

Please report problems that you have in our `issue tracker <https://github.com/dylan-lang/opendylan/issues>`_.

You can get it from our website, `http://opendylan.org/download/ <http://opendylan.org/download/>`_.
On Windows there is an installer, on UNIX systems unpack into ``/opt``.
On 64 bit Linux, you will need to have the Boehm GC installed for our
executables to run. (Ubuntu: ``apt-get install libgc``)

Good luck and happy Dylan hacking!
