:Author: Bruce Mitchener, Jr.
:Date: 2013-12-23 00:00:00

Open Dylan 2013.2 released
==========================

Dear Dylan Hacker,

It is a pleasure for us to announce a new release of Open Dylan.

We have extensive `release notes <http://opendylan.org/documentation/release-notes/2013.2.html>`_,
but this release has been about iterative improvement rather than major changes.

* Testworks has seen substantial improvements.
* Streams now default to being unlocked.
* We've begun to work on improving cross-compilation.
* Logging and hash-algorithms libraries have been added to the core release.
* Common Dylan has improved timers (simple-timers) replacing the old compiler primitives.
* Documentation has been improved.

Please report problems that you have in our `issue tracker <https://github.com/dylan-lang/opendylan/issues>`_.

You can get it from our website, `http://opendylan.org/download/ <http://opendylan.org/download/>`_.
On Windows there is an installer, on UNIX systems unpack into ``/opt``.
On 64 bit Linux, you will need to have the Boehm GC installed for our
executables to run. (Ubuntu: ``apt-get install libgc``)

Good luck and happy Dylan hacking!
