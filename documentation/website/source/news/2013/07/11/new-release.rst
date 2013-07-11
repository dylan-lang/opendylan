:Author: Bruce Mitchener, Jr.
:Date: 2013-07-11 00:00:00

Open Dylan 2013.1 released
==========================

Dear Dylan Hacker,

It is a pleasure for us to announce a new release of Open Dylan.

We have extensive `release notes <http://opendylan.org/documentation/release-notes/2013.1.html>`_,
but this release has been about iterative improvement rather than major changes.

* A deadlock when using threads on x86-linux has been addressed.
* A crash when using threads on x86-linux has also been addressed.
* A crash from memory corruption on Mac OS X has been fixed.
* Some errors in how the Windows Registry was used have been corrected.
* There have been significant improvements to the documentation.
* A licensing issue with an unacknowledged copyright has been corrected.

Please report problems that you have in our `issue tracker <https://github.com/dylan-lang/opendylan/issues>`_.

You can get it from our website, `http://opendylan.org/download/ <http://opendylan.org/download/>`_.
On Windows there is an installer, on UNIX systems unpack into ``/opt``.
On 64 bit Linux, you will need to have the Boehm GC installed for our
executables to run. (Ubuntu: ``apt-get install libgc``)

Also, remember that this weekend, July 13-14 is our `hack-a-thon`_ and we hope that you'll consider participating.
You may also sign up for the hack-a-thon at our `Google Plus event page`_.

Good luck and happy Dylan hacking!

.. _hack-a-thon: http://opendylan.org/news/2013/06/30/dylan-hack-a-thon.html
.. _Google Plus event page: https://plus.google.com/events/cuj0dbm59r9heuf4pqm5qj9u0uc
