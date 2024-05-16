:orphan:
:Author: Carl Gay
:Date: 2020-10-10 00:00:00

Open Dylan 2020.1 Released
==========================

Posted on 10 October 2020

Dear Dylan Hacker,

It is a pleasure for us to announce a new release of Open Dylan.

Some highlights in this release:

* This is our first release with "batteries included". The binary distributions
  contain all necessary dependent libraries, including clang, libunwind, and
  libgc, making installation much easier.

* The LLVM back-end is now the default back-end. Its performance is 30% to 40%
  better than the C or HARP back-ends.

* The LLVM back-end now tracks function inlining in generated debug
  information, allowing breakpoints to be set in inlined code.

* Platforms: The AArch64 (64-bit ARM) platform is now supported on Linux.

* The compiler now displays a text mode progress bar during the compilation and
  link phases. (The new progress-stream library also is available for users.)

* Network: A deadlock that occurred when shutting down listening socket threads
  has been fixed.

* IO: A number of bugs in the locators library were fixed, ``resolve-locator``
  was added, and various definitions in the print module were documented.

* Testworks now generates reports in JSON format, along with other Testworks
  improvements.

For full details on changes in this release see the :doc:`release notes
</release-notes/2020.1>`, or go
directly to the :doc:`download page </download/index>`.

For more information on Open Dylan, see our https://opendylan.org/.

Good luck and happy Dylan hacking!
