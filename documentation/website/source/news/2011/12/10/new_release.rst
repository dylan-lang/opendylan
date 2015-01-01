:Author: Hannes Mehnert
:Date: 2011-12-10 02:00:00

Open Dylan 2011.1 released
==========================

Dear Dylan Hacker,

It is a pleasure for us to finally announce a release of Open Dylan.

The last release with an announcement was beta4, in April 2007. Since
then we have decided to use another versioning scheme: $year.$count;
this release version is 2011.1.  We will be doing more regular
releases and are already hard at work on 2012.1 for next month!

One of the most important changes from the last four years is that the
remaining rights holders of Functional Objects (the original
developers of what became Open Dylan) agreed to re-license all of
their code for the compiler and runtime libraries under the MIT
license.

We have also moved to GitHub:

    https://github.com/dylan-lang/

Another big effort was made to decrease the size of the code base in
order to improve its maintainability. In this release, compared to
(the unannounced) beta5, we removed about 1/3 of the code base, from
1,200,000 lines of code down to 850,000 (according to
https://www.openhub.net/p/open-dylan).

This is the first joint release on all major platforms:

- Windows (32-bit only, this is the only platform with the IDE right now, native back-end)
- Mac OS X (32-bit, Intel (10.6+) and PPC (10.5) using the C back-end)
- Linux (32-bit (with the native back-end) and 64-bit (with the C back-end))
- FreeBSD (32-bit only, native back-end)

The C back-end currently does not support multi-threaded programs.

We also developed a SLIME (http://common-lisp.net/project/slime/)
back-end, so you can develop in Emacs and get cross references, M-.
and arguments.

The library functional-dylan has been phased out. It was mainly
importing and exporting dylan and common-dylan. Use dylan or
common-dylan as appropriate.

The release also includes several bug fixes and improves error
reporting.  However, many bugs and issues with error reporting
remain. Please report problems that you have in our issue tracker:

    https://github.com/dylan-lang/opendylan/issues

You can get it from our website, http://opendylan.org/download/ .
On Windows there is an installer, on UNIX systems unpack into /opt.


Now follows a more detailed list of changes:

Notable compiler changes
========================

The command-line compiler is now called dylan-compiler instead of
minimal-console-compiler (or opendylan).

The 'remove build products' command has been renamed to 'clean', which
is more common and intuitive.

We fixed the error reporting in several ways: cleanup handlers are
called correctly now (no "Attempting to build during another build!"
anymore). Also, when an error is caused, the reporting does not error.
Additionally warnings and errors from subprojects are now reported
(rather than only put into the log file).

We support output of internal debug messages via a dylan-compiler
argument (internal-debug, set to a list of debug-targets, like
project-manager, linker,...). Additionally Harp, DFM and assembler
output of the compiler can be written to the build directory (via
harp, dfm, assembler command-line flags).

The dylan-compiler supports -version and -shortversion flags.

In Deuce, a buffer which is modified is now marked with an asterisk.

On application shutdown, a list of closures is called, which can be
registered via register-application-exit-function. Also the GC is
properly shut down.

The build system both on UNIX and Windows has lots of improvements and
is even less code. There is no longer support to checkout code via
CVS, though.

The runtime lives in dylan-support.o, which is linked into libdylan,
rather than a _glue.o which was emitted for every application. The
system library contains offsets into C structures (like stat) for all
major platforms, including 64-bit versions.

The function closure-size no longer lives in dfmc-execution, but in
dfmc-flow-graph. The library dfmc-execution was removed.


Library Improvements
====================

We consolidated file and standard-io under the same buffered
file-accessor. We also improved the run-application function, which
now can spawn asynchronous processes, redirect standard-input,
standard-output and standard-error, set the working directory and
environment variables.

If write is interrupted (by EINTR), we call write again until it has
written all bytes.

The function join now works on an empty sequence.

The function position takes start and end keyword parameters, and its
performance was improved.

A new function, integer-length, is now in common-dylan.

The function directory-contents now returns the contents of the
directory.

We now support a with-input-from-string macro in the streams module.

A new library, ssl-network, wraps openssl and integrates into the
already existing network library.

The element-setter for the forward-iteration-protocol of
<ordered-key-collection> has been fixed.

Shift operations on machine words are inlined now.

The <smtp-error> class contains a slot with the error-code.

Testworks has been vastly improved, and improved test suites are
provided for the Dylan library, common-dylan and jam.


Mac OS X
========

The support on Mac OS X improved immensely; the code which retrieves
the path to the running application now uses _NSGetExecutablePath.
Also, setting environment variables is now supported on Mac OS X.  The
system library is renamed on Mac OS X to odsystem to prevent
interfering with the built-in System library.


Back-ends
=========

In the native back-end we pass integer and floating point exceptions
to the Dylan runtime on Linux and FreeBSD platforms.

In the C back-end we emit volatile for all the variables inside of a
bind-exit block which need to be volatile.

We defer the back-end selection to the end of the compilation of the
compiler to reduce the compilation time for compiler hackers.


Removed code
============

The majority of the removed code was not used, on the one hand code
for the LispWorks emulator, on the other hand (half-finished) support
for outdated platforms: namely a native PPC backend, a half-finished
Java backend; Mac OS (Classic), IRIX, SunOS, Solaris and OSF3 platform
support.

It also included the separation in three disjoint releases, basic,
enhanced and enterprise - which was useful back when the compiler was
developed commercially, but is no longer.

The amount of applications for the command-line compiler has been
decreased from eight to four, namely dylan-compiler, dylan-environment
(including execution and debugging) and dylan-\*-with-tools, which
includes the tools interface (remote debugging, scepter, OLE).

Also, the old pentium-dw (DylanWorks) application was removed since it
is no longer used and is unnecesary..

We unified the dfmc-shell/command-shell and
environment-commands/commands libraries, which are the interactive
shell. (Previously the otherwise obsolete dfmc-shell was used in the
interactor).
