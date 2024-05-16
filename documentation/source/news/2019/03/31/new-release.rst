:orphan:
:Author: Carl Gay
:Date: 2019-03-31 00:00:00

Open Dylan 2019.1 Released
==========================

Posted on 31 March 2019

Dear Dylan Hacker,

It is a pleasure for us to announce a new release of Open Dylan.

Dylan is a multi-paradigm functional and object-oriented programming
language. It is dynamic while providing a programming model designed to support
efficient machine code generation, including fine-grained control over dynamic
and static behaviors.

Although there are many changes in this release, the main highlight is that the
**LLVM back-end**, which uses LLVM 7.x or later for code generation, is now
full-featured and mature on i386 and x86_64 Linux, FreeBSD, and macOS
platforms.  Other highlights include:

* A backtrace is printed when the program exits due to an unhandled error.
* Linking is faster, with a new ``-jobs`` option to use multiple processes.
* Compiler warning improvements, including colorized output.
* Optimizations to limited integer types.
* Support for building C++ code as part of a Dylan project.
* A new ``lldb`` wrapper script that preloads the Dylan integration scripts.
* Run-time primitives to aid in profiling and event logging.

For full details on changes in this release see the :doc:`release notes
</release-notes/2019.1>`, or go
directly to the :doc:`download page </download/index>`.

For more information on Open Dylan, see https://opendylan.org/.

Good luck and happy Dylan hacking!
