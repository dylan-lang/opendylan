:Author: Bruce Mitchener, Jr.
:Date: 2015-01-01 00:00:00

Open Dylan 2014.1 released
==========================

Dear Dylan Hacker,

It is a pleasure for us to announce a new release of Open Dylan.

Dylan is a multi-paradigm functional and object-oriented programming language. It is dynamic while providing a programming model designed to support efficient machine code generation, including fine-grained control over dynamic and static behaviors.

For more information on Open Dylan, see our `website <http://opendylan.org/>`_ and our `documentation <http://opendylan.org/documentation/>`_.

We have extensive `release notes <http://opendylan.org/documentation/release-notes/2014.1.html>`_, but highlights include:

* bash completion scripts are available.
* The foundation of an Objective C / Dylan bridge has been integrated with the compiler.
  This enables using the `objc-dylan <https://github.com/dylan-foundry/objc-dylan>`_ library.
* Passing structs by value using C-FFI now works on both the C and HARP back-ends.
* Performance improvements have been made to various libraries.
* Laying the groundwork for LLVM compiler back-end support to arrive in 2015.
* Laying the groundwork for improving our Unicode support.
* Many other miscellaneous fixes and improvements.

Please report problems that you have in our `issue tracker <https://github.com/dylan-lang/opendylan/issues>`_.

You can get it from our website, `http://opendylan.org/download/ <http://opendylan.org/download/>`_.
On Windows there is an installer, on UNIX systems unpack into ``/opt``.
On 64 bit Linux, you will need to have the Boehm GC installed for our
executables to run. (Ubuntu: ``apt-get install libgc``)

Our plan is to have a first release of Open Dylan with an LLVM-backed compiler back-end within the next couple of months as our 2015.1 release. Here's hoping we can make that happen!

Good luck and happy Dylan hacking!
