:Author: Bruce Mitchener, Jr.
:Date: 2014-01-28 00:00:00

A Call For Help
===============

Dylan is a multi-paradigm functional and object-oriented programming
language. It is dynamic while providing a programming model designed
to support efficient machine code generation, including fine-grained
control over dynamic and static behaviors.

Dylan is (and was) a vision of a different world: one with the
flexibility of Smalltalk, the power of Lisp, while still allowing
for compilation to efficient code.

During the 1990s, multiple teams of people at Apple, Carnegie Mellon
University, and Harlequin (defunct makers of LispWorks, now a product
of Xanalys) created and worked on Dylan for many years. Dylan fell by
the wayside when its sponsoring organizations ran into financial
troubles, and has been maintained by an open source project over the
last decade as "Open Dylan".

Open Dylan is the open source adaptation of the commercial Dylan
tools that were developed at Harlequin. Open Dylan has a very sophisticated
compiler, tools and IDE (although the IDE and some of the tools are
currently Windows only). These tools were truly amazing for their
time. Open Dylan is preserving, maintaining, and enhancing this
powerful programming language.

We believe that Dylan has value in terms of the history behind it,
the philosophy of the language design, and the unique perspective
it provides for solving modern-day software engineering problems.
The Open Dylan project brings all of these capabilities together
in a free, open-source project that anyone can use.

Learn how to get started with Dylan by visiting
http://opendylan.org/download/.

----------

We have a number of projects where we currently lack the knowledge,
experience, or available people to begin or complete them. This
isn't to say that we've been idle! We've been improving performance
of our core libraries, optimizing our `HTTP`_ code, creating a
stand-alone `binary-data_` library and many other things.

We're making a call to the community of interested people to say
that we're more than willing to help people get started with
Dylan and to help deal with problems that arise in the course of
making progress on some of our open projects.

We have a `partial list of open projects`_ in our wiki, but we'd like
to call special attention to a subset of them.

If you're interested in any of the below, please `get in touch`_ with
us via our mailing list, IRC or ping us on `Twitter`_.  If none of
the projects below sound interesting, feel free to check out the list
on the wiki to see the others.

Read on to see how you can help!

Cocoa Bindings
--------------

We've recently made substantial progress on our Objective C / Dylan
bridge and the development branch of our compiler will soon support
sending Objective C messages. With this, we can begin to wrap the
Mac OS X Objective C APIs, in particular those for creating GUI
applications.

Our current Objective C bridge lets us do things like:

.. code-block:: dylan

  let c = objc/get-class("NSObject");
  let s = objc/register-selector("alloc");
  let o
    = primitive-wrap-machine-word
        (%objc-msgsend (c.as-raw-class, s.as-raw-selector)
           () => (obj :: <raw-machine-word>)
           ()
         end);

We will make this simpler and provide bindings for core Cocoa
libraries so that programming experience is more pleasurable
and less verbose.

DUIM
----

DUIM is the Dylan User Interface Manager. It is a cross-platform
GUI library that inherits some of the design from Common Lisp's CLIM
and descends from the Symbolics Open Genera Lisp Machine OS.

Despite being built to be cross-platform, the only currently fully
functional backend for DUIM is the Windows version.

Improving DUIM and making it available on additional operating
systems is key to the future of the Open Dylan IDE (which is
currently only available on Windows).

DUIM/Windows
~~~~~~~~~~~~

DUIM currently runs on Windows, but it is quite dated and doesn't
fully respect modern interface standards. The port needs to be
updated to properly use the theming mechanisms available.

DUIM/Cocoa
~~~~~~~~~~

We would like to have a Cocoa backend to DUIM. This work has
not yet been started and would need to progress in parallel with
the creation of Cocoa bindings for Dylan.

DUIM/Gtk
~~~~~~~~

We have a good start on a GTK+ backend to DUIM in the Open Dylan
repository, but it needs a good bit of work, especially from
someone that knows GTK+ well.

Extensive work was done in this area during the summer of 2013,
when we created a full set of bindings for GTK+-3.0 and related
APIs.

Code Mirror Editor Support
--------------------------

Code Mirror is a great editor that runs in the web browser. It is written
in JavaScript and supports over 60 languages. It is also a key element
of the `Light Table IDE`_.

We'd like to have `Code Mirror`_ support for Dylan. This would initially
involve correcting our existing syntax highlighting and then moving
into further improvements like supporting auto-indentation and other
Code Mirror features.

This would involve writing JavaScript code.

Networking Library
------------------

We need to add IPv6 support to our networking library.

Packaging
---------

We need assistance with packaging on pretty much every operating
system / distribution apart from FreeBSD, Gentoo Linux and ArchLinux.
We would particularly appreciate support in producing Debian, Ubuntu,
and Mac OS X Homebrew packages.

Our Windows installer also needs some support, particularly in the
bundling of the PellesC compiler to improve the first time experience
for Windows users.

.. _HTTP: https://github.com/dylan-lang/http
.. _binary data: https://github.com/dylan-lang/binary-data
.. _partial list of open projects: https://github.com/dylan-lang/opendylan/wiki
.. _get in touch: http://opendylan.org/community/
.. _Twitter: http://twitter.com/DylanLanguage
.. _Code Mirror: http://codemirror.net/
.. _Light Table IDE: http://www.lighttable.com/
