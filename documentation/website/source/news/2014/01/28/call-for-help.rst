:Author: Bruce Mitchener, Jr.
:Date: 2014-01-28 00:00:00

A Call For Help
===============

We have a number of projects where we currently lack the knowledge,
experience, or available people to begin or complete them.

We're making a call to the community of interested people to say
that we're more than willing to help people get started with
Dylan and to help deal with problems that arise in the course of
making progress on some of our open projects.

We have a `partial list of open projects`_ in our wiki, but we'd like
to call special attention to a subset of them. If none of the projects
below sound interesting, feel free to check out the list on the wiki
to see the others.

Read on to see how you can help!

Cocoa Bindings
--------------

We've recently made substantial progress on our Objective C / Dylan
bridge and the development branch of our compiler will soon support
sending Objective C messages. With this, we can begin to wrap the
Mac OS X Objective C APIs, in particular those for creating GUI
applications.

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

Code Mirror Editor Support
--------------------------

We'd like to have `Code Mirror`_ support for Dylan. This would initially
involve correcting our existing syntax highlighting and then moving
into further improvements like supporting auto-indentation and other
Code Mirror features.

This would improve the experience of editing Dylan in the `Light Table
IDE`_.

This would involve writing JavaScript code.

Library Improvements
--------------------

Networking
~~~~~~~~~~

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

.. _partial list of open projects: https://github.com/dylan-lang/opendylan/wiki
.. _Code Mirror: http://codemirror.net/
.. _Light Table IDE: http://www.lighttable.com/
