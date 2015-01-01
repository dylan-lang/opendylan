:Author: Bruce Mitchener, Jr.
:Date: 2013-08-15 00:00:00

Bringing back DUIM/Gtk
======================

.. figure:: duim-gtk.png
   :align: right

Lately, we've been working on bringing back DUIM's GTK+ back-end.

`DUIM`_ is Dylan's user interface framework. While it was
originally cross-platform, the only back-end that works now
is for Windows.  Bringing back the GTK+ back-end and completing
it will enable us to support Linux, FreeBSD and perhaps Mac OS X.

This is a great first step towards making the `IDE`_ more widely
available. Apart from being able to use the IDE on additional
platforms, which is great in and of itself, having the IDE available to
more of us makes it easier to fix bugs and improve the functionality
as most of us aren't working on the Windows platform.

So far, we have a new set of GTK+ bindings for GTK+ 3.x available
in `sources/gtk`_ and have begun the work to update the old
`DUIM GTK+`_ back-end and make further improvements.

At this point, we would welcome some assistance. There are
further improvements to make to the GTK+ bindings and a lot of
work to do on DUIM/Gtk. (We have started building a
`list of bugs`_.) We've also started to collect some notes
for the `hacker's guide`_, especially for working on Mac OS X.

If you're interested in learning more or helping out, get
in touch with us on `IRC or the mailing list`_!

.. _DUIM: http://opendylan.org/documentation/building-with-duim/
.. _IDE: http://opendylan.org/documentation/getting-started-ide/
.. _sources/gtk: https://github.com/dylan-lang/opendylan/tree/master/sources/gtk
.. _DUIM GTK+: https://github.com/dylan-lang/opendylan/tree/master/sources/duim/gtk
.. _list of bugs: https://github.com/dylan-lang/opendylan/labels/lib-DUIM%20%2F%20Gtk
.. _hacker's guide: http://opendylan.org/documentation/hacker-guide/duim.html
.. _IRC or the mailing list: http://opendylan.org/community/
