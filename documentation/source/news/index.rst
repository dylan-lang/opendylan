****
News
****

Significant events in the Open Dylan space...


Release 2023.1
==============

**2023-07-19** Release 2023.1 is now available! A few highlights...

* Updated versions of LLVM and the BDW garbage collector.
* New ``"""multi-line"""`` and ``#r"raw"`` string literals (`DEP 12
  <../proposals/dep-0012-string-literals.rst>`_).
* The ``dylan`` tool (renamed to `deft <https://package.opendylan.org/deft/>`_
  in Open Dylan 2025.1) continues to evolve with a new, simplified workspace
  model.
* Bug fixes for json, command-line-parser, system, and others.

See the `release notes <../release-notes/2023.1.rst>`_ for more details.


New website
===========

**2023-07-07** - The opendylan.org website has had a long overdue major
overhaul. The goals of this revamp were:

1. Better navigation. For example, when reading documentation in the Library
   Reference it should be easy to navigate to the Hacker Guide or package
   docs. Now everything is accessible from the left sidebar.

#. Better accessibility. The old site had a very low contrast top navbar and
   other low contrast elements. The new `Furo
   <https://pradyunsg.me/furo/quickstart/>`_ theme has much better contrast
   plus light and dark themes.

#. Local table of contents in the right sidebar. Hooray `Furo
   <https://pradyunsg.me/furo/quickstart/>`_!

#. Integrated `package docs <https://package.opendylan.org/>`_.

#. Simplicity! We're a small team so we want to avoid any extra maintenance
   burden. To that end, we no longer use a Dylan-specific Sphinx theme or
   templates. If it ain't in `Furo <https://pradyunsg.me/furo/quickstart/>`_,
   we don't want it! :-)

We'd love to hear feedback on the new site. You can `file a bug
<https://github.com/dylan-lang/opendylan/issues>`_ or `drop us a note on Matrix
<https://app.element.io/#/room/#dylan-language:matrix.org>`_.


Release 2022.1
==============

**2022-11-28** - :doc:`New release: 2022.1 </news/2022/11/28/new-release>`


Dylan Playground
================

**2020-12-30** - :doc:`Dylan Playground Launched </news/2020/12/30/playground>`


Release 2020.1
==============

**2020-10-10** - :doc:`New release: 2020.1 </news/2020/10/10/new-release>`


Release 2019.1
==============

**2019-03-31** - :doc:`New release: 2019.1 </news/2019/03/31/new-release>`


Release 2014.1
==============

**2015-01-01** - :doc:`New release: 2014.1 </news/2015/01/01/new-release>`


Nix Packages
============

**2014-05-28** - :doc:`Nix Packages Available
</news/2014/05/28/nix-packages-available>`

Open Dylan builds have been added to the `Nix package manager`_.  This makes
Open Dylan installable from systems using Nix, or the `NixOS Linux
distribution`_.

.. _Nix package manager: https://nixos.org/
.. _NixOS Linux distribution: https://nixos.org/

Release 2013.2
==============

**2013-12-23** - :doc:`New release: 2013.2 </news/2013/12/23/new-release>`


DUIM/Gtx
========

**2013-08-15** - :doc:`Bringing Back DUIM/Gtk </news/2013/08/15/duim-gtk>`

We're bringing back the GTK+ backend for DUIM, the Dylan UI framework.  See how
you can help!


Release 2013.1
==============

**2013-07-11** - :doc:`New release: 2013.1 </news/2013/07/11/new-release>`

We just released Open Dylan 2013.1 in preparation for the hack-a-thon.


Hack-a-thon
===========

**2013-06-30** - :doc:`Dylan Hack-a-thon: July 13-14, 2013
</news/2013/06/30/dylan-hack-a-thon>`

No matter if you know Dylan or not, we can help you `learn Dylan
<https://package.opendylan.org/dylan-programming-book/>`_ or put your other
skills to use. We've got a good `list of tasks
<https://github.com/dylan-lang/opendylan/wiki>`_ that we're collecting and some
people will bring their own Dylan-related projects.


Updated Dylan Programming Guide
===============================

**2013-01-21** - :doc:`Updated Dylan Programming Guide
</news/2013/01/21/dylan-programming-guide>`

The Dylan Programming Guide has been updated to our new documentation format
and is now available in PDF and ePub formats.


Release 2012.1
==============

**2012-12-20** - :doc:`New release: 2012.1 </news/2012/12/20/new-release>`

We just released Open Dylan 2012.1. This release brings many bugfixes, improved
platform support and new features.


Improved editor support
=======================

**2012-10-18** - :doc:`Improved editor support </news/2012/10/18/editor-support>`

Bugs in the syntax highlighting support for Textmate and Sublime Text as well
as for vim have been fixed.


command-line-parser
===================

**2012-10-15** - :doc:`command-line-parser library </news/2012/10/15/command-line-parser>`

The `command-line-parser <https://github.com/dylan-lang/command-line-parser>`_
library has been completely rewritten.


New strings Library
===================

**2012-05-18** - :doc:`New strings library </news/2012/05/18/strings>`

A new library of basic string operations is now available as a standard
library.


C3 Linearization
================

**2012-01-25** - :doc:`C3 superclass linearization </news/2012/01/25/c3>`

Open Dylan now uses the C3 superclass linearization algorithm. It is enabled
by default. This is more intuitive and will allow for further performance
improvements.


DSWANK
======

**2011-12-12** - :doc:`Dswank - emacs and DIME </news/2011/12/12/dswank>`

We developed DIME, the Dylan interaction mode for emacs, based on SLIME. Now,
you can browse the class hierarchy, get argument lists, locate definitions,
compile, view warnings and more, all from within emacs!


Release 2011.1
==============

**2011-12-10** - :doc:`New release: 2011.1 </news/2011/12/10/new_release>`

We just released Open Dylan 2011.1. This release is our first since moving to
GitHub and relicensing under a more liberal license. It brings many bugfixes,
inmproved platform support and new features.


New documentation
=================

**2011-11-22** - :doc:`New Documentation </news/2011/11/22/new_documentation>`

We're revitalizing our documentation and moving to a new publishing
system. We'll soon be able to provide our documentation in PDF and ePub as well
as HTML, with it looking much better and being more maintainable than in the
past.


New website
===========

**2011-11-22** - :doc:`Welcome to the New Website! </news/2011/11/22/welcome>`

As part of recharging Open Dylan development, we've re-done the website and
cleaned up a lot of the old, out-dated material here.

If you're looking for some of the old material, it can be found over at
`<https://web.archive.org/web/20170313134206/http://old.opendylan.org/>`_.
