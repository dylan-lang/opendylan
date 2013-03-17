.. raw:: html

  <div class="row">
    <div class="span3 bs-docs-sidebar">
      <ul class="nav nav-list bs-docs-sidenav" data-spy="affix">
        <li><a href="#learning-dylan"><i class="icon-chevron-right"></i> Learning Dylan</a></li>
        <li><a href="#articles"><i class="icon-chevron-right"></i> Articles</a></li>
        <li><a href="#publications"><i class="icon-chevron-right"></i> Publications</a></li>
        <li><a href="#cheat-sheets"><i class="icon-chevron-right"></i> Cheat Sheets</a></li>
        <li><a href="#references"><i class="icon-chevron-right"></i> References</a></li>
        <li><a href="#for-open-dylan-developers"><i class="icon-chevron-right"></i> For Open Dylan Developers</a></li>
        <li><a href="#archived-documentation"><i class="icon-chevron-right"></i> Archived Documentation</a></li>
      </ul>
    </div>
    <div class="span9">

*************
Documentation
*************

.. warning:: We are in the process of converting over to a new documentation
   publishing system. Some documents are not entirely correct yet. We've
   retained links to the 'Old HTML' where relevant. These will be going away
   in the near future.
   :class: alert alert-block alert-warning

Learning Dylan
==============

.. raw:: html

     <div class="alert alert-block alert-success">
       <p>Just getting started with Open Dylan?  We recommend that
       you read the <a href="intro-dylan/">Introduction to Dylan</a>
       to get a feel for the language. After that, you can broaden
       your knowledge with the <a href="../books/dpg/">Dylan Programming</a>
       book.</p>
     </div>

`An Introduction to Dylan <intro-dylan/index.html>`_ [`pdf <intro-dylan/IntroductiontoDylan.pdf>`__] [`epub <intro-dylan/AnIntroductiontoDylan.epub>`__]
    This tutorial is written primarily for those with solid programming
    experience in C++ or another object-oriented, static language. It
    provides a gentler introduction to Dylan than does the Dylan Reference
    Manual (DRM).

`Dylan Programming <http://opendylan.org/books/dpg/>`_ [`pdf <http://opendylan.org/books/dpg/DylanProgramming.pdf>`__] [`epub <http://opendylan.org/books/dpg/DylanProgramming.epub>`__]
    A good, book length Dylan tutorial by several Harlequin employees.

`Getting Started with Open Dylan <getting-started/index.html>`_ [`pdf <getting-started/GettingStartedWithOpenDylan.pdf>`__] [`epub <getting-started/GettingStartedWithOpenDylan.epub>`__] [`old HTML <http://opendylan.org/documentation/opendylan/env/index.htm>`__]
    Describes Open Dylan's development environment. The first two
    chapters are useful on any platform, whereas the remaining
    chapters document the Open Dylan IDE on Windows.

`Building Applications Using DUIM <building-with-duim/index.html>`_ [`pdf <building-with-duim/BuildingApplicationsWithDUIM.pdf>`__] [`epub <building-with-duim/BuildingApplicationsWithDUIM.epub>`__] [`old HTML <http://opendylan.org/documentation/opendylan/dguide/index.htm>`__]
    Describes how to use DUIM (Dylan User Interface Manager),
    the portable window programming toolkit. This is only useful
    if you are using Open Dylan on Windows.

Articles
========

.. raw:: html

    <div class="alert alert-block alert-info">
      <p>Featured articles and blog postings.</p>
    </div>

    <h3>Learning Dylan</h3>

`Dylan Macro System <../articles/macro-system/index.html>`_ by Dustin Voss.
    This article holds hard-won knowledge about how the Dylan macro system works
    and how to work around some of the gotchas that may catch a macro writer.

`Procedural Dylan <../articles/procedural-dylan/index.html>`_ by Paul Haahr.
    This essay explores Dylan from the perspective of a programmer used to
    traditional procedural languages, such as Pascal or C.

.. raw:: html

    <h3>Tools</h3>

`Development inside emacs using DIME <../news/2011/12/12/dswank.html>`_
    An exciting look at using DIME and emacs for Dylan development.

Publications
============

`See our publications page <publications.html>`_.

Cheat Sheets
============

.. raw:: html

     <div class="alert alert-block alert-info">
       <p>Quick one-page sheets for common tasks.</p>
     </div>

.. hlist::

   * `Basics of Dylan Syntax <cheatsheet.html>`_
   * `Iteration <cheatsheets/iteration.html>`_
   * `Conditionals <cheatsheets/conditionals.html>`_
   * `Collections <cheatsheets/collections.html>`_
   * `For Scheme programmers <cheatsheets/scheme.html>`_

References
==========

.. raw:: html

     <div class="alert alert-block alert-info">
       <p>These are some lengthier reference materials. While they
       make for dry reading, they're full of invaluable information!</p>
     </div>

`Dylan Reference Manual <http://opendylan.org/books/drm/>`_ (`Errata <http://opendylan.org/books/drm/drm_errata.html>`_)
    The official definition of the Dylan language and standard library.

`Dylan Library Reference <library-reference/index.html>`_ [`pdf <library-reference/DylanLibraryReference.pdf>`__] [`epub <library-reference/DylanLibraryReference.epub>`__]
    Describes the Open Dylan implementation of the Dylan language, a
    core set of Dylan libraries, and a library interchange mechanism.
    The core libraries provide many language extensions, a threads
    interface, and object finalization, printing and output formatting modules,
    a streams module, a sockets module, and modules providing an
    interface to operating system features such as the file system,
    time and date information, the host machine environment, as well
    as a foreign function interface and some low-level access to the
    Microsoft Win32 API.

`DUIM library reference <http://opendylan.org/documentation/opendylan/dref/index.htm>`_
    Describes the libraries forming DUIM (Dylan User Interface Manager),
    the portable window programming toolkit. It complements
    Building Applications Using DUIM.

For Open Dylan Developers
=========================

.. raw:: html

     <div class="alert alert-block alert-info">
       <p>Notes and materials useful to those working on
       Open Dylan itself or those who have an interest in the low
       level details.</p>
     </div>

`Open Dylan Hacker's Guide <hacker-guide/index.html>`_ [`pdf <hacker-guide/OpenDylanHackersGuide.pdf>`__] [`epub <hacker-guide/OpenDylanHackersGuide.epub>`__]
    A work in progress to help out people who are hacking on Open Dylan itself.

`Dylan Style Guide <style-guide/index.html>`_ [`pdf <style-guide/StyleGuide.pdf>`__] [`epub <style-guide/StyleGuide.epub>`__]
    Notes and thoughts on how to format your Dylan code. This is the style
    guide that we aspire to adhere to in the Open Dylan sources.

`Dylan Enhancement Proposals <../proposals/index.html>`_
    A series of proposals for improvements to the Open Dylan
    implementation and related libraries.


Archived Documentation
======================

.. raw:: html

      <div class="alert alert-block alert-warning">
        <p>This is old documentation that we don't plan to
        bring forward. Let us know if there's interest in this
        material.</p>
      </div>

`Developing Component Software with CORBA <http://opendylan.org/documentation/opendylan/corba/index.htm>`_
    A tutorial and reference for CORBA interoperability using the Open Dylan ORB.

`OLE, COM, ActiveX and DBMS library reference <http://opendylan.org/documentation/opendylan/interop2/index.htm>`_
    Describes high and low level interfaces to COM, OLE, and
    ActiveX component technology, and generic DBMS support, through
    SQL with an ODBC backend.

.. raw:: html

      </div>
    </div>
