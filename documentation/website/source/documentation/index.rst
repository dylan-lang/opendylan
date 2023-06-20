*************
Documentation
*************

Learn Dylan
===========

`Tour of Dylan`_
    A quick overview of the language with examples of major features.

`Introduction to Dylan`_ [`pdf <intro-dylan/IntroductiontoDylan.pdf>`__] [`epub <intro-dylan/AnIntroductiontoDylan.epub>`__]
    This tutorial is written primarily for those with solid programming
    experience in C++ or another object-oriented, static language. It
    provides a gentler introduction to Dylan than does the `Dylan Reference
    Manual`_ (DRM).

`Dylan Programming Guide`_ [`pdf <https://opendylan.org/books/dpg/DylanProgramming.pdf>`__] [`epub <https://opendylan.org/books/dpg/DylanProgramming.epub>`__]
    A book length Dylan tutorial.

`Getting Started -- CLI`_ [`pdf <getting-started-cli/GettingStartedWithTheOpenDylanCLI.pdf>`__] [`epub <getting-started-cli/GettingStartedWithTheOpenDylanCLI.epub>`__]
    Describes development using the Open Dylan command line tools
    and editor integration (like emacs). This is mainly for
    Linux, FreeBSD, and macOS users.

`Getting Started -- IDE`_ [`pdf <getting-started-ide/GettingStartedWithTheOpenDylanIDE.pdf>`__] [`epub <getting-started-ide/GettingStartedWithTheOpenDylanIDE.epub>`__]
    Describes Open Dylan's integrated development environment (Windows only).

`Building Applications Using DUIM`_ [`pdf <building-with-duim/BuildingApplicationsWithDUIM.pdf>`__] [`epub <building-with-duim/BuildingApplicationsWithDUIM.epub>`__]
    Describes how to use DUIM (Dylan User Interface Manager),
    the portable window programming toolkit. (Windows only.)

References
==========

`Dylan Reference Manual`_ (`Errata`_)
    The official definition of the Dylan language and standard library.

`Library Reference`_ [`pdf <library-reference/DylanLibraryReference.pdf>`__] [`epub <library-reference/DylanLibraryReference.epub>`__]
    Reference docs for core libraries packaged with Open Dylan.

`DUIM library reference <duim-reference/index.html>`_ [`pdf <duim-reference/DUIMReference.pdf>`__] [`epub <duim-reference/DUIMReference.epub>`__] [`old HTML <http://web.archive.org/web/20170102233258/http://opendylan.org/documentation/opendylan/dref/index.htm>`__]
    Describes the libraries forming DUIM (Dylan User Interface Manager),
    the portable window programming toolkit. It complements
    Building Applications Using DUIM. (Currently Windows only.)

Advanced Guides
===============

`Developing Component Software with CORBA <corba-guide/index.html>`_
    A tutorial and reference for CORBA interoperability using the Open
    Dylan ORB.

External Libraries
==================

These are libraries that aren't bundled with Open Dylan.

`Binary-Data </documentation/binary-data/>`_ [`pdf </documentation/binary-data/BinaryData.pdf>`__] [`epub </documentation/binary-data/BinaryData.epub>`__]
    Parse and assemble binary data using high level Dylan objects.

`Concurrency </documentation/concurrency/>`_ [`pdf </documentation/concurrency/ConcurrencyUserGuide.pdf>`__] [`epub </documentation/concurrency/ConcurrencyUserGuide.epub>`__]
    Concurrency utilities.

`dylan-tool </documentation/dylan-tool/>`_ [`epub </documentation/dylan-tool/dylan-tool.epub>`__]
    Provides the ``dylan`` command to manage workspaces and package
    dependencies, and create the registry.

`HTTP </documentation/http/>`_ [`pdf </documentation/http/HTTPLibraries.pdf>`__] [`epub </documentation/http/HTTPLibraries.epub>`__]
    An HTTP client, server, and templating engine.

`Melange </documentation/melange/>`_ [`pdf </documentation/melange/MelangeUserGuide.pdf>`__] [`epub </documentation/melange/MelangeUserGuide.epub>`__]
    Generates Dylan code to wrap C libraries.

`Objective C Bridge </documentation/objc-dylan/>`_ [`pdf </documentation/objc-dylan/ObjectiveCBridgeUserGuide.pdf>`__] [`epub </documentation/objc-dylan/ObjectiveCBridgeUserGuide.epub>`__]
    Provides a bridge between Objective C and Dylan, allowing integration with
    Objective C libraries.

`Statistics </documentation/statistics/>`_ [`pdf </documentation/statistics/StatisticsUserGuide.pdf>`__] [`epub </documentation/statistics/StatisticsUserGuide.epub>`__]
    A collection of libraries for performing statistical analysis.

`Testworks </documentation/testworks/>`_ [`pdf </documentation/testworks/TestworksUserGuide.pdf>`__] [`epub </documentation/testworks/TestworksUserGuide.epub>`__]
    A unit testing and benchmarking framework.

`Tracing </documentation/tracing/>`_ [`pdf </documentation/tracing/TracingUserGuide.pdf>`__] [`epub </documentation/tracing/TracingUserGuide.epub>`__]
    An alternative to traditional logging and performance measurements.

Articles
========

`Dylan Macro System <../articles/macro-system.html>`_ by Dustin Voss.
    This article holds hard-won knowledge about how the Dylan macro system works
    and how to work around some of the gotchas that may catch a macro writer.

`Procedural Dylan <../articles/procedural-dylan/index.html>`_ by Paul Haahr.
    This essay explores Dylan from the perspective of a programmer used to
    traditional procedural languages, such as Pascal or C.

`Dylan Web in 60 Seconds </documentation/http/server/quickstart/index.html>`_
    A quick introduction to web development in Dylan.

`Development inside emacs using DIME <../news/2011/12/12/dswank.html>`_
    An exciting look at using DIME and emacs for Dylan development.
    DIME is for Dylan what `SLIME <https://common-lisp.net/project/slime/>`_
    is for Common Lisp.

`Beyond Java? <../articles/beyond-java.html>`_
    A discussion of some of the advantages Dylan provides over Java, based on
    Jason Trenouth's experience writing a CORBA IDL compiler in both Java and
    Dylan.  (Written around 2001 and Java now has better handling of numeric
    boxing/unboxing.)

Publications
============

See the `publications page <publications.html>`_ for a full list. Below are a
few highlighted publications.

**LLVM Code Generation for Open Dylan** (by Peter Housel at ELS 2020 `pdf <https://zenodo.org/record/3742567/files/els2020-opendylan.pdf?download=1>`__ `bib <../_static/documentation/housel_peter_s_2020_3742567.bib>`__ `slides <https://european-lisp-symposium.org/static/2020/housel-slides.pdf>`__ `video <https://www.youtube.com/watch?v=6dcrXBzw4H4>`__)
  The Open Dylan compiler, DFMC, was originally designed in the 1990s
  to compile Dylan language code targeting the 32-bit Intel x86
  platform, or other platforms via portable C. As platforms have
  evolved since, this approach has been unable to provide efficient
  code generation for a broader range of target platforms, or to
  adequately support tools such as debuggers, profilers, and code
  coverage analyzers.

  Developing a code generator for Open Dylan that uses the LLVM
  compiler infrastructure is enabling us to support these goals and
  modernize our implementation. This work describes the design
  decisions and engineering trade-offs that have influenced the
  implementation of the LLVM back-end and its associated run-time
  support.

  https://doi.org/10.5281/zenodo.3742567

**Extending Dylan's type system for better type inference and error detection** (by Hannes Mehnert at ILC 2010 `pdf <http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.627.5175&rep=rep1&type=pdf>`__ `bib <../_static/documentation/mehnert2010.bib>`__)
    Whereas dynamic typing enables rapid prototyping and easy
    experimentation, static typing provides early error detection and
    better compile time optimization. Gradual typing provides the best
    of both worlds. This paper shows how to define and implement
    gradual typing in Dylan, traditionally a dynamically typed
    language. Dylan poses several special challenges for gradual
    typing, such as multiple return values, variable-arity methods and
    generic functions (multiple dispatch).

    In this paper Dylan is extended with function types and parametric
    polymorphism. We implemented the type system and a
    unification-based type inference algorithm in the mainstream Dylan
    compiler. As case study we use the Dylan standard library (roughly
    32000 lines of code), which witnesses that the implementation
    generates faster code with fewer errors. Some previously
    undiscovered errors in the Dylan library were revealed.

    https://dl.acm.org/doi/10.1145/1869643.1869645

**D-Expressions: Lisp Power, Dylan Style** [`pdf <http://people.csail.mit.edu/jrb/Projects/dexprs.pdf>`__] [`bib <../_static/documentation/bachrach1999.bib>`__]
    This paper aims to demonstrate that it is possible for a language
    with a rich, conventional syntax to provide Lisp-style macro power
    and simplicity. We describe a macro system and syntax manipulation
    toolkit designed for the Dylan programming language that meets,
    and in some areas exceeds, this standard. The debt to Lisp is
    great, however, since although Dylan has a conventional algebraic
    syntax, the approach taken to describe and represent that syntax
    is distinctly Lisp-like in philosophy.

For Open Dylan Developers
=========================

.. note:: Notes and materials useful to those working on Open Dylan itself or
          those who have an interest in the low level details.

`Open Dylan Hacker's Guide <hacker-guide/index.html>`_ [`pdf <hacker-guide/OpenDylanHackersGuide.pdf>`__] [`epub <hacker-guide/OpenDylanHackersGuide.epub>`__]
    A work in progress to help out people who are hacking on Open Dylan itself.

`Dylan Style Guide <style-guide/index.html>`_ [`pdf <style-guide/StyleGuide.pdf>`__] [`epub <style-guide/StyleGuide.epub>`__]
    Notes and thoughts on how to format your Dylan code. This is the style
    guide that we aspire to adhere to in the Open Dylan sources.

`Dylan Enhancement Proposals <../proposals/index.html>`_
    A series of proposals for improvements to the Open Dylan
    implementation and related libraries.

`Open Dylan Release Notes <release-notes/index.html>`_
    Notes on new features and bug fixes in each release of Open Dylan.


.. TODO: some of the below links should be relative so they work in a test installation
   of the website.

.. _Building Applications Using DUIM: building-with-duim/index.html
.. _Dylan Programming Guide: https://opendylan.org/books/dpg/
.. _Dylan Reference Manual: https://opendylan.org/books/drm/
.. _Errata: https://opendylan.org/books/drm/Errata
.. _Getting Started -- CLI: getting-started-cli/index.html
.. _Getting Started -- IDE: getting-started-ide/index.html
.. _Gitter: https://gitter.im/dylan-lang/general
.. _Introduction to Dylan: intro-dylan/index.html
.. _Library Reference: library-reference/index.html
.. _Tour of Dylan: /about/

.. toctree::
   :hidden:

   cheatsheets/index
   Publications <publications>
