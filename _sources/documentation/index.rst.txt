*************
Documentation
*************

Learn Dylan
===========

:doc:`../about/index`
    A quick overview of the language with examples of major features.

:doc:`../intro-dylan/index`
    This tutorial is written primarily for those with solid programming
    experience in C++ or another object-oriented, static language. It
    provides a gentler introduction to Dylan than does the :drm:`Dylan Reference
    Manual <Title>` (DRM).

`Dylan Programming Guide <https://package.opendylan.org/dylan-programming-book/>`_
    A book length Dylan tutorial.

:doc:`../getting-started-cli/index`
    Describes development using the Open Dylan command line tools
    and editor integration (like emacs). This is mainly for
    Linux, FreeBSD, and macOS users.

:doc:`../getting-started-ide/index`
    Describes Open Dylan's integrated development environment (Windows only).

:doc:`../building-with-duim/index`
    Describes how to use DUIM (Dylan User Interface Manager),
    the portable window programming toolkit. (Windows only.)

References
==========

:drm:`Dylan Reference Manual <Title>` (:drm:`Errata`)
    The official definition of the Dylan language and standard library.

:doc:`../library-reference/index`
    Reference docs for core libraries packaged with Open Dylan.

:doc:`../duim-reference/index`
    Describes the libraries forming DUIM (Dylan User Interface Manager),
    the portable window programming toolkit. It complements
    Building Applications Using DUIM. (Currently Windows only.)

:doc:`../corba-guide/index`
    A tutorial and reference for CORBA interoperability using the Open
    Dylan ORB.

Articles
========

:doc:`../articles/macro-system` by Dustin Voss.
    This article holds hard-won knowledge about how the Dylan macro system works
    and how to work around some of the gotchas that may catch a macro writer.

:doc:`../articles/procedural-dylan/index` by Paul Haahr.
    This essay explores Dylan from the perspective of a programmer used to
    traditional procedural languages, such as Pascal or C.

`Dylan Package Documentation <https://package.opendylan.org/http/server/quickstart/>`_
    A quick introduction to web development in Dylan.

:doc:`../news/2011/12/12/dswank` by Hannes Mehnert.
    An exciting look at using DIME and emacs for Dylan development.
    DIME is for Dylan what `SLIME <https://slime.common-lisp.dev/>`_
    is for Common Lisp.

:doc:`../articles/beyond-java`
    A discussion of some of the advantages Dylan provides over Java, based on
    Jason Trenouth's experience writing a CORBA IDL compiler in both Java and
    Dylan.  (Written around 2001 and Java now has better handling of numeric
    boxing/unboxing and, arguably, some other improvements.)

Publications
============

See the :doc:`publications page <publications>` for a full
list. Below are a few highlighted publications.

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

  https://zenodo.org/records/3742567

**Extending Dylan's type system for better type inference and error detection** (by Hannes Mehnert at ILC 2010 `dl.acm.org <https://doi.org/10.1145/1869643.1869645>`__ `bib <../_static/documentation/mehnert2010.bib>`__)
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

    https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.627.5175&rep=rep1&type=pdf

**D-Expressions: Lisp Power, Dylan Style** [`pdf <https://people.csail.mit.edu/jrb/Projects/dexprs.pdf>`__] [`bib <../_static/documentation/bachrach1999.bib>`__]
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

:doc:`../hacker-guide/index`
    A work in progress to help out people who are hacking on Open Dylan itself.

:doc:`../style-guide/index`
    Notes and thoughts on how to format your Dylan code. This is the style
    guide that we aspire to adhere to in the Open Dylan sources.

:doc:`../proposals/index`
    A series of proposals for improvements to the Open Dylan
    implementation and related libraries.

:doc:`../release-notes/index`
    Notes on new features and bug fixes in each release of Open Dylan.


.. TODO: some of the below links should be relative so they work in a test installation
   of the website.

.. toctree::
   :hidden:

   News </news/index>
   Publications <publications>
   Intro to Dylan </intro-dylan/index>
   CORBA Guide </corba-guide/index>
   DUIM Guide </building-with-duim/index>
   DUIM Reference </duim-reference/index>
   Getting Started / IDE </getting-started-ide/index>
   Release Notes </release-notes/index>
   Style Guide </style-guide/index>
   Dylan History </history/index>
