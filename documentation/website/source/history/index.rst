.. raw:: html

  <div class="row">
    <div class="span3 bs-docs-sidebar">
      <ul class="nav nav-list bs-docs-sidenav" data-spy="affix">
        <li><a href="#apple-dylan"><i class="icon-chevron-right"></i> Apple Dylan</a></li>
        <li><a href="#change-in-syntax"><i class="icon-chevron-right"></i> Change in Syntax</a></li>
        <li><a href="#harlequin-dylan"><i class="icon-chevron-right"></i> Harlequin Dylan</a></li>
        <li><a href="#gwydion-dylan"><i class="icon-chevron-right"></i> Gwydion Dylan</a></li>
        <li><a href="#open-source"><i class="icon-chevron-right"></i> Open Source</a></li>
        <li><a href="#other-implementations"><i class="icon-chevron-right"></i> Other Implementations</a></li>
      </ul>
    </div>
    <div class="span9">

*******
History
*******

Dylan is alive and well today, but it has a long history.

Dylan was created in the early 1990s. It was originally a project within
Apple Computer, but was expanded to a partnership between Apple, Harlequin
and Carnegie Mellon University (CMU).

The original Dylan is now referred to as "Prefix Dylan" and used a
traditional Lisp-style syntax::

   (define-method sum ((numbers <sequence>))
     (reduce1 + numbers))
   (define-method square ((x <number>)) (* x x))
   (define-method square-all ((coords <sequence>))
     (map square coords)
   (define distance (compose sqrt sum square-all))

Wikipedia also provides a `history of the Dylan programming language
<http://en.wikipedia.org/wiki/History_of_the_Dylan_programming_language>`_
which provides additional information not included here.

Apple Dylan
===========

At this time, the primary implementation of Dylan was **Apple Dylan**.
While Dylan was originally intended for use on the Apple Newton platform,
that didn't work out and their focus shifted to Dylan on the desktop
on classic Mac OS.

Some years ago, Paul R Potts wrote about his experiences with
`Apple Dylan <apple-dylan/index>`_, including some screenshots. Apple
distributed a `brochure at WWDC in 1994 <../_static/dylanwwdc94brochure.pdf>`_
(PDF) that described Dylan and the features that it was going to bring to
the developer experience.

Change in Syntax
================

In late 1993 or 1994, CMU and Apple decided to change the Dylan syntax to
what we currently have now, the infix syntax. The value of this change was
a subject of much debate, both at the time and in the subsequent
years. At the time, it was thought that the world wasn't ready for a
mainstream language with a Lisp syntax, so a more familiar syntax
might be better. There were also discussions at the time of supporting
both the prefix and the infix syntax within the same compiler, something
which never came to fruition.

Harlequin Dylan
===============

Sometime in 1993, Harlequin started a project to produce a Dylan
development environment, DylanWorks. Harlequin, at the time, was known
for their LispWorks product. Keith Playford was their first hire to
work specifically on the Dylan project. Keith had previously
been involved `EuLisp <http://en.wikipedia.org/wiki/EuLisp>`_.

Harlequin's Dylan was originally implemented using the prefix syntax
and later converted to the current infix syntax. The first implementation
was a Dylan emulator running on top of LispWorks as described in this
`Hacker News comment <https://news.ycombinator.com/item?id=8853095>`_:

   When Harlequin started out with Dylan, to bootstrap we first quickly
   morphed LispWorks into a high-functioning Dylan IDE. This was done
   through a combination of macro, reader macro and CLOS MetaObject
   Protocol abuse, allowing us to achieve a faithful rendition of
   Dylan's syntax and object system semantics respectively.

   When Dylan went from prefix to infix syntax the reader macro
   indirection got all the more elaborate: what started out as a
   trivial reader macro that treated colons differently started
   delegating to a full-blown lex & yacc style parser instead.

   All the better, most of this was achieved modularly: Lisp editor
   buffers and Lisp listeners would run happily alongside Dylan
   editor buffers and Dylan listeners, Lisp code could still compile
   load and run alongside Dylan code, and Lisp classes could
   interoperate cleanly with Dylan classes.

This `emulator <https://github.com/dylan-lang/opendylan/tree/5f9225d246d771cfaa2fa4f255a4447bff6991b8/old/Sources/emulator>`_
can be found in an old revision of Open Dylan's git repository.

While DylanWorks ran on multiple platforms, the commercial goal
was to ship a full development environment on Windows. Unfortunately,
at the time, LispWorks did not yet run on Windows and there were
goals that were difficult to achieve with LispWorks (like producing
native shared libraries and executables). Work began to replace
the original emulator with a full-blown compiler and code-generator
written in Dylan.

HARP
----

HARP is the Harlequin Abstract RISC Processor and was designed and
developed at Harlequin in the late 1980s. It was used in Harlequin's
LispWorks and later translated to Dylan for use in Harlequin's
DylanWorks (which is now Open Dylan).

Clive Tong, an engineer at Harlequin in 1989, `briefly described
<http://clivetong.wordpress.com/2012/12/21/so-many-architectures-so-little-time/>`_
it as:

   The compiler targeted an instruction set known as HARP (Harlequin
   Abstract RISC Processor), and then HARP instructions were translated
   into machine instructions using a template matching scheme. HARP had
   an infinite set of registers, and the register colouring happened as
   part of this templating processing.

Some additional details about the early design of HARP are available in
`Techniques for Dynamic Software Migration <http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.47.2525>`_
from 1988. Other early sources of information about it, written by Hunter
and Knightbridge, appear to be lost to the sands of time.

DFMC
----

DFMC (Dylan Flow Machine Compiler) was the Dylan compiler that resulted
from the work at Harlequin on DylanWorks. (There were other earlier
compilers.) This is the compiler that is still in use today in Open Dylan.

DUIM
----

Scott McKay, formerly of `Symbolics <http://en.wikipedia.org/wiki/Symbolics>`_,
produced a cross-platform GUI toolkit for DylanWorks called DUIM, or the
Dylan User Interface Manager. This was based on his previous work on
`CLIM <http://en.wikipedia.org/wiki/Common_Lisp_Interface_Manager>`_,
which in turn was based on Dynamic Windows from Symbolics. While CLIM was
the result of collaboration between multiple Lisp companies, DUIM was the
work of Harlequin and drew heavily upon the CLIM design.

DUIM is the basis for the Open Dylan IDE. Unfortunately, only the Windows
backend for DUIM remains functional.

DUIM documentation can be found amongst the `Open Dylan documentation
<../documentation/>`_.

Deuce
-----

Scott McKay also worked on the Deuce editor that was used in the DylanWorks
IDE. He wrote about it briefly in a `newsgroup posting
<https://groups.google.com/forum/#!msg/comp.lang.dylan/3uuUb3Z9pAc/6NbE9gYpeAIJ>`_.
Bruce Mitchener also wrote briefly about the architecture of Deuce on the
`Atom editor discussion forums <https://discuss.atom.io/t/the-deuce-editor-architecture/2218>`_.

Memory Pool System
------------------

The `Memory Pool System <http://www.ravenbrook.com/project/mps>`_ garbage
collector was originally written for DylanWorks and subsequently re-used
in other projects. It lives on today as a separate product (and is still
used by Open Dylan).

Harlequin's Demise
------------------

Unfortunately, for a variety of reasons, Harlequin fell upon hard times.
A group of Harlequin employees started a company **Functional Objects**
and re-named **Harlequin DylanWorks** to **Functional Developer** and
attempted to continue the development.

Open Sourcing
-------------

In 2004, **Functional Objects** decided to open source the **Functional
Developer** code. It was adopted by an open source community and renamed
**Open Dylan**. Open Dylan development continues today.

Gwydion Dylan
=============

In 1993, Carnegie Mellon University kicked off a project to create a
new Dylan implementation, the Gwydion Project. The team at CMU had
previously done extensive work in the Lisp field including work on
`Spice Lisp <http://en.wikipedia.org/wiki/Spice_Lisp>`_ which evolved
into `CMUCL <http://en.wikipedia.org/wiki/CMU_Common_Lisp>`_.  This
team was led by `Scott Fahlman <http://en.wikipedia.org/wiki/Scott_Fahlman>`_.

Mindy
-----

The Gwydion team first produced a Dylan environment known as **Mindy**
or **Mindy Is Not Dylan Yet**. This compiler was used to bootstrap
the subsequent d2c compiler and supported a subset of the full
Dylan language. Notably, it did not support macros.

d2c
---

The Gwydion team's main Dylan implementation work was focused on
**d2c**, an optimizing Dylan compiler that generated C code which
could then be compiled to native code.

Gwydion's Demise
----------------

The Gwydion team ceased their work on Dylan due to lack of funding
and other concerns.

Scott Fahlman announced that Dylan was in the hands of Harlequin
in a `September 1998 newsgroup post <https://groups.google.com/forum/embed/#!topic/comp.lang.dylan/h1Hs6Jm-rb4>`_.

The Gywdion Dylan implementation at that point had already been
picked up by an open source community, led by Eric Kidd and
Andreas Bogk.

Open Source
===========

In the late 1990s, a group of programmers took over the maintenance
and development of the Gwydion Dylan implementation. They continued
this development for almost 15 years until the Gwydion Dylan compiler
was retired in favor of a focus on the Open Dylan compiler.

The Open Dylan compiler was the result of the open sourcing of the
Functional Developer (previously Harlequin DylanWorks) compiler in
2004. Development on Open Dylan continued in parallel with Gwydion
Dylan until 2012, when it became the focus of the Dylan hackers.

Other Implementations
=====================

Over the years, there have been a variety of other Dylan implementations.

* In 1992, Digital Equipment Corporation (DEC) had an implementation,
  Thomas, of the original prefix Dylan, written in Scheme, running
  on top of Gambit Scheme.
  One of the authors wrote about his `experiences with creating Thomas
  <https://groups.csail.mit.edu/mac/ftpdir/scheme-mail/HTML/rrrs-1992/msg00197.html>`_.
* In 1994, Jonathan Bachrach had an implementation written on top of
  CMUCL using a set of macros. This implementation has been lost.
* The `Marlais <http://marlais.sourceforge.net/>`_ implementation was
  abandoned in 2001.
* Dominique Boucher wrote `IDyl <https://github.com/schemeway/idyl>`_,
  a Dylan interpreter in Scheme in the mid-1990s.
* RScheme is Scheme-like language perhaps inspired by Dylan. There was
  some interest in Dylan from UTexas and Paul R. Wilson, but it isn't
  clear from the remaining historical data what exactly happened.

.. toctree::
   :hidden:

   apple-dylan/index

.. raw:: html

     </div>
   </div>
