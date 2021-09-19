Open Dylan
**********

An open source implementation of the `Dylan`_ programming language.

.. raw:: html

   <div class="dylan-top-quote">

..
   TODO: either reference
   https://blog.codecentric.de/en/2015/08/essence-of-object-functional-programming-practical-potential-of-scala/
   or better, write up our own description of object functional.

Dylan is an `object-functional`_ language originally `created by Apple`_ for the
Newton. Dylan provides a programming model designed to support efficient machine code
generation, including fine-grained control over dynamic and static behaviors.

.. raw:: html

   </div>

.. _Dylan: /books/drm/Title
.. _object-functional: /about/examples/generic_functions.html
.. _created by Apple: /history/


* *Gradual typing* - specify types only where necessary for clarity or performance.
* *Generic functions* - dispatch on all positional arguments, for a powerful form of OO.
* *Multiple inheritance* - use mix-ins to add behavior.
* *Sealed domains* - for high performance method dispatch.
* *Garbage collection* - you know you want it.
* *Macros* - to extend the syntax.
* *Exception handling* - doesn't unwind the stack, advanced recovery protocols.
* *Regular and simple syntax*.
* *Everything returns values* - including conditionals like ``if`` and ``select``.


Get Started
===========

.. raw:: html

   <a href="/download" class="button">Install Dylan...</a>

How do you like to learn?
-------------------------

* The `Tour of Dylan </about/>`_ is great if you like to learn by example.
* If you want to play around without having to install anything, try the `Playground`_.
* `Introduction to Dylan`_ provides a high-level overview of language features.
* The `Dylan Programming Guide`_ is a book length Dylan tutorial.
* Like to jump in the deep end?  The `Dylan Reference Manual`_ might be for you.
* If you have questions, give us a shout on `Gitter`_!

Or explore `all the docs </documentation/>`_, including cheat sheets, articles, and all
the library docs. Happy hacking!

.. _Playground: https://play.opendylan.org
.. _Introduction to Dylan: https://opendylan.org/documentation/intro-dylan/
.. _Dylan Programming Guide: https://opendylan.org/books/dpg/
.. _Dylan Reference Manual: https://opendylan.org/books/drm/
.. _Gitter: https://gitter.im/dylan-lang/general

.. toctree::
   :maxdepth: 1
   :hidden:
   :glob:

   */*
   articles/*/*
   documentation/cheatsheets/*
   news/*/*/*/*
   community/gsoc/*

.. -*- tab-width: 4 -*-
