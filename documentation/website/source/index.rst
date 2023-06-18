:hide-toc:

**********
Open Dylan
**********

..
   TODO: either reference
   https://blog.codecentric.de/en/2015/08/essence-of-object-functional-programming-practical-potential-of-scala/
   or better, write up our own description of object functional.

.. raw:: html

   <div style="font-style: italic">

Dylan is an `object-functional`_ language originally `created by Apple`_ for the
Newton. Dylan provides a programming model designed to support efficient machine code
generation, including fine-grained control over dynamic and static behaviors.

.. raw:: html

   </div>

.. _Dylan: books/drm/Title
.. _object-functional: about/examples/generic_functions.html
.. _created by Apple: history/


* *Gradual typing* - specify types only where necessary for clarity or performance.
* *Generic functions* - dispatch on all positional arguments, for a powerful form of OO.
* *Multiple inheritance* - use mix-ins to add behavior.
* *Sealed domains* - for high performance method dispatch.
* *Garbage collection* - you know you want it.
* *Macros* - to extend the syntax.
* *Exception handling* - doesn't unwind the stack, advanced recovery protocols.
* *Regular and simple syntax*.
* *Everything returns values* - including conditionals like ``if`` and ``select``.

.. raw:: html

   <style>
   .button,a.button {
       background-color: #EA8D2F;
       /* border-radius: 5px; */
       border: none;
       cursor: pointer;
       display: inline-block;
       font-size: 18px;
       font-style: bold;
       color: white;
       margin: 20px 2px;
       padding: 10px 32px;
       text-align: center;
       text-decoration: none;
   }
   </style>
   <a href="download/" class="button">Install Dylan...</a>

How do you like to learn?
-------------------------

* The `Tour of Dylan </about/>`_ is a quick (30 minute) overview of the language.
* If you want to play around without having to install anything, try the `Playground`_.
* `Introduction to Dylan`_ provides a high-level overview of language features.
* The `Dylan Programming Guide`_ is a book length Dylan tutorial.
* Like to jump in the deep end?  The `Dylan Reference Manual`_ might be for you.
* If you have questions, give us a shout on `Matrix`_!

Or explore `all the docs </documentation/>`_, including cheat sheets, articles, and all
the library docs. Happy hacking!

.. _Playground: https://play.opendylan.org
.. _Introduction to Dylan: https://opendylan.org/documentation/intro-dylan/
.. _Dylan Programming Guide: https://opendylan.org/books/dpg/
.. _Dylan Reference Manual: https://opendylan.org/books/drm/
.. _Matrix: https://app.element.io/#/room/#dylan-language:matrix.org

.. toctree::
   :maxdepth: 3
   :hidden:

   Tour of Dylan <about/index>
   Documentation <documentation/index>
   Get Involved <community/index>
   Downloads <download/index>
   articles/index
   news/index
   Enhancement Proposals <proposals/index>
   history/index

.. -*- tab-width: 4 -*-
