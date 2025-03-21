:hide-toc:

**********
Open Dylan
**********

..
   TODO: either reference
   https://blog.codecentric.de/en/2015/08/essence-of-object-functional-programming-practical-potential-of-scala/
   or better, write up our own description of object functional.

.. raw:: html

   <div style="font-style: italic; margin-left: 50px; margin-right: 50px">

Dylan is an :doc:`object-functional <about/examples/generic_functions>`
language originally :doc:`created by Apple <history/index>` for the
Newton. Dylan is a direct descendant of Scheme and CLOS (without the Lisp
syntax) with a programming model designed to support efficient machine code
generation, including fine-grained control over dynamic and static behaviors.

.. raw:: html

   </div>

New to Dylan?  We recommend starting with the :doc:`Tour of Dylan
<about/index>`, a quick (30 minute) overview of the language, and/or stop by
and ask questions on `Matrix`_.

If you want to play around without having to install anything, try out the
`Playground`_. It has a fixed set of imports so you don't have to worry about
how to define libraries and modules right away.

Then move on to one of these in-depth guides:

* :doc:`intro-dylan/index` provides a high-level overview of language
  features.
* `Dylan Programming Guide`_ is a book length Dylan tutorial.

The `Dylan Reference Manual`_, besides being the official language definition,
has an excellent, very brief `introduction
<https://opendylan.org/books/drm/Introduction>`_ describing the language
background and goals.

Or explore :doc:`all the docs <documentation/index>`, including cheat sheets,
articles, and all the library docs.

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
   <center>
     <a href="download/" class="button">Install Dylan...</a>
     &nbsp;&nbsp;&nbsp;&nbsp;
     <a href="https://play.opendylan.org" class="button">Try Dylan in your browser</a>
   </center>

*Happy hacking!*

.. _Dylan Programming Guide: https://package.opendylan.org/dylan-programming-book/
.. _Dylan Reference Manual: https://opendylan.org/books/drm/
.. _Dylan: books/drm/Title
.. _Matrix: https://app.element.io/#/room/#dylan-language:matrix.org
.. _Playground: https://play.opendylan.org

.. toctree::
   :caption: Community
   :hidden:

   Get Involved <community/index>
   Download <download/index>
   Hacker Guide <hacker-guide/index>
   Enhancement Proposals <proposals/index>

.. toctree::
   :caption: Learning
   :hidden:

   Tour of Dylan <about/index>
   Getting Started Guide <getting-started-cli/index>
   Dylan Programming Guide <https://package.opendylan.org/dylan-programming-book/>
   Dylan Playground <https://play.opendylan.org>
   Cheat Sheets <documentation/cheatsheets/index>

.. toctree::
   :caption: Reference
   :hidden:

   Dylan Reference Manual <https://opendylan.org/books/drm/>
   Package Docs <https://package.opendylan.org/>
   Bundled Libraries <library-reference/index>
   Other Documentation <documentation/index>
   Full Index <genindex>
