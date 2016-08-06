######################
The Dylan Macro System
######################

Dylan macros simplify boilerplate code and provide syntactic shorthand. They are
useful in small jobs within a particular file (e.g. making a series of
repetitive declarations) and for larger jobs (e.g. constructing a GUI through
creating and associating objects). Easy jobs are easy to do with macros, but the
complicated jobs get hard fast.

This document describes how the Dylan macro system works and some techniques you
can use when writing your own macros. I gloss over some of the implementation
details and present this information more informally than the `Dylan
Reference Manual`:title: does.

.. toctree::
   :maxdepth: 2

   background-overview
   macro-types
   patterns
   pattern-variables
   substitutions
   auxiliary-rules
   hygiene
   faq-tips
