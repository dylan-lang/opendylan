:hide-toc:

*********************
A Quick Tour of Dylan
*********************

Before starting the Tour, a few quick notes:

1. You may find it useful to get a better feel for Dylan by pasting some of the
   Tour examples into the Dylan Playground: https://play.opendylan.org (We hope
   to integrate the playground directly into the Tour soon, to make this
   easier.)

2. A little more background on Dylan's history and goals may be helpful, in
   which case keep reading below. Otherwise, feel free to go directly to
   :doc:`examples/hello_world`.

A bit more background on Dylan
==============================

Scheme and Common Lisp had a strong influence on Dylan. Like Scheme, Dylan is a "LISP-1",
meaning it has a single namespace for variables and functions. Dylan's object system is
derived from the Common Lisp Object System (CLOS), but simplified to allow for better
compiler optimizations.

In general, Dylan attempts to address potential performance issues by
introducing "natural" limits to the full flexibility of Lisp systems, for
example by allowing the compiler to clearly understand compilable units (i.e.,
libraries).

In Dylan, all values (numbers, functions, classes, types, etc.) are first-class
objects. Dylan supports multiple inheritance, polymorphism, multiple dispatch,
keyword arguments, object introspection, pattern-based syntax extension macros,
and many other features.

Dylan programs have fine-grained control over dynamism, for applications that occupy a
continuum between dynamic and static programming and supporting evolutionary development
(allowing for rapid prototyping followed by incremental refinement and optimization). The
Open Dylan compiler provides both "production" and "rapid development" modes. In
development mode many of the static restrictions are removed from the generated code, for
increased flexibility.


.. toctree::
    :maxdepth: 1
    :hidden:

    examples/hello_world
    examples/everything_value
    examples/classes
    examples/generic_functions
    examples/getters_setters_functions
    examples/keyword_arguments
    examples/limited_types
    examples/macros
    examples/multiple_return_values
