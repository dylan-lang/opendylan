************************
Alternative Curry Syntax
************************

.. warning:: This is an experimental extension. It is not
   well specified and may change or disappear in the future.
   This documentation is largely in the format of a DEP in
   case we want to propose it in the future.

Abstract
========

An alternative syntax for currying functions has been provided
within Open Dylan.

Copyright
=========

This document is public domain.

Specification
=============

Functions called with an "omitted" or placeholder parameter return
a curried function as if :drm:`curry` and :drm:`rcurry` had been
used.

There may be multiple placeholder parameters and they can be in
any parameter position. A curried function that has 2 placeholder
parameters will need 2 parameters when called.

This can be used with operators as well.

Motivation
==========

There are situations in which using :drm:`curry` and :drm:`rcurry`
can result in confusing code that is difficult to understand at
a quick glance. The alternative syntax provides an easy to understand
way to set up a curried function.

Rationale
=========

The alternative curry syntax was implemented in 1999 by Keith Playford
and has been in the compiler since then. It does not break any existing
code.

Examples
========

In this example, the two curried functions will have the same result:

.. code-block:: dylan

  let to-string-1 = curry(as, <byte-string>);
  let to-string-2 = as(<byte-string>, _);

As indicated earlier, multiple arguments being omitted is supported:

.. code-block:: dylan

  define function adder(a, b, c)
    a + b + c
  end;

  let a = adder(_, 2, _);
  let r = a(1, 3); // r == 6

Placeholder arguments can be used with operators as well:

.. code-block:: dylan

  let incr = _ + 1;
  let double = _ * 2;

These can be used inline with functions that take functions as arguments:

.. code-block:: dylan

  format-out("%=", map(_ + 1, #(1, 2, 3, 4));

Taking some examples from the Open Dylan compiler, some simplifications are
possible:

.. code-block:: dylan

  map(method (o) print-condition(o, #t) end, o.subnotes)

  let outer-values
    = map(method (variable :: <string>)
            jam-variable(jam, variable, default: #f)
          end,
          variables);

could be rewritten as:

.. code-block:: dylan

  map(print-condition(_, #t), o.subnotes)

  let outer-values = map(jam-variable(jam, _, default: #f), variables);


Backwards Compatibility
=======================

There are no backwards compatibility considerations for this functionality
as the ``_`` is not a valid variable name in the language as specified by
the DRM.

Reference Implementation
========================

This functionality has been present within Open Dylan since 1999. The code
is largely focused in these methods in ``dfmc/conversion/convert.dylan``:

* ``curried-arguments?``
* ``convert-curried-function-call``
