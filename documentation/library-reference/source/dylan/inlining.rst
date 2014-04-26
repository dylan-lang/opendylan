Inlining adjectives for methods, constants, functions, and slots
----------------------------------------------------------------

To *inline* a value is to replace, at compile time, a reference to a
variable with the value of that variable. Such inlining often allows
compile-time evaluation ("constant folding") or partial evaluation.

The Open Dylan compiler can perform inlining on generic function
methods, constants, class slots, and functions (created with ``define
function`` — see :doc:`Function Definition <define-function>`). We have
extended the Dylan language specification of ``define method``,
``define constant``, and class slots with inlining definition adjectives
and have included those same adjectives in our language extension
``define function``. The adjectives are:

- ``not-inline`` Never inline this item.
- ``default-inline`` (default)
  Inline this item within a library, at the compiler’s discretion. Never
  inline a cross-library reference.
- ``may-inline`` Inline this item within or between libraries, at the
  compiler’s discretion.
- ``inline`` Inline this item wherever the compiler can do so.

In addition, ``define constant`` and ``define function`` permit the
adjective ``inline-only``, which forces every reference to the constant
or function to be inlined.

.. note:: If you export from a library any variables created with
   ``may-inline``, ``inline``, or ``inline-only``, and then change the
   values of the variables, client libraries may need to be recompiled.
