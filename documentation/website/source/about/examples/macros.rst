******
Macros
******

Dylan macros allow you to create new control constructs and other high-level forms. They can be used to automatically release resources, simplify class creation, or adapt Dylan for a specific problem domain.

Let's say you find this code a little too verbose:

.. code-block:: dylan

  if (test())
    f(x)
  else
    g(x)
  end

and you'd rather be able to write it this way:

.. code-block:: dylan

  iff(test(), f(x), g(x))

You can't just write ``iff`` as a function because then both ``f(x)`` and ``g(x)`` will be evaluated.  The following macro will do the trick:

.. code-block:: dylan

  define macro iff
    { iff(?test:expression, ?true:expression, ?false:expression) }
    => { if (?test) ?true else ?false end }
  end;

See also:

* `uncommon-dylan.dylan <https://github.com/dylan-lang/uncommon-dylan/blob/master/uncommon-dylan.dylan>`_ has a version of the ``iff`` macro that also accepts the syntax ``iff(test(), f(x))``.  Other macro examples in that file are ``inc!``, ``dec!``,  and ``ignore-errors``.
* The `define table macro <https://github.com/dylan-lang/opendylan/blob/master/sources/common-dylan/macros.dylan>`_ in common-dylan is a simple example of making a new top-level "define" form.
* {{drm: Built-In_Macros_and_Special_Definitions, Built-in macros}} -- Many of the basic features of Dylan are implemented as macros.
* The {{drm: Macros}} chapter in the {{drm: Index, DRM}}.

