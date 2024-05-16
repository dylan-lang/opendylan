******
Macros
******

Dylan macros allow you to create new control constructs and other high-level
forms. They can be used to automatically release resources, simplify class
creation, or adapt Dylan for a specific problem domain.

Let's say you find this code a little too verbose:

.. code-block:: dylan

  if (test())
    f1(many, arguments, here)
  else
    long-function-name(and, more, args)
  end

and you'd rather be able to write it this way:

.. code-block:: dylan

  iff(test(),
      f1(many, arguments, here),
      long-function-name(and, more, args))

You can't just define ``iff`` as a function because then the calls to both
``f1`` and ``long-function-name`` will be evaluated.  Instead, you can write a
macro:

.. code-block:: dylan

  define macro iff
      { iff(?test:expression, ?true:expression, ?false:expression) }
   => { if (?test) ?true else ?false end }
  end;

See also:

* The `define table macro
  <https://github.com/dylan-lang/opendylan/blob/master/sources/common-dylan/macros.dylan>`_
  in common-dylan is a simple example of making a new top-level "define" form.
* :drm:`Built-in macros <Built-In_Macros_and_Special_Definitions>` -- Many of
  the basic features of Dylan are implemented as macros.
* The :doc:`Dylan Macro System </articles/macro-system>` article by Dustin Voss.
* The :drm:`Macros` chapter in the :drm:`DRM <Index>`.
