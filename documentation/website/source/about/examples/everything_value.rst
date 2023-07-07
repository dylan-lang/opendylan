*********************
Everything is a value
*********************

Every Dylan statement or expression returns a value.  Functions and control
constructs like :drm:`select` and :drm:`for` return the values of the last
expression in their body.

:drm:`if` returns a value so it may be used in return position:

.. code-block:: dylan

    let abs = if (x >= 0) x else -x end;

Dylan functions return the values of the last expression in their body to be
evaluated.  This function returns either "foo" or "bar":

.. code-block:: dylan

    define function foobar ()
      if (odd?(random(100)))
        "foo"
      else
        "bar"
      end
    end;

If there is no return value declaration for a function then any number of
values of any type can be returned. Use ``=> (...decls...)`` to declare return
values.  This function returns no values:

.. code-block:: dylan

    define function foo () => ()
      format-out("foo");
    end;

This function returns an integer and a string:

.. code-block:: dylan

   define function bar () => (size :: <integer>, description :: <string>)
     values(42, "the meaning of everything")
   end;
