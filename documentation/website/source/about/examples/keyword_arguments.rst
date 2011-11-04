*****************
Keyword Arguments
*****************

Keyword arguments are optional arguments that are passed by name.

Keyword arguments do not take part in method dispatch.  That is, they have no
role in determining which method of a generic function is actually called.

Keyword arguments in method definitions must match the rules spelled out in the
generic function.

Keyword arguments can be useful when you need to add occasionally-used
parameters to existing functions, similar to the way you can use default
parameter values in languages like C++.

.. code-block:: dylan

  define function describe-list
      (my-list :: <list>, #key verbose?) => ()
    format(*standard-output*, "{a <list>, size: %d", my-list.size);
    if (verbose?)
      format(*standard-output*, ", elements:");
      for (item in my-list)
        format(*standard-output*, " %=", item);
      end for;
    end if;
    format(*standard-output*, "}");
  end function;

This method could be invoked in one of several ways. The first specifies no
keyword arguments, and exhibits the default behavior:

.. code-block:: dylan

  describe-list(#(1, 2, 3))
  // prints "{a <list>, size: 3}"

Alternatively, the verbose? keyword argument could be used to indicate the
'verbose' behavior was desired:

.. code-block:: dylan

  describe-list(#(5, 7, 3), verbose?: #t);
  // prints "{a <list>, size: 3, elements: 5 7 3}"

