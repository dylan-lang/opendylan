*********************
Everything is a value
*********************

Every Dylan statement or expression returns a value.  Control
constructs and methods generally return the value of the last
expression in their body.

``if`` returns a value so it may be used in return position:

.. code-block:: dylan

    format-out("abs(x) = %d", if (x >= 0) x else -x end);

Dylan methods return the value of the last expression in their body to
be evaluated.  If there is no return value declaration for the generic
function then any number of values of type <object> may be returned.
This function returns the string "foo":

.. code-block:: dylan

    define method foo ()
      "foo"
    end;

Sometimes you write a function for which there is no useful return
value.  In these cases it is sometimes useful (especially for external
APIs) to tell the compiler that no values are returned since it can
then give you warnings when callers expect a value.  ``=> ()`` is the
way to say this:

.. code-block:: dylan

    define method foo () => ()
      format-out("foo");
    end;

See also, {{Parameter Lists}} and {{Code Examples}}.

