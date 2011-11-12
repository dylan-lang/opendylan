**********************
Multiple Return Values
**********************

Any Dylan expression may return multiple values and those values can
be captured by ``let`` (using :drm:`round` as an example):

.. code-block:: dylan

  let (integer, remainder) = round(3.2);

``integer`` is bound to the :drm:`<integer>` value 3 and ``remainder`` is
bound to the :drm:`<real>` value 0.2.

----

Any function may return multiple values by calling the ``values``
function in return position:

.. code-block:: dylan

  define function parse-integer
      (input :: <string>, start :: <integer>)
   => (integer :: <integer>, _end :: <integer>)
    let index = start;
    let integer = 0;
    ...code that sets index and integer...
    values(integer, index)
  end;

Call it like this:

.. code-block:: dylan

  let (int, epos) = parse-integer(" 123 ", 1);

or like this if you don't need the second value:

.. code-block:: dylan

  let int = parse-integer(" 123 ", 1);

Note that if you don't declare that the function returns multiple
values then the compiler may not generate code to return them.  (One
hopes to receive a nice warning in that case.)  Also, if you declare
multiple return values but don't return that many, #f will be returned
in place of the missing values.

----

Sometimes you may have a :drm:`<sequence>` of known values and want to
bind them to variables temporarily:

.. code-block:: dylan

  let point = #(100, 200, 300);
  let (x, y, z) = apply(values, point);

----

You may bind multiple trailing values to a single variable by using
:drm:`#rest <Parameter_Lists>`.  In the following example ``more`` will be bound to ``#(3,
4, 5)``.

.. code-block:: dylan

  let (first, second, #rest more) = values(1, 2, 3, 4, 5);

----

Extra values are ignored and missing values are bound to #f:

.. code-block:: dylan

  let (x, y) = values(1, 2, 3);
  // x = 1, y = 2


  let (x, y, z) = values(1, 2);
  // x = 1, y = 2, z = #f

----

**Notes:**

* Multiple value returns can be more efficient than returning a tuple,
  as some other languages do, because the values can be returned on
  the stack without the need to allocate memory for a tuple.
* There are implementation-defined limits on the number of values that
  may be returned from a function.
* If you find yourself returning a large number of values (e.g. more
  than 2 or 3) you may want to consider returning a single object with
  slots instead.  A rule of thumb is to think about whether it will be
  common for the caller to only use the first value.  But as always
  there may be some cases where it's valid to ignore this rule.

