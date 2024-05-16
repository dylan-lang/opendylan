**********************
Multiple Return Values
**********************

Any Dylan expression may return multiple values and those values can
be captured by :drm:`let`.  Using :drm:`round` as an example:

.. code-block:: dylan

  let (integer, remainder) = round(3.2);

Here ``integer`` is bound to the value ``3`` and ``remainder`` is bound to the
value ``0.2``.

----

Any function may return multiple values by calling the :drm:`values`
function in return position:

.. code-block:: dylan

  define function parse-integer
      (input :: <string>) => (integer :: <integer>, end-pos :: <integer>)
    let integer = 0;
    let pos = 0;
    ...code that sets pos and integer...
    values(integer, pos)
  end;

Call it like this:

.. code-block:: dylan

  let (int, epos) = parse-integer("123 blah");

or like this if you don't need the second value:

.. code-block:: dylan

  let int = parse-integer("123 blah");

----

Sometimes you may have a :drm:`<sequence>` of known values and want to
bind them to variables temporarily:

.. code-block:: dylan

  let point = #(100, 200, 300);
  let (x, y, z) = apply(values, point);

----

You may bind multiple trailing values to a single variable by using :drm:`#rest
<Parameter_Lists>`.  In the following example ``more`` will be bound to ``#(3,
4, 5)``.

.. code-block:: dylan

  let (first, second, #rest more) = values(1, 2, 3, 4, 5);

----

Extra values are ignored and missing values are bound to :drm:`#f`:

.. code-block:: dylan

  let (x, y) = values(1, 2, 3);
  // x = 1, y = 2


  let (x, y, z) = values(1, 2);
  // x = 1, y = 2, z = #f
